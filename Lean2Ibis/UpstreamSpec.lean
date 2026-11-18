import APS210.Upstream
import Lean2Ibis.IR

namespace Lean2Ibis.UpstreamSpec

open APS210.Upstream

/--
APS 210 upstream & cross-row aggregation pipeline compiled from Lean → Ibis:
1. Date-to-bucket & encumbrance classification (`dateDiff` against `reporting_date`)
2. Derivative MNA netting (`groupBy ["mna_id"]` for eligible MNAs vs gross non-MNA)
3. Customer-group `$250k` FCS / `$2m` SME aggregation & Att A Para 42 term-deposit pool tainting (`window` partitionBy)
4. LCR Para 9(e) foreign sovereign HQLA1 per-currency cap (`groupBy ["currency"]`)
5. Att A Table 3 Row 11(d) 24-month rolling 30-day collateral flow lookback (`window` ordered rolling + `abs` + `agg .max`)
6. Att A Para 50 12-month monthly average net cash outflows (`agg .avg`)
-/

def effectiveMaturityDateExpr : SExpr :=
  .ite (.col "has_call_option") (.col "earliest_call_date") (.col "maturity_date")

def positionsEnrichedRel : RelExpr :=
  .withColumns
    [ ("effective_maturity_date", effectiveMaturityDateExpr)
    , ("residual_days",
        .binOp .max (.col "notice_days")
          (.binOp .dateDiff (.col "effective_maturity_date") (.var "reporting_date")))
    , ("encumbrance_days",
        .ite (.col "is_encumbered")
          (.unOp .nonNeg (.binOp .dateDiff (.col "encumbrance_end_date") (.var "reporting_date")))
          (.litRat 0 1))
    ]
    (.table "positions")

def mnaNettedRel : RelExpr :=
  .withColumns
    [ ("net_payable", .unOp .nonNeg (.col "net_mtm"))
    , ("net_receivable", .unOp .nonNeg (.binOp .sub (.litRat 0 1) (.col "net_mtm")))
    ]
    (.groupBy ["mna_id"]
      [ ("net_mtm", .sum, .binOp .sub (.col "mtm_payable") (.col "mtm_receivable")) ]
      (.filter (.col "has_eligible_mna") (.table "derivative_trades")))

def nonMnaGrossRel : RelExpr :=
  .withColumns
    [ ("gross_payable", .unOp .nonNeg (.col "mtm_payable"))
    , ("gross_receivable", .unOp .nonNeg (.col "mtm_receivable"))
    ]
    (.filter (.unOp .not (.col "has_eligible_mna")) (.table "derivative_trades"))

def depositsWindowedRel : RelExpr :=
  .window
    [ ("cust_total_balance", .sum, .col "balance", { partitionBy := ["customer_group_id"] })
    , ("pool_tainted", .anyBool, .col "has_early_withdrawal_no_penalty", { partitionBy := ["product_category"] })
    ]
    (.table "deposit_accounts")

def depositsClassifiedRel : RelExpr :=
  let isSmeEligible :=
    .binOp .and
      (.binOp .eq (.col "customer_type") (.litStr "sme"))
      (.binOp .lt (.col "cust_total_balance") (.litRat 2000000 1))
  let isRetailOrSme :=
    .binOp .or (.binOp .eq (.col "customer_type") (.litStr "retail")) isSmeEligible
  let fcsShare :=
    .binOp .div
      (.binOp .mul (.col "balance") (.binOp .min (.col "cust_total_balance") (.litRat 250000 1)))
      (.col "cust_total_balance")
  .withColumns
    [ ("is_sme_eligible", isSmeEligible)
    , ("fcs_insured_amt",
        .ite isRetailOrSme (.binOp .min (.col "balance") fcsShare) (.litRat 0 1))
    , ("is_excluded_term_deposit",
        .binOp .and (.binOp .gt (.col "residual_days") (.litRat 30 1)) (.unOp .not (.col "pool_tainted")))
    ]
    depositsWindowedRel

def foreignSovCappedByCcyRel : RelExpr :=
  .withColumns
    [ ("capped_hqla1",
        .binOp .min (.col "total_hqla1") (.unOp .nonNeg (.col "total_outflow")))
    ]
    (.groupBy ["currency"]
      [ ("total_hqla1", .sum, .col "hqla1_foreign_sov_amt")
      , ("total_outflow", .sum, .col "ccy_net_outflow")
      ]
      (.table "ccy_hqla1_positions"))

def collateralRolling30dRel : RelExpr :=
  .withColumns
    [ ("abs_30d_flow", .unOp .abs (.col "rolling_30d_flow")) ]
    (.window
      [ ("rolling_30d_flow", .sum, .col "net_collateral_flow",
          { partitionBy := []
          , orderBy := [("valuation_date", true)]
          , preceding? := some 29
          , following? := some 0 }) ]
      (.table "daily_collateral_flows"))

def upstreamProgram : Program :=
  { name := "evaluate_aps210_upstream"
  , tableParams :=
      [ ("positions", ["id", "amount", "maturity_date", "earliest_call_date", "has_call_option", "notice_days", "encumbrance_end_date", "is_encumbered"])
      , ("derivative_trades", ["trade_id", "mna_id", "has_eligible_mna", "mtm_payable", "mtm_receivable"])
      , ("deposit_accounts", ["account_id", "customer_group_id", "customer_type", "product_category", "balance", "has_early_withdrawal_no_penalty", "residual_days"])
      , ("ccy_hqla1_positions", ["currency", "hqla1_foreign_sov_amt", "ccy_net_outflow"])
      , ("daily_collateral_flows", ["valuation_date", "net_collateral_flow"])
      , ("monthly_net_outflows", ["month_end_date", "net_cash_outflow"])
      ]
  , scalarParams :=
      [ ("reporting_date", .date) ]
  , lets :=
      [ ("pos_lcr_within_30d_amt",
          .agg .sum
            (.filter (.binOp .le (.col "residual_days") (.litRat 30 1)) positionsEnrichedRel)
            (.col "amount"))
      , ("pos_nsfr_lt_6m_amt",
          .agg .sum
            (.filter (.binOp .lt (.col "residual_days") (.litRat 180 1)) positionsEnrichedRel)
            (.col "amount"))
      , ("pos_nsfr_6m_to_1y_amt",
          .agg .sum
            (.filter
              (.binOp .and
                (.binOp .ge (.col "residual_days") (.litRat 180 1))
                (.binOp .lt (.col "residual_days") (.litRat 365 1)))
              positionsEnrichedRel)
            (.col "amount"))
      , ("pos_nsfr_ge_1y_amt",
          .agg .sum
            (.filter (.binOp .ge (.col "residual_days") (.litRat 365 1)) positionsEnrichedRel)
            (.col "amount"))
      , ("pos_enc_ge_1y_amt",
          .agg .sum
            (.filter
              (.binOp .and (.col "is_encumbered") (.binOp .ge (.col "encumbrance_days") (.litRat 365 1)))
              positionsEnrichedRel)
            (.col "amount"))
      , ("mna_net_outflow", .agg .sum mnaNettedRel (.col "net_payable"))
      , ("mna_net_inflow", .agg .sum mnaNettedRel (.col "net_receivable"))
      , ("gross_deriv_outflow", .agg .sum nonMnaGrossRel (.col "gross_payable"))
      , ("gross_deriv_inflow", .agg .sum nonMnaGrossRel (.col "gross_receivable"))
      , ("deriv_net_outflow", .scalar (.binOp .add (.var "mna_net_outflow") (.var "gross_deriv_outflow")))
      , ("deriv_net_inflow", .scalar (.binOp .add (.var "mna_net_inflow") (.var "gross_deriv_inflow")))
      , ("sme_eligible_deposits",
          .agg .sum (.filter (.col "is_sme_eligible") depositsClassifiedRel) (.col "balance"))
      , ("fcs_insured_deposits",
          .agg .sum depositsClassifiedRel (.col "fcs_insured_amt"))
      , ("excluded_term_deposits",
          .agg .sum (.filter (.col "is_excluded_term_deposit") depositsClassifiedRel) (.col "balance"))
      , ("capped_foreign_sov_hqla1",
          .agg .sum foreignSovCappedByCcyRel (.col "capped_hqla1"))
      , ("max_30d_collateral_outflow",
          .agg .max collateralRolling30dRel (.col "abs_30d_flow"))
      , ("avg_12m_net_outflow",
          .agg .avg (.table "monthly_net_outflows") (.col "net_cash_outflow"))
      ]
  , outputs :=
      [ ("pos_lcr_within_30d_amt", .var "pos_lcr_within_30d_amt")
      , ("pos_nsfr_lt_6m_amt", .var "pos_nsfr_lt_6m_amt")
      , ("pos_nsfr_6m_to_1y_amt", .var "pos_nsfr_6m_to_1y_amt")
      , ("pos_nsfr_ge_1y_amt", .var "pos_nsfr_ge_1y_amt")
      , ("pos_enc_ge_1y_amt", .var "pos_enc_ge_1y_amt")
      , ("deriv_net_outflow", .var "deriv_net_outflow")
      , ("deriv_net_inflow", .var "deriv_net_inflow")
      , ("sme_eligible_deposits", .var "sme_eligible_deposits")
      , ("fcs_insured_deposits", .var "fcs_insured_deposits")
      , ("excluded_term_deposits", .var "excluded_term_deposits")
      , ("capped_foreign_sov_hqla1", .var "capped_foreign_sov_hqla1")
      , ("max_30d_collateral_outflow", .var "max_30d_collateral_outflow")
      , ("avg_12m_net_outflow", .var "avg_12m_net_outflow")
      ]
  }

def customerTypeToStr : DepositCustomerType → String
  | .retail => "retail"
  | .sme => "sme"
  | .wholesale => "wholesale"

def upstreamInputToTableEnv (input : UpstreamInput) : TableEnv :=
  [ ("positions", input.positions.map (fun p =>
      [ ("id", .str p.id)
      , ("amount", .rat p.amount)
      , ("maturity_date", .date p.maturityDate)
      , ("earliest_call_date", .date p.earliestCallDate)
      , ("has_call_option", .bool p.hasCallOption)
      , ("notice_days", .rat p.noticeDays)
      , ("encumbrance_end_date", .date p.encumbranceEndDate)
      , ("is_encumbered", .bool p.isEncumbered)
      ]))
  , ("derivative_trades", input.derivativeTrades.map (fun t =>
      [ ("trade_id", .str t.tradeId)
      , ("mna_id", .str t.mnaId)
      , ("has_eligible_mna", .bool t.hasEligibleMna)
      , ("mtm_payable", .rat t.mtmPayable)
      , ("mtm_receivable", .rat t.mtmReceivable)
      ]))
  , ("deposit_accounts", input.depositAccounts.map (fun a =>
      [ ("account_id", .str a.accountId)
      , ("customer_group_id", .str a.customerGroupId)
      , ("customer_type", .str (customerTypeToStr a.customerType))
      , ("product_category", .str a.productCategory)
      , ("balance", .rat a.balance)
      , ("has_early_withdrawal_no_penalty", .bool a.hasEarlyWithdrawalNoPenalty)
      , ("residual_days", .rat a.residualDays)
      ]))
  , ("ccy_hqla1_positions", input.ccyHqla1Positions.map (fun c =>
      [ ("currency", .str c.currency)
      , ("hqla1_foreign_sov_amt", .rat c.hqla1ForeignSovAmt)
      , ("ccy_net_outflow", .rat c.ccyNetOutflow)
      ]))
  , ("daily_collateral_flows", input.dailyCollateralFlows.map (fun f =>
      [ ("valuation_date", .date f.valuationDate)
      , ("net_collateral_flow", .rat f.netCollateralFlow)
      ]))
  , ("monthly_net_outflows", input.monthlyNetOutflows.map (fun m =>
      [ ("month_end_date", .date m.monthEndDate)
      , ("net_cash_outflow", .rat m.netCashOutflow)
      ]))
  ]

def upstreamResultToRow (r : UpstreamResult) : Row :=
  [ ("pos_lcr_within_30d_amt", .rat r.posLcrWithin30dAmt)
  , ("pos_nsfr_lt_6m_amt", .rat r.posNsfrLt6mAmt)
  , ("pos_nsfr_6m_to_1y_amt", .rat r.posNsfr6mTo1yAmt)
  , ("pos_nsfr_ge_1y_amt", .rat r.posNsfrGe1yAmt)
  , ("pos_enc_ge_1y_amt", .rat r.posEncGe1yAmt)
  , ("deriv_net_outflow", .rat r.derivNetOutflow)
  , ("deriv_net_inflow", .rat r.derivNetInflow)
  , ("sme_eligible_deposits", .rat r.smeEligibleDeposits)
  , ("fcs_insured_deposits", .rat r.fcsInsuredDeposits)
  , ("excluded_term_deposits", .rat r.excludedTermDeposits)
  , ("capped_foreign_sov_hqla1", .rat r.cappedForeignSovHqla1)
  , ("max_30d_collateral_outflow", .rat r.max30dCollateralOutflow)
  , ("avg_12m_net_outflow", .rat r.avg12mNetOutflow)
  ]

def smokeInput : UpstreamInput :=
  { reportingDate := civilDate 2026 6 30
  , positions :=
      [ { id := "p1", amount := 100, maturityDate := civilDate 2026 7 15, earliestCallDate := civilDate 2026 7 15, hasCallOption := false, noticeDays := 0, encumbranceEndDate := civilDate 2026 6 30, isEncumbered := false }
      , { id := "p2", amount := 200, maturityDate := civilDate 2027 7 15, earliestCallDate := civilDate 2026 9 1, hasCallOption := true, noticeDays := 0, encumbranceEndDate := civilDate 2026 6 30, isEncumbered := false }
      , { id := "p3", amount := 300, maturityDate := civilDate 2027 1 15, earliestCallDate := civilDate 2027 1 15, hasCallOption := false, noticeDays := 0, encumbranceEndDate := civilDate 2027 8 1, isEncumbered := true }
      , { id := "p4", amount := 400, maturityDate := civilDate 2026 7 5, earliestCallDate := civilDate 2026 7 5, hasCallOption := false, noticeDays := 400, encumbranceEndDate := civilDate 2026 6 30, isEncumbered := false }
      ]
  , derivativeTrades :=
      [ { tradeId := "t1", mnaId := "mna1", hasEligibleMna := true, mtmPayable := 100, mtmReceivable := 40 }
      , { tradeId := "t2", mnaId := "mna1", hasEligibleMna := true, mtmPayable := 20, mtmReceivable := 50 }
      , { tradeId := "t3", mnaId := "mna2", hasEligibleMna := true, mtmPayable := 10, mtmReceivable := 60 }
      , { tradeId := "t4", mnaId := "none", hasEligibleMna := false, mtmPayable := 25, mtmReceivable := 10 }
      ]
  , depositAccounts :=
      [ { accountId := "a1", customerGroupId := "cg1", customerType := .retail, productCategory := "prodA", balance := 200000, hasEarlyWithdrawalNoPenalty := true, residualDays := 10 }
      , { accountId := "a2", customerGroupId := "cg1", customerType := .retail, productCategory := "prodA", balance := 300000, hasEarlyWithdrawalNoPenalty := false, residualDays := 60 }
      , { accountId := "a3", customerGroupId := "cg2", customerType := .sme, productCategory := "prodB", balance := 1500000, hasEarlyWithdrawalNoPenalty := false, residualDays := 60 }
      , { accountId := "a4", customerGroupId := "cg3", customerType := .sme, productCategory := "prodB", balance := 2500000, hasEarlyWithdrawalNoPenalty := false, residualDays := 90 }
      ]
  , ccyHqla1Positions :=
      [ { currency := "USD", hqla1ForeignSovAmt := 120, ccyNetOutflow := 60 }
      , { currency := "USD", hqla1ForeignSovAmt := 30, ccyNetOutflow := 40 }
      , { currency := "EUR", hqla1ForeignSovAmt := 40, ccyNetOutflow := 80 }
      , { currency := "JPY", hqla1ForeignSovAmt := 50, ccyNetOutflow := -10 }
      ]
  , dailyCollateralFlows :=
      (List.range 35).map (fun (i : Nat) =>
        { valuationDate := civilDate 2026 1 1 + (i : Int)
        , netCollateralFlow := if i < 30 then 1 else -20
        })
  , monthlyNetOutflows :=
      (List.range 12).map (fun (i : Nat) =>
        { monthEndDate := civilDate 2025 7 28 + ((i * 30 : Nat) : Int)
        , netCashOutflow := 100 + 10 * (i : Int)
        })
  }

theorem upstream_eval_matches_domain :
    evalProgram upstreamProgram (upstreamInputToTableEnv smokeInput) [("reporting_date", .date smokeInput.reportingDate)] =
      upstreamResultToRow (evaluateUpstream smokeInput) := by
  native_decide

end Lean2Ibis.UpstreamSpec
