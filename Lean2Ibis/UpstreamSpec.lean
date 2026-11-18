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
  .ite (.col "has_call_option") (.col "earliest_call_date")
    (.ite (.col "has_extension_option") (.col "latest_extension_date") (.col "maturity_date"))

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
  let inLookback :=
    .filter
      (.binOp .and
        (.binOp .ge (.binOp .dateDiff (.col "valuation_date") (.dateAddMonths (.var "reporting_date") (-24))) (.litRat 0 1))
        (.binOp .le (.binOp .dateDiff (.col "valuation_date") (.var "reporting_date")) (.litRat 0 1)))
      (.table "daily_collateral_flows")
  .withColumns
    [ ("abs_30d_flow", .unOp .abs (.col "rolling_30d_flow")) ]
    (.window
      [ ("rolling_30d_flow", .sum, .col "net_collateral_flow",
          { partitionBy := []
          , orderBy := [("valuation_date", true)]
          , precedingDays? := some 29 }) ]
      inLookback)

def collateralLookbackInWindowRel : RelExpr :=
  .withColumns
    [ ("days_after_23m_anchor", .binOp .dateDiff (.col "valuation_date") (.dateAddMonths (.var "reporting_date") (-23))) ]
    (.filter
      (.binOp .and
        (.binOp .ge (.binOp .dateDiff (.col "valuation_date") (.dateAddMonths (.var "reporting_date") (-24))) (.litRat 0 1))
        (.binOp .le (.binOp .dateDiff (.col "valuation_date") (.var "reporting_date")) (.litRat 0 1)))
      (.table "daily_collateral_flows"))

def monthly12mWindowRel : RelExpr :=
  .filter
    (.binOp .and
      (.binOp .gt (.binOp .dateDiff (.col "month_end_date") (.dateAddMonths (.var "reporting_date") (-12))) (.litRat 0 1))
      (.binOp .le (.binOp .dateDiff (.col "month_end_date") (.var "reporting_date")) (.litRat 0 1)))
    (.table "monthly_net_outflows")

def ecaiGradeRankExpr : SExpr :=
  .caseStr (.col "rating_str")
    [ ("AAA", .litRat 1 1)
    , ("AA+", .litRat 1 1)
    , ("AA", .litRat 1 1)
    , ("AA-", .litRat 1 1)
    , ("A+", .litRat 2 1)
    , ("A", .litRat 2 1)
    , ("A-", .litRat 2 1)
    , ("BBB+", .litRat 3 1)
    , ("BBB", .litRat 3 1)
    , ("BBB-", .litRat 3 1)
    , ("below_bbb_minus", .litRat 4 1)
    ]
    (.litRat 7 1)

def ecaiResolvedRel : RelExpr :=
  let rated :=
    .filter (.binOp .lt (.col "grade_rank") (.litRat 7 1))
      (.withColumns [("grade_rank", ecaiGradeRankExpr)] (.table "ecai_assessments"))
  let ranked :=
    .window
      [ ("rn", .rowNumber, .col "grade_rank",
          { partitionBy := ["exposure_id"], orderBy := [("grade_rank", true)] })
      , ("cnt", .count, .col "grade_rank",
          { partitionBy := ["exposure_id"] })
      ]
      rated
  .filter
    (.binOp .or
      (.binOp .and (.binOp .eq (.col "cnt") (.litRat 1 1)) (.binOp .eq (.col "rn") (.litRat 1 1)))
      (.binOp .and (.binOp .ge (.col "cnt") (.litRat 2 1)) (.binOp .eq (.col "rn") (.litRat 2 1))))
    ranked

def amortisationEnrichedRel : RelExpr :=
  .withColumns
    [ ("cf_days", .binOp .dateDiff (.col "payment_date") (.var "reporting_date")) ]
    (.table "amortisation_cashflows")

def sftLegsAdjustedRel : RelExpr :=
  let netAdj (colName : String) : SExpr :=
    .ite (.col "netting_eligible") (.col colName) (.binOp .min (.col colName) (.litRat 0 1))
  .withColumns
    [ ("adj_hqla1", netAdj "hqla1_delta")
    , ("adj_hqla2a", netAdj "hqla2a_delta")
    , ("adj_hqla2b", netAdj "hqla2b_delta")
    ]
    (.filter
      (.binOp .and
        (.col "meets_operational_reqs")
        (.binOp .and
          (.binOp .le (.col "residual_days") (.litRat 30 1))
          (.binOp .or (.unOp .not (.var "is_aud_only")) (.binOp .eq (.col "currency") (.litStr "AUD")))))
      (.table "sft_legs"))

def clfRecognisedExpr : SExpr :=
  let boundA := .unOp .nonNeg (.var "clf_contractual")
  let boundB :=
    .unOp .nonNeg
      (.binOp .add
        (.binOp .sub (.var "clf_contractual") (.unOp .nonNeg (.var "clf_pledged_zero_runoff")))
        (.unOp .nonNeg (.var "clf_received_zero_inflow")))
  let boundC := .unOp .nonNeg (.var "clf_collateral_after_margin")
  .ite (.var "clf_eligible")
    (.binOp .min boundA (.binOp .min boundB boundC))
    (.litRat 0 1)

def upstreamProgram : Program :=
  { name := "evaluate_aps210_upstream"
  , tableParams :=
      [ ("positions", ["id", "amount", "maturity_date", "earliest_call_date", "has_call_option", "latest_extension_date", "has_extension_option", "notice_days", "encumbrance_end_date", "is_encumbered"])
      , ("derivative_trades", ["trade_id", "mna_id", "has_eligible_mna", "mtm_payable", "mtm_receivable"])
      , ("deposit_accounts", ["account_id", "customer_group_id", "customer_type", "product_category", "balance", "has_early_withdrawal_no_penalty", "residual_days"])
      , ("ccy_hqla1_positions", ["currency", "hqla1_foreign_sov_amt", "ccy_net_outflow"])
      , ("daily_collateral_flows", ["valuation_date", "net_collateral_flow"])
      , ("monthly_net_outflows", ["month_end_date", "net_cash_outflow"])
      , ("ecai_assessments", ["exposure_id", "rating_str"])
      , ("amortisation_cashflows", ["loan_id", "payment_date", "principal_amount"])
      , ("sft_legs", ["leg_id", "currency", "residual_days", "meets_operational_reqs", "netting_eligible", "hqla1_delta", "hqla2a_delta", "hqla2b_delta"])
      ]
  , scalarParams :=
      [ ("reporting_date", .date)
      , ("is_aud_only", .bool)
      , ("clf_eligible", .bool)
      , ("clf_contractual", .rat)
      , ("clf_pledged_zero_runoff", .rat)
      , ("clf_received_zero_inflow", .rat)
      , ("clf_collateral_after_margin", .rat)
      ]
  , lets :=
      [ ("six_months_days", .scalar (.binOp .dateDiff (.dateAddMonths (.var "reporting_date") 6) (.var "reporting_date")))
      , ("one_year_days", .scalar (.binOp .dateDiff (.dateAddMonths (.var "reporting_date") 12) (.var "reporting_date")))
      , ("pos_lcr_within_30d_amt",
          .agg .sum
            (.filter (.binOp .le (.col "residual_days") (.litRat 30 1)) positionsEnrichedRel)
            (.col "amount"))
      , ("pos_nsfr_lt_6m_amt",
          .agg .sum
            (.filter (.binOp .lt (.col "residual_days") (.var "six_months_days")) positionsEnrichedRel)
            (.col "amount"))
      , ("pos_nsfr_6m_to_1y_amt",
          .agg .sum
            (.filter
              (.binOp .and
                (.binOp .ge (.col "residual_days") (.var "six_months_days"))
                (.binOp .lt (.col "residual_days") (.var "one_year_days")))
              positionsEnrichedRel)
            (.col "amount"))
      , ("pos_nsfr_ge_1y_amt",
          .agg .sum
            (.filter (.binOp .ge (.col "residual_days") (.var "one_year_days")) positionsEnrichedRel)
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
      , ("collat_cnt",
          .agg .count collateralLookbackInWindowRel (.col "valuation_date"))
      , ("collat_min_after_23m",
          .agg .min collateralLookbackInWindowRel (.col "days_after_23m_anchor"))
      , ("collat_distinct_months",
          .agg .countDistinct collateralLookbackInWindowRel (.yearMonth (.col "valuation_date")))
      , ("collateral_lookback_valid",
          .scalar (.binOp .and (.binOp .gt (.var "collat_cnt") (.litRat 0 1))
            (.binOp .and (.binOp .le (.var "collat_min_after_23m") (.litRat 0 1))
              (.binOp .ge (.var "collat_distinct_months") (.litRat 24 1)))))
      , ("avg_12m_net_outflow",
          .agg .avg monthly12mWindowRel (.col "net_cash_outflow"))
      , ("m12_cnt",
          .agg .count monthly12mWindowRel (.col "month_end_date"))
      , ("m12_distinct",
          .agg .countDistinct monthly12mWindowRel (.yearMonth (.col "month_end_date")))
      , ("avg_12m_valid",
          .scalar (.binOp .and (.binOp .eq (.var "m12_cnt") (.litRat 12 1)) (.binOp .eq (.var "m12_distinct") (.litRat 12 1))))
      , ("ecai_resolved_grade_sum",
          .agg .sum ecaiResolvedRel (.col "grade_rank"))
      , ("ecai_investment_grade_count",
          .agg .count (.filter (.binOp .le (.col "grade_rank") (.litRat 3 1)) ecaiResolvedRel) (.col "grade_rank"))
      , ("amort_nsfr_lt_6m_amt",
          .agg .sum
            (.filter (.binOp .lt (.col "cf_days") (.var "six_months_days")) amortisationEnrichedRel)
            (.col "principal_amount"))
      , ("amort_nsfr_6m_to_1y_amt",
          .agg .sum
            (.filter
              (.binOp .and
                (.binOp .ge (.col "cf_days") (.var "six_months_days"))
                (.binOp .lt (.col "cf_days") (.var "one_year_days")))
              amortisationEnrichedRel)
            (.col "principal_amount"))
      , ("amort_nsfr_ge_1y_amt",
          .agg .sum
            (.filter (.binOp .ge (.col "cf_days") (.var "one_year_days")) amortisationEnrichedRel)
            (.col "principal_amount"))
      , ("sft_unwind_hqla1", .agg .sum sftLegsAdjustedRel (.col "adj_hqla1"))
      , ("sft_unwind_hqla2a", .agg .sum sftLegsAdjustedRel (.col "adj_hqla2a"))
      , ("sft_unwind_hqla2b", .agg .sum sftLegsAdjustedRel (.col "adj_hqla2b"))
      , ("clf_recognised_amount", .scalar clfRecognisedExpr)
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
      , ("collateral_lookback_valid", .var "collateral_lookback_valid")
      , ("avg_12m_net_outflow", .var "avg_12m_net_outflow")
      , ("avg_12m_valid", .var "avg_12m_valid")
      , ("ecai_resolved_grade_sum", .var "ecai_resolved_grade_sum")
      , ("ecai_investment_grade_count", .var "ecai_investment_grade_count")
      , ("amort_nsfr_lt_6m_amt", .var "amort_nsfr_lt_6m_amt")
      , ("amort_nsfr_6m_to_1y_amt", .var "amort_nsfr_6m_to_1y_amt")
      , ("amort_nsfr_ge_1y_amt", .var "amort_nsfr_ge_1y_amt")
      , ("sft_unwind_hqla1", .var "sft_unwind_hqla1")
      , ("sft_unwind_hqla2a", .var "sft_unwind_hqla2a")
      , ("sft_unwind_hqla2b", .var "sft_unwind_hqla2b")
      , ("clf_recognised_amount", .var "clf_recognised_amount")
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
      , ("latest_extension_date", .date p.latestExtensionDate)
      , ("has_extension_option", .bool p.hasExtensionOption)
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
  , ("ecai_assessments", input.ecaiAssessments.map (fun e =>
      [ ("exposure_id", .str e.exposureId)
      , ("rating_str", .str e.ratingStr)
      ]))
  , ("amortisation_cashflows", input.amortisationCashflows.map (fun c =>
      [ ("loan_id", .str c.loanId)
      , ("payment_date", .date c.paymentDate)
      , ("principal_amount", .rat c.principalAmount)
      ]))
  , ("sft_legs", input.sftLegs.map (fun s =>
      [ ("leg_id", .str s.legId)
      , ("currency", .str s.currency)
      , ("residual_days", .rat (s.residualDays : Rat))
      , ("meets_operational_reqs", .bool s.meetsOperationalReqs)
      , ("netting_eligible", .bool s.nettingEligible)
      , ("hqla1_delta", .rat s.hqla1Delta)
      , ("hqla2a_delta", .rat s.hqla2aDelta)
      , ("hqla2b_delta", .rat s.hqla2bDelta)
      ]))
  ]

def upstreamInputToScalars (input : UpstreamInput) : Row :=
  [ ("reporting_date", .date input.reportingDate)
  , ("is_aud_only", .bool input.isAudOnly)
  , ("clf_eligible", .bool input.clf.eligible)
  , ("clf_contractual", .rat input.clf.contractualAmount)
  , ("clf_pledged_zero_runoff", .rat input.clf.pledgedZeroRunoff)
  , ("clf_received_zero_inflow", .rat input.clf.receivedZeroInflow)
  , ("clf_collateral_after_margin", .rat input.clf.collateralAfterMargin)
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
  , ("collateral_lookback_valid", .bool r.collateralLookbackValid)
  , ("avg_12m_net_outflow", .rat r.avg12mNetOutflow)
  , ("avg_12m_valid", .bool r.avg12mValid)
  , ("ecai_resolved_grade_sum", .rat r.ecaiResolvedGradeSum)
  , ("ecai_investment_grade_count", .rat r.ecaiInvestmentGradeCount)
  , ("amort_nsfr_lt_6m_amt", .rat r.amortNsfrLt6mAmt)
  , ("amort_nsfr_6m_to_1y_amt", .rat r.amortNsfr6mTo1yAmt)
  , ("amort_nsfr_ge_1y_amt", .rat r.amortNsfrGe1yAmt)
  , ("sft_unwind_hqla1", .rat r.sftUnwindHqla1)
  , ("sft_unwind_hqla2a", .rat r.sftUnwindHqla2a)
  , ("sft_unwind_hqla2b", .rat r.sftUnwindHqla2b)
  , ("clf_recognised_amount", .rat r.clfRecognisedAmount)
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
      ((List.range 25).map (fun (m : Nat) =>
        { valuationDate := addMonthsEpoch (civilDate 2024 6 30) (m : Int), netCollateralFlow := 0 })) ++
      (List.range 35).map (fun (i : Nat) =>
        { valuationDate := civilDate 2026 1 1 + (i : Int)
        , netCollateralFlow := if i < 30 then 1 else -20
        })
  , monthlyNetOutflows :=
      (List.range 12).map (fun (i : Nat) =>
        { monthEndDate := civilDate 2025 7 28 + ((i * 30 : Nat) : Int)
        , netCashOutflow := 100 + 10 * (i : Int)
        })
  , ecaiAssessments :=
      [ { exposureId := "e1", ratingStr := "A+" }           -- 1 rating -> grade 2
      , { exposureId := "e2", ratingStr := "AAA" }
      , { exposureId := "e2", ratingStr := "BBB-" }         -- 2 ratings -> worse = grade 3
      , { exposureId := "e3", ratingStr := "AA+" }
      , { exposureId := "e3", ratingStr := "A" }
      , { exposureId := "e3", ratingStr := "below_bbb_minus" } -- 3 ratings -> 2nd best = grade 2
      , { exposureId := "e4", ratingStr := "unrated" }      -- 0 rated -> excluded
      ]
  , amortisationCashflows :=
      [ { loanId := "L1", paymentDate := civilDate 2026 9 1, principalAmount := 50 }
      , { loanId := "L1", paymentDate := civilDate 2027 3 1, principalAmount := 75 }
      , { loanId := "L1", paymentDate := civilDate 2028 6 30, principalAmount := 125 }
      ]
  , sftLegs :=
      [ { legId := "s1", meetsOperationalReqs := true, nettingEligible := true, hqla1Delta := 20, hqla2aDelta := -5, hqla2bDelta := 0 }
      , { legId := "s2", meetsOperationalReqs := true, nettingEligible := false, hqla1Delta := 15, hqla2aDelta := -10, hqla2bDelta := 3 }
      , { legId := "s3", meetsOperationalReqs := false, nettingEligible := true, hqla1Delta := 100, hqla2aDelta := 0, hqla2bDelta := 0 }
      ]
  , clf :=
      { eligible := true
      , contractualAmount := 500
      , pledgedZeroRunoff := 80
      , receivedZeroInflow := 20
      , collateralAfterMargin := 460
      }
  }

theorem upstream_eval_matches_domain :
    evalProgram upstreamProgram (upstreamInputToTableEnv smokeInput) (upstreamInputToScalars smokeInput) =
      upstreamResultToRow (evaluateUpstream smokeInput) := by
  native_decide

end Lean2Ibis.UpstreamSpec
