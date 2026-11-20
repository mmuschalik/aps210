import APS210.Upstream
import Lean2Ibis.IR

namespace Lean2Ibis.UpstreamSpec

open APS210.Upstream
open APS210.Facts

/-!
APS 210 upstream & cross-row aggregation pipeline compiled from Lean → Ibis:
1. Date-to-bucket & encumbrance classification (`dateDiff` against `reporting_date`)
2. Derivative MNA netting (`groupBy ["mna_id"]` for eligible MNAs vs gross non-MNA)
3. Customer-group `$250k` FCS / `$2m` SME aggregation & Att A Para 42 term-deposit pool tainting (`window` partitionBy)
4. LCR Para 9(e) foreign sovereign HQLA1 per-currency cap (`groupBy ["currency"]`)
5. Att A Table 3 Row 11(d) 24-month rolling 30-day collateral flow lookback (`window` ordered rolling + `abs` + `agg .max`)
6. Att A Para 50 12-month monthly average net cash outflows (`agg .avg`)
-/

/-- Per-position option aggregates in days from the reporting date, for options matching `holderOk`
    and `kind`. -/
def positionOptionAggRel (cpOnly : Bool) (kind : String) (aggCol countCol : String) (op : AggOp) : RelExpr :=
  let holderOk : SExpr := if cpOnly then .binOp .eq (.col "holder") (.litStr "counterparty") else .litBool true
  .groupBy ["id"] [(countCol, .count, .col "opt_days"), (aggCol, op, .col "opt_days")]
    (.withColumns [("opt_days", .binOp .dateDiff (.col "resulting_date") (.var "reporting_date"))]
      (.filter (.binOp .and holderOk (.binOp .eq (.col "kind") (.litStr kind))) (.table "position_options")))

/-- Worst-case days (Att A paras 45 & 60, Att C paras 8 & 20): what we owe at the earliest qualifying
    early redemption, what is owed to us at the latest qualifying extension. -/
def worstCaseDaysExpr (earlyN earlyMin extN extMax : String) : SExpr :=
  let matDays := .binOp .dateDiff (.col "maturity_date") (.var "reporting_date")
  .ite (.binOp .eq (.col "side") (.litStr "we_owe"))
    (.ite (.binOp .gt (.coalesceZero (.col earlyN)) (.litRat 0 1)) (.binOp .min matDays (.col earlyMin)) matDays)
    (.ite (.binOp .gt (.coalesceZero (.col extN)) (.litRat 0 1)) (.binOp .max matDays (.col extMax)) matDays)

def positionsEnrichedRel : RelExpr :=
  let withOpts :=
    .leftJoin "id"
      (.leftJoin "id"
        (.leftJoin "id"
          (.leftJoin "id" (.table "positions") (positionOptionAggRel true "early_redemption" "lcr_early_min" "lcr_early_n" .min))
          (positionOptionAggRel true "extension" "lcr_ext_max" "lcr_ext_n" .max))
        (positionOptionAggRel false "early_redemption" "nsfr_early_min" "nsfr_early_n" .min))
      (positionOptionAggRel false "extension" "nsfr_ext_max" "nsfr_ext_n" .max)
  -- With unknown options, what we owe matures on the reporting date (0 days); what is owed to us
  -- has no established maturity (outside the LCR horizon, in the NSFR >= 1y bucket).
  -- For liabilities with known options and a withdrawal notice period (notice_days > 0), earliest
  -- redemption is min(notice_days, nonNeg wc) (Att C Para 8).
  let known := .col "options_known"
  let isWeOwe := .binOp .eq (.col "side") (.litStr "we_owe")
  let valid := .binOp .or known isWeOwe
  let resid (wc : SExpr) :=
    .ite isWeOwe
      (.ite known
        (.ite (.binOp .gt (.col "notice_days") (.litRat 0 1))
          (.binOp .min (.col "notice_days") (.unOp .nonNeg wc))
          (.unOp .nonNeg wc))
        (.litRat 0 1))
      (.binOp .max (.col "notice_days") (.ite known wc (.litRat 0 1)))
  .withColumns
    [ ("maturity_valid", valid)
    , ("lcr_residual_days", resid (worstCaseDaysExpr "lcr_early_n" "lcr_early_min" "lcr_ext_n" "lcr_ext_max"))
    , ("nsfr_residual_days", resid (worstCaseDaysExpr "nsfr_early_n" "nsfr_early_min" "nsfr_ext_n" "nsfr_ext_max"))
    , ("encumbrance_days",
        .ite (.col "is_encumbered")
          (.unOp .nonNeg (.binOp .dateDiff (.col "encumbrance_end_date") (.var "reporting_date")))
          (.litRat 0 1))
    ]
    withOpts

def derivGroupedRel : RelExpr :=
  .withColumns
    [ ("net_payable",
        .ite (.col "all_eligible") (.unOp .nonNeg (.col "net_mtm")) (.col "gross_payable"))
    , ("net_receivable",
        .ite (.col "all_eligible") (.unOp .nonNeg (.binOp .sub (.litRat 0 1) (.col "net_mtm"))) (.col "gross_receivable"))
    ]
    (.groupBy ["mna_id"]
      [ ("all_eligible", .allBool, .col "has_eligible_mna")
      , ("net_mtm", .sum, .binOp .sub (.unOp .nonNeg (.col "mtm_payable")) (.unOp .nonNeg (.col "mtm_receivable")))
      , ("gross_payable", .sum, .unOp .nonNeg (.col "mtm_payable"))
      , ("gross_receivable", .sum, .unOp .nonNeg (.col "mtm_receivable"))
      ]
      (.table "derivative_trades"))

def depositsWindowedRel : RelExpr :=
  .window
    [ ("cust_total_balance", .sum, .unOp .nonNeg (.col "balance"), { partitionBy := ["customer_group_id"] })
    , ("pool_tainted", .anyBool, .col "has_early_withdrawal_no_penalty", { partitionBy := ["product_category"] })
    ]
    (.table "deposit_accounts")

def depositsClassifiedRel : RelExpr :=
  let bal := .unOp .nonNeg (.col "balance")
  let isSmeEligible :=
    .binOp .and
      (.binOp .eq (.col "customer_type") (.litStr "sme"))
      (.binOp .and (.col "managed_as_retail")
        (.binOp .lt (.col "cust_total_balance") (.litRat 2000000 1)))
  let isRetailOrSme :=
    .binOp .or (.binOp .eq (.col "customer_type") (.litStr "retail")) isSmeEligible
  let fcsShare :=
    .binOp .div
      (.binOp .mul bal (.binOp .min (.col "cust_total_balance") (.litRat 250000 1)))
      (.col "cust_total_balance")
  .withColumns
    [ ("is_sme_eligible", isSmeEligible)
    , ("fcs_insured_amt",
        .ite isRetailOrSme (.binOp .min bal fcsShare) (.litRat 0 1))
    , ("is_excluded_term_deposit",
        .binOp .and (.binOp .gt (.col "residual_days") (.litRat 30 1)) (.unOp .not (.col "pool_tainted")))
    ]
    depositsWindowedRel

def foreignSovCappedByCcyRel : RelExpr :=
  .withColumns
    [ ("capped_hqla1",
        .binOp .min (.unOp .nonNeg (.col "total_hqla1")) (.col "cap_outflow"))
    ]
    (.groupBy ["currency"]
      [ ("total_hqla1", .sum, .unOp .nonNeg (.col "hqla1_foreign_sov_amt"))
      , ("cap_outflow", .min, .unOp .nonNeg (.col "ccy_net_outflow"))
      ]
      (.table "ccy_hqla1_positions"))

def collateralDailyInLookbackRel : RelExpr :=
  let inLookback :=
    .filter
      (.binOp .and
        (.binOp .ge (.binOp .dateDiff (.col "valuation_date") (.dateAddMonths (.var "reporting_date") (-24))) (.litRat 0 1))
        (.binOp .le (.binOp .dateDiff (.col "valuation_date") (.var "reporting_date")) (.litRat 0 1)))
      (.table "daily_collateral_flows")
  .groupBy ["valuation_date"]
    [ ("day_flow", .sum, .col "net_collateral_flow") ]
    inLookback

def collateralRolling30dRel : RelExpr :=
  let windowed :=
    .window
      [ ("back_30d_flow", .sum, .col "day_flow",
          { partitionBy := []
          , orderBy := [("valuation_date", true)]
          , precedingDays? := some 29 })
      , ("fwd_incl_30d_flow", .sum, .col "day_flow",
          { partitionBy := []
          , orderBy := [("valuation_date", true)]
          , followingDays? := some 30 }) ]
      collateralDailyInLookbackRel
  .withColumns
    [ ("abs_30d_flow",
        .binOp .max
          (.unOp .abs (.col "back_30d_flow"))
          (.ite (.binOp .ge (.binOp .dateDiff (.var "reporting_date") (.col "valuation_date")) (.litRat 30 1))
            (.unOp .abs (.binOp .sub (.col "fwd_incl_30d_flow") (.col "day_flow")))
            (.litRat 0 1))) ]
    windowed

def collateralLookbackInWindowRel : RelExpr :=
  .withColumns
    [ ("days_after_23m_anchor", .binOp .dateDiff (.col "valuation_date") (.dateAddMonths (.var "reporting_date") (-23))) ]
    (.filter
      (.binOp .and
        (.binOp .ge (.binOp .dateDiff (.col "valuation_date") (.dateAddMonths (.var "reporting_date") (-24))) (.litRat 0 1))
        (.binOp .le (.binOp .dateDiff (.col "valuation_date") (.var "reporting_date")) (.litRat 0 1)))
      (.table "daily_collateral_flows"))

def monthly12mWindowRel : RelExpr :=
  let start12m := .dateAddMonths (.var "reporting_date") (-12)
  -- Uniquely true iff `month_end_date` is Feb 29 (the only date where adding -12m then +12m
  -- shifts by -1 day) AND `start12m` is Feb 28 of that same leap year (`dateDiff == 1`).
  let isLeapFeb29Excl :=
    .binOp .and
      (.binOp .eq
        (.binOp .dateDiff
          (.dateAddMonths (.dateAddMonths (.col "month_end_date") (-12)) 12)
          (.col "month_end_date"))
        (.litRat (-1) 1))
      (.binOp .eq (.binOp .dateDiff (.col "month_end_date") start12m) (.litRat 1 1))
  .filter
    (.binOp .and
      (.binOp .gt (.binOp .dateDiff (.col "month_end_date") start12m) (.litRat 0 1))
      (.binOp .and
        (.binOp .le (.binOp .dateDiff (.col "month_end_date") (.var "reporting_date")) (.litRat 0 1))
        (.unOp .not isLeapFeb29Excl)))
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
    , ("BB+", .litRat 4 1)
    , ("BB", .litRat 4 1)
    , ("BB-", .litRat 4 1)
    , ("B+", .litRat 5 1)
    , ("B", .litRat 5 1)
    , ("B-", .litRat 5 1)
    , ("CCC", .litRat 6 1)
    , ("below_b_minus", .litRat 6 1)
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
  -- Att A Para 8: Symmetric leg unwind for short-term HQLA-for-HQLA SFTs (no APS 110 Att D suppression).
  .withColumns
    [ ("adj_hqla1", .col "hqla1_delta")
    , ("adj_hqla2a", .col "hqla2a_delta")
    , ("adj_hqla2b", .col "hqla2b_delta")
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
  let graceOk := .binOp .and
    (.var "clf_recently_ineligible_days_known")
    (.binOp .le (.var "clf_recently_ineligible_days") (.litRat 30 1))
  let boundC :=
    .binOp .add
      (.unOp .nonNeg (.var "clf_collateral_after_margin"))
      (.ite graceOk (.unOp .nonNeg (.var "clf_recently_ineligible_collateral_after_margin")) (.litRat 0 1))
  let drawdownOk := .binOp .or
    (.unOp .not (.var "clf_drawn_non_intraday"))
    (.var "clf_drawdown_attested_notified")
  let clfEligible :=
    .binOp .and (.var "clf_approved_by_apra")
      (.binOp .and (.binOp .eq (.var "clf_collateral_operational_met") (.litStr "true"))
      (.binOp .and (.var "clf_collateral_diversified")
        (.binOp .and (.var "clf_annual_reapproval_current")
          (.binOp .and (.var "clf_included_in_funding_strategy")
            (.binOp .and (.var "clf_balance_sheet_efforts_exhausted") drawdownOk)))))
  .ite clfEligible
    (.binOp .min boundA (.binOp .min boundB boundC))
    (.litRat 0 1)

def upstreamProgram : Program :=
  { name := "evaluate_aps210_upstream"
  , tableParams :=
      [ ("positions", ["id", "side", "amount", "maturity_date", "options_known", "notice_days", "encumbrance_end_date", "is_encumbered"])
      , ("position_options", ["id", "holder", "kind", "resulting_date"])
      , ("derivative_trades", ["trade_id", "mna_id", "has_eligible_mna", "mtm_payable", "mtm_receivable"])
      , ("deposit_accounts", ["account_id", "customer_group_id", "customer_type", "managed_as_retail", "product_category", "balance", "has_early_withdrawal_no_penalty", "residual_days"])
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
      , ("clf_approved_by_apra", .bool)
      , ("clf_contractual", .rat)
      , ("clf_pledged_zero_runoff", .rat)
      , ("clf_received_zero_inflow", .rat)
      , ("clf_collateral_after_margin", .rat)
      , ("clf_recently_ineligible_collateral_after_margin", .rat)
      , ("clf_collateral_diversified", .bool)
      , ("clf_annual_reapproval_current", .bool)
      , ("clf_included_in_funding_strategy", .bool)
      , ("clf_balance_sheet_efforts_exhausted", .bool)
      , ("clf_drawn_non_intraday", .bool)
      , ("clf_drawdown_attested_notified", .bool)
      , ("clf_collateral_operational_met", .str)
      , ("clf_recently_ineligible_days_known", .bool)
      , ("clf_recently_ineligible_days", .rat)
      ]
  , lets :=
      [ ("six_months_days", .scalar (.binOp .dateDiff (.dateAddMonths (.var "reporting_date") 6) (.var "reporting_date")))
      , ("one_year_days", .scalar (.binOp .dateDiff (.dateAddMonths (.var "reporting_date") 12) (.var "reporting_date")))
      , ("pos_lcr_within_30d_amt",
          .agg .sum
            (.filter (.binOp .and (.col "maturity_valid") (.binOp .le (.col "lcr_residual_days") (.litRat 30 1)))
              positionsEnrichedRel)
            (.col "amount"))
      , ("pos_nsfr_lt_6m_amt",
          .agg .sum
            (.filter (.binOp .and (.col "maturity_valid") (.binOp .lt (.col "nsfr_residual_days") (.var "six_months_days")))
              positionsEnrichedRel)
            (.col "amount"))
      , ("pos_nsfr_6m_to_1y_amt",
          .agg .sum
            (.filter
              (.binOp .and (.col "maturity_valid")
                (.binOp .and
                  (.binOp .ge (.col "nsfr_residual_days") (.var "six_months_days"))
                  (.binOp .lt (.col "nsfr_residual_days") (.var "one_year_days"))))
              positionsEnrichedRel)
            (.col "amount"))
      , ("pos_nsfr_ge_1y_amt",
          .agg .sum
            (.filter (.binOp .or (.unOp .not (.col "maturity_valid"))
                (.binOp .ge (.col "nsfr_residual_days") (.var "one_year_days")))
              positionsEnrichedRel)
            (.col "amount"))
      , ("pos_enc_ge_1y_amt",
          .agg .sum
            (.filter
              (.binOp .and (.col "is_encumbered") (.binOp .ge (.col "encumbrance_days") (.var "one_year_days")))
              positionsEnrichedRel)
            (.col "amount"))
      , ("deriv_net_outflow", .agg .sum derivGroupedRel (.col "net_payable"))
      , ("deriv_net_inflow", .agg .sum derivGroupedRel (.col "net_receivable"))
      , ("sme_eligible_deposits",
          .agg .sum (.filter (.col "is_sme_eligible") depositsClassifiedRel) (.unOp .nonNeg (.col "balance")))
      , ("fcs_insured_deposits",
          .agg .sum depositsClassifiedRel (.col "fcs_insured_amt"))
      , ("excluded_term_deposits",
          .agg .sum (.filter (.col "is_excluded_term_deposit") depositsClassifiedRel) (.unOp .nonNeg (.col "balance")))
      , ("capped_foreign_sov_hqla1",
          .agg .sum foreignSovCappedByCcyRel (.col "capped_hqla1"))
      , ("max_30d_collateral_window",
          .agg .max collateralRolling30dRel (.col "abs_30d_flow"))
      , ("tail_30d_collateral_sum",
          .agg .sum
            (.filter
              (.binOp .le (.binOp .dateDiff (.var "reporting_date") (.col "valuation_date")) (.litRat 29 1))
              collateralDailyInLookbackRel)
            (.col "day_flow"))
      , ("max_30d_collateral_outflow",
          .scalar (.binOp .max (.var "max_30d_collateral_window") (.unOp .abs (.var "tail_30d_collateral_sum"))))
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
      , ("side", .str (match p.side with | .weOwe => "we_owe" | .owedToUs => "owed_to_us"))
      , ("amount", .rat p.amount)
      , ("maturity_date", .date p.maturityDate)
      , ("options_known", .bool (p.options.toOption.isSome))
      , ("notice_days", .rat p.noticeDays)
      , ("encumbrance_end_date", .date p.encumbranceEndDate)
      , ("is_encumbered", .bool p.isEncumbered)
      ]))
  , ("position_options", input.positions.flatMap (fun p =>
      (p.options.resolve []).map (fun o =>
        [ ("id", .str p.id)
        , ("holder", .str (match o.holder with | .us => "us" | .counterparty => "counterparty"))
        , ("kind", .str (match o.kind with | .earlyRedemption => "early_redemption" | .extension => "extension"))
        , ("resulting_date", .date o.resultingSettlementDate) ])))
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
      , ("managed_as_retail", .bool a.managedAsRetail)
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
  , ("clf_approved_by_apra", .bool input.clf.approvedByApra)
  , ("clf_contractual", .rat input.clf.contractualAmount)
  , ("clf_pledged_zero_runoff", .rat input.clf.pledgedZeroRunoff)
  , ("clf_received_zero_inflow", .rat input.clf.receivedZeroInflow)
  , ("clf_collateral_after_margin", .rat input.clf.collateralAfterMargin)
  , ("clf_recently_ineligible_collateral_after_margin", .rat input.clf.recentlyIneligibleCollateralAfterMargin)
  , ("clf_collateral_diversified", .bool input.clf.collateralDiversified)
  , ("clf_annual_reapproval_current", .bool input.clf.annualReapprovalCurrent)
  , ("clf_included_in_funding_strategy", .bool input.clf.includedInFundingStrategy)
  , ("clf_balance_sheet_efforts_exhausted", .bool input.clf.balanceSheetEffortsExhausted)
  , ("clf_drawn_non_intraday", .bool input.clf.clfDrawnNonIntraday)
  , ("clf_drawdown_attested_notified", .bool input.clf.drawdownAttestedAndNotifiedToApra)
  , ("clf_collateral_operational_met", .str (match input.clf.collateralOperationalMet with
      | .known true => "true" | .known false => "false" | .unknown _ => "unknown" | .refused _ => "refused"))
  , ("clf_recently_ineligible_days_known", .bool input.clf.recentlyIneligibleCollateralDays.toOption.isSome)
  , ("clf_recently_ineligible_days", .rat ((input.clf.recentlyIneligibleCollateralDays.resolve 0 : Nat) : Rat))
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

private def obs0 : ObservationContext :=
  { validAt := civilDate 2026 6 30, knownAt := civilDate 2026 7 2 * 86400, sourceReference := "smoke" }

private def cpCall (d : APS210.Upstream.EpochDate) : ContractOption :=
  { holder := .counterparty, kind := .earlyRedemption, exerciseDate := d, resultingSettlementDate := d }

def smokeInput : UpstreamInput :=
  { asOf := obs0
  , positions :=
      [ { id := "p1", side := .weOwe, amount := 100, maturityDate := civilDate 2026 7 15, options := .known [], noticeDays := 0, encumbranceEndDate := civilDate 2026 6 30, isEncumbered := false, observation := obs0 }
      , { id := "p2", side := .weOwe, amount := 200, maturityDate := civilDate 2027 7 15, options := .known [cpCall (civilDate 2026 9 1)], noticeDays := 0, encumbranceEndDate := civilDate 2026 6 30, isEncumbered := false, observation := obs0 }
      , { id := "p3", side := .weOwe, amount := 300, maturityDate := civilDate 2027 1 15, options := .known [], noticeDays := 0, encumbranceEndDate := civilDate 2027 8 1, isEncumbered := true, observation := obs0 }
      , { id := "p4", side := .weOwe, amount := 400, maturityDate := civilDate 2026 7 5, options := .known [], noticeDays := 400, encumbranceEndDate := civilDate 2026 6 30, isEncumbered := false, observation := obs0 }
      -- Our own call on a liability counts for the NSFR (earliest redemption) but not the LCR.
      , { id := "p5", side := .weOwe, amount := 50, maturityDate := civilDate 2027 9 1, options := .known [{ cpCall (civilDate 2026 7 10) with holder := .us }], noticeDays := 0, encumbranceEndDate := civilDate 2026 6 30, isEncumbered := false, observation := obs0 }
      -- A row with both a counterparty call and an extension: what we owe settles at the call.
      , { id := "p6", side := .weOwe, amount := 60, maturityDate := civilDate 2026 12 1, options := .known [cpCall (civilDate 2026 7 20), { holder := .counterparty, kind := .extension, exerciseDate := civilDate 2026 11 1, resultingSettlementDate := civilDate 2028 1 1 }], noticeDays := 0, encumbranceEndDate := civilDate 2026 6 30, isEncumbered := false, observation := obs0 }
      -- An asset the counterparty can extend: owed to us settles at the latest extension.
      , { id := "p7", side := .owedToUs, amount := 70, maturityDate := civilDate 2026 7 10, options := .known [{ holder := .counterparty, kind := .extension, exerciseDate := civilDate 2026 7 1, resultingSettlementDate := civilDate 2027 8 1 }], noticeDays := 0, encumbranceEndDate := civilDate 2026 6 30, isEncumbered := false, observation := obs0 }
      -- An asset with an unknown option schedule: outside the LCR horizon, NSFR >= 1y.
      , { id := "p8", side := .owedToUs, amount := 80, maturityDate := civilDate 2026 7 10, options := .unknown "option schedule not loaded", noticeDays := 0, encumbranceEndDate := civilDate 2026 6 30, isEncumbered := false, observation := obs0 }
      ]
  , derivativeTrades :=
      [ { observation := obs0, tradeId := "t1", mnaId := "mna1", hasEligibleMna := true, mtmPayable := 100, mtmReceivable := 40 }
      , { observation := obs0, tradeId := "t2", mnaId := "mna1", hasEligibleMna := true, mtmPayable := 20, mtmReceivable := 50 }
      , { observation := obs0, tradeId := "t3", mnaId := "mna2", hasEligibleMna := true, mtmPayable := 10, mtmReceivable := 60 }
      , { observation := obs0, tradeId := "t4", mnaId := "none", hasEligibleMna := false, mtmPayable := 25, mtmReceivable := 10 }
      ]
  , depositAccounts :=
      [ { observation := obs0, accountId := "a1", customerGroupId := "cg1", customerType := .retail, productCategory := "prodA", balance := 200000, hasEarlyWithdrawalNoPenalty := true, residualDays := 10 }
      , { observation := obs0, accountId := "a2", customerGroupId := "cg1", customerType := .retail, productCategory := "prodA", balance := 300000, hasEarlyWithdrawalNoPenalty := false, residualDays := 60 }
      , { observation := obs0, accountId := "a3", customerGroupId := "cg2", customerType := .sme, productCategory := "prodB", balance := 1500000, hasEarlyWithdrawalNoPenalty := false, residualDays := 60 }
      , { observation := obs0, accountId := "a4", customerGroupId := "cg3", customerType := .sme, productCategory := "prodB", balance := 2500000, hasEarlyWithdrawalNoPenalty := false, residualDays := 90 }
      ]
  , ccyHqla1Positions :=
      [ { observation := obs0, currency := "USD", hqla1ForeignSovAmt := 120, ccyNetOutflow := 60 }
      , { observation := obs0, currency := "USD", hqla1ForeignSovAmt := 30, ccyNetOutflow := 40 }
      , { observation := obs0, currency := "EUR", hqla1ForeignSovAmt := 40, ccyNetOutflow := 80 }
      , { observation := obs0, currency := "JPY", hqla1ForeignSovAmt := 50, ccyNetOutflow := -10 }
      ]
  , dailyCollateralFlows :=
      ((List.range 25).map (fun (m : Nat) =>
        { observation := obs0, valuationDate := addMonthsEpoch (civilDate 2024 6 30) (m : Int), netCollateralFlow := 0 })) ++
      (List.range 35).map (fun (i : Nat) =>
        { observation := obs0, valuationDate := civilDate 2026 1 1 + (i : Int)
        , netCollateralFlow := if i < 30 then 1 else -20
        })
  , monthlyNetOutflows :=
      (List.range 12).map (fun (i : Nat) =>
        { observation := obs0, monthEndDate := civilDate 2025 7 28 + ((i * 30 : Nat) : Int)
        , netCashOutflow := 100 + 10 * (i : Int)
        })
  , ecaiAssessments :=
      [ { observation := obs0, exposureId := "e1", ratingStr := "A+" }           -- 1 rating -> grade 2
      , { observation := obs0, exposureId := "e2", ratingStr := "AAA" }
      , { observation := obs0, exposureId := "e2", ratingStr := "BBB-" }         -- 2 ratings -> worse = grade 3
      , { observation := obs0, exposureId := "e3", ratingStr := "AA+" }
      , { observation := obs0, exposureId := "e3", ratingStr := "A" }
      , { observation := obs0, exposureId := "e3", ratingStr := "below_bbb_minus" } -- 3 ratings -> 2nd best = grade 2
      , { observation := obs0, exposureId := "e4", ratingStr := "unrated" }      -- 0 rated -> excluded
      ]
  , amortisationCashflows :=
      [ { observation := obs0, loanId := "L1", paymentDate := civilDate 2026 9 1, principalAmount := 50 }
      , { observation := obs0, loanId := "L1", paymentDate := civilDate 2027 3 1, principalAmount := 75 }
      , { observation := obs0, loanId := "L1", paymentDate := civilDate 2028 6 30, principalAmount := 125 }
      ]
  , sftLegs :=
      [ { observation := obs0, legId := "s1", meetsOperationalReqs := true, nettingEligible := true, hqla1Delta := 20, hqla2aDelta := -5, hqla2bDelta := 0 }
      , { observation := obs0, legId := "s2", meetsOperationalReqs := true, nettingEligible := false, hqla1Delta := 15, hqla2aDelta := -10, hqla2bDelta := 3 }
      , { observation := obs0, legId := "s3", meetsOperationalReqs := false, nettingEligible := true, hqla1Delta := 100, hqla2aDelta := 0, hqla2bDelta := 0 }
      ]
  , clf :=
      { approvedByApra := true
      , contractualAmount := 500
      , pledgedZeroRunoff := 80
      , receivedZeroInflow := 20
      , collateralAfterMargin := 460
      , collateralDiversified := true
      , annualReapprovalCurrent := true
      , includedInFundingStrategy := true
      , balanceSheetEffortsExhausted := true
      , collateralOperationalMet := .known true
      , recentlyIneligibleCollateralDays := .unknown "no recently ineligible collateral"
      , observation := obs0
      }
  }

theorem upstream_eval_matches_domain :
    evalProgram upstreamProgram (upstreamInputToTableEnv smokeInput) (upstreamInputToScalars smokeInput) =
      upstreamResultToRow (evaluateUpstream smokeInput) := by
  native_decide

end Lean2Ibis.UpstreamSpec
