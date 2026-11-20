import APS210.CashFlows
import Lean2Ibis.IR

namespace Lean2Ibis.CashFlowsSpec

open APS210
open APS210.Facts
open APS210.CashFlows

/-- Per-flow LCR option aggregates (Att A paras 45 & 60): counterparty early-redemption and extension
    options, keyed by flow `id`. -/
def optionAggRel (kind : String) (aggCol countCol : String) (op : AggOp) : RelExpr :=
  .groupBy ["id"] [(countCol, .count, .col "resulting_days"), (aggCol, op, .col "resulting_days")]
    (.filter (.binOp .and (.binOp .eq (.col "holder") (.litStr "counterparty")) (.binOp .eq (.col "kind") (.litStr kind)))
      (.table "cash_flow_options"))

/-- Worst-case LCR settlement day from contractual days and the joined option aggregates. -/
def wcRawDaysExpr : SExpr :=
  .ite (.binOp .eq (.col "side") (.litStr "we_owe"))
    (.ite (.binOp .gt (.coalesceZero (.col "n_early")) (.litRat 0 1))
      (.binOp .min (.col "settlement_days") (.col "min_early")) (.col "settlement_days"))
    (.ite (.binOp .gt (.coalesceZero (.col "n_ext")) (.litRat 0 1))
      (.binOp .max (.col "settlement_days") (.col "max_ext")) (.col "settlement_days"))

def horizonRankExpr (daysExpr : SExpr) : SExpr :=
  .ite (.binOp .eq daysExpr (.litRat 0 1)) (.litRat 0 1)
    (.ite (.binOp .le daysExpr (.litRat 7 1)) (.litRat 1 1)
      (.ite (.binOp .le daysExpr (.litRat 30 1)) (.litRat 2 1)
        (.ite (.binOp .lt daysExpr (.litRat 180 1)) (.litRat 3 1)
          (.ite (.binOp .lt daysExpr (.litRat 365 1)) (.litRat 4 1)
            (.litRat 5 1)))))

/-- Att A Paras 31, 54, 60, 66 over a row carrying `wc_known` / `wc_days`. -/
def isEligibleLcrInflowExpr : SExpr :=
  .binOp .and (.binOp .eq (.col "side") (.litStr "owed_to_us"))
    (.binOp .and (.col "wc_known")
      (.binOp .and (.binOp .le (.col "wc_days") (.litRat 30 1))
        (.binOp .and
          (.binOp .or (.unOp .not (.col "is_open_maturity")) (.col "is_minimum_contractual_payment"))
          (.binOp .and (.unOp .not (.col "is_non_financial_revenue"))
            (.binOp .and (.unOp .not (.col "counted_in_hqla"))
              (.binOp .eq (.col "performing") (.litStr "true")))))))

/-- Att A Para 45 over a row carrying `wc_days` (0 when what we owe has an unknown date or options). -/
def isLcrOutflowWithin30dExpr : SExpr :=
  .binOp .and (.binOp .eq (.col "side") (.litStr "we_owe"))
    (.binOp .le (.col "wc_days") (.litRat 30 1))

def isLadderValidExpr : SExpr :=
  .binOp .or (.binOp .eq (.col "side") (.litStr "we_owe"))
    (.binOp .and (.col "wc_known")
      (.binOp .and (.binOp .eq (.col "performing") (.litStr "true"))
        (.binOp .and (.unOp .not (.col "is_non_financial_revenue"))
          (.binOp .and (.unOp .not (.col "counted_in_hqla"))
            (.binOp .or (.unOp .not (.col "is_open_maturity")) (.col "is_minimum_contractual_payment"))))))

def enrichedCashFlowsRel : RelExpr :=
  .withColumns
    [ ("is_elig_lcr_inflow", isEligibleLcrInflowExpr)
    , ("is_lcr_outflow_30d", isLcrOutflowWithin30dExpr)
    , ("is_ladder_valid", isLadderValidExpr)
    , ("horizon_rank", horizonRankExpr (.col "wc_days"))
    , ("nonneg_amt", .unOp .nonNeg (.col "amount"))
    , ("signed_amt",
        .ite (.binOp .eq (.col "side") (.litStr "owed_to_us"))
          (.unOp .nonNeg (.col "amount"))
          (.binOp .sub (.litRat 0 1) (.unOp .nonNeg (.col "amount"))))
    ]
    (.withColumns
      [ ("wc_known",
          .binOp .and (.binOp .and (.col "settlement_known") (.col "options_known"))
            (.binOp .or (.binOp .eq (.col "side") (.litStr "we_owe"))
              (.binOp .ge wcRawDaysExpr (.litRat 0 1))))
      , ("wc_days", .ite (.binOp .and (.col "settlement_known") (.col "options_known"))
          (.unOp .nonNeg wcRawDaysExpr) (.litRat 0 1)) ]
      (.leftJoin "id"
        (.leftJoin "id" (.table "cash_flows") (optionAggRel "early_redemption" "min_early" "n_early" .min))
        (optionAggRel "extension" "max_ext" "n_ext" .max)))

def cashFlowLadderRel : RelExpr :=
  .window
    [ ("cum_net_cash_flow", .sum, .col "net_cash_flow",
        { partitionBy := []
        , orderBy := [("horizon_rank", true)]
        , preceding? := none
        , following? := some 0 }) ]
    (.groupBy ["horizon_rank"]
      [ ("net_cash_flow", .sum, .col "signed_amt") ]
      (.filter
        (.binOp .and (.col "is_ladder_valid")
          (.binOp .or (.unOp .not (.var "use_currency_filter"))
            (.binOp .eq (.col "currency") (.var "target_currency"))))
        enrichedCashFlowsRel))

/-- Derivative payments inside the 30-day horizon (Att A fn 5 & 8). -/
def derivPaymentsRel : RelExpr :=
  .filter
    (.binOp .and (.binOp .eq (.col "source") (.litStr "derivative"))
      (.binOp .or
        (.binOp .and (.binOp .eq (.col "side") (.litStr "we_owe")) (.col "is_lcr_outflow_30d"))
        (.binOp .and (.binOp .eq (.col "side") (.litStr "owed_to_us")) (.col "is_elig_lcr_inflow"))))
    enrichedCashFlowsRel

def isMnaNettableExpr : SExpr :=
  .col "has_valid_mna"

def isFxSameDayNettableExpr : SExpr :=
  .binOp .and (.unOp .not (.col "has_valid_mna"))
    (.binOp .and (.col "is_same_day_fx_principal_exchange") (.col "wc_known"))

def isNettableExpr : SExpr :=
  .binOp .or isMnaNettableExpr isFxSameDayNettableExpr

/-- Collateral movements within 30 days: what we owe with an unknown date is due now; what is owed to us
    needs a known future date (`0 <= settlement_days <= 30`). -/
def liveCollateralRel : RelExpr :=
  .filter
    (.ite (.col "settlement_known")
      (.ite (.binOp .eq (.col "side") (.litStr "we_owe"))
        (.binOp .le (.col "settlement_days") (.litRat 30 1))
        (.binOp .and (.binOp .ge (.col "settlement_days") (.litRat 0 1))
          (.binOp .le (.col "settlement_days") (.litRat 30 1))))
      (.binOp .eq (.col "side") (.litStr "we_owe")))
    (.table "collateral")

/-- Collateral owed to us offsets outflows only when known eligible and known not already in HQLA. -/
def offsetsOutflowsExpr : SExpr :=
  .binOp .and (.binOp .eq (.col "side") (.litStr "owed_to_us"))
    (.binOp .and (.binOp .eq (.col "eligible_for_offset") (.litStr "true"))
      (.binOp .eq (.col "already_counted_in_hqla") (.litStr "false")))

def nettedSetsRel : RelExpr :=
  .groupBy ["netting_set_id", "currency"]
    [ ("pay_signed", .sum, .ite (.binOp .eq (.col "side") (.litStr "we_owe"))
        (.col "nonneg_amt") (.binOp .sub (.litRat 0 1) (.col "nonneg_amt")))
    , ("n_pay", .count, .col "nonneg_amt") ]
    (.filter isMnaNettableExpr derivPaymentsRel)

def fxNettedRel : RelExpr :=
  .withColumns
    [ ("fx_net_outflow", .ite (.binOp .ge (.col "fx_signed") (.litRat 0 1)) (.col "fx_signed") (.litRat 0 1))
    , ("fx_net_inflow", .ite (.binOp .lt (.col "fx_signed") (.litRat 0 1)) (.binOp .sub (.litRat 0 1) (.col "fx_signed")) (.litRat 0 1)) ]
    (.groupBy ["netting_set_id", "wc_days"]
      [ ("fx_signed", .sum, .ite (.binOp .eq (.col "side") (.litStr "we_owe"))
          (.col "nonneg_amt") (.binOp .sub (.litRat 0 1) (.col "nonneg_amt"))) ]
      (.filter isFxSameDayNettableExpr derivPaymentsRel))

def derivNettedRel : RelExpr :=
  .withColumns
    [ ("net_outflow", .ite (.binOp .ge (.col "base") (.litRat 0 1))
        (.unOp .nonNeg (.binOp .sub (.col "base") (.coalesceZero (.col "coll_offset")))) (.litRat 0 1))
    , ("net_inflow", .ite (.binOp .lt (.col "base") (.litRat 0 1))
        (.binOp .sub (.litRat 0 1) (.col "base")) (.litRat 0 1)) ]
    (.withColumns
      [ ("base", .binOp .add (.col "pay_signed") (.coalesceZero (.col "coll_owe"))) ]
      (.leftJoinOn ["netting_set_id", "currency"] nettedSetsRel
        (.groupBy ["netting_set_id", "currency"]
          [ ("coll_owe", .sum, .ite (.binOp .eq (.col "side") (.litStr "we_owe")) (.unOp .nonNeg (.col "amount")) (.litRat 0 1))
          , ("coll_offset", .sum, .ite offsetsOutflowsExpr (.unOp .nonNeg (.col "amount")) (.litRat 0 1)) ]
          liveCollateralRel)))

def derivGrossOutRel : RelExpr :=
  .withColumns
    [ ("gross_outflow", .unOp .nonNeg (.binOp .sub (.col "nonneg_amt") (.coalesceZero (.col "linked_offset")))) ]
    (.leftJoin "id"
      (.filter (.binOp .and (.unOp .not isNettableExpr) (.binOp .eq (.col "side") (.litStr "we_owe"))) derivPaymentsRel)
      (.groupBy ["id"] [("linked_offset", .sum, .unOp .nonNeg (.col "amount"))]
        (.withColumns [("id", .col "obligation_id")] (.filter offsetsOutflowsExpr liveCollateralRel))))

/-- Collateral we owe in a netting set with no netted payments is an outflow on its own. -/
def collateralOutRel : RelExpr :=
  .filter (.binOp .le (.coalesceZero (.col "n_pay")) (.litRat 0 1))
    (.leftJoinOn ["netting_set_id", "currency"]
      (.filter (.binOp .eq (.col "side") (.litStr "we_owe")) liveCollateralRel)
      nettedSetsRel)

/-- Complete APS 210 Cash-Flow Engine program in the Lean-to-Ibis IR. -/
def cashFlowsProgram : Program :=
  { name := "evaluate_aps210_cashflows"
  , tableParams :=
      [ ("cash_flows",
          [ "id", "currency", "side", "amount", "settlement_known", "settlement_days", "options_known"
          , "performing", "is_open_maturity", "is_minimum_contractual_payment"
          , "is_non_financial_revenue", "counted_in_hqla", "source"
          , "netting_set_id", "has_valid_mna", "is_same_day_fx_principal_exchange" ])
      , ("cash_flow_options", ["id", "holder", "kind", "resulting_days"])
      , ("collateral",
          [ "obligation_id", "netting_set_id", "side", "amount", "currency", "settlement_known"
          , "settlement_days", "eligible_for_offset", "already_counted_in_hqla" ])
      , ("nsfr_liability_legs", ["id", "amount", "days"])
      , ("nsfr_loan_legs", ["id", "amount", "days"])
      ]
  , scalarParams :=
      [ ("reporting_date", .date)
      , ("initial_liquid_buffer", .rat)
      , ("use_currency_filter", .bool)
      , ("target_currency", .str)
      ]
  , lets :=
      [ ("six_months_days", .scalar (.binOp .dateDiff (.dateAddMonths (.var "reporting_date") 6) (.var "reporting_date")))
      , ("one_year_days", .scalar (.binOp .dateDiff (.dateAddMonths (.var "reporting_date") 12) (.var "reporting_date")))
      , ("min_cum_net_cf", .agg .min cashFlowLadderRel (.col "cum_net_cash_flow"))
      , ("survival_buffer_met", .scalar
          (.binOp .ge
            (.binOp .add (.unOp .nonNeg (.var "initial_liquid_buffer")) (.var "min_cum_net_cf"))
            (.litRat 0 1)))
      , ("sched_outflow_30d",
          .agg .sum
            (.filter
              (.binOp .and (.col "is_lcr_outflow_30d")
                (.binOp .ne (.col "source") (.litStr "derivative")))
              enrichedCashFlowsRel)
            (.col "nonneg_amt"))
      , ("spv_outflow_30d",
          .agg .sum
            (.filter
              (.binOp .and (.col "is_lcr_outflow_30d")
                (.binOp .eq (.col "source") (.litStr "securitisation_spv")))
              enrichedCashFlowsRel)
            (.col "nonneg_amt"))
      , ("sched_inflow_30d",
          .agg .sum
            (.filter
              (.binOp .and (.col "is_elig_lcr_inflow")
                (.binOp .ne (.col "source") (.litStr "derivative")))
              enrichedCashFlowsRel)
            (.col "nonneg_amt"))
      , ("deriv_net_outflow", .agg .sum derivNettedRel (.col "net_outflow"))
      , ("deriv_net_inflow", .agg .sum derivNettedRel (.col "net_inflow"))
      , ("deriv_fx_net_outflow", .agg .sum fxNettedRel (.col "fx_net_outflow"))
      , ("deriv_fx_net_inflow", .agg .sum fxNettedRel (.col "fx_net_inflow"))
      , ("deriv_gross_outflow", .agg .sum derivGrossOutRel (.col "gross_outflow"))
      , ("deriv_gross_inflow", .agg .sum
          (.filter (.binOp .and (.unOp .not isNettableExpr) (.binOp .eq (.col "side") (.litStr "owed_to_us"))) derivPaymentsRel)
          (.col "nonneg_amt"))
      , ("deriv_collateral_outflow", .agg .sum collateralOutRel (.unOp .nonNeg (.col "amount")))
      , ("lcr_deriv_outflow", .scalar (.binOp .add (.var "deriv_net_outflow")
          (.binOp .add (.var "deriv_fx_net_outflow")
            (.binOp .add (.var "deriv_gross_outflow") (.var "deriv_collateral_outflow")))))
      , ("lcr_deriv_inflow", .scalar (.binOp .add (.var "deriv_net_inflow")
          (.binOp .add (.var "deriv_fx_net_inflow") (.var "deriv_gross_inflow"))))
      , ("nsfr_asf_lt6m",
          .agg .sum
            (.filter (.binOp .lt (.col "days") (.var "six_months_days")) (.table "nsfr_liability_legs"))
            (.unOp .nonNeg (.col "amount")))
      , ("nsfr_asf_6m_to_1y",
          .agg .sum
            (.filter
              (.binOp .and
                (.binOp .ge (.col "days") (.var "six_months_days"))
                (.binOp .lt (.col "days") (.var "one_year_days")))
              (.table "nsfr_liability_legs"))
            (.unOp .nonNeg (.col "amount")))
      , ("nsfr_asf_ge1y",
          .agg .sum
            (.filter (.binOp .ge (.col "days") (.var "one_year_days")) (.table "nsfr_liability_legs"))
            (.unOp .nonNeg (.col "amount")))
      , ("nsfr_loan_lt1y",
          .agg .sum
            (.filter (.binOp .lt (.col "days") (.var "one_year_days")) (.table "nsfr_loan_legs"))
            (.unOp .nonNeg (.col "amount")))
      , ("nsfr_loan_ge1y",
          .agg .sum
            (.filter (.binOp .ge (.col "days") (.var "one_year_days")) (.table "nsfr_loan_legs"))
            (.unOp .nonNeg (.col "amount")))
      ]
  , outputs :=
      [ ("min_cum_net_cf", .var "min_cum_net_cf")
      , ("survival_buffer_met", .var "survival_buffer_met")
      , ("sched_outflow_30d", .var "sched_outflow_30d")
      , ("spv_outflow_30d", .var "spv_outflow_30d")
      , ("sched_inflow_30d", .var "sched_inflow_30d")
      , ("lcr_deriv_outflow", .var "lcr_deriv_outflow")
      , ("lcr_deriv_inflow", .var "lcr_deriv_inflow")
      , ("nsfr_asf_lt6m", .var "nsfr_asf_lt6m")
      , ("nsfr_asf_6m_to_1y", .var "nsfr_asf_6m_to_1y")
      , ("nsfr_asf_ge1y", .var "nsfr_asf_ge1y")
      , ("nsfr_loan_lt1y", .var "nsfr_loan_lt1y")
      , ("nsfr_loan_ge1y", .var "nsfr_loan_ge1y")
      ]
  }

-- ============================================================================
-- Lean-to-Ibis IR Equivalence Proofs for CashFlows
-- ============================================================================

def sideToStr : Side → String
  | .weOwe => "we_owe"
  | .owedToUs => "owed_to_us"

def readingBoolToStr : Reading Bool → String
  | .known true => "true"
  | .known false => "false"
  | .unknown _ => "unknown"
  | .refused _ => "refused"

/-- A cash-flow row with its worst-case LCR settlement day, as computed by `enrichedCashFlowsRel`. -/
def cashFlowRow (f : ContractualCashFlow) : Row :=
  [ ("side", .str (sideToStr f.side))
  , ("wc_known", .bool (f.worstCaseDays .lcr).isSome)
  , ("wc_days", .rat (((f.worstCaseDays .lcr).getD 0 : Nat) : Rat))
  , ("is_open_maturity", .bool f.isOpenMaturity)
  , ("is_minimum_contractual_payment", .bool f.isMinimumContractualPayment)
  , ("is_non_financial_revenue", .bool f.isNonFinancialRevenue)
  , ("counted_in_hqla", .bool f.countedInHqla)
  , ("performing", .str (readingBoolToStr f.obligation.performing))
  ]

private theorem nat_rat_le (a b : Nat) : ((a : Rat) ≤ (b : Rat)) ↔ a ≤ b := by
  change (((a : Int) : Rat) ≤ ((b : Int) : Rat)) ↔ a ≤ b
  rw [Rat.intCast_le_intCast, Int.ofNat_le]

private theorem le30 (n : Nat) : ((n : Rat) ≤ 30 / 1) ↔ n ≤ 30 := by
  have h : (30 : Rat) / 1 = ((30 : Nat) : Rat) := by native_decide
  rw [h]; exact nat_rat_le n 30

private theorem str_ne1 : (Val.str "owed_to_us" == Val.str "we_owe") = false := by decide
private theorem str_ne2 : (Val.str "we_owe" == Val.str "owed_to_us") = false := by decide
private theorem side_ne1 : (Side.owedToUs == Side.weOwe) = false := by decide
private theorem side_ne2 : (Side.weOwe == Side.owedToUs) = false := by decide

private theorem weOwe_days_some (f : ContractualCashFlow) (h : f.side = .weOwe) :
    (f.worstCaseDays .lcr).isSome = true := by
  simp only [ContractualCashFlow.worstCaseDays, Obligation.worstCaseSettlementDate,
    ContractualCashFlow.side] at h ⊢
  rw [h]
  cases f.obligation.settlementDate <;> cases f.obligation.optionality <;> rfl

private theorem affirmative_eq_str (r : Reading Bool) :
    r.affirmative = (Val.str (readingBoolToStr r) == Val.str "true") := by
  cases r with
  | known b => cases b <;> decide
  | unknown _ => simp [Reading.affirmative, readingBoolToStr]
  | refused _ => simp [Reading.affirmative, readingBoolToStr]

theorem isLcrOutflowWithin30d_ir_equiv (f : ContractualCashFlow) :
    evalSExpr (cashFlowRow f) isLcrOutflowWithin30dExpr = .bool f.isLcrOutflowWithin30d := by
  simp only [isLcrOutflowWithin30dExpr, ContractualCashFlow.isLcrOutflowWithin30d, evalSExpr,
    cashFlowRow, Row.get, List.find?, Val.asRat, Val.asBool]
  cases hs : f.side with
  | weOwe =>
    have hsome := weOwe_days_some f hs
    cases hd : f.worstCaseDays .lcr with
    | none => simp [hd] at hsome
    | some d => simp (config := { decide := true }) [sideToStr, le30]
  | owedToUs => simp [sideToStr, str_ne1, side_ne1]

theorem isEligibleLcrInflow_ir_equiv (f : ContractualCashFlow) :
    evalSExpr (cashFlowRow f) isEligibleLcrInflowExpr = .bool f.isEligibleLcrInflow := by
  simp only [isEligibleLcrInflowExpr, ContractualCashFlow.isEligibleLcrInflow, evalSExpr,
    cashFlowRow, Row.get, List.find?, Val.asRat, Val.asBool]
  rw [affirmative_eq_str]
  cases hs : f.side with
  | weOwe => simp [sideToStr, str_ne2, side_ne2]
  | owedToUs =>
    cases hd : f.worstCaseDays .lcr with
    | none => simp [sideToStr]
    | some d => simp [sideToStr, le30, Bool.and_assoc]

-- ============================================================================
-- End-to-end: the same portfolio through the IR and through the Lean domain
-- ============================================================================

private def cfRow (id side : String) (amt days : Rat) (source : String := "standard")
    (ns : String := "") (mna fx : Bool := false) (optionsKnown : Bool := true) : Row :=
  [ ("id", .str id), ("currency", .str "AUD"), ("side", .str side), ("amount", .rat amt)
  , ("settlement_known", .bool true), ("settlement_days", .rat days), ("options_known", .bool optionsKnown)
  , ("performing", .str "true"), ("is_open_maturity", .bool false), ("is_minimum_contractual_payment", .bool false)
  , ("is_non_financial_revenue", .bool false), ("counted_in_hqla", .bool false), ("source", .str source)
  , ("netting_set_id", .str ns), ("has_valid_mna", .bool mna), ("is_same_day_fx_principal_exchange", .bool fx) ]

private def collRow (obl ns side : String) (amt : Rat) (known : Bool) (days : Rat) (elig counted : String) : Row :=
  [ ("obligation_id", .str obl), ("netting_set_id", .str ns), ("side", .str side), ("amount", .rat amt)
  , ("currency", .str "AUD"), ("settlement_known", .bool known), ("settlement_days", .rat days)
  , ("eligible_for_offset", .str elig), ("already_counted_in_hqla", .str counted) ]

/-- Portfolio: SPV and standard flows; a liability the investor can redeem on day 10; an asset the
    counterparty can extend to day 40; an asset with an unknown option schedule; MNA and same-day-FX
    derivatives; a gross derivative secured by eligible collateral; collateral we must return; and
    collateral we owe with an unknown date. -/
example :
    let tenv : TableEnv :=
      [ ("cash_flows",
          [ cfRow "out1" "we_owe" 60 5 "securitisation_spv"
          , cfRow "in1" "owed_to_us" 40 20
          , cfRow "out2" "we_owe" 50 90
          , cfRow "ext" "owed_to_us" 999 20
          , cfRow "unk" "owed_to_us" 77 5 (optionsKnown := false)
          , cfRow "d1p" "we_owe" 80 5 "derivative" "cp1" true false
          , cfRow "d1r" "owed_to_us" 30 5 "derivative" "cp1" true false
          , cfRow "d2p" "we_owe" 20 3 "derivative" "cp2" false true
          , cfRow "d2r" "owed_to_us" 50 3 "derivative" "cp2" false true
          , cfRow "g1" "we_owe" 40 5 "derivative" "cp3" ])
      , ("cash_flow_options",
          [ [("id", .str "out2"), ("holder", .str "counterparty"), ("kind", .str "early_redemption"), ("resulting_days", .rat 10)]
          , [("id", .str "ext"), ("holder", .str "counterparty"), ("kind", .str "extension"), ("resulting_days", .rat 40)] ])
      , ("collateral",
          [ collRow "d1p" "cp1" "we_owe" 15 true 5 "false" "false"
          , collRow "g1" "cp3" "owed_to_us" 10 true 5 "true" "false"
          , collRow "x" "cp9" "we_owe" 5 false 0 "false" "false" ])
      , ("nsfr_liability_legs",
          [ [("id", .str "l1"), ("amount", .rat 100), ("days", .rat 90)]
          , [("id", .str "l2"), ("amount", .rat 200), ("days", .rat 240)]
          , [("id", .str "l3"), ("amount", .rat 300), ("days", .rat 400)] ])
      , ("nsfr_loan_legs",
          [ [("id", .str "a1"), ("amount", .rat 100), ("days", .rat 180)]
          , [("id", .str "a2"), ("amount", .rat 400), ("days", .rat 730)] ])
      ]
    let out := evalProgram cashFlowsProgram tenv
      [ ("initial_liquid_buffer", .rat 130)
      , ("use_currency_filter", .bool false)
      , ("target_currency", .str "AUD") ]
    out =
      [ ("min_cum_net_cf", .rat (-130))
      , ("survival_buffer_met", .bool true)
      , ("sched_outflow_30d", .rat 110)
      , ("spv_outflow_30d", .rat 60)
      , ("sched_inflow_30d", .rat 40)
      , ("lcr_deriv_outflow", .rat 100)
      , ("lcr_deriv_inflow", .rat 30)
      , ("nsfr_asf_lt6m", .rat 100)
      , ("nsfr_asf_6m_to_1y", .rat 200)
      , ("nsfr_asf_ge1y", .rat 300)
      , ("nsfr_loan_lt1y", .rat 100)
      , ("nsfr_loan_ge1y", .rat 400)
      ] := by native_decide

private def obs0 : ObservationContext := { validAt := 0, knownAt := 0, sourceReference := "spec" }

private def cfD (id : String) (side : Side) (amt : Rat) (days : Int) (opts : Reading (List ContractOption) := .known [])
    (source : CashFlowSource := .standard) : ContractualCashFlow :=
  { obligation := { id, currency := .aud, side, amount := Amount.ofRat amt, settlementDate := .known days,
                    optionality := opts, performing := .known true, observation := obs0 }
    isOpenMaturity := false, isMinimumContractualPayment := false, isNonFinancialRevenue := false,
    countedInHqla := false, source }

/-- The same portfolio through the Lean domain gives the same figures. -/
example :
    let call : ContractOption := { holder := .counterparty, kind := .earlyRedemption, exerciseDate := 5, resultingSettlementDate := 10 }
    let ext : ContractOption := { holder := .counterparty, kind := .extension, exerciseDate := 15, resultingSettlementDate := 40 }
    let flows : List ContractualCashFlow :=
      [ cfD "out1" .weOwe 60 5 (source := .securitisationSpv)
      , cfD "in1" .owedToUs 40 20
      , cfD "out2" .weOwe 50 90 (.known [call])
      , cfD "ext" .owedToUs 999 20 (.known [ext])
      , cfD "unk" .owedToUs 77 5 (.unknown "not loaded")
      , cfD "d1p" .weOwe 80 5 (source := .derivative "cp1" true false)
      , cfD "d1r" .owedToUs 30 5 (source := .derivative "cp1" true false)
      , cfD "d2p" .weOwe 20 3 (source := .derivative "cp2" false true)
      , cfD "d2r" .owedToUs 50 3 (source := .derivative "cp2" false true)
      , cfD "g1" .weOwe 40 5 (source := .derivative "cp3" false false) ]
    let coll (o n : String) (side : Side) (amt : Rat) (d : Reading Int) (e c : Reading Bool) : CollateralMovement :=
      { obligationId := o, nettingSetId := n, side, amount := Amount.ofRat amt, currency := .aud,
        settlementDate := d, eligibleForOffset := e, alreadyCountedInHqla := c, observation := obs0 }
    let collateral :=
      [ coll "d1p" "cp1" .weOwe 15 (.known 5) (.known false) (.known false)
      , coll "g1" "cp3" .owedToUs 10 (.known 5) (.known true) (.known false)
      , coll "x" "cp9" .weOwe 5 (.unknown "date not captured") (.known false) (.known false) ]
    let (dOut, dIn) := deriveLcrDerivativeOutflowsAndInflows flows collateral
    let nonDeriv := flows.filter (fun f => match f.source with | .derivative _ _ _ => false | _ => true)
    let ladder := projectCashFlowLadder flows
    sumBy dOut LCR.OutflowItem.outflowAmount = 100 ∧
    sumBy dIn (fun i => i.amount) = 30 ∧
    sumBy (nonDeriv.filter ContractualCashFlow.isLcrOutflowWithin30d) (·.amount) = 110 ∧
    sumBy (nonDeriv.filter ContractualCashFlow.isEligibleLcrInflow) (·.amount) = 40 ∧
    (ladder.map (·.cumulativeNetCashFlow)).foldl min 0 = -130 ∧
    hasPositiveSurvivalBuffer 130 flows = true := by native_decide

end Lean2Ibis.CashFlowsSpec
