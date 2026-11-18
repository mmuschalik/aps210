import APS210.CashFlows
import Lean2Ibis.IR

namespace Lean2Ibis.CashFlowsSpec

open APS210
open APS210.CashFlows

/-- Scalar IR expressions corresponding to `APS210.CashFlows` functions. -/
def effectiveOutflowDaysExpr : SExpr :=
  .ite (.col "has_earliest_call")
    (.binOp .min (.col "contractual_days") (.col "earliest_call_days"))
    (.col "contractual_days")

def effectiveInflowDaysExpr : SExpr :=
  .ite (.col "has_latest_extension")
    (.binOp .max (.col "contractual_days") (.col "latest_extension_days"))
    (.col "contractual_days")

def effectiveDaysExpr : SExpr :=
  .ite (.binOp .eq (.col "direction") (.litStr "outflow"))
    effectiveOutflowDaysExpr
    effectiveInflowDaysExpr

def horizonRankExpr (daysExpr : SExpr) : SExpr :=
  .ite (.binOp .eq daysExpr (.litRat 0 1)) (.litRat 0 1)
    (.ite (.binOp .le daysExpr (.litRat 7 1)) (.litRat 1 1)
      (.ite (.binOp .le daysExpr (.litRat 30 1)) (.litRat 2 1)
        (.ite (.binOp .lt daysExpr (.litRat 180 1)) (.litRat 3 1)
          (.ite (.binOp .lt daysExpr (.litRat 365 1)) (.litRat 4 1)
            (.litRat 5 1)))))

def isEligibleLcrInflowExpr : SExpr :=
  .binOp .and (.binOp .eq (.col "direction") (.litStr "inflow"))
    (.binOp .and (.binOp .le effectiveInflowDaysExpr (.litRat 30 1))
      (.binOp .and
        (.binOp .or (.unOp .not (.col "is_open_maturity")) (.col "is_minimum_contractual_payment"))
        (.binOp .and (.unOp .not (.col "is_non_financial_revenue"))
          (.binOp .and (.unOp .not (.col "counted_in_hqla")) (.col "is_performing")))))

def isLcrOutflowWithin30dExpr : SExpr :=
  .binOp .and (.binOp .eq (.col "direction") (.litStr "outflow"))
    (.binOp .le effectiveOutflowDaysExpr (.litRat 30 1))

def isLadderValidExpr : SExpr :=
  .binOp .or (.binOp .eq (.col "direction") (.litStr "outflow"))
    (.binOp .and (.col "is_performing")
      (.binOp .and (.unOp .not (.col "is_non_financial_revenue"))
        (.binOp .or (.unOp .not (.col "is_open_maturity")) (.col "is_minimum_contractual_payment"))))

def enrichedCashFlowsRel : RelExpr :=
  .withColumns
    [ ("eff_out_days", effectiveOutflowDaysExpr)
    , ("eff_in_days", effectiveInflowDaysExpr)
    , ("eff_days", effectiveDaysExpr)
    , ("horizon_rank", horizonRankExpr (.col "eff_days"))
    , ("is_elig_lcr_inflow", isEligibleLcrInflowExpr)
    , ("is_lcr_outflow_30d", isLcrOutflowWithin30dExpr)
    , ("is_ladder_valid", isLadderValidExpr)
    , ("nonneg_amt", .unOp .nonNeg (.col "amount"))
    , ("signed_amt",
        .ite (.binOp .eq (.col "direction") (.litStr "inflow"))
          (.col "nonneg_amt")
          (.binOp .sub (.litRat 0 1) (.col "nonneg_amt")))
    ]
    (.table "cash_flows")

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

def derivNettableRel : RelExpr :=
  .withColumns
    [ ("net_outflow",
        .ite (.binOp .ge (.col "raw_net_payable") (.litRat 0 1))
          (.unOp .nonNeg (.binOp .sub (.col "raw_net_payable") (.col "col_adj")))
          (.litRat 0 1))
    , ("net_inflow",
        .ite (.binOp .lt (.col "raw_net_payable") (.litRat 0 1))
          (.unOp .nonNeg (.binOp .sub (.binOp .sub (.litRat 0 1) (.col "raw_net_payable")) (.col "col_adj")))
          (.litRat 0 1))
    ]
    (.groupBy ["counterparty", "currency"]
      [ ("raw_net_payable", .sum,
          .binOp .sub (.unOp .nonNeg (.col "payable")) (.unOp .nonNeg (.col "receivable")))
      , ("col_adj", .sum, .unOp .nonNeg (.col "uncounted_hqla_collateral"))
      ]
      (.filter
        (.binOp .or (.col "has_valid_mna") (.col "is_same_day_fx_principal_exchange"))
        (.table "lcr_derivative_legs")))

def derivGrossRel : RelExpr :=
  .withColumns
    [ ("gross_outflow",
        .unOp .nonNeg (.binOp .sub (.unOp .nonNeg (.col "payable")) (.unOp .nonNeg (.col "uncounted_hqla_collateral"))))
    , ("gross_inflow",
        .unOp .nonNeg (.binOp .sub (.unOp .nonNeg (.col "receivable")) (.unOp .nonNeg (.col "uncounted_hqla_collateral"))))
    ]
    (.filter
      (.binOp .and (.unOp .not (.col "has_valid_mna"))
        (.unOp .not (.col "is_same_day_fx_principal_exchange")))
      (.table "lcr_derivative_legs"))

/-- Complete APS 210 Cash-Flow Engine program in the Lean-to-Ibis IR. -/
def cashFlowsProgram : Program :=
  { name := "evaluate_aps210_cashflows"
  , tableParams :=
      [ ("cash_flows",
          [ "id", "currency", "direction", "amount", "contractual_days"
          , "has_earliest_call", "earliest_call_days"
          , "has_latest_extension", "latest_extension_days"
          , "is_open_maturity", "is_minimum_contractual_payment"
          , "is_non_financial_revenue", "counted_in_hqla", "is_performing", "source"
          , "counterparty", "has_valid_mna", "is_same_day_fx_principal_exchange" ])
      , ("lcr_derivative_legs",
          [ "counterparty", "currency", "has_valid_mna", "is_same_day_fx_principal_exchange"
          , "payable", "receivable", "uncounted_hqla_collateral" ])
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
      , ("cf_deriv_nettable_out", .agg .sum
          (.withColumns
            [ ("net_out", .unOp .nonNeg (.binOp .sub (.col "tot_pay") (.col "tot_rec"))) ]
            (.groupBy ["counterparty", "currency"]
              [ ("tot_pay", .sum, .ite (.col "is_lcr_outflow_30d") (.col "nonneg_amt") (.litRat 0 1))
              , ("tot_rec", .sum, .ite (.col "is_elig_lcr_inflow") (.col "nonneg_amt") (.litRat 0 1)) ]
              (.filter
                (.binOp .and (.binOp .eq (.col "source") (.litStr "derivative"))
                  (.binOp .and (.col "has_valid_mna")
                    (.unOp .not (.col "is_same_day_fx_principal_exchange"))))
                enrichedCashFlowsRel)))
          (.col "net_out"))
      , ("cf_deriv_nettable_in", .agg .sum
          (.withColumns
            [ ("net_in", .unOp .nonNeg (.binOp .sub (.col "tot_rec") (.col "tot_pay"))) ]
            (.groupBy ["counterparty", "currency"]
              [ ("tot_pay", .sum, .ite (.col "is_lcr_outflow_30d") (.col "nonneg_amt") (.litRat 0 1))
              , ("tot_rec", .sum, .ite (.col "is_elig_lcr_inflow") (.col "nonneg_amt") (.litRat 0 1)) ]
              (.filter
                (.binOp .and (.binOp .eq (.col "source") (.litStr "derivative"))
                  (.binOp .and (.col "has_valid_mna")
                    (.unOp .not (.col "is_same_day_fx_principal_exchange"))))
                enrichedCashFlowsRel)))
          (.col "net_in"))
      , ("cf_deriv_outflow_30d",
          .agg .sum
            (.filter
              (.binOp .and (.col "is_lcr_outflow_30d")
                (.binOp .and (.binOp .eq (.col "source") (.litStr "derivative"))
                  (.binOp .or (.unOp .not (.col "has_valid_mna"))
                    (.col "is_same_day_fx_principal_exchange"))))
              enrichedCashFlowsRel)
            (.col "nonneg_amt"))
      , ("cf_deriv_inflow_30d",
          .agg .sum
            (.filter
              (.binOp .and (.col "is_elig_lcr_inflow")
                (.binOp .and (.binOp .eq (.col "source") (.litStr "derivative"))
                  (.binOp .or (.unOp .not (.col "has_valid_mna"))
                    (.col "is_same_day_fx_principal_exchange"))))
              enrichedCashFlowsRel)
            (.col "nonneg_amt"))
      , ("deriv_net_outflow", .agg .sum derivNettableRel (.col "net_outflow"))
      , ("deriv_net_inflow", .agg .sum derivNettableRel (.col "net_inflow"))
      , ("deriv_gross_outflow", .agg .sum derivGrossRel (.col "gross_outflow"))
      , ("deriv_gross_inflow", .agg .sum derivGrossRel (.col "gross_inflow"))
      , ("lcr_deriv_outflow", .scalar (.binOp .add (.var "cf_deriv_nettable_out")
          (.binOp .add (.var "cf_deriv_outflow_30d")
            (.binOp .add (.var "deriv_net_outflow") (.var "deriv_gross_outflow")))))
      , ("lcr_deriv_inflow", .scalar (.binOp .add (.var "cf_deriv_nettable_in")
          (.binOp .add (.var "cf_deriv_inflow_30d")
            (.binOp .add (.var "deriv_net_inflow") (.var "deriv_gross_inflow")))))
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

private def dirToStr : CashFlowDirection → String
  | .inflow => "inflow"
  | .outflow => "outflow"

private def cashFlowRow (f : ContractualCashFlow) : Row :=
  [ ("direction", .str (dirToStr f.direction))
  , ("amount", .rat f.amount)
  , ("contractual_days", .rat (f.contractualDays : Rat))
  , ("has_earliest_call", .bool f.earliestCallDays.isSome)
  , ("earliest_call_days", .rat (f.earliestCallDays.getD 0 : Rat))
  , ("has_latest_extension", .bool f.latestExtensionDays.isSome)
  , ("latest_extension_days", .rat (f.latestExtensionDays.getD 0 : Rat))
  , ("is_open_maturity", .bool f.isOpenMaturity)
  , ("is_minimum_contractual_payment", .bool f.isMinimumContractualPayment)
  , ("is_non_financial_revenue", .bool f.isNonFinancialRevenue)
  , ("counted_in_hqla", .bool f.countedInHqla)
  , ("is_performing", .bool f.isPerforming)
  ]

private theorem nat_rat_le (a b : Nat) : ((a : Rat) ≤ (b : Rat)) ↔ a ≤ b := by
  change (((a : Int) : Rat) ≤ ((b : Int) : Rat)) ↔ a ≤ b
  rw [Rat.intCast_le_intCast, Int.ofNat_le]

theorem effectiveOutflowDays_ir_equiv (f : ContractualCashFlow) :
    evalSExpr (cashFlowRow f) effectiveOutflowDaysExpr = .rat (f.effectiveOutflowDays : Rat) := by
  cases h : f.earliestCallDays <;>
  simp [ContractualCashFlow.effectiveOutflowDays, effectiveOutflowDaysExpr, cashFlowRow,
    evalSExpr, Row.get, Val.asBool, Val.asRat, h]
  · rename_i c
    by_cases hle : f.contractualDays ≤ c
    · have hler : (f.contractualDays : Rat) ≤ (c : Rat) := (nat_rat_le _ _).mpr hle
      simp [min, hle, hler]
    · have hler : ¬ ((f.contractualDays : Rat) ≤ (c : Rat)) := fun h' => hle ((nat_rat_le _ _).mp h')
      simp [min, hle, hler]

theorem effectiveInflowDays_ir_equiv (f : ContractualCashFlow) :
    evalSExpr (cashFlowRow f) effectiveInflowDaysExpr = .rat (f.effectiveInflowDays : Rat) := by
  cases h : f.latestExtensionDays <;>
  simp [ContractualCashFlow.effectiveInflowDays, effectiveInflowDaysExpr, cashFlowRow,
    evalSExpr, Row.get, Val.asBool, Val.asRat, h]
  · rename_i e
    by_cases hle : f.contractualDays ≤ e
    · have hler : (f.contractualDays : Rat) ≤ (e : Rat) := (nat_rat_le _ _).mpr hle
      simp [max, hle, hler]
    · have hler : ¬ ((f.contractualDays : Rat) ≤ (e : Rat)) := fun h' => hle ((nat_rat_le _ _).mp h')
      simp [max, hle, hler]

theorem isLcrOutflowWithin30d_ir_equiv (f : ContractualCashFlow) :
    evalSExpr (cashFlowRow f) isLcrOutflowWithin30dExpr = .bool f.isLcrOutflowWithin30d := by
  have hout := effectiveOutflowDays_ir_equiv f
  have h30div : ((30 : Int) : Rat) / ((1 : Nat) : Rat) = (30 : Rat) := by native_decide
  have hle30 (n : Nat) : ((n : Rat) ≤ (30 : Rat)) ↔ n ≤ 30 := by
    change ((n : Rat) ≤ ((30 : Nat) : Rat)) ↔ n ≤ 30
    exact nat_rat_le n 30
  have hneq : (Val.str "inflow" == Val.str "outflow") = false := by decide
  simp only [isLcrOutflowWithin30dExpr, ContractualCashFlow.isLcrOutflowWithin30d, evalSExpr, hout, h30div]
  cases hd : f.direction
  · simp [dirToStr, cashFlowRow, Row.get, Val.asRat, Val.asBool, hd, hneq]
  · simp [dirToStr, cashFlowRow, Row.get, Val.asRat, Val.asBool, hd, hle30]

theorem isEligibleLcrInflow_ir_equiv (f : ContractualCashFlow) :
    evalSExpr (cashFlowRow f) isEligibleLcrInflowExpr = .bool f.isEligibleLcrInflow := by
  have hin := effectiveInflowDays_ir_equiv f
  have h30div : ((30 : Int) : Rat) / ((1 : Nat) : Rat) = (30 : Rat) := by native_decide
  have hle30 (n : Nat) : ((n : Rat) ≤ (30 : Rat)) ↔ n ≤ 30 := by
    change ((n : Rat) ≤ ((30 : Nat) : Rat)) ↔ n ≤ 30
    exact nat_rat_le n 30
  have hneq : (Val.str "outflow" == Val.str "inflow") = false := by decide
  simp only [isEligibleLcrInflowExpr, ContractualCashFlow.isEligibleLcrInflow, evalSExpr, hin, h30div]
  cases hd : f.direction
  · simp [dirToStr, cashFlowRow, Row.get, Val.asRat, Val.asBool, hd, Bool.and_assoc, hle30]
  · simp [dirToStr, cashFlowRow, Row.get, Val.asRat, Val.asBool, hd, hneq]

-- End-to-end equivalence check on a multi-horizon, SPV, MNA/FX derivative, and NSFR-split portfolio
example :
    let tenv : TableEnv :=
      [ ("cash_flows",
          [ [ ("id", .str "out1"), ("currency", .str "AUD"), ("direction", .str "outflow"), ("amount", .rat 60)
            , ("contractual_days", .rat 5), ("has_earliest_call", .bool false), ("earliest_call_days", .rat 0)
            , ("has_latest_extension", .bool false), ("latest_extension_days", .rat 0)
            , ("is_open_maturity", .bool false), ("is_minimum_contractual_payment", .bool false)
            , ("is_non_financial_revenue", .bool false), ("counted_in_hqla", .bool false)
            , ("is_performing", .bool true), ("source", .str "securitisation_spv") ]
          , [ ("id", .str "in1"), ("currency", .str "AUD"), ("direction", .str "inflow"), ("amount", .rat 40)
            , ("contractual_days", .rat 20), ("has_earliest_call", .bool false), ("earliest_call_days", .rat 0)
            , ("has_latest_extension", .bool false), ("latest_extension_days", .rat 0)
            , ("is_open_maturity", .bool false), ("is_minimum_contractual_payment", .bool false)
            , ("is_non_financial_revenue", .bool false), ("counted_in_hqla", .bool false)
            , ("is_performing", .bool true), ("source", .str "standard") ]
          , [ ("id", .str "out2"), ("currency", .str "AUD"), ("direction", .str "outflow"), ("amount", .rat 50)
            , ("contractual_days", .rat 90), ("has_earliest_call", .bool false), ("earliest_call_days", .rat 0)
            , ("has_latest_extension", .bool false), ("latest_extension_days", .rat 0)
            , ("is_open_maturity", .bool false), ("is_minimum_contractual_payment", .bool false)
            , ("is_non_financial_revenue", .bool false), ("counted_in_hqla", .bool false)
            , ("is_performing", .bool true), ("source", .str "standard") ]
          ])
      , ("lcr_derivative_legs",
          [ [ ("counterparty", .str "cp1"), ("currency", .str "AUD"), ("has_valid_mna", .bool true)
            , ("is_same_day_fx_principal_exchange", .bool false), ("payable", .rat 80), ("receivable", .rat 30)
            , ("uncounted_hqla_collateral", .rat 0) ]
          , [ ("counterparty", .str "cp2"), ("currency", .str "AUD"), ("has_valid_mna", .bool false)
            , ("is_same_day_fx_principal_exchange", .bool true), ("payable", .rat 20), ("receivable", .rat 50)
            , ("uncounted_hqla_collateral", .rat 0) ]
          ])
      , ("nsfr_liability_legs",
          [ [("id", .str "l1"), ("amount", .rat 100), ("days", .rat 90)]
          , [("id", .str "l2"), ("amount", .rat 200), ("days", .rat 240)]
          , [("id", .str "l3"), ("amount", .rat 300), ("days", .rat 400)] ])
      , ("nsfr_loan_legs",
          [ [("id", .str "a1"), ("amount", .rat 100), ("days", .rat 180)]
          , [("id", .str "a2"), ("amount", .rat 400), ("days", .rat 730)] ])
      ]
    let out := evalProgram cashFlowsProgram tenv
      [ ("initial_liquid_buffer", .rat 70)
      , ("use_currency_filter", .bool false)
      , ("target_currency", .str "AUD") ]
    out =
      [ ("min_cum_net_cf", .rat (-70))
      , ("survival_buffer_met", .bool true)
      , ("sched_outflow_30d", .rat 60)
      , ("spv_outflow_30d", .rat 60)
      , ("sched_inflow_30d", .rat 40)
      , ("lcr_deriv_outflow", .rat 50)
      , ("lcr_deriv_inflow", .rat 30)
      , ("nsfr_asf_lt6m", .rat 100)
      , ("nsfr_asf_6m_to_1y", .rat 200)
      , ("nsfr_asf_ge1y", .rat 300)
      , ("nsfr_loan_lt1y", .rat 100)
      , ("nsfr_loan_ge1y", .rat 400)
      ] := by native_decide

end Lean2Ibis.CashFlowsSpec
