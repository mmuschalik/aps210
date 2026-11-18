import Lean2Ibis.IR
import APS210.LCR

namespace Lean2Ibis.LCRSpec

open APS210
open APS210.LCR

def hqlaCategoryToStr : HqlaCategory → String
  | .hqla1 => "hqla1"
  | .hqla2a => "hqla2a"
  | .hqla2bRmbs => "hqla2b_rmbs"
  | .hqla2bCorporateDebt => "hqla2b_corporate_debt"
  | .hqla2bEquity => "hqla2b_equity"

def table3ToStr : Table3OutflowCategory → String
  | .retailStable => "retail_stable"
  | .retailLessStable => "retail_less_stable"
  | .retailHigherRunoffLessStable => "retail_higher_runoff_less_stable"
  | .operationalDepositInsured => "operational_deposit_insured"
  | .operationalDepositUninsured => "operational_deposit_uninsured"
  | .excessOperationalNonFinInsured => "excess_operational_non_fin_insured"
  | .excessOperationalNonFinUninsured => "excess_operational_non_fin_uninsured"
  | .excessOperationalFi => "excess_operational_fi"
  | .wholesaleNoticeDepositCalled => "wholesale_notice_deposit_called"
  | .nonOperationalCorpSovCbPseMdbSmeGt2mInsured => "non_op_corp_sov_cb_pse_mdb_sme_gt2m_insured"
  | .nonOperationalCorpSovCbPseMdbSmeGt2mUninsured => "non_op_corp_sov_cb_pse_mdb_sme_gt2m_uninsured"
  | .nonOperationalOther => "non_op_other"
  | .unsecuredDebtIssuance => "unsecured_debt_issuance"
  | .securedByHqla1 => "secured_by_hqla1"
  | .securedByClfEligibleWithinLimit => "secured_by_clf_eligible_within_limit"
  | .securedByHqla2a => "secured_by_hqla2a"
  | .securedDomesticSovPseMdbRwLe20 => "secured_domestic_sov_pse_mdb_rw_le20"
  | .securedByHqla2bRmbs => "secured_by_hqla2b_rmbs"
  | .securedByOtherHqla2b => "secured_by_other_hqla2b"
  | .securedAllOther => "secured_all_other"
  | .structuredMaturingIssuedByAdi => "structured_maturing_issued_by_adi"
  | .structuredSpvConduitSivLossOfFunding => "structured_spv_conduit_siv_loss_of_funding"
  | .derivativesCashOutflows => "derivatives_cash_outflows"
  | .downgradeThreeNotchCollateral => "downgrade_three_notch_collateral"
  | .excessNonSegregatedCollateralCallable => "excess_non_segregated_collateral_callable"
  | .contractuallyRequiredCollateralNotYetDemanded => "contractually_required_collateral_not_yet_demanded"
  | .collateralSubstitutionToNonHqla => "collateral_substitution_to_non_hqla"
  | .derivativeValuationLargest30dFlow24m _ => "derivative_valuation_largest_30d_flow_24m"
  | .postedNonHqla1CollateralValuationChange => "posted_non_hqla1_collateral_valuation_change"
  | .committedFacilityRetailAndSme => "committed_facility_retail_and_sme"
  | .committedCreditNonFinCorpSovCbPse => "committed_credit_non_fin_corp_sov_cb_pse"
  | .committedLiquidityNonFinCorpSovCbPse => "committed_liquidity_non_fin_corp_sov_cb_pse"
  | .committedFacilityBanksAndSupervisedAdis => "committed_facility_banks_and_supervised_adis"
  | .committedCreditOtherFinancialInstitutions => "committed_credit_other_financial_institutions"
  | .committedLiquidityOtherFinancialInstitutions => "committed_liquidity_other_financial_institutions"
  | .committedFacilityOtherLegalEntities => "committed_facility_other_legal_entities"
  | .revocableCreditAndLiquidityFacilities => "revocable_credit_and_liquidity_facilities"
  | .tradeFinanceObligations => "trade_finance_obligations"
  | .guaranteesAndLcsNonTradeFinance => "guarantees_and_lcs_non_trade_finance"
  | .buybacksDomesticDebtShortTerm _ => "buybacks_domestic_debt_short_term"
  | .buybacksDomesticDebtLongTerm _ => "buybacks_domestic_debt_long_term"
  | .nonContractualStructuredProductsAndManagedFunds => "non_contractual_structured_products_and_managed_funds"
  | .affiliatedDealerOrMarketMaker _ => "affiliated_dealer_or_market_maker"
  | .otherNonContractualContingent _ => "other_non_contractual_contingent"
  | .collateralSwapApraMethod => "collateral_swap_apra_method"
  | .otherContractualCashOutflows => "other_contractual_cash_outflows"

def inflowCategoryToStr : InflowCategory → String
  | .securedLending .hqla1 false => "sec_hqla1"
  | .securedLending .clfEligibleWithinLimit false => "sec_clf"
  | .securedLending .hqla2a false => "sec_hqla2a"
  | .securedLending .hqla2bRmbs false => "sec_hqla2b_rmbs"
  | .securedLending .otherHqla2b false => "sec_other_hqla2b"
  | .securedLending .marginLendingNonHqlaNonClf false => "sec_margin_lending"
  | .securedLending .allOtherCollateral false => "sec_all_other"
  | .securedLending _ true => "sec_short_position"
  | .linesOfCreditHeldAtOtherInstitutions => "lines_of_credit_held"
  | .retailAndSme => "retail_and_sme"
  | .financialInstitutionAndCentralBank => "fi_and_central_bank"
  | .otherWholesaleEntity => "other_wholesale_entity"
  | .maturingNonHqlaSecurities => "maturing_non_hqla_securities"
  | .operationalDepositsAtOtherFis => "operational_deposits_at_other_fis"
  | .cooperativeNetworkCentralisedDeposits => "cooperative_network_centralised_deposits"
  | .derivativesCashInflows => "derivatives_cash_inflows"
  | .otherContractualInflows true => "other_contractual_non_fin_rev"
  | .otherContractualInflows false => "other_contractual_fin"

def hqlaWeightExpr : SExpr :=
  .caseStr (.col "category")
    [ ("hqla1", .litRat 1 1)
    , ("hqla2a", .litRat 85 100)
    , ("hqla2b_rmbs", .litRat 75 100)
    , ("hqla2b_corporate_debt", .litRat 50 100)
    , ("hqla2b_equity", .litRat 50 100) ]
    (.litRat 0 1)

def hqlaValueAfterHaircutExpr : SExpr :=
  let opOk := .binOp .and (.col "convertible_to_cash_anytime")
    (.binOp .and (.col "unencumbered_and_controlled")
      (.binOp .and (.col "control_evidenced")
        (.binOp .and (.col "not_pledged_or_operational_cost")
          (.col "not_rehypothecated_if_received_in_sft"))))
  let basisOk := .ite (.var "is_level2")
    (.litBool true)
    (.unOp .not (.col "is_host_recognised_ala"))
  let divOk := .binOp .or
    (.binOp .ne (.col "category") (.litStr "hqla2a"))
    (.col "hqla2a_diversified")
  let stressLimit := .caseStr (.col "category")
    [ ("hqla2a", .litRat 10 100)
    , ("hqla2b_rmbs", .litRat 20 100)
    , ("hqla2b_corporate_debt", .litRat 20 100)
    , ("hqla2b_equity", .litRat 40 100) ]
    (.litRat 1 1)
  let stressOk := .binOp .or
    (.binOp .eq (.col "category") (.litStr "hqla1"))
    (.binOp .and
      (.binOp .le (.unOp .nonNeg (.col "max_30d_price_decline")) stressLimit)
      (.binOp .le (.unOp .nonNeg (.col "max_30d_haircut_increase")) stressLimit))
  let graceOk := .binOp .and (.col "became_ineligible_within_30_days")
    (.binOp .le (.col "days_since_ineligible") (.litRat 30 1))
  let eligOk := .binOp .or
    (.binOp .and (.col "issuer_restriction_satisfied") (.col "recognised_by_regulator"))
    graceOk
  let ok := .binOp .and opOk (.binOp .and eligOk (.binOp .and basisOk (.binOp .and divOk stressOk)))
  let mv := .unOp .nonNeg (.col "market_value")
  let cappedMv := .ite
    (.binOp .and (.var "is_level2") (.unOp .not (.col "freely_transferable_at_level2")))
    (.binOp .min mv (.unOp .nonNeg (.col "level2_transferable_cap")))
    mv
  .ite ok (.binOp .mul cappedMv hqlaWeightExpr) (.litRat 0 1)

/-- Attachment A, Tables 1 & 2: Retail deposit outflow expression (with `$250k` / `guarantee_limit` split). -/
def retailOutflowExpr : SExpr :=
  let bal := .unOp .nonNeg (.col "balance")
  let lim := .unOp .nonNeg (.col "guarantee_limit")
  let baseScore :=
    .binOp .add
      (.ite (.col "is_online_account") (.litRat 2 1) (.litRat 0 1))
      (.binOp .add
        (.ite (.col "no_established_relationship") (.litRat 1 1) (.litRat 0 1))
        (.ite (.col "is_heavily_rate_driven") (.litRat 1 1) (.litRat 0 1)))
  let excessScore := .binOp .add (.litRat 2 1) baseScore
  let excessRate := .ite (.binOp .le excessScore (.litRat 2 1)) (.litRat 10 100) (.litRat 25 100)
  let effScore := .binOp .add
    (.ite (.binOp .or (.col "above_guarantee_limit_or_250k") (.binOp .gt bal lim))
      (.litRat 2 1) (.litRat 0 1))
    baseScore
  let lessStableRate := .ite (.binOp .le effScore (.litRat 2 1)) (.litRat 10 100) (.litRat 25 100)
  let stableOutflow :=
    .binOp .add
      (.binOp .mul (.binOp .min bal lim) (.litRat 5 100))
      (.binOp .mul (.unOp .nonNeg (.binOp .sub bal lim)) excessRate)
  let excludedTerm := .binOp .and (.unOp .not (.col "notice_deposit_called"))
    (.binOp .and (.col "residual_maturity_or_notice_gt_30d")
      (.binOp .and (.col "no_withdrawal_right_or_material_penalty")
        (.unOp .not (.col "category_tainted_by_early_withdrawal"))))
  .ite (.binOp .and (.col "is_intermediated") (.unOp .not (.col "intermediated_look_through_met")))
    (.ite excludedTerm (.litRat 0 1) bal)
    (.ite (.col "notice_deposit_called")
      bal
      (.ite excludedTerm
        (.litRat 0 1)
        (.ite (.binOp .and (.var "is_level2") (.col "has_host_override"))
          (.binOp .mul bal (.unOp .nonNeg (.col "host_override_rate")))
          (.ite (.col "is_stable") stableOutflow (.binOp .mul bal lessStableRate)))))

/-- Attachment A, Table 3: All 40 cash outflow categories. -/
def table3RateExpr : SExpr :=
  .caseStr (.col "category")
    [ ("retail_stable", .litRat 5 100)
    , ("retail_less_stable", .litRat 10 100)
    , ("retail_higher_runoff_less_stable", .litRat 25 100)
    , ("operational_deposit_insured", .litRat 5 100)
    , ("operational_deposit_uninsured", .litRat 25 100)
    , ("excess_operational_non_fin_insured", .litRat 20 100)
    , ("excess_operational_non_fin_uninsured", .litRat 40 100)
    , ("excess_operational_fi", .litRat 1 1)
    , ("wholesale_notice_deposit_called", .litRat 1 1)
    , ("non_op_corp_sov_cb_pse_mdb_sme_gt2m_insured", .litRat 20 100)
    , ("non_op_corp_sov_cb_pse_mdb_sme_gt2m_uninsured", .litRat 40 100)
    , ("non_op_other", .litRat 1 1)
    , ("unsecured_debt_issuance", .litRat 1 1)
    , ("secured_by_hqla1", .litRat 0 1)
    , ("secured_by_clf_eligible_within_limit", .litRat 0 1)
    , ("secured_by_hqla2a", .litRat 15 100)
    , ("secured_domestic_sov_pse_mdb_rw_le20", .litRat 25 100)
    , ("secured_by_hqla2b_rmbs", .litRat 25 100)
    , ("secured_by_other_hqla2b", .litRat 50 100)
    , ("secured_all_other", .litRat 1 1)
    , ("structured_maturing_issued_by_adi", .litRat 1 1)
    , ("structured_spv_conduit_siv_loss_of_funding", .litRat 1 1)
    , ("derivatives_cash_outflows", .litRat 1 1)
    , ("downgrade_three_notch_collateral", .litRat 1 1)
    , ("excess_non_segregated_collateral_callable", .litRat 1 1)
    , ("contractually_required_collateral_not_yet_demanded", .litRat 1 1)
    , ("collateral_substitution_to_non_hqla", .litRat 1 1)
    , ("derivative_valuation_largest_30d_flow_24m", .litRat 1 1)
    , ("posted_non_hqla1_collateral_valuation_change", .litRat 20 100)
    , ("committed_facility_retail_and_sme", .litRat 5 100)
    , ("committed_credit_non_fin_corp_sov_cb_pse", .litRat 10 100)
    , ("committed_liquidity_non_fin_corp_sov_cb_pse", .litRat 30 100)
    , ("committed_facility_banks_and_supervised_adis", .litRat 40 100)
    , ("committed_credit_other_financial_institutions", .litRat 40 100)
    , ("committed_liquidity_other_financial_institutions", .litRat 1 1)
    , ("committed_facility_other_legal_entities", .litRat 1 1)
    , ("revocable_credit_and_liquidity_facilities", .litRat 5 100)
    , ("trade_finance_obligations", .litRat 1 1)
    , ("guarantees_and_lcs_non_trade_finance", .litRat 50 100)
    , ("buybacks_domestic_debt_short_term", .unOp .nonNeg (.col "custom_rate"))
    , ("buybacks_domestic_debt_long_term", .unOp .nonNeg (.col "custom_rate"))
    , ("non_contractual_structured_products_and_managed_funds", .litRat 5 100)
    , ("affiliated_dealer_or_market_maker", .unOp .nonNeg (.col "custom_rate"))
    , ("other_non_contractual_contingent", .unOp .nonNeg (.col "custom_rate"))
    , ("collateral_swap_apra_method", .litRat 1 1)
    , ("other_contractual_cash_outflows", .litRat 1 1) ]
    (.litRat 0 1)

def table3OutflowExpr : SExpr :=
  .binOp .mul (.unOp .nonNeg (.col "amount")) table3RateExpr

/-- Attachment A, Table 4 & Paragraphs 54–66: Cash inflow expression. -/
def inflowRateExpr : SExpr :=
  .caseStr (.col "category")
    [ ("sec_hqla1", .litRat 0 1)
    , ("sec_clf", .litRat 0 1)
    , ("sec_hqla2a", .litRat 15 100)
    , ("sec_hqla2b_rmbs", .litRat 25 100)
    , ("sec_other_hqla2b", .litRat 50 100)
    , ("sec_margin_lending", .litRat 50 100)
    , ("sec_all_other", .litRat 1 1)
    , ("sec_short_position", .litRat 0 1)
    , ("lines_of_credit_held", .litRat 0 1)
    , ("retail_and_sme", .litRat 50 100)
    , ("fi_and_central_bank", .litRat 1 1)
    , ("other_wholesale_entity", .litRat 50 100)
    , ("maturing_non_hqla_securities", .litRat 1 1)
    , ("operational_deposits_at_other_fis", .litRat 0 1)
    , ("cooperative_network_centralised_deposits", .litRat 0 1)
    , ("derivatives_cash_inflows", .litRat 1 1)
    , ("other_contractual_non_fin_rev", .litRat 0 1)
    , ("other_contractual_fin", .litRat 1 1) ]
    (.litRat 0 1)

def inflowAmountExpr : SExpr :=
  let ok := .binOp .and (.col "is_fully_performing") (.unOp .not (.col "counted_in_hqla"))
  .ite ok (.binOp .mul (.unOp .nonNeg (.col "amount")) inflowRateExpr) (.litRat 0 1)

/-- Complete LCR program in the Lean-to-Ibis IR (including AUD filtering, SFT unwinds,
    15%/40% HQLA caps, CLF 3-way min, 75% inflow cap, and Foreign ADI 50% HO facility). -/
def lcrProgram : Program :=
  let audFilter (tbl : RelExpr) :=
    .filter (.binOp .or (.unOp .not (.var "is_aud_only")) (.binOp .eq (.col "currency") (.litStr "AUD"))) tbl
  { name := "evaluate_lcr"
    tableParams :=
      [ ("hqla_assets", ["category", "currency", "market_value", "convertible_to_cash_anytime",
          "unencumbered_and_controlled", "control_evidenced", "not_pledged_or_operational_cost",
          "not_rehypothecated_if_received_in_sft", "freely_transferable_at_level2",
          "level2_transferable_cap", "is_host_recognised_ala", "issuer_restriction_satisfied",
          "recognised_by_regulator", "became_ineligible_within_30_days", "days_since_ineligible",
          "hqla2a_diversified", "max_30d_price_decline", "max_30d_haircut_increase"])
      , ("retail_deposits", ["currency", "balance", "guarantee_limit", "is_intermediated",
          "intermediated_look_through_met", "notice_deposit_called",
          "residual_maturity_or_notice_gt_30d", "no_withdrawal_right_or_material_penalty",
          "category_tainted_by_early_withdrawal", "has_host_override",
          "host_override_rate", "is_stable", "above_guarantee_limit_or_250k",
          "is_online_account", "no_established_relationship", "is_heavily_rate_driven"])
      , ("other_outflows", ["category", "currency", "amount", "custom_rate"])
      , ("inflows", ["category", "currency", "amount", "is_fully_performing", "counted_in_hqla"]) ]
    scalarParams :=
      [ ("is_aud_only", .bool), ("is_level2", .bool), ("required_min_rate", .rat)
      , ("delta_hqla1", .rat), ("delta_hqla2a", .rat), ("delta_hqla2b", .rat)
      , ("clf_recognised", .rat), ("foreign_ho_facility_eligible", .rat) ]
    lets :=
      [ ("unadj_hqla1", .aggSum
          (.filter (.binOp .eq (.col "category") (.litStr "hqla1")) (audFilter (.table "hqla_assets")))
          hqlaValueAfterHaircutExpr)
      , ("unadj_hqla2a", .aggSum
          (.filter (.binOp .eq (.col "category") (.litStr "hqla2a")) (audFilter (.table "hqla_assets")))
          hqlaValueAfterHaircutExpr)
      , ("unadj_hqla2b", .aggSum
          (.filter (.binOp .or (.binOp .eq (.col "category") (.litStr "hqla2b_rmbs"))
            (.binOp .or (.binOp .eq (.col "category") (.litStr "hqla2b_corporate_debt"))
              (.binOp .eq (.col "category") (.litStr "hqla2b_equity"))))
            (audFilter (.table "hqla_assets")))
          hqlaValueAfterHaircutExpr)
      , ("adj1", .scalar (.unOp .nonNeg (.binOp .add (.col "unadj_hqla1") (.col "delta_hqla1"))))
      , ("adj2a", .scalar (.unOp .nonNeg (.binOp .add (.col "unadj_hqla2a") (.col "delta_hqla2a"))))
      , ("adj2b", .scalar (.unOp .nonNeg (.binOp .add (.col "unadj_hqla2b") (.col "delta_hqla2b"))))
      , ("excess_15", .scalar (.unOp .nonNeg (.binOp .sub (.col "adj2b")
          (.binOp .mul (.litRat 15 85) (.binOp .add (.col "adj1") (.col "adj2a"))))))
      , ("excess_40", .scalar (.unOp .nonNeg (.binOp .sub
          (.binOp .add (.col "adj2a") (.binOp .sub (.col "adj2b") (.col "excess_15")))
          (.binOp .mul (.litRat 2 3) (.col "adj1")))))
      , ("stock_ex_clf", .scalar (.unOp .nonNeg (.binOp .sub
          (.binOp .add (.col "unadj_hqla1") (.binOp .add (.col "unadj_hqla2a") (.col "unadj_hqla2b")))
          (.binOp .add (.col "excess_15") (.col "excess_40")))))
      , ("stock_of_hqla", .scalar (.binOp .add (.col "stock_ex_clf") (.unOp .nonNeg (.col "clf_recognised"))))
      , ("retail_out", .aggSum (audFilter (.table "retail_deposits")) retailOutflowExpr)
      , ("other_out", .aggSum (audFilter (.table "other_outflows")) table3OutflowExpr)
      , ("gross_outflows", .scalar (.binOp .add (.col "retail_out") (.col "other_out")))
      , ("gross_inflows", .aggSum (audFilter (.table "inflows")) inflowAmountExpr)
      , ("capped_inflows", .scalar (.binOp .min (.col "gross_inflows")
          (.binOp .mul (.litRat 75 100) (.col "gross_outflows"))))
      , ("net_cash_outflows", .scalar (.unOp .nonNeg (.binOp .sub (.col "gross_outflows") (.col "capped_inflows"))))
      , ("ho_facility_recognised", .scalar (.binOp .min (.unOp .nonNeg (.col "foreign_ho_facility_eligible"))
          (.binOp .mul (.litRat 50 100) (.binOp .mul (.col "required_min_rate") (.col "net_cash_outflows")))))
      , ("lcr_numerator", .scalar (.binOp .add (.col "stock_of_hqla") (.col "ho_facility_recognised")))
      , ("compliant", .scalar
          (.ite (.binOp .le (.col "net_cash_outflows") (.litRat 0 1))
            (.binOp .le (.litRat 0 1) (.col "lcr_numerator"))
            (.binOp .le (.binOp .mul (.col "required_min_rate") (.col "net_cash_outflows")) (.col "lcr_numerator")))) ]
    outputs :=
      [ ("stock_of_hqla", .col "stock_of_hqla")
      , ("gross_outflows", .col "gross_outflows")
      , ("capped_inflows", .col "capped_inflows")
      , ("net_cash_outflows", .col "net_cash_outflows")
      , ("ho_facility_recognised", .col "ho_facility_recognised")
      , ("lcr_numerator", .col "lcr_numerator")
      , ("compliant", .col "compliant") ] }

theorem hqlaWeight_ir_equiv (c : HqlaCategory) :
    evalSExpr [("category", .str (hqlaCategoryToStr c))] hqlaWeightExpr = .rat c.weight := by
  cases c <;> native_decide

theorem inflowRate_ir_equiv (c : InflowCategory) :
    evalSExpr [("category", .str (inflowCategoryToStr c))] inflowRateExpr = .rat c.rate := by
  cases c with
  | securedLending col short => cases col <;> cases short <;> native_decide
  | otherContractualInflows rev => cases rev <;> native_decide
  | _ => native_decide

theorem table3Rate_ir_equiv (c : Table3OutflowCategory) :
    let customRate : Rat := match c with
      | .buybacksDomesticDebtShortTerm r => APS210.nonNeg (r.getD (pct 10))
      | .buybacksDomesticDebtLongTerm r => APS210.nonNeg (r.getD (pct 5))
      | .affiliatedDealerOrMarketMaker r => APS210.nonNeg r
      | .otherNonContractualContingent r => APS210.nonNeg r
      | _ => 0
    evalSExpr [("category", .str (table3ToStr c)), ("custom_rate", .rat customRate)] table3RateExpr = .rat c.rate := by
  cases c <;>
  simp [table3ToStr, table3RateExpr, Table3OutflowCategory.rate,
    evalSExpr, evalCaseStr, Row.get, Val.asStr, Val.asRat] <;>
  try native_decide
  all_goals (
    have hnonneg : ∀ x : Rat, APS210.nonNeg (APS210.nonNeg x) = APS210.nonNeg x := by
      intro x
      simp only [APS210.nonNeg]
      change max (if x ≤ 0 then 0 else x) 0 = if x ≤ 0 then 0 else x
      split
      · native_decide
      · next h =>
        rcases @Rat.le_total x 0 with hle | hge
        · exact absurd hle h
        · change (if x ≤ 0 then 0 else x) = x
          simp [h]
    simp [hnonneg]
  )

end Lean2Ibis.LCRSpec
