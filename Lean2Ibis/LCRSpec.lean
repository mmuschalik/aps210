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
  | .forwardStartingLending _ => "forward_starting_lending"
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
  -- Paras 10 & 12: HQLA2 needs both stress observations known and within the limit.
  let stressOk := .binOp .or
    (.binOp .eq (.col "category") (.litStr "hqla1"))
    (.binOp .and
      (.binOp .and (.col "price_decline_known") (.col "haircut_increase_known"))
      (.binOp .and
        (.binOp .le (.col "max_30d_price_decline") stressLimit)
        (.binOp .le (.col "max_30d_haircut_increase") stressLimit)))
  -- Para 25: once an asset becomes ineligible (`ineligible_days_known = true`), it may only remain
  -- in HQLA during the 30-day grace period (`days_since_ineligible <= 30`).
  -- Paras 9, 10, 12, 28: regulator recognition required for HQLA2A/2B/host ALA, not domestic HQLA1.
  let graceOk := .binOp .and (.col "ineligible_days_known")
    (.binOp .le (.col "days_since_ineligible") (.litRat 30 1))
  let regOk := .binOp .or
    (.binOp .and (.binOp .eq (.col "category") (.litStr "hqla1")) (.unOp .not (.col "is_host_recognised_ala")))
    (.col "recognised_by_regulator")
  let eligOk := .binOp .or
    (.binOp .and (.unOp .not (.col "ineligible_days_known"))
      (.binOp .and (.col "issuer_restriction_satisfied") (.binOp .and regOk stressOk)))
    graceOk
  let ok := .binOp .and opOk (.binOp .and eligOk (.binOp .and basisOk divOk))
  let mv := .unOp .nonNeg (.col "market_value")
  .ite ok (.binOp .mul mv hqlaWeightExpr) (.litRat 0 1)

/-- Attachment A, Paragraph 29: whether an asset is subject to its group's Level 2 cap. -/
def level2RestrictedExpr : SExpr :=
  .binOp .and (.var "is_level2") (.unOp .not (.col "freely_transferable_at_level2"))

/-- Attachment A, Tables 1 & 2 & APG 210: Retail deposit outflow expression. Expects the customer's
    aggregate balance `cust_total` and lowest guarantee limit `cust_limit` joined onto each row. Scorecard
    criteria are reading columns ("true", "false", "unknown", "refused"); anything but "false" scores. -/
def retailOutflowExpr : SExpr :=
  let bal := .unOp .nonNeg (.col "balance")
  let total := .col "cust_total"
  let lim := .col "cust_limit"
  let scored (c : String) (pts : Nat) : SExpr :=
    .ite (.binOp .ne (.col c) (.litStr "false")) (.litRat pts 1) (.litRat 0 1)
  let baseScore :=
    .binOp .add (scored "is_online_account" 2)
      (.binOp .add (scored "no_established_relationship" 1) (scored "is_heavily_rate_driven" 1))
  let rateFor (score : SExpr) : SExpr :=
    .ite (.binOp .le score (.litRat 2 1)) (.litRat 10 100) (.litRat 25 100)
  let scorecardThreshold := .ite (.binOp .gt lim (.litRat 0 1)) lim (.litRat 250000 1)
  let excessRate := rateFor (.binOp .add (.litRat 2 1) baseScore)
  let lessStableRate := rateFor (.binOp .add (.ite (.binOp .gt total scorecardThreshold) (.litRat 2 1) (.litRat 0 1)) baseScore)
  let insured := .ite (.binOp .le total (.litRat 0 1)) (.litRat 0 1)
    (.binOp .div (.binOp .mul bal (.binOp .min total lim)) total)
  let stableOutflow :=
    .binOp .add
      (.binOp .mul insured (.litRat 5 100))
      (.binOp .mul (.unOp .nonNeg (.binOp .sub bal insured)) excessRate)
  let excludedTerm := .binOp .and (.unOp .not (.col "notice_deposit_called"))
    (.binOp .and (.col "residual_maturity_or_notice_gt_30d")
      (.binOp .and (.col "no_withdrawal_right_or_material_penalty")
        (.unOp .not (.col "category_tainted_by_early_withdrawal"))))
  let excludedWholesaleTerm := .binOp .and (.unOp .not (.col "notice_deposit_called"))
    (.binOp .and (.col "residual_maturity_or_notice_gt_30d")
      (.binOp .and (.col "no_legal_withdrawal_right_within_30d")
        (.unOp .not (.col "category_tainted_by_early_withdrawal"))))
  .ite (.binOp .and (.col "is_intermediated") (.unOp .not (.col "intermediated_look_through_met")))
    (.ite excludedWholesaleTerm (.litRat 0 1) bal)
    (.ite (.col "notice_deposit_called")
      bal
      (.ite excludedTerm
        (.litRat 0 1)
        (.ite (.binOp .and (.var "is_level2")
                (.binOp .and (.col "host_jurisdiction_applies_basel") (.col "has_host_override")))
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
    , ("forward_starting_lending", .litRat 1 1)
    , ("other_contractual_cash_outflows", .litRat 1 1) ]
    (.litRat 0 1)

def table3OutflowExpr : SExpr :=
  .ite
    (.binOp .and
      (.binOp .eq (.col "category") (.litStr "secured_by_clf_eligible_within_limit"))
      (.unOp .not (.binOp .and (.var "is_locally_incorporated") (.binOp .gt (.var "clf_recognised") (.litRat 0 1)))))
    (.unOp .nonNeg (.col "amount"))
    (.ite
      (.binOp .and
        (.binOp .eq (.col "category") (.litStr "derivative_valuation_largest_30d_flow_24m"))
        (.col "has_apra_alt_amount"))
      (.unOp .nonNeg (.col "apra_alt_amount"))
      (.ite (.binOp .eq (.col "category") (.litStr "forward_starting_lending"))
        (.binOp .mul
          (.unOp .nonNeg (.binOp .sub (.unOp .nonNeg (.col "amount")) (.unOp .nonNeg (.col "rollover_assumed"))))
          table3RateExpr)
        (.binOp .mul (.unOp .nonNeg (.col "amount")) table3RateExpr)))

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
  let ok := .binOp .and (.binOp .eq (.col "is_fully_performing") (.litStr "true")) (.unOp .not (.col "counted_in_hqla"))
  .ite ok (.binOp .mul (.unOp .nonNeg (.col "amount")) inflowRateExpr) (.litRat 0 1)

/-- Complete LCR program in the Lean-to-Ibis IR (including AUD filtering, SFT unwinds,
    15%/40% HQLA caps, CLF 3-way min, 75% inflow cap, and Foreign ADI 50% HO facility). -/
def lcrProgram : Program :=
  let audFilter (tbl : RelExpr) :=
    .filter (.binOp .or (.unOp .not (.var "is_aud_only")) (.binOp .eq (.col "currency") (.litStr "AUD"))) tbl
  let hqlaPre := .withColumns [("val_pre", hqlaValueAfterHaircutExpr)] (audFilter (.table "hqla_assets"))
  -- Para 29: one cap per Level 2 transfer group, applied to the group total and allocated pro rata.
  let groupTotals :=
    .groupBy ["level2_transfer_group"] [("grp_total", .sum, .col "val_pre")] (.filter level2RestrictedExpr hqlaPre)
  let groupCaps :=
    .groupBy ["level2_transfer_group"] [("grp_cap", .min, .unOp .nonNeg (.col "cap"))] (.table "level2_transferable_caps")
  let groupScale :=
    .withColumns
      [("grp_scale", .ite (.binOp .le (.col "grp_total") (.litRat 0 1)) (.litRat 0 1)
          (.binOp .min (.litRat 1 1) (.binOp .div (.coalesceZero (.col "grp_cap")) (.col "grp_total"))))]
      (.leftJoin "level2_transfer_group" groupTotals groupCaps)
  let hqlaScaled :=
    .withColumns
      [("val", .ite level2RestrictedExpr (.binOp .mul (.col "val_pre") (.coalesceZero (.col "grp_scale"))) (.col "val_pre"))]
      (.leftJoin "level2_transfer_group" hqlaPre groupScale)
  let para9eRel :=
    .filter
      (.binOp .and
        (.binOp .eq (.col "category") (.litStr "hqla1"))
        (.col "is_non_zero_rw_foreign_sov_in_foreign_currency"))
      hqlaScaled
  -- APG 210 / Att A Paras 35 & 37: per-(customer_id, currency) guarantee aggregation,
  -- excluding non-look-through intermediated deposits (FI wholesale deposits).
  let retailRel := audFilter (.table "retail_deposits")
  let retailGenuineRel :=
    .filter (.unOp .not (.binOp .and (.col "is_intermediated") (.unOp .not (.col "intermediated_look_through_met"))))
      retailRel
  let customerAgg :=
    .groupBy ["customer_id", "currency"]
      [("cust_total", .sum, .unOp .nonNeg (.col "balance")), ("cust_limit", .min, .unOp .nonNeg (.col "guarantee_limit"))]
      retailGenuineRel
  -- AUD-only subset for Para 14 CLF shortfall cap in All-Currencies LCR.
  let audHqlaPre := .withColumns [("val_pre", hqlaValueAfterHaircutExpr)]
    (.filter (.binOp .eq (.col "currency") (.litStr "AUD")) (.table "hqla_assets"))
  let audGroupTotals :=
    .groupBy ["level2_transfer_group"] [("grp_total", .sum, .col "val_pre")] (.filter level2RestrictedExpr audHqlaPre)
  let audGroupScale :=
    .withColumns
      [("grp_scale", .ite (.binOp .le (.col "grp_total") (.litRat 0 1)) (.litRat 0 1)
          (.binOp .min (.litRat 1 1) (.binOp .div (.coalesceZero (.col "grp_cap")) (.col "grp_total"))))]
      (.leftJoin "level2_transfer_group" audGroupTotals groupCaps)
  let audHqlaScaled :=
    .withColumns
      [("val", .ite level2RestrictedExpr (.binOp .mul (.col "val_pre") (.coalesceZero (.col "grp_scale"))) (.col "val_pre"))]
      (.leftJoin "level2_transfer_group" audHqlaPre audGroupScale)
  let audRetailRel := .filter (.binOp .eq (.col "currency") (.litStr "AUD")) (.table "retail_deposits")
  let audRetailGenuineRel :=
    .filter (.unOp .not (.binOp .and (.col "is_intermediated") (.unOp .not (.col "intermediated_look_through_met"))))
      audRetailRel
  let audCustomerAgg :=
    .groupBy ["customer_id", "currency"]
      [("cust_total", .sum, .unOp .nonNeg (.col "balance")), ("cust_limit", .min, .unOp .nonNeg (.col "guarantee_limit"))]
      audRetailGenuineRel
  let para9eGrouped :=
    .groupBy ["currency"] [("cur_total", .sum, .col "val")] para9eRel
  let capsDedup :=
    .groupBy ["currency"] [("cap", .min, .unOp .nonNeg (.col "cap"))] (.table "foreign_sov_currency_caps")
  let para9eJoined :=
    .withColumns
      [("capped_val", .binOp .min (.col "cur_total") (.unOp .nonNeg (.coalesceZero (.col "cap"))))]
      (.leftJoin "currency" para9eGrouped capsDedup)
  { name := "evaluate_lcr"
    tableParams :=
      [ ("hqla_assets", ["category", "currency", "market_value", "convertible_to_cash_anytime",
          "unencumbered_and_controlled", "control_evidenced", "not_pledged_or_operational_cost",
          "not_rehypothecated_if_received_in_sft", "freely_transferable_at_level2",
          "level2_transfer_group", "is_host_recognised_ala", "issuer_restriction_satisfied",
          "recognised_by_regulator", "ineligible_days_known", "days_since_ineligible",
          "hqla2a_diversified", "price_decline_known", "max_30d_price_decline",
          "haircut_increase_known", "max_30d_haircut_increase",
          "is_non_zero_rw_foreign_sov_in_foreign_currency"])
      , ("level2_transferable_caps", ["level2_transfer_group", "cap"])
      , ("retail_deposits", ["customer_id", "currency", "balance", "guarantee_limit", "is_intermediated",
          "intermediated_look_through_met", "notice_deposit_called",
          "residual_maturity_or_notice_gt_30d", "no_withdrawal_right_or_material_penalty",
          "no_legal_withdrawal_right_within_30d",
          "category_tainted_by_early_withdrawal", "host_jurisdiction_applies_basel", "has_host_override",
          "host_override_rate", "is_stable",
          "is_online_account", "no_established_relationship", "is_heavily_rate_driven"])
      , ("other_outflows", ["category", "currency", "amount", "custom_rate", "has_apra_alt_amount", "apra_alt_amount",
          "rollover_assumed"])
      , ("inflows", ["category", "currency", "amount", "is_fully_performing", "counted_in_hqla"])
      , ("foreign_sov_currency_caps", ["currency", "cap"]) ]
    scalarParams :=
      [ ("is_locally_incorporated", .bool)
      , ("is_aud_only", .bool), ("is_level2", .bool), ("required_min_rate", .rat)
      , ("delta_hqla1", .rat), ("delta_hqla2a", .rat), ("delta_hqla2b", .rat)
      , ("clf_recognised", .rat), ("foreign_ho_facility_eligible", .rat)
      , ("has_apra_inflow_cap_override", .bool), ("apra_inflow_cap_override", .rat)
      , ("internal_lcr_buffer", .rat) ]
    lets :=
      [ ("standard_hqla1", .agg .sum
          (.filter
            (.binOp .and
              (.binOp .eq (.col "category") (.litStr "hqla1"))
              (.unOp .not (.col "is_non_zero_rw_foreign_sov_in_foreign_currency")))
            hqlaScaled)
          (.col "val"))
      , ("capped_para9e_hqla1", .agg .sum para9eJoined (.col "capped_val"))
      , ("unadj_hqla1", .scalar (.binOp .add (.col "standard_hqla1") (.col "capped_para9e_hqla1")))
      , ("unadj_hqla2a", .agg .sum
          (.filter (.binOp .eq (.col "category") (.litStr "hqla2a")) hqlaScaled)
          (.col "val"))
      , ("unadj_hqla2b", .agg .sum
          (.filter (.binOp .or (.binOp .eq (.col "category") (.litStr "hqla2b_rmbs"))
            (.binOp .or (.binOp .eq (.col "category") (.litStr "hqla2b_corporate_debt"))
              (.binOp .eq (.col "category") (.litStr "hqla2b_equity"))))
            hqlaScaled)
          (.col "val"))
      , ("adj1", .scalar (.unOp .nonNeg (.binOp .add (.col "unadj_hqla1") (.col "delta_hqla1"))))
      , ("adj2a", .scalar (.unOp .nonNeg (.binOp .add (.col "unadj_hqla2a") (.col "delta_hqla2a"))))
      , ("adj2b", .scalar (.unOp .nonNeg (.binOp .add (.col "unadj_hqla2b") (.col "delta_hqla2b"))))
      , ("unadj_excess_15", .scalar (.unOp .nonNeg (.binOp .sub (.col "unadj_hqla2b")
          (.binOp .min
            (.binOp .mul (.litRat 15 85) (.binOp .add (.col "unadj_hqla1") (.col "unadj_hqla2a")))
            (.binOp .mul (.litRat 1 4) (.col "unadj_hqla1"))))))
      , ("unadj_excess_40", .scalar (.unOp .nonNeg (.binOp .sub
          (.binOp .add (.col "unadj_hqla2a") (.binOp .sub (.col "unadj_hqla2b") (.col "unadj_excess_15")))
          (.binOp .mul (.litRat 2 3) (.col "unadj_hqla1")))))
      , ("adj_excess_15", .scalar (.unOp .nonNeg (.binOp .sub (.col "adj2b")
          (.binOp .min
            (.binOp .mul (.litRat 15 85) (.binOp .add (.col "adj1") (.col "adj2a")))
            (.binOp .mul (.litRat 1 4) (.col "adj1"))))))
      , ("adj_excess_40", .scalar (.unOp .nonNeg (.binOp .sub
          (.binOp .add (.col "adj2a") (.binOp .sub (.col "adj2b") (.col "adj_excess_15")))
          (.binOp .mul (.litRat 2 3) (.col "adj1")))))
      , ("total_excess", .scalar (.binOp .max
          (.binOp .add (.col "unadj_excess_15") (.col "unadj_excess_40"))
          (.binOp .add (.col "adj_excess_15") (.col "adj_excess_40"))))
      , ("stock_ex_clf", .scalar (.unOp .nonNeg (.binOp .sub
          (.binOp .add (.col "unadj_hqla1") (.binOp .add (.col "unadj_hqla2a") (.col "unadj_hqla2b")))
          (.col "total_excess"))))
      -- AUD-only stock & NCO for Para 14 CLF shortfall cap in All-Currencies LCR
      , ("aud_unadj_hqla1", .agg .sum
          (.filter
            (.binOp .and
              (.binOp .eq (.col "category") (.litStr "hqla1"))
              (.unOp .not (.col "is_non_zero_rw_foreign_sov_in_foreign_currency")))
            audHqlaScaled)
          (.col "val"))
      , ("aud_unadj_hqla2a", .agg .sum
          (.filter (.binOp .eq (.col "category") (.litStr "hqla2a")) audHqlaScaled)
          (.col "val"))
      , ("aud_unadj_hqla2b", .agg .sum
          (.filter (.binOp .or (.binOp .eq (.col "category") (.litStr "hqla2b_rmbs"))
            (.binOp .or (.binOp .eq (.col "category") (.litStr "hqla2b_corporate_debt"))
              (.binOp .eq (.col "category") (.litStr "hqla2b_equity"))))
            audHqlaScaled)
          (.col "val"))
      , ("aud_unadj_excess_15", .scalar (.unOp .nonNeg (.binOp .sub (.col "aud_unadj_hqla2b")
          (.binOp .min
            (.binOp .mul (.litRat 15 85) (.binOp .add (.col "aud_unadj_hqla1") (.col "aud_unadj_hqla2a")))
            (.binOp .mul (.litRat 1 4) (.col "aud_unadj_hqla1"))))))
      , ("aud_unadj_excess_40", .scalar (.unOp .nonNeg (.binOp .sub
          (.binOp .add (.col "aud_unadj_hqla2a") (.binOp .sub (.col "aud_unadj_hqla2b") (.col "aud_unadj_excess_15")))
          (.binOp .mul (.litRat 2 3) (.col "aud_unadj_hqla1")))))
      , ("aud_adj1", .scalar (.unOp .nonNeg (.binOp .add (.col "aud_unadj_hqla1") (.col "delta_hqla1"))))
      , ("aud_adj2a", .scalar (.unOp .nonNeg (.binOp .add (.col "aud_unadj_hqla2a") (.col "delta_hqla2a"))))
      , ("aud_adj2b", .scalar (.unOp .nonNeg (.binOp .add (.col "aud_unadj_hqla2b") (.col "delta_hqla2b"))))
      , ("aud_adj_excess_15", .scalar (.unOp .nonNeg (.binOp .sub (.col "aud_adj2b")
          (.binOp .min
            (.binOp .mul (.litRat 15 85) (.binOp .add (.col "aud_adj1") (.col "aud_adj2a")))
            (.binOp .mul (.litRat 1 4) (.col "aud_adj1"))))))
      , ("aud_adj_excess_40", .scalar (.unOp .nonNeg (.binOp .sub
          (.binOp .add (.col "aud_adj2a") (.binOp .sub (.col "aud_adj2b") (.col "aud_adj_excess_15")))
          (.binOp .mul (.litRat 2 3) (.col "aud_adj1")))))
      , ("aud_total_excess", .scalar (.binOp .max
          (.binOp .add (.col "aud_unadj_excess_15") (.col "aud_unadj_excess_40"))
          (.binOp .add (.col "aud_adj_excess_15") (.col "aud_adj_excess_40"))))
      , ("aud_stock_ex_clf", .scalar (.unOp .nonNeg (.binOp .sub
          (.binOp .add (.col "aud_unadj_hqla1") (.binOp .add (.col "aud_unadj_hqla2a") (.col "aud_unadj_hqla2b")))
          (.col "aud_total_excess"))))
      , ("aud_retail_out", .agg .sum (.leftJoinOn ["customer_id", "currency"] audRetailRel audCustomerAgg) retailOutflowExpr)
      , ("aud_other_out", .agg .sum (.filter (.binOp .eq (.col "currency") (.litStr "AUD")) (.table "other_outflows")) table3OutflowExpr)
      , ("aud_gross_outflows", .scalar (.binOp .add (.col "aud_retail_out") (.col "aud_other_out")))
      , ("aud_gross_inflows", .agg .sum (.filter (.binOp .eq (.col "currency") (.litStr "AUD")) (.table "inflows")) inflowAmountExpr)
      , ("aud_effective_inflow_cap", .scalar (.binOp .mul (.litRat 75 100) (.col "aud_gross_outflows")))
      , ("aud_capped_inflows", .scalar (.binOp .min (.col "aud_gross_inflows") (.col "aud_effective_inflow_cap")))
      , ("aud_net_cash_outflows", .scalar (.unOp .nonNeg (.binOp .sub (.col "aud_gross_outflows") (.col "aud_capped_inflows"))))
      , ("aud_shortfall", .scalar (.unOp .nonNeg
          (.binOp .sub (.col "aud_net_cash_outflows") (.col "aud_stock_ex_clf"))))
      , ("eff_clf_recognised", .scalar (.ite (.var "is_locally_incorporated")
          (.ite (.var "is_aud_only")
            (.unOp .nonNeg (.col "clf_recognised"))
            (.binOp .min (.unOp .nonNeg (.col "clf_recognised")) (.col "aud_shortfall")))
          (.litRat 0 1)))
      , ("stock_of_hqla", .scalar (.binOp .add (.col "stock_ex_clf") (.col "eff_clf_recognised")))
      , ("retail_out", .agg .sum (.leftJoinOn ["customer_id", "currency"] retailRel customerAgg) retailOutflowExpr)
      , ("other_out", .agg .sum (audFilter (.table "other_outflows")) table3OutflowExpr)
      , ("gross_outflows", .scalar (.binOp .add (.col "retail_out") (.col "other_out")))
      , ("gross_inflows", .agg .sum (audFilter (.table "inflows")) inflowAmountExpr)
      , ("effective_inflow_cap", .scalar (.ite (.var "has_apra_inflow_cap_override")
          (.binOp .min (.binOp .mul (.litRat 75 100) (.col "gross_outflows"))
            (.unOp .nonNeg (.var "apra_inflow_cap_override")))
          (.binOp .mul (.litRat 75 100) (.col "gross_outflows"))))
      , ("capped_inflows", .scalar (.binOp .min (.col "gross_inflows") (.col "effective_inflow_cap")))
      , ("net_cash_outflows", .scalar (.unOp .nonNeg (.binOp .sub (.col "gross_outflows") (.col "capped_inflows"))))
      , ("ho_facility_recognised", .scalar (.ite (.unOp .not (.var "is_locally_incorporated"))
          (.binOp .min (.unOp .nonNeg (.col "foreign_ho_facility_eligible"))
            (.binOp .mul (.litRat 50 100) (.binOp .mul (.col "required_min_rate") (.col "net_cash_outflows"))))
          (.litRat 0 1)))
      , ("lcr_numerator", .scalar (.binOp .add (.col "stock_of_hqla") (.col "ho_facility_recognised")))
      , ("compliant", .scalar
          (.ite (.binOp .le (.col "net_cash_outflows") (.litRat 0 1))
            (.binOp .lt (.litRat 0 1) (.col "lcr_numerator"))
            (.binOp .le (.binOp .mul (.col "required_min_rate") (.col "net_cash_outflows")) (.col "lcr_numerator"))))
      , ("buffer_met", .scalar
          (.binOp .ge (.col "lcr_numerator")
            (.binOp .add (.binOp .mul (.col "required_min_rate") (.col "net_cash_outflows"))
              (.unOp .nonNeg (.col "internal_lcr_buffer"))))) ]
    outputs :=
      [ ("stock_of_hqla", .col "stock_of_hqla")
      , ("gross_outflows", .col "gross_outflows")
      , ("capped_inflows", .col "capped_inflows")
      , ("net_cash_outflows", .col "net_cash_outflows")
      , ("ho_facility_recognised", .col "ho_facility_recognised")
      , ("lcr_numerator", .col "lcr_numerator")
      , ("compliant", .col "compliant")
      , ("buffer_met", .col "buffer_met") ] }

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
