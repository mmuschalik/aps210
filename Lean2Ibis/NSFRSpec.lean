import Lean2Ibis.IR
import APS210.NSFR

namespace Lean2Ibis.NSFRSpec

open APS210
open APS210.NSFR

def asfCategoryToStr : AsfCategory → String
  | .regulatoryCapitalExTier2Lt1Yr => "reg_cap_ex_t2_lt1y"
  | .otherCapitalGe1YrNoEarlyOption => "other_cap_ge1y_no_opt"
  | .borrowingsAndLiabilitiesGe1Yr => "borrowings_ge1y"
  | .stableRetailAndSmeDepositsLt1Yr => "stable_retail_sme_lt1y"
  | .intermediatedStableRetail12mNotice => "intermediated_stable_retail_12m"
  | .lessStableRetailAndSmeDepositsLt1Yr => "less_stable_retail_sme_lt1y"
  | .intermediatedLessStableRetail12mNotice => "intermediated_less_stable_retail_12m"
  | .nonFinCorporateFundingLt1Yr => "non_fin_corp_lt1y"
  | .operationalDeposits => "operational_deposits"
  | .superannuationOperationalDeposits => "super_operational_deposits"
  | .intermediatedRetail6mNotice => "intermediated_retail_6m"
  | .sovereignPseMdbNdbFundingLt1Yr => "sov_pse_mdb_ndb_lt1y"
  | .otherFunding6mToLt1Yr => "other_funding_6m_to_lt1y"
  | .otherFundingLt6m => "other_funding_lt6m"
  | .noStatedMaturityShortOrOpen => "no_stated_maturity_short_open"
  | .deferredTaxLiability .oneYearOrMore => "dtl_ge1y"
  | .deferredTaxLiability .sixMonthsToLessThanOneYear => "dtl_6m_to_lt1y"
  | .deferredTaxLiability .lessThanSixMonths => "dtl_lt6m"
  | .minorityInterest .oneYearOrMore => "minority_ge1y"
  | .minorityInterest .sixMonthsToLessThanOneYear => "minority_6m_to_lt1y"
  | .minorityInterest .lessThanSixMonths => "minority_lt6m"
  | .cooperativeNetworkDeposit _ => "coop_network_deposit"
  | .allOtherLiabilitiesAndEquity => "all_other_liabilities_equity"

def rsfCategoryToStr : RsfCategory → String
  | .notesAndCoinsImmediatelyAvailable => "notes_and_coins"
  | .centralBankReservesAndBalances _ => "cb_reserves_balances"
  | .centralBankClaimsLt6m => "cb_claims_lt6m"
  | .tradeDateReceivablesStandardSettlement => "trade_date_receivables"
  | .unencumberedHqla1NonZeroPctRsf => "unencumbered_hqla1_5pct"
  | .loanToFiLt6mSecuredByHqla1Rehypothecatable => "loan_fi_lt6m_hqla1_rehyp"
  | .clfThirdPartyDebtOrHostAla => "clf_third_party_or_host_ala"
  | .selfSecuritisedClfEligible => "self_securitised_clf"
  | .unencumberedHqla2a => "unencumbered_hqla2a"
  | .otherLoanToFiLt6m => "other_loan_fi_lt6m"
  | .unencumberedHqla2b => "unencumbered_hqla2b"
  | .hqlaEncumbered6mToLt1Yr => "hqla_enc_6m_to_lt1y"
  | .loansToFiAndCentralBanks6mToLt1Yr => "loans_fi_cb_6m_to_lt1y"
  | .operationalDepositsAtOtherAdis => "op_deposits_other_adis"
  | .asx200NonFiEquities => "asx200_non_fi_equities"
  | .otherNonHqlaLt1Yr => "other_non_hqla_lt1y"
  | .standardResidentialPropertyLoansLvrLe80Ge1Yr => "std_res_mortgage_lvr_le80_ge1y"
  | .otherLoansNonFiRwLe35Ge1Yr => "other_loans_non_fi_rw_le35_ge1y"
  | .initialMarginAndCcpDefaultFund _ _ => "initial_margin_ccp_default"
  | .otherPerformingLoansNonFiGe1Yr => "other_performing_loans_non_fi_ge1y"
  | .nonDefaultedNonHqlaSecuritiesGe1YrAndExchangeEquities => "non_defaulted_sec_ge1y_eq"
  | .physicalTradedCommoditiesInclGold => "physical_traded_commodities"
  | .allOtherAssets => "all_other_assets"

def obsCategoryToStr : ObsExposureCategory → String
  | .irrevocableOrConditionallyRevocableFacility => "irrev_or_cond_rev_facility"
  | .unconditionallyRevocableFacility => "uncond_rev_facility"
  | .tradeFinanceAndNonTradeGuaranteesLcs => "trade_finance_and_guarantees"

def asfFactorExpr : SExpr :=
  let base := .caseStr (.col "category")
    [ ("reg_cap_ex_t2_lt1y", .litRat 1 1)
    , ("other_cap_ge1y_no_opt", .litRat 1 1)
    , ("borrowings_ge1y", .litRat 1 1)
    , ("stable_retail_sme_lt1y", .litRat 95 100)
    , ("intermediated_stable_retail_12m", .litRat 95 100)
    , ("less_stable_retail_sme_lt1y", .litRat 90 100)
    , ("intermediated_less_stable_retail_12m", .litRat 90 100)
    , ("non_fin_corp_lt1y", .litRat 50 100)
    , ("operational_deposits", .litRat 50 100)
    , ("super_operational_deposits", .litRat 50 100)
    , ("intermediated_retail_6m", .litRat 50 100)
    , ("sov_pse_mdb_ndb_lt1y", .litRat 50 100)
    , ("other_funding_6m_to_lt1y", .litRat 50 100)
    , ("other_funding_lt6m", .litRat 0 1)
    , ("no_stated_maturity_short_open", .litRat 0 1)
    , ("dtl_ge1y", .litRat 1 1)
    , ("dtl_6m_to_lt1y", .litRat 50 100)
    , ("dtl_lt6m", .litRat 0 1)
    , ("minority_ge1y", .litRat 1 1)
    , ("minority_6m_to_lt1y", .litRat 50 100)
    , ("minority_lt6m", .litRat 0 1)
    , ("coop_network_deposit", .binOp .min (.unOp .nonNeg (.col "custom_factor")) (.litRat 85 100))
    , ("all_other_liabilities_equity", .litRat 0 1) ]
    (.litRat 0 1)
  .ite (.col "is_interdependent_approved") (.litRat 0 1) base

def rsfBaseFactorExpr : SExpr :=
  .caseStr (.col "category")
    [ ("notes_and_coins", .litRat 0 1)
    , ("cb_reserves_balances", .unOp .nonNeg (.col "custom_factor"))
    , ("cb_claims_lt6m", .litRat 0 1)
    , ("trade_date_receivables", .litRat 0 1)
    , ("unencumbered_hqla1_5pct", .litRat 5 100)
    , ("loan_fi_lt6m_hqla1_rehyp", .litRat 10 100)
    , ("clf_third_party_or_host_ala", .litRat 10 100)
    , ("self_securitised_clf", .litRat 10 100)
    , ("unencumbered_hqla2a", .litRat 15 100)
    , ("other_loan_fi_lt6m", .litRat 15 100)
    , ("unencumbered_hqla2b", .litRat 50 100)
    , ("hqla_enc_6m_to_lt1y", .litRat 50 100)
    , ("loans_fi_cb_6m_to_lt1y", .litRat 50 100)
    , ("op_deposits_other_adis", .litRat 50 100)
    , ("asx200_non_fi_equities", .litRat 50 100)
    , ("other_non_hqla_lt1y", .litRat 50 100)
    , ("std_res_mortgage_lvr_le80_ge1y", .litRat 65 100)
    , ("other_loans_non_fi_rw_le35_ge1y", .litRat 65 100)
    , ("initial_margin_ccp_default",
        .ite (.col "on_behalf_of_customer_no_guarantee") (.litRat 0 1)
          (.binOp .max (.litRat 85 100) (.col "custom_factor")))
    , ("other_performing_loans_non_fi_ge1y", .litRat 85 100)
    , ("non_defaulted_sec_ge1y_eq", .litRat 85 100)
    , ("physical_traded_commodities", .litRat 85 100)
    , ("all_other_assets", .litRat 1 1) ]
    (.litRat 1 1)

def rsfEffectiveFactorExpr : SExpr :=
  let customerMarginExempt :=
    .binOp .and (.binOp .eq (.col "category") (.litStr "initial_margin_ccp_default"))
      (.col "on_behalf_of_customer_no_guarantee")
  let encApplied :=
    .ite (.col "has_exceptional_cb_factor")
      (.col "exceptional_cb_factor")
      (.caseStr (.col "encumbrance")
        [ ("unencumbered", rsfBaseFactorExpr)
        , ("less_than_six_months", rsfBaseFactorExpr)
        , ("six_months_to_less_than_one_year", .binOp .max (.litRat 50 100) rsfBaseFactorExpr)
        , ("one_year_or_more", .litRat 1 1) ]
        (.litRat 1 1))
  .ite (.binOp .or (.col "is_interdependent_approved")
      (.binOp .or (.col "excluded_from_balance_sheet_rsf") customerMarginExempt))
    (.litRat 0 1)
    encApplied

def obsFactorExpr : SExpr :=
  .caseStr (.col "category")
    [ ("irrev_or_cond_rev_facility", .litRat 5 100)
    , ("uncond_rev_facility", .litRat 1 100)
    , ("trade_finance_and_guarantees", .litRat 1 1) ]
    (.litRat 0 1)

/-- Complete Attachment C (NSFR) program in the Lean-to-Ibis IR, including CLF 10% RSF caps
    (para 32(a)-(b)) and derivative netting + 20% add-on (paras 9-10, 27-28, 37(b), 37(d)). -/
def nsfrProgram : Program :=
  let is32a := .binOp .and (.binOp .eq (.col "category") (.litStr "clf_third_party_or_host_ala"))
    (.binOp .eq rsfEffectiveFactorExpr (.litRat 10 100))
  let is32b := .binOp .and (.binOp .eq (.col "category") (.litStr "self_securitised_clf"))
    (.binOp .eq rsfEffectiveFactorExpr (.litRat 10 100))
  let mkClfBucketRel (pred : SExpr) (limVar : String) (defaultFb : SExpr) : RelExpr :=
    let base :=
      .withColumns
        [ ("val", .unOp .nonNeg (.col "carrying_value"))
        , ("fb", .ite (.col "has_excess_clf_fallback_override")
            (.unOp .nonNeg (.col "excess_clf_fallback_factor")) defaultFb) ]
        (.filter pred (.table "rsf_assets"))
    let windowed :=
      .window
        [ ("cum_incl", .sum, .col "val",
            { partitionBy := [], orderBy := [("fb", false)], preceding? := none, following? := some 0 }) ]
        base
    .withColumns
      [ ("cum_prev", .unOp .nonNeg (.binOp .sub (.col "cum_incl") (.col "val")))
      , ("rem_before", .unOp .nonNeg (.binOp .sub (.var limVar) (.col "cum_prev")))
      , ("within", .binOp .min (.col "val") (.col "rem_before"))
      , ("excess", .unOp .nonNeg (.binOp .sub (.col "val") (.col "within")))
      , ("item_rsf", .binOp .add
          (.binOp .mul (.litRat 10 100) (.col "within"))
          (.binOp .mul (.col "fb") (.col "excess"))) ]
      windowed
  { name := "evaluate_nsfr"
    tableParams :=
      [ ("asf_items", ["category", "carrying_value", "custom_factor", "is_interdependent_approved"])
      , ("rsf_assets", ["category", "carrying_value", "encumbrance", "custom_factor",
          "on_behalf_of_customer_no_guarantee", "has_exceptional_cb_factor",
          "exceptional_cb_factor", "is_interdependent_approved", "excluded_from_balance_sheet_rsf",
          "has_excess_clf_fallback_override", "excess_clf_fallback_factor"])
      , ("obs_exposures", ["category", "amount"]) ]
    scalarParams :=
      [ ("required_min_rate", .rat)
      , ("gross_deriv_liabilities", .rat), ("variation_margin_posted", .rat)
      , ("gross_deriv_assets", .rat), ("eligible_cash_vm_received", .rat)
      , ("has_clf_limit", .bool), ("recognised_clf_limit", .rat), ("host_ala_in_lcr_limit", .rat) ]
    lets :=
      [ ("total_asf", .aggSum (.table "asf_items")
          (.binOp .mul (.unOp .nonNeg (.col "carrying_value")) asfFactorExpr))
      , ("non_clf_rsf", .aggSum
          (.filter (.unOp .not (.binOp .or is32a is32b)) (.table "rsf_assets"))
          (.binOp .mul (.unOp .nonNeg (.col "carrying_value")) rsfEffectiveFactorExpr))
      , ("total_32a", .aggSum (.filter is32a (.table "rsf_assets")) (.unOp .nonNeg (.col "carrying_value")))
      , ("clf_lim", .scalar (.ite (.var "has_clf_limit") (.unOp .nonNeg (.var "recognised_clf_limit")) (.litRat 0 1)))
      , ("clf_32a_lim", .scalar (.binOp .add (.var "clf_lim") (.unOp .nonNeg (.var "host_ala_in_lcr_limit"))))
      , ("rem_32a_lim", .scalar (.unOp .nonNeg (.binOp .sub (.var "clf_32a_lim") (.var "total_32a"))))
      , ("rem_lim", .scalar (.binOp .min (.var "clf_lim") (.var "rem_32a_lim")))
      , ("rsf_32a", .agg .sum (mkClfBucketRel is32a "clf_32a_lim" (.litRat 50 100)) (.col "item_rsf"))
      , ("rsf_32b", .agg .sum (mkClfBucketRel is32b "rem_lim" (.litRat 85 100)) (.col "item_rsf"))
      , ("asset_rsf", .scalar (.binOp .add (.col "non_clf_rsf") (.binOp .add (.col "rsf_32a") (.col "rsf_32b"))))
      , ("obs_rsf", .aggSum (.table "obs_exposures")
          (.binOp .mul (.unOp .nonNeg (.col "amount")) obsFactorExpr))
      , ("nsfr_deriv_liab", .scalar (.unOp .nonNeg
          (.binOp .sub (.unOp .nonNeg (.col "gross_deriv_liabilities"))
            (.unOp .nonNeg (.col "variation_margin_posted")))))
      , ("nsfr_deriv_assets", .scalar (.unOp .nonNeg
          (.binOp .sub (.unOp .nonNeg (.col "gross_deriv_assets"))
            (.unOp .nonNeg (.col "eligible_cash_vm_received")))))
      , ("net_deriv_rsf", .scalar (.unOp .nonNeg
          (.binOp .sub (.col "nsfr_deriv_assets") (.col "nsfr_deriv_liab"))))
      , ("deriv_addon_rsf", .scalar (.binOp .mul (.litRat 20 100) (.unOp .nonNeg (.col "gross_deriv_liabilities"))))
      , ("total_rsf", .scalar (.binOp .add (.col "asset_rsf")
          (.binOp .add (.col "obs_rsf") (.binOp .add (.col "net_deriv_rsf") (.col "deriv_addon_rsf")))))
      , ("compliant", .scalar
          (.ite (.binOp .le (.col "total_rsf") (.litRat 0 1))
            (.binOp .le (.litRat 0 1) (.col "total_asf"))
            (.binOp .le (.binOp .mul (.col "required_min_rate") (.col "total_rsf")) (.col "total_asf")))) ]
    outputs :=
      [ ("total_asf", .col "total_asf")
      , ("total_rsf", .col "total_rsf")
      , ("net_deriv_rsf", .col "net_deriv_rsf")
      , ("deriv_addon_rsf", .col "deriv_addon_rsf")
      , ("obs_rsf", .col "obs_rsf")
      , ("compliant", .col "compliant") ] }

theorem obsFactor_ir_equiv (c : ObsExposureCategory) :
    evalSExpr [("category", .str (obsCategoryToStr c))] obsFactorExpr = .rat c.factor := by
  cases c <;> native_decide

theorem asfFactor_ir_equiv (item : AsfItem) :
    let customF : Rat := match item.category with
      | .cooperativeNetworkDeposit f => APS210.nonNeg (f.getD 0)
      | _ => 0
    let row : Row :=
      [ ("category", .str (asfCategoryToStr item.category))
      , ("custom_factor", .rat customF)
      , ("is_interdependent_approved", .bool item.isInterdependentApproved) ]
    evalSExpr row asfFactorExpr = .rat item.effectiveFactor := by
  have h0 : (0 : Rat) / 100 = 0 := by native_decide
  have h85 : (85 : Rat) / 100 = pct 85 := by native_decide
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
  cases hcat : item.category with
  | deferredTaxLiability m =>
    cases m <;> cases hinter : item.isInterdependentApproved <;>
    simp [AsfItem.effectiveFactor, AsfCategory.factor, asfCategoryToStr, asfFactorExpr,
      hcat, hinter, evalSExpr, evalCaseStr, Row.get, Val.asStr, Val.asBool] <;>
    native_decide
  | minorityInterest m =>
    cases m <;> cases hinter : item.isInterdependentApproved <;>
    simp [AsfItem.effectiveFactor, AsfCategory.factor, asfCategoryToStr, asfFactorExpr,
      hcat, hinter, evalSExpr, evalCaseStr, Row.get, Val.asStr, Val.asBool] <;>
    native_decide
  | cooperativeNetworkDeposit f =>
    cases hinter : item.isInterdependentApproved <;>
    simp [AsfItem.effectiveFactor, AsfCategory.factor, asfCategoryToStr, asfFactorExpr,
      hcat, hinter, evalSExpr, evalCaseStr, Row.get, Val.asStr, Val.asBool, Val.asRat, h85, hnonneg] <;>
    native_decide
  | _ =>
    cases hinter : item.isInterdependentApproved <;>
    simp [AsfItem.effectiveFactor, AsfCategory.factor, asfCategoryToStr, asfFactorExpr,
      hcat, hinter, evalSExpr, evalCaseStr, Row.get, Val.asStr, Val.asBool] <;>
    native_decide

theorem rsfBaseFactor_ir_equiv (c : RsfCategory) :
    let customF : Rat := match c with
      | .centralBankReservesAndBalances h => h.getD 0
      | .initialMarginAndCcpDefaultFund h _ => h.getD 0
      | _ => 0
    let onBehalf : Bool := match c with
      | .initialMarginAndCcpDefaultFund _ ob => ob
      | _ => false
    let row : Row :=
      [ ("category", .str (rsfCategoryToStr c))
      , ("custom_factor", .rat customF)
      , ("on_behalf_of_customer_no_guarantee", .bool onBehalf) ]
    evalSExpr row rsfBaseFactorExpr = .rat c.baseFactor := by
  cases c with
  | centralBankReservesAndBalances h =>
    have h0 : pct 0 = 0 := by native_decide
    have hmax : ∀ a b : Rat, max a b = max b a := by
      intro a b
      change (if a ≤ b then b else a) = (if b ≤ a then a else b)
      rcases @Rat.le_total a b with hab | hba
      · simp [hab]
        intro hba2
        exact Rat.le_antisymm hba2 hab
      · simp [hba]
        intro hab2
        exact Rat.le_antisymm hba hab2
    simp [RsfCategory.baseFactor, rsfCategoryToStr, rsfBaseFactorExpr,
      evalSExpr, evalCaseStr, Row.get, Val.asStr, Val.asRat, APS210.nonNeg, h0, hmax]
  | initialMarginAndCcpDefaultFund h ob =>
    have h85 : (85 : Rat) / 100 = pct 85 := by native_decide
    cases ob <;>
    simp [RsfCategory.baseFactor, rsfCategoryToStr, rsfBaseFactorExpr,
      evalSExpr, evalCaseStr, Row.get, Val.asStr, Val.asBool, Val.asRat, h85] <;>
    try native_decide
  | _ => native_decide

end Lean2Ibis.NSFRSpec
