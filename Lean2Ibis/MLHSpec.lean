import Lean2Ibis.IR
import APS210.MLH

namespace Lean2Ibis.MLHSpec

open APS210
open APS210.MLH

def mlhCategoryToStr : MlhAssetCategory → String
  | .notesAndCoinAndSettlementFunds => "notes_and_coin_and_settlement_funds"
  | .commonwealthAndSemiGovtSecurities => "commonwealth_and_semi_govt_securities"
  | .govtGuaranteedDebtSecurities => "govt_guaranteed_debt_securities"
  | .supranationalAndForeignGovtDebt => "supranational_and_foreign_govt_debt"
  | .adiBankBillsCdsAndDebt => "adi_bank_bills_cds_and_debt"
  | .netDepositsWithOtherAdisTwoBusinessDays => "net_deposits_with_other_adis_two_business_days"
  | .apraApprovedOtherSecurities => "apra_approved_other_securities"

def mlhAssetToRow (a : MlhAsset) : Row :=
  [ ("category", .str (mlhCategoryToStr a.category))
  , ("market_value", .rat a.marketValue)
  , ("unencumbered_or_apra_approved", .bool a.unencumberedOrApraApproved)
  , ("rba_repo_eligible", .bool a.rbaRepoEligible)
  , ("non_subordinated", .bool a.nonSubordinated)
  , ("is_debt_security", .bool a.isDebtSecurity)
  , ("liquidatable_within_two_business_days", .bool a.liquidatableWithinTwoBusinessDays)
  , ("liquidation_days_ok", .bool (match a.liquidationBusinessDays with | some d => d <= 2 | none => true)) ]

def mlhLiabilityToRow (l : MlhLiabilityItem) : Row :=
  [ ("is_on_balance_sheet_or_irrevocable", .bool l.isOnBalanceSheetOrIrrevocableCommitment)
  , ("apra_approved_exclusion", .bool l.apraApprovedExclusion)
  , ("amount", .rat l.amount) ]

/-- IR expression for `MlhAssetCategory.isDebtSecurity`. -/
def mlhIsDebtExpr : SExpr :=
  .caseStr (.col "category")
    [ ("commonwealth_and_semi_govt_securities", .litBool true)
    , ("govt_guaranteed_debt_securities", .litBool true)
    , ("supranational_and_foreign_govt_debt", .litBool true)
    , ("adi_bank_bills_cds_and_debt", .litBool true)
    , ("apra_approved_other_securities", .col "is_debt_security")
    , ("notes_and_coin_and_settlement_funds", .litBool false)
    , ("net_deposits_with_other_adis_two_business_days", .litBool false) ]
    (.litBool false)

/-- IR expression for `MlhAsset.eligibleValue`. -/
def mlhEligibleValueExpr : SExpr :=
  let debtOk := .binOp .or (.unOp .not mlhIsDebtExpr)
    (.binOp .and (.col "rba_repo_eligible") (.col "non_subordinated"))
  let eligible := .binOp .and (.col "unencumbered_or_apra_approved")
    (.binOp .and debtOk
      (.binOp .and (.col "liquidatable_within_two_business_days") (.col "liquidation_days_ok")))
  .ite eligible (.unOp .nonNeg (.col "market_value")) (.litRat 0 1)

/-- IR expression for `MlhLiabilityItem.includedAmount`. -/
def mlhLiabilityIncludedExpr : SExpr :=
  let included := .binOp .and (.col "is_on_balance_sheet_or_irrevocable")
    (.unOp .not (.col "apra_approved_exclusion"))
  .ite included (.unOp .nonNeg (.col "amount")) (.litRat 0 1)

/-- Complete APS 210 Attachment B (MLH) program in the Lean-to-Ibis IR. -/
def mlhProgram : Program :=
  { name := "evaluate_mlh"
    tableParams :=
      [ ("assets", ["category", "market_value", "unencumbered_or_apra_approved",
                    "rba_repo_eligible", "non_subordinated", "is_debt_security",
                    "liquidatable_within_two_business_days", "liquidation_days_ok"])
      , ("liabilities", ["is_on_balance_sheet_or_irrevocable", "apra_approved_exclusion", "amount"]) ]
    scalarParams := [("placements_by_other_adis", .rat), ("required_min_rate", .rat), ("trigger_ratio", .rat)]
    lets :=
      [ ("non_2f_assets", .aggSum
          (.filter (.unOp .not (.binOp .eq (.col "category") (.litStr "net_deposits_with_other_adis_two_business_days")))
            (.table "assets"))
          mlhEligibleValueExpr)
      , ("gross_2f_deposits", .aggSum
          (.filter (.binOp .eq (.col "category") (.litStr "net_deposits_with_other_adis_two_business_days"))
            (.table "assets"))
          mlhEligibleValueExpr)
      , ("net_2f_deposits", .scalar
          (.unOp .nonNeg (.binOp .sub (.col "gross_2f_deposits") (.unOp .nonNeg (.col "placements_by_other_adis")))))
      , ("eligible_liquid_assets", .scalar
          (.binOp .add (.col "non_2f_assets") (.col "net_2f_deposits")))
      , ("total_liabilities", .aggSum (.table "liabilities") mlhLiabilityIncludedExpr)
      , ("compliant", .scalar
          (.ite (.binOp .le (.col "total_liabilities") (.litRat 0 1))
            (.binOp .le (.litRat 0 1) (.col "eligible_liquid_assets"))
            (.binOp .le (.binOp .mul (.col "required_min_rate") (.col "total_liabilities")) (.col "eligible_liquid_assets"))))
      , ("trigger_ratio_valid", .scalar
          (.binOp .gt (.col "trigger_ratio") (.col "required_min_rate")))
      , ("above_trigger_ratio", .scalar
          (.ite (.binOp .le (.col "total_liabilities") (.litRat 0 1))
            (.binOp .le (.litRat 0 1) (.col "eligible_liquid_assets"))
            (.binOp .le (.binOp .mul (.col "trigger_ratio") (.col "total_liabilities")) (.col "eligible_liquid_assets")))) ]
    outputs :=
      [ ("eligible_liquid_assets", .col "eligible_liquid_assets")
      , ("total_liabilities", .col "total_liabilities")
      , ("compliant", .col "compliant")
      , ("trigger_ratio_valid", .col "trigger_ratio_valid")
      , ("above_trigger_ratio", .col "above_trigger_ratio") ] }

theorem mlhAsset_ir_equiv (a : MlhAsset) :
    evalSExpr (mlhAssetToRow a) mlhEligibleValueExpr = .rat a.eligibleValue := by
  have h0 : (0 : Rat) / 1 = 0 := by native_decide
  cases hcat : a.category <;>
  simp [mlhAssetToRow, mlhEligibleValueExpr, mlhIsDebtExpr, MlhAsset.eligibleValue,
    MlhAsset.isEligible, MlhAsset.requiresDebtCriteria, MlhAssetCategory.isDebtSecurity,
    mlhCategoryToStr, hcat,
    evalSExpr, evalCaseStr, Row.get, Val.asStr, Val.asBool, Val.asRat, h0] <;>
  cases a.unencumberedOrApraApproved <;>
  cases a.rbaRepoEligible <;>
  cases a.nonSubordinated <;>
  cases a.isDebtSecurity <;>
  cases a.liquidatableWithinTwoBusinessDays <;>
  cases a.liquidationBusinessDays <;>
  simp
  all_goals (
    rename_i val
    by_cases hval : val ≤ 2 <;> simp [hval]
  )

theorem mlhLiability_ir_equiv (l : MlhLiabilityItem) :
    evalSExpr (mlhLiabilityToRow l) mlhLiabilityIncludedExpr = .rat l.includedAmount := by
  have h0 : (0 : Rat) / 1 = 0 := by native_decide
  simp [mlhLiabilityToRow, mlhLiabilityIncludedExpr, MlhLiabilityItem.includedAmount,
    evalSExpr, Row.get, Val.asBool, Val.asRat, h0]
  cases l.isOnBalanceSheetOrIrrevocableCommitment <;>
  cases l.apraApprovedExclusion <;>
  simp

private theorem sumBy_foldl_ext {α : Type} (xs : List α) (f g : α → Rat)
    (h : ∀ x ∈ xs, f x = g x) :
    ∀ acc, xs.foldl (fun a x => a + f x) acc = xs.foldl (fun a x => a + g x) acc := by
  induction xs with
  | nil => intro acc; rfl
  | cons x xs ih =>
    intro acc
    simp only [List.foldl_cons]
    have hx : f x = g x := h x (by simp)
    rw [hx]
    exact ih (fun y hy => h y (by simp [hy])) _

private theorem sumBy_map {α β : Type} (xs : List α) (f : α → β) (g : β → Rat) :
    APS210.sumBy (xs.map f) g = APS210.sumBy xs (fun x => g (f x)) := by
  simp only [APS210.sumBy]
  generalize (0 : Rat) = acc
  induction xs generalizing acc with
  | nil => rfl
  | cons x xs ih => simp [List.foldl_cons, ih]

private theorem map_filter_comm {α β : Type} (xs : List α) (f : α → β) (p : α → Bool) (q : β → Bool)
    (hpq : ∀ x, q (f x) = p x) :
    (xs.map f).filter q = (xs.filter p).map f := by
  induction xs with
  | nil => rfl
  | cons x xs ih =>
    simp only [List.map_cons, List.filter_cons, hpq x]
    cases p x <;> simp [ih]

/-- Full program-level equivalence: `evalProgram mlhProgram` computes the exact same
    `eligibleLiquidAssets`, `totalLiabilities`, `compliant`, `triggerRatioValid`, and
    `aboveTriggerRatio` as `APS210.MLH.evaluateMlh` for EVERY `MlhInput` and `requiredMinRate`. -/
theorem mlhProgram_equiv_evaluateMlh (requiredMinRate : Rate) (input : MlhInput) :
    let tenv : TableEnv :=
      [ ("assets", input.assets.map mlhAssetToRow)
      , ("liabilities", input.liabilities.map mlhLiabilityToRow) ]
    let scalars : Row :=
      [ ("placements_by_other_adis", .rat input.placementsByOtherAdis)
      , ("required_min_rate", .rat requiredMinRate)
      , ("trigger_ratio", .rat input.triggerRatio) ]
    let out := evalProgram mlhProgram tenv scalars
    let ref := evaluateMlh requiredMinRate input
    out =
      [ ("eligible_liquid_assets", .rat ref.eligibleLiquidAssets)
      , ("total_liabilities", .rat ref.totalLiabilities)
      , ("compliant", .bool ref.ratioCheck.compliant)
      , ("trigger_ratio_valid", .bool ref.triggerRatioValid)
      , ("above_trigger_ratio", .bool ref.aboveTriggerRatio) ] := by
  intro tenv scalars out ref
  have h0 : (0 : Rat) / 1 = 0 := by native_decide
  have h_non2f_filter : ∀ a : MlhAsset,
      (evalSExpr (mlhAssetToRow a) (.unOp .not (.binOp .eq (.col "category") (.litStr "net_deposits_with_other_adis_two_business_days")))).asBool =
      (a.category != .netDepositsWithOtherAdisTwoBusinessDays) := by
    intro a; cases hcat : a.category <;> simp [mlhAssetToRow, mlhCategoryToStr, hcat, evalSExpr, Row.get, Val.asBool] <;> decide
  have h_2f_filter : ∀ a : MlhAsset,
      (evalSExpr (mlhAssetToRow a) (.binOp .eq (.col "category") (.litStr "net_deposits_with_other_adis_two_business_days"))).asBool =
      (a.category == .netDepositsWithOtherAdisTwoBusinessDays) := by
    intro a; cases hcat : a.category <;> simp [mlhAssetToRow, mlhCategoryToStr, hcat, evalSExpr, Row.get, Val.asBool] <;> decide
  have h_asset_val : ∀ (env : Row) (a : MlhAsset),
      (evalSExpr (mlhAssetToRow a ++ env) mlhEligibleValueExpr).asRat = a.eligibleValue := by
    intro env a
    cases hcat : a.category <;>
    simp [mlhAssetToRow, mlhEligibleValueExpr, mlhIsDebtExpr, MlhAsset.eligibleValue,
      MlhAsset.isEligible, MlhAsset.requiresDebtCriteria, MlhAssetCategory.isDebtSecurity,
      mlhCategoryToStr, hcat,
      evalSExpr, evalCaseStr, Row.get, Val.asStr, Val.asBool, Val.asRat, h0] <;>
    cases a.unencumberedOrApraApproved <;>
    cases a.rbaRepoEligible <;>
    cases a.nonSubordinated <;>
    cases a.isDebtSecurity <;>
    cases a.liquidatableWithinTwoBusinessDays <;>
    cases a.liquidationBusinessDays <;>
    simp
    all_goals (
      rename_i val
      by_cases hval : val ≤ 2 <;> simp [hval]
    )
  have h_liab_val : ∀ (env : Row) (l : MlhLiabilityItem),
      (evalSExpr (mlhLiabilityToRow l ++ env) mlhLiabilityIncludedExpr).asRat = l.includedAmount := by
    intro env l
    simp [mlhLiabilityToRow, mlhLiabilityIncludedExpr, MlhLiabilityItem.includedAmount,
      evalSExpr, Row.get, Val.asBool, Val.asRat, h0]
    cases l.isOnBalanceSheetOrIrrevocableCommitment <;>
    cases l.apraApprovedExclusion <;>
    simp
  have h_non2f : ∀ (env : Row),
      APS210.sumBy ((input.assets.map mlhAssetToRow).filter
        (fun row => (evalSExpr row (.unOp .not (.binOp .eq (.col "category") (.litStr "net_deposits_with_other_adis_two_business_days")))).asBool))
        (fun r => (evalSExpr (r ++ env) mlhEligibleValueExpr).asRat) =
      APS210.sumBy (input.assets.filter (·.category != .netDepositsWithOtherAdisTwoBusinessDays))
        MlhAsset.eligibleValue := by
    intro env
    have hflt :
        (input.assets.map mlhAssetToRow).filter
          (fun row => (evalSExpr row (.unOp .not (.binOp .eq (.col "category") (.litStr "net_deposits_with_other_adis_two_business_days")))).asBool) =
        (input.assets.filter (·.category != .netDepositsWithOtherAdisTwoBusinessDays)).map mlhAssetToRow :=
      map_filter_comm input.assets mlhAssetToRow _ _ h_non2f_filter
    rw [hflt, sumBy_map]
    exact sumBy_foldl_ext _ _ _ (fun a _ => h_asset_val env a) 0
  have h_2f : ∀ (env : Row),
      APS210.sumBy ((input.assets.map mlhAssetToRow).filter
        (fun row => (evalSExpr row (.binOp .eq (.col "category") (.litStr "net_deposits_with_other_adis_two_business_days"))).asBool))
        (fun r => (evalSExpr (r ++ env) mlhEligibleValueExpr).asRat) =
      APS210.sumBy (input.assets.filter (·.category == .netDepositsWithOtherAdisTwoBusinessDays))
        MlhAsset.eligibleValue := by
    intro env
    have hflt :
        (input.assets.map mlhAssetToRow).filter
          (fun row => (evalSExpr row (.binOp .eq (.col "category") (.litStr "net_deposits_with_other_adis_two_business_days"))).asBool) =
        (input.assets.filter (·.category == .netDepositsWithOtherAdisTwoBusinessDays)).map mlhAssetToRow :=
      map_filter_comm input.assets mlhAssetToRow _ _ h_2f_filter
    rw [hflt, sumBy_map]
    exact sumBy_foldl_ext _ _ _ (fun a _ => h_asset_val env a) 0
  have h_liab : ∀ (env : Row),
      APS210.sumBy (input.liabilities.map mlhLiabilityToRow)
        (fun r => (evalSExpr (r ++ env) mlhLiabilityIncludedExpr).asRat) =
      APS210.sumBy input.liabilities MlhLiabilityItem.includedAmount := by
    intro env
    rw [sumBy_map]
    exact sumBy_foldl_ext _ _ _ (fun l _ => h_liab_val env l) 0
  have ht_assets : tenv.get "assets" = input.assets.map mlhAssetToRow := rfl
  have ht_liab : tenv.get "liabilities" = input.liabilities.map mlhLiabilityToRow := rfl
  dsimp only [out, evalProgram, mlhProgram]
  simp only [List.foldl_cons, List.foldl_nil, evalRel, evalRelWithEnv, List.append_nil, ht_assets, ht_liab, h_non2f, h_2f, h_liab]
  simp [evalSExpr, Row.get, Val.asRat, Val.asBool, h0, scalars, ref, evaluateMlh, checkRatio]
  by_cases hle : (APS210.sumBy input.liabilities MlhLiabilityItem.includedAmount ≤ 0) <;>
  simp [hle]

end Lean2Ibis.MLHSpec
