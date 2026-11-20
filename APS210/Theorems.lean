import APS210.Basic
import APS210.Governance
import APS210.LCR.HQLA
import APS210.LCR.Outflows
import APS210.LCR.Inflows
import APS210.LCR
import APS210.MLH
import APS210.NSFR
import APS210.Compliance
import APS210.CashFlows

namespace APS210.Theorems

open APS210.Facts

open APS210.LCR
open APS210.NSFR
open APS210.CashFlows

private theorem rat_min_le_right (a b : Rat) : min a b ≤ b := by
  change (if a ≤ b then a else b) ≤ b
  split
  · assumption
  · exact Rat.le_refl

private theorem rat_le_max_right (a b : Rat) : b ≤ max a b := by
  change b ≤ (if a ≤ b then b else a)
  split
  · exact Rat.le_refl
  · next h =>
    rcases @Rat.le_total a b with hab | hba
    · exact absurd hab h
    · exact hba

private theorem rat_le_max_left (a b : Rat) : a ≤ max a b := by
  change a ≤ (if a ≤ b then b else a)
  split
  · assumption
  · exact Rat.le_refl

/-- Table 1 (Attachment A, para 40): The scorecard total is always at most 6. -/
theorem scorecard_le_six (above : Bool) (s : LessStableScorecardInput) : s.score above ≤ 6 := by
  simp only [LessStableScorecardInput.score]
  cases above <;>
  cases s.isOnlineAccount.affirmativeOrMissing <;>
  cases s.noEstablishedRelationship.affirmativeOrMissing <;>
  cases s.isHeavilyRateDriven.affirmativeOrMissing <;>
  decide

/-- Table 2 (Attachment A, para 40): Less stable run-off rate is 10% iff score <= 2, else 25%. -/
theorem lessStableRunoff_cases (score : Nat) :
    (score ≤ 2 → lessStableRunoffRate score = pct 10) ∧
    (score > 2 → lessStableRunoffRate score = pct 25) := by
  constructor
  · intro h
    simp [lessStableRunoffRate, h]
  · intro h
    have hnot : ¬ (score ≤ 2) := Nat.not_le_of_gt h
    simp [lessStableRunoffRate, hnot]

/-- Attachment A, Paragraph 56 & Table 4: Short-position-covering collateral always yields 0% inflow. -/
theorem shortPosition_zero_inflow (c : SecuredLendingCollateral) :
    securedLendingInflowRate c true = 0 := by
  have hpct0 : pct 0 = 0 := by native_decide
  simp [securedLendingInflowRate, hpct0]

/-- Attachment A, Paragraph 30: The 75% inflow cap ensures `effectiveCappedInflows <= 3/4 * grossOut`. -/
theorem inflow_cap_le_75pct (totalOutflows : Money) (inflows : List InflowItem)
    (overrideCap : Option Money) :
    (computeNetCashOutflows totalOutflows inflows overrideCap).effectiveCappedInflows ≤
      pct 75 * nonNeg totalOutflows := by
  simp only [computeNetCashOutflows]
  cases overrideCap with
  | none => exact rat_min_le_right _ _
  | some c =>
    have h1 := rat_min_le_right (sumBy inflows InflowItem.inflowAmount) (min (pct 75 * nonNeg totalOutflows) (nonNeg c))
    have h2 : min (pct 75 * nonNeg totalOutflows) (nonNeg c) ≤ pct 75 * nonNeg totalOutflows := by
      change (if pct 75 * nonNeg totalOutflows ≤ nonNeg c then pct 75 * nonNeg totalOutflows else nonNeg c) ≤ pct 75 * nonNeg totalOutflows
      split
      · exact Rat.le_refl
      · next h =>
        rcases @Rat.le_total (pct 75 * nonNeg totalOutflows) (nonNeg c) with hle | hge
        · exact absurd hle h
        · exact hge
    exact Rat.le_trans h1 h2

/-- Attachment A, Paragraph 30: `totalNetCashOutflows` is always non-negative. -/
theorem netCashOutflows_nonneg (totalOutflows : Money) (inflows : List InflowItem)
    (overrideCap : Option Money) :
    0 ≤ (computeNetCashOutflows totalOutflows inflows overrideCap).totalNetCashOutflows := by
  simp only [computeNetCashOutflows, nonNeg]
  exact rat_le_max_right _ _

/-- Main Body, Paragraph 55(a)–(b): Minimum All-Currencies LCR is 100% for locally incorporated
    LCR ADIs and 40% for foreign LCR ADIs (when no higher APRA minimum is set). -/
theorem default_lcr_minimums :
    ({ incorporation := .locallyIncorporated, classification := .lcr } : Governance.AdiProfile).requiredAllCurrenciesLcr = some (pct 100) ∧
    ({ incorporation := .foreign, classification := .lcr } : Governance.AdiProfile).requiredAllCurrenciesLcr = some (pct 40) := by
  native_decide

/-- Main Body, Paragraph 59: Foreign ADIs and MLH ADIs have no NSFR requirement. -/
theorem nsfr_only_local_lcr :
    ({ incorporation := .foreign, classification := .lcr } : Governance.AdiProfile).requiredNsfr = none ∧
    ({ incorporation := .locallyIncorporated, classification := .mlh } : Governance.AdiProfile).requiredNsfr = none := by
  native_decide

/-- Attachment C, Paragraph 23: Without exceptional CB approval, encumbrance >= 1 year always yields 100% RSF,
    and encumbrance 6m..<1yr yields at least 50% RSF. -/
theorem encumbrance_rsf_bounds (base : Rate) :
    applyEncumbranceFactor base .oneYearOrMore none = pct 100 ∧
    pct 50 ≤ applyEncumbranceFactor base .sixMonthsToLessThanOneYear none := by
  constructor
  · rfl
  · unfold applyEncumbranceFactor
    exact rat_le_max_left _ _

/-- Attachment C, Paragraphs 38–40: Interdependent assets and liabilities receive 0% factor. -/
theorem interdependent_zero_factor (asf : AsfItem) (rsf : RsfAssetItem)
    (h1 : asf.isInterdependentApproved = true)
    (h2 : rsf.isInterdependentApproved = true) :
    asf.effectiveFactor = 0 ∧ rsf.effectiveFactor = 0 := by
  have hpct0 : pct 0 = 0 := by native_decide
  simp [AsfItem.effectiveFactor, RsfAssetItem.effectiveFactor, h1, h2, hpct0]

/-- Attachment A, Paragraph 6–8: `stockExClf` is always non-negative. -/
theorem hqla_stock_nonneg (u1 u2a u2b : Money) (uw : HqlaUnwindAdjustments) :
    0 ≤ (computeHqlaCaps u1 u2a u2b uw).stockExClf := by
  simp only [computeHqlaCaps, nonNeg]
  exact rat_le_max_right _ _

/-- Attachment A, Paragraph 59: Foreign ADI head office facility contribution never exceeds
    50% of the LCR requirement (for any non-negative minimum LCR rate). -/
theorem foreign_ho_facility_le_50pct
    (minRate : Rate) (hmin : 0 ≤ minRate) (input : LcrInput) :
    (evaluateLcr .foreign false minRate input).recognisedHeadOfficeFacility ≤
      pct 50 * (minRate * (evaluateLcr .foreign false minRate input).netCashOutflows.totalNetCashOutflows) := by
  simp only [evaluateLcr]
  cases input.headOfficeFacility with
  | none =>
    have hpct50 : (0 : Rat) ≤ pct 50 := by native_decide
    exact Rat.mul_nonneg hpct50 (Rat.mul_nonneg hmin (netCashOutflows_nonneg _ _ _))
  | some f => exact rat_min_le_right _ _

/-- Top-level soundness: `overallCompliant = true` implies both qualitative and quantitative compliance. -/
theorem overallCompliant_implies_both (input : AdiComplianceInput)
    (h : (evaluateAdiCompliance input).overallCompliant = true) :
    (evaluateAdiCompliance input).qualitativeCompliant = true ∧
    (evaluateAdiCompliance input).quantitativeCompliant = true := by
  simp only [evaluateAdiCompliance, Bool.and_eq_true] at h ⊢
  exact h

private theorem rat_min_le_left (a b : Rat) : min a b ≤ a := by
  change (if a ≤ b then a else b) ≤ a
  split
  · exact Rat.le_refl
  · next h =>
    rcases @Rat.le_total a b with hab | hba
    · exact absurd hab h
    · exact hba

private theorem rat_sub_max_zero_le (x y : Rat) : x - max (x - y) 0 ≤ y := by
  change x - (if x - y ≤ 0 then 0 else x - y) ≤ y
  split
  · next h =>
    have h1 : (x - y) + y ≤ 0 + y := Rat.add_le_add_right.mpr h
    have hx : (x - y) + y = x := by
      rw [Rat.sub_eq_add_neg, Rat.add_assoc, Rat.neg_add_cancel, Rat.add_zero]
    rw [hx, Rat.zero_add] at h1
    rw [Rat.sub_eq_add_neg, Rat.neg_zero, Rat.add_zero]
    exact h1
  · have hxy : x - (x - y) = y := by
      rw [Rat.sub_eq_add_neg, Rat.sub_eq_add_neg, Rat.neg_add, Rat.neg_neg, ← Rat.add_assoc, Rat.add_neg_cancel, Rat.zero_add]
    rw [hxy]
    exact Rat.le_refl

private theorem cap15_alg (a b : Rat) (h : b ≤ (15 : Rat) / 85 * a) :
    b ≤ (15 : Rat) / 100 * (a + b) := by
  have h85 : (85 : Rat) * b ≤ 15 * a := by
    have hm := Rat.mul_le_mul_of_nonneg_left h (show (0 : Rat) ≤ 85 by native_decide)
    rw [← Rat.mul_assoc] at hm
    have hc : (85 : Rat) * ((15 : Rat) / 85) = 15 := by native_decide
    rw [hc] at hm
    exact hm
  have h100 : (100 : Rat) * b ≤ 15 * (a + b) := by
    have hsplit : (100 : Rat) * b = 85 * b + 15 * b := by
      rw [← Rat.add_mul]
      have hc : (85 : Rat) + 15 = 100 := by native_decide
      rw [hc]
    rw [hsplit, Rat.mul_add]
    exact Rat.add_le_add_right.mpr h85
  have hdiv := Rat.mul_le_mul_of_nonneg_left h100 (show (0 : Rat) ≤ (1 : Rat) / 100 by native_decide)
  rw [← Rat.mul_assoc, ← Rat.mul_assoc] at hdiv
  have hl : ((1 : Rat) / 100) * 100 = 1 := by native_decide
  have hr : ((1 : Rat) / 100) * 15 = (15 : Rat) / 100 := by native_decide
  rw [hl, hr, Rat.one_mul] at hdiv
  exact hdiv

private theorem cap40_alg (a b : Rat) (h : b ≤ (2 : Rat) / 3 * a) :
    b ≤ (40 : Rat) / 100 * (a + b) := by
  have h3 : (3 : Rat) * b ≤ 2 * a := by
    have hm := Rat.mul_le_mul_of_nonneg_left h (show (0 : Rat) ≤ 3 by native_decide)
    rw [← Rat.mul_assoc] at hm
    have hc : (3 : Rat) * ((2 : Rat) / 3) = 2 := by native_decide
    rw [hc] at hm
    exact hm
  have h5 : (5 : Rat) * b ≤ 2 * (a + b) := by
    have hsplit : (5 : Rat) * b = 3 * b + 2 * b := by
      rw [← Rat.add_mul]
      have hc : (3 : Rat) + 2 = 5 := by native_decide
      rw [hc]
    rw [hsplit, Rat.mul_add]
    exact Rat.add_le_add_right.mpr h3
  have hdiv := Rat.mul_le_mul_of_nonneg_left h5 (show (0 : Rat) ≤ (1 : Rat) / 5 by native_decide)
  rw [← Rat.mul_assoc, ← Rat.mul_assoc] at hdiv
  have hl : ((1 : Rat) / 5) * 5 = 1 := by native_decide
  have hr : ((1 : Rat) / 5) * 2 = (40 : Rat) / 100 := by native_decide
  rw [hl, hr, Rat.one_mul] at hdiv
  exact hdiv

/-- Attachment A, Paragraph 6–8: The `15/85` formula in `computeHqlaCaps` enforces that
    capped HQLA2B never exceeds 15% of post-15%-cap total HQLA (`adj1 + adj2a + capped2b`). -/
theorem hqla_cap15_bound (u1 u2a u2b : Money) (uw : HqlaUnwindAdjustments) :
    let b := computeHqlaCaps u1 u2a u2b uw
    let capped2b := b.adjHqla2b - b.excess15Cap
    capped2b ≤ pct 15 * (b.adjHqla1 + b.adjHqla2a + capped2b) := by
  dsimp only [computeHqlaCaps, nonNeg, pct]
  exact cap15_alg _ _ (Rat.le_trans (rat_sub_max_zero_le _ _) (rat_min_le_left _ _))

/-- Attachment A, Paragraph 6–8: The `2/3` formula in `computeHqlaCaps` enforces that
    capped HQLA2 (`adj2After15 - excess40Cap`) never exceeds 40% of post-cap total HQLA. -/
theorem hqla_cap40_bound (u1 u2a u2b : Money) (uw : HqlaUnwindAdjustments) :
    let b := computeHqlaCaps u1 u2a u2b uw
    let capped2 := b.adjHqla2a + (b.adjHqla2b - b.excess15Cap) - b.excess40Cap
    capped2 ≤ pct 40 * (b.adjHqla1 + capped2) := by
  dsimp only [computeHqlaCaps, nonNeg, pct]
  exact cap40_alg _ _ (rat_sub_max_zero_le _ _)

/-- Attachment A, Paragraph 15(a)–(c) & 17: Unapproved CLF yields 0, and recognised CLF
    is always non-negative and bounded by both contractual amount and collateral after RBA margins. -/
theorem clf_recognised_bounds (clf : ClfInput) :
    0 ≤ clf.recognisedAmount ∧
    clf.recognisedAmount ≤ nonNeg clf.contractualAmount ∧
    clf.recognisedAmount ≤
      nonNeg clf.collateralHeldAfterRbaMargins + nonNeg clf.recentlyIneligibleCollateralAfterRbaMargins ∧
    (clf.approvedByApra = false → clf.recognisedAmount = 0) := by
  simp only [ClfInput.recognisedAmount]
  cases h : clf.approvedByApra
  · have hnn_col : 0 ≤ nonNeg clf.collateralHeldAfterRbaMargins + nonNeg clf.recentlyIneligibleCollateralAfterRbaMargins :=
      Rat.add_nonneg (rat_le_max_right _ _) (rat_le_max_right _ _)
    refine ⟨Rat.le_refl, rat_le_max_right _ _, hnn_col, fun _ => rfl⟩
  · simp only [Bool.not_true, Bool.false_eq_true, ↓reduceIte]
    cases hp : clf.processEligible
    · have hnn_col : 0 ≤ nonNeg clf.collateralHeldAfterRbaMargins + nonNeg clf.recentlyIneligibleCollateralAfterRbaMargins :=
        Rat.add_nonneg (rat_le_max_right _ _) (rat_le_max_right _ _)
      refine ⟨Rat.le_refl, rat_le_max_right _ _, hnn_col, fun hfalse => Bool.noConfusion hfalse⟩
    · simp only [Bool.not_true, Bool.false_eq_true, ↓reduceIte]
      let graceOk : Bool := match clf.recentlyIneligibleCollateralDays with
        | .known d => decide (d ≤ 30)
        | _ => false
      let c := nonNeg clf.collateralHeldAfterRbaMargins +
        (if graceOk = true then nonNeg clf.recentlyIneligibleCollateralAfterRbaMargins else 0)
      have ha : 0 ≤ nonNeg clf.contractualAmount := rat_le_max_right _ _
      have hb : 0 ≤ nonNeg (clf.contractualAmount - nonNeg clf.pledgedUnderZeroRunoffSecFunding + nonNeg clf.receivedUnderZeroInflowSecLending) :=
        rat_le_max_right _ _
      have hc_grace : 0 ≤ (if graceOk = true then nonNeg clf.recentlyIneligibleCollateralAfterRbaMargins else 0) := by
        split
        · exact rat_le_max_right _ _
        · exact Rat.le_refl
      have hc : 0 ≤ c := Rat.add_nonneg (rat_le_max_right _ _) hc_grace
      have hmin_bc_nonneg : 0 ≤ min (nonNeg (clf.contractualAmount - nonNeg clf.pledgedUnderZeroRunoffSecFunding + nonNeg clf.receivedUnderZeroInflowSecLending)) c := by
        change 0 ≤ (if _ ≤ _ then _ else _)
        split <;> assumption
      have hmin_abc_nonneg : 0 ≤ min (nonNeg clf.contractualAmount) (min (nonNeg (clf.contractualAmount - nonNeg clf.pledgedUnderZeroRunoffSecFunding + nonNeg clf.receivedUnderZeroInflowSecLending)) c) := by
        change 0 ≤ (if _ ≤ _ then _ else _)
        split <;> assumption
      have hle_a : min (nonNeg clf.contractualAmount) (min (nonNeg (clf.contractualAmount - nonNeg clf.pledgedUnderZeroRunoffSecFunding + nonNeg clf.receivedUnderZeroInflowSecLending)) c) ≤ nonNeg clf.contractualAmount :=
        rat_min_le_left _ _
      have hc_le : c ≤ nonNeg clf.collateralHeldAfterRbaMargins + nonNeg clf.recentlyIneligibleCollateralAfterRbaMargins := by
        dsimp only [c]
        rw [Rat.add_le_add_left]
        split
        · exact Rat.le_refl
        · exact rat_le_max_right _ _
      have hle_c : min (nonNeg clf.contractualAmount) (min (nonNeg (clf.contractualAmount - nonNeg clf.pledgedUnderZeroRunoffSecFunding + nonNeg clf.receivedUnderZeroInflowSecLending)) c) ≤
          nonNeg clf.collateralHeldAfterRbaMargins + nonNeg clf.recentlyIneligibleCollateralAfterRbaMargins :=
        Rat.le_trans (Rat.le_trans (rat_min_le_right _ _) (rat_min_le_right _ _)) hc_le
      exact ⟨hmin_abc_nonneg, hle_a, hle_c, fun hfalse => Bool.noConfusion hfalse⟩

/-- Table 2 (Attachment A, para 40): Higher scorecard risk never reduces the less-stable run-off rate. -/
theorem lessStableRunoff_monotone {s1 s2 : Nat} (h : s1 ≤ s2) :
    lessStableRunoffRate s1 ≤ lessStableRunoffRate s2 := by
  by_cases h1 : s1 ≤ 2 <;> by_cases h2 : s2 ≤ 2
  · simp [lessStableRunoffRate, h1, h2]
  · simp [lessStableRunoffRate, h1, h2]
    native_decide
  · exact absurd (Nat.le_trans h h2) h1
  · simp [lessStableRunoffRate, h1, h2]

-- ============================================================================
-- Cash-Flow Conservation, Filtering, & Derivative Netting Theorems
-- ============================================================================

private theorem foldl_add_shift {α : Type} (xs : List α) (f : α → Rat) (init : Rat) :
    xs.foldl (fun acc x => acc + f x) init = init + xs.foldl (fun acc x => acc + f x) 0 := by
  induction xs generalizing init with
  | nil => simp [Rat.add_zero]
  | cons x rest ih =>
    simp only [List.foldl_cons, Rat.zero_add]
    rw [ih (init + f x), ih (f x), Rat.add_assoc]

private theorem sumBy_cons {α : Type} (x : α) (xs : List α) (f : α → Rat) :
    sumBy (x :: xs) f = f x + sumBy xs f := by
  simp only [sumBy, List.foldl_cons, Rat.zero_add]
  exact foldl_add_shift xs f (f x)

private theorem sumBy_split2 {α : Type} (xs : List α) (p : α → Bool) (f : α → Rat) :
    sumBy (xs.filter p) f + sumBy (xs.filter (fun x => !p x)) f = sumBy xs f := by
  induction xs with
  | nil => simp [sumBy, Rat.add_zero]
  | cons x rest ih =>
    by_cases hp : p x = true
    · simp only [List.filter_cons, hp, Bool.not_true, Bool.false_eq_true, ↓reduceIte]
      rw [sumBy_cons, sumBy_cons, Rat.add_assoc, ih]
    · have hpf : p x = false := Bool.eq_false_iff.mpr hp
      simp only [List.filter_cons, hpf, Bool.false_eq_true, Bool.not_false, ↓reduceIte]
      rw [sumBy_cons, sumBy_cons]
      have hcomm : sumBy (rest.filter p) f + (f x + sumBy (rest.filter (fun y => !p y)) f) =
                   f x + (sumBy (rest.filter p) f + sumBy (rest.filter (fun y => !p y)) f) := by
        rw [← Rat.add_assoc, Rat.add_comm (sumBy (rest.filter p) f) (f x), Rat.add_assoc]
      rw [hcomm, ih]

private theorem sumBy_split3 {α : Type} (xs : List α) (c : α → NSFR.NsfrMaturityBucket) (f : α → Rat) :
    sumBy (xs.filter (fun x => c x == .lessThanSixMonths)) f +
    sumBy (xs.filter (fun x => c x == .sixMonthsToLessThanOneYear)) f +
    sumBy (xs.filter (fun x => c x == .oneYearOrMore)) f = sumBy xs f := by
  induction xs with
  | nil => simp [sumBy, Rat.add_zero]
  | cons x rest ih =>
    cases hc : c x
    · have hfl : (List.filter (fun y => c y == .lessThanSixMonths) (x :: rest)) =
          x :: List.filter (fun y => c y == .lessThanSixMonths) rest := by simp [hc]
      have hfm : (List.filter (fun y => c y == .sixMonthsToLessThanOneYear) (x :: rest)) =
          List.filter (fun y => c y == .sixMonthsToLessThanOneYear) rest := by simp [hc]
      have hfh : (List.filter (fun y => c y == .oneYearOrMore) (x :: rest)) =
          List.filter (fun y => c y == .oneYearOrMore) rest := by simp [hc]
      rw [hfl, hfm, hfh, sumBy_cons, sumBy_cons, Rat.add_assoc, Rat.add_assoc,
          ← Rat.add_assoc (sumBy (List.filter (fun y => c y == .lessThanSixMonths) rest) f), ih]
    · have hfl : (List.filter (fun y => c y == .lessThanSixMonths) (x :: rest)) =
          List.filter (fun y => c y == .lessThanSixMonths) rest := by simp [hc]
      have hfm : (List.filter (fun y => c y == .sixMonthsToLessThanOneYear) (x :: rest)) =
          x :: List.filter (fun y => c y == .sixMonthsToLessThanOneYear) rest := by simp [hc]
      have hfh : (List.filter (fun y => c y == .oneYearOrMore) (x :: rest)) =
          List.filter (fun y => c y == .oneYearOrMore) rest := by simp [hc]
      rw [hfl, hfm, hfh, sumBy_cons, sumBy_cons]
      rw [← Rat.add_assoc (sumBy (List.filter (fun y => c y == .lessThanSixMonths) rest) f),
          Rat.add_comm (sumBy (List.filter (fun y => c y == .lessThanSixMonths) rest) f) (f x),
          Rat.add_assoc (f x), Rat.add_assoc (f x), ih]
    · have hfl : (List.filter (fun y => c y == .lessThanSixMonths) (x :: rest)) =
          List.filter (fun y => c y == .lessThanSixMonths) rest := by simp [hc]
      have hfm : (List.filter (fun y => c y == .sixMonthsToLessThanOneYear) (x :: rest)) =
          List.filter (fun y => c y == .sixMonthsToLessThanOneYear) rest := by simp [hc]
      have hfh : (List.filter (fun y => c y == .oneYearOrMore) (x :: rest)) =
          x :: List.filter (fun y => c y == .oneYearOrMore) rest := by simp [hc]
      rw [hfl, hfm, hfh, sumBy_cons, sumBy_cons]
      rw [← Rat.add_assoc (sumBy (List.filter (fun y => c y == .lessThanSixMonths) rest) f +
            sumBy (List.filter (fun y => c y == .sixMonthsToLessThanOneYear) rest) f) (f x),
          Rat.add_comm (sumBy (List.filter (fun y => c y == .lessThanSixMonths) rest) f +
            sumBy (List.filter (fun y => c y == .sixMonthsToLessThanOneYear) rest) f) (f x),
          Rat.add_assoc (f x), ih]

/-- Attachment C, Paragraph 22: Splitting an amortising loan across `<1y` and `>=1y` RSF buckets
    exact-conserves total carrying value. -/
theorem splitAmortisingLoanForRsf_conserves_principal
    (idPrefix : String) (lt1yCat ge1yCat : NSFR.RsfCategory)
    (legs : List (Money × Nat))
    (enc : NSFR.EncumbrancePeriod) (interdep : Bool) (obs : ObservationContext) :
    sumBy (splitAmortisingLoanForRsf idPrefix lt1yCat ge1yCat legs enc interdep none obs) (·.carryingValue) =
      sumBy legs (fun (a, _) => nonNeg a) := by
  simp only [splitAmortisingLoanForRsf, sumBy, List.foldl_cons, List.foldl_nil, Rat.zero_add]
  have h1 : legs.filter (fun (_, d) => d ≥ 365) = legs.filter (fun x => !(decide (x.2 < 365))) := by
    congr 1
    funext ⟨_, d⟩
    by_cases h : d < 365
    · simp [h, Nat.not_le_of_gt h]
    · simp [h, Nat.not_lt.mp h]
  rw [h1]
  exact sumBy_split2 legs (fun x => decide (x.2 < 365)) (fun (a, _) => nonNeg a)

/-- Attachment C, Paragraphs 8 & 11(c): Splitting liability cash flows across `<6m`, `6m..<1y`,
    and `>=1y` ASF buckets exact-conserves total carrying value. -/
theorem splitLiabilityCashFlowsForAsf_conserves_carrying
    (idPrefix : String) (lt6mCat sixTo1yCat ge1yCat : NSFR.AsfCategory)
    (legs : List (Money × Nat)) (interdep : Bool) (obs : ObservationContext) :
    sumBy (splitLiabilityCashFlowsForAsf idPrefix lt6mCat sixTo1yCat ge1yCat legs interdep none obs) (·.carryingValue) =
      sumBy legs (fun (a, _) => nonNeg a) := by
  simp only [splitLiabilityCashFlowsForAsf, sumBy, List.foldl_cons, List.foldl_nil, Rat.zero_add]
  exact sumBy_split3 legs (fun (_, d) => classifyNsfrDays d) (fun (a, _) => nonNeg a)

/-- Attachment A, Paragraphs 31 & 66: Cash inflows from HQLA assets or non-financial revenues
    are never eligible as LCR inflows. -/
theorem cashFlow_hqla_and_nonFinRev_excluded (f : ContractualCashFlow) :
    (f.countedInHqla = true → f.isEligibleLcrInflow = false) ∧
    (f.isNonFinancialRevenue = true → f.isEligibleLcrInflow = false) := by
  constructor <;> intro h <;> simp [ContractualCashFlow.isEligibleLcrInflow, h]

/-- Attachment C, Paragraph 37(b): `netDerivativeRsf` is always non-negative. -/
theorem nsfr_netDerivativeRsf_nonneg (d : NsfrDerivativeInput) :
    0 ≤ d.netDerivativeRsf := by
  simp only [NsfrDerivativeInput.netDerivativeRsf, nonNeg]
  exact rat_le_max_right _ _

-- ============================================================================
-- Universal Regulatory Condition, Upstream & Monotonicity Invariants
-- ============================================================================

/-- Attachment A, Paragraphs 16–21: If any CLF process/operational/diversification
    condition fails (`processEligible = false`), recognised CLF is 0. -/
theorem clf_processEligible_false_recognised_zero (clf : ClfInput)
    (h : clf.processEligible = false) :
    clf.recognisedAmount = 0 := by
  simp [ClfInput.recognisedAmount, h]

/-- Attachment A, Paragraphs 22–25: Failing any operational requirement zeroes HQLA asset value. -/
theorem hqla_operational_false_zero (basis : ConsolidationBasis) (a : HqlaAsset)
    (h : a.operational.isMet = false) :
    a.valueAfterHaircut basis = 0 := by
  simp [HqlaAsset.valueAfterHaircut, HqlaAsset.eligibleMarketValue, h]

/-- Attachment A, Paragraph 29: Non-transferable HQLA in a group with no Level 2 cap contributes 0 at
    Level 2, whatever the rest of the portfolio holds. -/
theorem hqla_level2_nontransferable_zero_cap (assets : List HqlaAsset) (caps : List (String × Money))
    (a : HqlaAsset)
    (hf : a.freelyTransferableAtLevel2 = false)
    (hc : caps.filter (fun (k, _) => k == a.level2TransferGroup) = []) :
    level2TransferScale .level2 assets caps a = 0 := by
  simp only [level2TransferScale, hf, lowestCapFor, hc]
  split
  · rfl
  · grind

/-- Attachment A, Paragraph 29: The Level 2 scale factor never exceeds 1, so the group cap can only
    reduce recognised HQLA. -/
theorem level2TransferScale_le_one (basis : ConsolidationBasis) (assets : List HqlaAsset)
    (caps : List (String × Money)) (a : HqlaAsset) :
    level2TransferScale basis assets caps a ≤ 1 := by
  simp only [level2TransferScale]
  split
  · split
    · native_decide
    · exact rat_min_le_left _ _
  · exact Rat.le_refl

/-- Attachment A, Paragraph 25: With an unknown day count there is no grace period. -/
theorem hqla_unknown_ineligible_days_no_grace (a : HqlaAsset) (reason : String)
    (h : a.daysSinceIneligible = .unknown reason) : a.graceOk = false := by
  simp [HqlaAsset.graceOk, h]

/-- Attachment A, Paragraphs 10 & 12: An HQLA2 asset with an unknown stress observation fails the
    stress criterion. -/
theorem hqla2_unknown_stress_fails (a : HqlaAsset) (reason : String)
    (hcat : a.category ≠ .hqla1) (h : a.max30dPriceDecline = .unknown reason) : a.stressOk = false := by
  unfold HqlaAsset.stressOk
  cases hc : a.category <;> simp_all

/-- Upstream schema invariant (Attachment A Paragraph 17): without current annual re-approval,
    upstream recognised CLF is always 0. -/
theorem upstream_clf_requires_reapproval (input : Upstream.UpstreamInput)
    (h : input.clf.annualReapprovalCurrent = false) :
    (Upstream.evaluateUpstream input).clfRecognisedAmount = 0 := by
  simp [Upstream.evaluateUpstream, Upstream.ClfSummaryInput.recognised,
    Upstream.ClfSummaryInput.toLcrClfInput, LCR.ClfInput.recognisedAmount,
    LCR.ClfInput.processEligible, h]

/-- Upstream schema invariant (Attachment A Paragraphs 17–20): without APRA approval,
    upstream recognised CLF is always 0. -/
theorem upstream_clf_requires_apra_approval (input : Upstream.UpstreamInput)
    (h : input.clf.approvedByApra = false) :
    (Upstream.evaluateUpstream input).clfRecognisedAmount = 0 := by
  simp [Upstream.evaluateUpstream, Upstream.ClfSummaryInput.recognised,
    Upstream.ClfSummaryInput.toLcrClfInput, LCR.ClfInput.recognisedAmount, h]

/-- Upstream schema invariant (Attachment A Paragraph 16(b)): without collateral diversification,
    upstream recognised CLF is always 0. -/
theorem upstream_clf_requires_diversification (input : Upstream.UpstreamInput)
    (h : input.clf.collateralDiversified = false) :
    (Upstream.evaluateUpstream input).clfRecognisedAmount = 0 := by
  simp [Upstream.evaluateUpstream, Upstream.ClfSummaryInput.recognised,
    Upstream.ClfSummaryInput.toLcrClfInput, LCR.ClfInput.recognisedAmount,
    LCR.ClfInput.processEligible, h]

/-- Monotonicity of ratio compliance: increasing denominator (`den₁ ≤ den₂`, `0 < den₁`, `0 ≤ minRate`)
    never makes a non-compliant ratio compliant (more outflows never raise LCR compliance; more RSF never raises NSFR compliance). -/
theorem checkRatio_monotone_den (num den₁ den₂ minRate : Rat)
    (hm : 0 ≤ minRate) (hd1 : 0 < den₁) (hle : den₁ ≤ den₂)
    (hc : (checkRatio num den₂ minRate).compliant = true) :
    (checkRatio num den₁ minRate).compliant = true := by
  have hnot1 : ¬(den₁ ≤ 0) := Rat.not_le.mpr hd1
  have hnot2 : ¬(den₂ ≤ 0) := fun h2 => hnot1 (Rat.le_trans hle h2)
  simp only [checkRatio, hnot1, hnot2, ↓reduceIte, decide_eq_true_eq] at hc ⊢
  exact Rat.le_trans (Rat.mul_le_mul_of_nonneg_left hle hm) hc

-- ============================================================================
-- Portfolio Permutation Invariance, Calendar Windows, Shared Limits,
-- Eligibility, No Double-Counting, Monotonicity & Compliance Aggregation
-- ============================================================================

/-- Permutation invariance of `sumBy`: reordering items never changes the sum. -/
theorem sumBy_perm {α : Type} {xs ys : List α} (f : α → Rat) (h : xs.Perm ys) :
    sumBy xs f = sumBy ys f := by
  induction h with
  | nil => rfl
  | cons x _ ih => simp only [sumBy_cons, ih]
  | swap x y zs =>
    simp only [sumBy_cons, ← Rat.add_assoc, Rat.add_comm (f y) (f x)]
  | trans _ _ ih1 ih2 => exact ih1.trans ih2

/-- Attachment B (MLH): Reordering MLH assets or liabilities leaves every `evaluateMlh` figure unchanged
    (only the order of reported data issues can differ). -/
theorem evaluateMlh_perm (minRate : Rate) (asOf : ObservationContext)
    {a1 a2 : List MLH.MlhAsset} {l1 l2 : List MLH.MlhLiabilityItem}
    (ha : a1.Perm a2) (hl : l1.Perm l2)
    (placements : Money) (trigger : Rate) :
    { MLH.evaluateMlh minRate { asOf, assets := a1, placementsByOtherAdis := placements, liabilities := l1, triggerRatio := trigger } with dataIssues := [] } =
    { MLH.evaluateMlh minRate { asOf, assets := a2, placementsByOtherAdis := placements, liabilities := l2, triggerRatio := trigger } with dataIssues := [] } := by
  simp only [MLH.evaluateMlh]
  have hnon2f : sumBy (a1.filter (·.category != .netDepositsWithOtherAdisTwoBusinessDays)) MLH.MlhAsset.eligibleValue =
      sumBy (a2.filter (·.category != .netDepositsWithOtherAdisTwoBusinessDays)) MLH.MlhAsset.eligibleValue :=
    sumBy_perm _ (ha.filter _)
  have h2f : sumBy (a1.filter (·.category == .netDepositsWithOtherAdisTwoBusinessDays)) MLH.MlhAsset.eligibleValue =
      sumBy (a2.filter (·.category == .netDepositsWithOtherAdisTwoBusinessDays)) MLH.MlhAsset.eligibleValue :=
    sumBy_perm _ (ha.filter _)
  have hliab : sumBy l1 MLH.MlhLiabilityItem.includedAmount =
      sumBy l2 MLH.MlhLiabilityItem.includedAmount :=
    sumBy_perm _ hl
  rw [hnon2f, h2f, hliab]

/-- APG 210: A customer's aggregate balance and guarantee limit do not depend on deposit order. -/
theorem customerGuaranteePosition_perm {r1 r2 : List RetailDeposit} (h : r1.Perm r2) (c : String) (cur : Currency) :
    customerGuaranteePosition r1 c cur = customerGuaranteePosition r2 c cur := by
  simp only [customerGuaranteePosition]
  rw [sumBy_perm _ (h.filter _), (h.filter _).foldl_eq' (by intro x _ y _ z; cases z <;> simp <;> grind) none]

/-- Attachment A (LCR): Reordering retail deposits or other outflows leaves total expected cash outflows unchanged. -/
theorem computeTotalExpectedCashOutflows_perm
    {r1 r2 : List RetailDeposit} {o1 o2 : List OutflowItem}
    (hr : r1.Perm r2) (ho : o1.Perm o2) (basis : ConsolidationBasis) :
    computeTotalExpectedCashOutflows r1 o1 basis =
    computeTotalExpectedCashOutflows r2 o2 basis := by
  simp only [computeTotalExpectedCashOutflows]
  have hfun : RetailDeposit.outflowAmount basis r1 = RetailDeposit.outflowAmount basis r2 := by
    funext d
    simp only [RetailDeposit.outflowAmount, customerGuaranteePosition_perm hr]
  rw [hfun, sumBy_perm _ hr, sumBy_perm _ ho]

/-- Attachment A (LCR): Reordering inflows leaves `computeNetCashOutflows` unchanged. -/
theorem computeNetCashOutflows_perm (totalOutflows : Money)
    {i1 i2 : List InflowItem} (hi : i1.Perm i2) (overrideCap : Option Money) :
    computeNetCashOutflows totalOutflows i1 overrideCap =
    computeNetCashOutflows totalOutflows i2 overrideCap := by
  simp only [computeNetCashOutflows, sumBy_perm _ hi]

/-- Attachment C (NSFR): Reordering ASF items or OBS exposures leaves `evaluateNsfr` unchanged. -/
theorem evaluateNsfr_asf_obs_perm (minRate : Rate) (asOf : ObservationContext)
    {asf1 asf2 : List AsfItem} {obs1 obs2 : List ObsExposureItem}
    (hasf : asf1.Perm asf2) (hobs : obs1.Perm obs2)
    (rsf : List RsfAssetItem) (deriv : NsfrDerivativeInput)
    (clfLim : Option Money) (hostAla : Money) (buf : Money) :
    { evaluateNsfr minRate
      { asOf, asfItems := asf1, rsfAssets := rsf, obsExposures := obs1, derivatives := deriv,
        recognisedClfLimit := clfLim, hostAlaInLcrLimit := hostAla, internalNsfrBuffer := buf } with dataIssues := [] } =
    { evaluateNsfr minRate
      { asOf, asfItems := asf2, rsfAssets := rsf, obsExposures := obs2, derivatives := deriv,
        recognisedClfLimit := clfLim, hostAlaInLcrLimit := hostAla, internalNsfrBuffer := buf } with dataIssues := [] } := by
  simp only [evaluateNsfr, sumBy_perm _ hasf, sumBy_perm _ hobs]

/-- Attachment C, Paragraphs 8 & 11(c) (Calendar-aware): With any explicit `reportingDate`,
    splitting liability cash flows across `<6m`, `6m..<1y`, and `>=1y` exact-conserves total carrying value. -/
theorem splitLiabilityCashFlowsForAsfCal_conserves_carrying
    (rd : Upstream.EpochDate) (idPrefix : String)
    (lt6mCat sixTo1yCat ge1yCat : NSFR.AsfCategory)
    (legs : List (Money × Nat)) (interdep : Bool) (obs : ObservationContext) :
    sumBy (splitLiabilityCashFlowsForAsfCal rd idPrefix lt6mCat sixTo1yCat ge1yCat legs interdep obs) (·.carryingValue) =
      sumBy legs (fun (a, _) => nonNeg a) := by
  simp only [splitLiabilityCashFlowsForAsfCal, splitLiabilityCashFlowsForAsf,
    sumBy, List.foldl_cons, List.foldl_nil, Rat.zero_add]
  exact sumBy_split3 legs (fun (_, d) => classifyNsfrDaysCal (some rd) d) (fun (a, _) => nonNeg a)

/-- Attachment C, Paragraph 22 (Calendar-aware): With any explicit `reportingDate`,
    splitting an amortising loan across `<1y` and `>=1y` exact-conserves total principal. -/
theorem splitAmortisingLoanForRsfCal_conserves_principal
    (rd : Upstream.EpochDate) (idPrefix : String)
    (lt1yCat ge1yCat : NSFR.RsfCategory)
    (legs : List (Money × Nat))
    (enc : NSFR.EncumbrancePeriod) (interdep : Bool) (obs : ObservationContext) :
    sumBy (splitAmortisingLoanForRsfCal rd idPrefix lt1yCat ge1yCat legs enc interdep obs) (·.carryingValue) =
      sumBy legs (fun (a, _) => nonNeg a) := by
  simp only [splitAmortisingLoanForRsfCal, splitAmortisingLoanForRsf,
    sumBy, List.foldl_cons, List.foldl_nil, Rat.zero_add]
  let oneY := (Upstream.addMonthsEpoch rd 12 - rd).toNat
  have h1 : legs.filter (fun (_, d) => d ≥ oneY) = legs.filter (fun x => !(decide (x.2 < oneY))) := by
    congr 1
    funext ⟨_, d⟩
    by_cases h : d < oneY
    · simp [h, Nat.not_le_of_gt h]
    · simp [h, Nat.not_lt.mp h]
  rw [h1]
  exact sumBy_split2 legs (fun x => decide (x.2 < oneY)) (fun (a, _) => nonNeg a)

/-- Calendar-window boundary correctness: exact 6-month and 12-month calendar boundaries
    enter `.sixMonthsToLessThanOneYear` and `.oneYearOrMore` respectively. -/
theorem classifyNsfrDaysCal_boundary (rd? : Option Upstream.EpochDate) (d : Nat)
    (hmono : (nsfrBoundaryDays rd?).1 ≤ (nsfrBoundaryDays rd?).2) :
    (d < (nsfrBoundaryDays rd?).1 → classifyNsfrDaysCal rd? d = .lessThanSixMonths) ∧
    ((nsfrBoundaryDays rd?).1 ≤ d → d < (nsfrBoundaryDays rd?).2 → classifyNsfrDaysCal rd? d = .sixMonthsToLessThanOneYear) ∧
    ((nsfrBoundaryDays rd?).2 ≤ d → classifyNsfrDaysCal rd? d = .oneYearOrMore) := by
  refine ⟨?_, ?_, ?_⟩
  · intro h
    simp [classifyNsfrDaysCal, h]
  · intro h1 h2
    have hnot : ¬(d < (nsfrBoundaryDays rd?).1) := Nat.not_lt.mpr h1
    simp [classifyNsfrDaysCal, hnot, h2]
  · intro h1
    have hnot2 : ¬(d < (nsfrBoundaryDays rd?).2) := Nat.not_lt.mpr h1
    have hge1 : (nsfrBoundaryDays rd?).1 ≤ d := Nat.le_trans hmono h1
    have hnot1 : ¬(d < (nsfrBoundaryDays rd?).1) := Nat.not_lt.mpr hge1
    simp [classifyNsfrDaysCal, hnot1, hnot2]

/-- Attachment B (MLH): An ineligible MLH asset contributes 0 and appending it
    never changes `evaluateMlh` (adding an ineligible asset cannot improve the MLH ratio). -/
theorem mlh_ineligible_asset_zero (a : MLH.MlhAsset) (h : a.isEligible = false) :
    a.eligibleValue = 0 := by
  simp [MLH.MlhAsset.eligibleValue, h]

theorem mlh_append_ineligible_unchanged (minRate : Rate) (input : MLH.MlhInput)
    (a : MLH.MlhAsset) (h : a.isEligible = false) :
    { MLH.evaluateMlh minRate { input with assets := input.assets ++ [a] } with dataIssues := [] } =
    { MLH.evaluateMlh minRate input with dataIssues := [] } := by
  simp only [MLH.evaluateMlh, List.filter_append]
  have helig : a.eligibleValue = 0 := mlh_ineligible_asset_zero a h
  have hsplit : ∀ (p : MLH.MlhAsset → Bool),
      sumBy (input.assets.filter p ++ [a].filter p) MLH.MlhAsset.eligibleValue =
      sumBy (input.assets.filter p) MLH.MlhAsset.eligibleValue := by
    intro p
    by_cases hp : p a = true
    · simp only [List.filter_cons, hp, ↓reduceIte, List.filter_nil, sumBy, List.foldl_append,
        List.foldl_cons, List.foldl_nil, helig, Rat.add_zero]
    · have hpf : p a = false := Bool.eq_false_iff.mpr hp
      simp only [List.filter_cons, hpf, Bool.false_eq_true, ↓reduceIte, List.filter_nil, List.append_nil]
  rw [hsplit _, hsplit _]

/-- No double-counting (Attachment A, Paragraph 45): An inflow item already counted in HQLA
    (or not fully performing) contributes 0 eligible inflow. -/
theorem inflow_countedInHqla_zero (i : InflowItem)
    (h : i.countedInHqla = true ∨ i.isFullyPerforming.affirmative = false) :
    i.inflowAmount = 0 := by
  rcases h with hc | hp
  · simp [InflowItem.inflowAmount, hc]
  · simp [InflowItem.inflowAmount, hp]

/-- Attachment C, Paragraph 28: Variation margin whose eligibility is not known offsets nothing. -/
theorem nsfr_vm_unknown_eligibility_no_offset (vm : NettingSetVariationMargin) (reason : String)
    (h : vm.eligibility = .unknown reason ∨ vm.eligibility = .refused reason) :
    vm.eligibleAmount = 0 := by
  rcases h with h | h <;> simp [NettingSetVariationMargin.eligibleAmount, h]

/-- Attachment C, Paragraph 28: Variation margin that does not fully extinguish the MtM exposure
    offsets nothing. -/
theorem nsfr_vm_not_extinguishing_no_offset (vm : NettingSetVariationMargin)
    (e : External.CashVariationMarginEligibility) (h : vm.eligibility = .known e)
    (hx : e.fullyExtinguishesMtmExposure = false) :
    vm.eligibleAmount = 0 := by
  simp [NettingSetVariationMargin.eligibleAmount, h,
    External.CashVariationMarginEligibility.isEligibleForNsfrOffset, hx]

/-- Attachment C, Paragraph 28: NSFR derivative assets are never negative: each netting set's VM offset
    is capped at that set's positive replacement cost. -/
theorem nsfr_derivative_assets_nonneg (d : NsfrDerivativeInput) : 0 ≤ d.nsfrDerivativeAssets := by
  simp only [NsfrDerivativeInput.nsfrDerivativeAssets]
  generalize nettingSetTotals d.contracts = xs
  induction xs with
  | nil => exact Rat.le_refl
  | cons x rest ih =>
    rw [sumBy_cons]
    exact Rat.add_nonneg (rat_le_max_right _ _) ih

/-- Attachment C, Paragraph 26 (Netting): An ineligible SFT netting group is treated gross
    (`netNsfrReceivable = nonNeg receivable`), with zero netting offset from `payable`. -/
theorem nsfr_sft_ineligible_no_netting (g : NsfrSftNettingGroup)
    (h : g.nettingCriteria.isEligible = false) :
    g.netNsfrReceivable = nonNeg g.receivable := by
  simp [NsfrSftNettingGroup.netNsfrReceivable, h]

/-- Monotonicity of ratio compliance: raising the required minimum rate (`minRate₁ ≤ minRate₂`)
    cannot turn failure into success (`compliant` at `minRate₂` implies `compliant` at `minRate₁`). -/
theorem checkRatio_monotone_minRate (num den minRate₁ minRate₂ : Rat)
    (hden : 0 ≤ den) (hle : minRate₁ ≤ minRate₂)
    (hc : (checkRatio num den minRate₂).compliant = true) :
    (checkRatio num den minRate₁).compliant = true := by
  by_cases h0 : den ≤ 0
  · simp only [checkRatio, h0, ↓reduceIte, decide_eq_true_eq] at hc ⊢
    exact hc
  · simp only [checkRatio, h0, ↓reduceIte, decide_eq_true_eq] at hc ⊢
    exact Rat.le_trans (Rat.mul_le_mul_of_nonneg_right hle hden) hc

private theorem rat_min_mono_right {a b c : Rat} (h : b ≤ c) : min a b ≤ min a c := by
  change (if a ≤ b then a else b) ≤ (if a ≤ c then a else c)
  split
  · next hab =>
    have hac : a ≤ c := Rat.le_trans hab h
    simp [hac]
  · next hnab =>
    split
    · next hac =>
      rcases @Rat.le_total a b with hab | hba
      · exact absurd hab hnab
      · exact hba
    · exact h

private theorem rat_max_mono_left {a b c : Rat} (h : a ≤ b) : max a c ≤ max b c := by
  change (if a ≤ c then c else a) ≤ (if b ≤ c then c else b)
  split
  · split
    · exact Rat.le_refl
    · next hnbc =>
      rcases @Rat.le_total b c with hbc | hcb
      · exact absurd hbc hnbc
      · exact hcb
  · split
    · next hnac hbc => exact absurd (Rat.le_trans h hbc) hnac
    · exact h

/-- Monotonicity of LCR inflow cap: tightening an APRA inflow cap (`0 ≤ c₁ ≤ c₂`)
    cannot reduce `totalNetCashOutflows`. -/
theorem netCashOutflows_monotone_inflowCap (totalOutflows : Money) (inflows : List InflowItem)
    {c₁ c₂ : Money} (hc1 : 0 ≤ c₁) (hle : c₁ ≤ c₂) :
    (computeNetCashOutflows totalOutflows inflows (some c₂)).totalNetCashOutflows ≤
    (computeNetCashOutflows totalOutflows inflows (some c₁)).totalNetCashOutflows := by
  simp only [computeNetCashOutflows, nonNeg]
  have h1 : max c₁ 0 = c₁ := by
    change (if c₁ ≤ 0 then 0 else c₁) = c₁
    split
    · next hle0 => exact Rat.le_antisymm hc1 hle0
    · rfl
  have h2 : max c₂ 0 = c₂ := by
    change (if c₂ ≤ 0 then 0 else c₂) = c₂
    split
    · next hle0 => exact Rat.le_antisymm (Rat.le_trans hc1 hle) hle0
    · rfl
  rw [h1, h2]
  have hmin_cap : min (pct 75 * max totalOutflows 0) c₁ ≤ min (pct 75 * max totalOutflows 0) c₂ :=
    rat_min_mono_right hle
  have hmin_in : min (sumBy inflows InflowItem.inflowAmount) (min (pct 75 * max totalOutflows 0) c₁) ≤
      min (sumBy inflows InflowItem.inflowAmount) (min (pct 75 * max totalOutflows 0) c₂) :=
    rat_min_mono_right hmin_cap
  have hsub : max totalOutflows 0 - min (sumBy inflows InflowItem.inflowAmount) (min (pct 75 * max totalOutflows 0) c₂) ≤
      max totalOutflows 0 - min (sumBy inflows InflowItem.inflowAmount) (min (pct 75 * max totalOutflows 0) c₁) := by
    rw [Rat.sub_eq_add_neg, Rat.sub_eq_add_neg, Rat.add_le_add_left, Rat.neg_le_neg_iff]
    exact hmin_in
  exact rat_max_mono_left hsub

/-- Compliance Aggregation (Main Body & Attachments A–C):
    `overallCompliant = true` implies every applicable quantitative requirement is satisfied
    (LCR AUD & All-Currencies and MLH may use the explicit `stressBreachPermitted` exception per
     Main Body Para 54 & Att B Para 5; NSFR requires `ratioCheck.compliant` at all times). -/
theorem overallCompliant_implies_all_applicable_quant
    (input : AdiComplianceInput)
    (hno_adj : input.apraAdjustments = [])
    (h : (evaluateAdiCompliance input).overallCompliant = true) :
    let res := evaluateAdiCompliance input
    let stressOk := input.inFinancialStress &&
      input.notifications.experiencingSevereStress &&
      input.notifications.notifiedApraImmediatelyOfStress
    let lcrStressOk := stressOk && input.notifications.notifiedApraOfLcrBreach
    let mlhStressOk := stressOk &&
      input.notifications.notifiedApraImmediatelyOfMlhShortfall &&
      input.notifications.advisedApraOfMlhRemedialAction
    (input.profile.requiredAudLcr.isSome →
      match res.lcrAudResult with
      | some r => r.ratioCheck.compliant = true ∨ lcrStressOk = true
      | none => False) ∧
    (input.profile.requiredAllCurrenciesLcr.isSome →
      match res.lcrAllCurrenciesResult with
      | some r => r.ratioCheck.compliant = true ∨ lcrStressOk = true
      | none => False) ∧
    (input.profile.requiredMlh.isSome →
      match res.mlhResult with
      | some r => r.triggerRatioValid = true ∧ ((r.ratioCheck.compliant = true ∧ r.aboveTriggerRatio = true) ∨ mlhStressOk = true)
      | none => False) ∧
    (input.profile.requiredNsfr.isSome →
      match res.nsfrResult with
      | some r => r.ratioCheck.compliant = true
      | none => False) := by
  dsimp only [evaluateAdiCompliance] at h ⊢
  rw [hno_adj] at h ⊢
  simp only [List.filter_nil, List.map_nil, List.foldl_nil] at h ⊢
  rcases (Bool.and_eq_true _ _).mp h with ⟨_, hq⟩
  simp only [Bool.and_eq_true] at hq
  rcases hq with ⟨⟨⟨⟨⟨⟨⟨⟨⟨⟨⟨⟨hlcrAud, hlcrAll⟩, _⟩, _⟩, hmlh⟩, _⟩, hnsfr⟩, _⟩, _⟩, _⟩, _⟩, _⟩, _⟩
  refine ⟨?_, ?_, ?_, ?_⟩
  · intro hsome
    cases hreq : input.profile.requiredAudLcr with
    | none => simp [hreq] at hsome
    | some m =>
      rw [hreq] at hlcrAud
      cases hres : input.lcrAudInput with
      | none => simp [hres] at hlcrAud
      | some lcrIn =>
        by_cases hb : lcrIn.basis == .level1
        · simp only [hres, hb, ↓reduceIte] at hlcrAud ⊢
          simpa [Bool.or_eq_true] using hlcrAud
        · simp [hres, hb] at hlcrAud
  · intro hsome
    cases hreq : input.profile.requiredAllCurrenciesLcr with
    | none => simp [hreq] at hsome
    | some m =>
      rw [hreq] at hlcrAll
      cases hres : input.lcrAllCurrenciesInput with
      | none => simp [hres] at hlcrAll
      | some lcrIn =>
        by_cases hb : lcrIn.basis == .level1
        · simp only [hres, hb, ↓reduceIte] at hlcrAll ⊢
          simpa [Bool.or_eq_true] using hlcrAll
        · simp [hres, hb] at hlcrAll
  · intro hsome
    cases hreq : input.profile.requiredMlh with
    | none => simp [hreq] at hsome
    | some m =>
      rw [hreq] at hmlh
      cases hres : input.mlhInput with
      | none => simp [hres] at hmlh
      | some mlhIn =>
        simp only [hres] at hmlh ⊢
        simpa [Bool.and_eq_true, Bool.or_eq_true] using hmlh
  · intro hsome
    cases hreq : input.profile.requiredNsfr with
    | none => simp [hreq] at hsome
    | some m =>
      rw [hreq] at hnsfr
      cases hres : input.nsfrInput with
      | none => simp [hres] at hnsfr
      | some nsfrIn =>
        simp only [hres] at hnsfr ⊢
        exact hnsfr

/-- Shared-limit allocation (Attachment C, Paragraph 32): When `initLimit = 0`
    (e.g., `recognisedClfLimit = none` and `hostAlaInLcrLimit = 0`), `foldClfBucketRsf`
    gives 0 within-limit 10% treatment and applies each item's full fallback factor `fb`. -/
theorem nsfr_zero_clf_limit_uses_fallback
    (items : List RsfAssetItem) (defaultFallback : Rate) :
    (foldClfBucketRsf items 0 defaultFallback).1 =
      sumBy (sortByFallbackDesc defaultFallback items)
        (fun a => nonNeg (a.excessClfFallbackFactor.getD defaultFallback) * nonNeg a.carryingValue) ∧
    (foldClfBucketRsf items 0 defaultFallback).2 = 0 := by
  simp only [foldClfBucketRsf]
  suffices h : ∀ (xs : List RsfAssetItem) (acc : Money),
      (xs.foldl (fun (accRsf, remLim) a =>
        let v := nonNeg a.carryingValue
        let within := min v remLim
        let excess := nonNeg (v - within)
        let fb := nonNeg (a.excessClfFallbackFactor.getD defaultFallback)
        (accRsf + pct 10 * within + fb * excess, nonNeg (remLim - within))) (acc, 0)) =
      (acc + sumBy xs (fun a => nonNeg (a.excessClfFallbackFactor.getD defaultFallback) * nonNeg a.carryingValue), 0) by
    have hres := h (sortByFallbackDesc defaultFallback items) 0
    simp only [Rat.zero_add] at hres
    rw [hres]
    exact ⟨rfl, rfl⟩
  intro xs
  induction xs with
  | nil => intro acc; simp [sumBy, Rat.add_zero]
  | cons x rest ih =>
    intro acc
    have hv : 0 ≤ nonNeg x.carryingValue := rat_le_max_right _ _
    have hmin : min (nonNeg x.carryingValue) 0 = 0 := by
      change (if nonNeg x.carryingValue ≤ 0 then nonNeg x.carryingValue else 0) = 0
      split
      · next hle => exact Rat.le_antisymm hle hv
      · rfl
    have hnnv : nonNeg (nonNeg x.carryingValue - 0) = nonNeg x.carryingValue := by
      rw [Rat.sub_eq_add_neg, Rat.neg_zero, Rat.add_zero]
      change (if nonNeg x.carryingValue ≤ 0 then 0 else nonNeg x.carryingValue) = nonNeg x.carryingValue
      split
      · next hle => exact Rat.le_antisymm hv hle
      · rfl
    have h00 : nonNeg ((0 : Rat) - 0) = 0 := by native_decide
    have hmul0 : pct 10 * 0 = 0 := by native_decide
    simp only [List.foldl_cons, hmin, hnnv, h00, hmul0, Rat.add_zero]
    rw [ih, sumBy_cons, ← Rat.add_assoc, Rat.mul_comm]

/-- Shared-limit allocation (Attachment A, Paragraph 9(e)): With no currency caps supplied,
    `computeCappedForeignSovHqla1` is always 0, and for a single asset + cap it never exceeds `nonNeg cap`. -/
theorem computeCappedForeignSovHqla1_empty_caps_zero
    (basis : ConsolidationBasis) (assets : List HqlaAsset) :
    computeCappedForeignSovHqla1 basis assets [] = 0 := by
  simp only [computeCappedForeignSovHqla1]
  suffices h : ∀ (cs : List Currency) (acc : Money),
      cs.foldl (fun acc c =>
        match ([] : List (Currency × Money)).filter (fun (c', _) => c' == c) with
        | [] => acc
        | (_, cap0) :: rest =>
          acc + min (sumBy ((assets.filter (fun a => a.category == .hqla1 && a.isNonZeroRwForeignSovInForeignCurrency)).filter (·.currency == c)) (HqlaAsset.cappedValue basis assets []))
            (rest.foldl (fun m (_, c'') => min m (nonNeg c'')) (nonNeg cap0))) acc = acc by
    exact h _ 0
  intro cs
  induction cs with
  | nil => intro acc; rfl
  | cons c rest ih => intro acc; simp only [List.foldl_cons, List.filter_nil]; exact ih acc

theorem computeCappedForeignSovHqla1_single_asset_le_cap
    (basis : ConsolidationBasis) (a : HqlaAsset) (cur : Currency) (cap : Money) :
    computeCappedForeignSovHqla1 basis [a] [(cur, cap)] ≤ nonNeg cap := by
  simp only [computeCappedForeignSovHqla1]
  by_cases ha : (a.category == .hqla1 && a.isNonZeroRwForeignSovInForeignCurrency) = true
  · simp only [List.filter_cons, ha, ↓reduceIte, List.filter_nil, List.map_cons, List.map_nil]
    have hed : [a.currency].eraseDups = [a.currency] := rfl
    rw [hed]
    simp only [List.foldl_cons, List.foldl_nil]
    by_cases hc : cur == a.currency
    · simp only [hc, ↓reduceIte, List.foldl_nil, Rat.zero_add]
      exact rat_min_le_right _ _
    · simp only [hc, Bool.false_eq_true, ↓reduceIte]
      exact rat_le_max_right _ _
  · have haf : (a.category == .hqla1 && a.isNonZeroRwForeignSovInForeignCurrency) = false :=
      Bool.eq_false_iff.mpr ha
    simp only [List.filter_cons, haf, Bool.false_eq_true, ↓reduceIte, List.filter_nil, List.map_nil]
    have hed : ([] : List Currency).eraseDups = [] := rfl
    rw [hed]
    simp only [List.foldl_nil]
    exact rat_le_max_right _ _

/-- Position-splitting invariance (Attachment B, MLH): Splitting an eligible or ineligible
    MLH asset with `0 ≤ v₁, 0 ≤ v₂` into two identical-category holdings `v₁` and `v₂`
    preserves `evaluateMlh`. -/
theorem mlh_split_asset_invariant (minRate : Rate) (a : MLH.MlhAsset)
    (v₁ v₂ : Money) (hv1 : 0 ≤ v₁) (hv2 : 0 ≤ v₂)
    (rest : List MLH.MlhAsset) (placements : Money) (liabs : List MLH.MlhLiabilityItem) (trig : Rate)
    (asOf : ObservationContext) :
    { MLH.evaluateMlh minRate
      { asOf, assets := { a with marketValue := v₁ + v₂ } :: rest
      , placementsByOtherAdis := placements, liabilities := liabs, triggerRatio := trig } with dataIssues := [] } =
    { MLH.evaluateMlh minRate
      { asOf, assets := { a with marketValue := v₁ } :: { a with marketValue := v₂ } :: rest
      , placementsByOtherAdis := placements, liabilities := liabs, triggerRatio := trig } with dataIssues := [] } := by
  have heq_elig : ({ a with marketValue := v₁ + v₂ } : MLH.MlhAsset).isEligible = a.isEligible ∧
      ({ a with marketValue := v₁ } : MLH.MlhAsset).isEligible = a.isEligible ∧
      ({ a with marketValue := v₂ } : MLH.MlhAsset).isEligible = a.isEligible := by
    simp [MLH.MlhAsset.isEligible, MLH.MlhAsset.requiresDebtCriteria]
  have hval : ({ a with marketValue := v₁ + v₂ } : MLH.MlhAsset).eligibleValue =
      ({ a with marketValue := v₁ } : MLH.MlhAsset).eligibleValue +
      ({ a with marketValue := v₂ } : MLH.MlhAsset).eligibleValue := by
    simp only [MLH.MlhAsset.eligibleValue, heq_elig.1, heq_elig.2.1, heq_elig.2.2]
    by_cases he : a.isEligible = true
    · simp only [he, ↓reduceIte, nonNeg]
      have h1 : max v₁ 0 = v₁ := by
        change (if v₁ ≤ 0 then 0 else v₁) = v₁
        split
        · next hle => exact Rat.le_antisymm hv1 hle
        · rfl
      have h2 : max v₂ 0 = v₂ := by
        change (if v₂ ≤ 0 then 0 else v₂) = v₂
        split
        · next hle => exact Rat.le_antisymm hv2 hle
        · rfl
      have h12 : max (v₁ + v₂) 0 = v₁ + v₂ := by
        change (if v₁ + v₂ ≤ 0 then 0 else v₁ + v₂) = v₁ + v₂
        split
        · next hle => exact Rat.le_antisymm (Rat.add_nonneg hv1 hv2) hle
        · rfl
      rw [h1, h2, h12]
    · simp [he, Rat.add_zero]
  simp only [MLH.evaluateMlh]
  cases hcat : (a.category == .netDepositsWithOtherAdisTwoBusinessDays)
  · have hnot : (a.category != .netDepositsWithOtherAdisTwoBusinessDays) = true := by simp [bne, hcat]
    simp only [List.filter_cons, hcat, hnot, ↓reduceIte, Bool.false_eq_true, sumBy_cons, hval, Rat.add_assoc]
  · have hnot : (a.category != .netDepositsWithOtherAdisTwoBusinessDays) = false := by simp [bne, hcat]
    simp only [List.filter_cons, hcat, hnot, ↓reduceIte, Bool.false_eq_true, sumBy_cons, hval, Rat.add_assoc]

end APS210.Theorems
