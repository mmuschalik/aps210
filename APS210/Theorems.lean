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
theorem scorecard_le_six (s : LessStableScorecardInput) : s.score ≤ 6 := by
  simp only [LessStableScorecardInput.score]
  cases s.aboveGuaranteeLimitOr250k <;>
  cases s.isOnlineAccount <;>
  cases s.noEstablishedRelationship <;>
  cases s.isHeavilyRateDriven <;>
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

/-- Attachment A, Paragraph 35 & Table 4: Short-position-covering collateral always yields 0% inflow. -/
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

/-- Attachment A, Paragraph 55(a)–(b): Minimum All-Currencies LCR is 100% for locally incorporated
    LCR ADIs and 40% for foreign LCR ADIs (when no higher APRA minimum is set). -/
theorem default_lcr_minimums :
    ({ incorporation := .locallyIncorporated, classification := .lcr } : Governance.AdiProfile).requiredAllCurrenciesLcr = some (pct 100) ∧
    ({ incorporation := .foreign, classification := .lcr } : Governance.AdiProfile).requiredAllCurrenciesLcr = some (pct 40) := by
  native_decide

/-- Attachment A, Paragraph 59: Foreign ADIs and MLH ADIs have no NSFR requirement. -/
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
  · simp [applyEncumbranceFactor, rat_le_max_left]

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
    have hnco : 0 ≤ (computeNetCashOutflows
      (computeTotalExpectedCashOutflows input.retailDeposits input.otherOutflows input.basis)
      input.inflows input.apraInflowCapOverride).totalNetCashOutflows :=
      netCashOutflows_nonneg _ _ _
    have hpct50 : (0 : Rat) ≤ pct 50 := by native_decide
    exact Rat.mul_nonneg hpct50 (Rat.mul_nonneg hmin hnco)
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
  simp only [computeHqlaCaps, nonNeg, pct]
  exact cap15_alg _ _ (rat_sub_max_zero_le _ _)

/-- Attachment A, Paragraph 6–8: The `2/3` formula in `computeHqlaCaps` enforces that
    capped HQLA2 (`adj2After15 - excess40Cap`) never exceeds 40% of post-cap total HQLA. -/
theorem hqla_cap40_bound (u1 u2a u2b : Money) (uw : HqlaUnwindAdjustments) :
    let b := computeHqlaCaps u1 u2a u2b uw
    let capped2 := b.adjHqla2a + (b.adjHqla2b - b.excess15Cap) - b.excess40Cap
    capped2 ≤ pct 40 * (b.adjHqla1 + capped2) := by
  simp only [computeHqlaCaps, nonNeg, pct]
  exact cap40_alg _ _ (rat_sub_max_zero_le _ _)

/-- Attachment A, Paragraph 15(a)–(c) & 17: Unapproved CLF yields 0, and recognised CLF
    is always non-negative and bounded by both contractual amount and collateral after RBA margins. -/
theorem clf_recognised_bounds (clf : ClfInput) :
    0 ≤ clf.recognisedAmount ∧
    clf.recognisedAmount ≤ nonNeg clf.contractualAmount ∧
    clf.recognisedAmount ≤ nonNeg clf.collateralHeldAfterRbaMargins ∧
    (clf.approvedByApra = false → clf.recognisedAmount = 0) := by
  simp only [ClfInput.recognisedAmount]
  cases h : clf.approvedByApra
  · refine ⟨Rat.le_refl, rat_le_max_right _ _, rat_le_max_right _ _, fun _ => rfl⟩
  · simp only [Bool.not_true, Bool.false_eq_true, ↓reduceIte]
    cases hp : clf.processEligible
    · refine ⟨Rat.le_refl, rat_le_max_right _ _, rat_le_max_right _ _, fun hfalse => Bool.noConfusion hfalse⟩
    · simp only [Bool.not_true, Bool.false_eq_true, ↓reduceIte]
      have ha : 0 ≤ nonNeg clf.contractualAmount := rat_le_max_right _ _
      have hb : 0 ≤ nonNeg (clf.contractualAmount - nonNeg clf.pledgedUnderZeroRunoffSecFunding + nonNeg clf.receivedUnderZeroInflowSecLending) :=
        rat_le_max_right _ _
      have hc : 0 ≤ nonNeg clf.collateralHeldAfterRbaMargins := rat_le_max_right _ _
      have hmin_bc_nonneg : 0 ≤ min (nonNeg (clf.contractualAmount - nonNeg clf.pledgedUnderZeroRunoffSecFunding + nonNeg clf.receivedUnderZeroInflowSecLending)) (nonNeg clf.collateralHeldAfterRbaMargins) := by
        change 0 ≤ (if _ ≤ _ then _ else _)
        split <;> assumption
      have hmin_abc_nonneg : 0 ≤ min (nonNeg clf.contractualAmount) (min (nonNeg (clf.contractualAmount - nonNeg clf.pledgedUnderZeroRunoffSecFunding + nonNeg clf.receivedUnderZeroInflowSecLending)) (nonNeg clf.collateralHeldAfterRbaMargins)) := by
        change 0 ≤ (if _ ≤ _ then _ else _)
        split <;> assumption
      have hle_a : min (nonNeg clf.contractualAmount) (min (nonNeg (clf.contractualAmount - nonNeg clf.pledgedUnderZeroRunoffSecFunding + nonNeg clf.receivedUnderZeroInflowSecLending)) (nonNeg clf.collateralHeldAfterRbaMargins)) ≤ nonNeg clf.contractualAmount :=
        rat_min_le_left _ _
      have hle_c : min (nonNeg clf.contractualAmount) (min (nonNeg (clf.contractualAmount - nonNeg clf.pledgedUnderZeroRunoffSecFunding + nonNeg clf.receivedUnderZeroInflowSecLending)) (nonNeg clf.collateralHeldAfterRbaMargins)) ≤ nonNeg clf.collateralHeldAfterRbaMargins :=
        Rat.le_trans (rat_min_le_right _ _) (rat_min_le_right _ _)
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
    (enc : NSFR.EncumbrancePeriod) (interdep : Bool) :
    sumBy (splitAmortisingLoanForRsf idPrefix lt1yCat ge1yCat legs enc interdep) (·.carryingValue) =
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
    (legs : List (Money × Nat)) (interdep : Bool) :
    sumBy (splitLiabilityCashFlowsForAsf idPrefix lt6mCat sixTo1yCat ge1yCat legs interdep) (·.carryingValue) =
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

end APS210.Theorems
