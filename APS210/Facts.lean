import APS210.Basic

namespace APS210.Facts

/-- A source observation, never an implicit favourable assumption. -/
inductive Reading (α : Type) where
  | known (value : α)
  | unknown (reason : String)
  | refused (reason : String)
  deriving DecidableEq, Repr

def Reading.map {α β : Type} (f : α → β) : Reading α → Reading β
  | .known value => .known (f value)
  | .unknown reason => .unknown reason
  | .refused reason => .refused reason

/-- Eligibility requires affirmative evidence; unresolved reasons remain in the result. -/
def Reading.affirmative : Reading Bool → Bool
  | .known true => true
  | _ => false

theorem unknown_not_affirmative (reason : String) :
    (Reading.unknown reason : Reading Bool).affirmative = false := rfl

theorem refused_not_affirmative (reason : String) :
    (Reading.refused reason : Reading Bool).affirmative = false := rfl

/-- Direction is a contractual fact, independent of the amount's sign. -/
inductive Side where
  | weOwe
  | owedToUs
  deriving DecidableEq, Repr

/-- Amounts in factual obligations cannot silently contain negative values. -/
structure Amount where
  value : Money
  nonnegative : 0 ≤ value
  deriving Repr

def Side.signedOutflow (side : Side) (amount : Amount) : Money :=
  match side with
  | .weOwe => amount.value
  | .owedToUs => -amount.value

theorem opposite_sides_cancel (amount : Amount) :
    Side.weOwe.signedOutflow amount + Side.owedToUs.signedOutflow amount = 0 := by
  simp only [Side.signedOutflow]
  exact Rat.add_neg_cancel amount.value

/-- Days since the epoch; knowledge timestamps are epoch seconds.
    Units are explicit so observation time cannot be confused with settlement date. -/
structure ObservationContext where
  validAt : Int
  knownAt : Int
  sourceReference : String
  deriving DecidableEq, Repr

inductive OptionHolder where
  | us
  | counterparty
  deriving DecidableEq, Repr

inductive OptionKind where
  | earlyRedemption
  | extension
  deriving DecidableEq, Repr

/-- Preserve each option, its owner, exercise date and resulting settlement date.
    Rule-specific maturity interpretation belongs in a separate evaluator. -/
structure ContractOption where
  holder : OptionHolder
  kind : OptionKind
  exerciseDate : Int
  resultingSettlementDate : Int
  deriving DecidableEq, Repr

/-- No defaults: an absent option list is distinguished from known absence of options. -/
structure Obligation where
  id : String
  currency : Currency
  side : Side
  amount : Amount
  settlementDate : Reading Int
  optionality : Reading (List ContractOption)
  performing : Reading Bool
  observation : ObservationContext
  deriving Repr

/-- Collateral is represented as directed movements, not an unsigned adjustment. -/
structure CollateralMovement where
  obligationId : String
  nettingSetId : String
  side : Side
  amount : Amount
  currency : Currency
  settlementDate : Reading Int
  eligibleForOffset : Reading Bool
  alreadyCountedInHqla : Reading Bool
  observation : ObservationContext
  deriving Repr

-- ============================================================================
-- Construction helpers, decidable equality, and data issues
-- ============================================================================

theorem nonNeg_nonneg (x : Rat) : 0 ≤ nonNeg x := by
  change 0 ≤ (if x ≤ 0 then 0 else x)
  split
  · exact Rat.le_refl
  · next h =>
    rcases @Rat.le_total x 0 with hx | hx
    · exact absurd hx h
    · exact hx

/-- Build an `Amount` from a rational, clamping a negative input at zero. -/
def Amount.ofRat (x : Rat) : Amount := ⟨nonNeg x, nonNeg_nonneg x⟩

instance : DecidableEq Amount := fun a b =>
  if h : a.value = b.value then
    isTrue (by cases a; cases b; simp only at h; subst h; rfl)
  else
    isFalse (fun hab => h (hab ▸ rfl))

instance : OfNat Amount n := ⟨Amount.ofRat n⟩

deriving instance DecidableEq for Obligation
deriving instance DecidableEq for CollateralMovement

/-- True when the fact is known to hold or is not known, i.e. a missing fact is assumed
    against the ADI. Use for facts whose presence increases a requirement. -/
def Reading.affirmativeOrMissing : Reading Bool → Bool
  | .known b => b
  | _ => true

/-- Resolve a reading with the rule's conservative substitute when it is not known. -/
def Reading.resolve {α : Type} (conservative : α) : Reading α → α
  | .known v => v
  | _ => conservative

def Reading.toOption {α : Type} : Reading α → Option α
  | .known v => some v
  | _ => none

/-- Why an input could not be used as supplied. -/
inductive DataIssueKind where
  | unknownReading
  | refusedReading
  /-- `validAt` differs from the evaluation reporting date. -/
  | staleRecord
  /-- `knownAt` is after the evaluation data cut. -/
  | notYetKnown
  deriving DecidableEq, Repr

/-- A data issue carried into calculation output: which record and field, why, and how it was treated. -/
structure DataIssue where
  recordId : String
  field : String
  kind : DataIssueKind
  reason : String
  treatment : String
  deriving DecidableEq, Repr

/-- Report a reading that is not known, with the treatment the rule applied. -/
def readingIssues {α : Type} (recordId field treatment : String) : Reading α → List DataIssue
  | .known _ => []
  | .unknown reason => [{ recordId, field, kind := .unknownReading, reason, treatment }]
  | .refused reason => [{ recordId, field, kind := .refusedReading, reason, treatment }]

/-- Report a record observed for a different reporting date, or after the data cut. -/
def observationIssues (asOf : ObservationContext) (recordId : String) (o : ObservationContext) :
    List DataIssue :=
  (if o.validAt != asOf.validAt then
    [{ recordId, field := "validAt", kind := .staleRecord,
       reason := s!"record describes day {o.validAt}, evaluation is for day {asOf.validAt}",
       treatment := "included as supplied" }]
  else []) ++
  (if o.knownAt > asOf.knownAt then
    [{ recordId, field := "knownAt", kind := .notYetKnown,
       reason := s!"record known at {o.knownAt}, after data cut {asOf.knownAt}",
       treatment := "included as supplied" }]
  else [])

-- ============================================================================
-- Rule-specific maturity evaluator (Att A paras 45 & 60, Att C paras 8 & 20)
-- ============================================================================

/-- Which maturity rule applies. -/
inductive MaturityRegime where
  /-- Att A para 45: funding includes options exercisable at the investor's discretion.
      Att A para 60: inflows at the latest date given the counterparty's contractual rights. -/
  | lcr
  /-- Att C para 8: funding at the earliest date the funds may be redeemed.
      Att C para 20: assets at the latest possible date; extension options assumed exercised. -/
  | nsfr
  deriving DecidableEq, Repr

/-- Whether an option counts against the ADI: under the LCR only the counterparty's options,
    under the NSFR any holder's options. -/
def ContractOption.counts (regime : MaturityRegime) (o : ContractOption) : Bool :=
  match regime with
  | .lcr => o.holder == .counterparty
  | .nsfr => true

/-- Worst-case settlement date where the counterparty acts against us: what we owe settles at the
    earliest date an early-redemption option allows, what is owed to us at the latest date an
    extension allows. Rows with both a call and an extension are handled by the same fold. -/
def worstCaseSettlementDate (regime : MaturityRegime) (side : Side)
    (contractual : Int) (options : List ContractOption) : Int :=
  match side with
  | .weOwe =>
    (options.filter (fun o => o.counts regime && o.kind == .earlyRedemption)).foldl
      (fun acc o => min acc o.resultingSettlementDate) contractual
  | .owedToUs =>
    (options.filter (fun o => o.counts regime && o.kind == .extension)).foldl
      (fun acc o => max acc o.resultingSettlementDate) contractual

/-- Worst-case settlement date of an obligation. When the settlement date or option list is not
    known, what we owe is due on the reporting date and what is owed to us never arrives (`none`). -/
def Obligation.worstCaseSettlementDate (regime : MaturityRegime) (o : Obligation)
    (asOfValidAt? : Option Int := none) : Option Int :=
  match o.settlementDate, o.optionality with
  | .known d, .known opts => some (APS210.Facts.worstCaseSettlementDate regime o.side d opts)
  | _, _ =>
    match o.side with
    | .weOwe => some (asOfValidAt?.getD o.observation.validAt)
    | .owedToUs => none

def Obligation.dataIssues (o : Obligation) : List DataIssue :=
  readingIssues o.id "settlementDate"
    (match o.side with
     | .weOwe => "treated as due on the reporting date"
     | .owedToUs => "excluded: settlement date not established") o.settlementDate ++
  readingIssues o.id "optionality"
    (match o.side with
     | .weOwe => "treated as due on the reporting date"
     | .owedToUs => "excluded: options not established") o.optionality ++
  (match o.side with
   | .owedToUs => readingIssues o.id "performing" "treated as non-performing" o.performing
   | .weOwe => [])

theorem worstCase_weOwe_le (regime : MaturityRegime) (d : Int) (opts : List ContractOption) :
    worstCaseSettlementDate regime .weOwe d opts ≤ d := by
  simp only [worstCaseSettlementDate]
  generalize opts.filter _ = xs
  induction xs generalizing d with
  | nil => exact Int.le_refl d
  | cons x xs ih =>
    simp only [List.foldl_cons]
    exact Int.le_trans (ih (min d x.resultingSettlementDate)) (Int.min_le_left _ _)

theorem worstCase_owedToUs_ge (regime : MaturityRegime) (d : Int) (opts : List ContractOption) :
    d ≤ worstCaseSettlementDate regime .owedToUs d opts := by
  simp only [worstCaseSettlementDate]
  generalize opts.filter _ = xs
  induction xs generalizing d with
  | nil => exact Int.le_refl d
  | cons x xs ih =>
    simp only [List.foldl_cons]
    exact Int.le_trans (Int.le_max_left _ _) (ih (max d x.resultingSettlementDate))

end APS210.Facts
