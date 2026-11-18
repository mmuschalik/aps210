import Std

namespace APS210

/-- Exact rational monetary amount (e.g., in AUD or a specified currency). -/
abbrev Money := Rat

/-- Exact rational rate or factor (e.g., `5 / 100` represents 5%). -/
abbrev Rate := Rat

/-- Convert a natural percentage (e.g., `5`) to a `Rate` (`5 / 100`). -/
def pct (n : Nat) : Rate := (n : Rat) / 100

/-- Clamp a rational value below at zero: `max x 0`. -/
def nonNeg (x : Rat) : Rat := max x 0

/-- Sum a list of items using a projection to `Rat`. -/
def sumBy {α : Type} (xs : List α) (f : α → Rat) : Rat :=
  xs.foldl (fun acc x => acc + f x) 0

/-- Paragraph 3: Level 1 (solo) vs Level 2 (consolidated banking group). -/
inductive ConsolidationBasis where
  | level1
  | level2
  deriving DecidableEq, Repr

/-- Paragraph 15, 52–53, 55, 59, 67: Locally incorporated ADI vs Foreign ADI. -/
inductive AdiIncorporation where
  | locallyIncorporated
  | foreign
  deriving DecidableEq, Repr

/-- Paragraph 52–53: APRA classification of an ADI. -/
inductive AdiClassification where
  | lcr
  | mlh
  deriving DecidableEq, Repr

/-- Currency identifier (AUD vs foreign currency). -/
inductive Currency where
  | aud
  | foreign (code : String)
  deriving DecidableEq, Repr

/-- ECAI long-term credit rating tiers relevant to APS 210 (Attachment A paras 10, 12). -/
inductive CreditRating where
  | aaa
  | aaPlus
  | aa
  | aaMinus
  | aPlus
  | a
  | aMinus
  | bbbPlus
  | bbb
  | bbbMinus
  | belowBbbMinus
  | unrated
  deriving DecidableEq, Repr

namespace CreditRating

/-- True if rating is AA or higher (Attachment A para 12(a)(ii) for RMBS). -/
def isAtLeastAA : CreditRating → Bool
  | .aaa | .aaPlus | .aa => true
  | _ => false

/-- True if rating is AA- or higher (Attachment A para 10(b)(iii) for HQLA2A). -/
def isAtLeastAAMinus : CreditRating → Bool
  | .aaa | .aaPlus | .aa | .aaMinus => true
  | _ => false

/-- True if rating is between A+ and BBB- inclusive (Attachment A para 12(b)(ii)(1)). -/
def isBetweenAPlusAndBBBMinus : CreditRating → Bool
  | .aPlus | .a | .aMinus | .bbbPlus | .bbb | .bbbMinus => true
  | _ => false

end CreditRating

/-- Counterparty classification used across LCR (Attachment A) and NSFR (Attachment C). -/
inductive CounterpartyKind where
  | naturalPerson
  | sme (aggregatedFundingAud : Money) (managedAsRetail : Bool)
  | nonFinancialCorporate
  | sovereign (domestic : Bool) (aps112RiskWeightPct : Nat)
  | rba
  | centralBank
  | pse (aps112RiskWeightPct : Nat)
  | mdb (aps112RiskWeightPct : Nat)
  | nationalDevelopmentBank
  | adiOrBank (prudentiallySupervised : Bool)
  | otherFinancialInstitution
  /-- Attachment A, Paragraph 52: Hedge funds, money market funds, and SPVs. -/
  | hedgeFundOrMmfOrSpv
  | otherLegalEntity
  deriving DecidableEq, Repr

/-- Result of checking a regulatory ratio `numerator / denominator >= requiredMinimum`. -/
structure RatioCheck where
  numerator : Money
  denominator : Money
  requiredMinimum : Rate
  ratio : Option Rate
  compliant : Bool
  deriving DecidableEq, Repr

/-- Evaluate `numerator / denominator >= requiredMinimum` without division-by-zero ambiguity.
    When `denominator <= 0`, compliance holds iff `numerator >= 0` and `denominator = 0`
    (zero net requirement covered by non-negative buffer), or `numerator >= requiredMinimum * denominator`. -/
def checkRatio (numerator denominator : Money) (requiredMinimum : Rate) : RatioCheck :=
  let ratio := if denominator == 0 then none else some (numerator / denominator)
  let compliant :=
    if denominator <= 0 then numerator >= 0
    else numerator >= requiredMinimum * denominator
  { numerator, denominator, requiredMinimum, ratio, compliant }

end APS210
