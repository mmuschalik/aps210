import APS210.Basic

namespace APS210.External

/-- APS 112 Attachment F, Table 21: Credit rating grades 1 to 6 (or unrated). -/
inductive CreditRatingGrade where
  | grade1  -- AAA to AA-
  | grade2  -- A+ to A-
  | grade3  -- BBB+ to BBB-
  | grade4  -- BB+ to BB-
  | grade5  -- B+ to B-
  | grade6  -- Below B-
  | unrated
  deriving DecidableEq, Repr

/-- Rank of a credit rating grade (1 = best, 6 = worst, 7 = unrated for comparison). -/
def CreditRatingGrade.rank : CreditRatingGrade → Nat
  | .grade1 => 1
  | .grade2 => 2
  | .grade3 => 3
  | .grade4 => 4
  | .grade5 => 5
  | .grade6 => 6
  | .unrated => 7

/-- Map APS 210 `CreditRating` (ECAI scale) to APS 112 Table 21 `CreditRatingGrade`. -/
def CreditRatingGrade.ofCreditRating : CreditRating → CreditRatingGrade
  | .aaa | .aaPlus | .aa | .aaMinus => .grade1
  | .aPlus | .a | .aMinus => .grade2
  | .bbbPlus | .bbb | .bbbMinus => .grade3
  | .belowBbbMinus => .grade4
  | .unrated => .unrated

/-- APS 112 Attachment F (Multiple assessments):
    - 0 ratings -> `.unrated`
    - 1 rating -> that grade
    - 2 ratings -> worse (higher risk weight / higher rank) of the two
    - >= 3 ratings -> second-best (higher of the two lowest risk weights / lowest two ranks). -/
def resolveMultipleEcaiAssessments (grades : List CreditRatingGrade) : CreditRatingGrade :=
  let rated := grades.filter (· != .unrated)
  match rated with
  | [] => .unrated
  | [g] => g
  | [g1, g2] => if g1.rank >= g2.rank then g1 else g2
  | g1 :: g2 :: rest =>
    -- Sort ascending by rank and take the 2nd element
    let sorted := (g1 :: g2 :: rest).mergeSort (fun a b => a.rank ≤ b.rank)
    match sorted with
    | _ :: second :: _ => second
    | _ => .unrated

/-- APS 112 Attachment B, Table 5: Risk weight (%) for sovereign exposures.
    Includes special rules for Australian Government / RBA in AUD (0%) and
    listed zero-RW international institutions & MDBs (para 3(c), footnote 7). -/
def sovereignRiskWeightPct
    (grade : CreditRatingGrade)
    (isAusGovtOrRbaInAud : Bool := false)
    (isZeroRwInstitutionOrMdb : Bool := false) : Nat :=
  if isAusGovtOrRbaInAud || isZeroRwInstitutionOrMdb then 0
  else match grade with
    | .grade1 => 0
    | .grade2 => 20
    | .grade3 => 50
    | .grade4 | .grade5 => 100
    | .grade6 => 150
    | .unrated => 100

/-- APS 112 Attachment B, Table 6: Risk weight (%) for domestic PSEs. -/
def domesticPseRiskWeightPct (grade : CreditRatingGrade) : Nat :=
  match grade with
  | .grade1 => 20
  | .grade2 => 50
  | .grade3 => 50
  | .grade4 | .grade5 => 100
  | .grade6 => 150
  | .unrated => 50

/-- APS 112 Attachment B, Table 7: Risk weight (%) for bank and non-0%-RW MDB exposures. -/
def bankOrNonZeroMdbRiskWeightPct
    (grade : CreditRatingGrade)
    (isShortTerm : Bool := false) : Nat :=
  if isShortTerm then
    match grade with
    | .grade1 | .grade2 | .grade3 | .unrated => 20
    | .grade4 | .grade5 => 50
    | .grade6 => 150
  else
    match grade with
    | .grade1 => 20
    | .grade2 => 30
    | .grade3 => 50
    | .grade4 | .grade5 => 100
    | .grade6 => 150
    | .unrated => 50

/-- APS 112 Attachment A: Standard residential property loan qualification
    for <= 35% risk weight / NSFR 65% RSF (Attachment C para 35(a)). -/
structure StandardResidentialMortgageCriteria where
  toNaturalPerson : Bool
  isStandardLoanPerAps112 : Bool
  lvrPct : Rat
  residualMaturityOneYearOrMore : Bool
  deriving DecidableEq, Repr

def StandardResidentialMortgageCriteria.qualifiesFor65PctRsf
    (m : StandardResidentialMortgageCriteria) : Bool :=
  m.toNaturalPerson &&
  m.isStandardLoanPerAps112 &&
  m.lvrPct <= 80 &&
  m.residualMaturityOneYearOrMore

/-- APS 112 Attachment H, Paragraphs 9–11: Eligible bilateral netting contract criteria
    for derivative replacement cost netting (APS 210 Attachment C paras 9 & 27). -/
structure BilateralNettingAgreement where
  writtenAndLegallyEnforceable : Bool
  singleLegalObligationCloseOut : Bool
  noWalkawayClause : Bool
  reasonedLegalOpinionsInAllJurisdictions : Bool
  deriving DecidableEq, Repr

def BilateralNettingAgreement.isEligible (b : BilateralNettingAgreement) : Bool :=
  b.writtenAndLegallyEnforceable &&
  b.singleLegalObligationCloseOut &&
  b.noWalkawayClause &&
  b.reasonedLegalOpinionsInAllJurisdictions

/-- APS 110 Attachment D, Paragraph 23 & APS 210 Attachment C, Paragraph 28:
    Cash variation margin eligibility to offset positive derivative replacement cost. -/
structure CashVariationMarginEligibility where
  isCash : Bool
  notSegregatedIfNonQccp : Bool
  calculatedAndExchangedDailyMtM : Bool
  inContractMnaOrCcpCurrency : Bool
  inSameSettlementCurrencyAsDerivative : Bool
  deriving DecidableEq, Repr

def CashVariationMarginEligibility.isEligibleForNsfrOffset
    (vm : CashVariationMarginEligibility) : Bool :=
  vm.isCash &&
  vm.notSegregatedIfNonQccp &&
  vm.calculatedAndExchangedDailyMtM &&
  vm.inContractMnaOrCcpCurrency &&
  vm.inSameSettlementCurrencyAsDerivative

/-- APS 110 Attachment D, Paragraph 38: Netting of SFT cash payables and receivables
    with a single counterparty (APS 210 Attachment C para 26).
    Note: Transactions with no explicit end date that can be unwound at any time by either
    party (open repos) are explicitly NOT eligible. -/
structure SftNettingCriteria where
  sameCounterparty : Bool
  sameExplicitFinalSettlementDate : Bool
  notOpenEndedUnwindable : Bool := true
  legallyEnforceableNettingNormalAndDefault : Bool
  settledNetOrSimultaneously : Bool
  deriving DecidableEq, Repr

def SftNettingCriteria.isEligible (s : SftNettingCriteria) : Bool :=
  s.sameCounterparty &&
  s.sameExplicitFinalSettlementDate &&
  s.notOpenEndedUnwindable &&
  s.legallyEnforceableNettingNormalAndDefault &&
  s.settledNetOrSimultaneously

/-- APS 113 Paragraph 40: SME retail IRB sub-asset class threshold (< AUD 1.5 million
    business-related exposure, referenced in APS 210 Attachment A footnote 7). -/
def qualifiesAsSmeUnderAps113Para40 (businessRelatedExposureAud : Money) : Bool :=
  businessRelatedExposureAud >= 0 && businessRelatedExposureAud < 1500000

/-- APS 220 (Default / Non-performing exposure, referenced in APS 210 Attachment C fn 27):
    An exposure is non-performing if >= 90 days past due OR borrower is unlikely to pay. -/
def isPerformingLoanPerAps220 (daysPastDue : Nat) (unlikelyToPay : Bool) : Bool :=
  daysPastDue < 90 && !unlikelyToPay

/-- APS 120: Operational requirements for regulatory capital relief on a securitisation
    (referenced in APS 210 Paragraph 5). -/
structure Aps120CapitalReliefCriteria where
  significantCreditRiskTransferred : Bool
  cleanBreakFromOriginator : Bool
  noImplicitOrMoralRecourse : Bool
  armsLengthTerms : Bool
  deriving DecidableEq, Repr

def Aps120CapitalReliefCriteria.isMet (c : Aps120CapitalReliefCriteria) : Bool :=
  c.significantCreditRiskTransferred &&
  c.cleanBreakFromOriginator &&
  c.noImplicitOrMoralRecourse &&
  c.armsLengthTerms

end APS210.External
