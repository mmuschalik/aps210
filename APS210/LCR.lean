import APS210.Basic
import APS210.Governance
import APS210.LCR.HQLA
import APS210.LCR.Outflows
import APS210.LCR.Inflows

namespace APS210.LCR

/-- Attachment A, Paragraphs 58–59: Foreign ADI head office committed funding facility. -/
structure ForeignHeadOfficeFacility where
  apraApproved : Bool
  irrevocableAndDocumented : Bool
  quantifiedAmount : Money
  hasLocalBankingSubsidiaryInAustralia : Bool
  deriving DecidableEq, Repr

def ForeignHeadOfficeFacility.eligibleAmount (f : ForeignHeadOfficeFacility) : Money :=
  if f.apraApproved && f.irrevocableAndDocumented && !f.hasLocalBankingSubsidiaryInAustralia then
    nonNeg f.quantifiedAmount
  else
    0

/-- Full inputs for an LCR calculation (either AUD-only or All-currencies). -/
structure LcrInput where
  basis : ConsolidationBasis
  hqlaAssets : List HqlaAsset
  unwind : HqlaUnwindAdjustments := {}
  /-- Optional raw <= 30-day SFT / collateral swap legs (added to `unwind` if provided). -/
  sftUnwindLegs : List ShortTermSftLeg := []
  /-- Attachment A, Para 9(e): Per-currency stressed net cash outflow caps for
      non-0% RW foreign sovereign debt issued in foreign currencies. -/
  foreignSovCurrencyCaps : List (Currency × Money) := []
  clf : Option ClfInput := none
  retailDeposits : List RetailDeposit
  otherOutflows : List OutflowItem
  inflows : List InflowItem
  apraInflowCapOverride : Option Money := none
  headOfficeFacility : Option ForeignHeadOfficeFacility := none
  /-- Attachment A, Paragraph 4: Internal HQLA buffer above the minimum LCR requirement. -/
  internalHqlaBuffer : Money := 0
  deriving DecidableEq, Repr

/-- Complete LCR evaluation result. -/
structure LcrResult where
  stockOfHqla : Money
  hqlaCaps : HqlaCapBreakdown
  recognisedClf : Money
  recognisedHeadOfficeFacility : Money
  netCashOutflows : NetCashOutflowsResult
  ratioCheck : RatioCheck
  bufferMet : Bool
  deriving DecidableEq, Repr

/-- Compute LCR and check compliance against `requiredMinRate` (e.g. 100% or 40%).
    Note: CLF is only recognised for locally incorporated ADIs in AUD LCR (paras 13–14).
    Head office committed funding facility is only recognised for qualifying foreign ADIs
    up to 50% of the LCR requirement (paras 58–59). -/
def evaluateLcr
    (incorporation : AdiIncorporation := .locallyIncorporated)
    (isAudOnly : Bool := false)
    (requiredMinRate : Rate := pct 100)
    (input : LcrInput) : LcrResult :=
  let hqlaAssets := if isAudOnly then input.hqlaAssets.filter (·.currency == .aud) else input.hqlaAssets
  let retailDeposits := if isAudOnly then input.retailDeposits.filter (·.currency == .aud) else input.retailDeposits
  let otherOutflows := if isAudOnly then input.otherOutflows.filter (·.currency == .aud) else input.otherOutflows
  let inflows := if isAudOnly then input.inflows.filter (·.currency == .aud) else input.inflows
  let legUnwind := computeUnwindAdjustments input.sftUnwindLegs isAudOnly
  let totalUnwind : HqlaUnwindAdjustments :=
    { deltaHqla1 := input.unwind.deltaHqla1 + legUnwind.deltaHqla1
      deltaHqla2a := input.unwind.deltaHqla2a + legUnwind.deltaHqla2a
      deltaHqla2b := input.unwind.deltaHqla2b + legUnwind.deltaHqla2b }
  let clfForCalc := match incorporation, isAudOnly with
    | .locallyIncorporated, true => input.clf
    | _, _ => none
  let (stock, caps) := computeStockOfHqla input.basis hqlaAssets totalUnwind clfForCalc input.foreignSovCurrencyCaps
  let clfAmt := match clfForCalc with
    | some c => c.recognisedAmount
    | none => 0
  let totalOut := computeTotalExpectedCashOutflows retailDeposits otherOutflows input.basis
  let nco := computeNetCashOutflows totalOut inflows input.apraInflowCapOverride
  let hoAmt := match incorporation, input.headOfficeFacility with
    | .foreign, some f =>
      let lcrReq := requiredMinRate * nco.totalNetCashOutflows
      min f.eligibleAmount (pct 50 * lcrReq)
    | _, _ => 0
  let numerator := stock + hoAmt
  let rc := checkRatio numerator nco.totalNetCashOutflows requiredMinRate
  let bufferMet := numerator >= requiredMinRate * nco.totalNetCashOutflows + nonNeg input.internalHqlaBuffer
  { stockOfHqla := stock
    hqlaCaps := caps
    recognisedClf := clfAmt
    recognisedHeadOfficeFacility := hoAmt
    netCashOutflows := nco
    ratioCheck := rc
    bufferMet }

end APS210.LCR
