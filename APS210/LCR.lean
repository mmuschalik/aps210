import APS210.Basic
import APS210.Governance
import APS210.LCR.HQLA
import APS210.LCR.Outflows
import APS210.LCR.Inflows

namespace APS210.LCR

open APS210.Facts

/-- Attachment A, Paragraphs 58–59: Foreign ADI head office committed funding facility. -/
structure ForeignHeadOfficeFacility where
  apraApproved : Bool
  irrevocableAndDocumented : Bool
  quantifiedAmount : Money
  hasLocalBankingSubsidiaryInAustralia : Bool
  currency : Currency := .aud
  deriving DecidableEq, Repr

def ForeignHeadOfficeFacility.eligibleAmount (f : ForeignHeadOfficeFacility) : Money :=
  if f.apraApproved && f.irrevocableAndDocumented && !f.hasLocalBankingSubsidiaryInAustralia then
    nonNeg f.quantifiedAmount
  else
    0

/-- Full inputs for an LCR calculation (either AUD-only or All-currencies). -/
structure LcrInput where
  /-- Reporting date (`validAt`) and data cut (`knownAt`) this calculation is for. -/
  asOf : ObservationContext
  basis : ConsolidationBasis
  hqlaAssets : List HqlaAsset
  unwind : HqlaUnwindAdjustments := {}
  /-- Optional raw <= 30-day SFT / collateral swap legs (added to `unwind` if provided). -/
  sftUnwindLegs : List ShortTermSftLeg := []
  /-- Attachment A, Para 9(e): Per-currency stressed net cash outflow caps for
      non-0% RW foreign sovereign debt issued in foreign currencies. -/
  foreignSovCurrencyCaps : List (Currency × Money) := []
  /-- Attachment A, Paragraph 29: Level 2 cap per legal entity / jurisdiction for its
      non-transferable HQLA (`HqlaAsset.level2TransferGroup`); a group with no cap has a cap of 0. -/
  level2TransferableCaps : List (String × Money) := []
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
  /-- Inputs that were not known or did not match `asOf`, with the treatment applied. -/
  dataIssues : List DataIssue
  deriving DecidableEq, Repr

/-- Data issues across every record in an LCR input. -/
def LcrInput.dataIssues (input : LcrInput) : List DataIssue :=
  let obs := observationIssues input.asOf
  (input.hqlaAssets.flatMap fun a => a.dataIssues ++ obs a.id a.observation) ++
  (input.retailDeposits.flatMap fun d => d.dataIssues ++ obs d.id d.observation) ++
  (input.otherOutflows.flatMap fun o => obs o.id o.observation) ++
  (input.inflows.flatMap fun i => i.dataIssues ++ obs i.id i.observation) ++
  (match input.clf with
   | some c => c.dataIssues ++ obs "clf" c.observation
   | none => [])

/-- Compute LCR and check compliance against `requiredMinRate` (e.g. 100% or 40%).
    - Attachment A, Paras 13–15: CLF is recognised for locally incorporated ADIs in AUD LCR,
      and in All-Currencies LCR up to the AUD shortfall (`max(0, requiredMinRate * audNCO - audHQLA)`),
      so CLF can cover an AUD shortfall without subsidising foreign-currency shortfalls.
    - Attachment A, Paras 58–59: Head office committed funding facility is only recognised for
      qualifying foreign ADIs up to 50% of the LCR requirement (and filtered by currency in AUD-only LCR). -/
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
  let (_, caps) := computeStockOfHqla input.basis hqlaAssets totalUnwind none
    input.foreignSovCurrencyCaps input.level2TransferableCaps
  let hasApprovedClf := match incorporation, input.clf with
    | .locallyIncorporated, some c => c.approvedByApra && c.contractualAmount > 0
    | _, _ => false
  -- Att A Table 3: 0% run-off on CLF-backed secured funding applies only where the ADI has an
  -- APRA-approved CLF with the RBA; otherwise it falls back to 100% (`securedAllOther`).
  let effectiveOtherOutflows :=
    if hasApprovedClf then otherOutflows
    else otherOutflows.map (fun o =>
      if o.category == .securedByClfEligibleWithinLimit then { o with category := .securedAllOther }
      else o)
  let totalOut := computeTotalExpectedCashOutflows retailDeposits effectiveOtherOutflows input.basis
  let nco := computeNetCashOutflows totalOut inflows input.apraInflowCapOverride
  let clfAmt := match incorporation, input.clf with
    | .locallyIncorporated, some c =>
      let rawClf := c.recognisedAmount
      if isAudOnly then
        rawClf
      else
        -- Para 14: CLF may only be used to cover an AUD shortfall between AUD HQLA and AUD NCO
        -- (using AUD-only SFT unwinds and statutory 100% AUD LCR requirement, without leaking FX).
        let audAssets := input.hqlaAssets.filter (·.currency == .aud)
        let audRetail := input.retailDeposits.filter (·.currency == .aud)
        let audOther := effectiveOtherOutflows.filter (·.currency == .aud)
        let audInflows := input.inflows.filter (·.currency == .aud)
        let audUnwind := computeUnwindAdjustments input.sftUnwindLegs true
        let (_, audCaps) := computeStockOfHqla input.basis audAssets audUnwind none
          input.foreignSovCurrencyCaps input.level2TransferableCaps
        let audOut := computeTotalExpectedCashOutflows audRetail audOther input.basis
        let audNco := computeNetCashOutflows audOut audInflows none
        let audShortfall := nonNeg (pct 100 * audNco.totalNetCashOutflows - audCaps.stockExClf)
        min rawClf audShortfall
    | _, _ => 0
  let stock := caps.stockExClf + clfAmt
  let hoAmt := match incorporation, input.headOfficeFacility with
    | .foreign, some f =>
      if isAudOnly && f.currency != .aud then 0
      else
        let lcrReq := requiredMinRate * nco.totalNetCashOutflows
        min f.eligibleAmount (pct 50 * lcrReq)
    | _, _ => 0
  let numerator := stock + hoAmt
  let rc := checkRatio numerator nco.totalNetCashOutflows requiredMinRate
  let bufferMet := numerator >= requiredMinRate * nco.totalNetCashOutflows + nonNeg input.internalHqlaBuffer
  let issues :=
    if isAudOnly then
      let obs := observationIssues input.asOf
      (hqlaAssets.flatMap fun a => a.dataIssues ++ obs a.id a.observation) ++
      (retailDeposits.flatMap fun d => d.dataIssues ++ obs d.id d.observation) ++
      (otherOutflows.flatMap fun o => obs o.id o.observation) ++
      (inflows.flatMap fun i => i.dataIssues ++ obs i.id i.observation) ++
      (match input.clf with
       | some c => c.dataIssues ++ obs "clf" c.observation
       | none => [])
    else
      input.dataIssues
  { stockOfHqla := stock
    hqlaCaps := caps
    recognisedClf := clfAmt
    recognisedHeadOfficeFacility := hoAmt
    netCashOutflows := nco
    ratioCheck := rc
    bufferMet
    dataIssues := issues }

end APS210.LCR
