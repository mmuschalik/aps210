import APS210.Basic
import APS210.ExternalStandards

namespace APS210.LCR

/-- Attachment A, Paragraphs 6–12: HQLA asset classification. -/
inductive HqlaCategory where
  | hqla1
  | hqla2a
  | hqla2bRmbs
  | hqla2bCorporateDebt
  | hqla2bEquity
  deriving DecidableEq, Repr

/-- Haircut rate per Attachment A, Paragraphs 9, 10, 12. -/
def HqlaCategory.haircut : HqlaCategory → Rate
  | .hqla1 => pct 0
  | .hqla2a => pct 15
  | .hqla2bRmbs => pct 25
  | .hqla2bCorporateDebt => pct 50
  | .hqla2bEquity => pct 50

/-- Post-haircut factor `(1 - haircut)` per Attachment A, Paragraphs 9, 10, 12. -/
def HqlaCategory.weight (c : HqlaCategory) : Rate :=
  1 - c.haircut

/-- Attachment A, Paragraphs 10 & 12: Maximum 30-day stress price decline / haircut
    increase thresholds (10% HQLA2A, 20% HQLA2B RMBS/corporate debt, 40% HQLA2B equity). -/
def HqlaCategory.meetsStressThreshold
    (c : HqlaCategory) (max30dPriceDecline max30dHaircutIncrease : Rate) : Bool :=
  let lim := match c with
    | .hqla1 => pct 100
    | .hqla2a => pct 10
    | .hqla2bRmbs | .hqla2bCorporateDebt => pct 20
    | .hqla2bEquity => pct 40
  max30dPriceDecline <= lim && max30dHaircutIncrease <= lim

/-- Attachment A, Paragraphs 9–12: Classify a marketable debt security from raw attributes
    (APS 112 risk weight, ECAI credit rating, issuer type, and liquidity/market criteria). -/
def classifyHqlaDebtSecurity
    (isSovereignCbPseMdb : Bool)
    (aps112RiskWeightPct : Nat)
    (rating : CreditRating)
    (isIssuedByFinancialInstitutionOrAssoc : Bool)
    (isCoveredBondNotOwnOrAssoc : Bool)
    (isPlainVanilla : Bool)
    (deepActiveMarketAndProvenLiquidity : Bool) : Option HqlaCategory :=
  if !deepActiveMarketAndProvenLiquidity then none
  else if isSovereignCbPseMdb && aps112RiskWeightPct == 0 && !isIssuedByFinancialInstitutionOrAssoc then
    some .hqla1
  else if isSovereignCbPseMdb && aps112RiskWeightPct == 20 && !isIssuedByFinancialInstitutionOrAssoc then
    some .hqla2a
  else if (!isSovereignCbPseMdb && !isIssuedByFinancialInstitutionOrAssoc && isPlainVanilla || isCoveredBondNotOwnOrAssoc) &&
      rating.isAtLeastAAMinus then
    some .hqla2a
  else if !isSovereignCbPseMdb && !isIssuedByFinancialInstitutionOrAssoc && rating.isBetweenAPlusAndBBBMinus then
    some .hqla2bCorporateDebt
  else
    none

/-- Derive HQLA category for a sovereign/CB/PSE/MDB debt security directly from multiple
    APS 112 ECAI ratings using APS 112 Attachment B & Attachment F. -/
def classifySovereignPseMdbHqla
    (cp : CounterpartyKind)
    (ecaiGrades : List External.CreditRatingGrade)
    (isAusGovtOrRbaInAud : Bool := false)
    (isZeroRwMdbOrInst : Bool := false)
    (deepActiveMarketAndProvenLiquidity : Bool := true) : Option HqlaCategory :=
  if !deepActiveMarketAndProvenLiquidity then none
  else
    let effGrade := External.resolveMultipleEcaiAssessments ecaiGrades
    let rw := match cp with
      | .sovereign _ _ | .centralBank | .rba =>
        External.sovereignRiskWeightPct effGrade isAusGovtOrRbaInAud isZeroRwMdbOrInst
      | .pse _ => External.domesticPseRiskWeightPct effGrade
      | .mdb _ => External.bankOrNonZeroMdbRiskWeightPct effGrade false
      | _ => 100
    if isZeroRwMdbOrInst || rw == 0 then some .hqla1
    else if rw == 20 then some .hqla2a
    else none

/-- Attachment A, Paragraphs 9(d)–(e): Classify non-0% RW sovereign/central bank debt
    issued in domestic currency (`9(d)`) or foreign currency (`9(e)`, subject to
    per-currency stressed net cash outflow cap). -/
def classifyHqla9d9e
    (isDomesticCurrency : Bool)
    (isForeignCurrencyOfSubsidiaryOrBranch : Bool)
    (meetsMarketLiquidityCriteria : Bool) : Option (HqlaCategory × Bool) :=
  if !meetsMarketLiquidityCriteria then none
  else if isDomesticCurrency then some (.hqla1, false)
  else if isForeignCurrencyOfSubsidiaryOrBranch then some (.hqla1, true)
  else none

/-- Attachment A, Paragraph 12(a): Classify RMBS as HQLA2B (25% haircut).
    Includes 12(a)(v): pool contains only residential mortgages and no structured products. -/
def classifyHqla2bRmbs
    (rating : CreditRating)
    (maxAvgLvrPct : Nat)
    (fullRecourseLoans : Bool)
    (subjectToRiskRetention : Bool)
    (onlyResidentialMortgagesNoStructuredProducts : Bool)
    (notIssuedByAdiOrAffiliate : Bool)
    (meets20PctStressAndLiquidity : Bool) : Option HqlaCategory :=
  if rating.isAtLeastAA && maxAvgLvrPct <= 80 && fullRecourseLoans &&
     subjectToRiskRetention && onlyResidentialMortgagesNoStructuredProducts &&
     notIssuedByAdiOrAffiliate && meets20PctStressAndLiquidity then
    some .hqla2bRmbs
  else
    none

/-- Attachment A, Paragraph 10(b)(iii) & 12(b)(ii)(1) IRB route: Classify corporate debt
    securities for IRB-approved ADIs using internal PD mapped to AA- or BBB- thresholds. -/
def classifyHqlaCorpDebtByIrbPd
    (irbApproved : Bool)
    (notIssuedByFiOrAffiliate : Bool)
    (internalPdBps : Nat)
    (aaMinusMaxPdBps : Nat)
    (bbbMinusMaxPdBps : Nat)
    (meetsMarketLiquidityCriteria : Bool) : Option HqlaCategory :=
  if !irbApproved || !notIssuedByFiOrAffiliate || !meetsMarketLiquidityCriteria then none
  else if internalPdBps <= aaMinusMaxPdBps then some .hqla2a
  else if internalPdBps <= bbbMinusMaxPdBps then some .hqla2bCorporateDebt
  else none

/-- Attachment A, Paragraph 12(c): Classify common equities as HQLA2B (50% haircut). -/
def classifyHqla2bEquity
    (notIssuedByFiOrAffiliate : Bool)
    (exchangeTradedAndCentrallyCleared : Bool)
    (inMajorIndexOrAsx200 : Bool)
    (inAudOrMajorCurrency : Bool)
    (meets40PctStressAndLiquidity : Bool) : Option HqlaCategory :=
  if notIssuedByFiOrAffiliate && exchangeTradedAndCentrallyCleared &&
     inMajorIndexOrAsx200 && inAudOrMajorCurrency && meets40PctStressAndLiquidity then
    some .hqla2bEquity
  else
    none

/-- Attachment A, Paragraphs 22–24: Operational requirements for HQLA / CLF collateral.
    Note: Per Paragraph 25, the 30-calendar-day grace period applies to rating/eligibility
    downgrades (`becameIneligibleWithin30Days` on `HqlaAsset`), NOT to encumbrance or
    pledging operational requirements. -/
structure HqlaOperationalCompliance where
  /-- Para 22(a): Available to convert into cash at any time. -/
  convertibleToCashAnytime : Bool
  /-- Para 22(b): Unencumbered and under control of liquidity management function. -/
  unencumberedAndControlled : Bool
  /-- Para 22(c): Control evidenced by separate pool or monetisation capability over 30-day stress. -/
  controlEvidenced : Bool
  /-- Para 23: Not pledged to secure/collateralise/credit-enhance or cover operational costs. -/
  notPledgedOrOperationalCost : Bool
  /-- Para 23: If received in reverse repo/SFT, not rehypothecated and legally/contractually available. -/
  notRehypothecatedIfReceivedInSft : Bool
  deriving DecidableEq, Repr

def HqlaOperationalCompliance.isMet (op : HqlaOperationalCompliance) : Bool :=
  op.convertibleToCashAnytime &&
  op.unencumberedAndControlled &&
  op.controlEvidenced &&
  op.notPledgedOrOperationalCost &&
  op.notRehypothecatedIfReceivedInSft

/-- An asset held in the HQLA portfolio with its market value and eligibility attributes. -/
structure HqlaAsset where
  id : String
  category : HqlaCategory
  currency : Currency
  marketValue : Money
  operational : HqlaOperationalCompliance
  /-- Para 9(e): True if this is a non-0% RW domestic sovereign/CB debt security issued in a
      foreign currency (subject to aggregate cap at stressed net cash outflows in that currency). -/
  isNonZeroRwForeignSovInForeignCurrency : Bool := false
  /-- Para 24, 29: At Level 2, whether excess liquidity is freely transferable across borders. -/
  freelyTransferableAtLevel2 : Bool := true
  /-- Para 29: When `freelyTransferableAtLevel2 = false`, the portion eligible at Level 2
      (up to the subsidiary/jurisdiction's own net cash outflow needs; defaults to 0 if `none`). -/
  level2TransferableCap : Option Money := none
  /-- Para 9(c)(iv), 10(a)(iv), 10(b)(i)-(ii), 12(a)(i), 12(b)(i), 12(c)(i):
      Whether issuer/originator restriction is satisfied. -/
  issuerRestrictionSatisfied : Bool := true
  /-- Para 10, 12: Recognised by APRA or host prudential regulator. -/
  recognisedByRegulator : Bool := true
  /-- Para 25: 30-calendar-day grace period if asset recently became ineligible (e.g. rating downgrade). -/
  becameIneligibleWithin30Days : Bool := false
  /-- Para 25: Optional numeric day count since ineligibility (when supplied, must be <= 30). -/
  daysSinceIneligible : Option Nat := none
  /-- Para 26(b), 28: Host-recognised alternative liquid asset (only eligible at Level 2). -/
  isHostRecognisedAla : Bool := false
  /-- Para 11: HQLA2A diversification requirement satisfied (must be explicitly attested). -/
  hqla2aDiversified : Bool := false
  /-- Paras 10 & 12: Optional observed 30-day stress price decline / haircut increase
      (when supplied, checked against 10% HQLA2A, 20% HQLA2B RMBS/corp, 40% HQLA2B equity). -/
  max30dPriceDecline : Option Rate := none
  max30dHaircutIncrease : Option Rate := none
  deriving DecidableEq, Repr

/-- Eligible market value before haircut and before aggregate para 9(e) currency cap. -/
def HqlaAsset.eligibleMarketValue (basis : ConsolidationBasis) (a : HqlaAsset) : Money :=
  let basisOk := match basis with
    | .level1 => !a.isHostRecognisedAla
    | .level2 => true
  let divOk := a.category != .hqla2a || a.hqla2aDiversified
  let stressOk := a.category.meetsStressThreshold
    (a.max30dPriceDecline.getD 0) (a.max30dHaircutIncrease.getD 0)
  let graceOk := a.becameIneligibleWithin30Days && match a.daysSinceIneligible with
    | some d => d <= 30
    | none => true
  let eligibilityOk := (a.issuerRestrictionSatisfied && a.recognisedByRegulator) || graceOk
  if a.operational.isMet && eligibilityOk && basisOk && divOk && stressOk then
    let mv := nonNeg a.marketValue
    match basis, a.freelyTransferableAtLevel2 with
    | .level2, false => min mv (nonNeg (a.level2TransferableCap.getD 0))
    | _, _ => mv
  else
    0

/-- Eligible value after applying category haircut (Attachment A paras 9, 10, 12). -/
def HqlaAsset.valueAfterHaircut (basis : ConsolidationBasis) (a : HqlaAsset) : Money :=
  a.eligibleMarketValue basis * a.category.weight

/-- Attachment A, Paragraph 8: Raw short-term (<= 30 calendar days) SFT or collateral swap leg
    for computing unwind adjustments to HQLA1, HQLA2A, and HQLA2B. -/
structure ShortTermSftLeg where
  /-- Post-haircut HQLA value of the cash/security leg received or delivered.
      Positive = asset returned to ADI upon unwind; negative = asset delivered by ADI upon unwind. -/
  hqla1Delta : Rat := 0
  hqla2aDelta : Rat := 0
  hqla2bDelta : Rat := 0
  /-- Para 8: Whether the exchanged HQLA meets (or would meet if unencumbered) paras 22–25. -/
  meetsOperationalReqs : Bool := true
  /-- Optional APS 110 Att D SFT netting criteria (if ineligible, positive HQLA netting benefit is disallowed). -/
  nettingCriteria : Option External.SftNettingCriteria := none
  deriving DecidableEq, Repr

/-- Attachment A, Paragraph 8: Net unwind adjustments per HQLA tier. -/
structure HqlaUnwindAdjustments where
  deltaHqla1 : Rat := 0
  deltaHqla2a : Rat := 0
  deltaHqla2b : Rat := 0
  deriving DecidableEq, Repr

private def applySftNetting (leg : ShortTermSftLeg) (d : Rat) : Rat :=
  let netOk := match leg.nettingCriteria with
    | some c => c.isEligible
    | none => true
  if netOk then d else min d 0

/-- Derive `HqlaUnwindAdjustments` from individual <= 30-day SFT / collateral swap legs. -/
def computeUnwindAdjustments (legs : List ShortTermSftLeg) : HqlaUnwindAdjustments :=
  let active := legs.filter (·.meetsOperationalReqs)
  { deltaHqla1 := sumBy active (fun l => applySftNetting l l.hqla1Delta)
    deltaHqla2a := sumBy active (fun l => applySftNetting l l.hqla2aDelta)
    deltaHqla2b := sumBy active (fun l => applySftNetting l l.hqla2bDelta) }

/-- Attachment A, Paragraphs 6–8: Compute capped HQLA stock (excluding CLF). -/
structure HqlaCapBreakdown where
  unadjHqla1 : Money
  unadjHqla2a : Money
  unadjHqla2b : Money
  adjHqla1 : Money
  adjHqla2a : Money
  adjHqla2b : Money
  excess15Cap : Money
  excess40Cap : Money
  stockExClf : Money
  deriving DecidableEq, Repr

def computeHqlaCaps
    (unadj1 unadj2a unadj2b : Money)
    (unwind : HqlaUnwindAdjustments := {}) : HqlaCapBreakdown :=
  let adj1 := nonNeg (unadj1 + unwind.deltaHqla1)
  let adj2a := nonNeg (unadj2a + unwind.deltaHqla2a)
  let adj2b := nonNeg (unadj2b + unwind.deltaHqla2b)
  let cap15Limit : Money := (15 : Rat) / 85 * (adj1 + adj2a)
  let excess15 := nonNeg (adj2b - cap15Limit)
  let adj2After15 := adj2a + (adj2b - excess15)
  let cap40Limit : Money := (2 : Rat) / 3 * adj1
  let excess40 := nonNeg (adj2After15 - cap40Limit)
  let unadjTotal := unadj1 + unadj2a + unadj2b
  let stockExClf := nonNeg (unadjTotal - excess15 - excess40)
  { unadjHqla1 := unadj1
    unadjHqla2a := unadj2a
    unadjHqla2b := unadj2b
    adjHqla1 := adj1
    adjHqla2a := adj2a
    adjHqla2b := adj2b
    excess15Cap := excess15
    excess40Cap := excess40
    stockExClf }

/-- Attachment A, Paragraphs 13–21: Committed Liquidity Facility (CLF) inputs (AUD LCR only). -/
structure ClfInput where
  /-- Para 17–20: Approved by APRA and agreed with the RBA. -/
  approvedByApra : Bool
  /-- Para 15(a): Contractual amount of the CLF. -/
  contractualAmount : Money
  /-- Para 15(b): Market value (after RBA margins) of eligible CLF securities pledged
      under maturing secured funding transactions with 0% run-off rate. -/
  pledgedUnderZeroRunoffSecFunding : Money
  /-- Para 15(b): Market value (after RBA margins) of eligible CLF securities received
      under maturing secured lending transactions (not covering short positions) with 0% inflow rate. -/
  receivedUnderZeroInflowSecLending : Money
  /-- Para 15(c): Market value of assets held as collateral for the CLF after RBA margins
      (must satisfy para 16 operational & diversification requirements). -/
  collateralHeldAfterRbaMargins : Money
  /-- Para 16(a): CLF collateral satisfies operational requirements (paras 22–25). -/
  collateralOperational : Option HqlaOperationalCompliance := none
  /-- Para 16(b): CLF collateral satisfies diversification requirements. -/
  collateralDiversified : Bool := false
  /-- Para 17: Annual CLF re-approval is current. -/
  annualReapprovalCurrent : Bool := false
  /-- Para 18: CLF amount is incorporated in the ADI's funding strategy. -/
  includedInFundingStrategy : Bool := false
  /-- Para 18: Every reasonable effort made to manage liquidity via the balance sheet first. -/
  balanceSheetEffortsExhausted : Bool := false
  /-- Para 21: True if funds were drawn under the CLF for non-intraday / non-ESA end-of-day purposes. -/
  clfDrawnNonIntraday : Bool := false
  /-- Para 21: If drawn non-intraday, senior management attestation and APRA notice completed. -/
  drawdownAttestedAndNotifiedToApra : Bool := false
  deriving DecidableEq, Repr

/-- Attachment A, Paragraphs 16–21: Whether CLF governance, operational, and diversification conditions hold. -/
def ClfInput.processEligible (clf : ClfInput) : Bool :=
  let opOk := match clf.collateralOperational with
    | some op => op.isMet
    | none => true
  opOk &&
  clf.collateralDiversified &&
  clf.annualReapprovalCurrent &&
  clf.includedInFundingStrategy &&
  clf.balanceSheetEffortsExhausted &&
  (!clf.clfDrawnNonIntraday || clf.drawdownAttestedAndNotifiedToApra)

/-- Attachment A, Paragraph 15(a)–(c) & 16–21: Lowest of the three CLF bounds
    when APRA-approved and all CLF process/diversification conditions are met. -/
def ClfInput.recognisedAmount (clf : ClfInput) : Money :=
  if !clf.approvedByApra then 0
  else if !clf.processEligible then 0
  else
    let a := nonNeg clf.contractualAmount
    let b := nonNeg (clf.contractualAmount
      - nonNeg clf.pledgedUnderZeroRunoffSecFunding
      + nonNeg clf.receivedUnderZeroInflowSecLending)
    let c := nonNeg clf.collateralHeldAfterRbaMargins
    min a (min b c)

/-- Attachment A, Paragraph 9(e): Apply aggregate per-currency cap to non-0% RW
    domestic sovereign/CB debt securities issued in foreign currencies.
    Deduplicates currencies across `para9eAssets` so duplicate keys in
    `foreignSovCurrencyCaps` cannot double-count; currencies absent from
    `foreignSovCurrencyCaps` receive a 0 cap (stressed net cash outflows = 0). -/
def computeCappedForeignSovHqla1
    (basis : ConsolidationBasis)
    (assets : List HqlaAsset)
    (foreignSovCurrencyCaps : List (Currency × Money)) : Money :=
  let para9eAssets := assets.filter (fun a => a.category == .hqla1 && a.isNonZeroRwForeignSovInForeignCurrency)
  let distinctCurrencies := (para9eAssets.map (·.currency)).eraseDups
  distinctCurrencies.foldl (fun acc cur =>
    let curTotal := sumBy (para9eAssets.filter (·.currency == cur)) (HqlaAsset.valueAfterHaircut basis)
    match foreignSovCurrencyCaps.find? (fun (c, _) => c == cur) with
    | some (_, cap) => acc + min curTotal (nonNeg cap)
    | none => acc
  ) 0

/-- Compute total HQLA stock (including CLF for AUD LCR of locally incorporated ADIs). -/
def computeStockOfHqla
    (basis : ConsolidationBasis)
    (assets : List HqlaAsset)
    (unwind : HqlaUnwindAdjustments := {})
    (clf : Option ClfInput := none)
    (foreignSovCurrencyCaps : List (Currency × Money) := []) : Money × HqlaCapBreakdown :=
  let standardHqla1 := sumBy
    (assets.filter (fun a => a.category == .hqla1 && !a.isNonZeroRwForeignSovInForeignCurrency))
    (HqlaAsset.valueAfterHaircut basis)
  let cappedPara9e := computeCappedForeignSovHqla1 basis assets foreignSovCurrencyCaps
  let h1 := standardHqla1 + cappedPara9e
  let h2a := sumBy (assets.filter (·.category == .hqla2a)) (HqlaAsset.valueAfterHaircut basis)
  let h2b := sumBy (assets.filter (fun a =>
    a.category == .hqla2bRmbs || a.category == .hqla2bCorporateDebt || a.category == .hqla2bEquity))
    (HqlaAsset.valueAfterHaircut basis)
  let caps := computeHqlaCaps h1 h2a h2b unwind
  let clfAmt := match clf with
    | some c => c.recognisedAmount
    | none => 0
  (caps.stockExClf + clfAmt, caps)

end APS210.LCR
