import APS210.Basic
import APS210.Facts
import APS210.ExternalStandards

namespace APS210.LCR

open APS210.Facts

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
    (APS 112 risk weight, ECAI credit rating, issuer type, and liquidity/market criteria).
    Supports optional short-term equivalent rating fallback (para 12(b)(ii)(1)) and
    separate 10% (HQLA2A) vs 20% (HQLA2B) stress thresholds. -/
def classifyHqlaDebtSecurity
    (isSovereignCbPseMdb : Bool)
    (aps112RiskWeightPct : Nat)
    (rating : CreditRating)
    (isIssuedByFinancialInstitutionOrAssoc : Bool)
    (isCoveredBondNotOwnOrAssoc : Bool)
    (isPlainVanilla : Bool)
    (deepActiveMarketAndProvenLiquidity : Bool)
    (shortTermEquivAPlusToBBBMinus : Bool := false)
    (meets10PctStress : Bool := true)
    (meets20PctStress : Bool := true) : Option HqlaCategory :=
  if !deepActiveMarketAndProvenLiquidity then none
  else if isSovereignCbPseMdb && aps112RiskWeightPct == 0 && !isIssuedByFinancialInstitutionOrAssoc then
    some .hqla1
  else if isSovereignCbPseMdb && aps112RiskWeightPct == 20 && !isIssuedByFinancialInstitutionOrAssoc &&
      meets10PctStress then
    some .hqla2a
  else if (!isSovereignCbPseMdb && !isIssuedByFinancialInstitutionOrAssoc && isPlainVanilla || isCoveredBondNotOwnOrAssoc) &&
      rating.isAtLeastAAMinus && meets10PctStress then
    some .hqla2a
  else if !isSovereignCbPseMdb && !isIssuedByFinancialInstitutionOrAssoc &&
      (rating.isBetweenAPlusAndBBBMinus || shortTermEquivAPlusToBBBMinus) && meets20PctStress then
    some .hqla2bCorporateDebt
  else
    none

/-- Derive HQLA category for a sovereign/CB/PSE/MDB debt security directly from multiple
    APS 112 ECAI ratings using APS 112 Attachment B & Attachment F.
    Enforces Attachment A paras 9(c)(iv) & 10(a)(iv) (not an obligation of a financial
    institution or associated entity) and restricts `isZeroRwMdbOrInst` to eligible
    sovereign/CB/PSE/MDB counterparties. -/
def classifySovereignPseMdbHqla
    (cp : CounterpartyKind)
    (ecaiGrades : List External.CreditRatingGrade)
    (isAusGovtOrRbaInAud : Bool := false)
    (isZeroRwMdbOrInst : Bool := false)
    (deepActiveMarketAndProvenLiquidity : Bool := true)
    (notIssuedByFiOrAffiliate : Bool := true)
    (meets10PctStress : Bool := true) : Option HqlaCategory :=
  if !deepActiveMarketAndProvenLiquidity || !notIssuedByFiOrAffiliate then none
  else
    let isEligibleCp := match cp with
      | .sovereign _ _ | .centralBank | .rba | .pse _ | .mdb _ => true
      | _ => false
    if !isEligibleCp then none
    else
      let effGrade := External.resolveMultipleEcaiAssessments ecaiGrades
      let rw := match cp with
        | .sovereign _ _ | .centralBank | .rba =>
          External.sovereignRiskWeightPct effGrade isAusGovtOrRbaInAud isZeroRwMdbOrInst
        | .pse _ => External.domesticPseRiskWeightPct effGrade
        | .mdb _ => External.bankOrNonZeroMdbRiskWeightPct effGrade false
        | _ => 100
      if isZeroRwMdbOrInst || rw == 0 then some .hqla1
      else if rw == 20 && meets10PctStress then some .hqla2a
      else none

/-- Attachment A, Paragraphs 9(d)–(e): Classify non-0% RW sovereign/central bank debt
    issued in domestic currency (`9(d)`) or foreign currency (`9(e)`, subject to
    per-currency stressed net cash outflow cap). -/
def classifyHqla9d9e
    (isDomesticCurrency : Bool)
    (isForeignCurrencyOfSubsidiaryOrBranch : Bool)
    (meetsMarketLiquidityCriteria : Bool := true)
    (isNonZeroRwSovereignOrCb : Bool := true) : Option (HqlaCategory × Bool) :=
  if !isNonZeroRwSovereignOrCb || !meetsMarketLiquidityCriteria then none
  else if isDomesticCurrency then some (.hqla1, false)
  else if isForeignCurrencyOfSubsidiaryOrBranch then some (.hqla1, true)
  else none

/-- Attachment A, Paragraph 12(a): Classify RMBS as HQLA2B (25% haircut).
    Includes 12(a)(i) (not issued by nor underlying exposures originated by ADI or associated
    entity), 12(a)(ii) (AA or higher, or short-term equivalent), 12(a)(v) (residential mortgages
    only, no structured products), and 12(a)(vi) (full recourse, max average LVR <= 80%). -/
def classifyHqla2bRmbs
    (rating : CreditRating)
    (maxAvgLvrPct : Rat)
    (fullRecourseLoans : Bool)
    (subjectToRiskRetention : Bool)
    (onlyResidentialMortgagesNoStructuredProducts : Bool)
    (notIssuedByAdiOrAffiliate : Bool)
    (meets20PctStressAndLiquidity : Bool)
    (notOriginatedByAdiOrAffiliate : Bool := true)
    (shortTermEquivAtLeastAA : Bool := false) : Option HqlaCategory :=
  if (rating.isAtLeastAA || shortTermEquivAtLeastAA) && maxAvgLvrPct <= 80 && fullRecourseLoans &&
     subjectToRiskRetention && onlyResidentialMortgagesNoStructuredProducts &&
     notIssuedByAdiOrAffiliate && notOriginatedByAdiOrAffiliate && meets20PctStressAndLiquidity then
    some .hqla2bRmbs
  else
    none

/-- Attachment A, Paragraph 10(b)(i)–(v) & 12(b)(ii)(2) IRB route: Classify corporate debt
    securities (and covered bonds for HQLA2A) for IRB-approved ADIs using internal PD mapped
    to AA- or BBB- thresholds, only where the asset has no credit assessment by a recognised ECAI. -/
def classifyHqlaCorpDebtByIrbPd
    (irbApproved : Bool)
    (notIssuedByFiOrAffiliate : Bool)
    (internalPdBps : Nat)
    (aaMinusMaxPdBps : Nat)
    (bbbMinusMaxPdBps : Nat)
    (meetsMarketLiquidityCriteria : Bool)
    (unratedByRecognisedEcai : Bool := true)
    (isPlainVanilla : Bool := true)
    (isCoveredBondNotOwnOrAssoc : Bool := false)
    (meets10PctStress : Bool := true)
    (meets20PctStress : Bool := true) : Option HqlaCategory :=
  if !irbApproved || !unratedByRecognisedEcai || !meetsMarketLiquidityCriteria then none
  else if ((notIssuedByFiOrAffiliate && isPlainVanilla) || isCoveredBondNotOwnOrAssoc) &&
      meets10PctStress && internalPdBps <= aaMinusMaxPdBps then
    some .hqla2a
  else if notIssuedByFiOrAffiliate && meets20PctStress && internalPdBps <= bbbMinusMaxPdBps then
    some .hqla2bCorporateDebt
  else none

/-- Attachment A, Paragraph 12(c): Classify common equities as HQLA2B (50% haircut).
    Para 12(c)(iv): Denominated in the currency of the jurisdiction where liquidity risk is taken. -/
def classifyHqla2bEquity
    (notIssuedByFiOrAffiliate : Bool)
    (exchangeTradedAndCentrallyCleared : Bool)
    (inMajorIndexOrAsx200 : Bool)
    (inLiquidityRiskJurisdictionCurrency : Bool)
    (meets40PctStressAndLiquidity : Bool) : Option HqlaCategory :=
  if notIssuedByFiOrAffiliate && exchangeTradedAndCentrallyCleared &&
     inMajorIndexOrAsx200 && inLiquidityRiskJurisdictionCurrency && meets40PctStressAndLiquidity then
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
  freelyTransferableAtLevel2 : Bool
  /-- Para 29: Legal entity or jurisdiction holding the asset. Non-transferable assets in the same
      group share one Level 2 cap (`LcrInput.level2TransferableCaps`), applied to the group total. -/
  level2TransferGroup : String
  /-- Para 9(c)(iv), 10(a)(iv), 10(b)(i)-(ii), 12(a)(i), 12(b)(i), 12(c)(i):
      Whether issuer/originator restriction is satisfied. -/
  issuerRestrictionSatisfied : Bool := false
  /-- Para 10, 12: Recognised by APRA or host prudential regulator. -/
  recognisedByRegulator : Bool := false
  /-- Para 25: Calendar days since the asset became ineligible (e.g. rating downgrade).
      `known none` = it has not become ineligible; the 30-day grace applies only to `known (some d)`
      with `d <= 30`. An unknown day count grants no grace. -/
  daysSinceIneligible : Reading (Option Nat)
  /-- Para 26(b), 28: Host-recognised alternative liquid asset (only eligible at Level 2). -/
  isHostRecognisedAla : Bool := false
  /-- Para 26(b), 27: Whether the offshore host jurisdiction applies the Basel Committee global
      liquidity framework (required to include host-recognised ALA at Level 2). -/
  hostJurisdictionAppliesBasel : Bool := true
  /-- Para 28: Whether APRA has allowed inclusion of the host-recognised liquid asset. -/
  apraApprovedHostAla : Bool := true
  /-- Para 9(b): For central bank balances, whether the balance may be drawn down in times of
      stress (defaults to true for non-reserve assets; set false for undrawable statutory reserves). -/
  drawableInStress : Bool := true
  /-- Para 11: HQLA2A diversification requirement satisfied (must be explicitly attested). -/
  hqla2aDiversified : Bool := false
  /-- Paras 10 & 12: Observed maximum 30-day stress price decline / haircut increase, checked
      against 10% HQLA2A, 20% HQLA2B RMBS/corp, 40% HQLA2B equity. Not known means the
      stress criterion is not met for HQLA2 assets. -/
  max30dPriceDecline : Reading Rate
  max30dHaircutIncrease : Reading Rate
  observation : ObservationContext
  deriving DecidableEq, Repr

/-- Paras 10 & 12: Stress criterion. HQLA1 has none; HQLA2 needs both observations known. -/
def HqlaAsset.stressOk (a : HqlaAsset) : Bool :=
  match a.category, a.max30dPriceDecline, a.max30dHaircutIncrease with
  | .hqla1, _, _ => true
  | c, .known p, .known h => c.meetsStressThreshold p h
  | _, _, _ => false

/-- Para 25: Grace applies only with a known day count of at most 30. -/
def HqlaAsset.graceOk (a : HqlaAsset) : Bool :=
  match a.daysSinceIneligible with
  | .known (some d) => decide (d <= 30)
  | _ => false

/-- Eligible market value before haircut, before the aggregate para 9(e) currency cap and before the
    group-level Level 2 transferability cap (`level2TransferScale`).
    - Paras 9, 10, 12, 28: Regulator recognition is required for HQLA2A, HQLA2B, and host ALA
      (not domestic HQLA1).
    - Para 25: The 30-day grace period retains an asset that became ineligible (e.g. rating
      downgrade, regulator de-recognition, or stress threshold breach), while operational
      requirements (paras 22–23) and drawdown availability (para 9(b)) remain mandatory. -/
def HqlaAsset.eligibleMarketValue (basis : ConsolidationBasis) (a : HqlaAsset) : Money :=
  let basisOk := match basis with
    | .level1 => !a.isHostRecognisedAla
    | .level2 => !a.isHostRecognisedAla || (a.hostJurisdictionAppliesBasel && a.apraApprovedHostAla)
  let divOk := a.category != .hqla2a || a.hqla2aDiversified
  let regOk := (a.category == .hqla1 && !a.isHostRecognisedAla) || a.recognisedByRegulator
  -- Para 25: Once an asset has become ineligible (`known (some d)`), it may only remain in HQLA
  -- during the 30-calendar-day grace period (`d <= 30`); expired grace (`d > 30`) excludes it.
  let notBecameIneligible : Bool := match a.daysSinceIneligible with
    | .known none => true
    | _ => false
  let eligibilityOk := (notBecameIneligible && a.issuerRestrictionSatisfied && regOk && a.stressOk) || a.graceOk
  if a.operational.isMet && a.drawableInStress && eligibilityOk && basisOk && divOk then
    nonNeg a.marketValue
  else
    0

def HqlaAsset.dataIssues (a : HqlaAsset) : List DataIssue :=
  (if a.category == .hqla1 then [] else
    readingIssues a.id "max30dPriceDecline" "stress criterion not met; excluded from HQLA" a.max30dPriceDecline ++
    readingIssues a.id "max30dHaircutIncrease" "stress criterion not met; excluded from HQLA" a.max30dHaircutIncrease) ++
  readingIssues a.id "daysSinceIneligible" "no para 25 grace period" a.daysSinceIneligible

/-- Eligible value after applying category haircut (Attachment A paras 9, 10, 12). -/
def HqlaAsset.valueAfterHaircut (basis : ConsolidationBasis) (a : HqlaAsset) : Money :=
  a.eligibleMarketValue basis * a.category.weight

/-- Lowest cap supplied for a key; a key with no cap has a cap of 0. -/
def lowestCapFor {κ : Type} [BEq κ] (caps : List (κ × Money)) (key : κ) : Money :=
  match caps.filter (fun (k, _) => k == key) with
  | [] => 0
  | (_, c0) :: rest => rest.foldl (fun m (_, c) => min m (nonNeg c)) (nonNeg c0)

/-- Attachment A, Paragraph 29: At Level 2, non-transferable assets held in one legal entity or
    jurisdiction are recognised only up to that group's cap, applied to the group total (post-haircut)
    and allocated pro rata across the group's assets. Transferable assets and Level 1 are unscaled. -/
def level2TransferScale (basis : ConsolidationBasis) (assets : List HqlaAsset)
    (level2TransferableCaps : List (String × Money)) (a : HqlaAsset) : Rat :=
  match basis, a.freelyTransferableAtLevel2 with
  | .level2, false =>
    let grp := assets.filter (fun b => !b.freelyTransferableAtLevel2 && b.level2TransferGroup == a.level2TransferGroup)
    let total := sumBy grp (HqlaAsset.valueAfterHaircut basis)
    let cap := lowestCapFor level2TransferableCaps a.level2TransferGroup
    if total <= 0 then 0 else min 1 (cap / total)
  | _, _ => 1

/-- Post-haircut value after the para 29 Level 2 group cap. -/
def HqlaAsset.cappedValue (basis : ConsolidationBasis) (assets : List HqlaAsset)
    (level2TransferableCaps : List (String × Money)) (a : HqlaAsset) : Money :=
  a.valueAfterHaircut basis * level2TransferScale basis assets level2TransferableCaps a

/-- Attachment A, Paragraph 8: Raw short-term (<= 30 calendar days) SFT or collateral swap leg
    for computing unwind adjustments to HQLA1, HQLA2A, and HQLA2B.
    Note: Per Para 8, unwind applies only to transactions involving the exchange of HQLA for
    HQLA (not non-HQLA for cash), and unwinds both legs symmetrically (APS 110 Att D para 38
    netting applies to NSFR Att C para 26, not LCR Att A para 8). -/
structure ShortTermSftLeg where
  /-- Currency of the SFT/collateral swap leg (needed for AUD-only LCR filtering). -/
  currency : Currency := .aud
  /-- Residual maturity in calendar days (must be <= 30 days to enter LCR unwind). -/
  residualMaturityDays : Nat := 30
  /-- Post-haircut HQLA value of the cash/security leg received or delivered.
      Positive = asset returned to ADI upon unwind; negative = asset delivered by ADI upon unwind. -/
  hqla1Delta : Rat := 0
  hqla2aDelta : Rat := 0
  hqla2bDelta : Rat := 0
  /-- Para 8: Whether the exchanged HQLA meets (or would meet if unencumbered) paras 22–25. -/
  meetsOperationalReqs : Bool
  /-- Para 8: Whether BOTH legs of the transaction involve HQLA (transactions exchanging
      non-HQLA collateral for cash are excluded from Para 8 unwinds). -/
  bothLegsAreHqla : Bool := true
  /-- Retained for metadata/auditability; APS 110 Att D netting does not restrict LCR Para 8 unwinds. -/
  nettingCriteria : Option External.SftNettingCriteria := none
  deriving DecidableEq, Repr

/-- Attachment A, Paragraph 8: Net unwind adjustments per HQLA tier. -/
structure HqlaUnwindAdjustments where
  deltaHqla1 : Rat := 0
  deltaHqla2a : Rat := 0
  deltaHqla2b : Rat := 0
  deriving DecidableEq, Repr

/-- Derive `HqlaUnwindAdjustments` from individual <= 30-day SFT / collateral swap legs,
    enforcing HQLA-for-HQLA scope, operational requirements, <= 30-day horizon, and AUD
    filtering when `isAudOnly = true`. Both legs unwind symmetrically per Para 8. -/
def computeUnwindAdjustments (legs : List ShortTermSftLeg) (isAudOnly : Bool := false) : HqlaUnwindAdjustments :=
  let active := legs.filter (fun l =>
    l.bothLegsAreHqla && l.meetsOperationalReqs && l.residualMaturityDays ≤ 30 &&
    (!isAudOnly || l.currency == .aud))
  { deltaHqla1 := sumBy active (·.hqla1Delta)
    deltaHqla2a := sumBy active (·.hqla2aDelta)
    deltaHqla2b := sumBy active (·.hqla2bDelta) }

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
  let u1 := nonNeg unadj1
  let u2a := nonNeg unadj2a
  let u2b := nonNeg unadj2b
  let adj1 := nonNeg (unadj1 + unwind.deltaHqla1)
  let adj2a := nonNeg (unadj2a + unwind.deltaHqla2a)
  let adj2b := nonNeg (unadj2b + unwind.deltaHqla2b)
  -- Paras 6–7 & ARS 210.0 / Basel III Annex 1: Both unadjusted and unwind-adjusted 15%/40% caps
  -- must be respected; max adjusted HQLA2B is min(15/85*(h1+h2a), 1/4*h1).
  -- Compare total cap deductions across the two states so 15% and 40% excesses are never
  -- double-counted across unadjusted vs. unwind-adjusted states.
  let unadjCap15 : Money := min ((15 : Rat) / 85 * (u1 + u2a)) ((1 : Rat) / 4 * u1)
  let unadjExcess15 := nonNeg (u2b - unadjCap15)
  let unadjExcess40 := nonNeg ((u2a + (u2b - unadjExcess15)) - (2 : Rat) / 3 * u1)
  let adjCap15 : Money := min ((15 : Rat) / 85 * (adj1 + adj2a)) ((1 : Rat) / 4 * adj1)
  let adjExcess15 := nonNeg (adj2b - adjCap15)
  let adjExcess40 := nonNeg ((adj2a + (adj2b - adjExcess15)) - (2 : Rat) / 3 * adj1)
  let totalExcess := max (unadjExcess15 + unadjExcess40) (adjExcess15 + adjExcess40)
  let unadjTotal := unadj1 + unadj2a + unadj2b
  let stockExClf := nonNeg (unadjTotal - totalExcess)
  { unadjHqla1 := unadj1
    unadjHqla2a := unadj2a
    unadjHqla2b := unadj2b
    adjHqla1 := adj1
    adjHqla2a := adj2a
    adjHqla2b := adj2b
    excess15Cap := adjExcess15
    excess40Cap := adjExcess40
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
  /-- Para 16(a): CLF collateral operational requirements (paras 22–25). Not known fails para 16. -/
  collateralOperational : Reading HqlaOperationalCompliance
  /-- Para 16(b): CLF collateral satisfies diversification requirements. -/
  collateralDiversified : Bool := false
  /-- Para 19: Annual CLF re-approval is current. -/
  annualReapprovalCurrent : Bool := false
  /-- Para 18: CLF amount is incorporated in the ADI's funding strategy. -/
  includedInFundingStrategy : Bool := false
  /-- Para 18: Every reasonable effort made to manage liquidity via the balance sheet first. -/
  balanceSheetEffortsExhausted : Bool := false
  /-- Para 21: True if funds were drawn under the CLF for non-intraday / non-ESA end-of-day purposes. -/
  clfDrawnNonIntraday : Bool := false
  /-- Para 21: If drawn non-intraday, senior management attestation and APRA notice completed. -/
  drawdownAttestedAndNotifiedToApra : Bool := false
  /-- Para 25: Optional CLF collateral that recently became ineligible (kept in para 15(c) bound
      for up to 30 calendar days; excluded from bound `c` once > 30 days). -/
  recentlyIneligibleCollateralAfterRbaMargins : Money := 0
  /-- Para 25: Calendar days since `recentlyIneligibleCollateralAfterRbaMargins` became ineligible.
      It stays in the 15(c) bound only with a known day count of at most 30. -/
  recentlyIneligibleCollateralDays : Reading Nat
  observation : ObservationContext
  deriving DecidableEq, Repr

/-- Attachment A, Paragraphs 16–21: Whether CLF governance, operational, and diversification conditions hold. -/
def ClfInput.processEligible (clf : ClfInput) : Bool :=
  let opOk := match clf.collateralOperational with
    | .known op => op.isMet
    | _ => false
  opOk &&
  clf.collateralDiversified &&
  clf.annualReapprovalCurrent &&
  clf.includedInFundingStrategy &&
  clf.balanceSheetEffortsExhausted &&
  (!clf.clfDrawnNonIntraday || clf.drawdownAttestedAndNotifiedToApra)

/-- Attachment A, Paragraph 15(a)–(c), 16–21 & 25: Lowest of the three CLF bounds
    when APRA-approved and all CLF process/diversification conditions are met.
    Para 25 grace period only retains/removes `recentlyIneligibleCollateralAfterRbaMargins`
    from the collateral bound in 15(c) rather than zeroing the entire CLF. -/
def ClfInput.recognisedAmount (clf : ClfInput) : Money :=
  if !clf.approvedByApra then 0
  else if !clf.processEligible then 0
  else
    let graceOk : Bool := match clf.recentlyIneligibleCollateralDays with
      | .known d => decide (d <= 30)
      | _ => false
    let a := nonNeg clf.contractualAmount
    let b := nonNeg (clf.contractualAmount
      - nonNeg clf.pledgedUnderZeroRunoffSecFunding
      + nonNeg clf.receivedUnderZeroInflowSecLending)
    let c := nonNeg clf.collateralHeldAfterRbaMargins +
      (if graceOk then nonNeg clf.recentlyIneligibleCollateralAfterRbaMargins else 0)
    min a (min b c)

def ClfInput.dataIssues (clf : ClfInput) : List DataIssue :=
  readingIssues "clf" "collateralOperational" "para 16 not met; CLF not recognised" clf.collateralOperational ++
  (if clf.recentlyIneligibleCollateralAfterRbaMargins > 0 then
    readingIssues "clf" "recentlyIneligibleCollateralDays"
      "recently ineligible collateral excluded from para 15(c) bound" clf.recentlyIneligibleCollateralDays
  else [])

/-- Attachment A, Paragraph 9(e): Apply aggregate per-currency cap to non-0% RW
    domestic sovereign/CB debt securities issued in foreign currencies.
    Deduplicates currencies across `para9eAssets` so duplicate keys in
    `foreignSovCurrencyCaps` cannot double-count; currencies absent from
    `foreignSovCurrencyCaps` receive a 0 cap (stressed net cash outflows = 0). -/
def computeCappedForeignSovHqla1
    (basis : ConsolidationBasis)
    (assets : List HqlaAsset)
    (foreignSovCurrencyCaps : List (Currency × Money))
    (level2TransferableCaps : List (String × Money) := []) : Money :=
  let para9eAssets := assets.filter (fun a => a.category == .hqla1 && a.isNonZeroRwForeignSovInForeignCurrency)
  let distinctCurrencies := (para9eAssets.map (·.currency)).eraseDups
  distinctCurrencies.foldl (fun acc cur =>
    let curTotal := sumBy (para9eAssets.filter (·.currency == cur))
      (HqlaAsset.cappedValue basis assets level2TransferableCaps)
    let matchingCaps := foreignSovCurrencyCaps.filter (fun (c, _) => c == cur)
    match matchingCaps with
    | [] => acc
    | (_, cap0) :: rest =>
      let effCap := rest.foldl (fun m (_, c) => min m (nonNeg c)) (nonNeg cap0)
      acc + min curTotal effCap
  ) 0

/-- Compute total HQLA stock (including CLF for AUD LCR of locally incorporated ADIs). -/
def computeStockOfHqla
    (basis : ConsolidationBasis)
    (assets : List HqlaAsset)
    (unwind : HqlaUnwindAdjustments := {})
    (clf : Option ClfInput := none)
    (foreignSovCurrencyCaps : List (Currency × Money) := [])
    (level2TransferableCaps : List (String × Money) := []) : Money × HqlaCapBreakdown :=
  let value := HqlaAsset.cappedValue basis assets level2TransferableCaps
  let standardHqla1 := sumBy
    (assets.filter (fun a => a.category == .hqla1 && !a.isNonZeroRwForeignSovInForeignCurrency)) value
  let cappedPara9e := computeCappedForeignSovHqla1 basis assets foreignSovCurrencyCaps level2TransferableCaps
  let h1 := standardHqla1 + cappedPara9e
  let h2a := sumBy (assets.filter (·.category == .hqla2a)) value
  let h2b := sumBy (assets.filter (fun a =>
    a.category == .hqla2bRmbs || a.category == .hqla2bCorporateDebt || a.category == .hqla2bEquity)) value
  let caps := computeHqlaCaps h1 h2a h2b unwind
  let clfAmt := match clf with
    | some c => c.recognisedAmount
    | none => 0
  (caps.stockExClf + clfAmt, caps)

end APS210.LCR
