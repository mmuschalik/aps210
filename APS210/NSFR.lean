import APS210.Basic
import APS210.ExternalStandards

namespace APS210.NSFR

/-- Attachment C, Paragraphs 8, 20–23: Effective residual maturity / encumbrance period. -/
inductive NsfrMaturityBucket where
  | lessThanSixMonths
  | sixMonthsToLessThanOneYear
  | oneYearOrMore
  deriving DecidableEq, Repr

/-- Attachment C, Paragraph 23: Remaining encumbrance period on an asset. -/
inductive EncumbrancePeriod where
  | unencumbered
  | lessThanSixMonths
  | sixMonthsToLessThanOneYear
  | oneYearOrMore
  deriving DecidableEq, Repr

/-- Attachment C, Paragraphs 11–15: ASF liability & capital categories. -/
inductive AsfCategory where
  -- 100% ASF (para 11)
  | regulatoryCapitalExTier2Lt1Yr
  | otherCapitalGe1YrNoEarlyOption
  | borrowingsAndLiabilitiesGe1Yr
  -- 95% ASF (para 12)
  | stableRetailAndSmeDepositsLt1Yr
  | intermediatedStableRetail12mNotice
  -- 90% ASF (para 13)
  | lessStableRetailAndSmeDepositsLt1Yr
  | intermediatedLessStableRetail12mNotice
  -- 50% ASF (para 14)
  | nonFinCorporateFundingLt1Yr
  | operationalDeposits
  | superannuationOperationalDeposits
  | intermediatedRetail6mNotice
  | sovereignPseMdbNdbFundingLt1Yr
  | otherFunding6mToLt1Yr
  -- 0% ASF (para 15)
  | otherFundingLt6m
  | noStatedMaturityShortOrOpen
  | deferredTaxLiability (maturity : NsfrMaturityBucket)
  | minorityInterest (maturity : NsfrMaturityBucket)
  | cooperativeNetworkDeposit (apraApprovedFactor : Option Rate := none)
  | allOtherLiabilitiesAndEquity
  deriving DecidableEq, Repr

/-- Base ASF factor per Attachment C, Paragraphs 11–15 (including footnote 18). -/
def AsfCategory.factor : AsfCategory → Rate
  | .regulatoryCapitalExTier2Lt1Yr => pct 100
  | .otherCapitalGe1YrNoEarlyOption => pct 100
  | .borrowingsAndLiabilitiesGe1Yr => pct 100
  | .stableRetailAndSmeDepositsLt1Yr => pct 95
  | .intermediatedStableRetail12mNotice => pct 95
  | .lessStableRetailAndSmeDepositsLt1Yr => pct 90
  | .intermediatedLessStableRetail12mNotice => pct 90
  | .nonFinCorporateFundingLt1Yr => pct 50
  | .operationalDeposits => pct 50
  | .superannuationOperationalDeposits => pct 50
  | .intermediatedRetail6mNotice => pct 50
  | .sovereignPseMdbNdbFundingLt1Yr => pct 50
  | .otherFunding6mToLt1Yr => pct 50
  | .otherFundingLt6m => pct 0
  | .noStatedMaturityShortOrOpen => pct 0
  | .deferredTaxLiability m => match m with
    | .oneYearOrMore => pct 100
    | .sixMonthsToLessThanOneYear => pct 50
    | .lessThanSixMonths => pct 0
  | .minorityInterest m => match m with
    | .oneYearOrMore => pct 100
    | .sixMonthsToLessThanOneYear => pct 50
    | .lessThanSixMonths => pct 0
  | .cooperativeNetworkDeposit f => min (nonNeg (f.getD 0)) (pct 85)
  | .allOtherLiabilitiesAndEquity => pct 0

/-- An ASF capital or liability item (Attachment C, Paragraph 7). -/
structure AsfItem where
  id : String
  category : AsfCategory
  carryingValue : Money
  /-- Paras 38–40: APRA-determined interdependent liability receives 0% ASF. -/
  isInterdependentApproved : Bool := false
  deriving DecidableEq, Repr

def AsfItem.effectiveFactor (item : AsfItem) : Rate :=
  if item.isInterdependentApproved then pct 0 else item.category.factor

def AsfItem.weightedAmount (item : AsfItem) : Money :=
  nonNeg item.carryingValue * item.effectiveFactor

/-- Attachment C, Paragraphs 29–37: On-balance-sheet RSF asset categories. -/
inductive RsfCategory where
  -- 0% RSF (para 29)
  | notesAndCoinsImmediatelyAvailable
  | centralBankReservesAndBalances (apraHigherFactor : Option Rate := none)
  | centralBankClaimsLt6m
  | tradeDateReceivablesStandardSettlement
  -- 5% RSF (para 30)
  | unencumberedHqla1NonZeroPctRsf
  -- 10% RSF (paras 31–32)
  | loanToFiLt6mSecuredByHqla1Rehypothecatable
  | clfThirdPartyDebtOrHostAla
  | selfSecuritisedClfEligible
  -- 15% RSF (para 33)
  | unencumberedHqla2a
  | otherLoanToFiLt6m
  -- 50% RSF (para 34)
  | unencumberedHqla2b
  | hqlaEncumbered6mToLt1Yr
  | loansToFiAndCentralBanks6mToLt1Yr
  | operationalDepositsAtOtherAdis
  | asx200NonFiEquities
  | otherNonHqlaLt1Yr
  -- 65% RSF (para 35)
  | standardResidentialPropertyLoansLvrLe80Ge1Yr
  | otherLoansNonFiRwLe35Ge1Yr
  -- 85% RSF (para 36)
  | initialMarginAndCcpDefaultFund (underlyingHigherFactor : Option Rate := none) (onBehalfOfCustomerNoGuarantee : Bool := false)
  | otherPerformingLoansNonFiGe1Yr
  | nonDefaultedNonHqlaSecuritiesGe1YrAndExchangeEquities
  | physicalTradedCommoditiesInclGold
  -- 100% RSF (para 37)
  | allOtherAssets
  deriving DecidableEq, Repr

/-- Unencumbered base RSF factor per Attachment C, Paragraphs 29–37. -/
def RsfCategory.baseFactor : RsfCategory → Rate
  | .notesAndCoinsImmediatelyAvailable => pct 0
  | .centralBankReservesAndBalances h => max (pct 0) (h.getD 0)
  | .centralBankClaimsLt6m => pct 0
  | .tradeDateReceivablesStandardSettlement => pct 0
  | .unencumberedHqla1NonZeroPctRsf => pct 5
  | .loanToFiLt6mSecuredByHqla1Rehypothecatable => pct 10
  | .clfThirdPartyDebtOrHostAla => pct 10
  | .selfSecuritisedClfEligible => pct 10
  | .unencumberedHqla2a => pct 15
  | .otherLoanToFiLt6m => pct 15
  | .unencumberedHqla2b => pct 50
  | .hqlaEncumbered6mToLt1Yr => pct 50
  | .loansToFiAndCentralBanks6mToLt1Yr => pct 50
  | .operationalDepositsAtOtherAdis => pct 50
  | .asx200NonFiEquities => pct 50
  | .otherNonHqlaLt1Yr => pct 50
  | .standardResidentialPropertyLoansLvrLe80Ge1Yr => pct 65
  | .otherLoansNonFiRwLe35Ge1Yr => pct 65
  | .initialMarginAndCcpDefaultFund higher onBehalf =>
    if onBehalf then pct 0 else max (pct 85) (higher.getD 0)
  | .otherPerformingLoansNonFiGe1Yr => pct 85
  | .nonDefaultedNonHqlaSecuritiesGe1YrAndExchangeEquities => pct 85
  | .physicalTradedCommoditiesInclGold => pct 85
  | .allOtherAssets => pct 100

/-- Attachment C, Paragraphs 23–24: Apply encumbrance adjustment to an RSF factor. -/
def applyEncumbranceFactor
    (base : Rate)
    (enc : EncumbrancePeriod)
    (exceptionalCbApraFactor : Option Rate := none) : Rate :=
  match exceptionalCbApraFactor with
  | some r => nonNeg r
  | none =>
    match enc with
    | .unencumbered => base
    | .lessThanSixMonths => base
    | .sixMonthsToLessThanOneYear => max (pct 50) base
    | .oneYearOrMore => pct 100

/-- An on-balance-sheet RSF asset item (Attachment C, Paragraphs 17–37). -/
structure RsfAssetItem where
  id : String
  category : RsfCategory
  carryingValue : Money
  encumbrance : EncumbrancePeriod := .unencumbered
  /-- Para 24: APRA-approved lower RSF factor for exceptional central bank liquidity operations. -/
  exceptionalCbApraFactor : Option Rate := none
  /-- Paras 38–40: APRA-determined interdependent asset receives 0% RSF. -/
  isInterdependentApproved : Bool := false
  /-- Para 25 / fn 12: Exclude borrowed securities without beneficial ownership or
      variation margin assets already deducted from derivative liabilities. -/
  excludedFromBalanceSheetRsf : Bool := false
  /-- Para 32(a)–(b): Fallback RSF factor for CLF-eligible assets above the recognised
      CLF limit (defaults to 50% for third-party debt / 85% for self-securitised assets). -/
  excessClfFallbackFactor : Option Rate := none
  deriving DecidableEq, Repr

def RsfAssetItem.effectiveFactor (item : RsfAssetItem) : Rate :=
  let customerMarginExempt := match item.category with
    | .initialMarginAndCcpDefaultFund _ true => true
    | _ => false
  if item.isInterdependentApproved || item.excludedFromBalanceSheetRsf || customerMarginExempt then
    pct 0
  else
    applyEncumbranceFactor item.category.baseFactor item.encumbrance item.exceptionalCbApraFactor

def RsfAssetItem.weightedAmount (item : RsfAssetItem) : Money :=
  nonNeg item.carryingValue * item.effectiveFactor

/-- Attachment C, Paragraphs 33–37: Classify a loan's RSF category directly from
    APS 220 performing status, APS 112 residential mortgage criteria / risk weight,
    counterparty kind, and maturity bucket. -/
def classifyLoanRsf
    (cp : CounterpartyKind)
    (maturity : NsfrMaturityBucket)
    (daysPastDue : Nat)
    (unlikelyToPay : Bool)
    (resMortgage : Option External.StandardResidentialMortgageCriteria := none)
    (aps112RiskWeightPct : Nat := 100)
    (securedByHqla1Rehypothecatable : Bool := false) : RsfCategory :=
  if !External.isPerformingLoanPerAps220 daysPastDue unlikelyToPay then
    .allOtherAssets
  else match cp with
    | .adiOrBank _ | .otherFinancialInstitution | .hedgeFundOrMmfOrSpv =>
      match maturity with
      | .lessThanSixMonths =>
        if securedByHqla1Rehypothecatable then .loanToFiLt6mSecuredByHqla1Rehypothecatable
        else .otherLoanToFiLt6m
      | .sixMonthsToLessThanOneYear => .loansToFiAndCentralBanks6mToLt1Yr
      | .oneYearOrMore => .allOtherAssets
    | .centralBank | .rba =>
      match maturity with
      | .lessThanSixMonths => .centralBankClaimsLt6m
      | .sixMonthsToLessThanOneYear => .loansToFiAndCentralBanks6mToLt1Yr
      | .oneYearOrMore =>
        if aps112RiskWeightPct <= 35 then .otherLoansNonFiRwLe35Ge1Yr
        else .otherPerformingLoansNonFiGe1Yr
    | _ =>
      match maturity with
      | .lessThanSixMonths | .sixMonthsToLessThanOneYear => .otherNonHqlaLt1Yr
      | .oneYearOrMore =>
        let passes35a := match resMortgage with
          | some m => m.qualifiesFor65PctRsf
          | none => false
        if passes35a then .standardResidentialPropertyLoansLvrLe80Ge1Yr
        else if aps112RiskWeightPct <= 35 then .otherLoansNonFiRwLe35Ge1Yr
        else .otherPerformingLoansNonFiGe1Yr

/-- Attachment C, Paragraphs 41–42 & Table 1 – OBS exposure categories. -/
inductive ObsExposureCategory where
  /-- Table 1: Irrevocable and conditionally revocable credit and liquidity facilities (5%). -/
  | irrevocableOrConditionallyRevocableFacility
  /-- Table 1: Unconditionally revocable credit and liquidity facilities (1%). -/
  | unconditionallyRevocableFacility
  /-- Para 42: Trade finance-related obligations and non-trade guarantees/LCs
      (100% of actual net outflows in the most recent 12-month period). -/
  | tradeFinanceAndNonTradeGuaranteesLcs
  deriving DecidableEq, Repr

def ObsExposureCategory.factor : ObsExposureCategory → Rate
  | .irrevocableOrConditionallyRevocableFacility => pct 5
  | .unconditionallyRevocableFacility => pct 1
  | .tradeFinanceAndNonTradeGuaranteesLcs => pct 100

/-- An off-balance-sheet exposure item for NSFR RSF (Attachment C, Paras 41–42). -/
structure ObsExposureItem where
  id : String
  category : ObsExposureCategory
  /-- Undrawn facility amount (for Table 1 rows) or actual 12-month net outflows (for para 42). -/
  amount : Money
  deriving DecidableEq, Repr

def ObsExposureItem.weightedAmount (item : ObsExposureItem) : Money :=
  nonNeg item.amount * item.category.factor

/-- Attachment C, Paragraphs 9 & 27: Individual derivative contract or netting-set leg
    for computing gross derivative assets and liabilities under eligible bilateral netting. -/
structure DerivativeContractMtM where
  nettingSetId : String
  mtm : Rat
  bilateralNetting : Option External.BilateralNettingAgreement := none
  deriving DecidableEq, Repr

private def insertMtMGroup (key : String) (x : DerivativeContractMtM) :
    List (String × List DerivativeContractMtM) → List (String × List DerivativeContractMtM)
  | [] => [(key, [x])]
  | (k, xs) :: rest =>
    if k == key then (k, xs ++ [x]) :: rest
    else (k, xs) :: insertMtMGroup key x rest

/-- Attachment C, Paragraphs 9 & 27: Aggregate individual derivative contract MtMs into
    `(grossDerivativeAssets, grossDerivativeLiabilities)`.
    Contracts in a netting set whose `BilateralNettingAgreement.isEligible = true`
    (all legs) are netted to a single net replacement cost; otherwise positive and negative
    replacement costs are summed gross without cross-contract netting. -/
def computeNettedDerivativeTotals (contracts : List DerivativeContractMtM) : Money × Money :=
  let groups := contracts.foldl (fun acc c => insertMtMGroup c.nettingSetId c acc) []
  groups.foldl (fun (accAssets, accLiabs) (_, grp) =>
    let allEligible := grp.all (fun c =>
      match c.bilateralNetting with
      | some b => b.isEligible
      | none => false)
    if allEligible then
      let netRc := sumBy grp (·.mtm)
      if netRc >= 0 then (accAssets + netRc, accLiabs)
      else (accAssets, accLiabs + (-netRc))
    else
      let posSum := sumBy grp (fun c => nonNeg c.mtm)
      let negSum := sumBy grp (fun c => nonNeg (-c.mtm))
      (accAssets + posSum, accLiabs + negSum)
  ) (0, 0)

/-- Attachment C, Paragraph 26 & APS 110 Attachment D Paragraph 38:
    Net SFT cash receivables and payables with a single counterparty only when
    `SftNettingCriteria.isEligible` holds; otherwise treat receivable gross. -/
structure NsfrSftNettingGroup where
  receivable : Money
  payable : Money
  nettingCriteria : External.SftNettingCriteria
  deriving DecidableEq, Repr

def NsfrSftNettingGroup.netNsfrReceivable (g : NsfrSftNettingGroup) : Money :=
  if g.nettingCriteria.isEligible then
    nonNeg (nonNeg g.receivable - nonNeg g.payable)
  else
    nonNeg g.receivable

/-- Attachment C, Paragraphs 9–10, 15(c), 27–28, 37(b), 37(d): Derivative inputs for NSFR. -/
structure NsfrDerivativeInput where
  /-- Para 9: Sum of negative replacement costs (after eligible bilateral netting). -/
  grossDerivativeLiabilities : Money := 0
  /-- Para 10: Total collateral posted as variation margin on derivative liabilities. -/
  variationMarginPosted : Money := 0
  /-- Para 27: Sum of positive replacement costs (after eligible bilateral netting). -/
  grossDerivativeAssets : Money := 0
  /-- Para 28: Cash variation margin received meeting APS 110 Att D para 23 in same settlement currency. -/
  eligibleCashVariationMarginReceived : Money := 0
  /-- Optional contract-level MtM legs for Paras 9 & 27 bilateral netting calculation. -/
  contracts : List DerivativeContractMtM := []
  /-- Optional APS 110 Att D para 23 cash variation margin eligibility check. -/
  cashVmReceivedEligibility : Option External.CashVariationMarginEligibility := none
  deriving DecidableEq, Repr

private def NsfrDerivativeInput.effectiveGrossAssets (d : NsfrDerivativeInput) : Money :=
  if d.contracts.isEmpty then nonNeg d.grossDerivativeAssets
  else (computeNettedDerivativeTotals d.contracts).1

private def NsfrDerivativeInput.effectiveGrossLiabilities (d : NsfrDerivativeInput) : Money :=
  if d.contracts.isEmpty then nonNeg d.grossDerivativeLiabilities
  else (computeNettedDerivativeTotals d.contracts).2

def NsfrDerivativeInput.nsfrDerivativeLiabilities (d : NsfrDerivativeInput) : Money :=
  nonNeg (d.effectiveGrossLiabilities - nonNeg d.variationMarginPosted)

def NsfrDerivativeInput.nsfrDerivativeAssets (d : NsfrDerivativeInput) : Money :=
  let vmOk := match d.cashVmReceivedEligibility with
    | some vm => vm.isEligibleForNsfrOffset
    | none => true
  let vmAmt := if vmOk then nonNeg d.eligibleCashVariationMarginReceived else 0
  nonNeg (d.effectiveGrossAssets - vmAmt)

/-- Para 37(b) & fn 29: `100% * MAX(NSFR derivative assets - NSFR derivative liabilities, 0)`.
    Bilateral netting eligibility (Paras 9 & 27) governs contract-level replacement cost netting
    in `contracts`, never disabling the aggregate Para 37(b) offset. -/
def NsfrDerivativeInput.netDerivativeRsf (d : NsfrDerivativeInput) : Money :=
  nonNeg (d.nsfrDerivativeAssets - d.nsfrDerivativeLiabilities)

/-- Para 37(d) & fn 20: `20% * gross derivative liabilities` (before variation margin deduction). -/
def NsfrDerivativeInput.derivativeLiabilityAddOnRsf (d : NsfrDerivativeInput) : Money :=
  pct 20 * d.effectiveGrossLiabilities

/-- Full inputs for Attachment C NSFR calculation. -/
structure NsfrInput where
  asfItems : List AsfItem
  rsfAssets : List RsfAssetItem
  obsExposures : List ObsExposureItem
  derivatives : NsfrDerivativeInput := {}
  /-- Attachment C, Paragraph 32(a)–(b): Optional recognised LCR CLF limit
      to cap 10% RSF treatment on CLF-eligible third-party debt and self-securitised assets.
      Standalone `evaluateNsfr` treats `none` as `0`; `evaluateAdiCompliance` populates it
      from the corresponding AUD LCR `recognisedClf`. -/
  recognisedClfLimit : Option Money := none
  /-- Attachment C, Paragraph 32(a): Host-recognised ALA / other jurisdictions' ALA allowed by APRA
      in the LCR numerator (receives 10% RSF under Para 32(a) independently of AUD CLF). -/
  hostAlaInLcrLimit : Money := 0
  /-- Attachment C, Paragraph 4: Internal ASF buffer above the minimum NSFR requirement. -/
  internalNsfrBuffer : Money := 0
  deriving DecidableEq, Repr

/-- Complete NSFR evaluation result. -/
structure NsfrResult where
  totalAsf : Money
  totalRsf : Money
  netDerivativeRsf : Money
  derivativeLiabilityAddOnRsf : Money
  obsRsf : Money
  ratioCheck : RatioCheck
  bufferMet : Bool
  deriving DecidableEq, Repr

private def insertByFallbackDesc
    (defaultFallback : Rate) (x : RsfAssetItem) : List RsfAssetItem → List RsfAssetItem
  | [] => [x]
  | y :: ys =>
    let fx := nonNeg (x.excessClfFallbackFactor.getD defaultFallback)
    let fy := nonNeg (y.excessClfFallbackFactor.getD defaultFallback)
    if fx >= fy then x :: y :: ys else y :: insertByFallbackDesc defaultFallback x ys

private def sortByFallbackDesc (defaultFallback : Rate) (items : List RsfAssetItem) : List RsfAssetItem :=
  items.foldr (insertByFallbackDesc defaultFallback) []

private def foldClfBucketRsf
    (items : List RsfAssetItem)
    (initLimit : Money)
    (defaultFallback : Rate) : Money × Money :=
  let sorted := sortByFallbackDesc defaultFallback items
  sorted.foldl (fun (accRsf, remLim) a =>
    let v := nonNeg a.carryingValue
    let within := min v remLim
    let excess := nonNeg (v - within)
    let fb := nonNeg (a.excessClfFallbackFactor.getD defaultFallback)
    (accRsf + pct 10 * within + fb * excess, nonNeg (remLim - within))
  ) (0, initLimit)

/-- Compute NSFR = ASF / RSF and check compliance against `requiredMinRate` (default 100%, para 60).
    Always enforces Attachment C Paragraph 32(a)–(b) 10% RSF caps (defaulting `none` to 0);
    allocates the limit greedily to highest-fallback-RSF items first (order-independent) and
    defaults excess fallback to 85% for both 32(a) third-party debt / host ALA (para 36(c)
    conservative fallback for >= 1y non-HQLA securities) and 32(b) self-securitised assets
    (para 36(c)), both overridable via `excessClfFallbackFactor` (e.g. `some (pct 50)` for
    para 34(f) qualifying < 1y / corporate securities). -/
def evaluateNsfr
    (requiredMinRate : Rate := pct 100)
    (input : NsfrInput) : NsfrResult :=
  let totalAsf := sumBy input.asfItems AsfItem.weightedAmount
  let clfLim := nonNeg (input.recognisedClfLimit.getD 0)
  let hostAlaLim := nonNeg input.hostAlaInLcrLimit
  let isEligible32a (a : RsfAssetItem) : Bool :=
    a.category == .clfThirdPartyDebtOrHostAla && a.effectiveFactor == pct 10
  let isEligible32b (a : RsfAssetItem) : Bool :=
    a.category == .selfSecuritisedClfEligible && a.effectiveFactor == pct 10
  let nonClfRsf := sumBy
    (input.rsfAssets.filter (fun a => !isEligible32a a && !isEligible32b a))
    RsfAssetItem.weightedAmount
  let (rsf32a, rem32aLim) := foldClfBucketRsf (input.rsfAssets.filter isEligible32a) (clfLim + hostAlaLim) (pct 85)
  let remClfLim := min clfLim rem32aLim
  let (rsf32b, _) := foldClfBucketRsf (input.rsfAssets.filter isEligible32b) remClfLim (pct 85)
  let assetRsf := nonClfRsf + rsf32a + rsf32b
  let obsRsf := sumBy input.obsExposures ObsExposureItem.weightedAmount
  let netDerivRsf := input.derivatives.netDerivativeRsf
  let derivAddOn := input.derivatives.derivativeLiabilityAddOnRsf
  let totalRsf := assetRsf + obsRsf + netDerivRsf + derivAddOn
  let rc := checkRatio totalAsf totalRsf requiredMinRate
  let bufferMet := totalAsf >= requiredMinRate * totalRsf + nonNeg input.internalNsfrBuffer
  { totalAsf
    totalRsf
    netDerivativeRsf := netDerivRsf
    derivativeLiabilityAddOnRsf := derivAddOn
    obsRsf
    ratioCheck := rc
    bufferMet }

end APS210.NSFR
