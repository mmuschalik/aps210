import APS210.Basic
import APS210.ExternalStandards
import APS210.Governance
import APS210.LCR
import APS210.MLH
import APS210.NSFR
import APS210.Compliance
import APS210.CashFlows

namespace APS210.Tests

open APS210.Facts
open APS210.LCR
open APS210.MLH
open APS210.NSFR
open APS210.CashFlows

/-- Observation stamp shared by test records (reporting day 0, data cut 0). -/
def obs : ObservationContext := { validAt := 0, knownAt := 0, sourceReference := "test" }

/-- Retail scorecard with every criterion known to be absent. -/
def noScore : LessStableScorecardInput :=
  { isOnlineAccount := .known false, noEstablishedRelationship := .known false, isHeavilyRateDriven := .known false }

/-- Outflow of a deposit held by a customer with no other deposits. -/
def _root_.APS210.LCR.RetailDeposit.solo (d : RetailDeposit) (basis : ConsolidationBasis := .level1) : Money :=
  d.outflowAmount basis [d]

/-- A derivative contract MtM in its own netting set, with no bilateral netting agreement. -/
def mtm (nettingSetId : String) (v : Rat) : DerivativeContractMtM :=
  { nettingSetId, mtm := v, observation := obs }

/-- A contractual cash flow observed on day 0, settling `days` later, with a known option list,
    known performing, and standard source. -/
def cf (id : String) (side : Side) (amount : Rat) (days : Int) (opts : List ContractOption := []) :
    ContractualCashFlow :=
  { obligation := { id, currency := .aud, side, amount := Amount.ofRat amount, settlementDate := .known days,
                    optionality := .known opts, performing := .known true, observation := obs }
    isOpenMaturity := false, isMinimumContractualPayment := false, isNonFinancialRevenue := false,
    countedInHqla := false, source := .standard }

/-- A derivative collateral movement settling on day 5. -/
def coll (obligationId nettingSetId : String) (side : Side) (amount : Rat)
    (eligibleForOffset alreadyCountedInHqla : Reading Bool) : CollateralMovement :=
  { obligationId, nettingSetId, side, amount := Amount.ofRat amount, currency := .aud,
    settlementDate := .known 5, eligibleForOffset, alreadyCountedInHqla, observation := obs }

/-- HQLA / CLF collateral meeting every operational requirement. -/
def opOk : HqlaOperationalCompliance :=
  {
    convertibleToCashAnytime := true, unencumberedAndControlled := true, controlEvidenced := true, notPledgedOrOperationalCost := true, notRehypothecatedIfReceivedInSft := true }

-- ============================================================================
-- 1. Attachment A, Tables 1 & 2: Scorecard & Retail Deposit Outflows
-- ============================================================================
example :
    ({ isOnlineAccount := .known true
       noEstablishedRelationship := .known false
       isHeavilyRateDriven := .known false } : LessStableScorecardInput).score true = 4 := by decide

-- A criterion that is not known scores as present.
example :
    ({ isOnlineAccount := .unknown "not captured"
       noEstablishedRelationship := .known false
       isHeavilyRateDriven := .refused "source withheld" } : LessStableScorecardInput).score false = 3 := by decide

example : lessStableRunoffRate 0 = pct 10 := by native_decide
example : lessStableRunoffRate 2 = pct 10 := by native_decide
example : lessStableRunoffRate 3 = pct 25 := by native_decide
example : lessStableRunoffRate 6 = pct 25 := by native_decide

-- Retail deposit scenarios: stable (5%), less stable <=2 (10%), >=3 (25%),
-- excluded term deposit (0%), tainted term deposit (10%), intermediated fail (100%),
-- Level 2 host override (15%) vs Level 1 ignoring host override.
example :
    ({ customerId := "c48_5", guaranteeLimit := 250000, scorecard := noScore, observation := obs, id := "r1", currency := .aud, balance := 100, isStable := true } : RetailDeposit).solo = 5 := by native_decide
example :
    ({
       customerId := "c50_5", guaranteeLimit := 50, observation := obs, id := "r2", currency := .aud, balance := 100, isStable := false, scorecard := {
                      isOnlineAccount := .known false, noEstablishedRelationship := .known false, isHeavilyRateDriven := .known false } } : RetailDeposit).solo = 10 := by native_decide
example :
    ({
       customerId := "c54_5", guaranteeLimit := 50, observation := obs, id := "r3", currency := .aud, balance := 100, isStable := false, scorecard := {
                      isOnlineAccount := .known false, noEstablishedRelationship := .known true, isHeavilyRateDriven := .known false } } : RetailDeposit).solo = 25 := by native_decide
example :
    ({
       customerId := "c58_5", guaranteeLimit := 250000, scorecard := noScore, observation := obs, id := "r4", currency := .aud, balance := 100, isStable := false, residualMaturityOrNoticeGt30Days := true,
       noWithdrawalRightOrMaterialPenalty := true } : RetailDeposit).solo = 0 := by native_decide
example :
    ({
       customerId := "c62_5", guaranteeLimit := 250000, scorecard := noScore, observation := obs, id := "r5", currency := .aud, balance := 100, isStable := false, residualMaturityOrNoticeGt30Days := true,
       noWithdrawalRightOrMaterialPenalty := true,
       categoryTaintedByEarlyWithdrawal := true } : RetailDeposit).solo = 10 := by native_decide
example :
    ({
       customerId := "c67_5", guaranteeLimit := 250000, scorecard := noScore, observation := obs, id := "r6", currency := .aud, balance := 100, isStable := true, isIntermediated := true, intermediatedLookThroughMet := false } : RetailDeposit).solo = 100 := by native_decide
example :
    let d : RetailDeposit := { customerId := "c70_29", guaranteeLimit := 250000, scorecard := noScore, observation := obs, id := "r7", currency := .foreign "NZD", balance := 100, isStable := true,
                               hostJurisdictionAppliesBasel := true,
                               hostSupervisorOverrideRate := some (pct 15) }
    d.solo .level2 = 15 ∧ d.solo .level1 = 5 := by native_decide
example :
    let d : RetailDeposit := { customerId := "c75_29", guaranteeLimit := 250000, scorecard := noScore, observation := obs, id := "r8", currency := .foreign "NZD", balance := 100, isStable := true,
                               hostJurisdictionAppliesBasel := false,
                               hostSupervisorOverrideRate := some (pct 15) }
    d.solo .level2 = 5 := by native_decide

-- ============================================================================
-- 2. Attachment A, HQLA caps (40% and 15%), SFT unwinds, Para 9(e), Para 25, CLF
-- ============================================================================
example : (computeHqlaCaps 100 60 30).stockExClf = (500 : Rat) / 3 := by native_decide
example : (computeHqlaCaps 100 0 30).stockExClf = (2000 : Rat) / 17 := by native_decide

-- Raw SFT unwind legs & Para 9(e) aggregate foreign sovereign cap & Para 25 grace period
example :
    let opOk : HqlaOperationalCompliance :=
      { convertibleToCashAnytime := true
        unencumberedAndControlled := true
        controlEvidenced := true
        notPledgedOrOperationalCost := true
        notRehypothecatedIfReceivedInSft := true }
    let opPledged : HqlaOperationalCompliance := { opOk with notPledgedOrOperationalCost := false }
    let assets : List HqlaAsset :=
      [ {
          freelyTransferableAtLevel2 := false, level2TransferGroup := "g", daysSinceIneligible := .known none, max30dPriceDecline := .known 0, max30dHaircutIncrease := .known 0, observation := obs, id := "cash", category := .hqla1, currency := .aud, marketValue := 100, operational := opOk, issuerRestrictionSatisfied := true, recognisedByRegulator := true }
      , { freelyTransferableAtLevel2 := false, level2TransferGroup := "g", daysSinceIneligible := .known none, max30dPriceDecline := .known 0, max30dHaircutIncrease := .known 0, observation := obs, id := "f-sov-1", category := .hqla1, currency := .foreign "XYZ", marketValue := 30,
          operational := opOk, issuerRestrictionSatisfied := true, recognisedByRegulator := true,
          isNonZeroRwForeignSovInForeignCurrency := true }
      , { freelyTransferableAtLevel2 := false, level2TransferGroup := "g", daysSinceIneligible := .known none, max30dPriceDecline := .known 0, max30dHaircutIncrease := .known 0, observation := obs, id := "f-sov-2", category := .hqla1, currency := .foreign "XYZ", marketValue := 20,
          operational := opOk, issuerRestrictionSatisfied := true, recognisedByRegulator := true,
          isNonZeroRwForeignSovInForeignCurrency := true }
      , {
          freelyTransferableAtLevel2 := false, level2TransferGroup := "g", daysSinceIneligible := .known (some 10), max30dPriceDecline := .known 0, max30dHaircutIncrease := .known 0, observation := obs, id := "downgraded-ok", category := .hqla2a, currency := .aud, marketValue := 20, operational := opOk, issuerRestrictionSatisfied := true, recognisedByRegulator := false,
          hqla2aDiversified := true }
      , {
          freelyTransferableAtLevel2 := false, level2TransferGroup := "g", daysSinceIneligible := .known none, max30dPriceDecline := .known 0, max30dHaircutIncrease := .known 0, observation := obs, id := "pledged-bad", category := .hqla1, currency := .aud, marketValue := 50, operational := opPledged, issuerRestrictionSatisfied := true, recognisedByRegulator := true } ]
    let unwind := computeUnwindAdjustments [{ meetsOperationalReqs := true, hqla1Delta := 10, hqla2aDelta := -5 }]
    let (stock, _) := computeStockOfHqla .level1 assets unwind none [(.foreign "XYZ", 40)]
    -- HQLA1 = 100 + min(30+20, 40) = 140; HQLA2A = 20 * 0.85 = 17; pledged-bad = 0; stock = 157
    stock = 157 := by native_decide

-- CLF 3-way minimum
example :
    ({ approvedByApra := true
       contractualAmount := 50
       pledgedUnderZeroRunoffSecFunding := 10
       receivedUnderZeroInflowSecLending := 5
       collateralHeldAfterRbaMargins := 48
       collateralDiversified := true
       annualReapprovalCurrent := true
       includedInFundingStrategy := true
       balanceSheetEffortsExhausted := true
       drawdownAttestedAndNotifiedToApra := true
       collateralOperational := .known opOk
       recentlyIneligibleCollateralDays := .unknown "no recently ineligible collateral"
       observation := obs } : ClfInput).recognisedAmount = 45 := by native_decide

-- CLF collateral whose operational status is not known fails para 16; CLF is not recognised.
example :
    ({
       approvedByApra := true, contractualAmount := 50, pledgedUnderZeroRunoffSecFunding := 0, receivedUnderZeroInflowSecLending := 0, collateralHeldAfterRbaMargins := 48,
       collateralDiversified := true, annualReapprovalCurrent := true, includedInFundingStrategy := true,
       balanceSheetEffortsExhausted := true,
       collateralOperational := .unknown "custody report missing",
       recentlyIneligibleCollateralDays := .unknown "n/a", observation := obs } : ClfInput).recognisedAmount = 0 := by
  native_decide

-- ============================================================================
-- 3. Attachment A, Table 3: Exhaustive verification of ALL 40 outflow categories
-- ============================================================================
example : Table3OutflowCategory.retailStable.rate = pct 5 := by native_decide
example : Table3OutflowCategory.retailLessStable.rate = pct 10 := by native_decide
example : Table3OutflowCategory.retailHigherRunoffLessStable.rate = pct 25 := by native_decide
example : Table3OutflowCategory.operationalDepositInsured.rate = pct 5 := by native_decide
example : Table3OutflowCategory.operationalDepositUninsured.rate = pct 25 := by native_decide
example : Table3OutflowCategory.nonOperationalCorpSovCbPseMdbSmeGt2mInsured.rate = pct 20 := by native_decide
example : Table3OutflowCategory.nonOperationalCorpSovCbPseMdbSmeGt2mUninsured.rate = pct 40 := by native_decide
example : Table3OutflowCategory.nonOperationalOther.rate = pct 100 := by native_decide
example : Table3OutflowCategory.unsecuredDebtIssuance.rate = pct 100 := by native_decide
example : Table3OutflowCategory.securedByHqla1.rate = pct 0 := by native_decide
example : Table3OutflowCategory.securedByClfEligibleWithinLimit.rate = pct 0 := by native_decide
example : Table3OutflowCategory.securedByHqla2a.rate = pct 15 := by native_decide
example : Table3OutflowCategory.securedDomesticSovPseMdbRwLe20.rate = pct 25 := by native_decide
example : Table3OutflowCategory.securedByHqla2bRmbs.rate = pct 25 := by native_decide
example : Table3OutflowCategory.securedByOtherHqla2b.rate = pct 50 := by native_decide
example : Table3OutflowCategory.securedAllOther.rate = pct 100 := by native_decide
example : Table3OutflowCategory.structuredMaturingIssuedByAdi.rate = pct 100 := by native_decide
example : Table3OutflowCategory.structuredSpvConduitSivLossOfFunding.rate = pct 100 := by native_decide
example : Table3OutflowCategory.derivativesCashOutflows.rate = pct 100 := by native_decide
example : Table3OutflowCategory.downgradeThreeNotchCollateral.rate = pct 100 := by native_decide
example : Table3OutflowCategory.excessNonSegregatedCollateralCallable.rate = pct 100 := by native_decide
example : Table3OutflowCategory.contractuallyRequiredCollateralNotYetDemanded.rate = pct 100 := by native_decide
example : Table3OutflowCategory.collateralSubstitutionToNonHqla.rate = pct 100 := by native_decide
example : (Table3OutflowCategory.derivativeValuationLargest30dFlow24m none).rate = pct 100 := by native_decide
example : Table3OutflowCategory.postedNonHqla1CollateralValuationChange.rate = pct 20 := by native_decide
example : Table3OutflowCategory.committedFacilityRetailAndSme.rate = pct 5 := by native_decide
example : Table3OutflowCategory.committedCreditNonFinCorpSovCbPse.rate = pct 10 := by native_decide
example : Table3OutflowCategory.committedLiquidityNonFinCorpSovCbPse.rate = pct 30 := by native_decide
example : Table3OutflowCategory.committedFacilityBanksAndSupervisedAdis.rate = pct 40 := by native_decide
example : Table3OutflowCategory.committedCreditOtherFinancialInstitutions.rate = pct 40 := by native_decide
example : Table3OutflowCategory.committedLiquidityOtherFinancialInstitutions.rate = pct 100 := by native_decide
example : Table3OutflowCategory.committedFacilityOtherLegalEntities.rate = pct 100 := by native_decide
example : Table3OutflowCategory.revocableCreditAndLiquidityFacilities.rate = pct 5 := by native_decide
example : Table3OutflowCategory.tradeFinanceObligations.rate = pct 100 := by native_decide
example : Table3OutflowCategory.guaranteesAndLcsNonTradeFinance.rate = pct 50 := by native_decide
example : (Table3OutflowCategory.buybacksDomesticDebtShortTerm none).rate = pct 10 := by native_decide
example : (Table3OutflowCategory.buybacksDomesticDebtLongTerm none).rate = pct 5 := by native_decide
example : Table3OutflowCategory.nonContractualStructuredProductsAndManagedFunds.rate = pct 5 := by native_decide
example : (Table3OutflowCategory.affiliatedDealerOrMarketMaker (pct 12)).rate = pct 12 := by native_decide
example : (Table3OutflowCategory.otherNonContractualContingent (pct 8)).rate = pct 8 := by native_decide
example : Table3OutflowCategory.otherContractualCashOutflows.rate = pct 100 := by native_decide

-- ============================================================================
-- 4. Attachment A, Table 4: Exhaustive verification of ALL 14 (7x2) cells
-- ============================================================================
example : securedLendingInflowRate .hqla1 false = pct 0 := by native_decide
example : securedLendingInflowRate .hqla1 true = pct 0 := by native_decide
example : securedLendingInflowRate .clfEligibleWithinLimit false = pct 0 := by native_decide
example : securedLendingInflowRate .clfEligibleWithinLimit true = pct 0 := by native_decide
example : securedLendingInflowRate .hqla2a false = pct 15 := by native_decide
example : securedLendingInflowRate .hqla2a true = pct 0 := by native_decide
example : securedLendingInflowRate .hqla2bRmbs false = pct 25 := by native_decide
example : securedLendingInflowRate .hqla2bRmbs true = pct 0 := by native_decide
example : securedLendingInflowRate .otherHqla2b false = pct 50 := by native_decide
example : securedLendingInflowRate .otherHqla2b true = pct 0 := by native_decide
example : securedLendingInflowRate .marginLendingNonHqlaNonClf false = pct 50 := by native_decide
example : securedLendingInflowRate .marginLendingNonHqlaNonClf true = pct 0 := by native_decide
example : securedLendingInflowRate .allOtherCollateral false = pct 100 := by native_decide
example : securedLendingInflowRate .allOtherCollateral true = pct 0 := by native_decide

-- ============================================================================
-- 5. Attachment A, Foreign ADI LCR (40% min, 50% HO facility, AUD filtering)
-- ============================================================================
example :
    let res := evaluateLcr .foreign false (pct 40)
      { asOf := obs, basis := .level1
        hqlaAssets := [{
          freelyTransferableAtLevel2 := false, level2TransferGroup := "g", daysSinceIneligible := .known none, max30dPriceDecline := .known 0, max30dHaircutIncrease := .known 0, observation := obs, id := "cash", category := .hqla1, currency := .aud, marketValue := 22,
          issuerRestrictionSatisfied := true, recognisedByRegulator := true,
          operational := {
            convertibleToCashAnytime := true, unencumberedAndControlled := true,
            controlEvidenced := true, notPledgedOrOperationalCost := true,
            notRehypothecatedIfReceivedInSft := true
          }
        }]
        retailDeposits := []
        otherOutflows := [{ observation := obs, id := "ws", currency := .aud, category := .nonOperationalOther, amount := 100 }]
        inflows := []
        headOfficeFacility := some {
          apraApproved := true, irrevocableAndDocumented := true,
          quantifiedAmount := 30, hasLocalBankingSubsidiaryInAustralia := false
        } }
    res.recognisedHeadOfficeFacility = 20 ∧ res.ratioCheck.compliant = true := by native_decide

-- AUD-only filtering test: USD asset and USD outflow are excluded when isAudOnly = true
example :
    let opOk : HqlaOperationalCompliance :=
      {
        convertibleToCashAnytime := true, unencumberedAndControlled := true, controlEvidenced := true, notPledgedOrOperationalCost := true,
        notRehypothecatedIfReceivedInSft := true }
    let res := evaluateLcr .locallyIncorporated true (pct 100)
      { asOf := obs, basis := .level1
        hqlaAssets :=
          [ {
              freelyTransferableAtLevel2 := false, level2TransferGroup := "g", daysSinceIneligible := .known none, max30dPriceDecline := .known 0, max30dHaircutIncrease := .known 0, observation := obs, id := "aud-cash", category := .hqla1, currency := .aud, marketValue := 50, operational := opOk, issuerRestrictionSatisfied := true, recognisedByRegulator := true }
          , { freelyTransferableAtLevel2 := false, level2TransferGroup := "g", daysSinceIneligible := .known none, max30dPriceDecline := .known 0, max30dHaircutIncrease := .known 0, observation := obs, id := "usd-cash", category := .hqla1, currency := .foreign "USD", marketValue := 100, operational := opOk,
              issuerRestrictionSatisfied := true, recognisedByRegulator := true } ]
        retailDeposits := []
        otherOutflows :=
          [ { observation := obs, id := "aud-out", currency := .aud, category := .nonOperationalOther, amount := 40 }
          , { observation := obs, id := "usd-out", currency := .foreign "USD", category := .nonOperationalOther, amount := 200 } ]
        inflows := [] }
    res.stockOfHqla = 50 ∧ res.netCashOutflows.totalNetCashOutflows = 40 ∧ res.ratioCheck.compliant = true := by native_decide

-- ============================================================================
-- 6. Attachment B: MLH evaluation with para 2(f) netting & para 3 RBA repo check
-- ============================================================================
example :
    let res := evaluateMlh (pct 9)
      { asOf := obs, assets := [
          {
            observation := obs, id := "cgs", category := .commonwealthAndSemiGovtSecurities, marketValue := 8, unencumberedOrApraApproved := true, rbaRepoEligible := true, nonSubordinated := true,
            liquidatableWithinTwoBusinessDays := true },
          {
            observation := obs, id := "adi-dep", category := .netDepositsWithOtherAdisTwoBusinessDays, marketValue := 5, unencumberedOrApraApproved := true, liquidatableWithinTwoBusinessDays := true },
          {
            observation := obs, id := "non-rba", category := .adiBankBillsCdsAndDebt, marketValue := 10, unencumberedOrApraApproved := true, rbaRepoEligible := false, nonSubordinated := true,
            liquidatableWithinTwoBusinessDays := true }
        ]
        placementsByOtherAdis := 3
        liabilities := [{ observation := obs, id := "liab", amount := 100 }]
        triggerRatio := pct 10 }
    res.eligibleLiquidAssets = 10 ∧ res.ratioCheck.compliant = true ∧ res.aboveTriggerRatio = true := by native_decide

-- ============================================================================
-- 7. Attachment C: NSFR, Encumbrance, CLF Cap, Customer Margin Exemption, OBS Table 1
-- ============================================================================
example : applyEncumbranceFactor (pct 5) .unencumbered = pct 5 := by native_decide
example : applyEncumbranceFactor (pct 5) .lessThanSixMonths = pct 5 := by native_decide
example : applyEncumbranceFactor (pct 5) .sixMonthsToLessThanOneYear = pct 50 := by native_decide
example : applyEncumbranceFactor (pct 85) .sixMonthsToLessThanOneYear = pct 85 := by native_decide
example : applyEncumbranceFactor (pct 5) .oneYearOrMore = pct 100 := by native_decide
example : applyEncumbranceFactor (pct 5) .oneYearOrMore (some (pct 15)) = pct 15 := by native_decide

example :
    let res := evaluateNsfr (pct 100)
      { asOf := obs, asfItems := [
          {
          observation := obs, id := "cap", category := .regulatoryCapitalExTier2Lt1Yr, carryingValue := 30 }, { observation := obs, id := "ret", category := .stableRetailAndSmeDepositsLt1Yr, carryingValue := 100 }
        ]
        rsfAssets := [
          {
          observation := obs, id := "res-mort", category := .standardResidentialPropertyLoansLvrLe80Ge1Yr, carryingValue := 100 }, {
          observation := obs, id := "hqla1", category := .unencumberedHqla1NonZeroPctRsf, carryingValue := 20 }, {
            observation := obs, id := "cust-im", category := .initialMarginAndCcpDefaultFund none true, carryingValue := 50, encumbrance := .oneYearOrMore },
          {
          observation := obs, id := "clf-3p", category := .clfEligibleThirdPartyDebt, carryingValue := 15 }, { observation := obs, id := "clf-self", category := .selfSecuritisedClfEligible, carryingValue := 10 }
        ]
        obsExposures := [
          {
          observation := obs, id := "irrev", category := .irrevocableOrConditionallyRevocableFacility, amount := 100 }, { observation := obs, id := "uncond", category := .unconditionallyRevocableFacility, amount := 100 }
        ]
        derivatives := {
          contracts := [mtm "set-liab" (-10), mtm "set-asset" 8]
          variationMarginPosted := 5
          variationMarginReceived := []
        }
        recognisedClfLimit := some 20 }
    -- ASF = 30 + 95 = 125
    -- RSF = 65 + 1 + 0 (cust-im) + 20*0.10 (CLF capped at 20) + 5*0.85 (CLF excess fallback) + 5 + 1 + 3 + 2 = 333/4
    res.totalAsf = 125 ∧ res.totalRsf = (333 : Rat) / 4 ∧ res.ratioCheck.compliant = true := by native_decide

-- ============================================================================
-- 8. End-to-End Compliance (Governance + Quantitative + Stress Breach Rule)
-- ============================================================================
example :
    let allGovOk : Governance.BoardAndManagementGovernance :=
      {
        hasBoardApprovedRiskTolerance := true, hasBoardApprovedStrategyAndPolicy := true, hasOperatingStandards := true, hasBoardApprovedFundingStrategy := true,
        hasContingencyFundingPlan := true, personnelHaveNecessaryExperience := true,
        frameworkDocumentedAndReviewedAnnually := true, boardReviewsRegularReports := true,
        seniorManagementDutiesMet := true, understandsRiskLinkages := true,
        level2GroupComplianceEnsured := true }
    let fwOk : Governance.LiquidityFramework :=
      {
        riskToleranceDocumentedAndAppropriate := true, riskToleranceReviewedAnnually := true, riskToleranceSupportsProlongedStress := true, riskToleranceArticulatesRiskProfitTradeoff := true,
        setsOutLevel1AndLevel2Structure := true, coversUnsecuredAndSecuredLossEvents := true,
        integratedIntoOverallRiskManagement := true, oversightFunctionOperationallyIndependent := true,
        coversCompositionMaturityDiversityIntradayCurrencies := true,
        includesNormalAndStressQuantQualTargets := true, communicatedThroughoutOrganisation := true,
        immediateEscalationOfEmergingConcerns := true, reviewedOngoingAndUnderCps220 := true,
        considersLegalStructureJurisdictionsAndHomeHost := true,
        businessUnitsUnderstandAndFollowPolicies := true }
    let rmOk : Governance.LiquidityRiskManagement :=
      {
        projectsCashFlowsAcrossHorizons := true, setsAndReviewsLimitsWithEscalation := true, managesCollateralEncumberedVsUnencumbered := true, hasEarlyWarningIndicators := true,
        hasReliableForwardLookingMis := true, managesIntradayLiquidity := true,
        allocatesFundingLiquidityCostsAndBenefits := true, activeInMultipleCurrencies := false,
        multiCurrencyNeedsMatchedAndAnalysed := false }
    let fsOk : Governance.FundingStrategy :=
      {
        hasThreeYearStrategyDocumented := true, maintainsMarketPresenceAndRelationships := true, regularlyGaugesFundRaisingCapacity := true, reviewedAndApprovedByBoardAnnually := true,
        updatedAnnually := true, materialChangesAdvisedToApra := true }
    let cfpOk : Governance.ContingencyFundingPlan :=
      {
        hasFormalPlanWithInvocationAndEscalation := true, articulatesSourcesAmountsLeadTimes := true, integratedWithStressTestsAndIntraday := true, hasRetailDeposits := true,
        addressesRetailRunWithoutClosingChannels := true, reviewedTestedAndBoardApprovedAnnually := true }
    let notifOk : Governance.NotificationStatus :=
      {
        hasLiquidityConcerns := false, notifiedApraOfConcerns := false, experiencingSevereStress := false, notifiedApraImmediatelyOfStress := false,
        intendingToRequestEla := false, notifiedApraPriorToApproachingRba := false,
        hasOperationalCapabilityForElaInfo := true }
    let mlhRes := evaluateAdiCompliance
      { asOf := obs, profile := { incorporation := .locallyIncorporated, classification := .mlh }
        boardAndManagement := allGovOk, framework := fwOk, riskManagement := rmOk,
        fundingStrategy := fsOk, contingencyFundingPlan := cfpOk, notifications := notifOk,
        hasLevel2Group := false,
        apraAdjustmentsComplied := true,
        bufferPolicy := some .enforceAllBuffers,
        notifiedApraOfRelianceOrPriorDiscretion := true,
        mlhInput := some {
          asOf := obs, assets := [{
                       observation := obs, id := "cgs", category := .commonwealthAndSemiGovtSecurities, marketValue := 12, unencumberedOrApraApproved := true, rbaRepoEligible := true, nonSubordinated := true,
                       liquidatableWithinTwoBusinessDays := true }],
          liabilities := [{
          observation := obs, id := "dep", amount := 100 }], triggerRatio := pct 10
        } }
    mlhRes.overallCompliant = true ∧ mlhRes.mlhAboveTrigger = some true := by native_decide

-- ============================================================================
-- 9. Exhaustive Attachment C (ASF / RSF / OBS) & Classifier Tests
-- ============================================================================
example : AsfCategory.regulatoryCapitalExTier2Lt1Yr.factor = pct 100 := by native_decide
example : AsfCategory.otherCapitalGe1YrNoEarlyOption.factor = pct 100 := by native_decide
example : AsfCategory.borrowingsAndLiabilitiesGe1Yr.factor = pct 100 := by native_decide
example : AsfCategory.stableRetailAndSmeDepositsLt1Yr.factor = pct 95 := by native_decide
example : AsfCategory.intermediatedStableRetail12mNotice.factor = pct 95 := by native_decide
example : AsfCategory.lessStableRetailAndSmeDepositsLt1Yr.factor = pct 90 := by native_decide
example : AsfCategory.intermediatedLessStableRetail12mNotice.factor = pct 90 := by native_decide
example : AsfCategory.nonFinCorporateFundingLt1Yr.factor = pct 50 := by native_decide
example : AsfCategory.operationalDeposits.factor = pct 50 := by native_decide
example : AsfCategory.superannuationOperationalDeposits.factor = pct 50 := by native_decide
example : AsfCategory.intermediatedRetail6mNotice.factor = pct 50 := by native_decide
example : AsfCategory.sovereignPseMdbNdbFundingLt1Yr.factor = pct 50 := by native_decide
example : AsfCategory.otherFunding6mToLt1Yr.factor = pct 50 := by native_decide
example : AsfCategory.otherFundingLt6m.factor = pct 0 := by native_decide
example : AsfCategory.noStatedMaturityShortOrOpen.factor = pct 0 := by native_decide
example : (AsfCategory.deferredTaxLiability .oneYearOrMore).factor = pct 100 := by native_decide
example : (AsfCategory.deferredTaxLiability .sixMonthsToLessThanOneYear).factor = pct 50 := by native_decide
example : (AsfCategory.deferredTaxLiability .lessThanSixMonths).factor = pct 0 := by native_decide
example : (AsfCategory.minorityInterest .oneYearOrMore).factor = pct 100 := by native_decide
example : (AsfCategory.cooperativeNetworkDeposit (some (pct 95))).factor = pct 85 := by native_decide
example : AsfCategory.allOtherLiabilitiesAndEquity.factor = pct 0 := by native_decide

example : RsfCategory.notesAndCoinsImmediatelyAvailable.baseFactor = pct 0 := by native_decide
example : (RsfCategory.centralBankReservesAndBalances none).baseFactor = pct 0 := by native_decide
example : RsfCategory.centralBankClaimsLt6m.baseFactor = pct 0 := by native_decide
example : RsfCategory.tradeDateReceivablesStandardSettlement.baseFactor = pct 0 := by native_decide
example : RsfCategory.unencumberedHqla1NonZeroPctRsf.baseFactor = pct 5 := by native_decide
example : RsfCategory.loanToFiLt6mSecuredByHqla1Rehypothecatable.baseFactor = pct 10 := by native_decide
example : RsfCategory.clfEligibleThirdPartyDebt.baseFactor = pct 10 := by native_decide
example : RsfCategory.hostJurisdictionAla.baseFactor = pct 10 := by native_decide
example : RsfCategory.selfSecuritisedClfEligible.baseFactor = pct 10 := by native_decide
example : RsfCategory.unencumberedHqla2a.baseFactor = pct 15 := by native_decide
example : RsfCategory.otherLoanToFiLt6m.baseFactor = pct 15 := by native_decide
example : RsfCategory.unencumberedHqla2b.baseFactor = pct 50 := by native_decide
example : RsfCategory.hqlaEncumbered6mToLt1Yr.baseFactor = pct 50 := by native_decide
example : RsfCategory.loansToFiAndCentralBanks6mToLt1Yr.baseFactor = pct 50 := by native_decide
example : RsfCategory.operationalDepositsAtOtherAdis.baseFactor = pct 50 := by native_decide
example : RsfCategory.asx200NonFiEquities.baseFactor = pct 50 := by native_decide
example : RsfCategory.otherNonHqlaLt1Yr.baseFactor = pct 50 := by native_decide
example : RsfCategory.standardResidentialPropertyLoansLvrLe80Ge1Yr.baseFactor = pct 65 := by native_decide
example : RsfCategory.otherLoansNonFiRwLe35Ge1Yr.baseFactor = pct 65 := by native_decide
example : (RsfCategory.initialMarginAndCcpDefaultFund none false).baseFactor = pct 85 := by native_decide
example : RsfCategory.otherPerformingLoansNonFiGe1Yr.baseFactor = pct 85 := by native_decide
example : RsfCategory.nonDefaultedNonHqlaSecuritiesGe1YrAndExchangeEquities.baseFactor = pct 85 := by native_decide
example : RsfCategory.physicalTradedCommoditiesInclGold.baseFactor = pct 85 := by native_decide
example : RsfCategory.allOtherAssets.baseFactor = pct 100 := by native_decide

example : ObsExposureCategory.irrevocableOrConditionallyRevocableFacility.factor = pct 5 := by native_decide
example : ObsExposureCategory.unconditionallyRevocableFacility.factor = pct 1 := by native_decide
example : ObsExposureCategory.tradeFinanceAndNonTradeGuaranteesLcs.factor = pct 100 := by native_decide

-- Classifiers & collateral substitution helper
example : classifyHqlaDebtSecurity true 0 .aaa false false true true = some .hqla1 := by decide
example : classifyHqlaDebtSecurity true 20 .aaMinus false false true true = some .hqla2a := by decide
example : classifyHqlaDebtSecurity false 50 .bbb false false true true = some .hqla2bCorporateDebt := by decide
example : classifyNonOperationalWholesaleDeposit (.sme 1500000 true) true true 0 = .retailStable := by decide
example : classifyNonOperationalWholesaleDeposit (.sme 1500000 false) false false 0 = .nonOperationalCorpSovCbPseMdbSmeGt2mUninsured := by decide
example : classifyCommittedFacility (.adiOrBank true) true = .committedFacilityBanksAndSupervisedAdis := by decide
example : classifyCommittedFacility .otherFinancialInstitution true = .committedLiquidityOtherFinancialInstitutions := by decide
example : (substitutableCollateralOutflow .aud 40 true obs).outflowAmount = 40 := by native_decide

-- ============================================================================
-- 10. External Standards Integration (APS 112, 110, 113, 120, 220)
-- ============================================================================
-- APS 112 Attachment F multiple assessments:
-- 2 ratings -> worse; 3 ratings -> second-best
example : External.resolveMultipleEcaiAssessments [.grade1, .grade2] = .grade2 := by native_decide
example : External.resolveMultipleEcaiAssessments [.grade1, .grade3, .grade2] = .grade2 := by native_decide

-- APS 112 Attachment B sovereign/PSE/MDB risk weights & HQLA derivation
example : External.sovereignRiskWeightPct .grade1 = 0 := by decide
example : External.sovereignRiskWeightPct .grade2 = 20 := by decide
example : External.domesticPseRiskWeightPct .grade1 = 20 := by decide
example : External.bankOrNonZeroMdbRiskWeightPct .grade2 false = 30 := by decide
example :
    classifySovereignPseMdbHqla (.sovereign true 0) [.grade1, .grade2, .grade1] true false true = some .hqla1 := by native_decide
example :
    classifySovereignPseMdbHqla (.pse 20) [.grade1] false false true = some .hqla2a := by native_decide

-- APS 220 performing status + APS 112 residential mortgage LVR <= 80% -> NSFR RSF derivation
example :
    classifyLoanRsf .naturalPerson .oneYearOrMore 10 false
      (some {
              toNaturalPerson := true, isStandardLoanPerAps112 := true, lvrPct := 75, residualMaturityOneYearOrMore := true }) =
    .standardResidentialPropertyLoansLvrLe80Ge1Yr := by native_decide
-- >= 90 days past due -> non-performing per APS 220 -> 100% RSF (.allOtherAssets)
example :
    classifyLoanRsf .naturalPerson .oneYearOrMore 90 false
      (some {
              toNaturalPerson := true, isStandardLoanPerAps112 := true, lvrPct := 75, residualMaturityOneYearOrMore := true }) =
    .allOtherAssets := by native_decide

-- APS 112 Att H, APS 110 Att D, APS 113 para 40, APS 120
example : External.qualifiesAsSmeUnderAps113Para40 1400000 = true := by native_decide
example : External.qualifiesAsSmeUnderAps113Para40 1500000 = false := by native_decide
example :
    ({
       writtenAndLegallyEnforceable := true, singleLegalObligationCloseOut := true, noWalkawayClause := true, reasonedLegalOpinionsInAllJurisdictions := true } : External.BilateralNettingAgreement).isEligible = true := by decide
example :
    ({
       fullyExtinguishesMtmExposure := true, isCash := true, notSegregatedIfNonQccp := true, calculatedAndExchangedDailyMtM := true, inContractMnaOrCcpCurrency := true, inSameSettlementCurrencyAsDerivative := true } : External.CashVariationMarginEligibility).isEligibleForNsfrOffset = true := by decide
example :
    ({
       sameCounterparty := true, sameExplicitFinalSettlementDate := true, notOpenEndedUnwindable := true,
       legallyEnforceableNettingNormalAndDefault := true, settledNetOrSimultaneously := true } : External.SftNettingCriteria).isEligible = true := by decide
example :
    ({
       sameCounterparty := true, sameExplicitFinalSettlementDate := true, notOpenEndedUnwindable := false,
       legallyEnforceableNettingNormalAndDefault := true, settledNetOrSimultaneously := true } : External.SftNettingCriteria).isEligible = false := by decide
example :
    ({
       significantCreditRiskTransferred := true, cleanBreakFromOriginator := true, noImplicitOrMoralRecourse := true, armsLengthTerms := true } : External.Aps120CapitalReliefCriteria).isMet = true := by decide


-- ============================================================================
-- 12. Compliance-Soundness Regression Tests (Issues 1–5)
-- ============================================================================

-- Issue 1: Level 1 alone cannot pass `overallCompliant` when `hasLevel2Group = true`.
example :
    let allGovOk : Governance.BoardAndManagementGovernance :=
      {
        hasBoardApprovedRiskTolerance := true, hasBoardApprovedStrategyAndPolicy := true, hasOperatingStandards := true, hasBoardApprovedFundingStrategy := true,
        hasContingencyFundingPlan := true, personnelHaveNecessaryExperience := true,
        frameworkDocumentedAndReviewedAnnually := true, boardReviewsRegularReports := true,
        seniorManagementDutiesMet := true, understandsRiskLinkages := true,
        level2GroupComplianceEnsured := true }
    let fwOk : Governance.LiquidityFramework :=
      {
        riskToleranceDocumentedAndAppropriate := true, riskToleranceReviewedAnnually := true, riskToleranceSupportsProlongedStress := true, riskToleranceArticulatesRiskProfitTradeoff := true,
        setsOutLevel1AndLevel2Structure := true, coversUnsecuredAndSecuredLossEvents := true,
        integratedIntoOverallRiskManagement := true, oversightFunctionOperationallyIndependent := true,
        coversCompositionMaturityDiversityIntradayCurrencies := true,
        includesNormalAndStressQuantQualTargets := true, communicatedThroughoutOrganisation := true,
        immediateEscalationOfEmergingConcerns := true, reviewedOngoingAndUnderCps220 := true,
        considersLegalStructureJurisdictionsAndHomeHost := true,
        businessUnitsUnderstandAndFollowPolicies := true }
    let rmOk : Governance.LiquidityRiskManagement :=
      {
        projectsCashFlowsAcrossHorizons := true, setsAndReviewsLimitsWithEscalation := true, managesCollateralEncumberedVsUnencumbered := true, hasEarlyWarningIndicators := true,
        hasReliableForwardLookingMis := true, managesIntradayLiquidity := true,
        allocatesFundingLiquidityCostsAndBenefits := true, activeInMultipleCurrencies := false,
        multiCurrencyNeedsMatchedAndAnalysed := false }
    let fsOk : Governance.FundingStrategy :=
      {
        hasThreeYearStrategyDocumented := true, maintainsMarketPresenceAndRelationships := true, regularlyGaugesFundRaisingCapacity := true, reviewedAndApprovedByBoardAnnually := true,
        updatedAnnually := true, materialChangesAdvisedToApra := true }
    let cfpOk : Governance.ContingencyFundingPlan :=
      {
        hasFormalPlanWithInvocationAndEscalation := true, articulatesSourcesAmountsLeadTimes := true, integratedWithStressTestsAndIntraday := true, hasRetailDeposits := true,
        addressesRetailRunWithoutClosingChannels := true, reviewedTestedAndBoardApprovedAnnually := true }
    let notifOk : Governance.NotificationStatus :=
      {
        hasLiquidityConcerns := false, notifiedApraOfConcerns := false, experiencingSevereStress := false, notifiedApraImmediatelyOfStress := false,
        intendingToRequestEla := false, notifiedApraPriorToApproachingRba := false,
        hasOperationalCapabilityForElaInfo := false }
    let mlhIn : MLH.MlhInput :=
      { asOf := obs, assets := [{ observation := obs, id := "cgs", category := .commonwealthAndSemiGovtSecurities, marketValue := 12 }]
        liabilities := [{ observation := obs, id := "dep", amount := 100 }]
        triggerRatio := pct 10 }
    let res := evaluateAdiCompliance
      { asOf := obs, profile := { incorporation := .locallyIncorporated, classification := .mlh }
        boardAndManagement := allGovOk, framework := fwOk, riskManagement := rmOk,
        fundingStrategy := fsOk, contingencyFundingPlan := cfpOk, notifications := notifOk,
        hasLevel2Group := true,
        mlhInput := some mlhIn,
        mlhLevel2Input := none }
    res.overallCompliant = false := by native_decide

-- Issue 2: Retail deposits split at FCS $250,000 limit (para 37 & APG 210)
--   $400k stable -> $250k * 5% ($12,500) + $150k * 10% ($15,000) = $27,500
--   $400k stable + online (+2 + +2 = 4 >= 3) -> $250k * 5% ($12,500) + $150k * 25% ($37,500) = $50,000
example :
    ({ customerId := "c524_5", guaranteeLimit := 250000, scorecard := noScore, observation := obs, id := "fcs1", currency := .aud, balance := 400000, isStable := true } : RetailDeposit).solo = 27500 := by native_decide

example :
    ({
       customerId := "c527_5", guaranteeLimit := 250000, observation := obs, id := "fcs2", currency := .aud, balance := 400000, isStable := true, scorecard := {
                      isOnlineAccount := .known true, noEstablishedRelationship := .known false, isHeavilyRateDriven := .known false } } : RetailDeposit).solo = 50000 := by native_decide

-- APG 210: the $250k guarantee applies per customer. Two $200k stable deposits held by one customer
-- have $250k insured in total ($125k each), so $150k is excess at the less-stable rate with +2:
--   $250k * 5% ($12,500) + $150k * 10% ($15,000) = $27,500 (not 2 * $10,000 = $20,000).
example :
    let d1 : RetailDeposit := { customerId := "same", guaranteeLimit := 250000, scorecard := noScore, observation := obs,
                                id := "a", currency := .aud, balance := 200000, isStable := true }
    let d2 : RetailDeposit := { d1 with id := "b" }
    computeTotalExpectedCashOutflows [d1, d2] [] = 27500 ∧
    computeTotalExpectedCashOutflows [d1, { d2 with customerId := "other" }] [] = 20000 := by native_decide

-- Issue 3: NSFR 10% CLF treatment defaults to 0 cap (1,000 -> 850 fallback RSF, not 100)
example :
    let res := NSFR.evaluateNsfr (pct 100)
      { asOf := obs, derivatives := NsfrDerivativeInput.empty, asfItems := [{ observation := obs, id := "cap", category := .regulatoryCapitalExTier2Lt1Yr, carryingValue := 1000 }]
        rsfAssets := [{ observation := obs, id := "self-sec", category := .selfSecuritisedClfEligible, carryingValue := 1000 }]
        obsExposures := [] }
    res.totalRsf = 850 := by native_decide

-- Issue 4: Para 9(e) currency cap is 0 (not skipped) when `foreignSovCurrencyCaps` is empty
example :
    let op : HqlaOperationalCompliance :=
      {
        convertibleToCashAnytime := true, unencumberedAndControlled := true, controlEvidenced := true, notPledgedOrOperationalCost := true,
        notRehypothecatedIfReceivedInSft := true }
    let (stock, _) := computeStockOfHqla .level1
      [{ freelyTransferableAtLevel2 := false, level2TransferGroup := "g", daysSinceIneligible := .known none, max30dPriceDecline := .known 0, max30dHaircutIncrease := .known 0, observation := obs, id := "p9e", category := .hqla1, currency := .foreign "XYZ", marketValue := 100,
         operational := op, isNonZeroRwForeignSovInForeignCurrency := true }]
      {} none []
    stock = 0 := by native_decide

-- Issue 5: Att A paras 50, 51, 52, 33 & APG called notice deposits
example :
    sumBy (classifyOperationalDeposit "op1" .aud .nonFinancialCorporate 100 50 false false 0 obs) OutflowItem.outflowAmount = 25 + 20 := by native_decide

example :
    sumBy (classifyLiquidityFacilityWithMaturingDebt "lf1" .aud .nonFinancialCorporate 100 30 obs) OutflowItem.outflowAmount = 9 + 7 := by native_decide

-- Forward-starting lending nets the rollover already assumed in inflows: max(100 - 30, 0) at 100%.
example :
    ({ id := "fwd", currency := .aud, category := .forwardStartingLending 30, amount := 100, observation := obs } : OutflowItem).outflowAmount = 70 ∧
    ({ id := "fwd2", currency := .aud, category := .forwardStartingLending 150, amount := 100, observation := obs } : OutflowItem).outflowAmount = 0 := by
  native_decide

example :
    classifyCommittedFacility .hedgeFundOrMmfOrSpv false = .committedLiquidityOtherFinancialInstitutions := by decide

example :
    Table3OutflowCategory.collateralSwapApraMethod.rate = pct 100 := by native_decide

example :
    ({
       customerId := "c565_5", guaranteeLimit := 250000, scorecard := noScore, observation := obs, id := "notice-called", currency := .aud, balance := 100, isStable := true, residualMaturityOrNoticeGt30Days := true,
       noWithdrawalRightOrMaterialPenalty := true,
       noticeDepositCalled := true } : RetailDeposit).solo = 100 := by native_decide

example :
    Table3OutflowCategory.wholesaleNoticeDepositCalled.rate = pct 100 := by native_decide

-- Issue 6: NSFR CLF-eligible assets above limit use conservative 85% default fallback (para 36(c))
example :
    let res := NSFR.evaluateNsfr (pct 100)
      { asOf := obs, derivatives := NsfrDerivativeInput.empty, asfItems := [{ observation := obs, id := "cap", category := .regulatoryCapitalExTier2Lt1Yr, carryingValue := 1000 }]
        rsfAssets :=
          [ { observation := obs, id := "clf3p", category := .clfEligibleThirdPartyDebt, carryingValue := 100 }
          , { observation := obs, id := "clfself", category := .selfSecuritisedClfEligible, carryingValue := 100 } ]
        obsExposures := []
        recognisedClfLimit := some 0 }
    res.totalRsf = 85 + 85 := by native_decide

-- Issue 7: `classifyLoanRsf` fixes (CB >=1yr, 35(a)->35(b) fallback, FI <6m HQLA1 10%)
example :
    NSFR.classifyLoanRsf .centralBank .oneYearOrMore 0 false none 0 = .otherLoansNonFiRwLe35Ge1Yr := by decide

example :
    NSFR.classifyLoanRsf .naturalPerson .oneYearOrMore 0 false
      (some {
              toNaturalPerson := true, isStandardLoanPerAps112 := true, lvrPct := 85, residualMaturityOneYearOrMore := true }) 35 =
    .otherLoansNonFiRwLe35Ge1Yr := by decide

example :
    NSFR.classifyLoanRsf (.adiOrBank true) .lessThanSixMonths 0 false none 20 true =
    .loanToFiLt6mSecuredByHqla1Rehypothecatable := by decide

-- Issue 9: HQLA classifiers (9d/9e, 2B RMBS, 2B equity) & para 29 partial Level 2 transferability
example :
    classifyHqla9d9e false true true = some (.hqla1, true) := by decide

example :
    classifyHqla2bRmbs .aa 80 true true true true true = some .hqla2bRmbs := by decide

example :
    classifyHqla2bEquity true true true true true = some .hqla2bEquity := by decide

example :
    let op : HqlaOperationalCompliance :=
      {
        convertibleToCashAnytime := true, unencumberedAndControlled := true, controlEvidenced := true, notPledgedOrOperationalCost := true,
        notRehypothecatedIfReceivedInSft := true }
    ({ level2TransferGroup := "g", daysSinceIneligible := .known none, max30dPriceDecline := .known 0, max30dHaircutIncrease := .known 0, observation := obs, id := "p29", category := .hqla1, currency := .foreign "NZD", marketValue := 100,
       operational := op, issuerRestrictionSatisfied := true, recognisedByRegulator := true,
       freelyTransferableAtLevel2 := false } : HqlaAsset).cappedValue .level2 [] [("g", 40)] = 0 ∧
    (computeStockOfHqla .level2
      [{ level2TransferGroup := "nz", daysSinceIneligible := .known none, max30dPriceDecline := .known 0, max30dHaircutIncrease := .known 0,
         observation := obs, id := "p29a", category := .hqla1, currency := .foreign "NZD", marketValue := 100,
         operational := op, issuerRestrictionSatisfied := true, recognisedByRegulator := true, freelyTransferableAtLevel2 := false },
       { level2TransferGroup := "nz", daysSinceIneligible := .known none, max30dPriceDecline := .known 0, max30dHaircutIncrease := .known 0,
         observation := obs, id := "p29b", category := .hqla1, currency := .foreign "NZD", marketValue := 60,
         operational := op, issuerRestrictionSatisfied := true, recognisedByRegulator := true, freelyTransferableAtLevel2 := false }]
      {} none [] [("nz", 40)]).1 = 40 := by native_decide
-- Para 29: the cap of 40 applies to the group total (160), not to each asset (which would give 80).

-- ============================================================================
-- 13. Newly-Closed Governance, Buffer, Diversification, CLF & External Wiring Tests
-- ============================================================================

-- HQLA2A diversification (para 11) & CLF process/diversification (paras 16–21) & IRB route
example :
    let op : HqlaOperationalCompliance :=
      {
        convertibleToCashAnytime := true, unencumberedAndControlled := true, controlEvidenced := true, notPledgedOrOperationalCost := true,
        notRehypothecatedIfReceivedInSft := true }
    ({
       freelyTransferableAtLevel2 := false, level2TransferGroup := "g", daysSinceIneligible := .known none, max30dPriceDecline := .known 0, max30dHaircutIncrease := .known 0, observation := obs, id := "2a-undiv", category := .hqla2a, currency := .aud, marketValue := 100, operational := op, hqla2aDiversified := false } : HqlaAsset).valueAfterHaircut .level1 = 0 := by native_decide

example :
    ({
       approvedByApra := true, contractualAmount := 50, pledgedUnderZeroRunoffSecFunding := 0, receivedUnderZeroInflowSecLending := 0,
       collateralHeldAfterRbaMargins := 50,
       annualReapprovalCurrent := false, collateralOperational := .known opOk,
       recentlyIneligibleCollateralDays := .unknown "n/a", observation := obs } : ClfInput).recognisedAmount = 0 := by native_decide

example :
    classifyHqlaCorpDebtByIrbPd true true 5 10 50 true = some .hqla2a ∧
    classifyHqlaCorpDebtByIrbPd true true 30 10 50 true = some .hqla2bCorporateDebt := by decide

-- LCR & NSFR internal buffers (Att A para 4, Att C para 4)
example :
    let op : HqlaOperationalCompliance :=
      {
        convertibleToCashAnytime := true, unencumberedAndControlled := true, controlEvidenced := true, notPledgedOrOperationalCost := true,
        notRehypothecatedIfReceivedInSft := true }
    let res := evaluateLcr .locallyIncorporated false (pct 100)
      { asOf := obs, basis := .level1
        hqlaAssets := [{
                         freelyTransferableAtLevel2 := false, level2TransferGroup := "g", daysSinceIneligible := .known none, max30dPriceDecline := .known 0, max30dHaircutIncrease := .known 0, observation := obs, id := "h1", category := .hqla1, currency := .aud, marketValue := 105, operational := op, issuerRestrictionSatisfied := true, recognisedByRegulator := true }]
        retailDeposits := []
        otherOutflows := [{ observation := obs, id := "o1", currency := .aud, category := .nonOperationalOther, amount := 100 }]
        inflows := []
        internalHqlaBuffer := 10 }
    res.ratioCheck.compliant = true ∧ res.bufferMet = false := by native_decide

example :
    let res := NSFR.evaluateNsfr (pct 100)
      { asOf := obs, derivatives := NsfrDerivativeInput.empty, asfItems := [{ observation := obs, id := "cap", category := .regulatoryCapitalExTier2Lt1Yr, carryingValue := 105 }]
        rsfAssets := [{ observation := obs, id := "m", category := .allOtherAssets, carryingValue := 100 }]
        obsExposures := []
        internalNsfrBuffer := 10 }
    res.ratioCheck.compliant = true ∧ res.bufferMet = false := by native_decide

-- NSFR bilateral netting & cash VM eligibility wiring
-- Contract-level bilateral netting failure disables cross-contract netting in `contracts`,
-- while Para 37(b) always applies `MAX(nsfrDerivativeAssets - nsfrDerivativeLiabilities, 0)`
example :
    let ineligibleBna : External.BilateralNettingAgreement :=
      {
        writtenAndLegallyEnforceable := false, singleLegalObligationCloseOut := true, noWalkawayClause := true, reasonedLegalOpinionsInAllJurisdictions := true }
    let vmSegregated : External.CashVariationMarginEligibility :=
      { isCash := true, notSegregatedIfNonQccp := false, calculatedAndExchangedDailyMtM := true,
        inContractMnaOrCcpCurrency := true, inSameSettlementCurrencyAsDerivative := true,
        fullyExtinguishesMtmExposure := true }
    let d : NSFR.NsfrDerivativeInput :=
      { contracts :=
          [ { observation := obs, nettingSetId := "ns1", mtm := 50, bilateralNetting := some ineligibleBna }
          , { observation := obs, nettingSetId := "ns1", mtm := -30, bilateralNetting := some ineligibleBna } ]
        variationMarginPosted := 0
        variationMarginReceived :=
          [ { nettingSetId := "ns1", cashReceived := 20, observation := obs, eligibility := .known vmSegregated } ] }
    d.nsfrDerivativeAssets = 50 ∧ d.nsfrDerivativeLiabilities = 30 ∧ d.netDerivativeRsf = 20 := by native_decide

-- Specific LCR breach / MLH shortfall notifications & Securitisation APS 120 wiring
example :
    ({
       hasLiquidityConcerns := false, notifiedApraOfConcerns := false, experiencingSevereStress := false, notifiedApraImmediatelyOfStress := false,
       intendingToRequestEla := false, notifiedApraPriorToApproachingRba := false,
       hasOperationalCapabilityForElaInfo := false,
       actualOrLikelyLcrBreach := true, notifiedApraOfLcrBreach := false } : Governance.NotificationStatus).isCompliant = false := by decide

example :
    ({
       isOriginatingAdi := true, meetsAps120CapitalRelief := true, spvExcludedFromMlhAndNsfr := true,
       aps120Criteria := some
         {
           significantCreditRiskTransferred := false, cleanBreakFromOriginator := true, noImplicitOrMoralRecourse := true, armsLengthTerms := true } } : Governance.SecuritisationTreatment).isCompliant = false := by decide

-- Continuous-basis compliance requires non-empty all-compliant snapshots
example :
    evaluateContinuousCompliance [] = false := by decide

-- HQLA stress thresholds (10%/20%/40%), 30-day numeric grace check, SFT netting, MLH 2-day check
example :
    HqlaCategory.hqla2a.meetsStressThreshold (pct 10) (pct 10) = true ∧
    HqlaCategory.hqla2a.meetsStressThreshold (pct 11) (pct 10) = false ∧
    HqlaCategory.hqla2bRmbs.meetsStressThreshold (pct 20) (pct 20) = true ∧
    HqlaCategory.hqla2bEquity.meetsStressThreshold (pct 40) (pct 40) = true := by native_decide

example :
    let op : HqlaOperationalCompliance :=
      {
        convertibleToCashAnytime := true, unencumberedAndControlled := true, controlEvidenced := true, notPledgedOrOperationalCost := true,
        notRehypothecatedIfReceivedInSft := true }
    ({
       freelyTransferableAtLevel2 := false, level2TransferGroup := "g", max30dPriceDecline := .known 0, max30dHaircutIncrease := .known 0, observation := obs, id := "grace-31", category := .hqla2a, currency := .aud, marketValue := 100, operational := op, issuerRestrictionSatisfied := false,
       recognisedByRegulator := false,
       daysSinceIneligible := .known (some 31) } : HqlaAsset).valueAfterHaircut .level1 = 0 := by native_decide

-- Att A Para 8: Non-HQLA-for-cash SFT legs are excluded (`bothLegsAreHqla := false`),
-- while HQLA-for-HQLA legs unwind symmetrically regardless of APS 110 Att D netting criteria.
example :
    (computeUnwindAdjustments
      [{ meetsOperationalReqs := true, bothLegsAreHqla := false, hqla1Delta := 25 }]).deltaHqla1 = 0 ∧
    (computeUnwindAdjustments
      [{
         meetsOperationalReqs := true, bothLegsAreHqla := true, hqla1Delta := 25, nettingCriteria := some
           {
             sameCounterparty := true, sameExplicitFinalSettlementDate := true, notOpenEndedUnwindable := true,
             legallyEnforceableNettingNormalAndDefault := false,
             settledNetOrSimultaneously := true } }]).deltaHqla1 = 25 := by native_decide

example :
    ({
       observation := obs, id := "mlh-3d", category := .commonwealthAndSemiGovtSecurities, marketValue := 100, liquidationBusinessDays := some 3 } : MLH.MlhAsset).eligibleValue = 0 := by native_decide

example :
    let op : HqlaOperationalCompliance :=
      {
        convertibleToCashAnytime := true, unencumberedAndControlled := true, controlEvidenced := true, notPledgedOrOperationalCost := true,
        notRehypothecatedIfReceivedInSft := true }
    ({
       freelyTransferableAtLevel2 := false, level2TransferGroup := "g", daysSinceIneligible := .known none, max30dHaircutIncrease := .known 0, observation := obs, id := "stress-fail", category := .hqla2a, currency := .aud, marketValue := 100, operational := op, max30dPriceDecline := .known (pct 11) } : HqlaAsset).valueAfterHaircut .level1 = 0 := by native_decide

-- ============================================================================
-- 14. Regression Tests for Newly-Reported Issues (1–5)
-- ============================================================================

-- 14(1): `evaluateAdiCompliance` preserves LCR CLF (1,000 -> NSFR RSF = 100, not 850) when `recognisedClfLimit := none`
example :
    let op : HqlaOperationalCompliance :=
      {
        convertibleToCashAnytime := true, unencumberedAndControlled := true, controlEvidenced := true, notPledgedOrOperationalCost := true,
        notRehypothecatedIfReceivedInSft := true }
    let clf : ClfInput :=
      {
        approvedByApra := true, contractualAmount := 1000, pledgedUnderZeroRunoffSecFunding := 0, receivedUnderZeroInflowSecLending := 0,
        collateralHeldAfterRbaMargins := 1000, collateralDiversified := true,
        annualReapprovalCurrent := true, includedInFundingStrategy := true,
        balanceSheetEffortsExhausted := true, drawdownAttestedAndNotifiedToApra := true,
        collateralOperational := .known opOk, recentlyIneligibleCollateralDays := .unknown "n/a", observation := obs }
    let lcrAud : LcrInput :=
      {
        asOf := obs, basis := .level1, hqlaAssets := [], clf := some clf, retailDeposits := [],
        otherOutflows := [{
        observation := obs, id := "o1", currency := .aud, category := .nonOperationalOther, amount := 1000 }], inflows := [] }
    let lcrAll : LcrInput :=
      {
        asOf := obs, basis := .level1, hqlaAssets := [{
                         freelyTransferableAtLevel2 := false, level2TransferGroup := "g", daysSinceIneligible := .known none, max30dPriceDecline := .known 0, max30dHaircutIncrease := .known 0, observation := obs, id := "h1", category := .hqla1, currency := .aud, marketValue := 1000, operational := op, issuerRestrictionSatisfied := true, recognisedByRegulator := true }],
        retailDeposits := [],
        otherOutflows := [{
        observation := obs, id := "o1", currency := .aud, category := .nonOperationalOther, amount := 1000 }], inflows := [] }
    let nsfrIn : NSFR.NsfrInput :=
      {
        asOf := obs, derivatives := NsfrDerivativeInput.empty, asfItems := [{ observation := obs, id := "cap", category := .regulatoryCapitalExTier2Lt1Yr, carryingValue := 200 }], rsfAssets := [{
        observation := obs, id := "self-sec", category := .selfSecuritisedClfEligible, carryingValue := 1000 }], obsExposures := [] }
    let govOk : Governance.BoardAndManagementGovernance :=
      {
        hasBoardApprovedRiskTolerance := true, hasBoardApprovedStrategyAndPolicy := true, hasOperatingStandards := true, hasBoardApprovedFundingStrategy := true,
        hasContingencyFundingPlan := true, personnelHaveNecessaryExperience := true,
        frameworkDocumentedAndReviewedAnnually := true, boardReviewsRegularReports := true,
        seniorManagementDutiesMet := true, understandsRiskLinkages := true,
        level2GroupComplianceEnsured := true }
    let fwOk : Governance.LiquidityFramework :=
      {
        riskToleranceDocumentedAndAppropriate := true, riskToleranceReviewedAnnually := true, riskToleranceSupportsProlongedStress := true, riskToleranceArticulatesRiskProfitTradeoff := true,
        setsOutLevel1AndLevel2Structure := true, coversUnsecuredAndSecuredLossEvents := true,
        integratedIntoOverallRiskManagement := true, oversightFunctionOperationallyIndependent := true,
        coversCompositionMaturityDiversityIntradayCurrencies := true,
        includesNormalAndStressQuantQualTargets := true, communicatedThroughoutOrganisation := true,
        immediateEscalationOfEmergingConcerns := true, reviewedOngoingAndUnderCps220 := true,
        considersLegalStructureJurisdictionsAndHomeHost := true,
        businessUnitsUnderstandAndFollowPolicies := true }
    let rmOk : Governance.LiquidityRiskManagement :=
      {
        projectsCashFlowsAcrossHorizons := true, setsAndReviewsLimitsWithEscalation := true, managesCollateralEncumberedVsUnencumbered := true, hasEarlyWarningIndicators := true,
        hasReliableForwardLookingMis := true, managesIntradayLiquidity := true,
        allocatesFundingLiquidityCostsAndBenefits := true, activeInMultipleCurrencies := false,
        multiCurrencyNeedsMatchedAndAnalysed := false }
    let fsOk : Governance.FundingStrategy :=
      {
        hasThreeYearStrategyDocumented := true, maintainsMarketPresenceAndRelationships := true, regularlyGaugesFundRaisingCapacity := true, reviewedAndApprovedByBoardAnnually := true,
        updatedAnnually := true, materialChangesAdvisedToApra := true }
    let cfpOk : Governance.ContingencyFundingPlan :=
      {
        hasFormalPlanWithInvocationAndEscalation := true, articulatesSourcesAmountsLeadTimes := true, integratedWithStressTestsAndIntraday := true, hasRetailDeposits := true,
        addressesRetailRunWithoutClosingChannels := true, reviewedTestedAndBoardApprovedAnnually := true }
    let notifOk : Governance.NotificationStatus :=
      {
        hasLiquidityConcerns := false, notifiedApraOfConcerns := false, experiencingSevereStress := false, notifiedApraImmediatelyOfStress := false,
        intendingToRequestEla := false, notifiedApraPriorToApproachingRba := false,
        hasOperationalCapabilityForElaInfo := true }
    let stOk : Governance.StressTesting :=
      {
        conductsRegularShortAndProtractedTests := true, coversInstitutionSpecificMarketWideCombined := true,
        outcomesAdjustStrategyAndCfp := true, analysesLevel1Level2AndBusinessLines := true,
        documentedAndReportedToBoardAndApra := true,
        resultsFeedStrategicPlanningAndLimits := true }
    let res := evaluateAdiCompliance
      {
        asOf := obs, profile := { incorporation := .locallyIncorporated, classification := .lcr }, boardAndManagement := govOk, framework := fwOk, riskManagement := rmOk,
        fundingStrategy := fsOk, contingencyFundingPlan := cfpOk, notifications := notifOk,
        stressTesting := some stOk, hasLevel2Group := false,
        lcrAudInput := some lcrAud, lcrAllCurrenciesInput := some lcrAll, nsfrInput := some nsfrIn,
        apraAdjustmentsComplied := true, bufferPolicy := some .enforceAllBuffers,
        notifiedApraOfRelianceOrPriorDiscretion := true }
    res.overallCompliant = true ∧
    (res.nsfrResult.map (·.totalRsf) = some 100) := by native_decide

-- 14(2): Excess operational balance from `.otherLegalEntity` gets 100%, not 40%
example :
    sumBy (classifyOperationalDeposit "op-ole" .aud .otherLegalEntity 100 50 false false 0 obs) OutflowItem.outflowAmount = 25 + 50 := by native_decide

-- 14(3): NSFR CLF allocation is order-independent (greedy by fallback factor descending)
example :
    let a : NSFR.RsfAssetItem := { observation := obs, id := "a", category := .clfEligibleThirdPartyDebt, carryingValue := 100, excessClfFallbackFactor := some (pct 50) }
    let b : NSFR.RsfAssetItem := { observation := obs, id := "b", category := .clfEligibleThirdPartyDebt, carryingValue := 100, excessClfFallbackFactor := some (pct 85) }
    let r1 := NSFR.evaluateNsfr (pct 100) { asOf := obs, derivatives := NsfrDerivativeInput.empty, asfItems := [], rsfAssets := [a, b], obsExposures := [], recognisedClfLimit := some 100 }
    let r2 := NSFR.evaluateNsfr (pct 100) { asOf := obs, derivatives := NsfrDerivativeInput.empty, asfItems := [], rsfAssets := [b, a], obsExposures := [], recognisedClfLimit := some 100 }
    r1.totalRsf = 10 + 50 ∧ r1.totalRsf = r2.totalRsf := by native_decide

-- 14(4): Omitting `hqla2aDiversified` fails closed (0), and `ClfInput` process flags fail closed (0)
example :
    let op : HqlaOperationalCompliance :=
      {
        convertibleToCashAnytime := true, unencumberedAndControlled := true, controlEvidenced := true, notPledgedOrOperationalCost := true,
        notRehypothecatedIfReceivedInSft := true }
    ({ freelyTransferableAtLevel2 := false, level2TransferGroup := "g", daysSinceIneligible := .known none, max30dPriceDecline := .known 0, max30dHaircutIncrease := .known 0, observation := obs, id := "2a-default", category := .hqla2a, currency := .aud, marketValue := 100, operational := op } : HqlaAsset).valueAfterHaircut .level1 = 0 := by native_decide

-- 14(5): `RetailDeposit.runoffRate` agrees with `outflowAmount` on $400k deposit (27500/400000 = 11/160),
--        and `classifyHqla2bRmbs` rejects pools with structured products (12(a)(v))
example :
    let d : RetailDeposit := { customerId := "c848_29", guaranteeLimit := 250000, scorecard := noScore, observation := obs, id := "fcs", currency := .aud, balance := 400000, isStable := true }
    d.runoffRate .level1 [d] = (11 : Rat) / 160 ∧
    d.solo = nonNeg d.balance * d.runoffRate .level1 [d] := by native_decide

example :
    classifyHqla2bRmbs .aa 80 true true false true true = none := by decide

-- ============================================================================
-- 15. Regression Tests for Rule Fixes & CashFlows Module
-- ============================================================================

-- Fix: Non-0%/20%-RW sovereign/CB/PSE/MDB debt rated A+..BBB- is NOT HQLA2B corporate debt
example :
    classifyHqlaDebtSecurity true 50 .aPlus false false true true = none := by decide

-- Fix: Qualifying SME (< $2m, managed as retail) excess operational balance gets 10% retail rate
example :
    sumBy (classifyOperationalDeposit "op-sme" .aud (.sme 500000 true) 100 50 false false 0 obs) OutflowItem.outflowAmount = 25 + 5 := by native_decide

-- Fix: Intermediated non-look-through term deposit fails exclusion when category is tainted
example :
    ({
       customerId := "c869_5", guaranteeLimit := 250000, scorecard := noScore, observation := obs, id := "int-taint", currency := .aud, balance := 100, isStable := false, isIntermediated := true, intermediatedLookThroughMet := false,
       residualMaturityOrNoticeGt30Days := true, noWithdrawalRightOrMaterialPenalty := true,
       categoryTaintedByEarlyWithdrawal := true } : RetailDeposit).solo = 100 := by native_decide

-- Fix: Para 32(a) excess defaults conservatively to 85% (para 36(c)) and host ALA works independently of CLF = 0
example :
    let a : NSFR.RsfAssetItem := { observation := obs, id := "a", category := .hostJurisdictionAla, carryingValue := 100 }
    let rNoLim := NSFR.evaluateNsfr (pct 100) { asOf := obs, derivatives := NsfrDerivativeInput.empty, asfItems := [], rsfAssets := [a], obsExposures := [] }
    let rHostAla := NSFR.evaluateNsfr (pct 100) { asOf := obs, derivatives := NsfrDerivativeInput.empty, asfItems := [], rsfAssets := [a], obsExposures := [], hostAlaInLcrLimit := 100 }
    rNoLim.totalRsf = 85 ∧ rHostAla.totalRsf = 10 := by native_decide

-- Fix: Para 25 CLF grace period removes only recently-ineligible collateral from bound 15(c) after > 30 days
example :
    let base : ClfInput :=
      {
        approvedByApra := true, contractualAmount := 100, pledgedUnderZeroRunoffSecFunding := 0, receivedUnderZeroInflowSecLending := 0,
        collateralHeldAfterRbaMargins := 60,
        recentlyIneligibleCollateralAfterRbaMargins := 30,
        collateralDiversified := true, annualReapprovalCurrent := true,
        includedInFundingStrategy := true, balanceSheetEffortsExhausted := true,
        collateralOperational := .known opOk, recentlyIneligibleCollateralDays := .known 20, observation := obs }
    base.recognisedAmount = 90 ∧
    ({ base with recentlyIneligibleCollateralDays := .known 35 } : ClfInput).recognisedAmount = 60 ∧
    -- An unknown day count grants no grace (previously `none` granted grace indefinitely).
    ({ base with recentlyIneligibleCollateralDays := .unknown "downgrade date not captured" } : ClfInput).recognisedAmount = 60 := by
  native_decide

-- Fix: APRA-approved non-debt MLH securities do not require RBA repo-eligibility or non-subordination
example :
    ({
       observation := obs, id := "apra-eq", category := .apraApprovedOtherSecurities, marketValue := 50, unencumberedOrApraApproved := true, rbaRepoEligible := false, nonSubordinated := false,
       isDebtSecurity := false, liquidatableWithinTwoBusinessDays := true } : MLH.MlhAsset).eligibleValue = 50 := by native_decide

-- Fix: NSFR SFT netting per Attachment C para 26 & APS 110 Att D para 38
example :
    let openRepo : External.SftNettingCriteria :=
      {
        sameCounterparty := true, sameExplicitFinalSettlementDate := false, notOpenEndedUnwindable := false,
        legallyEnforceableNettingNormalAndDefault := true, settledNetOrSimultaneously := true }
    let termRepo : External.SftNettingCriteria :=
      {
        sameCounterparty := true, sameExplicitFinalSettlementDate := true, notOpenEndedUnwindable := true,
        legallyEnforceableNettingNormalAndDefault := true, settledNetOrSimultaneously := true }
    ({ receivable := 100, payable := 60, nettingCriteria := openRepo } : NSFR.NsfrSftNettingGroup).netNsfrReceivable = 100 ∧
    ({ receivable := 100, payable := 60, nettingCriteria := termRepo } : NSFR.NsfrSftNettingGroup).netNsfrReceivable = 40 := by native_decide

-- Fix: Conditional defaults (non-drawn CLF, solo ADI without Level 2, no APRA adjustments/prior discretion)
example :
    ({
       approvedByApra := true, contractualAmount := 50, pledgedUnderZeroRunoffSecFunding := 0, receivedUnderZeroInflowSecLending := 0,
       collateralHeldAfterRbaMargins := 50, collateralDiversified := true,
       annualReapprovalCurrent := true, includedInFundingStrategy := true,
       balanceSheetEffortsExhausted := true, collateralOperational := .known opOk,
       recentlyIneligibleCollateralDays := .unknown "n/a", observation := obs } : ClfInput).recognisedAmount = 50 := by native_decide

-- CashFlows: NSFR liability & amortising loan maturity splitting (Att C Paras 8, 11(c), 22)
example :
    let liabAsf := sumBy
      (splitLiabilityCashFlowsForAsf "bond" .otherFundingLt6m .otherFunding6mToLt1Yr .borrowingsAndLiabilitiesGe1Yr
        [(100, 90), (200, 240), (300, 400)] false none obs)
      NSFR.AsfItem.weightedAmount
    let loanRsf := sumBy
      (splitAmortisingLoanForRsf "loan" .otherNonHqlaLt1Yr .otherPerformingLoansNonFiGe1Yr
        [(100, 180), (400, 730)] .unencumbered false none obs)
      NSFR.RsfAssetItem.weightedAmount
    -- ASF = 100*0 + 200*0.5 + 300*1 = 400; RSF = 100*0.5 + 400*0.85 = 390
    liabAsf = 400 ∧ loanRsf = 390 := by native_decide

-- CashFlows: Multi-horizon cash-flow ladder & survival buffer (Main Body Para 36)
example :
    let flows : List ContractualCashFlow :=
      [ cf "out1" .weOwe 60 5
      , cf "in1" .owedToUs 40 20
      , cf "out2" .weOwe 50 90 ]
    hasPositiveSurvivalBuffer 70 flows = true ∧
    hasPositiveSurvivalBuffer 60 flows = false := by native_decide

-- CashFlows: LCR derivative MNA + same-day FX netting & SPV/Para 60/Para 31 filtering
example :
    let flows : List ContractualCashFlow :=
      [ { cf "spv-out" .weOwe 50 15 with source := .securitisationSpv }
      , { cf "open-min" .owedToUs 20 10 with isOpenMaturity := true, isMinimumContractualPayment := true }
      , { cf "open-full" .owedToUs 999 10 with isOpenMaturity := true }
      , cf "ext-late" .owedToUs 999 20 [{ holder := .counterparty, kind := .extension, exerciseDate := 15, resultingSettlementDate := 40 }]
      , { cf "hqla-in" .owedToUs 999 10 with countedInHqla := true }
      , { cf "d1p" .weOwe 80 5 with source := .derivative "cp1" true false }
      , { cf "d1r" .owedToUs 30 5 with source := .derivative "cp1" true false }
      , { cf "d2p" .weOwe 20 3 with source := .derivative "cp2" false true }
      , { cf "d2r" .owedToUs 50 3 with source := .derivative "cp2" false true } ]
    let (outs, ins) := deriveLcrFromCashFlows flows (fun _ => .nonOperationalOther) (fun _ => .otherWholesaleEntity) []
    sumBy outs OutflowItem.outflowAmount = 50 + 50 ∧
    sumBy ins InflowItem.inflowAmount = 10 + 30 := by native_decide

-- Derivative collateral direction (Att A fn 5): collateral is a directed movement, not an unsigned adjustment.
-- Netting set cp1 has a net payable of 50.
--   * collateral we hold and must return (we owe it) increases the outflow: 50 + 15 = 65;
--   * eligible collateral owed to us, not already in HQLA, reduces it: 50 - 20 = 30;
--   * collateral owed to us whose HQLA status is unknown does not reduce it: 50;
--   * collateral owed to us beyond the payable never becomes an inflow: 0 out, 0 in.
example :
    let pays : List ContractualCashFlow :=
      [ { cf "p" .weOwe 80 5 with source := .derivative "cp1" true false }
      , { cf "r" .owedToUs 30 5 with source := .derivative "cp1" true false } ]
    let run (c : List CollateralMovement) : Money × Money :=
      let (o, i) := deriveLcrDerivativeOutflowsAndInflows pays c
      (sumBy o OutflowItem.outflowAmount, sumBy i InflowItem.inflowAmount)
    run [coll "p" "cp1" .weOwe 15 (.known false) (.known false)] = (65, 0) ∧
    run [coll "p" "cp1" .owedToUs 20 (.known true) (.known false)] = (30, 0) ∧
    run [coll "p" "cp1" .owedToUs 20 (.known true) (.unknown "custody feed missing")] = (50, 0) ∧
    run [coll "p" "cp1" .owedToUs 100 (.known true) (.known false)] = (0, 0) := by native_decide

-- Without a netting agreement, eligible collateral owed to us offsets only the payment it secures,
-- and collateral we owe is an outflow on its own.
example :
    let pays : List ContractualCashFlow :=
      [ { cf "g1" .weOwe 40 5 with source := .derivative "cp3" false false }
      , { cf "g2" .weOwe 25 5 with source := .derivative "cp3" false false } ]
    let (o, _) := deriveLcrDerivativeOutflowsAndInflows pays
      [coll "g1" "cp3" .owedToUs 10 (.known true) (.known false), coll "g2" "cp3" .weOwe 5 (.known false) (.known false)]
    sumBy o OutflowItem.outflowAmount = (40 - 10) + 25 + 5 := by native_decide

-- Explicit options with their holder (Att A paras 45 & 60, Att C paras 8 & 20).
-- A liability due on day 100 that the investor can redeem on day 10 is inside the LCR horizon; if only the
-- ADI can call it, it is not (LCR), but the NSFR still takes the earliest redemption date.
-- A row with both a counterparty call (day 10) and an extension (day 400) settles on day 10 when we owe it.
example :
    let callCp : ContractOption := { holder := .counterparty, kind := .earlyRedemption, exerciseDate := 5, resultingSettlementDate := 10 }
    let callUs : ContractOption := { callCp with holder := .us }
    let ext : ContractOption := { holder := .counterparty, kind := .extension, exerciseDate := 90, resultingSettlementDate := 400 }
    (cf "l1" .weOwe 100 100 [callCp]).isLcrOutflowWithin30d = true ∧
    (cf "l2" .weOwe 100 100 [callUs]).isLcrOutflowWithin30d = false ∧
    (cf "l2" .weOwe 100 100 [callUs]).worstCaseDays .nsfr = some 10 ∧
    (cf "l3" .weOwe 100 100 [callCp, ext]).worstCaseDays .lcr = some 10 ∧
    (cf "a3" .owedToUs 100 100 [callCp, ext]).worstCaseDays .lcr = some 400 := by native_decide

-- No silent defaults: an obligation we owe with unknown options is due now; one owed to us with unknown
-- options or performance contributes no inflow, and the reason is reported.
example :
    let unknownOpts (f : ContractualCashFlow) : ContractualCashFlow :=
      { f with obligation := { f.obligation with optionality := .unknown "option schedule not loaded" } }
    let unknownPerf (f : ContractualCashFlow) : ContractualCashFlow :=
      { f with obligation := { f.obligation with performing := .refused "credit file restricted" } }
    (unknownOpts (cf "o" .weOwe 10 200)).isLcrOutflowWithin30d = true ∧
    (unknownOpts (cf "i" .owedToUs 10 5)).isEligibleLcrInflow = false ∧
    (unknownPerf (cf "i" .owedToUs 10 5)).isEligibleLcrInflow = false ∧
    (cashFlowDataIssues [unknownOpts (cf "i" .owedToUs 10 5)] []).length = 1 := by native_decide

-- Empty calculations are not compliant (previously ASF = RSF = 0 passed).
example :
    (NSFR.evaluateNsfr (pct 100)
      { asOf := obs, derivatives := NsfrDerivativeInput.empty, asfItems := [], rsfAssets := [], obsExposures := [] }
      ).ratioCheck.compliant = false ∧
    (checkRatio 10 0 (pct 100)).compliant = true := by native_decide

-- Para 28: VM offsets only its own netting set (capped at that set's positive RC), and only when it
-- fully extinguishes the MtM exposure.
example :
    let elig : External.CashVariationMarginEligibility :=
      { isCash := true, notSegregatedIfNonQccp := true, calculatedAndExchangedDailyMtM := true,
        inContractMnaOrCcpCurrency := true, inSameSettlementCurrencyAsDerivative := true,
        fullyExtinguishesMtmExposure := true }
    let d : NsfrDerivativeInput :=
      { contracts := [mtm "A" 10, mtm "B" 50]
        variationMarginPosted := 0
        variationMarginReceived := [{ nettingSetId := "A", cashReceived := 30, eligibility := .known elig, observation := obs }] }
    let vmA (e : Reading External.CashVariationMarginEligibility) : NettingSetVariationMargin :=
      { nettingSetId := "A", cashReceived := 30, observation := obs, eligibility := e }
    let dPartial : NsfrDerivativeInput :=
      { d with variationMarginReceived := [vmA (.known { elig with fullyExtinguishesMtmExposure := false })] }
    let dMissing : NsfrDerivativeInput :=
      { d with variationMarginReceived := [vmA (.unknown "CSA terms not captured")] }
    -- set A: max(10 - 30, 0) = 0; set B unaffected = 50. A total offset would give max(60 - 30, 0) = 30.
    d.nsfrDerivativeAssets = 50 ∧ dPartial.nsfrDerivativeAssets = 60 ∧ dMissing.nsfrDerivativeAssets = 60 := by native_decide

-- Para 32: host ALA has its own pool; it cannot enlarge the 32(b) self-securitised limit.
-- CLF 100, host ALA 50; third-party CLF debt 80 uses 80 of the CLF, leaving 20 for self-securitised assets.
example :
    let r := NSFR.evaluateNsfr (pct 100)
      { asOf := obs, derivatives := NsfrDerivativeInput.empty, asfItems := [], obsExposures := [],
        recognisedClfLimit := some 100, hostAlaInLcrLimit := 50,
        rsfAssets := [ { observation := obs, id := "tp", category := .clfEligibleThirdPartyDebt, carryingValue := 80 }
                     , { observation := obs, id := "ss", category := .selfSecuritisedClfEligible, carryingValue := 100 } ] }
    -- 80 * 10% + (20 * 10% + 80 * 85%) = 8 + 70 = 78 (the shared pool gave 8 + 70 * 10% + 30 * 85% = 40.5)
    r.totalRsf = 78 := by native_decide

-- Para 71: an APRA exclusion without approval evidence is not applied.
example :
    let unapproved : ApraAdjustment := { rule := .excludeNsfr, approval := .unknown "letter not on file", observation := obs }
    let approved : ApraAdjustment := { unapproved with approval := .known "APRA-2026-117" }
    unapproved.approved = false ∧ approved.approved = true ∧ unapproved.dataIssues.length = 1 := by native_decide

-- Regression: MLH 0/0 empty calculation returns both compliant = false and aboveTriggerRatio = false
example :
    let res := MLH.evaluateMlh (pct 9)
      { asOf := obs, assets := [], liabilities := [], triggerRatio := pct 10 }
    res.ratioCheck.compliant = false ∧ res.aboveTriggerRatio = false := by native_decide

-- Regression: Stale observation.validAt uses asOf.validAt for worstCaseDays
example :
    let staleObs : ObservationContext := { validAt := 100, knownAt := 100, sourceReference := "stale" }
    let cf : CashFlows.ContractualCashFlow :=
      { obligation :=
          { id := "stale-in", currency := .aud, side := .owedToUs, amount := Amount.ofRat 50,
            settlementDate := .known 150, optionality := .known [], performing := .known true,
            observation := staleObs },
        isOpenMaturity := false, isMinimumContractualPayment := true,
        isNonFinancialRevenue := false, countedInHqla := false, source := .standard }
    cf.worstCaseDays .lcr (some 140) = some 10 ∧ cf.worstCaseDays .lcr none = some 50 := by native_decide

-- Regression: CollateralMovement is included in CashFlows.hasPositiveSurvivalBuffer
example :
    let colOut : CollateralMovement :=
      { obligationId := "col-out", nettingSetId := "ns1", side := .weOwe, amount := Amount.ofRat 80,
        currency := .aud, settlementDate := .known 5, eligibleForOffset := .known true,
        alreadyCountedInHqla := .known false, observation := obs }
    CashFlows.hasPositiveSurvivalBuffer 50 [] none [colOut] (some 0) = false ∧
    CashFlows.hasPositiveSurvivalBuffer 100 [] none [colOut] (some 0) = true := by native_decide

-- ============================================================================
-- 16. Comprehensive APS 210 Faithfulness Audit Regression Tests
-- ============================================================================

-- Att A Para 7: 15% HQLA2B cap uses 1/4 * adj1 when 40% cap binds (exact excess15Cap / excess40Cap)
example :
    let b := computeHqlaCaps 60 30 (31 / 2)
    b.excess15Cap = 1 / 2 ∧ b.excess40Cap = 5 ∧ b.stockExClf = 100 := by native_decide

-- Att A Paras 13-15: CLF is included in All-Currencies LCR capped at AUD shortfall (cannot subsidise FX)
example :
    let op : HqlaOperationalCompliance :=
      { convertibleToCashAnytime := true, unencumberedAndControlled := true, controlEvidenced := true,
        notPledgedOrOperationalCost := true, notRehypothecatedIfReceivedInSft := true }
    let clf : ClfInput :=
      { approvedByApra := true, contractualAmount := 50, pledgedUnderZeroRunoffSecFunding := 0,
        receivedUnderZeroInflowSecLending := 0, collateralHeldAfterRbaMargins := 50,
        collateralOperational := .known op, collateralDiversified := true, annualReapprovalCurrent := true,
        includedInFundingStrategy := true, balanceSheetEffortsExhausted := true,
        recentlyIneligibleCollateralDays := .unknown "none", observation := obs }
    let lcrIn : LcrInput :=
      { asOf := obs, basis := .level1,
        hqlaAssets := [ { id := "aud1", category := .hqla1, currency := .aud, marketValue := 60,
                          operational := op, freelyTransferableAtLevel2 := true, level2TransferGroup := "g",
                          issuerRestrictionSatisfied := true, recognisedByRegulator := false,
                          daysSinceIneligible := .known none, max30dPriceDecline := .known 0,
                          max30dHaircutIncrease := .known 0, observation := obs }
                      , { id := "usd1", category := .hqla1, currency := .foreign "USD", marketValue := 10,
                          operational := op, freelyTransferableAtLevel2 := true, level2TransferGroup := "g",
                          issuerRestrictionSatisfied := true, recognisedByRegulator := false,
                          daysSinceIneligible := .known none, max30dPriceDecline := .known 0,
                          max30dHaircutIncrease := .known 0, observation := obs } ],
        clf := some clf, retailDeposits := [],
        otherOutflows := [ { id := "o-aud", currency := .aud, category := .nonOperationalOther, amount := 100, observation := obs }
                         , { id := "o-usd", currency := .foreign "USD", category := .nonOperationalOther, amount := 20, observation := obs } ],
        inflows := [] }
    let audRes := evaluateLcr .locallyIncorporated true (pct 100) lcrIn
    let allRes := evaluateLcr .locallyIncorporated false (pct 100) lcrIn
    -- AUD LCR: 60 + 50 = 110 >= 100; All-Currencies: CLF capped at AUD shortfall (40), so 60 + 10 + 40 = 110 < 120
    audRes.recognisedClf = 50 ∧ audRes.ratioCheck.compliant = true ∧
    allRes.recognisedClf = 40 ∧ allRes.stockOfHqla = 110 ∧ allRes.ratioCheck.compliant = false := by native_decide

-- Att A Para 25: 30-day grace retains HQLA2 asset that breached stress threshold
example :
    let op : HqlaOperationalCompliance :=
      { convertibleToCashAnytime := true, unencumberedAndControlled := true, controlEvidenced := true,
        notPledgedOrOperationalCost := true, notRehypothecatedIfReceivedInSft := true }
    let a : HqlaAsset :=
      { id := "2a-grace", category := .hqla2a, currency := .aud, marketValue := 100, operational := op,
        freelyTransferableAtLevel2 := true, level2TransferGroup := "g", issuerRestrictionSatisfied := true,
        recognisedByRegulator := true, hqla2aDiversified := true, daysSinceIneligible := .known (some 15),
        max30dPriceDecline := .known (pct 12), max30dHaircutIncrease := .known 0, observation := obs }
    a.valueAfterHaircut .level1 = 85 := by native_decide

-- Att A Paras 35 & 37: Uninsured FX deposit & non-look-through intermediated deposit do not contaminate AUD FCS
example :
    let dAud : RetailDeposit :=
      { id := "d-aud", customerId := "c1", currency := .aud, balance := 200000, isStable := true,
        guaranteeLimit := 250000, scorecard := noScore, observation := obs }
    let dUsd : RetailDeposit :=
      { id := "d-usd", customerId := "c1", currency := .foreign "USD", balance := 100000, isStable := false,
        guaranteeLimit := 0, scorecard := noScore, observation := obs }
    let dInterm : RetailDeposit :=
      { id := "d-int", customerId := "c1", currency := .aud, balance := 500000, isStable := false,
        isIntermediated := true, intermediatedLookThroughMet := false, guaranteeLimit := 0,
        scorecard := noScore, observation := obs }
    dAud.outflowAmount .level1 [dAud, dUsd, dInterm] = 10000 := by native_decide

-- Att A Para 46: SME >= AUD 2m committed facility gets corporate 10%/30%, not 5% retail/SME
example :
    classifyCommittedFacility (.sme 2000000 true) false = .committedCreditNonFinCorpSovCbPse ∧
    classifyCommittedFacility (.sme 2000000 true) true = .committedLiquidityNonFinCorpSovCbPse ∧
    classifyCommittedFacility (.sme 1999999 true) true = .committedFacilityRetailAndSme := by decide

-- Att A fn 5 & 8: Same-day FX principal exchange nets across currencies on same day, not across different days
example :
    let legUsd : CashFlows.ContractualCashFlow :=
      { obligation := { id := "fx1", currency := .foreign "USD", side := .weOwe, amount := Amount.ofRat 100,
                        settlementDate := .known 5, optionality := .known [], performing := .known true, observation := obs },
        isOpenMaturity := false, isMinimumContractualPayment := true, isNonFinancialRevenue := false,
        countedInHqla := false, source := .derivative "fx-set" false true }
    let legAudSame : CashFlows.ContractualCashFlow :=
      { obligation := { id := "fx2", currency := .aud, side := .owedToUs, amount := Amount.ofRat 80,
                        settlementDate := .known 5, optionality := .known [], performing := .known true, observation := obs },
        isOpenMaturity := false, isMinimumContractualPayment := true, isNonFinancialRevenue := false,
        countedInHqla := false, source := .derivative "fx-set" false true }
    let legAudDiff : CashFlows.ContractualCashFlow :=
      { obligation := { id := "fx3", currency := .aud, side := .owedToUs, amount := Amount.ofRat 80,
                        settlementDate := .known 20, optionality := .known [], performing := .known true, observation := obs },
        isOpenMaturity := false, isMinimumContractualPayment := true, isNonFinancialRevenue := false,
        countedInHqla := false, source := .derivative "fx-set" false true }
    let (outsSame, insSame) := CashFlows.deriveLcrDerivativeOutflowsAndInflows [legUsd, legAudSame] []
    let (outsDiff, insDiff) := CashFlows.deriveLcrDerivativeOutflowsAndInflows [legUsd, legAudDiff] []
    sumBy outsSame (·.amount) = 20 ∧ sumBy insSame (·.amount) = 0 ∧
    sumBy outsDiff (·.amount) = 100 ∧ sumBy insDiff (·.amount) = 80 := by native_decide

-- Att A Para 54 & Para 31: Past-due inflow rejected (not clamped to day 0), HQLA inflow not double-counted in ladder
example :
    let pastDue : CashFlows.ContractualCashFlow :=
      { obligation := { id := "pd", currency := .aud, side := .owedToUs, amount := Amount.ofRat 100,
                        settlementDate := .known (-5), optionality := .known [], performing := .known true, observation := obs },
        isOpenMaturity := false, isMinimumContractualPayment := true, isNonFinancialRevenue := false,
        countedInHqla := false, source := .standard }
    let hqlaIn : CashFlows.ContractualCashFlow :=
      { obligation := { id := "hq", currency := .aud, side := .owedToUs, amount := Amount.ofRat 100,
                        settlementDate := .known 5, optionality := .known [], performing := .known true, observation := obs },
        isOpenMaturity := false, isMinimumContractualPayment := true, isNonFinancialRevenue := false,
        countedInHqla := true, source := .standard }
    pastDue.isEligibleLcrInflow = false ∧
    CashFlows.hasPositiveSurvivalBuffer 50 [hqlaIn, { pastDue with obligation := { pastDue.obligation with side := .weOwe, settlementDate := .known 5 } }] none [] (some 0) = false := by native_decide

-- Att C Para 10 fn 11-12 & Para 37(b) fn 29: Excess VM posted flows into 100% RSF via negative nsfrDerivativeLiabilities
example :
    let d : NSFR.NsfrDerivativeInput :=
      { contracts := [{ nettingSetId := "s1", mtm := -100, observation := obs }]
        variationMarginPosted := 120
        variationMarginReceived := [] }
    d.nsfrDerivativeLiabilities = -20 ∧ d.netDerivativeRsf = 20 := by native_decide

-- Att C Para 23: Excess CLF asset encumbered 6m..<1y retains 85% fallback RSF (not 50%)
example :
    let a : NSFR.RsfAssetItem :=
      { id := "clf-enc", category := .clfEligibleThirdPartyDebt, carryingValue := 100,
        encumbrance := .sixMonthsToLessThanOneYear, observation := obs }
    a.effectiveFactor = pct 85 := by native_decide

-- Att C Para 36(a): Initial margin / CCP default fund stays 85% even when marked encumbered >= 1y
example :
    let a : NSFR.RsfAssetItem :=
      { id := "im", category := .initialMarginAndCcpDefaultFund (pct 85) false, carryingValue := 100,
        encumbrance := .oneYearOrMore, observation := obs }
    a.effectiveFactor = pct 85 := by native_decide

-- Att C Para 35(a): APRA assigns 65% RSF to ALL standard residential property loans to natural
-- persons with residual maturity >= 1y and LVR <= 80% (distinct from para 35(b)'s <= 35% RW rule).
example :
    let ooPi : External.StandardResidentialMortgageCriteria :=
      { toNaturalPerson := true, isStandardLoanPerAps112 := true, lvrPct := 80,
        residualMaturityOneYearOrMore := true, isOwnerOccupiedPrincipalAndInterest := true }
    let invIo : External.StandardResidentialMortgageCriteria :=
      { ooPi with isOwnerOccupiedPrincipalAndInterest := false }
    let highLvr : External.StandardResidentialMortgageCriteria :=
      { ooPi with lvrPct := 81 }
    ooPi.qualifiesFor65PctRsf = true ∧ invIo.qualifiesFor65PctRsf = true ∧ highLvr.qualifiesFor65PctRsf = false := by native_decide

-- Att A Para 25: Expired 30-day grace (daysSinceIneligible = 45) excludes asset even if other flags are true
example :
    let op : HqlaOperationalCompliance :=
      { convertibleToCashAnytime := true, unencumberedAndControlled := true, controlEvidenced := true,
        notPledgedOrOperationalCost := true, notRehypothecatedIfReceivedInSft := true }
    let expired : HqlaAsset :=
      { id := "exp", category := .hqla1, currency := .aud, marketValue := 100, operational := op,
        freelyTransferableAtLevel2 := true, level2TransferGroup := "g", issuerRestrictionSatisfied := true,
        recognisedByRegulator := true, daysSinceIneligible := .known (some 45),
        max30dPriceDecline := .known 0, max30dHaircutIncrease := .known 0, observation := obs }
    expired.valueAfterHaircut .level1 = 0 := by native_decide

-- Att A Para 40 Table 1: When no deposit guarantee scheme exists (limit = 0), +2 applies only > AUD 250,000
example :
    let smallUninsured : RetailDeposit :=
      { id := "u1", customerId := "c-no-scheme", currency := .foreign "USD", balance := 1000,
        isStable := false, guaranteeLimit := 0,
        scorecard := { isOnlineAccount := .known false, noEstablishedRelationship := .known true, isHeavilyRateDriven := .known false },
        observation := obs }
    let largeUninsured : RetailDeposit :=
      { smallUninsured with id := "u2", customerId := "c-big-no-scheme", balance := 300000 }
    -- small: score = 1 (<=2) -> 10% = 100; large: score = 2 + 1 = 3 (>2) -> 25% = 75000
    smallUninsured.solo = 100 ∧ largeUninsured.solo = 75000 := by native_decide

-- Att A Paras 6-7: Unadjusted HQLA 40% cap prevents 100% HQLA2 stock from passing via SFT unwind,
-- and total excess comparison never double-counts 15% vs 40% excess across unadjusted/adjusted states.
example :
    let b1 := computeHqlaCaps 0 100 0 { deltaHqla1 := 100, deltaHqla2a := -100, deltaHqla2b := 0 }
    let b2 := computeHqlaCaps 100 80 0 { deltaHqla1 := 0, deltaHqla2a := -30, deltaHqla2b := 30 }
    b1.stockExClf = 0 ∧ b2.excess15Cap = 5 ∧ b2.excess40Cap = (25 : Rat) / 3 ∧ b2.stockExClf = (500 : Rat) / 3 := by native_decide

-- Upstream: Sparse 30-day collateral lookback captures [2, 31] peak (200) even with no row on day 31
example :
    let flows : List Upstream.DailyCollateralFlow :=
      [ { valuationDate := 1, netCollateralFlow := -100, observation := obs }
      , { valuationDate := 15, netCollateralFlow := 200, observation := obs }
      , { valuationDate := 35, netCollateralFlow := -100, observation := obs } ]
    (Upstream.max30dCollateralOutflowCal 100 flows).1 = 200 := by native_decide

-- Upstream: Leap-year 12-month trailing window on 2025-02-28 excludes 2024-02-29 (12 months, not 13)
example :
    let rd := Upstream.civilDate 2025 2 28
    let monthDates : List Upstream.EpochDate :=
      [ Upstream.civilDate 2024 2 29
      , Upstream.civilDate 2024 3 31, Upstream.civilDate 2024 4 30, Upstream.civilDate 2024 5 31
      , Upstream.civilDate 2024 6 30, Upstream.civilDate 2024 7 31, Upstream.civilDate 2024 8 31
      , Upstream.civilDate 2024 9 30, Upstream.civilDate 2024 10 31, Upstream.civilDate 2024 11 30
      , Upstream.civilDate 2024 12 31, Upstream.civilDate 2025 1 31, Upstream.civilDate 2025 2 28 ]
    let months : List Upstream.MonthlyNetOutflow :=
      monthDates.map (fun dt =>
        { monthEndDate := dt, netCashOutflow := if dt == Upstream.civilDate 2024 2 29 then 999 else 100, observation := obs })
    Upstream.avg12mNetOutflowCal rd months = (100, true) := by native_decide

end APS210.Tests
