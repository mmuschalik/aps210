import APS210.Basic
import APS210.ExternalStandards
import APS210.Governance
import APS210.LCR
import APS210.MLH
import APS210.NSFR
import APS210.Compliance
import APS210.CashFlows

namespace APS210.Tests

open APS210.LCR
open APS210.MLH
open APS210.NSFR
open APS210.CashFlows

-- ============================================================================
-- 1. Attachment A, Tables 1 & 2: Scorecard & Retail Deposit Outflows
-- ============================================================================
example :
    ({ aboveGuaranteeLimitOr250k := true
       isOnlineAccount := true
       noEstablishedRelationship := false
       isHeavilyRateDriven := false } : LessStableScorecardInput).score = 4 := by decide

example : lessStableRunoffRate 0 = pct 10 := by native_decide
example : lessStableRunoffRate 2 = pct 10 := by native_decide
example : lessStableRunoffRate 3 = pct 25 := by native_decide
example : lessStableRunoffRate 6 = pct 25 := by native_decide

-- Retail deposit scenarios: stable (5%), less stable <=2 (10%), >=3 (25%),
-- excluded term deposit (0%), tainted term deposit (10%), intermediated fail (100%),
-- Level 2 host override (15%) vs Level 1 ignoring host override.
example :
    ({ id := "r1", currency := .aud, balance := 100, isStable := true } : RetailDeposit).outflowAmount = 5 := by native_decide
example :
    ({ id := "r2", currency := .aud, balance := 100, isStable := false,
       scorecard := { aboveGuaranteeLimitOr250k := true, isOnlineAccount := false,
                      noEstablishedRelationship := false, isHeavilyRateDriven := false } } : RetailDeposit).outflowAmount = 10 := by native_decide
example :
    ({ id := "r3", currency := .aud, balance := 100, isStable := false,
       scorecard := { aboveGuaranteeLimitOr250k := true, isOnlineAccount := false,
                      noEstablishedRelationship := true, isHeavilyRateDriven := false } } : RetailDeposit).outflowAmount = 25 := by native_decide
example :
    ({ id := "r4", currency := .aud, balance := 100, isStable := false,
       residualMaturityOrNoticeGt30Days := true,
       noWithdrawalRightOrMaterialPenalty := true } : RetailDeposit).outflowAmount = 0 := by native_decide
example :
    ({ id := "r5", currency := .aud, balance := 100, isStable := false,
       residualMaturityOrNoticeGt30Days := true,
       noWithdrawalRightOrMaterialPenalty := true,
       categoryTaintedByEarlyWithdrawal := true } : RetailDeposit).outflowAmount = 10 := by native_decide
example :
    ({ id := "r6", currency := .aud, balance := 100, isStable := true,
       isIntermediated := true, intermediatedLookThroughMet := false } : RetailDeposit).outflowAmount = 100 := by native_decide
example :
    let d : RetailDeposit := { id := "r7", currency := .foreign "NZD", balance := 100, isStable := true,
                               hostSupervisorOverrideRate := some (pct 15) }
    d.outflowAmount .level2 = 15 ∧ d.outflowAmount .level1 = 5 := by native_decide

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
      [ { id := "cash", category := .hqla1, currency := .aud, marketValue := 100, operational := opOk }
      , { id := "f-sov-1", category := .hqla1, currency := .foreign "XYZ", marketValue := 30,
          operational := opOk, isNonZeroRwForeignSovInForeignCurrency := true }
      , { id := "f-sov-2", category := .hqla1, currency := .foreign "XYZ", marketValue := 20,
          operational := opOk, isNonZeroRwForeignSovInForeignCurrency := true }
      , { id := "downgraded-ok", category := .hqla2a, currency := .aud, marketValue := 20,
          operational := opOk, recognisedByRegulator := false, becameIneligibleWithin30Days := true,
          hqla2aDiversified := true }
      , { id := "pledged-bad", category := .hqla1, currency := .aud, marketValue := 50,
          operational := opPledged, becameIneligibleWithin30Days := true } ]
    let unwind := computeUnwindAdjustments [{ hqla1Delta := 10, hqla2aDelta := -5 }]
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
       drawdownAttestedAndNotifiedToApra := true } : ClfInput).recognisedAmount = 45 := by native_decide

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
      { basis := .level1
        hqlaAssets := [{
          id := "cash", category := .hqla1, currency := .aud, marketValue := 22
          operational := {
            convertibleToCashAnytime := true, unencumberedAndControlled := true,
            controlEvidenced := true, notPledgedOrOperationalCost := true,
            notRehypothecatedIfReceivedInSft := true
          }
        }]
        retailDeposits := []
        otherOutflows := [{ id := "ws", currency := .aud, category := .nonOperationalOther, amount := 100 }]
        inflows := []
        headOfficeFacility := some {
          apraApproved := true, irrevocableAndDocumented := true,
          quantifiedAmount := 30, hasLocalBankingSubsidiaryInAustralia := false
        } }
    res.recognisedHeadOfficeFacility = 20 ∧ res.ratioCheck.compliant = true := by native_decide

-- AUD-only filtering test: USD asset and USD outflow are excluded when isAudOnly = true
example :
    let opOk : HqlaOperationalCompliance :=
      { convertibleToCashAnytime := true, unencumberedAndControlled := true,
        controlEvidenced := true, notPledgedOrOperationalCost := true,
        notRehypothecatedIfReceivedInSft := true }
    let res := evaluateLcr .locallyIncorporated true (pct 100)
      { basis := .level1
        hqlaAssets :=
          [ { id := "aud-cash", category := .hqla1, currency := .aud, marketValue := 50, operational := opOk }
          , { id := "usd-cash", category := .hqla1, currency := .foreign "USD", marketValue := 100, operational := opOk } ]
        retailDeposits := []
        otherOutflows :=
          [ { id := "aud-out", currency := .aud, category := .nonOperationalOther, amount := 40 }
          , { id := "usd-out", currency := .foreign "USD", category := .nonOperationalOther, amount := 200 } ]
        inflows := [] }
    res.stockOfHqla = 50 ∧ res.netCashOutflows.totalNetCashOutflows = 40 ∧ res.ratioCheck.compliant = true := by native_decide

-- ============================================================================
-- 6. Attachment B: MLH evaluation with para 2(f) netting & para 3 RBA repo check
-- ============================================================================
example :
    let res := evaluateMlh (pct 9)
      { assets := [
          { id := "cgs", category := .commonwealthAndSemiGovtSecurities, marketValue := 8 },
          { id := "adi-dep", category := .netDepositsWithOtherAdisTwoBusinessDays, marketValue := 5 },
          { id := "non-rba", category := .adiBankBillsCdsAndDebt, marketValue := 10, rbaRepoEligible := false }
        ]
        placementsByOtherAdis := 3
        liabilities := [{ id := "liab", amount := 100 }]
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
      { asfItems := [
          { id := "cap", category := .regulatoryCapitalExTier2Lt1Yr, carryingValue := 30 },
          { id := "ret", category := .stableRetailAndSmeDepositsLt1Yr, carryingValue := 100 }
        ]
        rsfAssets := [
          { id := "res-mort", category := .standardResidentialPropertyLoansLvrLe80Ge1Yr, carryingValue := 100 },
          { id := "hqla1", category := .unencumberedHqla1NonZeroPctRsf, carryingValue := 20 },
          { id := "cust-im", category := .initialMarginAndCcpDefaultFund none true,
            carryingValue := 50, encumbrance := .oneYearOrMore },
          { id := "clf-3p", category := .clfThirdPartyDebtOrHostAla, carryingValue := 15 },
          { id := "clf-self", category := .selfSecuritisedClfEligible, carryingValue := 10 }
        ]
        obsExposures := [
          { id := "irrev", category := .irrevocableOrConditionallyRevocableFacility, amount := 100 },
          { id := "uncond", category := .unconditionallyRevocableFacility, amount := 100 }
        ]
        derivatives := {
          grossDerivativeLiabilities := 10, variationMarginPosted := 5,
          grossDerivativeAssets := 8, eligibleCashVariationMarginReceived := 0
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
      { hasBoardApprovedRiskTolerance := true, hasBoardApprovedStrategyAndPolicy := true,
        hasOperatingStandards := true, hasBoardApprovedFundingStrategy := true,
        hasContingencyFundingPlan := true, personnelHaveNecessaryExperience := true,
        frameworkDocumentedAndReviewedAnnually := true, boardReviewsRegularReports := true,
        seniorManagementDutiesMet := true, understandsRiskLinkages := true,
        level2GroupComplianceEnsured := true }
    let fwOk : Governance.LiquidityFramework :=
      { riskToleranceDocumentedAndAppropriate := true, riskToleranceReviewedAnnually := true,
        riskToleranceSupportsProlongedStress := true, riskToleranceArticulatesRiskProfitTradeoff := true,
        setsOutLevel1AndLevel2Structure := true, coversUnsecuredAndSecuredLossEvents := true,
        integratedIntoOverallRiskManagement := true, oversightFunctionOperationallyIndependent := true,
        coversCompositionMaturityDiversityIntradayCurrencies := true,
        includesNormalAndStressQuantQualTargets := true, communicatedThroughoutOrganisation := true,
        immediateEscalationOfEmergingConcerns := true, reviewedOngoingAndUnderCps220 := true,
        considersLegalStructureJurisdictionsAndHomeHost := true,
        businessUnitsUnderstandAndFollowPolicies := true }
    let rmOk : Governance.LiquidityRiskManagement :=
      { projectsCashFlowsAcrossHorizons := true, setsAndReviewsLimitsWithEscalation := true,
        managesCollateralEncumberedVsUnencumbered := true, hasEarlyWarningIndicators := true,
        hasReliableForwardLookingMis := true, managesIntradayLiquidity := true,
        allocatesFundingLiquidityCostsAndBenefits := true, activeInMultipleCurrencies := false,
        multiCurrencyNeedsMatchedAndAnalysed := false }
    let fsOk : Governance.FundingStrategy :=
      { hasThreeYearStrategyDocumented := true, maintainsMarketPresenceAndRelationships := true,
        regularlyGaugesFundRaisingCapacity := true, reviewedAndApprovedByBoardAnnually := true,
        updatedAnnually := true, materialChangesAdvisedToApra := true }
    let cfpOk : Governance.ContingencyFundingPlan :=
      { hasFormalPlanWithInvocationAndEscalation := true, articulatesSourcesAmountsLeadTimes := true,
        integratedWithStressTestsAndIntraday := true, hasRetailDeposits := true,
        addressesRetailRunWithoutClosingChannels := true, reviewedTestedAndBoardApprovedAnnually := true }
    let notifOk : Governance.NotificationStatus :=
      { hasLiquidityConcerns := false, notifiedApraOfConcerns := false,
        experiencingSevereStress := false, notifiedApraImmediatelyOfStress := false,
        intendingToRequestEla := false, notifiedApraPriorToApproachingRba := false,
        hasOperationalCapabilityForElaInfo := false }
    let mlhRes := evaluateAdiCompliance
      { profile := { incorporation := .locallyIncorporated, classification := .mlh }
        boardAndManagement := allGovOk, framework := fwOk, riskManagement := rmOk,
        fundingStrategy := fsOk, contingencyFundingPlan := cfpOk, notifications := notifOk,
        hasLevel2Group := false,
        apraAdjustmentsComplied := true,
        bufferPolicy := some .enforceAllBuffers,
        notifiedApraOfRelianceOrPriorDiscretion := true,
        mlhInput := some {
          assets := [{ id := "cgs", category := .commonwealthAndSemiGovtSecurities, marketValue := 12 }],
          liabilities := [{ id := "dep", amount := 100 }],
          triggerRatio := pct 10
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
example : RsfCategory.clfThirdPartyDebtOrHostAla.baseFactor = pct 10 := by native_decide
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
example : classifyNonOperationalWholesaleDeposit (.sme 1500000 false) false = .nonOperationalCorpSovCbPseMdbSmeGt2mUninsured := by decide
example : classifyCommittedFacility (.adiOrBank true) true = .committedFacilityBanksAndSupervisedAdis := by decide
example : classifyCommittedFacility .otherFinancialInstitution true = .committedLiquidityOtherFinancialInstitutions := by decide
example : (substitutableCollateralOutflow .aud 40 true).outflowAmount = 40 := by native_decide

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
      (some { toNaturalPerson := true, isStandardLoanPerAps112 := true,
              lvrPct := 75, residualMaturityOneYearOrMore := true }) =
    .standardResidentialPropertyLoansLvrLe80Ge1Yr := by native_decide
-- >= 90 days past due -> non-performing per APS 220 -> 100% RSF (.allOtherAssets)
example :
    classifyLoanRsf .naturalPerson .oneYearOrMore 90 false
      (some { toNaturalPerson := true, isStandardLoanPerAps112 := true,
              lvrPct := 75, residualMaturityOneYearOrMore := true }) =
    .allOtherAssets := by native_decide

-- APS 112 Att H, APS 110 Att D, APS 113 para 40, APS 120
example : External.qualifiesAsSmeUnderAps113Para40 1400000 = true := by native_decide
example : External.qualifiesAsSmeUnderAps113Para40 1500000 = false := by native_decide
example :
    ({ writtenAndLegallyEnforceable := true, singleLegalObligationCloseOut := true,
       noWalkawayClause := true, reasonedLegalOpinionsInAllJurisdictions := true } : External.BilateralNettingAgreement).isEligible = true := by decide
example :
    ({ isCash := true, notSegregatedIfNonQccp := true, calculatedAndExchangedDailyMtM := true,
       inContractMnaOrCcpCurrency := true, inSameSettlementCurrencyAsDerivative := true } : External.CashVariationMarginEligibility).isEligibleForNsfrOffset = true := by decide
example :
    ({ sameCounterparty := true, sameExplicitFinalSettlementDate := true,
       notOpenEndedUnwindable := true,
       legallyEnforceableNettingNormalAndDefault := true, settledNetOrSimultaneously := true } : External.SftNettingCriteria).isEligible = true := by decide
example :
    ({ sameCounterparty := true, sameExplicitFinalSettlementDate := true,
       notOpenEndedUnwindable := false,
       legallyEnforceableNettingNormalAndDefault := true, settledNetOrSimultaneously := true } : External.SftNettingCriteria).isEligible = false := by decide
example :
    ({ significantCreditRiskTransferred := true, cleanBreakFromOriginator := true,
       noImplicitOrMoralRecourse := true, armsLengthTerms := true } : External.Aps120CapitalReliefCriteria).isMet = true := by decide


-- ============================================================================
-- 12. Compliance-Soundness Regression Tests (Issues 1–5)
-- ============================================================================

-- Issue 1: Level 1 alone cannot pass `overallCompliant` when `hasLevel2Group = true`.
example :
    let allGovOk : Governance.BoardAndManagementGovernance :=
      { hasBoardApprovedRiskTolerance := true, hasBoardApprovedStrategyAndPolicy := true,
        hasOperatingStandards := true, hasBoardApprovedFundingStrategy := true,
        hasContingencyFundingPlan := true, personnelHaveNecessaryExperience := true,
        frameworkDocumentedAndReviewedAnnually := true, boardReviewsRegularReports := true,
        seniorManagementDutiesMet := true, understandsRiskLinkages := true,
        level2GroupComplianceEnsured := true }
    let fwOk : Governance.LiquidityFramework :=
      { riskToleranceDocumentedAndAppropriate := true, riskToleranceReviewedAnnually := true,
        riskToleranceSupportsProlongedStress := true, riskToleranceArticulatesRiskProfitTradeoff := true,
        setsOutLevel1AndLevel2Structure := true, coversUnsecuredAndSecuredLossEvents := true,
        integratedIntoOverallRiskManagement := true, oversightFunctionOperationallyIndependent := true,
        coversCompositionMaturityDiversityIntradayCurrencies := true,
        includesNormalAndStressQuantQualTargets := true, communicatedThroughoutOrganisation := true,
        immediateEscalationOfEmergingConcerns := true, reviewedOngoingAndUnderCps220 := true,
        considersLegalStructureJurisdictionsAndHomeHost := true,
        businessUnitsUnderstandAndFollowPolicies := true }
    let rmOk : Governance.LiquidityRiskManagement :=
      { projectsCashFlowsAcrossHorizons := true, setsAndReviewsLimitsWithEscalation := true,
        managesCollateralEncumberedVsUnencumbered := true, hasEarlyWarningIndicators := true,
        hasReliableForwardLookingMis := true, managesIntradayLiquidity := true,
        allocatesFundingLiquidityCostsAndBenefits := true, activeInMultipleCurrencies := false,
        multiCurrencyNeedsMatchedAndAnalysed := false }
    let fsOk : Governance.FundingStrategy :=
      { hasThreeYearStrategyDocumented := true, maintainsMarketPresenceAndRelationships := true,
        regularlyGaugesFundRaisingCapacity := true, reviewedAndApprovedByBoardAnnually := true,
        updatedAnnually := true, materialChangesAdvisedToApra := true }
    let cfpOk : Governance.ContingencyFundingPlan :=
      { hasFormalPlanWithInvocationAndEscalation := true, articulatesSourcesAmountsLeadTimes := true,
        integratedWithStressTestsAndIntraday := true, hasRetailDeposits := true,
        addressesRetailRunWithoutClosingChannels := true, reviewedTestedAndBoardApprovedAnnually := true }
    let notifOk : Governance.NotificationStatus :=
      { hasLiquidityConcerns := false, notifiedApraOfConcerns := false,
        experiencingSevereStress := false, notifiedApraImmediatelyOfStress := false,
        intendingToRequestEla := false, notifiedApraPriorToApproachingRba := false,
        hasOperationalCapabilityForElaInfo := false }
    let mlhIn : MLH.MlhInput :=
      { assets := [{ id := "cgs", category := .commonwealthAndSemiGovtSecurities, marketValue := 12 }]
        liabilities := [{ id := "dep", amount := 100 }]
        triggerRatio := pct 10 }
    let res := evaluateAdiCompliance
      { profile := { incorporation := .locallyIncorporated, classification := .mlh }
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
    ({ id := "fcs1", currency := .aud, balance := 400000, isStable := true } : RetailDeposit).outflowAmount = 27500 := by native_decide

example :
    ({ id := "fcs2", currency := .aud, balance := 400000, isStable := true,
       scorecard := { aboveGuaranteeLimitOr250k := false, isOnlineAccount := true,
                      noEstablishedRelationship := false, isHeavilyRateDriven := false } } : RetailDeposit).outflowAmount = 50000 := by native_decide

-- Issue 3: NSFR 10% CLF treatment defaults to 0 cap (1,000 -> 850 fallback RSF, not 100)
example :
    let res := NSFR.evaluateNsfr (pct 100)
      { asfItems := [{ id := "cap", category := .regulatoryCapitalExTier2Lt1Yr, carryingValue := 1000 }]
        rsfAssets := [{ id := "self-sec", category := .selfSecuritisedClfEligible, carryingValue := 1000 }]
        obsExposures := [] }
    res.totalRsf = 850 := by native_decide

-- Issue 4: Para 9(e) currency cap is 0 (not skipped) when `foreignSovCurrencyCaps` is empty
example :
    let op : HqlaOperationalCompliance :=
      { convertibleToCashAnytime := true, unencumberedAndControlled := true,
        controlEvidenced := true, notPledgedOrOperationalCost := true,
        notRehypothecatedIfReceivedInSft := true }
    let (stock, _) := computeStockOfHqla .level1
      [{ id := "p9e", category := .hqla1, currency := .foreign "XYZ", marketValue := 100,
         operational := op, isNonZeroRwForeignSovInForeignCurrency := true }]
      {} none []
    stock = 0 := by native_decide

-- Issue 5: Att A paras 50, 51, 52, 33 & APG called notice deposits
example :
    sumBy (classifyOperationalDeposit "op1" .aud .nonFinancialCorporate 100 50 false) OutflowItem.outflowAmount = 25 + 20 := by native_decide

example :
    sumBy (classifyLiquidityFacilityWithMaturingDebt "lf1" .aud .nonFinancialCorporate 100 30) OutflowItem.outflowAmount = 9 + 7 := by native_decide

example :
    classifyCommittedFacility .hedgeFundOrMmfOrSpv false = .committedLiquidityOtherFinancialInstitutions := by decide

example :
    Table3OutflowCategory.collateralSwapApraMethod.rate = pct 100 := by native_decide

example :
    ({ id := "notice-called", currency := .aud, balance := 100, isStable := true,
       residualMaturityOrNoticeGt30Days := true,
       noWithdrawalRightOrMaterialPenalty := true,
       noticeDepositCalled := true } : RetailDeposit).outflowAmount = 100 := by native_decide

example :
    Table3OutflowCategory.wholesaleNoticeDepositCalled.rate = pct 100 := by native_decide

-- Issue 6: NSFR CLF-eligible assets above limit use their category fallback factor (50% 3rd-party / 85% self-sec default)
example :
    let res := NSFR.evaluateNsfr (pct 100)
      { asfItems := [{ id := "cap", category := .regulatoryCapitalExTier2Lt1Yr, carryingValue := 1000 }]
        rsfAssets :=
          [ { id := "clf3p", category := .clfThirdPartyDebtOrHostAla, carryingValue := 100 }
          , { id := "clfself", category := .selfSecuritisedClfEligible, carryingValue := 100 } ]
        obsExposures := []
        recognisedClfLimit := some 0 }
    res.totalRsf = 50 + 85 := by native_decide

-- Issue 7: `classifyLoanRsf` fixes (CB >=1yr, 35(a)->35(b) fallback, FI <6m HQLA1 10%)
example :
    NSFR.classifyLoanRsf .centralBank .oneYearOrMore 0 false none 0 = .otherLoansNonFiRwLe35Ge1Yr := by decide

example :
    NSFR.classifyLoanRsf .naturalPerson .oneYearOrMore 0 false
      (some { toNaturalPerson := true, isStandardLoanPerAps112 := true,
              lvrPct := 85, residualMaturityOneYearOrMore := true }) 35 =
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
      { convertibleToCashAnytime := true, unencumberedAndControlled := true,
        controlEvidenced := true, notPledgedOrOperationalCost := true,
        notRehypothecatedIfReceivedInSft := true }
    ({ id := "p29", category := .hqla1, currency := .foreign "NZD", marketValue := 100,
       operational := op, freelyTransferableAtLevel2 := false,
       level2TransferableCap := some 40 } : HqlaAsset).valueAfterHaircut .level2 = 40 := by native_decide

-- ============================================================================
-- 13. Newly-Closed Governance, Buffer, Diversification, CLF & External Wiring Tests
-- ============================================================================

-- HQLA2A diversification (para 11) & CLF process/diversification (paras 16–21) & IRB route
example :
    let op : HqlaOperationalCompliance :=
      { convertibleToCashAnytime := true, unencumberedAndControlled := true,
        controlEvidenced := true, notPledgedOrOperationalCost := true,
        notRehypothecatedIfReceivedInSft := true }
    ({ id := "2a-undiv", category := .hqla2a, currency := .aud, marketValue := 100,
       operational := op, hqla2aDiversified := false } : HqlaAsset).valueAfterHaircut .level1 = 0 := by native_decide

example :
    ({ approvedByApra := true, contractualAmount := 50,
       pledgedUnderZeroRunoffSecFunding := 0, receivedUnderZeroInflowSecLending := 0,
       collateralHeldAfterRbaMargins := 50,
       annualReapprovalCurrent := false } : ClfInput).recognisedAmount = 0 := by native_decide

example :
    classifyHqlaCorpDebtByIrbPd true true 5 10 50 true = some .hqla2a ∧
    classifyHqlaCorpDebtByIrbPd true true 30 10 50 true = some .hqla2bCorporateDebt := by decide

-- LCR & NSFR internal buffers (Att A para 4, Att C para 4)
example :
    let op : HqlaOperationalCompliance :=
      { convertibleToCashAnytime := true, unencumberedAndControlled := true,
        controlEvidenced := true, notPledgedOrOperationalCost := true,
        notRehypothecatedIfReceivedInSft := true }
    let res := evaluateLcr .locallyIncorporated false (pct 100)
      { basis := .level1
        hqlaAssets := [{ id := "h1", category := .hqla1, currency := .aud, marketValue := 105, operational := op }]
        retailDeposits := []
        otherOutflows := [{ id := "o1", currency := .aud, category := .nonOperationalOther, amount := 100 }]
        inflows := []
        internalHqlaBuffer := 10 }
    res.ratioCheck.compliant = true ∧ res.bufferMet = false := by native_decide

example :
    let res := NSFR.evaluateNsfr (pct 100)
      { asfItems := [{ id := "cap", category := .regulatoryCapitalExTier2Lt1Yr, carryingValue := 105 }]
        rsfAssets := [{ id := "m", category := .allOtherAssets, carryingValue := 100 }]
        obsExposures := []
        internalNsfrBuffer := 10 }
    res.ratioCheck.compliant = true ∧ res.bufferMet = false := by native_decide

-- NSFR bilateral netting & cash VM eligibility wiring
-- Contract-level bilateral netting failure disables cross-contract netting in `contracts`,
-- while Para 37(b) always applies `MAX(nsfrDerivativeAssets - nsfrDerivativeLiabilities, 0)`
example :
    let ineligibleBna : External.BilateralNettingAgreement :=
      { writtenAndLegallyEnforceable := false, singleLegalObligationCloseOut := true,
        noWalkawayClause := true, reasonedLegalOpinionsInAllJurisdictions := true }
    let d : NSFR.NsfrDerivativeInput :=
      { contracts :=
          [ { nettingSetId := "ns1", mtm := 50, bilateralNetting := some ineligibleBna }
          , { nettingSetId := "ns1", mtm := -30, bilateralNetting := some ineligibleBna } ]
        eligibleCashVariationMarginReceived := 20
        variationMarginPosted := 0
        cashVmReceivedEligibility := some
          { isCash := true, notSegregatedIfNonQccp := false, calculatedAndExchangedDailyMtM := true,
            inContractMnaOrCcpCurrency := true, inSameSettlementCurrencyAsDerivative := true } }
    d.nsfrDerivativeAssets = 50 ∧ d.nsfrDerivativeLiabilities = 30 ∧ d.netDerivativeRsf = 20 := by native_decide

-- Specific LCR breach / MLH shortfall notifications & Securitisation APS 120 wiring
example :
    ({ hasLiquidityConcerns := false, notifiedApraOfConcerns := false,
       experiencingSevereStress := false, notifiedApraImmediatelyOfStress := false,
       intendingToRequestEla := false, notifiedApraPriorToApproachingRba := false,
       hasOperationalCapabilityForElaInfo := false,
       actualOrLikelyLcrBreach := true, notifiedApraOfLcrBreach := false } : Governance.NotificationStatus).isCompliant = false := by decide

example :
    ({ isOriginatingAdi := true, meetsAps120CapitalRelief := true,
       spvExcludedFromMlhAndNsfr := true,
       aps120Criteria := some
         { significantCreditRiskTransferred := false, cleanBreakFromOriginator := true,
           noImplicitOrMoralRecourse := true, armsLengthTerms := true } } : Governance.SecuritisationTreatment).isCompliant = false := by decide

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
      { convertibleToCashAnytime := true, unencumberedAndControlled := true,
        controlEvidenced := true, notPledgedOrOperationalCost := true,
        notRehypothecatedIfReceivedInSft := true }
    ({ id := "grace-31", category := .hqla2a, currency := .aud, marketValue := 100,
       operational := op, issuerRestrictionSatisfied := false,
       recognisedByRegulator := false, becameIneligibleWithin30Days := true,
       daysSinceIneligible := some 31 } : HqlaAsset).valueAfterHaircut .level1 = 0 := by native_decide

example :
    (computeUnwindAdjustments
      [{ hqla1Delta := 25,
         nettingCriteria := some
           { sameCounterparty := true, sameExplicitFinalSettlementDate := true,
             notOpenEndedUnwindable := true,
             legallyEnforceableNettingNormalAndDefault := false,
             settledNetOrSimultaneously := true } }]).deltaHqla1 = 0 := by native_decide

example :
    ({ id := "mlh-3d", category := .commonwealthAndSemiGovtSecurities, marketValue := 100,
       liquidationBusinessDays := some 3 } : MLH.MlhAsset).eligibleValue = 0 := by native_decide

example :
    let op : HqlaOperationalCompliance :=
      { convertibleToCashAnytime := true, unencumberedAndControlled := true,
        controlEvidenced := true, notPledgedOrOperationalCost := true,
        notRehypothecatedIfReceivedInSft := true }
    ({ id := "stress-fail", category := .hqla2a, currency := .aud, marketValue := 100,
       operational := op, max30dPriceDecline := some (pct 11) } : HqlaAsset).valueAfterHaircut .level1 = 0 := by native_decide

-- ============================================================================
-- 14. Regression Tests for Newly-Reported Issues (1–5)
-- ============================================================================

-- 14(1): `evaluateAdiCompliance` preserves LCR CLF (1,000 -> NSFR RSF = 100, not 850) when `recognisedClfLimit := none`
example :
    let op : HqlaOperationalCompliance :=
      { convertibleToCashAnytime := true, unencumberedAndControlled := true,
        controlEvidenced := true, notPledgedOrOperationalCost := true,
        notRehypothecatedIfReceivedInSft := true }
    let clf : ClfInput :=
      { approvedByApra := true, contractualAmount := 1000,
        pledgedUnderZeroRunoffSecFunding := 0, receivedUnderZeroInflowSecLending := 0,
        collateralHeldAfterRbaMargins := 1000, collateralDiversified := true,
        annualReapprovalCurrent := true, includedInFundingStrategy := true,
        balanceSheetEffortsExhausted := true, drawdownAttestedAndNotifiedToApra := true }
    let lcrAud : LcrInput :=
      { basis := .level1, hqlaAssets := [], clf := some clf,
        retailDeposits := [],
        otherOutflows := [{ id := "o1", currency := .aud, category := .nonOperationalOther, amount := 1000 }],
        inflows := [] }
    let lcrAll : LcrInput :=
      { basis := .level1,
        hqlaAssets := [{ id := "h1", category := .hqla1, currency := .aud, marketValue := 1000, operational := op }],
        retailDeposits := [],
        otherOutflows := [{ id := "o1", currency := .aud, category := .nonOperationalOther, amount := 1000 }],
        inflows := [] }
    let nsfrIn : NSFR.NsfrInput :=
      { asfItems := [{ id := "cap", category := .regulatoryCapitalExTier2Lt1Yr, carryingValue := 200 }],
        rsfAssets := [{ id := "self-sec", category := .selfSecuritisedClfEligible, carryingValue := 1000 }],
        obsExposures := [] }
    let govOk : Governance.BoardAndManagementGovernance :=
      { hasBoardApprovedRiskTolerance := true, hasBoardApprovedStrategyAndPolicy := true,
        hasOperatingStandards := true, hasBoardApprovedFundingStrategy := true,
        hasContingencyFundingPlan := true, personnelHaveNecessaryExperience := true,
        frameworkDocumentedAndReviewedAnnually := true, boardReviewsRegularReports := true,
        seniorManagementDutiesMet := true, understandsRiskLinkages := true,
        level2GroupComplianceEnsured := true }
    let fwOk : Governance.LiquidityFramework :=
      { riskToleranceDocumentedAndAppropriate := true, riskToleranceReviewedAnnually := true,
        riskToleranceSupportsProlongedStress := true, riskToleranceArticulatesRiskProfitTradeoff := true,
        setsOutLevel1AndLevel2Structure := true, coversUnsecuredAndSecuredLossEvents := true,
        integratedIntoOverallRiskManagement := true, oversightFunctionOperationallyIndependent := true,
        coversCompositionMaturityDiversityIntradayCurrencies := true,
        includesNormalAndStressQuantQualTargets := true, communicatedThroughoutOrganisation := true,
        immediateEscalationOfEmergingConcerns := true, reviewedOngoingAndUnderCps220 := true,
        considersLegalStructureJurisdictionsAndHomeHost := true,
        businessUnitsUnderstandAndFollowPolicies := true }
    let rmOk : Governance.LiquidityRiskManagement :=
      { projectsCashFlowsAcrossHorizons := true, setsAndReviewsLimitsWithEscalation := true,
        managesCollateralEncumberedVsUnencumbered := true, hasEarlyWarningIndicators := true,
        hasReliableForwardLookingMis := true, managesIntradayLiquidity := true,
        allocatesFundingLiquidityCostsAndBenefits := true, activeInMultipleCurrencies := false,
        multiCurrencyNeedsMatchedAndAnalysed := false }
    let fsOk : Governance.FundingStrategy :=
      { hasThreeYearStrategyDocumented := true, maintainsMarketPresenceAndRelationships := true,
        regularlyGaugesFundRaisingCapacity := true, reviewedAndApprovedByBoardAnnually := true,
        updatedAnnually := true, materialChangesAdvisedToApra := true }
    let cfpOk : Governance.ContingencyFundingPlan :=
      { hasFormalPlanWithInvocationAndEscalation := true, articulatesSourcesAmountsLeadTimes := true,
        integratedWithStressTestsAndIntraday := true, hasRetailDeposits := true,
        addressesRetailRunWithoutClosingChannels := true, reviewedTestedAndBoardApprovedAnnually := true }
    let notifOk : Governance.NotificationStatus :=
      { hasLiquidityConcerns := false, notifiedApraOfConcerns := false,
        experiencingSevereStress := false, notifiedApraImmediatelyOfStress := false,
        intendingToRequestEla := false, notifiedApraPriorToApproachingRba := false,
        hasOperationalCapabilityForElaInfo := false }
    let stOk : Governance.StressTesting :=
      { conductsRegularShortAndProtractedTests := true,
        coversInstitutionSpecificMarketWideCombined := true,
        outcomesAdjustStrategyAndCfp := true, analysesLevel1Level2AndBusinessLines := true,
        documentedAndReportedToBoardAndApra := true,
        resultsFeedStrategicPlanningAndLimits := true }
    let res := evaluateAdiCompliance
      { profile := { incorporation := .locallyIncorporated, classification := .lcr },
        boardAndManagement := govOk, framework := fwOk, riskManagement := rmOk,
        fundingStrategy := fsOk, contingencyFundingPlan := cfpOk, notifications := notifOk,
        stressTesting := some stOk, hasLevel2Group := false,
        lcrAudInput := some lcrAud, lcrAllCurrenciesInput := some lcrAll, nsfrInput := some nsfrIn,
        apraAdjustmentsComplied := true, bufferPolicy := some .enforceAllBuffers,
        notifiedApraOfRelianceOrPriorDiscretion := true }
    res.overallCompliant = true ∧
    (res.nsfrResult.map (·.totalRsf) = some 100) := by native_decide

-- 14(2): Excess operational balance from `.otherLegalEntity` gets 100%, not 40%
example :
    sumBy (classifyOperationalDeposit "op-ole" .aud .otherLegalEntity 100 50 false) OutflowItem.outflowAmount = 25 + 50 := by native_decide

-- 14(3): NSFR CLF allocation is order-independent (greedy by fallback factor descending)
example :
    let a : NSFR.RsfAssetItem := { id := "a", category := .clfThirdPartyDebtOrHostAla, carryingValue := 100, excessClfFallbackFactor := some (pct 50) }
    let b : NSFR.RsfAssetItem := { id := "b", category := .clfThirdPartyDebtOrHostAla, carryingValue := 100, excessClfFallbackFactor := some (pct 85) }
    let r1 := NSFR.evaluateNsfr (pct 100) { asfItems := [], rsfAssets := [a, b], obsExposures := [], recognisedClfLimit := some 100 }
    let r2 := NSFR.evaluateNsfr (pct 100) { asfItems := [], rsfAssets := [b, a], obsExposures := [], recognisedClfLimit := some 100 }
    r1.totalRsf = 10 + 50 ∧ r1.totalRsf = r2.totalRsf := by native_decide

-- 14(4): Omitting `hqla2aDiversified` fails closed (0), and `ClfInput` process flags fail closed (0)
example :
    let op : HqlaOperationalCompliance :=
      { convertibleToCashAnytime := true, unencumberedAndControlled := true,
        controlEvidenced := true, notPledgedOrOperationalCost := true,
        notRehypothecatedIfReceivedInSft := true }
    ({ id := "2a-default", category := .hqla2a, currency := .aud, marketValue := 100, operational := op } : HqlaAsset).valueAfterHaircut .level1 = 0 := by native_decide

-- 14(5): `RetailDeposit.runoffRate` agrees with `outflowAmount` on $400k deposit (27500/400000 = 11/160),
--        and `classifyHqla2bRmbs` rejects pools with structured products (12(a)(v))
example :
    let d : RetailDeposit := { id := "fcs", currency := .aud, balance := 400000, isStable := true }
    d.runoffRate = (11 : Rat) / 160 ∧
    d.outflowAmount = nonNeg d.balance * d.runoffRate := by native_decide

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
    sumBy (classifyOperationalDeposit "op-sme" .aud (.sme 500000 true) 100 50 false) OutflowItem.outflowAmount = 25 + 5 := by native_decide

-- Fix: Intermediated non-look-through term deposit fails exclusion when category is tainted
example :
    ({ id := "int-taint", currency := .aud, balance := 100, isStable := false,
       isIntermediated := true, intermediatedLookThroughMet := false,
       residualMaturityOrNoticeGt30Days := true, noWithdrawalRightOrMaterialPenalty := true,
       categoryTaintedByEarlyWithdrawal := true } : RetailDeposit).outflowAmount = 100 := by native_decide

-- Fix: Para 32(a) excess defaults to 50% (not 85%) and host ALA works independently of CLF = 0
example :
    let a : NSFR.RsfAssetItem := { id := "a", category := .clfThirdPartyDebtOrHostAla, carryingValue := 100 }
    let rNoLim := NSFR.evaluateNsfr (pct 100) { asfItems := [], rsfAssets := [a], obsExposures := [] }
    let rHostAla := NSFR.evaluateNsfr (pct 100) { asfItems := [], rsfAssets := [a], obsExposures := [], hostAlaInLcrLimit := 100 }
    rNoLim.totalRsf = 50 ∧ rHostAla.totalRsf = 10 := by native_decide

-- Fix: NSFR SFT netting per Attachment C para 26 & APS 110 Att D para 38
example :
    let openRepo : External.SftNettingCriteria :=
      { sameCounterparty := true, sameExplicitFinalSettlementDate := false,
        notOpenEndedUnwindable := false,
        legallyEnforceableNettingNormalAndDefault := true, settledNetOrSimultaneously := true }
    let termRepo : External.SftNettingCriteria :=
      { sameCounterparty := true, sameExplicitFinalSettlementDate := true,
        notOpenEndedUnwindable := true,
        legallyEnforceableNettingNormalAndDefault := true, settledNetOrSimultaneously := true }
    ({ receivable := 100, payable := 60, nettingCriteria := openRepo } : NSFR.NsfrSftNettingGroup).netNsfrReceivable = 100 ∧
    ({ receivable := 100, payable := 60, nettingCriteria := termRepo } : NSFR.NsfrSftNettingGroup).netNsfrReceivable = 40 := by native_decide

-- Fix: Conditional defaults (non-drawn CLF, solo ADI without Level 2, no APRA adjustments/prior discretion)
example :
    ({ approvedByApra := true, contractualAmount := 50,
       pledgedUnderZeroRunoffSecFunding := 0, receivedUnderZeroInflowSecLending := 0,
       collateralHeldAfterRbaMargins := 50, collateralDiversified := true,
       annualReapprovalCurrent := true, includedInFundingStrategy := true,
       balanceSheetEffortsExhausted := true } : ClfInput).recognisedAmount = 50 := by native_decide

-- CashFlows: NSFR liability & amortising loan maturity splitting (Att C Paras 8, 11(c), 22)
example :
    let liabAsf := sumBy
      (splitLiabilityCashFlowsForAsf "bond" .otherFundingLt6m .otherFunding6mToLt1Yr .borrowingsAndLiabilitiesGe1Yr
        [(100, 90), (200, 240), (300, 400)])
      NSFR.AsfItem.weightedAmount
    let loanRsf := sumBy
      (splitAmortisingLoanForRsf "loan" .otherNonHqlaLt1Yr .otherPerformingLoansNonFiGe1Yr
        [(100, 180), (400, 730)])
      NSFR.RsfAssetItem.weightedAmount
    -- ASF = 100*0 + 200*0.5 + 300*1 = 400; RSF = 100*0.5 + 400*0.85 = 390
    liabAsf = 400 ∧ loanRsf = 390 := by native_decide

-- CashFlows: Multi-horizon cash-flow ladder & survival buffer (Main Body Para 36)
example :
    let flows : List ContractualCashFlow :=
      [ { id := "out1", currency := .aud, direction := .outflow, amount := 60, contractualDays := 5 }
      , { id := "in1", currency := .aud, direction := .inflow, amount := 40, contractualDays := 20 }
      , { id := "out2", currency := .aud, direction := .outflow, amount := 50, contractualDays := 90 } ]
    hasPositiveSurvivalBuffer 70 flows = true ∧
    hasPositiveSurvivalBuffer 60 flows = false := by native_decide

-- CashFlows: LCR derivative MNA + same-day FX netting & SPV/Para 60/Para 31 filtering
example :
    let flows : List ContractualCashFlow :=
      [ { id := "spv-out", currency := .aud, direction := .outflow, amount := 50, contractualDays := 15, source := .securitisationSpv }
      , { id := "open-min", currency := .aud, direction := .inflow, amount := 20, contractualDays := 10, isOpenMaturity := true, isMinimumContractualPayment := true }
      , { id := "open-full", currency := .aud, direction := .inflow, amount := 999, contractualDays := 10, isOpenMaturity := true, isMinimumContractualPayment := false }
      , { id := "ext-late", currency := .aud, direction := .inflow, amount := 999, contractualDays := 20, latestExtensionDays := some 40 }
      , { id := "hqla-in", currency := .aud, direction := .inflow, amount := 999, contractualDays := 10, countedInHqla := true } ]
    let derivLegs : List LcrDerivativeCashFlowLeg :=
      [ { counterparty := "cp1", currency := .aud, hasValidMna := true, payable := 80, receivable := 30 }
      , { counterparty := "cp2", currency := .aud, hasValidMna := false, isSameDayFxPrincipalExchange := true, payable := 20, receivable := 50 } ]
    let (outs, ins) := deriveLcrFromCashFlows flows (fun _ => .nonOperationalOther) (fun _ => .otherWholesaleEntity) derivLegs
    sumBy outs OutflowItem.outflowAmount = 50 + 50 ∧
    sumBy ins InflowItem.inflowAmount = 10 + 30 := by native_decide

end APS210.Tests
