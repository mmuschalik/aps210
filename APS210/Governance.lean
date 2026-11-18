import APS210.Basic
import APS210.ExternalStandards

namespace APS210.Governance

/-- Paragraph 5: Securitisation SPV treatment.
    If an ADI meets APS 120 regulatory capital relief operational requirements,
    it may exclude the SPV's assets and liabilities from MLH and NSFR,
    but must always include SPV cash flows in LCR. -/
structure SecuritisationTreatment where
  isOriginatingAdi : Bool
  meetsAps120CapitalRelief : Bool
  /-- Optional structured APS 120 operational criteria (overrides/refines `meetsAps120CapitalRelief` when supplied). -/
  aps120Criteria : Option External.Aps120CapitalReliefCriteria := none
  /-- Para 5: Whether the ADI is excluding securitisation SPVs from MLH/NSFR. -/
  spvExcludedFromMlhAndNsfr : Bool := false
  /-- Para 5: LCR must include cash flows from SPVs related to the ADI's securitisation activities. -/
  lcrIncludesSpvCashFlows : Bool := true
  deriving DecidableEq, Repr

def SecuritisationTreatment.mayExcludeFromMlhAndNsfr (s : SecuritisationTreatment) : Bool :=
  let reliefMet := match s.aps120Criteria with
    | some c => c.isMet
    | none => s.meetsAps120CapitalRelief
  s.isOriginatingAdi && reliefMet

def SecuritisationTreatment.isLcrCompliant (s : SecuritisationTreatment) : Bool :=
  !s.isOriginatingAdi || s.lcrIncludesSpvCashFlows

def SecuritisationTreatment.isCompliant (s : SecuritisationTreatment) : Bool :=
  s.isLcrCompliant && (!s.spvExcludedFromMlhAndNsfr || s.mayExcludeFromMlhAndNsfr)

/-- Paragraphs 15–20: Board (or senior officer outside Australia for foreign ADI)
    and senior management governance attestations. -/
structure BoardAndManagementGovernance where
  /-- Para 16(a): Board-approved liquidity risk tolerance statement. -/
  hasBoardApprovedRiskTolerance : Bool
  /-- Para 16(b): Board-approved liquidity management strategy and policy. -/
  hasBoardApprovedStrategyAndPolicy : Bool
  /-- Para 16(c): Operating standards (policies, procedures, controls). -/
  hasOperatingStandards : Bool
  /-- Para 16(d): Board-approved funding strategy. -/
  hasBoardApprovedFundingStrategy : Bool
  /-- Para 16(e): Contingency funding plan. -/
  hasContingencyFundingPlan : Bool
  /-- Para 17(a): Senior management and relevant personnel have necessary experience. -/
  personnelHaveNecessaryExperience : Bool
  /-- Para 17(b): Framework and practices documented and reviewed at least annually. -/
  frameworkDocumentedAndReviewedAnnually : Bool
  /-- Para 18: Board reviews regular liquidity reports and emerging risks. -/
  boardReviewsRegularReports : Bool
  /-- Para 19(a)–(i): Senior management core duties satisfied. -/
  seniorManagementDutiesMet : Bool
  /-- Para 20: Board and senior management demonstrate understanding of funding vs market liquidity
      and linkages to credit, market, operational and reputation risks. -/
  understandsRiskLinkages : Bool
  /-- Para 4: Holding company / authorised NOHC ensures Level 2 group compliance where applicable. -/
  level2GroupComplianceEnsured : Bool := false
  deriving DecidableEq, Repr

def BoardAndManagementGovernance.isCompliantFor
    (g : BoardAndManagementGovernance) (level2Applicable : Bool := true) : Bool :=
  g.hasBoardApprovedRiskTolerance &&
  g.hasBoardApprovedStrategyAndPolicy &&
  g.hasOperatingStandards &&
  g.hasBoardApprovedFundingStrategy &&
  g.hasContingencyFundingPlan &&
  g.personnelHaveNecessaryExperience &&
  g.frameworkDocumentedAndReviewedAnnually &&
  g.boardReviewsRegularReports &&
  g.seniorManagementDutiesMet &&
  g.understandsRiskLinkages &&
  (!level2Applicable || g.level2GroupComplianceEnsured)

def BoardAndManagementGovernance.isCompliant (g : BoardAndManagementGovernance) : Bool :=
  g.isCompliantFor true

/-- Paragraphs 21–35: Liquidity risk management framework requirements. -/
structure LiquidityFramework where
  riskToleranceDocumentedAndAppropriate : Bool
  riskToleranceReviewedAnnually : Bool
  riskToleranceSupportsProlongedStress : Bool
  riskToleranceArticulatesRiskProfitTradeoff : Bool
  setsOutLevel1AndLevel2Structure : Bool
  coversUnsecuredAndSecuredLossEvents : Bool
  integratedIntoOverallRiskManagement : Bool
  oversightFunctionOperationallyIndependent : Bool
  coversCompositionMaturityDiversityIntradayCurrencies : Bool
  includesNormalAndStressQuantQualTargets : Bool
  communicatedThroughoutOrganisation : Bool
  immediateEscalationOfEmergingConcerns : Bool
  reviewedOngoingAndUnderCps220 : Bool
  /-- Para 31: Strategy considers legal structure, jurisdictions, and home/host rules. -/
  considersLegalStructureJurisdictionsAndHomeHost : Bool := false
  /-- Para 34: Business units understand and follow the liquidity risk management policies. -/
  businessUnitsUnderstandAndFollowPolicies : Bool := false
  deriving DecidableEq, Repr

def LiquidityFramework.isCompliant (f : LiquidityFramework) : Bool :=
  f.riskToleranceDocumentedAndAppropriate &&
  f.riskToleranceReviewedAnnually &&
  f.riskToleranceSupportsProlongedStress &&
  f.riskToleranceArticulatesRiskProfitTradeoff &&
  f.setsOutLevel1AndLevel2Structure &&
  f.coversUnsecuredAndSecuredLossEvents &&
  f.integratedIntoOverallRiskManagement &&
  f.oversightFunctionOperationallyIndependent &&
  f.coversCompositionMaturityDiversityIntradayCurrencies &&
  f.includesNormalAndStressQuantQualTargets &&
  f.communicatedThroughoutOrganisation &&
  f.immediateEscalationOfEmergingConcerns &&
  f.reviewedOngoingAndUnderCps220 &&
  f.considersLegalStructureJurisdictionsAndHomeHost &&
  f.businessUnitsUnderstandAndFollowPolicies

/-- Paragraphs 36–43: Ongoing management of liquidity risk. -/
structure LiquidityRiskManagement where
  projectsCashFlowsAcrossHorizons : Bool
  setsAndReviewsLimitsWithEscalation : Bool
  managesCollateralEncumberedVsUnencumbered : Bool
  hasEarlyWarningIndicators : Bool
  hasReliableForwardLookingMis : Bool
  managesIntradayLiquidity : Bool
  allocatesFundingLiquidityCostsAndBenefits : Bool
  activeInMultipleCurrencies : Bool
  multiCurrencyNeedsMatchedAndAnalysed : Bool
  deriving DecidableEq, Repr

def LiquidityRiskManagement.isCompliant (m : LiquidityRiskManagement) : Bool :=
  m.projectsCashFlowsAcrossHorizons &&
  m.setsAndReviewsLimitsWithEscalation &&
  m.managesCollateralEncumberedVsUnencumbered &&
  m.hasEarlyWarningIndicators &&
  m.hasReliableForwardLookingMis &&
  m.managesIntradayLiquidity &&
  m.allocatesFundingLiquidityCostsAndBenefits &&
  (!m.activeInMultipleCurrencies || m.multiCurrencyNeedsMatchedAndAnalysed)

/-- Paragraphs 44–46: Funding strategy requirements. -/
structure FundingStrategy where
  hasThreeYearStrategyDocumented : Bool
  maintainsMarketPresenceAndRelationships : Bool
  regularlyGaugesFundRaisingCapacity : Bool
  reviewedAndApprovedByBoardAnnually : Bool
  updatedAnnually : Bool
  materialChangesAdvisedToApra : Bool
  deriving DecidableEq, Repr

def FundingStrategy.isCompliant (fs : FundingStrategy) : Bool :=
  fs.hasThreeYearStrategyDocumented &&
  fs.maintainsMarketPresenceAndRelationships &&
  fs.regularlyGaugesFundRaisingCapacity &&
  fs.reviewedAndApprovedByBoardAnnually &&
  fs.updatedAnnually &&
  fs.materialChangesAdvisedToApra

/-- Paragraphs 47–51: Contingency funding plan requirements. -/
structure ContingencyFundingPlan where
  hasFormalPlanWithInvocationAndEscalation : Bool
  articulatesSourcesAmountsLeadTimes : Bool
  integratedWithStressTestsAndIntraday : Bool
  hasRetailDeposits : Bool
  addressesRetailRunWithoutClosingChannels : Bool
  reviewedTestedAndBoardApprovedAnnually : Bool
  deriving DecidableEq, Repr

def ContingencyFundingPlan.isCompliant (cfp : ContingencyFundingPlan) : Bool :=
  cfp.hasFormalPlanWithInvocationAndEscalation &&
  cfp.articulatesSourcesAmountsLeadTimes &&
  cfp.integratedWithStressTestsAndIntraday &&
  (!cfp.hasRetailDeposits || cfp.addressesRetailRunWithoutClosingChannels) &&
  cfp.reviewedTestedAndBoardApprovedAnnually

/-- Paragraphs 62–66: Stress testing (required for LCR ADIs). -/
structure StressTesting where
  conductsRegularShortAndProtractedTests : Bool
  coversInstitutionSpecificMarketWideCombined : Bool
  outcomesAdjustStrategyAndCfp : Bool
  analysesLevel1Level2AndBusinessLines : Bool
  documentedAndReportedToBoardAndApra : Bool
  /-- Para 65: Stress-test results feed into strategic planning and internal limits. -/
  resultsFeedStrategicPlanningAndLimits : Bool := false
  deriving DecidableEq, Repr

def StressTesting.isCompliant (st : StressTesting) : Bool :=
  st.conductsRegularShortAndProtractedTests &&
  st.coversInstitutionSpecificMarketWideCombined &&
  st.outcomesAdjustStrategyAndCfp &&
  st.analysesLevel1Level2AndBusinessLines &&
  st.documentedAndReportedToBoardAndApra &&
  st.resultsFeedStrategicPlanningAndLimits

/-- Paragraphs 67–69: Local operational capacity (required for Foreign ADIs). -/
structure LocalOperationalCapacity where
  assessedAtLeastAnnually : Bool
  offshoreOutageMinBusinessDays : Nat
  /-- Para 68: Scenario explicitly covers time zones and public holidays. -/
  coversTimeZonesAndPublicHolidays : Bool := false
  deriving DecidableEq, Repr

/-- Paragraph 68: Must consider at least 3 business days without offshore staff assistance,
    accounting for time zones and public holidays. -/
def LocalOperationalCapacity.isCompliant (loc : LocalOperationalCapacity) : Bool :=
  loc.assessedAtLeastAnnually &&
  loc.offshoreOutageMinBusinessDays >= 3 &&
  loc.coversTimeZonesAndPublicHolidays

/-- Main Body Paragraph 14, Main Body Paragraph 70, Attachment A para 5, Attachment B para 6:
    Notification duties (concerns, LCR breach, MLH shortfall, severe stress, ELA). -/
structure NotificationStatus where
  hasLiquidityConcerns : Bool
  notifiedApraOfConcerns : Bool
  experiencingSevereStress : Bool
  notifiedApraImmediatelyOfStress : Bool
  intendingToRequestEla : Bool
  notifiedApraPriorToApproachingRba : Bool
  hasOperationalCapabilityForElaInfo : Bool
  /-- Attachment A para 5: Actual or likely breach of LCR requirement. -/
  actualOrLikelyLcrBreach : Bool := false
  notifiedApraOfLcrBreach : Bool := false
  /-- Attachment B para 6: Actual or likely shortfall below MLH requirement. -/
  actualOrLikelyMlhShortfall : Bool := false
  notifiedApraImmediatelyOfMlhShortfall : Bool := false
  deriving DecidableEq, Repr

def NotificationStatus.isCompliant (n : NotificationStatus) : Bool :=
  (!n.hasLiquidityConcerns || n.notifiedApraOfConcerns) &&
  (!n.experiencingSevereStress || n.notifiedApraImmediatelyOfStress) &&
  (!n.intendingToRequestEla || (n.notifiedApraPriorToApproachingRba && n.hasOperationalCapabilityForElaInfo)) &&
  (!n.actualOrLikelyLcrBreach || n.notifiedApraOfLcrBreach) &&
  (!n.actualOrLikelyMlhShortfall || n.notifiedApraImmediatelyOfMlhShortfall)

/-- Paragraphs 52–61: Quantitative minimum thresholds for an ADI,
    accounting for incorporation, APRA classification, and APRA-imposed higher minimums. -/
structure AdiProfile where
  incorporation : AdiIncorporation
  /-- For locally incorporated ADI, APRA determines LCR vs MLH (para 52).
      For foreign ADI, default is LCR unless APRA determines MLH (para 53). -/
  classification : AdiClassification
  /-- Para 56: Optional APRA-imposed higher minimum LCR (AUD / All currencies). -/
  apraHigherMinLcrAud : Option Rate := none
  apraHigherMinLcrAll : Option Rate := none
  /-- Para 58: Optional APRA-imposed higher minimum MLH. -/
  apraHigherMinMlh : Option Rate := none
  /-- Para 61: Optional APRA-imposed higher minimum NSFR. -/
  apraHigherMinNsfr : Option Rate := none
  deriving DecidableEq, Repr

/-- Paragraph 55(a)–(b), 56: Minimum required AUD LCR (only applicable to locally incorporated LCR ADIs). -/
def AdiProfile.requiredAudLcr (p : AdiProfile) : Option Rate :=
  match p.incorporation, p.classification with
  | .locallyIncorporated, .lcr =>
    some (max (pct 100) (p.apraHigherMinLcrAud.getD 0))
  | _, _ => none

/-- Paragraph 55(a)–(b), 56: Minimum required All-Currencies LCR. -/
def AdiProfile.requiredAllCurrenciesLcr (p : AdiProfile) : Option Rate :=
  match p.incorporation, p.classification with
  | .locallyIncorporated, .lcr =>
    some (max (pct 100) (p.apraHigherMinLcrAll.getD 0))
  | .foreign, .lcr =>
    some (max (pct 40) (p.apraHigherMinLcrAll.getD 0))
  | _, .mlh => none

/-- Paragraph 57–58: Minimum required MLH ratio (9% default). -/
def AdiProfile.requiredMlh (p : AdiProfile) : Option Rate :=
  match p.classification with
  | .mlh => some (max (pct 9) (p.apraHigherMinMlh.getD 0))
  | .lcr => none

/-- Paragraph 59–61: Minimum required NSFR (100% default, locally incorporated LCR ADIs only). -/
def AdiProfile.requiredNsfr (p : AdiProfile) : Option Rate :=
  match p.incorporation, p.classification with
  | .locallyIncorporated, .lcr =>
    some (max (pct 100) (p.apraHigherMinNsfr.getD 0))
  | _, _ => none

end APS210.Governance
