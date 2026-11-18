import APS210.Basic
import APS210.Governance
import APS210.LCR
import APS210.MLH
import APS210.NSFR

namespace APS210

/-- Complete quantitative and qualitative compliance input for an ADI under APS 210. -/
structure AdiComplianceInput where
  profile : Governance.AdiProfile
  boardAndManagement : Governance.BoardAndManagementGovernance
  framework : Governance.LiquidityFramework
  riskManagement : Governance.LiquidityRiskManagement
  fundingStrategy : Governance.FundingStrategy
  contingencyFundingPlan : Governance.ContingencyFundingPlan
  notifications : Governance.NotificationStatus
  stressTesting : Option Governance.StressTesting := none
  localOperationalCapacity : Option Governance.LocalOperationalCapacity := none
  securitisation : Option Governance.SecuritisationTreatment := none
  /-- Attachment A, Paragraph 28: LCRs of material offshore banking subsidiaries (each must be >= 100%). -/
  offshoreMaterialBankingSubsidiaryLcrs : List (String × Rate) := []
  /-- Main Body Paragraph 3 & Attachment A Paragraph 26: Whether the ADI is a member of a Level 2 group
      (when true, compliance must hold on BOTH Level 1 and Level 2 bases). -/
  hasLevel2Group : Bool := true
  lcrAudInput : Option LCR.LcrInput := none
  lcrAllCurrenciesInput : Option LCR.LcrInput := none
  lcrAudLevel2Input : Option LCR.LcrInput := none
  lcrAllCurrenciesLevel2Input : Option LCR.LcrInput := none
  mlhInput : Option MLH.MlhInput := none
  mlhLevel2Input : Option MLH.MlhInput := none
  nsfrInput : Option NSFR.NsfrInput := none
  nsfrLevel2Input : Option NSFR.NsfrInput := none
  /-- Paragraph 54–55: Financial stress exception (LCR/MLH may fall below minimum
      during stress provided APRA is notified immediately). -/
  inFinancialStress : Bool := false
  /-- Paragraph 71: ADI complies with any APRA-adjusted or excluded requirement. -/
  apraAdjustmentsComplied : Bool := false
  /-- Paragraph 72: ADI has contacted APRA if seeking to rely on prior APRA discretions. -/
  notifiedApraOfRelianceOrPriorDiscretion : Bool := false
  deriving DecidableEq, Repr

/-- Complete compliance assessment output for an ADI under APS 210. -/
structure AdiComplianceResult where
  qualitativeCompliant : Bool
  quantitativeCompliant : Bool
  overallCompliant : Bool
  allBuffersMet : Bool := true
  mlhAboveTrigger : Option Bool := none
  lcrAudResult : Option LCR.LcrResult := none
  lcrAllCurrenciesResult : Option LCR.LcrResult := none
  lcrAudLevel2Result : Option LCR.LcrResult := none
  lcrAllCurrenciesLevel2Result : Option LCR.LcrResult := none
  mlhResult : Option MLH.MlhResult := none
  mlhLevel2Result : Option MLH.MlhResult := none
  nsfrResult : Option NSFR.NsfrResult := none
  nsfrLevel2Result : Option NSFR.NsfrResult := none
  deriving DecidableEq, Repr

private def withClfLimit (lcrRes : Option LCR.LcrResult) (nsfrIn : NSFR.NsfrInput) : NSFR.NsfrInput :=
  let clfFromLcr := match lcrRes with
    | some r => r.recognisedClf
    | none => 0
  let effectiveLim := match nsfrIn.recognisedClfLimit with
    | some explicitLim => min clfFromLcr (nonNeg explicitLim)
    | none => clfFromLcr
  { nsfrIn with recognisedClfLimit := some effectiveLim }

/-- Evaluate full APS 210 compliance for an ADI across qualitative and quantitative requirements. -/
def evaluateAdiCompliance (input : AdiComplianceInput) : AdiComplianceResult :=
  let baseGovOk :=
    input.boardAndManagement.isCompliant &&
    input.framework.isCompliant &&
    input.riskManagement.isCompliant &&
    input.fundingStrategy.isCompliant &&
    input.contingencyFundingPlan.isCompliant &&
    input.notifications.isCompliant
  let stressOk := match input.profile.classification with
    | .lcr => match input.stressTesting with
      | some st => st.isCompliant
      | none => false
    | .mlh => true
  let locOk := match input.profile.incorporation with
    | .foreign => match input.localOperationalCapacity with
      | some loc => loc.isCompliant
      | none => false
    | .locallyIncorporated => true
  let qualOk := baseGovOk && stressOk && locOk

  let lcrAudRes := match input.profile.requiredAudLcr, input.lcrAudInput with
    | some minRate, some lcrIn =>
      if lcrIn.basis == .level1 then some (LCR.evaluateLcr input.profile.incorporation true minRate lcrIn)
      else none
    | _, _ => none
  let lcrAllRes := match input.profile.requiredAllCurrenciesLcr, input.lcrAllCurrenciesInput with
    | some minRate, some lcrIn =>
      if lcrIn.basis == .level1 then some (LCR.evaluateLcr input.profile.incorporation false minRate lcrIn)
      else none
    | _, _ => none
  let lcrAudL2Res := if !input.hasLevel2Group then none else
    match input.profile.requiredAudLcr, input.lcrAudLevel2Input with
    | some minRate, some lcrIn =>
      if lcrIn.basis == .level2 then some (LCR.evaluateLcr input.profile.incorporation true minRate lcrIn)
      else none
    | _, _ => none
  let lcrAllL2Res := if !input.hasLevel2Group then none else
    match input.profile.requiredAllCurrenciesLcr, input.lcrAllCurrenciesLevel2Input with
    | some minRate, some lcrIn =>
      if lcrIn.basis == .level2 then some (LCR.evaluateLcr input.profile.incorporation false minRate lcrIn)
      else none
    | _, _ => none
  let mlhRes := match input.profile.requiredMlh, input.mlhInput with
    | some minRate, some mlhIn =>
      some (MLH.evaluateMlh minRate mlhIn)
    | _, _ => none
  let mlhL2Res := if !input.hasLevel2Group then none else
    match input.profile.requiredMlh, input.mlhLevel2Input with
    | some minRate, some mlhIn =>
      some (MLH.evaluateMlh minRate mlhIn)
    | _, _ => none
  let nsfrRes := match input.profile.requiredNsfr, input.nsfrInput with
    | some minRate, some nsfrIn =>
      some (NSFR.evaluateNsfr minRate (withClfLimit lcrAudRes nsfrIn))
    | _, _ => none
  let nsfrL2Res := if !input.hasLevel2Group then none else
    match input.profile.requiredNsfr, input.nsfrLevel2Input with
    | some minRate, some nsfrIn =>
      some (NSFR.evaluateNsfr minRate (withClfLimit lcrAudL2Res nsfrIn))
    | _, _ => none

  let stressBreachPermitted :=
    input.inFinancialStress &&
    input.notifications.experiencingSevereStress &&
    input.notifications.notifiedApraImmediatelyOfStress

  let lcrAudOk := match input.profile.requiredAudLcr, lcrAudRes with
    | none, _ => true
    | some _, some r => r.ratioCheck.compliant || stressBreachPermitted
    | some _, none => false
  let lcrAllOk := match input.profile.requiredAllCurrenciesLcr, lcrAllRes with
    | none, _ => true
    | some _, some r => r.ratioCheck.compliant || stressBreachPermitted
    | some _, none => false
  let lcrAudL2Ok := if !input.hasLevel2Group then true else
    match input.profile.requiredAudLcr, lcrAudL2Res with
    | none, _ => true
    | some _, some r => r.ratioCheck.compliant || stressBreachPermitted
    | some _, none => false
  let lcrAllL2Ok := if !input.hasLevel2Group then true else
    match input.profile.requiredAllCurrenciesLcr, lcrAllL2Res with
    | none, _ => true
    | some _, some r => r.ratioCheck.compliant || stressBreachPermitted
    | some _, none => false
  let mlhOk := match input.profile.requiredMlh, mlhRes with
    | none, _ => true
    | some _, some r => r.triggerRatioValid && (r.ratioCheck.compliant || stressBreachPermitted)
    | some _, none => false
  let mlhL2Ok := if !input.hasLevel2Group then true else
    match input.profile.requiredMlh, mlhL2Res with
    | none, _ => true
    | some _, some r => r.triggerRatioValid && (r.ratioCheck.compliant || stressBreachPermitted)
    | some _, none => false
  let nsfrOk := match input.profile.requiredNsfr, nsfrRes with
    | none, _ => true
    | some _, some r => r.ratioCheck.compliant
    | some _, none => false
  let nsfrL2Ok := if !input.hasLevel2Group then true else
    match input.profile.requiredNsfr, nsfrL2Res with
    | none, _ => true
    | some _, some r => r.ratioCheck.compliant
    | some _, none => false
  let allBuffersMet :=
    (lcrAudRes.map (·.bufferMet) |>.getD true) &&
    (lcrAllRes.map (·.bufferMet) |>.getD true) &&
    (lcrAudL2Res.map (·.bufferMet) |>.getD true) &&
    (lcrAllL2Res.map (·.bufferMet) |>.getD true) &&
    (nsfrRes.map (·.bufferMet) |>.getD true) &&
    (nsfrL2Res.map (·.bufferMet) |>.getD true)
  let offshoreSubOk := input.offshoreMaterialBankingSubsidiaryLcrs.all (fun (_, r) => r >= pct 100)
  let secOk := match input.securitisation with
    | some s => s.isCompliant
    | none => true
  let apraDiscretionOk :=
    input.apraAdjustmentsComplied && input.notifiedApraOfRelianceOrPriorDiscretion

  let quantOk :=
    lcrAudOk && lcrAllOk && lcrAudL2Ok && lcrAllL2Ok &&
    mlhOk && mlhL2Ok && nsfrOk && nsfrL2Ok &&
    offshoreSubOk && secOk && apraDiscretionOk
  { qualitativeCompliant := qualOk
    quantitativeCompliant := quantOk
    overallCompliant := qualOk && quantOk
    allBuffersMet
    mlhAboveTrigger := mlhRes.map (·.aboveTriggerRatio)
    lcrAudResult := lcrAudRes
    lcrAllCurrenciesResult := lcrAllRes
    lcrAudLevel2Result := lcrAudL2Res
    lcrAllCurrenciesLevel2Result := lcrAllL2Res
    mlhResult := mlhRes
    mlhLevel2Result := mlhL2Res
    nsfrResult := nsfrRes
    nsfrLevel2Result := nsfrL2Res }

/-- Paragraph 54 & Paragraph 60: Continuous-basis ("at all times") compliance across
    a non-empty sequence of reporting/monitoring snapshots. -/
def evaluateContinuousCompliance (snapshots : List AdiComplianceInput) : Bool :=
  !snapshots.isEmpty && snapshots.all (fun s => (evaluateAdiCompliance s).overallCompliant)

end APS210
