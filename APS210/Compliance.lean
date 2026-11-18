import APS210.Basic
import APS210.Governance
import APS210.LCR
import APS210.MLH
import APS210.NSFR
import APS210.CashFlows

namespace APS210

/-- Explicit buffer enforcement policy required for quantitative compliance (Attachment A / Attachment C). -/
inductive BufferPolicy where
  | enforceAllBuffers
  | waivedDuringStress
  | customBufferFloor (lcrAudFloor lcrAllFloor nsfrFloor : Rate)
  deriving DecidableEq, Repr

/-- Paragraph 71: Executable APRA requirement adjustments and exclusions applied to ratio minimums. -/
inductive ApraAdjustmentRule where
  | overrideMinLcrAud (newMinRate : Rate)
  | overrideMinLcrAll (newMinRate : Rate)
  | overrideMinMlh (newMinRate : Rate)
  | overrideMinNsfr (newMinRate : Rate)
  | excludeLcrAud
  | excludeLcrAll
  | excludeMlh
  | excludeNsfr
  deriving DecidableEq, Repr

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
  /-- Paragraph 71: Whether APRA has adjusted or excluded a specific requirement for this ADI. -/
  hasApraAdjustmentOrExclusion : Bool := false
  /-- Paragraph 71: ADI complies with any APRA-adjusted or excluded requirement. -/
  apraAdjustmentsComplied : Bool := false
  /-- Paragraph 71: Executable APRA adjustments/exclusions applied to requirement minimums. -/
  apraAdjustments : List ApraAdjustmentRule := []
  /-- Explicit buffer enforcement policy (required: `none` fails quantitative compliance). -/
  bufferPolicy : Option BufferPolicy := none
  /-- Paragraph 72: Whether the ADI seeks to rely on a prior APRA exemption or discretion. -/
  seeksRelianceOnPriorDiscretion : Bool := false
  /-- Paragraph 72: ADI has contacted APRA if seeking to rely on prior APRA discretions. -/
  notifiedApraOfRelianceOrPriorDiscretion : Bool := false
  /-- Paragraph 36: Optional multi-horizon contractual cash flows and liquidity buffer
      for cash-flow projection survival check. -/
  scheduledCashFlows : List CashFlows.ContractualCashFlow := []
  cashFlowSurvivalBuffer : Option Money := none
  deriving DecidableEq, Repr

/-- Complete compliance assessment output for an ADI under APS 210. -/
structure AdiComplianceResult where
  qualitativeCompliant : Bool
  quantitativeCompliant : Bool
  overallCompliant : Bool
  allBuffersMet : Bool := true
  mlhAboveTrigger : Option Bool := none
  cashFlowLadder : List CashFlows.CashFlowLadderRow := []
  cashFlowSurvivalMet : Bool := true
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
    input.boardAndManagement.isCompliantFor input.hasLevel2Group &&
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

  let effReqLcrAud := input.apraAdjustments.foldl (fun acc r => match r with
    | .overrideMinLcrAud m => some m | .excludeLcrAud => none | _ => acc) input.profile.requiredAudLcr
  let effReqLcrAll := input.apraAdjustments.foldl (fun acc r => match r with
    | .overrideMinLcrAll m => some m | .excludeLcrAll => none | _ => acc) input.profile.requiredAllCurrenciesLcr
  let effReqMlh := input.apraAdjustments.foldl (fun acc r => match r with
    | .overrideMinMlh m => some m | .excludeMlh => none | _ => acc) input.profile.requiredMlh
  let effReqNsfr := input.apraAdjustments.foldl (fun acc r => match r with
    | .overrideMinNsfr m => some m | .excludeNsfr => none | _ => acc) input.profile.requiredNsfr

  let lcrAudRes := match effReqLcrAud, input.lcrAudInput with
    | some minRate, some lcrIn =>
      if lcrIn.basis == .level1 then some (LCR.evaluateLcr input.profile.incorporation true minRate lcrIn)
      else none
    | _, _ => none
  let lcrAllRes := match effReqLcrAll, input.lcrAllCurrenciesInput with
    | some minRate, some lcrIn =>
      if lcrIn.basis == .level1 then some (LCR.evaluateLcr input.profile.incorporation false minRate lcrIn)
      else none
    | _, _ => none
  let lcrAudL2Res := if !input.hasLevel2Group then none else
    match effReqLcrAud, input.lcrAudLevel2Input with
    | some minRate, some lcrIn =>
      if lcrIn.basis == .level2 then some (LCR.evaluateLcr input.profile.incorporation true minRate lcrIn)
      else none
    | _, _ => none
  let lcrAllL2Res := if !input.hasLevel2Group then none else
    match effReqLcrAll, input.lcrAllCurrenciesLevel2Input with
    | some minRate, some lcrIn =>
      if lcrIn.basis == .level2 then some (LCR.evaluateLcr input.profile.incorporation false minRate lcrIn)
      else none
    | _, _ => none
  let mlhRes := match effReqMlh, input.mlhInput with
    | some minRate, some mlhIn =>
      some (MLH.evaluateMlh minRate mlhIn)
    | _, _ => none
  let mlhL2Res := if !input.hasLevel2Group then none else
    match effReqMlh, input.mlhLevel2Input with
    | some minRate, some mlhIn =>
      some (MLH.evaluateMlh minRate mlhIn)
    | _, _ => none
  let nsfrRes := match effReqNsfr, input.nsfrInput with
    | some minRate, some nsfrIn =>
      some (NSFR.evaluateNsfr minRate (withClfLimit lcrAudRes nsfrIn))
    | _, _ => none
  let nsfrL2Res := if !input.hasLevel2Group then none else
    match effReqNsfr, input.nsfrLevel2Input with
    | some minRate, some nsfrIn =>
      some (NSFR.evaluateNsfr minRate (withClfLimit lcrAudL2Res nsfrIn))
    | _, _ => none

  let stressBreachPermitted :=
    input.inFinancialStress &&
    input.notifications.experiencingSevereStress &&
    input.notifications.notifiedApraImmediatelyOfStress

  let lcrAudOk := match effReqLcrAud, lcrAudRes with
    | none, _ => true
    | some _, some r => r.ratioCheck.compliant || stressBreachPermitted
    | some _, none => false
  let lcrAllOk := match effReqLcrAll, lcrAllRes with
    | none, _ => true
    | some _, some r => r.ratioCheck.compliant || stressBreachPermitted
    | some _, none => false
  let lcrAudL2Ok := if !input.hasLevel2Group then true else
    match effReqLcrAud, lcrAudL2Res with
    | none, _ => true
    | some _, some r => r.ratioCheck.compliant || stressBreachPermitted
    | some _, none => false
  let lcrAllL2Ok := if !input.hasLevel2Group then true else
    match effReqLcrAll, lcrAllL2Res with
    | none, _ => true
    | some _, some r => r.ratioCheck.compliant || stressBreachPermitted
    | some _, none => false
  let mlhOk := match effReqMlh, mlhRes with
    | none, _ => true
    | some _, some r => r.triggerRatioValid && r.ratioCheck.compliant
    | some _, none => false
  let mlhL2Ok := if !input.hasLevel2Group then true else
    match effReqMlh, mlhL2Res with
    | none, _ => true
    | some _, some r => r.triggerRatioValid && r.ratioCheck.compliant
    | some _, none => false
  let nsfrOk := match effReqNsfr, nsfrRes with
    | none, _ => true
    | some _, some r => r.ratioCheck.compliant
    | some _, none => false
  let nsfrL2Ok := if !input.hasLevel2Group then true else
    match effReqNsfr, nsfrL2Res with
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
  let meetsFloor (req : Option Rate) (res : Option RatioCheck) (floor : Rate) : Bool :=
    match req, res with
    | some m, some rc =>
      match rc.ratio with
      | some r => decide (r ≥ m + floor)
      | none => true
    | some _, none => false
    | none, _ => true
  let bufferPolicyOk := match input.bufferPolicy with
    | none => false
    | some .enforceAllBuffers => allBuffersMet
    | some .waivedDuringStress => allBuffersMet || stressBreachPermitted
    | some (.customBufferFloor lAud lAll nFloor) =>
      meetsFloor effReqLcrAud (lcrAudRes.map (·.ratioCheck)) lAud &&
      meetsFloor effReqLcrAll (lcrAllRes.map (·.ratioCheck)) lAll &&
      meetsFloor effReqNsfr (nsfrRes.map (·.ratioCheck)) nFloor &&
      (!input.hasLevel2Group || (
        meetsFloor effReqLcrAud (lcrAudL2Res.map (·.ratioCheck)) lAud &&
        meetsFloor effReqLcrAll (lcrAllL2Res.map (·.ratioCheck)) lAll &&
        meetsFloor effReqNsfr (nsfrL2Res.map (·.ratioCheck)) nFloor))
  let offshoreSubOk := input.offshoreMaterialBankingSubsidiaryLcrs.all (fun (_, r) => r >= pct 100)
  let secOk := match input.securitisation with
    | some s => s.isCompliant
    | none => true
  let apraDiscretionOk :=
    (!input.hasApraAdjustmentOrExclusion || !input.apraAdjustments.isEmpty || input.apraAdjustmentsComplied) &&
    (!input.seeksRelianceOnPriorDiscretion || input.notifiedApraOfRelianceOrPriorDiscretion)
  let ladder :=
    if input.scheduledCashFlows.isEmpty then []
    else CashFlows.projectCashFlowLadder input.scheduledCashFlows
  let cfSurvivalOk := match input.cashFlowSurvivalBuffer with
    | some buf => CashFlows.hasPositiveSurvivalBuffer buf input.scheduledCashFlows
    | none => true

  let quantOk :=
    lcrAudOk && lcrAllOk && lcrAudL2Ok && lcrAllL2Ok &&
    mlhOk && mlhL2Ok && nsfrOk && nsfrL2Ok &&
    bufferPolicyOk && offshoreSubOk && secOk && apraDiscretionOk && cfSurvivalOk
  { qualitativeCompliant := qualOk
    quantitativeCompliant := quantOk
    overallCompliant := qualOk && quantOk
    allBuffersMet
    mlhAboveTrigger := mlhRes.map (·.aboveTriggerRatio)
    cashFlowLadder := ladder
    cashFlowSurvivalMet := cfSurvivalOk
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
