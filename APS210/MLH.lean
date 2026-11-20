import APS210.Basic
import APS210.Facts

namespace APS210.MLH

open APS210.Facts

/-- Attachment B, Paragraph 2(a)–(g): Eligible MLH liquid asset categories. -/
inductive MlhAssetCategory where
  | notesAndCoinAndSettlementFunds
  | commonwealthAndSemiGovtSecurities
  | govtGuaranteedDebtSecurities
  | supranationalAndForeignGovtDebt
  | adiBankBillsCdsAndDebt
  | netDepositsWithOtherAdisTwoBusinessDays
  | apraApprovedOtherSecurities
  deriving DecidableEq, Repr

/-- Whether the category is a debt security subject to Attachment B Paragraph 3
    (must be RBA repo-eligible and non-subordinated). -/
def MlhAssetCategory.isDebtSecurity : MlhAssetCategory → Bool
  | .commonwealthAndSemiGovtSecurities => true
  | .govtGuaranteedDebtSecurities => true
  | .supranationalAndForeignGovtDebt => true
  | .adiBankBillsCdsAndDebt => true
  | .apraApprovedOtherSecurities => true
  | .notesAndCoinAndSettlementFunds => false
  | .netDepositsWithOtherAdisTwoBusinessDays => false

/-- An MLH liquid asset holding (Attachment B, Paragraphs 2–4). -/
structure MlhAsset where
  id : String
  category : MlhAssetCategory
  marketValue : Money
  /-- Para 2: Free from encumbrances, or encumbered with APRA prudential approval. -/
  unencumberedOrApraApproved : Bool := false
  /-- Para 3: Eligible for repurchase agreement with the RBA (required for debt securities). -/
  rbaRepoEligible : Bool := false
  /-- Para 3: Non-subordinated (required for debt securities). -/
  nonSubordinated : Bool := false
  /-- Para 2(g) & 3: For `apraApprovedOtherSecurities`, whether the security is a debt security
      (if true, para 3 RBA repo-eligibility and non-subordination apply). -/
  isDebtSecurity : Bool := true
  /-- Para 4: Operational capacity to liquidate within two business days. -/
  liquidatableWithinTwoBusinessDays : Bool := false
  /-- Para 4: Optional numeric liquidation horizon in business days (must be <= 2 when supplied). -/
  liquidationBusinessDays : Option Nat := none
  /-- Para 2(g): Explicit APRA approval for `apraApprovedOtherSecurities` (defaults true for
      other categories; must be true when `category == .apraApprovedOtherSecurities`). -/
  apraApproved : Bool := true
  observation : ObservationContext
  deriving DecidableEq, Repr

def MlhAsset.requiresDebtCriteria (a : MlhAsset) : Bool :=
  match a.category with
  | .apraApprovedOtherSecurities => a.isDebtSecurity
  | c => c.isDebtSecurity

def MlhAsset.isEligible (a : MlhAsset) : Bool :=
  let debtOk := !a.requiresDebtCriteria || (a.rbaRepoEligible && a.nonSubordinated)
  let daysOk := match a.liquidationBusinessDays with
    | some d => d <= 2
    | none => true
  let apraOk := a.category != .apraApprovedOtherSecurities || a.apraApproved
  a.unencumberedOrApraApproved && debtOk && a.liquidatableWithinTwoBusinessDays && daysOk && apraOk

def MlhAsset.eligibleValue (a : MlhAsset) : Money :=
  if a.isEligible then nonNeg a.marketValue else 0

/-- Attachment B, Paragraph 1: MLH liability base item. -/
structure MlhLiabilityItem where
  id : String
  isOnBalanceSheetOrIrrevocableCommitment : Bool := true
  apraApprovedExclusion : Bool := false
  amount : Money
  observation : ObservationContext
  deriving DecidableEq, Repr

def MlhLiabilityItem.includedAmount (l : MlhLiabilityItem) : Money :=
  if l.isOnBalanceSheetOrIrrevocableCommitment && !l.apraApprovedExclusion then
    nonNeg l.amount
  else
    0

/-- Inputs for Attachment B MLH calculation. -/
structure MlhInput where
  /-- Reporting date (`validAt`) and data cut (`knownAt`) this calculation is for. -/
  asOf : ObservationContext
  assets : List MlhAsset
  /-- Para 2(f): Placements by other ADIs to be netted against deposits held with other ADIs. -/
  placementsByOtherAdis : Money := 0
  liabilities : List MlhLiabilityItem
  /-- Para 5: ADI's internal trigger ratio (must be explicitly set and strictly above `requiredMinRate`). -/
  triggerRatio : Rate
  deriving DecidableEq, Repr

/-- Output of Attachment B MLH evaluation. -/
structure MlhResult where
  eligibleLiquidAssets : Money
  totalLiabilities : Money
  ratioCheck : RatioCheck
  triggerRatioValid : Bool
  aboveTriggerRatio : Bool
  dataIssues : List DataIssue
  deriving DecidableEq, Repr

/-- Compute MLH ratio and check compliance against `requiredMinRate` (default 9%, para 57).
    Implements Paragraph 2(f) netting of placements by other ADIs against 2(f) deposits. -/
def evaluateMlh
    (requiredMinRate : Rate := pct 9)
    (input : MlhInput) : MlhResult :=
  let non2fAssets := sumBy
    (input.assets.filter (·.category != .netDepositsWithOtherAdisTwoBusinessDays))
    MlhAsset.eligibleValue
  let gross2fDeposits := sumBy
    (input.assets.filter (·.category == .netDepositsWithOtherAdisTwoBusinessDays))
    MlhAsset.eligibleValue
  let net2fDeposits := nonNeg (gross2fDeposits - nonNeg input.placementsByOtherAdis)
  let totalLiquid := non2fAssets + net2fDeposits
  let totalLiab := sumBy input.liabilities MlhLiabilityItem.includedAmount
  let rc := checkRatio totalLiquid totalLiab requiredMinRate
  let triggerValid := input.triggerRatio > requiredMinRate
  let aboveTrigger :=
    if totalLiab <= 0 then totalLiquid > 0
    else totalLiquid >= input.triggerRatio * totalLiab
  { eligibleLiquidAssets := totalLiquid
    totalLiabilities := totalLiab
    ratioCheck := rc
    triggerRatioValid := triggerValid
    aboveTriggerRatio := aboveTrigger
    dataIssues :=
      (input.assets.flatMap fun a => observationIssues input.asOf a.id a.observation) ++
      (input.liabilities.flatMap fun l => observationIssues input.asOf l.id l.observation) }

end APS210.MLH
