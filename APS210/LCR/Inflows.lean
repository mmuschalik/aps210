import APS210.Basic
import APS210.Facts

namespace APS210.LCR

open APS210.Facts

/-- Attachment A, Paragraph 55–56, Table 4 – Maturing secured lending collateral categories. -/
inductive SecuredLendingCollateral where
  | hqla1
  | clfEligibleWithinLimit
  | hqla2a
  | hqla2bRmbs
  | otherHqla2b
  | marginLendingNonHqlaNonClf
  | allOtherCollateral
  deriving DecidableEq, Repr

/-- Attachment A, Table 4: Inflow rate for maturing secured lending (reverse repos / securities borrowing).
    If `usedToCoverShortPositions = true` (para 56), the inflow rate is always 0%. -/
def securedLendingInflowRate
    (collateral : SecuredLendingCollateral)
    (usedToCoverShortPositions : Bool) : Rate :=
  if usedToCoverShortPositions then pct 0
  else match collateral with
    | .hqla1 => pct 0
    | .clfEligibleWithinLimit => pct 0
    | .hqla2a => pct 15
    | .hqla2bRmbs => pct 25
    | .otherHqla2b => pct 50
    | .marginLendingNonHqlaNonClf => pct 50
    | .allOtherCollateral => pct 100

/-- Attachment A, Paragraphs 57–66: All cash inflow categories. -/
inductive InflowCategory where
  | securedLending (collateral : SecuredLendingCollateral) (usedToCoverShortPositions : Bool)
  | linesOfCreditHeldAtOtherInstitutions
  | retailAndSme
  | financialInstitutionAndCentralBank
  | otherWholesaleEntity
  | maturingNonHqlaSecurities
  | operationalDepositsAtOtherFis
  | cooperativeNetworkCentralisedDeposits
  | derivativesCashInflows
  | otherContractualInflows (isNonFinancialRevenue : Bool)
  deriving DecidableEq, Repr

def InflowCategory.rate : InflowCategory → Rate
  | .securedLending col usedForShort => securedLendingInflowRate col usedForShort
  | .linesOfCreditHeldAtOtherInstitutions => pct 0
  | .retailAndSme => pct 50
  | .financialInstitutionAndCentralBank => pct 100
  | .otherWholesaleEntity => pct 50
  | .maturingNonHqlaSecurities => pct 100
  | .operationalDepositsAtOtherFis => pct 0
  | .cooperativeNetworkCentralisedDeposits => pct 0
  | .derivativesCashInflows => pct 100
  | .otherContractualInflows isNonFinRev => if isNonFinRev then pct 0 else pct 100

/-- A contractual cash inflow item within the 30-day LCR horizon. -/
structure InflowItem where
  id : String
  currency : Currency
  category : InflowCategory
  amount : Money
  /-- Para 54: Must be fully performing with no expected default within 30 days.
      Only an affirmative reading counts; anything else contributes no inflow. -/
  isFullyPerforming : Reading Bool
  /-- Para 31: Must not double-count assets already included in the stock of HQLA. -/
  countedInHqla : Bool
  observation : ObservationContext
  deriving DecidableEq, Repr

def InflowItem.inflowAmount (item : InflowItem) : Money :=
  if item.isFullyPerforming.affirmative && !item.countedInHqla then
    nonNeg item.amount * item.category.rate
  else
    0

def InflowItem.dataIssues (item : InflowItem) : List DataIssue :=
  readingIssues item.id "isFullyPerforming" "treated as not fully performing; no inflow" item.isFullyPerforming

/-- Attachment A, Paragraphs 30 & 54: Compute gross inflows, capped inflows (75% of outflows,
    or stricter APRA concentration cap), and total net cash outflows. -/
structure NetCashOutflowsResult where
  totalExpectedCashOutflows : Money
  totalExpectedCashInflows : Money
  inflowCap75Pct : Money
  effectiveCappedInflows : Money
  totalNetCashOutflows : Money
  deriving DecidableEq, Repr

def computeNetCashOutflows
    (totalOutflows : Money)
    (inflows : List InflowItem)
    (apraInflowCapOverride : Option Money := none) : NetCashOutflowsResult :=
  let grossOut := nonNeg totalOutflows
  let grossIn := sumBy inflows InflowItem.inflowAmount
  let cap75 := pct 75 * grossOut
  let effectiveCap := match apraInflowCapOverride with
    | some c => min cap75 (nonNeg c)
    | none => cap75
  let cappedIn := min grossIn effectiveCap
  let netOut := nonNeg (grossOut - cappedIn)
  { totalExpectedCashOutflows := grossOut
    totalExpectedCashInflows := grossIn
    inflowCap75Pct := cap75
    effectiveCappedInflows := cappedIn
    totalNetCashOutflows := netOut }

end APS210.LCR
