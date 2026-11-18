import APS210.Basic
import APS210.ExternalStandards
import APS210.LCR.HQLA

namespace APS210.Upstream

/-- Days since 1970-01-01 (proleptic Gregorian calendar). -/
abbrev EpochDate := Int

/-- Convert Gregorian `(year, month, day)` to `EpochDate` (Howard Hinnant algorithm). -/
def civilDate (y : Int) (m d : Nat) : EpochDate :=
  let mInt : Int := m
  let yAdj : Int := if mInt ≤ 2 then y - 1 else y
  let era : Int := (if yAdj ≥ 0 then yAdj else yAdj - 399) / 400
  let yoe : Int := yAdj - era * 400
  let mAdj : Int := if mInt > 2 then mInt - 3 else mInt + 9
  let doy : Int := (153 * mAdj + 2) / 5 + (d : Int) - 1
  let doe : Int := yoe * 365 + yoe / 4 - yoe / 100 + doy
  era * 146097 + doe - 719468

-- ============================================================================
-- 1. Maturity & Encumbrance Bucketing (Attachment A / Attachment C)
-- ============================================================================

inductive NsfrBucket where
  | lt6m
  | sixMTo1y
  | ge1y
  deriving DecidableEq, Repr

structure PositionRow where
  id : String
  amount : Money
  maturityDate : EpochDate
  earliestCallDate : EpochDate
  hasCallOption : Bool
  latestExtensionDate : EpochDate := 0
  hasExtensionOption : Bool := false
  noticeDays : Rat
  encumbranceEndDate : EpochDate
  isEncumbered : Bool
  deriving DecidableEq, Repr

def PositionRow.effectiveMaturityDate (p : PositionRow) : EpochDate :=
  if p.hasCallOption then p.earliestCallDate
  else if p.hasExtensionOption then p.latestExtensionDate
  else p.maturityDate

def PositionRow.residualDays (reportingDate : EpochDate) (p : PositionRow) : Rat :=
  max p.noticeDays ((p.effectiveMaturityDate - reportingDate : Int) : Rat)

def PositionRow.encumbranceDays (reportingDate : EpochDate) (p : PositionRow) : Rat :=
  if p.isEncumbered then nonNeg ((p.encumbranceEndDate - reportingDate : Int) : Rat) else 0

def classifyNsfrBucket (days : Rat) : NsfrBucket :=
  if days < 180 then .lt6m
  else if days < 365 then .sixMTo1y
  else .ge1y

-- ============================================================================
-- 2. Derivative MNA Netting (Attachment A / Attachment C)
-- ============================================================================

structure DerivativeTrade where
  tradeId : String
  mnaId : String
  hasEligibleMna : Bool
  mtmPayable : Money
  mtmReceivable : Money
  deriving DecidableEq, Repr

private def insertGroup {α : Type} (key : String) (x : α) :
    List (String × List α) → List (String × List α)
  | [] => [(key, [x])]
  | (k, xs) :: rest =>
    if k == key then (k, xs ++ [x]) :: rest
    else (k, xs) :: insertGroup key x rest

private def groupByKey {α : Type} (keyFn : α → String) (xs : List α) : List (String × List α) :=
  xs.foldl (fun acc x => insertGroup (keyFn x) x acc) []

def netDerivativeOutflowInflow (trades : List DerivativeTrade) : Money × Money :=
  let eligible := trades.filter (·.hasEligibleMna)
  let nonMna := trades.filter (fun t => !t.hasEligibleMna)
  let mnaGroups := groupByKey (·.mnaId) eligible
  let mnaOut := sumBy mnaGroups (fun (_, grp) =>
    let netMtm := sumBy grp (fun t => t.mtmPayable - t.mtmReceivable)
    nonNeg netMtm)
  let mnaIn := sumBy mnaGroups (fun (_, grp) =>
    let netMtm := sumBy grp (fun t => t.mtmPayable - t.mtmReceivable)
    nonNeg (-netMtm))
  let grossOut := sumBy nonMna (fun t => nonNeg t.mtmPayable)
  let grossIn := sumBy nonMna (fun t => nonNeg t.mtmReceivable)
  (mnaOut + grossOut, mnaIn + grossIn)

-- ============================================================================
-- 3. Customer-Group $250k FCS / $2m SME & Att A Para 42 Pool Tainting
-- ============================================================================

inductive DepositCustomerType where
  | retail
  | sme
  | wholesale
  deriving DecidableEq, Repr

structure DepositAccount where
  accountId : String
  customerGroupId : String
  customerType : DepositCustomerType
  productCategory : String
  balance : Money
  hasEarlyWithdrawalNoPenalty : Bool
  residualDays : Rat
  deriving DecidableEq, Repr

structure DepositClassificationSummary where
  smeEligibleDeposits : Money
  fcsInsuredDeposits : Money
  excludedTermDeposits : Money
  deriving DecidableEq, Repr

def evaluateDeposits (accounts : List DepositAccount) : DepositClassificationSummary :=
  let custTotals := groupByKey (·.customerGroupId) accounts |>.map (fun (cg, grp) =>
    (cg, sumBy grp (·.balance)))
  let poolTaints := groupByKey (·.productCategory) accounts |>.map (fun (pc, grp) =>
    (pc, grp.any (·.hasEarlyWithdrawalNoPenalty)))
  let lookupTotal (cg : String) : Money :=
    (custTotals.find? (fun (k, _) => k == cg)).map (·.2) |>.getD 0
  let lookupTaint (pc : String) : Bool :=
    (poolTaints.find? (fun (k, _) => k == pc)).map (·.2) |>.getD false
  let isSmeEligible (a : DepositAccount) : Bool :=
    a.customerType == .sme && lookupTotal a.customerGroupId < 2000000
  let fcsInsuredShare (a : DepositAccount) : Money :=
    if a.customerType == .retail || isSmeEligible a then
      let tot := lookupTotal a.customerGroupId
      if tot == 0 then 0
      else min a.balance (a.balance * min tot 250000 / tot)
    else 0
  let isExcludedTerm (a : DepositAccount) : Bool :=
    a.residualDays > 30 && !(lookupTaint a.productCategory)
  { smeEligibleDeposits := sumBy (accounts.filter isSmeEligible) (·.balance)
  , fcsInsuredDeposits := sumBy accounts fcsInsuredShare
  , excludedTermDeposits := sumBy (accounts.filter isExcludedTerm) (·.balance)
  }

-- ============================================================================
-- 4. LCR Att A Para 9(e) Foreign Sovereign HQLA1 Per-Currency Cap
-- ============================================================================

structure CcyHqla1Position where
  currency : String
  hqla1ForeignSovAmt : Money
  ccyNetOutflow : Money
  deriving DecidableEq, Repr

def cappedForeignSovHqla1 (positions : List CcyHqla1Position) : Money :=
  let byCcy := groupByKey (·.currency) positions
  sumBy byCcy (fun (_, grp) =>
    let totalHqla1 := sumBy grp (·.hqla1ForeignSovAmt)
    let totalOutflow := sumBy grp (·.ccyNetOutflow)
    min totalHqla1 (nonNeg totalOutflow))

-- ============================================================================
-- 5. Att A Table 3 Row 11(d) 24-Month Rolling 30-Day Collateral Lookback
-- ============================================================================

structure DailyCollateralFlow where
  valuationDate : EpochDate
  netCollateralFlow : Money
  deriving DecidableEq, Repr

private def absRat (x : Rat) : Rat := if x < 0 then -x else x

def max30dCollateralOutflow (flows : List DailyCollateralFlow) : Money :=
  let sorted := flows.mergeSort (fun a b => a.valuationDate ≤ b.valuationDate)
  let n := sorted.length
  let rollingAbs := (List.range n).map (fun i =>
    let lo := i - 29
    let frame := (sorted.drop lo).take (i - lo + 1)
    absRat (sumBy frame (·.netCollateralFlow)))
  rollingAbs.foldl max 0

-- ============================================================================
-- 6. Att A Para 50 12-Month Monthly Average Net Cash Outflows
-- ============================================================================

structure MonthlyNetOutflow where
  monthEndDate : EpochDate
  netCashOutflow : Money
  deriving DecidableEq, Repr

def avg12mNetOutflow (months : List MonthlyNetOutflow) : Money :=
  if months.isEmpty then 0
  else sumBy months (·.netCashOutflow) / (months.length : Rat)

-- ============================================================================
-- 7. ECAI Letter-to-Grade Mapping & Multiple Assessment Resolution (APS 112 Att F)
-- ============================================================================

structure EcaiAssessmentRow where
  exposureId : String
  ratingStr : String
  deriving DecidableEq, Repr

def parseCreditRatingStr : String → CreditRating
  | "AAA" => .aaa
  | "AA+" => .aaPlus
  | "AA" => .aa
  | "AA-" => .aaMinus
  | "A+" => .aPlus
  | "A" => .a
  | "A-" => .aMinus
  | "BBB+" => .bbbPlus
  | "BBB" => .bbb
  | "BBB-" => .bbbMinus
  | "below_bbb_minus" => .belowBbbMinus
  | _ => .unrated

def resolveEcaiByExposure (rows : List EcaiAssessmentRow) : List (String × External.CreditRatingGrade) :=
  let rated := rows.filter (fun r => External.CreditRatingGrade.ofCreditRating (parseCreditRatingStr r.ratingStr) != .unrated)
  let groups := groupByKey (·.exposureId) rated
  groups.map (fun (eid, grp) =>
    let grades := grp.map (fun r => External.CreditRatingGrade.ofCreditRating (parseCreditRatingStr r.ratingStr))
    (eid, External.resolveMultipleEcaiAssessments grades))

-- ============================================================================
-- 8. Amortising Cash-Flow Schedules (Attachment C NSFR Bucketing)
-- ============================================================================

structure AmortisationCashflowRow where
  loanId : String
  paymentDate : EpochDate
  principalAmount : Money
  deriving DecidableEq, Repr

-- ============================================================================
-- 9. LCR SFT Unwinds & 3-Way CLF Bound (Attachment A Paras 8, 15-21)
-- ============================================================================

structure SftLegRow where
  legId : String
  meetsOperationalReqs : Bool
  nettingEligible : Bool
  hqla1Delta : Money
  hqla2aDelta : Money
  hqla2bDelta : Money
  deriving DecidableEq, Repr

def SftLegRow.toLcrLeg (s : SftLegRow) : LCR.ShortTermSftLeg :=
  { hqla1Delta := s.hqla1Delta
  , hqla2aDelta := s.hqla2aDelta
  , hqla2bDelta := s.hqla2bDelta
  , meetsOperationalReqs := s.meetsOperationalReqs
  , nettingCriteria := some
      { sameCounterparty := s.nettingEligible
      , sameExplicitFinalSettlementDate := s.nettingEligible
      , legallyEnforceableNettingNormalAndDefault := s.nettingEligible
      , settledNetOrSimultaneously := s.nettingEligible
      }
  }

structure ClfSummaryInput where
  eligible : Bool := false
  contractualAmount : Money := 0
  pledgedZeroRunoff : Money := 0
  receivedZeroInflow : Money := 0
  collateralAfterMargin : Money := 0
  deriving DecidableEq, Repr

def ClfSummaryInput.recognised (c : ClfSummaryInput) : Money :=
  let clf : LCR.ClfInput :=
    { approvedByApra := c.eligible
    , contractualAmount := c.contractualAmount
    , pledgedUnderZeroRunoffSecFunding := c.pledgedZeroRunoff
    , receivedUnderZeroInflowSecLending := c.receivedZeroInflow
    , collateralHeldAfterRbaMargins := c.collateralAfterMargin
    , collateralDiversified := c.eligible
    , annualReapprovalCurrent := c.eligible
    , includedInFundingStrategy := c.eligible
    , balanceSheetEffortsExhausted := c.eligible
    }
  clf.recognisedAmount

-- ============================================================================
-- Unified Upstream Evaluation & Compliance Properties
-- ============================================================================

structure UpstreamInput where
  reportingDate : EpochDate
  positions : List PositionRow
  derivativeTrades : List DerivativeTrade
  depositAccounts : List DepositAccount
  ccyHqla1Positions : List CcyHqla1Position
  dailyCollateralFlows : List DailyCollateralFlow
  monthlyNetOutflows : List MonthlyNetOutflow
  ecaiAssessments : List EcaiAssessmentRow := []
  amortisationCashflows : List AmortisationCashflowRow := []
  sftLegs : List SftLegRow := []
  clf : ClfSummaryInput := {}
  deriving DecidableEq, Repr

structure UpstreamResult where
  posLcrWithin30dAmt : Money
  posNsfrLt6mAmt : Money
  posNsfr6mTo1yAmt : Money
  posNsfrGe1yAmt : Money
  posEncGe1yAmt : Money
  derivNetOutflow : Money
  derivNetInflow : Money
  smeEligibleDeposits : Money
  fcsInsuredDeposits : Money
  excludedTermDeposits : Money
  cappedForeignSovHqla1 : Money
  max30dCollateralOutflow : Money
  avg12mNetOutflow : Money
  ecaiResolvedGradeSum : Rat
  ecaiInvestmentGradeCount : Rat
  amortNsfrLt6mAmt : Money
  amortNsfr6mTo1yAmt : Money
  amortNsfrGe1yAmt : Money
  sftUnwindHqla1 : Money
  sftUnwindHqla2a : Money
  sftUnwindHqla2b : Money
  clfRecognisedAmount : Money
  deriving DecidableEq, Repr

def evaluateUpstream (input : UpstreamInput) : UpstreamResult :=
  let lcr30d := sumBy (input.positions.filter (fun p => p.residualDays input.reportingDate ≤ 30)) (·.amount)
  let nsfrLt6m := sumBy (input.positions.filter (fun p => classifyNsfrBucket (p.residualDays input.reportingDate) == .lt6m)) (·.amount)
  let nsfr6mTo1y := sumBy (input.positions.filter (fun p => classifyNsfrBucket (p.residualDays input.reportingDate) == .sixMTo1y)) (·.amount)
  let nsfrGe1y := sumBy (input.positions.filter (fun p => classifyNsfrBucket (p.residualDays input.reportingDate) == .ge1y)) (·.amount)
  let encGe1y := sumBy (input.positions.filter (fun p => p.isEncumbered && p.encumbranceDays input.reportingDate ≥ 365)) (·.amount)
  let (dOut, dIn) := netDerivativeOutflowInflow input.derivativeTrades
  let dep := evaluateDeposits input.depositAccounts
  let ecaiResolved := resolveEcaiByExposure input.ecaiAssessments
  let amortDays (c : AmortisationCashflowRow) : Rat := ((c.paymentDate - input.reportingDate : Int) : Rat)
  let amortLt6m := sumBy (input.amortisationCashflows.filter (fun c => classifyNsfrBucket (amortDays c) == .lt6m)) (·.principalAmount)
  let amort6mTo1y := sumBy (input.amortisationCashflows.filter (fun c => classifyNsfrBucket (amortDays c) == .sixMTo1y)) (·.principalAmount)
  let amortGe1y := sumBy (input.amortisationCashflows.filter (fun c => classifyNsfrBucket (amortDays c) == .ge1y)) (·.principalAmount)
  let unwind := LCR.computeUnwindAdjustments (input.sftLegs.map SftLegRow.toLcrLeg)
  { posLcrWithin30dAmt := lcr30d
  , posNsfrLt6mAmt := nsfrLt6m
  , posNsfr6mTo1yAmt := nsfr6mTo1y
  , posNsfrGe1yAmt := nsfrGe1y
  , posEncGe1yAmt := encGe1y
  , derivNetOutflow := dOut
  , derivNetInflow := dIn
  , smeEligibleDeposits := dep.smeEligibleDeposits
  , fcsInsuredDeposits := dep.fcsInsuredDeposits
  , excludedTermDeposits := dep.excludedTermDeposits
  , cappedForeignSovHqla1 := cappedForeignSovHqla1 input.ccyHqla1Positions
  , max30dCollateralOutflow := max30dCollateralOutflow input.dailyCollateralFlows
  , avg12mNetOutflow := avg12mNetOutflow input.monthlyNetOutflows
  , ecaiResolvedGradeSum := sumBy ecaiResolved (fun (_, g) => (g.rank : Rat))
  , ecaiInvestmentGradeCount := ((ecaiResolved.filter (fun (_, g) => g.rank ≤ 3)).length : Rat)
  , amortNsfrLt6mAmt := amortLt6m
  , amortNsfr6mTo1yAmt := amort6mTo1y
  , amortNsfrGe1yAmt := amortGe1y
  , sftUnwindHqla1 := unwind.deltaHqla1
  , sftUnwindHqla2a := unwind.deltaHqla2a
  , sftUnwindHqla2b := unwind.deltaHqla2b
  , clfRecognisedAmount := input.clf.recognised
  }

/-- Every `PositionRow` is assigned to a unique NSFR maturity bucket. -/
theorem classifyNsfrBucket_exhaustive (days : Rat) :
    classifyNsfrBucket days = .lt6m ∨ classifyNsfrBucket days = .sixMTo1y ∨ classifyNsfrBucket days = .ge1y := by
  unfold classifyNsfrBucket
  by_cases h1 : days < 180
  · simp [h1]
  · by_cases h2 : days < 365
    · simp [h1, h2]
    · simp [h1, h2]

private theorem foldl_max_nonneg (xs : List Rat) (init : Rat) (h : 0 ≤ init) :
    0 ≤ xs.foldl max init := by
  induction xs generalizing init with
  | nil => exact h
  | cons x rest ih =>
    simp only [List.foldl_cons]
    apply ih
    change 0 ≤ (if init ≤ x then x else init)
    by_cases hix : init ≤ x
    · simp [hix]; exact Rat.le_trans h hix
    · simp [hix]; exact h

/-- `max30dCollateralOutflow` is always non-negative. -/
theorem max30dCollateralOutflow_nonneg (flows : List DailyCollateralFlow) :
    0 ≤ max30dCollateralOutflow flows := by
  unfold max30dCollateralOutflow
  exact foldl_max_nonneg _ 0 (by decide)

end APS210.Upstream
