import APS210.Basic
import APS210.Facts
import APS210.ExternalStandards
import APS210.LCR.HQLA

namespace APS210.Upstream

open APS210.Facts

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

/-- Inverse of `civilDate`: convert `EpochDate` to `(year, month, day)`. -/
def civilFromEpoch (z : EpochDate) : Int × Nat × Nat :=
  let z1 := z + 719468
  let era := (if z1 ≥ 0 then z1 else z1 - 146096) / 146097
  let doe := (z1 - era * 146097).toNat
  let yoe := (doe - doe / 1460 + doe / 36524 - doe / 146096) / 365
  let y := (yoe : Int) + era * 400
  let doy := doe - (365 * yoe + yoe / 4 - yoe / 100)
  let mp := (5 * doy + 2) / 153
  let d := doy - (153 * mp + 2) / 5 + 1
  let m := if mp < 10 then mp + 3 else mp - 9
  let yFinal := if m ≤ 2 then y + 1 else y
  (yFinal, m, d)

private def isLeapYear (y : Int) : Bool :=
  (y % 4 == 0 && y % 100 != 0) || (y % 400 == 0)

private def daysInMonth (y : Int) (m : Nat) : Nat :=
  match m with
  | 1 | 3 | 5 | 7 | 8 | 10 | 12 => 31
  | 4 | 6 | 9 | 11 => 30
  | 2 => if isLeapYear y then 29 else 28
  | _ => 30

/-- Add `deltaMonths` calendar months to `EpochDate`, clamping day to month-end. -/
def addMonthsEpoch (epochDays : EpochDate) (deltaMonths : Int) : EpochDate :=
  let (y, m, d) := civilFromEpoch epochDays
  let totalMonths : Int := (y * 12) + ((m : Int) - 1) + deltaMonths
  let newY : Int := if totalMonths ≥ 0 then totalMonths / 12 else (totalMonths - 11) / 12
  let newM0 : Int := totalMonths - newY * 12
  let newM : Nat := newM0.toNat + 1
  let newD : Nat := min d (daysInMonth newY newM)
  civilDate newY newM newD

-- ============================================================================
-- 1. Maturity & Encumbrance Bucketing (Attachment A / Attachment C)
-- ============================================================================

inductive NsfrBucket where
  | lt6m
  | sixMTo1y
  | ge1y
  deriving DecidableEq, Repr

/-- A balance-sheet position: who owes it, its contractual maturity, and every option on it with its
    holder. The maturity used for each rule is derived by `Facts.worstCaseSettlementDate`. -/
structure PositionRow where
  id : String
  side : Side
  amount : Money
  maturityDate : EpochDate
  options : Reading (List ContractOption)
  noticeDays : Rat
  encumbranceEndDate : EpochDate
  isEncumbered : Bool
  observation : ObservationContext
  deriving DecidableEq, Repr

/-- Worst-case maturity date under a regime (Att A paras 45 & 60, Att C paras 8 & 20). With an
    unknown option list, what we owe matures on the reporting date and what is owed to us has no
    established maturity (`none`). -/
def PositionRow.worstCaseMaturityDate (regime : MaturityRegime) (reportingDate : EpochDate)
    (p : PositionRow) : Option EpochDate :=
  match p.options with
  | .known opts => some (worstCaseSettlementDate regime p.side p.maturityDate opts)
  | _ =>
    match p.side with
    | .weOwe => some reportingDate
    | .owedToUs => none

/-- Residual days to the worst-case maturity (Att A Para 45, Att C Para 8).
    - For liabilities (`.weOwe`): if options are unknown, maturity is reporting date (0 days).
      If a withdrawal notice period is specified (`noticeDays > 0`), earliest redemption is the
      minimum of `noticeDays` and the contractual/option-adjusted residual days (Att C Para 8).
    - For assets (`.owedToUs`): residual days is at least `noticeDays`. -/
def PositionRow.residualDays (regime : MaturityRegime) (reportingDate : EpochDate) (p : PositionRow) :
    Option Rat :=
  match p.side, p.options with
  | .weOwe, .unknown _ | .weOwe, .refused _ => some 0
  | .weOwe, .known opts =>
    let d := worstCaseSettlementDate regime .weOwe p.maturityDate opts
    let contractDays := nonNeg ((d - reportingDate : Int) : Rat)
    some (if p.noticeDays > 0 then min p.noticeDays contractDays else contractDays)
  | .owedToUs, _ =>
    (p.worstCaseMaturityDate regime reportingDate).map (fun d =>
      max p.noticeDays ((d - reportingDate : Int) : Rat))

def PositionRow.encumbranceDays (reportingDate : EpochDate) (p : PositionRow) : Rat :=
  if p.isEncumbered then nonNeg ((p.encumbranceEndDate - reportingDate : Int) : Rat) else 0

/-- Calendar-based NSFR bucket classification relative to `reportingDate` (+6m, +12m). -/
def classifyNsfrBucketCal (reportingDate : EpochDate) (days : Rat) : NsfrBucket :=
  let sixMDays : Rat := ((addMonthsEpoch reportingDate 6 - reportingDate : Int) : Rat)
  let oneYDays : Rat := ((addMonthsEpoch reportingDate 12 - reportingDate : Int) : Rat)
  if days < sixMDays then .lt6m
  else if days < oneYDays then .sixMTo1y
  else .ge1y

/-- Legacy day-count NSFR bucket classification (180 / 365). -/
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
  observation : ObservationContext
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
  let byMna := groupByKey (·.mnaId) trades
  byMna.foldl (fun (accOut, accIn) (_, grp) =>
    if grp.all (·.hasEligibleMna) then
      let netMtm := sumBy grp (fun t => nonNeg t.mtmPayable - nonNeg t.mtmReceivable)
      (accOut + nonNeg netMtm, accIn + nonNeg (-netMtm))
    else
      let grossOut := sumBy grp (fun t => nonNeg t.mtmPayable)
      let grossIn := sumBy grp (fun t => nonNeg t.mtmReceivable)
      (accOut + grossOut, accIn + grossIn)
  ) (0, 0)

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
  managedAsRetail : Bool := false
  productCategory : String
  balance : Money
  hasEarlyWithdrawalNoPenalty : Bool
  /-- Att A Para 41: Whether the term deposit has no legal withdrawal right or imposes a material
      penalty for early withdrawal within 30 days (defaults to true for term-deposit rows where
      `!hasEarlyWithdrawalNoPenalty`). -/
  noWithdrawalRightOrMaterialPenalty : Bool := true
  residualDays : Rat
  observation : ObservationContext
  deriving DecidableEq, Repr

structure DepositClassificationSummary where
  smeEligibleDeposits : Money
  fcsInsuredDeposits : Money
  excludedTermDeposits : Money
  deriving DecidableEq, Repr

def evaluateDeposits (accounts : List DepositAccount) : DepositClassificationSummary :=
  let custTotals := groupByKey (·.customerGroupId) accounts |>.map (fun (cg, grp) =>
    (cg, sumBy grp (fun a => nonNeg a.balance)))
  let poolTaints := groupByKey (·.productCategory) accounts |>.map (fun (pc, grp) =>
    (pc, grp.any (·.hasEarlyWithdrawalNoPenalty)))
  let lookupTotal (cg : String) : Money :=
    (custTotals.find? (fun (k, _) => k == cg)).map (·.2) |>.getD 0
  let lookupTaint (pc : String) : Bool :=
    (poolTaints.find? (fun (k, _) => k == pc)).map (·.2) |>.getD false
  let isSmeEligible (a : DepositAccount) : Bool :=
    a.customerType == .sme && a.managedAsRetail && lookupTotal a.customerGroupId < 2000000
  let fcsInsuredShare (a : DepositAccount) : Money :=
    let bal := nonNeg a.balance
    if a.customerType == .retail || isSmeEligible a then
      let tot := lookupTotal a.customerGroupId
      if tot == 0 then 0
      else min bal (bal * min tot 250000 / tot)
    else 0
  let isExcludedTerm (a : DepositAccount) : Bool :=
    a.residualDays > 30 && a.noWithdrawalRightOrMaterialPenalty && !(lookupTaint a.productCategory)
  { smeEligibleDeposits := sumBy (accounts.filter isSmeEligible) (fun a => nonNeg a.balance)
  , fcsInsuredDeposits := sumBy accounts fcsInsuredShare
  , excludedTermDeposits := sumBy (accounts.filter isExcludedTerm) (fun a => nonNeg a.balance)
  }

-- ============================================================================
-- 4. LCR Att A Para 9(e) Foreign Sovereign HQLA1 Per-Currency Cap
-- ============================================================================

structure CcyHqla1Position where
  currency : String
  hqla1ForeignSovAmt : Money
  ccyNetOutflow : Money
  observation : ObservationContext
  deriving DecidableEq, Repr

def cappedForeignSovHqla1 (positions : List CcyHqla1Position) : Money :=
  let byCcy := groupByKey (·.currency) positions
  sumBy byCcy (fun (_, grp) =>
    let totalHqla1 := nonNeg (sumBy grp (fun p => nonNeg p.hqla1ForeignSovAmt))
    let capOutflow := match grp with
      | [] => 0
      | p0 :: rest => rest.foldl (fun m p => min m (nonNeg p.ccyNetOutflow)) (nonNeg p0.ccyNetOutflow)
    min totalHqla1 capOutflow)

-- ============================================================================
-- 5. Att A Table 3 Row 11(d) 24-Month Rolling 30-Day Collateral Lookback
-- ============================================================================

structure DailyCollateralFlow where
  valuationDate : EpochDate
  netCollateralFlow : Money
  observation : ObservationContext
  deriving DecidableEq, Repr

private def absRat (x : Rat) : Rat := if x < 0 then -x else x

private def yearMonthOfEpoch (d : EpochDate) : Int :=
  let (y, m, _) := civilFromEpoch d
  y * 100 + (m : Int)

/-- Calendar-date 30-day rolling collateral lookback over trailing 24 months.
    Input contract: missing calendar dates represent zero net collateral flow on that date;
    evaluates all critical 30-day rolling window endpoints (`d` when an event enters and
    `min (d + 30) reportingDate` just after an event exits) so sparse series never understate
    the true 30-day peak.
    Lookback validation requires non-empty flows spanning back `<= reportingDate - 23m`
    and covering `>= 24` distinct `(year, month)` buckets. -/
def max30dCollateralOutflowCal (reportingDate : EpochDate) (flows : List DailyCollateralFlow) : Money × Bool :=
  let lookback24m := addMonthsEpoch reportingDate (-24)
  let lookback23m := addMonthsEpoch reportingDate (-23)
  let inWindow := flows.filter (fun f => lookback24m ≤ f.valuationDate && f.valuationDate ≤ reportingDate)
  let endpoints := (inWindow.flatMap (fun r =>
    [r.valuationDate, min (r.valuationDate + 30) reportingDate])).eraseDups
  let rollingAbs := endpoints.map (fun endDate =>
    let frame := inWindow.filter (fun f => endDate - 29 ≤ f.valuationDate && f.valuationDate ≤ endDate)
    absRat (sumBy frame (·.netCollateralFlow)))
  let maxOut := rollingAbs.foldl max 0
  let hasEarlyCoverage := inWindow.any (fun f => f.valuationDate ≤ lookback23m)
  let distinctMonths := (inWindow.map (fun f => yearMonthOfEpoch f.valuationDate)).eraseDups.length
  (maxOut, !inWindow.isEmpty && hasEarlyCoverage && distinctMonths ≥ 24)

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
  observation : ObservationContext
  deriving DecidableEq, Repr

/-- Select trailing 12-month window `(reportingDate - 12m, reportingDate]`,
    handling leap-year month-end boundaries (e.g. `2025-02-28` excludes `2024-02-29` so exactly
    12 calendar month-ends fall in the trailing 12 months), validate 12 observations across 12
    distinct `(year, month)` buckets, and compute the average. -/
def avg12mNetOutflowCal (reportingDate : EpochDate) (months : List MonthlyNetOutflow) : Money × Bool :=
  let start12m := addMonthsEpoch reportingDate (-12)
  let (ry, rm, rd) := civilFromEpoch reportingDate
  let isRepMonthEnd := rd == daysInMonth ry rm
  let startYm := yearMonthOfEpoch start12m
  let inWindow := months.filter (fun m =>
    m.monthEndDate ≤ reportingDate &&
    if isRepMonthEnd then yearMonthOfEpoch m.monthEndDate > startYm
    else start12m < m.monthEndDate)
  let cnt := inWindow.length
  let distinctMonths := (inWindow.map (fun m => yearMonthOfEpoch m.monthEndDate)).eraseDups.length
  let avg := if cnt == 0 then 0 else sumBy inWindow (·.netCashOutflow) / (cnt : Rat)
  (avg, cnt == 12 && distinctMonths == 12)

def avg12mNetOutflow (months : List MonthlyNetOutflow) : Money :=
  if months.isEmpty then 0
  else sumBy months (·.netCashOutflow) / (months.length : Rat)

-- ============================================================================
-- 7. ECAI Letter-to-Grade Mapping & Multiple Assessment Resolution (APS 112 Att F)
-- ============================================================================

structure EcaiAssessmentRow where
  exposureId : String
  ratingStr : String
  observation : ObservationContext
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
  | "BB+" | "BB" | "BB-" => .belowBbbMinus
  | "B+" | "B" | "B-" => .bRating
  | "CCC" | "below_b_minus" => .belowBMinus
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
  observation : ObservationContext
  deriving DecidableEq, Repr

-- ============================================================================
-- 9. LCR SFT Unwinds & 3-Way CLF Bound (Attachment A Paras 8, 15-21)
-- ============================================================================

structure SftLegRow where
  legId : String
  currency : String := "AUD"
  residualDays : Nat := 30
  meetsOperationalReqs : Bool
  nettingEligible : Bool
  hqla1Delta : Money
  hqla2aDelta : Money
  hqla2bDelta : Money
  observation : ObservationContext
  deriving DecidableEq, Repr

def SftLegRow.toLcrLeg (s : SftLegRow) : LCR.ShortTermSftLeg :=
  { currency := if s.currency == "AUD" then .aud else .foreign s.currency
  , residualMaturityDays := s.residualDays
  , hqla1Delta := s.hqla1Delta
  , hqla2aDelta := s.hqla2aDelta
  , hqla2bDelta := s.hqla2bDelta
  , meetsOperationalReqs := s.meetsOperationalReqs
  , nettingCriteria := some
      { sameCounterparty := s.nettingEligible
      , sameExplicitFinalSettlementDate := s.nettingEligible
      , notOpenEndedUnwindable := s.nettingEligible
      , legallyEnforceableNettingNormalAndDefault := s.nettingEligible
      , settledNetOrSimultaneously := s.nettingEligible
      }
  }

structure ClfSummaryInput where
  approvedByApra : Bool := false
  contractualAmount : Money := 0
  pledgedZeroRunoff : Money := 0
  receivedZeroInflow : Money := 0
  collateralAfterMargin : Money := 0
  recentlyIneligibleCollateralAfterMargin : Money := 0
  collateralDiversified : Bool := false
  annualReapprovalCurrent : Bool := false
  includedInFundingStrategy : Bool := false
  balanceSheetEffortsExhausted : Bool := false
  clfDrawnNonIntraday : Bool := false
  drawdownAttestedAndNotifiedToApra : Bool := false
  /-- Para 16(a): whether CLF collateral meets the operational requirements of paras 22–25. -/
  collateralOperationalMet : Reading Bool
  /-- Para 25: days since `recentlyIneligibleCollateralAfterMargin` became ineligible. -/
  recentlyIneligibleCollateralDays : Reading Nat
  observation : ObservationContext
  deriving DecidableEq, Repr

private def allOperational (b : Bool) : LCR.HqlaOperationalCompliance :=
  { convertibleToCashAnytime := b, unencumberedAndControlled := b, controlEvidenced := b,
    notPledgedOrOperationalCost := b, notRehypothecatedIfReceivedInSft := b }

def ClfSummaryInput.toLcrClfInput (c : ClfSummaryInput) : LCR.ClfInput :=
  { approvedByApra := c.approvedByApra
  , contractualAmount := c.contractualAmount
  , pledgedUnderZeroRunoffSecFunding := c.pledgedZeroRunoff
  , receivedUnderZeroInflowSecLending := c.receivedZeroInflow
  , collateralHeldAfterRbaMargins := c.collateralAfterMargin
  , recentlyIneligibleCollateralAfterRbaMargins := c.recentlyIneligibleCollateralAfterMargin
  , collateralDiversified := c.collateralDiversified
  , annualReapprovalCurrent := c.annualReapprovalCurrent
  , includedInFundingStrategy := c.includedInFundingStrategy
  , balanceSheetEffortsExhausted := c.balanceSheetEffortsExhausted
  , clfDrawnNonIntraday := c.clfDrawnNonIntraday
  , drawdownAttestedAndNotifiedToApra := c.drawdownAttestedAndNotifiedToApra
  , collateralOperational := c.collateralOperationalMet.map allOperational
  , recentlyIneligibleCollateralDays := c.recentlyIneligibleCollateralDays
  , observation := c.observation
  }

def ClfSummaryInput.recognised (c : ClfSummaryInput) : Money :=
  c.toLcrClfInput.recognisedAmount

-- ============================================================================
-- Unified Upstream Evaluation & Compliance Properties
-- ============================================================================

structure UpstreamInput where
  /-- Reporting date (`validAt`) and data cut (`knownAt`) this evaluation is for. -/
  asOf : ObservationContext
  isAudOnly : Bool := false
  positions : List PositionRow
  derivativeTrades : List DerivativeTrade
  depositAccounts : List DepositAccount
  ccyHqla1Positions : List CcyHqla1Position
  dailyCollateralFlows : List DailyCollateralFlow
  monthlyNetOutflows : List MonthlyNetOutflow
  ecaiAssessments : List EcaiAssessmentRow := []
  amortisationCashflows : List AmortisationCashflowRow := []
  sftLegs : List SftLegRow := []
  clf : ClfSummaryInput
  deriving DecidableEq, Repr

def UpstreamInput.reportingDate (input : UpstreamInput) : EpochDate := input.asOf.validAt

/-- Data issues across every upstream record. -/
def UpstreamInput.dataIssues (input : UpstreamInput) : List DataIssue :=
  let obs := observationIssues input.asOf
  (input.positions.flatMap fun p =>
    obs p.id p.observation ++
    readingIssues p.id "options"
      (match p.side with
       | .weOwe => "treated as maturing on the reporting date"
       | .owedToUs => "excluded from the LCR horizon; placed in the NSFR >= 1y bucket") p.options) ++
  (input.derivativeTrades.flatMap fun t => obs t.tradeId t.observation) ++
  (input.depositAccounts.flatMap fun d => obs d.accountId d.observation) ++
  (input.ccyHqla1Positions.flatMap fun c => obs c.currency c.observation) ++
  (input.dailyCollateralFlows.flatMap fun f => obs s!"collateral-{f.valuationDate}" f.observation) ++
  (input.monthlyNetOutflows.flatMap fun m => obs s!"month-{m.monthEndDate}" m.observation) ++
  (input.ecaiAssessments.flatMap fun e => obs e.exposureId e.observation) ++
  (input.amortisationCashflows.flatMap fun a => obs a.loanId a.observation) ++
  (input.sftLegs.flatMap fun l => obs l.legId l.observation) ++
  obs "clf" input.clf.observation ++ input.clf.toLcrClfInput.dataIssues

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
  collateralLookbackValid : Bool
  avg12mNetOutflow : Money
  avg12mValid : Bool
  ecaiResolvedGradeSum : Rat
  ecaiInvestmentGradeCount : Rat
  amortNsfrLt6mAmt : Money
  amortNsfr6mTo1yAmt : Money
  amortNsfrGe1yAmt : Money
  sftUnwindHqla1 : Money
  sftUnwindHqla2a : Money
  sftUnwindHqla2b : Money
  clfRecognisedAmount : Money
  dataIssues : List DataIssue
  deriving DecidableEq, Repr

def evaluateUpstream (input : UpstreamInput) : UpstreamResult :=
  let rd := input.reportingDate
  let lcrWithin30 (p : PositionRow) : Bool := match p.residualDays .lcr rd with
    | some d => d ≤ 30
    | none => false
  let nsfrBucket (p : PositionRow) : NsfrBucket := match p.residualDays .nsfr rd with
    | some d => classifyNsfrBucketCal rd d
    | none => .ge1y
  let lcr30d := sumBy (input.positions.filter lcrWithin30) (·.amount)
  let nsfrLt6m := sumBy (input.positions.filter (fun p => nsfrBucket p == .lt6m)) (·.amount)
  let nsfr6mTo1y := sumBy (input.positions.filter (fun p => nsfrBucket p == .sixMTo1y)) (·.amount)
  let nsfrGe1y := sumBy (input.positions.filter (fun p => nsfrBucket p == .ge1y)) (·.amount)
  let encGe1y := sumBy (input.positions.filter (fun p =>
    p.isEncumbered && classifyNsfrBucketCal input.reportingDate (p.encumbranceDays input.reportingDate) == .ge1y)) (·.amount)
  let (dOut, dIn) := netDerivativeOutflowInflow input.derivativeTrades
  let dep := evaluateDeposits input.depositAccounts
  let ecaiResolved := resolveEcaiByExposure input.ecaiAssessments
  let amortDays (c : AmortisationCashflowRow) : Rat := ((c.paymentDate - input.reportingDate : Int) : Rat)
  let amortLt6m := sumBy (input.amortisationCashflows.filter (fun c => classifyNsfrBucketCal input.reportingDate (amortDays c) == .lt6m)) (·.principalAmount)
  let amort6mTo1y := sumBy (input.amortisationCashflows.filter (fun c => classifyNsfrBucketCal input.reportingDate (amortDays c) == .sixMTo1y)) (·.principalAmount)
  let amortGe1y := sumBy (input.amortisationCashflows.filter (fun c => classifyNsfrBucketCal input.reportingDate (amortDays c) == .ge1y)) (·.principalAmount)
  let unwind := LCR.computeUnwindAdjustments (input.sftLegs.map SftLegRow.toLcrLeg) input.isAudOnly
  let (maxCollat, collatValid) := max30dCollateralOutflowCal input.reportingDate input.dailyCollateralFlows
  let (avg12m, avgValid) := avg12mNetOutflowCal input.reportingDate input.monthlyNetOutflows
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
  , max30dCollateralOutflow := maxCollat
  , collateralLookbackValid := collatValid
  , avg12mNetOutflow := avg12m
  , avg12mValid := avgValid
  , ecaiResolvedGradeSum := sumBy ecaiResolved (fun (_, g) => (g.rank : Rat))
  , ecaiInvestmentGradeCount := ((ecaiResolved.filter (fun (_, g) => g.rank ≤ 3)).length : Rat)
  , amortNsfrLt6mAmt := amortLt6m
  , amortNsfr6mTo1yAmt := amort6mTo1y
  , amortNsfrGe1yAmt := amortGe1y
  , sftUnwindHqla1 := unwind.deltaHqla1
  , sftUnwindHqla2a := unwind.deltaHqla2a
  , sftUnwindHqla2b := unwind.deltaHqla2b
  , clfRecognisedAmount := input.clf.recognised
  , dataIssues := input.dataIssues
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
