import APS210.Basic
import APS210.LCR.Outflows
import APS210.LCR.Inflows
import APS210.NSFR
import APS210.Upstream

namespace APS210.CashFlows

open APS210.Facts

/-- Standard liquidity cash-flow projection buckets (Main Body Paragraphs 36 & 43). -/
inductive CashFlowHorizon where
  | intraday
  | days1To7
  | days8To30
  | days31ToLessThan6m
  | sixMonthsToLessThan1y
  | oneYearOrMore
  deriving DecidableEq, Repr

/-- Ordered list of all projection horizons from shortest to longest. -/
def allHorizons : List CashFlowHorizon :=
  [ .intraday
  , .days1To7
  , .days8To30
  , .days31ToLessThan6m
  , .sixMonthsToLessThan1y
  , .oneYearOrMore ]

/-- Source classification for SPV inclusion (Para 5) and derivative netting (Att A fn 5 & 8). -/
inductive CashFlowSource where
  | standard
  | securitisationSpv
  /-- A derivative payment in netting set `nettingSetId`; flows in the same netting set and currency
      net when a valid master netting agreement exists or they are same-day FX principal exchanges. -/
  | derivative (nettingSetId : String) (hasValidMna : Bool) (isSameDayFxPrincipalExchange : Bool)
  deriving DecidableEq, Repr

/-- A contractual cash flow: a factual `Obligation` (who owes it, when, with which options) plus the
    LCR treatment attributes of the item it arises from. -/
structure ContractualCashFlow where
  obligation : Obligation
  /-- Att A Para 60: True if the flow arises from a loan with no specific maturity. -/
  isOpenMaturity : Bool
  /-- Att A Para 60: Minimum contractual principal, fee, or interest payment on an open-maturity loan. -/
  isMinimumContractualPayment : Bool
  /-- Att A Para 66: Non-financial revenues are excluded from LCR inflows. -/
  isNonFinancialRevenue : Bool
  /-- Att A Para 31: Items included in the stock of HQLA cannot also be counted as LCR inflows. -/
  countedInHqla : Bool
  source : CashFlowSource
  deriving DecidableEq, Repr

def ContractualCashFlow.id (f : ContractualCashFlow) : String := f.obligation.id
def ContractualCashFlow.currency (f : ContractualCashFlow) : Currency := f.obligation.currency
def ContractualCashFlow.side (f : ContractualCashFlow) : Side := f.obligation.side
def ContractualCashFlow.amount (f : ContractualCashFlow) : Money := f.obligation.amount.value

/-- Att A Paras 45 & 60, Att C Paras 8 & 20: days from the reporting date to the worst-case settlement
    date. What we owe with an unknown date or option list is due now (day 0); what is owed to us with
    an unknown date or option list, OR a past-due settlement date (`d < refDate`, Att A Para 54),
    has no valid future settlement day (`none`). -/
def ContractualCashFlow.worstCaseDays (regime : MaturityRegime) (f : ContractualCashFlow)
    (asOfValidAt? : Option Int := none) : Option Nat :=
  let refDate := asOfValidAt?.getD f.obligation.observation.validAt
  match f.obligation.worstCaseSettlementDate regime asOfValidAt? with
  | none => none
  | some d =>
    match f.side with
    | .owedToUs => if d < refDate then none else some (d - refDate).toNat
    | .weOwe => some (d - refDate).toNat

/-- Classify day count into a `CashFlowHorizon` bucket. -/
def classifyHorizon (days : Nat) : CashFlowHorizon :=
  if days == 0 then .intraday
  else if days <= 7 then .days1To7
  else if days <= 30 then .days8To30
  else if days < 180 then .days31ToLessThan6m
  else if days < 365 then .sixMonthsToLessThan1y
  else .oneYearOrMore

/-- Calendar-based NSFR boundary days `(sixMonthsDays, oneYearDays)` relative to `reportingDate`. -/
def nsfrBoundaryDays (reportingDate? : Option Upstream.EpochDate := none) : Nat × Nat :=
  match reportingDate? with
  | some rd =>
    let sixM := (Upstream.addMonthsEpoch rd 6 - rd).toNat
    let oneY := (Upstream.addMonthsEpoch rd 12 - rd).toNat
    (sixM, oneY)
  | none => (180, 365)

/-- Classify day count into an NSFR maturity bucket using calendar boundaries when `reportingDate?` is provided. -/
def classifyNsfrDaysCal (reportingDate? : Option Upstream.EpochDate) (days : Nat) : NSFR.NsfrMaturityBucket :=
  let (sixM, oneY) := nsfrBoundaryDays reportingDate?
  if days < sixM then .lessThanSixMonths
  else if days < oneY then .sixMonthsToLessThanOneYear
  else .oneYearOrMore

/-- Classify day count into an NSFR maturity bucket (`<180d`, `180..364d`, `>=365d` fallback when no date given). -/
def classifyNsfrDays (days : Nat) : NSFR.NsfrMaturityBucket :=
  classifyNsfrDaysCal none days

/-- Whether an inflow is eligible under Att A Paras 31, 54, 60, and 66: owed to us, settling within
    30 days at the latest date the counterparty can choose, and affirmatively performing. -/
def ContractualCashFlow.isEligibleLcrInflow (f : ContractualCashFlow)
    (asOfValidAt? : Option Int := none) : Bool :=
  f.side == .owedToUs &&
  (match f.worstCaseDays .lcr asOfValidAt? with
   | some d => d <= 30
   | none => false) &&
  (!f.isOpenMaturity || f.isMinimumContractualPayment) &&
  !f.isNonFinancialRevenue &&
  !f.countedInHqla &&
  f.obligation.performing.affirmative

/-- Whether an outflow falls inside the 30-calendar-day LCR horizon at the earliest date the
    counterparty can demand it (Att A Para 45). -/
def ContractualCashFlow.isLcrOutflowWithin30d (f : ContractualCashFlow)
    (asOfValidAt? : Option Int := none) : Bool :=
  f.side == .weOwe &&
  (match f.worstCaseDays .lcr asOfValidAt? with
   | some d => d <= 30
   | none => true)

/-- One bucket in a multi-horizon cash-flow projection ladder (Main Body Para 36). -/
structure CashFlowLadderRow where
  horizon : CashFlowHorizon
  grossInflows : Money
  grossOutflows : Money
  netCashFlow : Rat
  cumulativeNetCashFlow : Rat
  deriving DecidableEq, Repr

/-- Convert a `CollateralMovement` to a `ContractualCashFlow` for horizon laddering. -/
def CollateralMovement.toLadderCashFlow (c : CollateralMovement) : ContractualCashFlow :=
  { obligation :=
      { id := c.obligationId
      , currency := c.currency
      , side := c.side
      , amount := c.amount
      , settlementDate := c.settlementDate
      , optionality := .known []
      , performing :=
          match c.side with
          | .weOwe => .known true
          | .owedToUs => .known (c.eligibleForOffset.affirmative && !c.alreadyCountedInHqla.affirmativeOrMissing)
      , observation := c.observation }
  , isOpenMaturity := false
  , isMinimumContractualPayment := true
  , isNonFinancialRevenue := false
  , countedInHqla := c.alreadyCountedInHqla.affirmativeOrMissing
  , source := .standard }

/-- Main Body Paragraphs 36 & 43: Project dated cash flows across time horizons
    (optionally filtered to a single currency), tracking cumulative net cash position.
    Inflows count only when performing, financial, with a known worst-case settlement day, and not
    open-maturity without a minimum contractual payment. -/
def projectCashFlowLadder
    (flows : List ContractualCashFlow)
    (currencyFilter : Option Currency := none)
    (collateral : List CollateralMovement := [])
    (asOfValidAt? : Option Int := none) : List CashFlowLadderRow :=
  let allFlows := flows ++ collateral.map CollateralMovement.toLadderCashFlow
  let scopedFlows : List ContractualCashFlow := match currencyFilter with
    | some c => allFlows.filter (·.currency == c)
    | none => allFlows
  let dated : List (ContractualCashFlow × Nat) := scopedFlows.filterMap (fun f =>
    match f.side, f.worstCaseDays .lcr asOfValidAt? with
    | .weOwe, some d => some (f, d)
    | .weOwe, none => some (f, 0)
    | .owedToUs, some d =>
      if f.obligation.performing.affirmative && !f.isNonFinancialRevenue && !f.countedInHqla &&
          (!f.isOpenMaturity || f.isMinimumContractualPayment) then some (f, d) else none
    | .owedToUs, none => none)
  let step := allHorizons.foldl (fun (acc : List CashFlowLadderRow × Rat) h =>
    let inBucket := dated.filter (fun (_, d) => classifyHorizon d == h)
    let ins := sumBy (inBucket.filter (fun (f, _) => f.side == .owedToUs)) (fun (f, _) => f.amount)
    let outs := sumBy (inBucket.filter (fun (f, _) => f.side == .weOwe)) (fun (f, _) => f.amount)
    let net := ins - outs
    let cum := acc.2 + net
    (acc.1 ++ [{ horizon := h, grossInflows := ins, grossOutflows := outs, netCashFlow := net, cumulativeNetCashFlow := cum }], cum)
  ) ([], 0)
  step.1

/-- Check whether initial unencumbered liquidity buffer covers cumulative net cash outflows
    across all projected time horizons (Main Body Paras 12, 36). -/
def hasPositiveSurvivalBuffer
    (initialLiquidBuffer : Money)
    (flows : List ContractualCashFlow)
    (currencyFilter : Option Currency := none)
    (collateral : List CollateralMovement := [])
    (asOfValidAt? : Option Int := none) : Bool :=
  let ladder := projectCashFlowLadder flows currencyFilter collateral asOfValidAt?
  let buf := nonNeg initialLiquidBuffer
  ladder.all (fun row => buf + row.cumulativeNetCashFlow >= 0)

-- ============================================================================
-- NSFR Cash-Flow Schedule Splitting (Attachment C Paras 8, 11(c), 22)
-- ============================================================================

/-- Attachment C Paras 8 & 11(c): Split a long-dated or multi-tranche liability's contractual
    cash flows (taking earliest redemption/call days per leg) across `<6m`, `6m..<1y`, `>=1y`
    ASF categories. Preserves total carrying value across the resulting `AsfItem`s. -/
def splitLiabilityCashFlowsForAsf
    (idPrefix : String)
    (lt6mCat sixTo1yCat ge1yCat : NSFR.AsfCategory)
    (legs : List (Money × Nat))
    (isInterdependentApproved : Bool)
    (reportingDate? : Option Upstream.EpochDate)
    (observation : ObservationContext) : List NSFR.AsfItem :=
  let lt6mAmt := sumBy (legs.filter (fun (_, d) => classifyNsfrDaysCal reportingDate? d == .lessThanSixMonths)) (fun (a, _) => nonNeg a)
  let midAmt := sumBy (legs.filter (fun (_, d) => classifyNsfrDaysCal reportingDate? d == .sixMonthsToLessThanOneYear)) (fun (a, _) => nonNeg a)
  let ge1yAmt := sumBy (legs.filter (fun (_, d) => classifyNsfrDaysCal reportingDate? d == .oneYearOrMore)) (fun (a, _) => nonNeg a)
  [ { id := idPrefix ++ "-lt6m", category := lt6mCat, carryingValue := lt6mAmt, isInterdependentApproved, observation }
  , { id := idPrefix ++ "-6m1y", category := sixTo1yCat, carryingValue := midAmt, isInterdependentApproved, observation }
  , { id := idPrefix ++ "-ge1y", category := ge1yCat, carryingValue := ge1yAmt, isInterdependentApproved, observation } ]

/-- Attachment C Para 22: Split an amortising loan or minimum contractual payment schedule
    between the `<1 year` portion (`lt1yCat`, e.g. 50% RSF) and `>=1 year` portion (`ge1yCat`).
    Preserves total principal across the resulting `RsfAssetItem`s. -/
def splitAmortisingLoanForRsf
    (idPrefix : String)
    (lt1yCat ge1yCat : NSFR.RsfCategory)
    (legs : List (Money × Nat))
    (encumbrance : NSFR.EncumbrancePeriod)
    (isInterdependentApproved : Bool)
    (reportingDate? : Option Upstream.EpochDate)
    (observation : ObservationContext) : List NSFR.RsfAssetItem :=
  let oneY := match reportingDate? with
    | some rd => (Upstream.addMonthsEpoch rd 12 - rd).toNat
    | none => 365
  let lt1yAmt := sumBy (legs.filter (fun (_, d) => d < oneY)) (fun (a, _) => nonNeg a)
  let ge1yAmt := sumBy (legs.filter (fun (_, d) => d >= oneY)) (fun (a, _) => nonNeg a)
  [ { id := idPrefix ++ "-lt1y", category := lt1yCat, carryingValue := lt1yAmt, encumbrance, isInterdependentApproved, observation }
  , { id := idPrefix ++ "-ge1y", category := ge1yCat, carryingValue := ge1yAmt, encumbrance, isInterdependentApproved, observation } ]

/-- Calendar-mandatory variant of `splitLiabilityCashFlowsForAsf` requiring `reportingDate`. -/
def splitLiabilityCashFlowsForAsfCal
    (reportingDate : Upstream.EpochDate)
    (idPrefix : String)
    (lt6mCat sixTo1yCat ge1yCat : NSFR.AsfCategory)
    (legs : List (Money × Nat))
    (isInterdependentApproved : Bool)
    (observation : ObservationContext) : List NSFR.AsfItem :=
  splitLiabilityCashFlowsForAsf idPrefix lt6mCat sixTo1yCat ge1yCat legs isInterdependentApproved (some reportingDate) observation

/-- Attachment C Paras 22, 29(c), 31, 33(b), 34(c): 3-way split of an amortising loan schedule
    across `<6m`, `6m..<1y`, and `>=1y` RSF categories (required for central bank and FI amortising
    loans where `<6m` and `6m..<1y` receive different RSF factors). -/
def splitAmortisingLoanForRsf3Way
    (idPrefix : String)
    (lt6mCat sixTo1yCat ge1yCat : NSFR.RsfCategory)
    (legs : List (Money × Nat))
    (encumbrance : NSFR.EncumbrancePeriod)
    (isInterdependentApproved : Bool)
    (reportingDate? : Option Upstream.EpochDate)
    (observation : ObservationContext) : List NSFR.RsfAssetItem :=
  let lt6mAmt := sumBy (legs.filter (fun (_, d) => classifyNsfrDaysCal reportingDate? d == .lessThanSixMonths)) (fun (a, _) => nonNeg a)
  let midAmt := sumBy (legs.filter (fun (_, d) => classifyNsfrDaysCal reportingDate? d == .sixMonthsToLessThanOneYear)) (fun (a, _) => nonNeg a)
  let ge1yAmt := sumBy (legs.filter (fun (_, d) => classifyNsfrDaysCal reportingDate? d == .oneYearOrMore)) (fun (a, _) => nonNeg a)
  [ { id := idPrefix ++ "-lt6m", category := lt6mCat, carryingValue := lt6mAmt, encumbrance, isInterdependentApproved, observation }
  , { id := idPrefix ++ "-6m1y", category := sixTo1yCat, carryingValue := midAmt, encumbrance, isInterdependentApproved, observation }
  , { id := idPrefix ++ "-ge1y", category := ge1yCat, carryingValue := ge1yAmt, encumbrance, isInterdependentApproved, observation } ]

/-- Calendar-mandatory variant of `splitAmortisingLoanForRsf` requiring `reportingDate`. -/
def splitAmortisingLoanForRsfCal
    (reportingDate : Upstream.EpochDate)
    (idPrefix : String)
    (lt1yCat ge1yCat : NSFR.RsfCategory)
    (legs : List (Money × Nat))
    (encumbrance : NSFR.EncumbrancePeriod)
    (isInterdependentApproved : Bool)
    (observation : ObservationContext) : List NSFR.RsfAssetItem :=
  splitAmortisingLoanForRsf idPrefix lt1yCat ge1yCat legs encumbrance isInterdependentApproved (some reportingDate) observation

-- ============================================================================
-- LCR 30-Day Derivative Cash-Flow Netting & Portfolio Derivation (Att A fn 5, 8)
-- ============================================================================

private def insertGroup {κ α : Type} [BEq κ] (key : κ) (x : α) :
    List (κ × List α) → List (κ × List α)
  | [] => [(key, [x])]
  | (k, xs) :: rest =>
    if k == key then (k, xs ++ [x]) :: rest
    else (k, xs) :: insertGroup key x rest

private def groupByKey {κ α : Type} [BEq κ] (keyFn : α → κ) (xs : List α) : List (κ × List α) :=
  xs.foldl (fun acc x => insertGroup (keyFn x) x acc) []

/-- Collateral the counterparty owes us may offset our derivative outflows only when it is known to be
    eligible to re-use and known not to be counted already in the stock of HQLA (Att A fn 5, para 31). -/
def _root_.APS210.Facts.CollateralMovement.offsetsOutflows (c : CollateralMovement) : Bool :=
  c.side == .owedToUs && c.eligibleForOffset.affirmative &&
  (match c.alreadyCountedInHqla with
   | .known false => true
   | _ => false)

/-- Collateral movement within the 30-day horizon: collateral we owe with an unknown date is due now;
    collateral owed to us needs a known future date (`0 <= d - validAt <= 30`, Att A Para 54). -/
def _root_.APS210.Facts.CollateralMovement.within30d (c : CollateralMovement) : Bool :=
  match c.side, c.settlementDate with
  | .weOwe, .known d => d - c.observation.validAt <= 30
  | .weOwe, _ => true
  | .owedToUs, .known d => 0 <= d - c.observation.validAt && d - c.observation.validAt <= 30
  | .owedToUs, _ => false

private def derivativeKey (f : ContractualCashFlow) : Option (String × Bool × Bool) :=
  match f.source with
  | .derivative ns mna fx => some (ns, mna, fx)
  | _ => none

/-- Attachment A footnotes 5 & 8: Derive LCR derivative cash outflows and inflows from directed
    payments and collateral movements within 30 days.

    * Payments in a netting set covered by a valid master netting agreement (`mna = true`) net per
      `(nettingSetId, currency)`. Collateral we owe in that set adds to the net outflow; eligible
      collateral owed to us reduces it, but never turns it into an inflow.
    * Same-day FX principal exchanges without an MNA (`mna = false, fx = true`, Att A fn 5 & 8)
      net across currencies by `(nettingSetId, settlementDay)` (same day only; legs on different
      days do not net, and legs with unknown dates fall through to gross).
    * Other payments are taken gross. Collateral we owe is an outflow on its own; eligible collateral
      owed to us offsets only the payment it secures (`obligationId`), down to zero.
    * Payments owed to us that are not affirmatively performing are excluded (Att A para 54). -/
def deriveLcrDerivativeOutflowsAndInflows
    (payments : List ContractualCashFlow) (collateral : List CollateralMovement) :
    List LCR.OutflowItem × List LCR.InflowItem :=
  let inHorizon := payments.filter (fun f =>
    (derivativeKey f).isSome &&
    match f.side with
    | .weOwe => f.isLcrOutflowWithin30d
    | .owedToUs => f.isEligibleLcrInflow)
  let isMnaNettable (f : ContractualCashFlow) : Bool :=
    match derivativeKey f with
    | some (_, mna, _) => mna
    | none => false
  let isFxSameDayNettable (f : ContractualCashFlow) : Bool :=
    match derivativeKey f with
    | some (_, false, true) => (f.worstCaseDays .lcr).isSome
    | _ => false
  let mnaNettable := inHorizon.filter isMnaNettable
  let fxNettable := inHorizon.filter isFxSameDayNettable
  let gross := inHorizon.filter (fun f => !isMnaNettable f && !isFxSameDayNettable f)
  let liveCollateral := collateral.filter (·.within30d)
  let nsOf (f : ContractualCashFlow) : String := match derivativeKey f with
    | some (ns, _, _) => ns
    | none => f.id
  let mnaGroups := groupByKey (fun f => (nsOf f, f.currency)) mnaNettable
  let fxGroups := groupByKey (fun f => (nsOf f, (f.worstCaseDays .lcr).getD 0)) fxNettable
  let isNettedSet (ns : String) (cur : Currency) : Bool :=
    mnaGroups.any (fun ((k, c), _) => k == ns && c == cur)
  let mnaNetItems := mnaGroups.foldl (fun (outs, ins) ((ns, cur), grp) =>
    let obs := match grp with
      | f :: _ => f.obligation.observation
      | [] => { validAt := 0, knownAt := 0, sourceReference := "" }
    let setCollateral := liveCollateral.filter (fun c => c.nettingSetId == ns && c.currency == cur)
    let base := sumBy grp (fun f => f.side.signedOutflow f.obligation.amount) +
      sumBy (setCollateral.filter (·.side == .weOwe)) (·.amount.value)
    let offset := sumBy (setCollateral.filter CollateralMovement.offsetsOutflows) (·.amount.value)
    if base >= 0 then
      (outs ++ [{ id := "deriv-net-out-" ++ ns, currency := cur, category := .derivativesCashOutflows,
                  amount := nonNeg (base - offset), observation := obs }], ins)
    else
      (outs, ins ++ [{ id := "deriv-net-in-" ++ ns, currency := cur, category := .derivativesCashInflows,
                       amount := -base, isFullyPerforming := .known true, countedInHqla := false,
                       observation := obs }])
  ) ([], [])
  let fxNetItems := fxGroups.foldl (fun (outs, ins) ((ns, _day), grp) =>
    let obs := match grp with
      | f :: _ => f.obligation.observation
      | [] => { validAt := 0, knownAt := 0, sourceReference := "" }
    let cur := match grp with
      | f :: rest => if rest.all (·.currency == f.currency) then f.currency else .aud
      | [] => .aud
    let base := sumBy grp (fun f => f.side.signedOutflow f.obligation.amount)
    if base >= 0 then
      (outs ++ [{ id := "deriv-fx-net-out-" ++ ns, currency := cur, category := .derivativesCashOutflows,
                  amount := base, observation := obs }], ins)
    else
      (outs, ins ++ [{ id := "deriv-fx-net-in-" ++ ns, currency := cur, category := .derivativesCashInflows,
                       amount := -base, isFullyPerforming := .known true, countedInHqla := false,
                       observation := obs }])
  ) ([], [])
  let grossOuts := (gross.filter (·.side == .weOwe)).map (fun f =>
    let offset := sumBy (liveCollateral.filter (fun c => c.obligationId == f.id && c.offsetsOutflows)) (·.amount.value)
    { id := "deriv-gross-out-" ++ f.id
      currency := f.currency
      category := .derivativesCashOutflows
      amount := nonNeg (f.amount - offset)
      observation := f.obligation.observation : LCR.OutflowItem })
  let grossIns := (gross.filter (·.side == .owedToUs)).map (fun f =>
    { id := "deriv-gross-in-" ++ f.id
      currency := f.currency
      category := .derivativesCashInflows
      amount := f.amount
      isFullyPerforming := .known true
      countedInHqla := false
      observation := f.obligation.observation : LCR.InflowItem })
  let collateralOuts := (liveCollateral.filter (fun c => c.side == .weOwe && !isNettedSet c.nettingSetId c.currency)).map (fun c =>
    { id := "deriv-collateral-out-" ++ c.obligationId
      currency := c.currency
      category := .derivativesCashOutflows
      amount := c.amount.value
      observation := c.observation : LCR.OutflowItem })
  (mnaNetItems.1 ++ fxNetItems.1 ++ grossOuts ++ collateralOuts, mnaNetItems.2 ++ fxNetItems.2 ++ grossIns)

/-- Derive 30-day LCR outflows and inflows from contractual cash flows and derivative collateral,
    including SPV cash flows (Main Body Para 5) and enforcing Att A Paras 31, 45, 54, 60, 66. -/
def deriveLcrFromCashFlows
    (flows : List ContractualCashFlow)
    (outflowCategoryFor : ContractualCashFlow → LCR.Table3OutflowCategory)
    (inflowCategoryFor : ContractualCashFlow → LCR.InflowCategory)
    (collateral : List CollateralMovement) : List LCR.OutflowItem × List LCR.InflowItem :=
  let nonDerivFlows := flows.filter (fun f => (derivativeKey f).isNone)
  let outFlows := (nonDerivFlows.filter ContractualCashFlow.isLcrOutflowWithin30d).map (fun f =>
    { id := f.id
      currency := f.currency
      category := outflowCategoryFor f
      amount := f.amount
      observation := f.obligation.observation : LCR.OutflowItem })
  let inFlows := (nonDerivFlows.filter ContractualCashFlow.isEligibleLcrInflow).map (fun f =>
    { id := f.id
      currency := f.currency
      category := inflowCategoryFor f
      amount := f.amount
      isFullyPerforming := f.obligation.performing
      countedInHqla := f.countedInHqla
      observation := f.obligation.observation : LCR.InflowItem })
  let (dOuts, dIns) := deriveLcrDerivativeOutflowsAndInflows flows collateral
  (outFlows ++ dOuts, inFlows ++ dIns)

/-- Data issues across contractual cash flows and collateral movements. -/
def cashFlowDataIssues (flows : List ContractualCashFlow) (collateral : List CollateralMovement) :
    List DataIssue :=
  (flows.flatMap fun f => f.obligation.dataIssues) ++
  (collateral.flatMap fun c =>
    let rid := c.obligationId ++ "/" ++ c.nettingSetId
    readingIssues rid "settlementDate"
      (match c.side with
       | .weOwe => "treated as due within 30 days"
       | .owedToUs => "excluded: settlement date not established") c.settlementDate ++
    (if c.side == .owedToUs then
      readingIssues rid "eligibleForOffset" "no offset against outflows" c.eligibleForOffset ++
      readingIssues rid "alreadyCountedInHqla" "no offset against outflows" c.alreadyCountedInHqla
    else []))

end APS210.CashFlows
