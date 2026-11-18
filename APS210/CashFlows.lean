import APS210.Basic
import APS210.LCR.Outflows
import APS210.LCR.Inflows
import APS210.NSFR
import APS210.Upstream

namespace APS210.CashFlows

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

/-- Direction of a contractual or contingent cash flow. -/
inductive CashFlowDirection where
  | inflow
  | outflow
  deriving DecidableEq, Repr

/-- Source classification for SPV inclusion (Para 5) and derivative netting (Att A fn 5 & 8). -/
inductive CashFlowSource where
  | standard
  | securitisationSpv
  | derivative (counterparty : String) (hasValidMna : Bool) (isSameDayFxPrincipalExchange : Bool)
  deriving DecidableEq, Repr

/-- A dated contractual cash-flow leg arising from an asset, liability, or OBS item. -/
structure ContractualCashFlow where
  id : String
  currency : Currency
  direction : CashFlowDirection
  amount : Money
  /-- Days from reporting date to contractual settlement (0 = intraday). -/
  contractualDays : Nat
  /-- Att A Para 45 & Att C Para 8: Earliest possible redemption/call date (in days) for outflows. -/
  earliestCallDays : Option Nat := none
  /-- Att A Para 60 & Att C Para 20: Latest possible maturity/extension date (in days) for inflows/assets. -/
  latestExtensionDays : Option Nat := none
  /-- Att A Para 60: True if the flow arises from a loan with no specific maturity. -/
  isOpenMaturity : Bool := false
  /-- Att A Para 60: Minimum contractual principal, fee, or interest payment on an open-maturity loan. -/
  isMinimumContractualPayment : Bool := false
  /-- Att A Para 66: Non-financial revenues are excluded from LCR inflows. -/
  isNonFinancialRevenue : Bool := false
  /-- Att A Para 31: Items included in the stock of HQLA cannot also be counted as LCR inflows. -/
  countedInHqla : Bool := false
  /-- Att A Para 54: Only fully performing exposures may contribute to LCR inflows. -/
  isPerforming : Bool := true
  source : CashFlowSource := .standard
  deriving DecidableEq, Repr

/-- Att A Para 45 & Att C Para 8: Effective days for outflows/funding (earliest possible call/redemption). -/
def ContractualCashFlow.effectiveOutflowDays (f : ContractualCashFlow) : Nat :=
  match f.earliestCallDays with
  | some c => min f.contractualDays c
  | none => f.contractualDays

/-- Att A Para 60 & Att C Para 20: Effective days for inflows/assets (latest possible maturity/extension). -/
def ContractualCashFlow.effectiveInflowDays (f : ContractualCashFlow) : Nat :=
  match f.latestExtensionDays with
  | some e => max f.contractualDays e
  | none => f.contractualDays

/-- Effective horizon days based on cash-flow direction. -/
def ContractualCashFlow.effectiveDays (f : ContractualCashFlow) : Nat :=
  match f.direction with
  | .outflow => f.effectiveOutflowDays
  | .inflow => f.effectiveInflowDays

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

/-- Whether an inflow leg is eligible under Att A Paras 31, 54, 60, and 66. -/
def ContractualCashFlow.isEligibleLcrInflow (f : ContractualCashFlow) : Bool :=
  f.direction == .inflow &&
  f.effectiveInflowDays <= 30 &&
  (!f.isOpenMaturity || f.isMinimumContractualPayment) &&
  !f.isNonFinancialRevenue &&
  !f.countedInHqla &&
  f.isPerforming

/-- Whether an outflow leg falls inside the 30-calendar-day LCR horizon (Att A Para 45). -/
def ContractualCashFlow.isLcrOutflowWithin30d (f : ContractualCashFlow) : Bool :=
  f.direction == .outflow && f.effectiveOutflowDays <= 30

/-- One bucket in a multi-horizon cash-flow projection ladder (Main Body Para 36). -/
structure CashFlowLadderRow where
  horizon : CashFlowHorizon
  grossInflows : Money
  grossOutflows : Money
  netCashFlow : Rat
  cumulativeNetCashFlow : Rat
  deriving DecidableEq, Repr

/-- Main Body Paragraphs 36 & 43: Project dated cash flows across time horizons
    (optionally filtered to a single currency), tracking cumulative net cash position.
    Excludes non-performing inflows and open-maturity inflows without minimum contractual payments. -/
def projectCashFlowLadder
    (flows : List ContractualCashFlow)
    (currencyFilter : Option Currency := none) : List CashFlowLadderRow :=
  let scopedFlows : List ContractualCashFlow := match currencyFilter with
    | some c => flows.filter (·.currency == c)
    | none => flows
  let validFlows : List ContractualCashFlow := scopedFlows.filter (fun f =>
    match f.direction with
    | .outflow => true
    | .inflow => f.isPerforming && !f.isNonFinancialRevenue && (!f.isOpenMaturity || f.isMinimumContractualPayment))
  let step := allHorizons.foldl (fun (acc : List CashFlowLadderRow × Rat) h =>
    let inBucket := validFlows.filter (fun f => classifyHorizon f.effectiveDays == h)
    let ins := sumBy (inBucket.filter (·.direction == .inflow)) (fun f => nonNeg f.amount)
    let outs := sumBy (inBucket.filter (·.direction == .outflow)) (fun f => nonNeg f.amount)
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
    (currencyFilter : Option Currency := none) : Bool :=
  let ladder := projectCashFlowLadder flows currencyFilter
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
    (isInterdependentApproved : Bool := false)
    (reportingDate? : Option Upstream.EpochDate := none) : List NSFR.AsfItem :=
  let lt6mAmt := sumBy (legs.filter (fun (_, d) => classifyNsfrDaysCal reportingDate? d == .lessThanSixMonths)) (fun (a, _) => nonNeg a)
  let midAmt := sumBy (legs.filter (fun (_, d) => classifyNsfrDaysCal reportingDate? d == .sixMonthsToLessThanOneYear)) (fun (a, _) => nonNeg a)
  let ge1yAmt := sumBy (legs.filter (fun (_, d) => classifyNsfrDaysCal reportingDate? d == .oneYearOrMore)) (fun (a, _) => nonNeg a)
  [ { id := idPrefix ++ "-lt6m", category := lt6mCat, carryingValue := lt6mAmt, isInterdependentApproved }
  , { id := idPrefix ++ "-6m1y", category := sixTo1yCat, carryingValue := midAmt, isInterdependentApproved }
  , { id := idPrefix ++ "-ge1y", category := ge1yCat, carryingValue := ge1yAmt, isInterdependentApproved } ]

/-- Attachment C Para 22: Split an amortising loan or minimum contractual payment schedule
    between the `<1 year` portion (`lt1yCat`, e.g. 50% RSF) and `>=1 year` portion (`ge1yCat`).
    Preserves total principal across the resulting `RsfAssetItem`s. -/
def splitAmortisingLoanForRsf
    (idPrefix : String)
    (lt1yCat ge1yCat : NSFR.RsfCategory)
    (legs : List (Money × Nat))
    (encumbrance : NSFR.EncumbrancePeriod := .unencumbered)
    (isInterdependentApproved : Bool := false)
    (reportingDate? : Option Upstream.EpochDate := none) : List NSFR.RsfAssetItem :=
  let oneY := match reportingDate? with
    | some rd => (Upstream.addMonthsEpoch rd 12 - rd).toNat
    | none => 365
  let lt1yAmt := sumBy (legs.filter (fun (_, d) => d < oneY)) (fun (a, _) => nonNeg a)
  let ge1yAmt := sumBy (legs.filter (fun (_, d) => d >= oneY)) (fun (a, _) => nonNeg a)
  [ { id := idPrefix ++ "-lt1y", category := lt1yCat, carryingValue := lt1yAmt, encumbrance, isInterdependentApproved }
  , { id := idPrefix ++ "-ge1y", category := ge1yCat, carryingValue := ge1yAmt, encumbrance, isInterdependentApproved } ]

-- ============================================================================
-- LCR 30-Day Derivative Cash-Flow Netting & Portfolio Derivation (Att A fn 5, 8)
-- ============================================================================

/-- A derivative cash-flow leg within the 30-day LCR horizon (Attachment A footnotes 5 & 8). -/
structure LcrDerivativeCashFlowLeg where
  counterparty : String
  currency : Currency
  hasValidMna : Bool
  isSameDayFxPrincipalExchange : Bool := false
  payable : Money
  receivable : Money
  /-- Att A fn 5 & Para 31: HQLA1/HQLA2 collateral posted/held not already counted in HQLA stock. -/
  uncountedHqlaCollateral : Money := 0
  deriving DecidableEq, Repr

private def insertGroup {κ α : Type} [BEq κ] (key : κ) (x : α) :
    List (κ × List α) → List (κ × List α)
  | [] => [(key, [x])]
  | (k, xs) :: rest =>
    if k == key then (k, xs ++ [x]) :: rest
    else (k, xs) :: insertGroup key x rest

private def groupByKey {κ α : Type} [BEq κ] (keyFn : α → κ) (xs : List α) : List (κ × List α) :=
  xs.foldl (fun acc x => insertGroup (keyFn x) x acc) []

/-- Attachment A footnotes 5 & 8: Derive LCR derivative cash outflows and inflows.
    Legs covered by a valid master netting agreement or simultaneous/same-day FX principal
    exchanges are netted per `(counterparty, currency)` net of uncounted HQLA collateral;
    all other legs are taken gross without cross-netting. -/
def deriveLcrDerivativeOutflowsAndInflows
    (legs : List LcrDerivativeCashFlowLeg) : List LCR.OutflowItem × List LCR.InflowItem :=
  let nettable := legs.filter (fun l => l.hasValidMna || l.isSameDayFxPrincipalExchange)
  let grossOnly := legs.filter (fun l => !l.hasValidMna && !l.isSameDayFxPrincipalExchange)
  let groups := groupByKey (fun l => (l.counterparty, l.currency)) nettable
  let netItems := groups.foldl (fun (outs, ins) ((cp, cur), grp) =>
    let rawNetPayable := sumBy grp (fun l => nonNeg l.payable - nonNeg l.receivable)
    let colAdj := sumBy grp (fun l => nonNeg l.uncountedHqlaCollateral)
    if rawNetPayable >= 0 then
      let netOut := nonNeg (rawNetPayable - colAdj)
      (outs ++ [{ id := "deriv-net-out-" ++ cp, currency := cur, category := .derivativesCashOutflows, amount := netOut }], ins)
    else
      let netIn := nonNeg ((-rawNetPayable) - colAdj)
      (outs, ins ++ [{ id := "deriv-net-in-" ++ cp, currency := cur, category := .derivativesCashInflows, amount := netIn }])
  ) ([], [])
  let grossOuts := grossOnly.map (fun l =>
    { id := "deriv-gross-out-" ++ l.counterparty
      currency := l.currency
      category := .derivativesCashOutflows
      amount := nonNeg (nonNeg l.payable - nonNeg l.uncountedHqlaCollateral) : LCR.OutflowItem })
  let grossIns := grossOnly.map (fun l =>
    { id := "deriv-gross-in-" ++ l.counterparty
      currency := l.currency
      category := .derivativesCashInflows
      amount := nonNeg (nonNeg l.receivable - nonNeg l.uncountedHqlaCollateral) : LCR.InflowItem })
  (netItems.1 ++ grossOuts, netItems.2 ++ grossIns)

/-- Derive 30-day LCR outflows and inflows from dated contractual cash flows,
    including SPV cash flows (Main Body Para 5) and enforcing Att A Paras 31, 45, 54, 60, 66. -/
def deriveLcrFromCashFlows
    (flows : List ContractualCashFlow)
    (outflowCategoryFor : ContractualCashFlow → LCR.Table3OutflowCategory)
    (inflowCategoryFor : ContractualCashFlow → LCR.InflowCategory)
    (derivativeLegs : List LcrDerivativeCashFlowLeg := []) : List LCR.OutflowItem × List LCR.InflowItem :=
  let nonDerivFlows := flows.filter (fun f =>
    match f.source with
    | .derivative _ _ _ => false
    | _ => true)
  let outFlows := (nonDerivFlows.filter ContractualCashFlow.isLcrOutflowWithin30d).map (fun f =>
    { id := f.id
      currency := f.currency
      category := outflowCategoryFor f
      amount := nonNeg f.amount : LCR.OutflowItem })
  let inFlows := (nonDerivFlows.filter ContractualCashFlow.isEligibleLcrInflow).map (fun f =>
    { id := f.id
      currency := f.currency
      category := inflowCategoryFor f
      amount := nonNeg f.amount
      isFullyPerforming := f.isPerforming
      countedInHqla := f.countedInHqla : LCR.InflowItem })
  let derivFromScheduled : List LcrDerivativeCashFlowLeg :=
    (flows.filter (fun f =>
      match f.source with
      | .derivative _ _ _ => f.effectiveDays <= 30
      | _ => false)).map (fun f =>
      match f.source with
      | .derivative cp mna fx =>
        { counterparty := cp
          currency := f.currency
          hasValidMna := mna
          isSameDayFxPrincipalExchange := fx
          payable := if f.direction == .outflow then nonNeg f.amount else 0
          receivable := if f.direction == .inflow then nonNeg f.amount else 0 }
      | _ =>
        { counterparty := f.id, currency := f.currency, hasValidMna := false, payable := 0, receivable := 0 })
  let (dOuts, dIns) := deriveLcrDerivativeOutflowsAndInflows (derivativeLegs ++ derivFromScheduled)
  (outFlows ++ dOuts, inFlows ++ dIns)

end APS210.CashFlows
