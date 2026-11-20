import APS210.Basic
import APS210.Facts

namespace APS210.LCR

open APS210.Facts

/-- Attachment A, Paragraph 40, Table 1 – The scorecard criteria for less stable retail deposits.
    The "+2 above the guarantee limit" criterion is derived from the customer's aggregate balance,
    not supplied. A criterion that is not known scores as present. -/
structure LessStableScorecardInput where
  /-- +2: Deposit is an on-line account. -/
  isOnlineAccount : Reading Bool
  /-- +1: Absence of an established customer relationship. -/
  noEstablishedRelationship : Reading Bool
  /-- +1: Deposit is heavily rate-driven. -/
  isHeavilyRateDriven : Reading Bool
  deriving DecidableEq, Repr

/-- Attachment A, Table 1: Compute scorecard total (0 to 6). -/
def LessStableScorecardInput.score (aboveGuaranteeLimit : Bool) (s : LessStableScorecardInput) : Nat :=
  (if aboveGuaranteeLimit then 2 else 0) +
  (if s.isOnlineAccount.affirmativeOrMissing then 2 else 0) +
  (if s.noEstablishedRelationship.affirmativeOrMissing then 1 else 0) +
  (if s.isHeavilyRateDriven.affirmativeOrMissing then 1 else 0)

/-- Attachment A, Table 2: Less stable deposit allocations.
    Score 0–2 -> 10%; Score 3 or more -> 25%. -/
def lessStableRunoffRate (score : Nat) : Rate :=
  if score <= 2 then pct 10 else pct 25

/-- Attachment A, Paragraphs 34–43 & 46: Retail and qualifying SME deposit item. -/
structure RetailDeposit where
  id : String
  /-- APG 210: the deposit guarantee applies per account-holder per ADI, so balances are
      aggregated by customer before the guarantee limit is applied. -/
  customerId : String
  currency : Currency
  balance : Money
  /-- Para 35: If intermediated, whether all four para 35(a)-(d) look-through conditions hold. -/
  isIntermediated : Bool := false
  intermediatedLookThroughMet : Bool := false
  /-- Para 41–43: Term deposit > 30d with no legal withdrawal right or material penalty. -/
  residualMaturityOrNoticeGt30Days : Bool := false
  noWithdrawalRightOrMaterialPenalty : Bool := false
  /-- Para 45: For wholesale (e.g. non-look-through intermediated FI) deposits, there is no legal
      right to withdraw within 30 days (unlike retail para 41, a material penalty alone does not
      exclude wholesale funding from the 30-day LCR window). -/
  noLegalWithdrawalRightWithin30Days : Bool := false
  /-- Para 42: Whether ADI has allowed non-hardship early withdrawal in this category (taints pool). -/
  categoryTaintedByEarlyWithdrawal : Bool := false
  /-- Para 37: Covered by FCS / effective government deposit insurance AND
      (established relationship OR transactional account). -/
  isStable : Bool
  /-- Para 37 & APG 210: Government deposit guarantee limit per customer (AUD 250,000 for the FCS).
      The customer's aggregate balance above this limit is less stable with +2 on the scorecard. -/
  guaranteeLimit : Money
  /-- APG 210: Notice deposits that have been called run off at 100%. -/
  noticeDepositCalled : Bool := false
  scorecard : LessStableScorecardInput
  /-- Para 26(a) & 27: Whether the host jurisdiction applies the Basel liquidity framework
      (required for `hostSupervisorOverrideRate` to apply at Level 2). -/
  hostJurisdictionAppliesBasel : Bool := false
  /-- Para 26(a): Optional host-supervisor override rate for offshore Level 2 subsidiaries. -/
  hostSupervisorOverrideRate : Option Rate := none
  observation : ObservationContext
  deriving DecidableEq, Repr

/-- Determine whether a retail deposit is excluded under Paragraphs 41–43. -/
def RetailDeposit.isExcludedTermDeposit (d : RetailDeposit) : Bool :=
  !d.noticeDepositCalled &&
  d.residualMaturityOrNoticeGt30Days &&
  d.noWithdrawalRightOrMaterialPenalty &&
  !d.categoryTaintedByEarlyWithdrawal

/-- Determine whether a wholesale (non-look-through intermediated FI) deposit is excluded under
    Paragraph 45 (earliest contractual maturity or binding notice period > 30 days, with no legal
    right to withdraw within 30 days). -/
def RetailDeposit.isExcludedWholesaleTermDeposit (d : RetailDeposit) : Bool :=
  !d.noticeDepositCalled &&
  d.residualMaturityOrNoticeGt30Days &&
  d.noLegalWithdrawalRightWithin30Days &&
  !d.categoryTaintedByEarlyWithdrawal

/-- APG 210 / Att A Paras 35 & 37: a customer's aggregate retail balance and lowest guarantee
    limit across their genuine retail deposits in the same currency.
    - Excludes non-look-through intermediated deposits (Para 35: treated as FI wholesale deposits).
    - Scopes to the deposit's currency so uninsured foreign-currency deposits (limit = 0) do not
      wipe out AUD FCS guarantee coverage in All-Currencies LCR. -/
def customerGuaranteePosition (deposits : List RetailDeposit) (customerId : String)
    (currency : Currency := .aud) : Money × Money :=
  let same := deposits.filter (fun x =>
    x.customerId == customerId &&
    x.currency == currency &&
    !(x.isIntermediated && !x.intermediatedLookThroughMet))
  let total := sumBy same (fun x => nonNeg x.balance)
  let limit := (same.foldl (fun (acc : Option Money) y =>
      some (match acc with
        | none => nonNeg y.guaranteeLimit
        | some m => min m (nonNeg y.guaranteeLimit))) none).getD 0
  (total, limit)

/-- Retail deposit outflow given the customer's aggregate balance and guarantee limit (Attachment A
    para 37, APG 210). For a stable deposit the insured share is allocated pro rata across the customer's
    deposits: it receives 5% and the remainder the less-stable rate with `+2` on the scorecard. A
    less-stable deposit scores `+2` when the customer's aggregate exceeds the limit. -/
def RetailDeposit.outflowGiven (basis : ConsolidationBasis) (customerTotal customerLimit : Money)
    (d : RetailDeposit) : Money :=
  let bal := nonNeg d.balance
  if d.isIntermediated && !d.intermediatedLookThroughMet then
    if d.isExcludedWholesaleTermDeposit then 0
    else bal * pct 100
  else if d.noticeDepositCalled then
    bal * pct 100
  else if d.isExcludedTermDeposit then
    0
  else match basis, d.hostJurisdictionAppliesBasel, d.hostSupervisorOverrideRate with
    | .level2, true, some r => bal * nonNeg r
    | _, _, _ =>
      -- Att A Para 40 Table 1: "+2" if balance > government deposit guarantee limit where it exists,
      -- and in its absence (limit <= 0), where balance > AUD 250,000 equivalent.
      let scorecardThreshold := if customerLimit > 0 then customerLimit else 250000
      let above := customerTotal > scorecardThreshold
      if d.isStable then
        let insured := if customerTotal <= 0 then 0 else bal * min customerTotal customerLimit / customerTotal
        let excessPart := nonNeg (bal - insured)
        insured * pct 5 + excessPart * lessStableRunoffRate (d.scorecard.score true)
      else
        bal * lessStableRunoffRate (d.scorecard.score above)

/-- Retail deposit outflow, aggregating the guarantee limit over `deposits` by `(customerId, currency)`. -/
def RetailDeposit.outflowAmount (basis : ConsolidationBasis) (deposits : List RetailDeposit)
    (d : RetailDeposit) : Money :=
  let (total, limit) := customerGuaranteePosition deposits d.customerId d.currency
  d.outflowGiven basis total limit

/-- Effective blended run-off rate for a retail/qualifying-SME deposit, consistent with
    `RetailDeposit.outflowAmount` (including the per-customer guarantee split). -/
def RetailDeposit.runoffRate (basis : ConsolidationBasis) (deposits : List RetailDeposit)
    (d : RetailDeposit) : Rate :=
  let bal := nonNeg d.balance
  if bal > 0 then
    d.outflowAmount basis deposits / bal
  else if d.isIntermediated && !d.intermediatedLookThroughMet then
    if d.isExcludedWholesaleTermDeposit then pct 0
    else pct 100
  else if d.noticeDepositCalled then pct 100
  else if d.isExcludedTermDeposit then pct 0
  else match basis, d.hostJurisdictionAppliesBasel, d.hostSupervisorOverrideRate with
    | .level2, true, some r => nonNeg r
    | _, _, _ =>
      let (total, limit) := customerGuaranteePosition deposits d.customerId d.currency
      let scorecardThreshold := if limit > 0 then limit else 250000
      if d.isStable then pct 5
      else lessStableRunoffRate (d.scorecard.score (total > scorecardThreshold))

def RetailDeposit.dataIssues (d : RetailDeposit) : List DataIssue :=
  readingIssues d.id "scorecard.isOnlineAccount" "scored as present (+2)" d.scorecard.isOnlineAccount ++
  readingIssues d.id "scorecard.noEstablishedRelationship" "scored as present (+1)" d.scorecard.noEstablishedRelationship ++
  readingIssues d.id "scorecard.isHeavilyRateDriven" "scored as present (+1)" d.scorecard.isHeavilyRateDriven

/-- Attachment A, Paragraph 53, Table 3 – All Cash Outflow Categories
    (for wholesale, secured funding, structured, derivatives, facilities, contingent, and other). -/
inductive Table3OutflowCategory where
  -- Retail & qualifying SME (when entered directly as Table 3 rows)
  | retailStable
  | retailLessStable
  | retailHigherRunoffLessStable
  -- Unsecured wholesale funding (paras 44–50)
  | operationalDepositInsured
  | operationalDepositUninsured
  /-- Para 50: Excess operational balances that do not qualify as operational deposits. -/
  | excessOperationalNonFinInsured
  | excessOperationalNonFinUninsured
  | excessOperationalFi
  /-- APG 210: Wholesale notice deposits that have been called (100% run-off). -/
  | wholesaleNoticeDepositCalled
  | nonOperationalCorpSovCbPseMdbSmeGt2mInsured
  | nonOperationalCorpSovCbPseMdbSmeGt2mUninsured
  | nonOperationalOther
  | unsecuredDebtIssuance
  -- Secured funding (<= 30 days)
  | securedByHqla1
  | securedByClfEligibleWithinLimit
  | securedByHqla2a
  | securedDomesticSovPseMdbRwLe20
  | securedByHqla2bRmbs
  | securedByOtherHqla2b
  | securedAllOther
  -- ABCP, ABS, covered bonds, structured financing
  | structuredMaturingIssuedByAdi
  | structuredSpvConduitSivLossOfFunding
  -- Derivatives & other transactions
  | derivativesCashOutflows
  | downgradeThreeNotchCollateral
  | excessNonSegregatedCollateralCallable
  | contractuallyRequiredCollateralNotYetDemanded
  | collateralSubstitutionToNonHqla
  | derivativeValuationLargest30dFlow24m (apraApprovedAltAmount : Option Money := none)
  | postedNonHqla1CollateralValuationChange
  -- Committed facilities (undrawn portion, paras 51–52)
  | committedFacilityRetailAndSme
  | committedCreditNonFinCorpSovCbPse
  | committedLiquidityNonFinCorpSovCbPse
  | committedFacilityBanksAndSupervisedAdis
  | committedCreditOtherFinancialInstitutions
  | committedLiquidityOtherFinancialInstitutions
  | committedFacilityOtherLegalEntities
  -- Other contingent funding obligations
  | revocableCreditAndLiquidityFacilities
  | tradeFinanceObligations
  | guaranteesAndLcsNonTradeFinance
  | buybacksDomesticDebtShortTerm (apraAgreedRate : Option Rate := none)
  | buybacksDomesticDebtLongTerm (apraAgreedRate : Option Rate := none)
  | nonContractualStructuredProductsAndManagedFunds
  | affiliatedDealerOrMarketMaker (adiOrApraRate : Rate)
  | otherNonContractualContingent (adiOrApraRate : Rate)
  /-- Para 33: APRA-determined outflow for active collateral swaps. -/
  | collateralSwapApraMethod
  /-- Forward-starting lending commitments: the committed amount net of the lending rollover
      already assumed in retail, SME and wholesale inflow rates (A&T CU7, ARS 210 item 15.8):
      `max(committed - rollover, 0)` at 100%. -/
  | forwardStartingLending (rolloverAssumedInInflows : Money)
  -- Other contractual cash outflows
  | otherContractualCashOutflows
  deriving DecidableEq, Repr

/-- Exact run-off / outflow factor from Attachment A, Table 3.
    Note: For `tradeFinanceObligations` and `guaranteesAndLcsNonTradeFinance`,
    the input `amount` is the average of actual monthly net outflows in a recent 12-month period,
    and Table 3 applies 100% and 50% respectively.
    For `derivativeValuationLargest30dFlow24m`, `amount` is the largest absolute net 30-day
    collateral flow realised in the past 24 months (100%), unless APRA approved an alternative (fn 6). -/
def Table3OutflowCategory.rate : Table3OutflowCategory → Rate
  | .retailStable => pct 5
  | .retailLessStable => pct 10
  | .retailHigherRunoffLessStable => pct 25
  | .operationalDepositInsured => pct 5
  | .operationalDepositUninsured => pct 25
  | .excessOperationalNonFinInsured => pct 20
  | .excessOperationalNonFinUninsured => pct 40
  | .excessOperationalFi => pct 100
  | .wholesaleNoticeDepositCalled => pct 100
  | .nonOperationalCorpSovCbPseMdbSmeGt2mInsured => pct 20
  | .nonOperationalCorpSovCbPseMdbSmeGt2mUninsured => pct 40
  | .nonOperationalOther => pct 100
  | .unsecuredDebtIssuance => pct 100
  | .securedByHqla1 => pct 0
  | .securedByClfEligibleWithinLimit => pct 0
  | .securedByHqla2a => pct 15
  | .securedDomesticSovPseMdbRwLe20 => pct 25
  | .securedByHqla2bRmbs => pct 25
  | .securedByOtherHqla2b => pct 50
  | .securedAllOther => pct 100
  | .structuredMaturingIssuedByAdi => pct 100
  | .structuredSpvConduitSivLossOfFunding => pct 100
  | .derivativesCashOutflows => pct 100
  | .downgradeThreeNotchCollateral => pct 100
  | .excessNonSegregatedCollateralCallable => pct 100
  | .contractuallyRequiredCollateralNotYetDemanded => pct 100
  | .collateralSubstitutionToNonHqla => pct 100
  | .derivativeValuationLargest30dFlow24m _ => pct 100
  | .postedNonHqla1CollateralValuationChange => pct 20
  | .committedFacilityRetailAndSme => pct 5
  | .committedCreditNonFinCorpSovCbPse => pct 10
  | .committedLiquidityNonFinCorpSovCbPse => pct 30
  | .committedFacilityBanksAndSupervisedAdis => pct 40
  | .committedCreditOtherFinancialInstitutions => pct 40
  | .committedLiquidityOtherFinancialInstitutions => pct 100
  | .committedFacilityOtherLegalEntities => pct 100
  | .revocableCreditAndLiquidityFacilities => pct 5
  | .tradeFinanceObligations => pct 100
  | .guaranteesAndLcsNonTradeFinance => pct 50
  | .buybacksDomesticDebtShortTerm r => nonNeg (r.getD (pct 10))
  | .buybacksDomesticDebtLongTerm r => nonNeg (r.getD (pct 5))
  | .nonContractualStructuredProductsAndManagedFunds => pct 5
  | .affiliatedDealerOrMarketMaker r => nonNeg r
  | .otherNonContractualContingent r => nonNeg r
  | .collateralSwapApraMethod => pct 100
  | .forwardStartingLending _ => pct 100
  | .otherContractualCashOutflows => pct 100

/-- A wholesale / off-balance-sheet / derivative outflow line item under Table 3. -/
structure OutflowItem where
  id : String
  currency : Currency
  category : Table3OutflowCategory
  amount : Money
  observation : ObservationContext
  deriving DecidableEq, Repr

def OutflowItem.outflowAmount (item : OutflowItem) : Money :=
  match item.category with
  | .derivativeValuationLargest30dFlow24m (some alt) => nonNeg alt
  | .forwardStartingLending rollover =>
    nonNeg (nonNeg item.amount - nonNeg rollover) * item.category.rate
  | _ => nonNeg item.amount * item.category.rate

/-- Total expected cash outflows over the 30-day LCR horizon (Attachment A para 30). -/
def computeTotalExpectedCashOutflows
    (retailDeposits : List RetailDeposit)
    (otherOutflows : List OutflowItem)
    (basis : ConsolidationBasis := .level1) : Money :=
  sumBy retailDeposits (RetailDeposit.outflowAmount basis retailDeposits) +
  sumBy otherOutflows OutflowItem.outflowAmount

/-- Attachment A, Paragraphs 44–46 & Table 3: Classify a non-operational wholesale deposit
    from `CounterpartyKind`, full deposit insurance status, and optional retail stability/score
    (used when an SME has < AUD 2m aggregated funding and is managed as retail per para 46). -/
def classifyNonOperationalWholesaleDeposit
    (cp : CounterpartyKind)
    (fullyCoveredByDepositInsurance : Bool)
    (smeIsStable : Bool)
    (smeScore : Nat) : Table3OutflowCategory :=
  match cp with
  | .nonFinancialCorporate | .sovereign _ _ | .centralBank | .rba | .pse _ | .mdb _ =>
    if fullyCoveredByDepositInsurance then .nonOperationalCorpSovCbPseMdbSmeGt2mInsured
    else .nonOperationalCorpSovCbPseMdbSmeGt2mUninsured
  | .sme aggAud managedAsRetail =>
    if aggAud < 2000000 && managedAsRetail then
      if smeIsStable then .retailStable
      else if smeScore <= 2 then .retailLessStable
      else .retailHigherRunoffLessStable
    else
      if fullyCoveredByDepositInsurance then .nonOperationalCorpSovCbPseMdbSmeGt2mInsured
      else .nonOperationalCorpSovCbPseMdbSmeGt2mUninsured
  | _ => .nonOperationalOther

/-- Attachment A, Paragraphs 46, 51–52 & Table 3: Classify a committed facility by counterparty
    and facility type (`isLiquidityFacility = true` vs credit facility).
    Per Para 46, only SMEs with `< AUD 2,000,000` aggregated funding managed as retail receive the
    5% retail/SME committed facility rate; larger or non-retail-managed SMEs are treated as
    non-financial corporates (10% credit / 30% liquidity). -/
def classifyCommittedFacility
    (cp : CounterpartyKind)
    (isLiquidityFacility : Bool) : Table3OutflowCategory :=
  match cp with
  | .naturalPerson => .committedFacilityRetailAndSme
  | .sme aggAud managedAsRetail =>
    if aggAud < 2000000 && managedAsRetail then
      .committedFacilityRetailAndSme
    else if isLiquidityFacility then
      .committedLiquidityNonFinCorpSovCbPse
    else
      .committedCreditNonFinCorpSovCbPse
  | .nonFinancialCorporate | .sovereign _ _ | .centralBank | .rba | .pse _ =>
    if isLiquidityFacility then .committedLiquidityNonFinCorpSovCbPse
    else .committedCreditNonFinCorpSovCbPse
  | .adiOrBank true => .committedFacilityBanksAndSupervisedAdis
  | .adiOrBank false | .otherFinancialInstitution =>
    if isLiquidityFacility then .committedLiquidityOtherFinancialInstitutions
    else .committedCreditOtherFinancialInstitutions
  -- Para 52: Facilities to hedge funds, money market funds or SPVs are always
  -- treated as liquidity facilities to other financial institutions (100% run-off).
  | .hedgeFundOrMmfOrSpv => .committedLiquidityOtherFinancialInstitutions
  | .mdb _ | .nationalDevelopmentBank | .otherLegalEntity => .committedFacilityOtherLegalEntities

/-- Attachment A, Paragraphs 46 & 50: Split an operational deposit into qualifying operational
    balances (5% insured / 25% uninsured) and excess non-operational balances
    (retail 5%/10%/25% for qualifying SMEs < AUD 2m managed as retail;
     20% insured / 40% uninsured for other non-FI corporates/sovereigns/CBs/PSEs/MDBs/SMEs >= 2m;
     100% for financial institutions and other legal entities).
    `excessCoveredByDepositInsurance?` allows separate insurance status for the excess balance
    (defaults to `fullyCoveredByDepositInsurance`). -/
def classifyOperationalDeposit
    (idPrefix : String)
    (currency : Currency)
    (cp : CounterpartyKind)
    (qualifyingOperationalAmount excessAmount : Money)
    (fullyCoveredByDepositInsurance : Bool)
    (smeIsStable : Bool)
    (smeScore : Nat)
    (observation : ObservationContext)
    (excessCoveredByDepositInsurance? : Option Bool := none) : List OutflowItem :=
  let excessInsured := excessCoveredByDepositInsurance?.getD fullyCoveredByDepositInsurance
  let opCat := if fullyCoveredByDepositInsurance then .operationalDepositInsured else .operationalDepositUninsured
  let excessCat := match cp with
    | .nonFinancialCorporate | .sovereign _ _ | .centralBank | .rba | .pse _ | .mdb _ =>
      if excessInsured then .excessOperationalNonFinInsured else .excessOperationalNonFinUninsured
    | .sme aggAud managedAsRetail =>
      if aggAud < 2000000 && managedAsRetail then
        if smeIsStable then .retailStable
        else if smeScore <= 2 then .retailLessStable
        else .retailHigherRunoffLessStable
      else
        if excessInsured then .excessOperationalNonFinInsured else .excessOperationalNonFinUninsured
    | _ => .excessOperationalFi
  [ { id := idPrefix ++ "-op", currency, category := opCat, amount := qualifyingOperationalAmount, observation }
  , { id := idPrefix ++ "-excess", currency, category := excessCat, amount := excessAmount, observation } ]

/-- Attachment A, Paragraph 51: For a backstop liquidity facility, only the portion
    covering debt maturing within 30 days is treated as a liquidity facility; any
    additional capacity is treated as a credit facility. -/
def classifyLiquidityFacilityWithMaturingDebt
    (idPrefix : String)
    (currency : Currency)
    (cp : CounterpartyKind)
    (totalFacilityAmount debtMaturingWithin30Days : Money)
    (observation : ObservationContext) : List OutflowItem :=
  let tot := nonNeg totalFacilityAmount
  let liq := min tot (nonNeg debtMaturingWithin30Days)
  let cred := nonNeg (tot - liq)
  [ { id := idPrefix ++ "-liq", currency, category := classifyCommittedFacility cp true, amount := liq, observation }
  , { id := idPrefix ++ "-cred", currency, category := classifyCommittedFacility cp false, amount := cred, observation } ]

/-- Attachment A, Table 3: Derive the 100% collateral substitution outflow from an HQLA
    item if the counterparty can substitute it to non-HQLA without the ADI's consent. -/
def substitutableCollateralOutflow
    (currency : Currency)
    (hqlaValueAfterHaircut : Money)
    (substitutableToNonHqlaWithoutConsent : Bool)
    (observation : ObservationContext) : OutflowItem :=
  { id := "collateral-substitution"
    currency
    observation
    category := .collateralSubstitutionToNonHqla
    amount := if substitutableToNonHqlaWithoutConsent then nonNeg hqlaValueAfterHaircut else 0 }

end APS210.LCR
