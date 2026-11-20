import Lean.Data.Json
import APS210.Upstream
import Lean2Ibis.UpstreamSpec

open Lean (Json)
open APS210.Upstream
open APS210.Facts
open Lean2Ibis
open Lean2Ibis.UpstreamSpec

private def parseRat (j : Json) : Except String Rat := do
  match j with
  | .num n =>
    pure ((n.mantissa : Rat) / ((10 ^ n.exponent : Nat) : Rat))
  | .str s =>
    match s.toInt? with
    | some k => pure (k : Rat)
    | none => throw s!"Expected numeric string, got: {s}"
  | _ => throw s!"Expected JSON number, got: {j.compress}"

private def parseDate (j : Json) : Except String EpochDate := do
  let s ← j.getStr?
  match s.splitOn "-" with
  | [ys, ms, ds] =>
    match ys.toInt?, ms.toNat?, ds.toNat? with
    | some y, some m, some d => pure (civilDate y m d)
    | _, _, _ => throw s!"Invalid date components: {s}"
  | _ => throw s!"Invalid YYYY-MM-DD date: {s}"

private def getField (j : Json) (k : String) : Except String Json :=
  j.getObjVal? k

/-- `{"valid_at": "YYYY-MM-DD", "known_at": <epoch seconds>, "source_reference": "..."}`. -/
private def parseObservation (j : Json) : Except String ObservationContext := do
  pure
    { validAt := ← parseDate (← getField j "valid_at")
    , knownAt := ← (← getField j "known_at").getInt?
    , sourceReference := ← (← getField j "source_reference").getStr? }

private def parseObs (j : Json) : Except String ObservationContext := do
  parseObservation (← getField j "observation")

/-- A reading: `{"known": v}`, `{"unknown": "why"}` or `{"refused": "why"}`. No other form is accepted. -/
private def parseReading {α : Type} (j : Json) (f : Json → Except String α) : Except String (Reading α) :=
  match j.getObjVal? "known", j.getObjVal? "unknown", j.getObjVal? "refused" with
  | .ok v, _, _ => return .known (← f v)
  | _, .ok r, _ => return .unknown (← r.getStr?)
  | _, _, .ok r => return .refused (← r.getStr?)
  | _, _, _ => throw s!"Expected a reading (known / unknown / refused), got: {j.compress}"

private def parseSide (j : Json) : Except String Side := do
  match ← j.getStr? with
  | "we_owe" => pure .weOwe
  | "owed_to_us" => pure .owedToUs
  | s => throw s!"Unknown side: {s}"

private def parseOption (j : Json) : Except String ContractOption := do
  let holder ← match ← (← getField j "holder").getStr? with
    | "us" => pure OptionHolder.us
    | "counterparty" => pure OptionHolder.counterparty
    | s => throw s!"Unknown option holder: {s}"
  let kind ← match ← (← getField j "kind").getStr? with
    | "early_redemption" => pure OptionKind.earlyRedemption
    | "extension" => pure OptionKind.extension
    | s => throw s!"Unknown option kind: {s}"
  pure
    { holder, kind
    , exerciseDate := ← parseDate (← getField j "exercise_date")
    , resultingSettlementDate := ← parseDate (← getField j "resulting_date") }

private def parsePosition (j : Json) : Except String PositionRow := do
  pure
    { id := ← (← getField j "id").getStr?
    , side := ← parseSide (← getField j "side")
    , amount := ← parseRat (← getField j "amount")
    , maturityDate := ← parseDate (← getField j "maturity_date")
    , options := ← parseReading (← getField j "options") (fun a => do (← a.getArr?).toList.mapM parseOption)
    , observation := ← parseObs j
    , noticeDays := ← parseRat (← getField j "notice_days")
    , encumbranceEndDate := ← parseDate (← getField j "encumbrance_end_date")
    , isEncumbered := ← (← getField j "is_encumbered").getBool?
    }

private def parseDerivative (j : Json) : Except String DerivativeTrade := do
  pure
    { tradeId := ← (← getField j "trade_id").getStr?
    , mnaId := ← (← getField j "mna_id").getStr?
    , hasEligibleMna := ← (← getField j "has_eligible_mna").getBool?
    , mtmPayable := ← parseRat (← getField j "mtm_payable")
    , mtmReceivable := ← parseRat (← getField j "mtm_receivable")
    , observation := ← parseObs j
    }

private def parseCustomerType (s : String) : Except String DepositCustomerType :=
  match s with
  | "retail" => pure .retail
  | "sme" => pure .sme
  | "wholesale" => pure .wholesale
  | _ => throw s!"Unknown customer_type: {s}"

private def parseDeposit (j : Json) : Except String DepositAccount := do
  let ctStr ← (← getField j "customer_type").getStr?
  pure
    { accountId := ← (← getField j "account_id").getStr?
    , customerGroupId := ← (← getField j "customer_group_id").getStr?
    , customerType := ← parseCustomerType ctStr
    , managedAsRetail := ← (← getField j "managed_as_retail").getBool?
    , productCategory := ← (← getField j "product_category").getStr?
    , balance := ← parseRat (← getField j "balance")
    , hasEarlyWithdrawalNoPenalty := ← (← getField j "has_early_withdrawal_no_penalty").getBool?
    , residualDays := ← parseRat (← getField j "residual_days")
    , observation := ← parseObs j
    }

private def parseCcyHqla1 (j : Json) : Except String CcyHqla1Position := do
  pure
    { currency := ← (← getField j "currency").getStr?
    , hqla1ForeignSovAmt := ← parseRat (← getField j "hqla1_foreign_sov_amt")
    , ccyNetOutflow := ← parseRat (← getField j "ccy_net_outflow")
    , observation := ← parseObs j
    }

private def parseCollateralFlow (j : Json) : Except String DailyCollateralFlow := do
  pure
    { valuationDate := ← parseDate (← getField j "valuation_date")
    , netCollateralFlow := ← parseRat (← getField j "net_collateral_flow")
    , observation := ← parseObs j
    }

private def parseMonthlyOutflow (j : Json) : Except String MonthlyNetOutflow := do
  pure
    { monthEndDate := ← parseDate (← getField j "month_end_date")
    , netCashOutflow := ← parseRat (← getField j "net_cash_outflow")
    , observation := ← parseObs j
    }

private def parseEcaiAssessment (j : Json) : Except String EcaiAssessmentRow := do
  pure
    { exposureId := ← (← getField j "exposure_id").getStr?
    , ratingStr := ← (← getField j "rating_str").getStr?
    , observation := ← parseObs j
    }

private def parseAmortCashflow (j : Json) : Except String AmortisationCashflowRow := do
  pure
    { loanId := ← (← getField j "loan_id").getStr?
    , paymentDate := ← parseDate (← getField j "payment_date")
    , principalAmount := ← parseRat (← getField j "principal_amount")
    , observation := ← parseObs j
    }

private def parseSftLeg (j : Json) : Except String SftLegRow := do
  pure
    { legId := ← (← getField j "leg_id").getStr?
    , currency := ← (← getField j "currency").getStr?
    , residualDays := (← (← getField j "residual_days").getNat?)
    , meetsOperationalReqs := ← (← getField j "meets_operational_reqs").getBool?
    , nettingEligible := ← (← getField j "netting_eligible").getBool?
    , hqla1Delta := ← parseRat (← getField j "hqla1_delta")
    , hqla2aDelta := ← parseRat (← getField j "hqla2a_delta")
    , hqla2bDelta := ← parseRat (← getField j "hqla2b_delta")
    , observation := ← parseObs j
    }

private def parseClf (j : Json) : Except String ClfSummaryInput := do
  pure
    { approvedByApra := ← (← getField j "approved_by_apra").getBool?
    , contractualAmount := ← parseRat (← getField j "contractual_amount")
    , pledgedZeroRunoff := ← parseRat (← getField j "pledged_zero_runoff")
    , receivedZeroInflow := ← parseRat (← getField j "received_zero_inflow")
    , collateralAfterMargin := ← parseRat (← getField j "collateral_after_margin")
    , recentlyIneligibleCollateralAfterMargin := ← parseRat (← getField j "recently_ineligible_collateral_after_margin")
    , collateralDiversified := ← (← getField j "collateral_diversified").getBool?
    , annualReapprovalCurrent := ← (← getField j "annual_reapproval_current").getBool?
    , includedInFundingStrategy := ← (← getField j "included_in_funding_strategy").getBool?
    , balanceSheetEffortsExhausted := ← (← getField j "balance_sheet_efforts_exhausted").getBool?
    , clfDrawnNonIntraday := ← (← getField j "clf_drawn_non_intraday").getBool?
    , drawdownAttestedAndNotifiedToApra := ← (← getField j "drawdown_attested_notified").getBool?
    , collateralOperationalMet := ← parseReading (← getField j "collateral_operational_met") (·.getBool?)
    , recentlyIneligibleCollateralDays := ← parseReading (← getField j "recently_ineligible_days") (·.getNat?)
    , observation := ← parseObs j
    }

private def parseArr {α : Type} (j : Json) (k : String) (f : Json → Except String α) : Except String (List α) := do
  let arr ← (← getField j k).getArr?
  arr.toList.mapM f

private def parseInput (j : Json) : Except String UpstreamInput := do
  pure
    { asOf := ← parseObservation (← getField j "as_of")
    , isAudOnly := ← (← getField j "is_aud_only").getBool?
    , positions := ← parseArr j "positions" parsePosition
    , derivativeTrades := ← parseArr j "derivative_trades" parseDerivative
    , depositAccounts := ← parseArr j "deposit_accounts" parseDeposit
    , ccyHqla1Positions := ← parseArr j "ccy_hqla1_positions" parseCcyHqla1
    , dailyCollateralFlows := ← parseArr j "daily_collateral_flows" parseCollateralFlow
    , monthlyNetOutflows := ← parseArr j "monthly_net_outflows" parseMonthlyOutflow
    , ecaiAssessments := ← parseArr j "ecai_assessments" parseEcaiAssessment
    , amortisationCashflows := ← parseArr j "amortisation_cashflows" parseAmortCashflow
    , sftLegs := ← parseArr j "sft_legs" parseSftLeg
    , clf := ← parseClf (← getField j "clf")
    }

private def ratToJson (r : Rat) : Json :=
  Json.str s!"{r.num}/{r.den}"

private def valToJson : Val → Json
  | .rat r => ratToJson r
  | .bool b => Json.bool b
  | .str s => Json.str s
  | .date d => Json.num d

private def rowToJson (row : Row) : Json :=
  Json.mkObj (row.map (fun (k, v) => (k, valToJson v)))

def main (args : List String) : IO UInt32 := do
  let [path] := args | do
    IO.eprintln "Usage: eval_upstream <input.json>"
    return 1
  let raw ← IO.FS.readFile path
  match Json.parse raw >>= parseInput with
  | .error err =>
    IO.eprintln s!"Parse error: {err}"
    return 1
  | .ok input =>
    let domainRes := evaluateUpstream input
    let irRes := evalProgram upstreamProgram (upstreamInputToTableEnv input) (upstreamInputToScalars input)
    let outJson := Json.mkObj
      [ ("domain", rowToJson (upstreamResultToRow domainRes))
      , ("ir", rowToJson irRes)
      ]
    IO.println outJson.compress
    return 0
