import Lean.Data.Json
import APS210.Upstream
import Lean2Ibis.UpstreamSpec

open Lean (Json)
open APS210.Upstream
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

private def parsePosition (j : Json) : Except String PositionRow := do
  pure
    { id := ← (← getField j "id").getStr?
    , amount := ← parseRat (← getField j "amount")
    , maturityDate := ← parseDate (← getField j "maturity_date")
    , earliestCallDate := ← parseDate (← getField j "earliest_call_date")
    , hasCallOption := ← (← getField j "has_call_option").getBool?
    , latestExtensionDate := ← parseDate (← getField j "latest_extension_date")
    , hasExtensionOption := ← (← getField j "has_extension_option").getBool?
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
    , productCategory := ← (← getField j "product_category").getStr?
    , balance := ← parseRat (← getField j "balance")
    , hasEarlyWithdrawalNoPenalty := ← (← getField j "has_early_withdrawal_no_penalty").getBool?
    , residualDays := ← parseRat (← getField j "residual_days")
    }

private def parseCcyHqla1 (j : Json) : Except String CcyHqla1Position := do
  pure
    { currency := ← (← getField j "currency").getStr?
    , hqla1ForeignSovAmt := ← parseRat (← getField j "hqla1_foreign_sov_amt")
    , ccyNetOutflow := ← parseRat (← getField j "ccy_net_outflow")
    }

private def parseCollateralFlow (j : Json) : Except String DailyCollateralFlow := do
  pure
    { valuationDate := ← parseDate (← getField j "valuation_date")
    , netCollateralFlow := ← parseRat (← getField j "net_collateral_flow")
    }

private def parseMonthlyOutflow (j : Json) : Except String MonthlyNetOutflow := do
  pure
    { monthEndDate := ← parseDate (← getField j "month_end_date")
    , netCashOutflow := ← parseRat (← getField j "net_cash_outflow")
    }

private def parseEcaiAssessment (j : Json) : Except String EcaiAssessmentRow := do
  pure
    { exposureId := ← (← getField j "exposure_id").getStr?
    , ratingStr := ← (← getField j "rating_str").getStr?
    }

private def parseAmortCashflow (j : Json) : Except String AmortisationCashflowRow := do
  pure
    { loanId := ← (← getField j "loan_id").getStr?
    , paymentDate := ← parseDate (← getField j "payment_date")
    , principalAmount := ← parseRat (← getField j "principal_amount")
    }

private def parseSftLeg (j : Json) : Except String SftLegRow := do
  pure
    { legId := ← (← getField j "leg_id").getStr?
    , meetsOperationalReqs := ← (← getField j "meets_operational_reqs").getBool?
    , nettingEligible := ← (← getField j "netting_eligible").getBool?
    , hqla1Delta := ← parseRat (← getField j "hqla1_delta")
    , hqla2aDelta := ← parseRat (← getField j "hqla2a_delta")
    , hqla2bDelta := ← parseRat (← getField j "hqla2b_delta")
    }

private def parseClf (j : Json) : Except String ClfSummaryInput := do
  pure
    { eligible := ← (← getField j "eligible").getBool?
    , contractualAmount := ← parseRat (← getField j "contractual_amount")
    , pledgedZeroRunoff := ← parseRat (← getField j "pledged_zero_runoff")
    , receivedZeroInflow := ← parseRat (← getField j "received_zero_inflow")
    , collateralAfterMargin := ← parseRat (← getField j "collateral_after_margin")
    }

private def parseArr {α : Type} (j : Json) (k : String) (f : Json → Except String α) : Except String (List α) := do
  let arr ← (← getField j k).getArr?
  arr.toList.mapM f

private def parseInput (j : Json) : Except String UpstreamInput := do
  pure
    { reportingDate := ← parseDate (← getField j "reporting_date")
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

private def rowToJson (row : Row) : Json :=
  Json.mkObj (row.map (fun (k, v) => (k, ratToJson v.asRat)))

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
