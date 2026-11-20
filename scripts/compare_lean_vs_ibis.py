from __future__ import annotations

import argparse
import datetime
import json
import random
import subprocess
import sys
import tempfile
from decimal import Decimal
from fractions import Fraction
from pathlib import Path

import ibis
import pandas as pd

ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(ROOT / "generated"))
sys.path.insert(0, str(ROOT / "scripts"))
from aps210_upstream_ibis import evaluate_aps210_upstream
from verify_upstream_ibis import SCHEMAS as UPSTREAM_SCHEMAS


def generate_dataset(seed: int) -> dict:
    rng = random.Random(seed)
    reporting_date = datetime.date(2026, 6, 30)

    observation = {
        "valid_at": reporting_date.isoformat(),
        "known_at": int(datetime.datetime(2026, 7, 2, tzinfo=datetime.UTC).timestamp()),
        "source_reference": f"synthetic-seed-{seed}",
    }

    positions = []
    for i in range(50):
        mat_offset = rng.randint(-10, 1200)
        options = []
        for _ in range(rng.choice([0, 0, 1, 1, 2])):
            kind = rng.choice(["early_redemption", "extension"])
            res_offset = (
                rng.randint(0, max(0, mat_offset))
                if kind == "early_redemption"
                else mat_offset + rng.randint(1, 400)
            )
            res_date = (
                reporting_date + datetime.timedelta(days=res_offset)
            ).isoformat()
            options.append(
                {
                    "holder": rng.choice(["us", "counterparty"]),
                    "kind": kind,
                    "exercise_date": res_date,
                    "resulting_date": res_date,
                }
            )
        enc_offset = rng.randint(10, 800)
        positions.append(
            {
                "id": f"pos_{i:03d}",
                "side": rng.choice(["we_owe", "owed_to_us"]),
                "amount": rng.randint(10_000, 5_000_000),
                "maturity_date": (
                    reporting_date + datetime.timedelta(days=mat_offset)
                ).isoformat(),
                "options": (
                    {"known": options}
                    if rng.random() < 0.85
                    else {"unknown": "option schedule not loaded"}
                ),
                "notice_days": rng.choice([0, 0, 0, 14, 35, 90, 200, 400]),
                "encumbrance_end_date": (
                    reporting_date + datetime.timedelta(days=enc_offset)
                ).isoformat(),
                "is_encumbered": rng.random() < 0.3,
                "observation": observation,
            }
        )

    derivative_trades = []
    for i in range(30):
        has_mna = rng.random() < 0.75
        derivative_trades.append(
            {
                "trade_id": f"trd_{i:03d}",
                "mna_id": f"mna_{rng.randint(1, 8)}" if has_mna else f"gross_{i}",
                "has_eligible_mna": has_mna,
                "mtm_payable": rng.randint(0, 2_000_000),
                "mtm_receivable": rng.randint(0, 2_000_000),
            }
        )

    deposit_accounts = []
    for i in range(40):
        deposit_accounts.append(
            {
                "account_id": f"dep_{i:03d}",
                "customer_group_id": f"cg_{rng.randint(1, 15):02d}",
                "customer_type": rng.choice(["retail", "retail", "sme", "wholesale"]),
                "managed_as_retail": rng.random() < 0.85,
                "product_category": f"prod_{rng.randint(1, 5)}",
                "balance": rng.randint(25_000, 1_500_000),
                "has_early_withdrawal_no_penalty": rng.random() < 0.12,
                "residual_days": rng.choice([0, 15, 30, 45, 90, 180, 365]),
            }
        )

    currencies = ["USD", "EUR", "GBP", "JPY", "SGD", "NZD"]
    ccy_hqla1_positions = []
    for i in range(20):
        ccy_hqla1_positions.append(
            {
                "currency": rng.choice(currencies),
                "hqla1_foreign_sov_amt": rng.randint(100_000, 5_000_000),
                "ccy_net_outflow": rng.randint(-1_500_000, 4_000_000),
            }
        )

    start_flow_date = reporting_date - datetime.timedelta(days=729)
    daily_collateral_flows = []
    for i in range(730):
        daily_collateral_flows.append(
            {
                "valuation_date": (
                    start_flow_date + datetime.timedelta(days=i)
                ).isoformat(),
                "net_collateral_flow": rng.randint(-250_000, 250_000),
            }
        )

    monthly_net_outflows = []
    for i in range(12):
        monthly_net_outflows.append(
            {
                "month_end_date": (
                    reporting_date - datetime.timedelta(days=30 * (11 - i))
                ).isoformat(),
                "net_cash_outflow": rng.randint(5_000_000, 25_000_000),
            }
        )

    rating_choices = [
        "AAA",
        "AA+",
        "AA",
        "AA-",
        "A+",
        "A",
        "A-",
        "BBB+",
        "BBB",
        "BBB-",
        "below_bbb_minus",
        "unrated",
    ]
    ecai_assessments = []
    for i in range(45):
        ecai_assessments.append(
            {
                "exposure_id": f"exp_{rng.randint(1, 20):02d}",
                "rating_str": rng.choice(rating_choices),
            }
        )

    amortisation_cashflows = []
    for i in range(60):
        amortisation_cashflows.append(
            {
                "loan_id": f"loan_{rng.randint(1, 15):02d}",
                "payment_date": (
                    reporting_date + datetime.timedelta(days=rng.randint(10, 900))
                ).isoformat(),
                "principal_amount": rng.randint(5_000, 500_000),
            }
        )

    sft_legs = []
    for i in range(25):
        sft_legs.append(
            {
                "leg_id": f"sft_{i:03d}",
                "currency": rng.choice(["AUD", "AUD", "USD", "EUR"]),
                "residual_days": rng.choice([7, 14, 30, 45, 90]),
                "meets_operational_reqs": rng.random() < 0.85,
                "netting_eligible": rng.random() < 0.70,
                "hqla1_delta": rng.randint(-500_000, 500_000),
                "hqla2a_delta": rng.randint(-200_000, 200_000),
                "hqla2b_delta": rng.randint(-100_000, 100_000),
            }
        )

    days_known = rng.random() < 0.7
    clf = {
        "approved_by_apra": rng.random() < 0.90,
        "contractual_amount": rng.randint(5_000_000, 25_000_000),
        "pledged_zero_runoff": rng.randint(0, 5_000_000),
        "received_zero_inflow": rng.randint(0, 3_000_000),
        "collateral_after_margin": rng.randint(5_000_000, 25_000_000),
        "recently_ineligible_collateral_after_margin": rng.randint(0, 2_000_000),
        "collateral_diversified": rng.random() < 0.90,
        "annual_reapproval_current": rng.random() < 0.95,
        "included_in_funding_strategy": True,
        "balance_sheet_efforts_exhausted": True,
        "clf_drawn_non_intraday": False,
        "drawdown_attested_notified": False,
        "collateral_operational_met": rng.choice(
            [{"known": True}] * 6
            + [
                {"known": False},
                {"unknown": "custody report missing"},
                {"refused": "n/a"},
            ]
        ),
        "recently_ineligible_days": (
            {"known": rng.choice([0, 15, 30, 31, 60])}
            if days_known
            else {"unknown": "downgrade date not captured"}
        ),
        "observation": observation,
    }

    data = {
        "as_of": observation,
        "is_aud_only": True,
        "positions": positions,
        "derivative_trades": derivative_trades,
        "deposit_accounts": deposit_accounts,
        "ccy_hqla1_positions": ccy_hqla1_positions,
        "daily_collateral_flows": daily_collateral_flows,
        "monthly_net_outflows": monthly_net_outflows,
        "ecai_assessments": ecai_assessments,
        "amortisation_cashflows": amortisation_cashflows,
        "sft_legs": sft_legs,
        "clf": clf,
    }
    # Every record carries its observation context (no defaults in the Lean CLI).
    for key, rows in data.items():
        if isinstance(rows, list) and key != "positions":
            for row in rows:
                row["observation"] = observation
    return data


def reading_str(r: dict) -> str:
    """A reading as the IR's string column: "true" / "false" / "unknown" / "refused"."""
    if "known" in r:
        return "true" if r["known"] else "false"
    return "unknown" if "unknown" in r else "refused"


def run_ibis(data: dict) -> dict[str, Decimal]:
    con = ibis.duckdb.connect()
    reporting_date = datetime.date.fromisoformat(data["as_of"]["valid_at"])

    def cell(v, ty):
        if ty == "date":
            return datetime.date.fromisoformat(v)
        if ty.startswith("decimal"):
            return Decimal(v)
        return v

    def to_memtable(name: str, rows: list[dict]) -> ibis.Table:
        schema = UPSTREAM_SCHEMAS[name]
        for r in rows:
            missing = set(schema) - set(r)
            assert not missing, f"{name}: missing {missing}"
        return ibis.memtable(
            pd.DataFrame(
                [[cell(r[c], t) for c, t in schema.items()] for r in rows],
                columns=list(schema),
            ),
            schema=ibis.schema(schema),
        )

    positions = [
        {**p, "options_known": "known" in p["options"]} for p in data["positions"]
    ]
    position_options = [
        {"id": p["id"], **o}
        for p in data["positions"]
        for o in p["options"].get("known", [])
    ]
    tables = {
        "positions": positions,
        "position_options": position_options,
        **{
            name: data[name]
            for name in UPSTREAM_SCHEMAS
            if name not in ("positions", "position_options")
        },
    }
    clf = data["clf"]
    days = clf["recently_ineligible_days"]
    exprs = evaluate_aps210_upstream(
        *[to_memtable(name, tables[name]) for name in UPSTREAM_SCHEMAS],
        reporting_date=reporting_date,
        is_aud_only=data["is_aud_only"],
        clf_approved_by_apra=clf["approved_by_apra"],
        clf_contractual=Decimal(clf["contractual_amount"]),
        clf_pledged_zero_runoff=Decimal(clf["pledged_zero_runoff"]),
        clf_received_zero_inflow=Decimal(clf["received_zero_inflow"]),
        clf_collateral_after_margin=Decimal(clf["collateral_after_margin"]),
        clf_recently_ineligible_collateral_after_margin=Decimal(
            clf["recently_ineligible_collateral_after_margin"]
        ),
        clf_collateral_diversified=clf["collateral_diversified"],
        clf_annual_reapproval_current=clf["annual_reapproval_current"],
        clf_included_in_funding_strategy=clf["included_in_funding_strategy"],
        clf_balance_sheet_efforts_exhausted=clf["balance_sheet_efforts_exhausted"],
        clf_drawn_non_intraday=clf["clf_drawn_non_intraday"],
        clf_drawdown_attested_notified=clf["drawdown_attested_notified"],
        clf_collateral_operational_met=reading_str(clf["collateral_operational_met"]),
        clf_recently_ineligible_days_known="known" in days,
        clf_recently_ineligible_days=Decimal(days.get("known", 0)),
    )
    out: dict[str, Decimal | bool] = {}
    for k, v in exprs.items():
        val = con.execute(v)
        out[k] = bool(val) if isinstance(val, bool) else Decimal(str(val))
    return out


def run_lean(json_path: Path) -> dict[str, dict[str, Fraction | bool]]:
    exe = ROOT / ".lake" / "build" / "bin" / "eval_upstream"
    proc = subprocess.run(
        [str(exe), str(json_path)],
        check=True,
        capture_output=True,
        text=True,
    )
    parsed = json.loads(proc.stdout)
    return {
        layer: {
            k: (v if isinstance(v, bool) else Fraction(v)) for k, v in metrics.items()
        }
        for layer, metrics in parsed.items()
    }


def main() -> None:
    parser = argparse.ArgumentParser(
        description="Generate synthetic APS 210 data and compare Lean vs Ibis/DuckDB"
    )
    parser.add_argument("--seed", type=int, default=42)
    args = parser.parse_args()

    data = generate_dataset(args.seed)
    print(
        f"Generated dataset (seed={args.seed}): "
        f"{len(data['positions'])} positions, "
        f"{len(data['derivative_trades'])} derivatives, "
        f"{len(data['deposit_accounts'])} deposits, "
        f"{len(data['ccy_hqla1_positions'])} ccy HQLA1, "
        f"{len(data['daily_collateral_flows'])} daily flows, "
        f"{len(data['monthly_net_outflows'])} monthly outflows"
    )

    with tempfile.NamedTemporaryFile("w", suffix=".json", delete=False) as tmp:
        json.dump(data, tmp)
        tmp_path = Path(tmp.name)

    try:
        lean_out = run_lean(tmp_path)
    finally:
        tmp_path.unlink(missing_ok=True)

    ibis_out = run_ibis(data)

    print(
        f"\n{'Metric':30s} | {'Lean (APS210)':>18s} | {'Lean (IR)':>18s} | {'Ibis/DuckDB':>18s} | {'Diff':>12s} | Status"
    )
    print("-" * 112)
    for k, ibis_val in ibis_out.items():
        dom_val = lean_out["domain"][k]
        ir_val = lean_out["ir"][k]
        assert dom_val == ir_val, (
            f"Lean domain != Lean IR for {k}: {dom_val} vs {ir_val}"
        )
        if isinstance(ibis_val, bool):
            ok = dom_val == ibis_val
            status = "MATCH" if ok else "MISMATCH"
            print(
                f"{k:30s} | {dom_val!s:>18s} | {ir_val!s:>18s} | {ibis_val!s:>18s} | {'exact':>12s} | {status}"
            )
            assert ok, f"Mismatch for {k}: Lean={dom_val}, Ibis={ibis_val}"
        else:
            assert isinstance(dom_val, Fraction) and isinstance(ir_val, Fraction)
            dom_dec = Decimal(dom_val.numerator) / Decimal(dom_val.denominator)
            diff = abs(dom_dec - ibis_val)
            ok = diff <= Decimal("1e-6")
            status = "MATCH" if ok else "MISMATCH"
            print(
                f"{k:30s} | {dom_dec:18.4f} | {float(ir_val):18.4f} | {ibis_val:18.4f} | {diff:12.8f} | {status}"
            )
            assert ok, f"Mismatch for {k}: Lean={dom_dec}, Ibis={ibis_val}, diff={diff}"

    print(
        f"\nALL {len(ibis_out)} METRICS MATCH BETWEEN LEAN (APS210 + IR) AND IBIS/DUCKDB!"
    )


if __name__ == "__main__":
    main()
