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

ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(ROOT / "generated"))
from aps210_upstream_ibis import evaluate_aps210_upstream


def generate_dataset(seed: int) -> dict:
    rng = random.Random(seed)
    reporting_date = datetime.date(2026, 6, 30)

    positions = []
    for i in range(50):
        mat_offset = rng.randint(-10, 1200)
        call_offset = (
            rng.randint(5, max(5, mat_offset)) if mat_offset > 5 else mat_offset
        )
        ext_offset = mat_offset + rng.randint(30, 400)
        enc_offset = rng.randint(10, 800)
        positions.append(
            {
                "id": f"pos_{i:03d}",
                "amount": rng.randint(10_000, 5_000_000),
                "maturity_date": (
                    reporting_date + datetime.timedelta(days=mat_offset)
                ).isoformat(),
                "earliest_call_date": (
                    reporting_date + datetime.timedelta(days=call_offset)
                ).isoformat(),
                "has_call_option": rng.random() < 0.35,
                "latest_extension_date": (
                    reporting_date + datetime.timedelta(days=ext_offset)
                ).isoformat(),
                "has_extension_option": rng.random() < 0.25,
                "notice_days": rng.choice([0, 0, 0, 14, 35, 90, 200, 400]),
                "encumbrance_end_date": (
                    reporting_date + datetime.timedelta(days=enc_offset)
                ).isoformat(),
                "is_encumbered": rng.random() < 0.3,
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

    clf = {
        "eligible": rng.random() < 0.85,
        "contractual_amount": rng.randint(5_000_000, 25_000_000),
        "pledged_zero_runoff": rng.randint(0, 5_000_000),
        "received_zero_inflow": rng.randint(0, 3_000_000),
        "collateral_after_margin": rng.randint(5_000_000, 25_000_000),
    }

    return {
        "reporting_date": reporting_date.isoformat(),
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


def run_ibis(data: dict) -> dict[str, Decimal]:
    con = ibis.duckdb.connect()
    reporting_date = datetime.date.fromisoformat(data["reporting_date"])

    def to_memtable(rows: list[dict], date_cols: set[str]) -> ibis.Table:
        cols: dict[str, list] = {k: [] for k in rows[0]}
        for r in rows:
            for k, v in r.items():
                if k in date_cols:
                    cols[k].append(datetime.date.fromisoformat(v))
                elif isinstance(v, int) and not isinstance(v, bool):
                    cols[k].append(Decimal(v))
                else:
                    cols[k].append(v)
        return ibis.memtable(cols)

    positions = to_memtable(
        data["positions"],
        {
            "maturity_date",
            "earliest_call_date",
            "latest_extension_date",
            "encumbrance_end_date",
        },
    )
    derivative_trades = to_memtable(data["derivative_trades"], set())
    deposit_accounts = to_memtable(data["deposit_accounts"], set())
    ccy_hqla1_positions = to_memtable(data["ccy_hqla1_positions"], set())
    daily_collateral_flows = to_memtable(
        data["daily_collateral_flows"], {"valuation_date"}
    )
    monthly_net_outflows = to_memtable(data["monthly_net_outflows"], {"month_end_date"})
    ecai_assessments = to_memtable(data["ecai_assessments"], set())
    amortisation_cashflows = to_memtable(
        data["amortisation_cashflows"], {"payment_date"}
    )
    sft_legs = to_memtable(data["sft_legs"], set())
    clf = data["clf"]

    exprs = evaluate_aps210_upstream(
        positions,
        derivative_trades,
        deposit_accounts,
        ccy_hqla1_positions,
        daily_collateral_flows,
        monthly_net_outflows,
        ecai_assessments,
        amortisation_cashflows,
        sft_legs,
        reporting_date,
        data["is_aud_only"],
        clf["eligible"],
        Decimal(clf["contractual_amount"]),
        Decimal(clf["pledged_zero_runoff"]),
        Decimal(clf["received_zero_inflow"]),
        Decimal(clf["collateral_after_margin"]),
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
