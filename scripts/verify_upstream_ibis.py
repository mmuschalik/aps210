from __future__ import annotations

import datetime
import sys
from decimal import Decimal
from pathlib import Path

import ibis
import pandas as pd

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "generated"))
from aps210_upstream_ibis import evaluate_aps210_upstream

D = "decimal(38, 12)"
REPORTING_DATE = datetime.date(2026, 6, 30)

# Every table column of `evaluate_aps210_upstream`, in the generated signature's table order.
SCHEMAS = {
    "positions": {
        "id": "string",
        "side": "string",  # "we_owe" / "owed_to_us"
        "amount": D,
        "maturity_date": "date",
        "options_known": "boolean",
        "notice_days": D,
        "encumbrance_end_date": "date",
        "is_encumbered": "boolean",
    },
    "position_options": {
        "id": "string",
        "holder": "string",  # "us" / "counterparty"
        "kind": "string",  # "early_redemption" / "extension"
        "resulting_date": "date",
    },
    "derivative_trades": {
        "trade_id": "string",
        "mna_id": "string",
        "has_eligible_mna": "boolean",
        "mtm_payable": D,
        "mtm_receivable": D,
    },
    "deposit_accounts": {
        "account_id": "string",
        "customer_group_id": "string",
        "customer_type": "string",
        "managed_as_retail": "boolean",
        "product_category": "string",
        "balance": D,
        "has_early_withdrawal_no_penalty": "boolean",
        "residual_days": D,
    },
    "ccy_hqla1_positions": {
        "currency": "string",
        "hqla1_foreign_sov_amt": D,
        "ccy_net_outflow": D,
    },
    "daily_collateral_flows": {"valuation_date": "date", "net_collateral_flow": D},
    "monthly_net_outflows": {"month_end_date": "date", "net_cash_outflow": D},
    "ecai_assessments": {"exposure_id": "string", "rating_str": "string"},
    "amortisation_cashflows": {
        "loan_id": "string",
        "payment_date": "date",
        "principal_amount": D,
    },
    "sft_legs": {
        "leg_id": "string",
        "currency": "string",
        "residual_days": D,
        "meets_operational_reqs": "boolean",
        "netting_eligible": "boolean",
        "hqla1_delta": D,
        "hqla2a_delta": D,
        "hqla2b_delta": D,
    },
}


def position(
    pid, side, amount, maturity, options_known=True, notice_days=0, enc_end=None
):
    """`enc_end=None` means unencumbered."""
    return {
        "id": pid,
        "side": side,
        "amount": Decimal(amount),
        "maturity_date": maturity,
        "options_known": options_known,
        "notice_days": Decimal(notice_days),
        "encumbrance_end_date": enc_end or REPORTING_DATE,
        "is_encumbered": enc_end is not None,
    }


def option(pid, holder, kind, resulting_date):
    return {"id": pid, "holder": holder, "kind": kind, "resulting_date": resulting_date}


d = datetime.date

# Positions (Att A paras 45 & 60, Att C paras 8 & 20; LCR counts only counterparty options):
#  p1  we owe, 15d                                    -> LCR <=30d, NSFR <6m
#  p2  we owe, 2027-07-15, counterparty call 63d      -> NSFR <6m
#  p3  we owe, 199d, encumbered 397d                  -> NSFR 6m..1y, encumbered >=1y
#  p4  we owe, 5d but notice 400d                     -> NSFR >=1y
#  p5  we owe, 2027-09-01, OUR call 10d               -> NSFR <6m (any holder), LCR no
#  p6  we owe, cp call 20d and cp extension 2028      -> LCR <=30d, NSFR <6m
#  p7  owed to us, 10d, cp extension to 2027-08-01    -> NSFR >=1y, LCR no
#  p8  owed to us, options unknown                    -> no maturity: NSFR >=1y, LCR no
#  p9  we owe, 2027-03-01, options unknown            -> due now: LCR <=30d, NSFR <6m
#  p10 owed to us, exactly 30d                        -> LCR <=30d, NSFR <6m
#  p11 owed to us, 31d                                -> NSFR <6m
#  p12 we owe, 2026-12-30 (+6m: 183d)                 -> NSFR 6m..1y (boundary)
#  p13 we owe, 2027-06-30 (+12m: 365d)                -> NSFR >=1y (boundary)
POSITIONS = [
    position("p1", "we_owe", 100, d(2026, 7, 15)),
    position("p2", "we_owe", 200, d(2027, 7, 15)),
    position("p3", "we_owe", 300, d(2027, 1, 15), enc_end=d(2027, 8, 1)),
    position("p4", "we_owe", 400, d(2026, 7, 5), notice_days=400),
    position("p5", "we_owe", 50, d(2027, 9, 1)),
    position("p6", "we_owe", 60, d(2026, 12, 1)),
    position("p7", "owed_to_us", 70, d(2026, 7, 10)),
    position("p8", "owed_to_us", 80, d(2026, 7, 10), options_known=False),
    position("p9", "we_owe", 90, d(2027, 3, 1), options_known=False),
    position("p10", "owed_to_us", 5, d(2026, 7, 30)),
    position("p11", "owed_to_us", 3, d(2026, 7, 31)),
    position("p12", "we_owe", 7, d(2026, 12, 30)),
    position("p13", "we_owe", 11, d(2027, 6, 30)),
]
POSITION_OPTIONS = [
    option("p2", "counterparty", "early_redemption", d(2026, 9, 1)),
    option("p5", "us", "early_redemption", d(2026, 7, 10)),
    option("p6", "counterparty", "early_redemption", d(2026, 7, 20)),
    option("p6", "counterparty", "extension", d(2028, 1, 1)),
    option("p7", "counterparty", "extension", d(2027, 8, 1)),
    # Listed for p8, whose option schedule is not known: ignored.
    option("p8", "counterparty", "early_redemption", d(2026, 7, 2)),
]

# MNA netting vs gross non-MNA: mna1 (100-40)+(20-50) = +30; mna2 10-60 -> 50 receivable;
# non-MNA t4: 25 out, 10 in.
DERIVATIVE_TRADES = [
    {
        "trade_id": t,
        "mna_id": m,
        "has_eligible_mna": e,
        "mtm_payable": Decimal(p),
        "mtm_receivable": Decimal(r),
    }
    for t, m, e, p, r in [
        ("t1", "mna1", True, 100, 40),
        ("t2", "mna1", True, 20, 50),
        ("t3", "mna2", True, 10, 60),
        ("t4", "none", False, 25, 10),
    ]
]

# Customer-group $250k FCS / $2m SME + para 42 pool tainting:
# cg1 (retail) 200k + 300k -> insured 250k; cg2 (sme) 1.5m -> SME, insured 250k;
# cg3 (sme) 2.5m -> not SME, insured 0. prodA tainted by a1, so a2 is not excluded;
# prodB untainted -> excluded term deposits 1.5m + 2.5m.
DEPOSIT_ACCOUNTS = [
    {
        "account_id": a,
        "customer_group_id": g,
        "customer_type": t,
        "managed_as_retail": True,
        "product_category": p,
        "balance": Decimal(b),
        "has_early_withdrawal_no_penalty": w,
        "residual_days": Decimal(r),
    }
    for a, g, t, p, b, w, r in [
        ("a1", "cg1", "retail", "prodA", 200000, True, 10),
        ("a2", "cg1", "retail", "prodA", 300000, False, 60),
        ("a3", "cg2", "sme", "prodB", 1500000, False, 60),
        ("a4", "cg3", "sme", "prodB", 2500000, False, 90),
    ]
]

# Para 9(e): USD min(150, 100) + EUR min(40, 80) + JPY min(50, 0) = 140
CCY_HQLA1_POSITIONS = [
    {"currency": c, "hqla1_foreign_sov_amt": Decimal(h), "ccy_net_outflow": Decimal(o)}
    for c, h, o in [
        ("USD", 120, 60),
        ("USD", 30, 40),
        ("EUR", 40, 80),
        ("JPY", 50, -10),
    ]
]

# Calendar 30d window + 24m lookback coverage
_anchors = [d(2024 + (6 + m - 1) // 12, ((6 + m - 1) % 12) + 1, 15) for m in range(25)]
DAILY_COLLATERAL_FLOWS = [
    {"valuation_date": v, "net_collateral_flow": Decimal(0)} for v in _anchors
] + [
    {
        "valuation_date": d(2026, 1, 1) + datetime.timedelta(days=i),
        "net_collateral_flow": Decimal(1) if i < 30 else Decimal(-20),
    }
    for i in range(35)
]

# 12-month average = 155
MONTHLY_NET_OUTFLOWS = [
    {"month_end_date": md, "net_cash_outflow": Decimal(100 + 10 * i)}
    for i, md in enumerate(
        [d(2025, m, 28) for m in range(7, 13)] + [d(2026, m, 28) for m in range(1, 7)]
    )
]

# e1 A+ -> 2; e2 AAA, BBB- -> worse = 3; e3 AA+, A, below -> 2nd best = 2; e4 unrated -> excluded
ECAI_ASSESSMENTS = [
    {"exposure_id": e, "rating_str": r}
    for e, r in [
        ("e1", "A+"),
        ("e2", "AAA"),
        ("e2", "BBB-"),
        ("e3", "AA+"),
        ("e3", "A"),
        ("e3", "below_bbb_minus"),
        ("e4", "unrated"),
    ]
]

AMORTISATION_CASHFLOWS = [
    {"loan_id": "L1", "payment_date": pd_, "principal_amount": Decimal(a)}
    for pd_, a in [(d(2026, 9, 1), 50), (d(2027, 3, 1), 75), (d(2028, 6, 30), 125)]
]

# SFT unwinds: s2 not netting-eligible keeps only negative deltas; s3 fails operational reqs.
SFT_LEGS = [
    {
        "leg_id": i,
        "currency": c,
        "residual_days": Decimal(r),
        "meets_operational_reqs": o,
        "netting_eligible": n,
        "hqla1_delta": Decimal(h1),
        "hqla2a_delta": Decimal(h2a),
        "hqla2b_delta": Decimal(h2b),
    }
    for i, c, r, o, n, h1, h2a, h2b in [
        ("s1", "AUD", 14, True, True, 20, -5, 0),
        ("s2", "AUD", 30, True, False, 15, -10, 3),
        ("s3", "AUD", 10, False, True, 100, 0, 0),
        ("s4", "USD", 15, True, True, 50, 0, 0),
    ]
]

TABLES = {
    "positions": POSITIONS,
    "position_options": POSITION_OPTIONS,
    "derivative_trades": DERIVATIVE_TRADES,
    "deposit_accounts": DEPOSIT_ACCOUNTS,
    "ccy_hqla1_positions": CCY_HQLA1_POSITIONS,
    "daily_collateral_flows": DAILY_COLLATERAL_FLOWS,
    "monthly_net_outflows": MONTHLY_NET_OUTFLOWS,
    "ecai_assessments": ECAI_ASSESSMENTS,
    "amortisation_cashflows": AMORTISATION_CASHFLOWS,
    "sft_legs": SFT_LEGS,
}

BASE_SCALARS = {
    "reporting_date": REPORTING_DATE,
    "is_aud_only": False,
    "clf_approved_by_apra": True,
    "clf_contractual": Decimal(500),
    "clf_pledged_zero_runoff": Decimal(80),
    "clf_received_zero_inflow": Decimal(20),
    "clf_collateral_after_margin": Decimal(460),
    "clf_recently_ineligible_collateral_after_margin": Decimal(0),
    "clf_collateral_diversified": True,
    "clf_annual_reapproval_current": True,
    "clf_included_in_funding_strategy": True,
    "clf_balance_sheet_efforts_exhausted": True,
    "clf_drawn_non_intraday": False,
    "clf_drawdown_attested_notified": False,
    "clf_collateral_operational_met": "true",  # reading
    "clf_recently_ineligible_days_known": False,
    "clf_recently_ineligible_days": Decimal(0),
}

# Expected values are from the Lean domain (`evaluateUpstream`, via `eval_upstream`).
EXPECTED = {
    "pos_lcr_within_30d_amt": Decimal(655),
    "pos_nsfr_lt_6m_amt": Decimal(908),
    "pos_nsfr_6m_to_1y_amt": Decimal(307),
    "pos_nsfr_ge_1y_amt": Decimal(161),
    "pos_enc_ge_1y_amt": Decimal(300),
    "deriv_net_outflow": Decimal(55),
    "deriv_net_inflow": Decimal(60),
    "sme_eligible_deposits": Decimal(1500000),
    "fcs_insured_deposits": Decimal(500000),
    "excluded_term_deposits": Decimal(4000000),
    "capped_foreign_sov_hqla1": Decimal(80),
    "max_30d_collateral_outflow": Decimal(100),
    "collateral_lookback_valid": True,
    "avg_12m_net_outflow": Decimal(155),
    "avg_12m_valid": True,
    "ecai_resolved_grade_sum": Decimal(7),
    "ecai_investment_grade_count": Decimal(3),
    "amort_nsfr_lt_6m_amt": Decimal(50),
    "amort_nsfr_6m_to_1y_amt": Decimal(75),
    "amort_nsfr_ge_1y_amt": Decimal(125),
    "sft_unwind_hqla1": Decimal(85),
    "sft_unwind_hqla2a": Decimal(-15),
    "sft_unwind_hqla2b": Decimal(3),
    "clf_recognised_amount": Decimal(440),
}

# CLF variants (Att A paras 15(c), 16, 25): collateral 400 + recently ineligible 50.
# bound a = 500, b = 500 - 80 + 20 = 440, c = 400 (+ 50 only with a known day count <= 30).
CLF_VARIANTS = [
    (
        "grace_30_days",
        {
            "clf_collateral_after_margin": Decimal(400),
            "clf_recently_ineligible_collateral_after_margin": Decimal(50),
            "clf_recently_ineligible_days_known": True,
            "clf_recently_ineligible_days": Decimal(30),
        },
        Decimal(440),
    ),
    (
        "grace_31_days",
        {
            "clf_collateral_after_margin": Decimal(400),
            "clf_recently_ineligible_collateral_after_margin": Decimal(50),
            "clf_recently_ineligible_days_known": True,
            "clf_recently_ineligible_days": Decimal(31),
        },
        Decimal(400),
    ),
    (
        "grace_days_unknown",
        {
            "clf_collateral_after_margin": Decimal(400),
            "clf_recently_ineligible_collateral_after_margin": Decimal(50),
            "clf_recently_ineligible_days_known": False,
            "clf_recently_ineligible_days": Decimal(1),
        },
        Decimal(400),
    ),
    ("operational_unknown", {"clf_collateral_operational_met": "unknown"}, Decimal(0)),
    ("operational_refused", {"clf_collateral_operational_met": "refused"}, Decimal(0)),
    ("operational_false", {"clf_collateral_operational_met": "false"}, Decimal(0)),
]


def memtable(name, rows):
    schema = SCHEMAS[name]
    for r in rows:
        assert set(r) == set(schema), (
            f"{name}: missing {set(schema) - set(r)}, extra {set(r) - set(schema)}"
        )
    return ibis.memtable(
        pd.DataFrame([[r[c] for c in schema] for r in rows], columns=list(schema)),
        schema=ibis.schema(schema),
    )


def run(con, scalars):
    args = [memtable(name, TABLES[name]) for name in SCHEMAS]
    results = {}
    for k, v in evaluate_aps210_upstream(*args, **scalars).items():
        val = con.execute(v)
        results[k] = bool(val) if isinstance(val, bool) else Decimal(str(val))
    return results


def main() -> None:
    con = ibis.duckdb.connect()

    results = run(con, BASE_SCALARS)
    for k, exp in EXPECTED.items():
        got = results[k]
        assert got == exp, f"Mismatch for {k}: got {got}, expected {exp}"
        print(f"  OK  {k:30s} = {got}")

    for name, overrides, exp in CLF_VARIANTS:
        unknown = set(overrides) - set(BASE_SCALARS)
        assert not unknown, unknown
        got = run(con, {**BASE_SCALARS, **overrides})["clf_recognised_amount"]
        assert got == exp, f"[clf_{name}] got {got}, expected {exp}"
        print(f"  OK  clf_{name:26s} = {got}")

    print(
        f"ALL {len(EXPECTED)} UPSTREAM + {len(CLF_VARIANTS)} CLF IBIS/DUCKDB CHECKS PASSED!"
    )


if __name__ == "__main__":
    main()
