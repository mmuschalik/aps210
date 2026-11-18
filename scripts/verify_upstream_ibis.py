from __future__ import annotations

import datetime
import sys
from decimal import Decimal
from pathlib import Path

import ibis

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "generated"))
from aps210_upstream_ibis import evaluate_aps210_upstream


def main() -> None:
    con = ibis.duckdb.connect()
    reporting_date = datetime.date(2026, 6, 30)

    # 1. Positions: test call option, notice_days, maturity buckets, and encumbrance
    positions = ibis.memtable(
        {
            "id": ["p1", "p2", "p3", "p4"],
            # p1: matures 2026-07-15 (15d <= 30d), unencumbered -> LCR <=30d & NSFR <6m
            # p2: matures 2027-07-15, but earliest_call_date 2026-09-01 (63d) with has_call_option=True -> NSFR <6m
            # p3: matures 2027-01-15 (199d -> 6m..1y), encumbered until 2027-08-01 (397d >= 365d)
            # p4: matures 2026-07-05 (5d), but notice_days=400 -> max(400, 5)=400d -> NSFR >=1y
            "amount": [Decimal("100"), Decimal("200"), Decimal("300"), Decimal("400")],
            "maturity_date": [
                datetime.date(2026, 7, 15),
                datetime.date(2027, 7, 15),
                datetime.date(2027, 1, 15),
                datetime.date(2026, 7, 5),
            ],
            "earliest_call_date": [
                datetime.date(2026, 7, 15),
                datetime.date(2026, 9, 1),
                datetime.date(2027, 1, 15),
                datetime.date(2026, 7, 5),
            ],
            "has_call_option": [False, True, False, False],
            "notice_days": [Decimal("0"), Decimal("0"), Decimal("0"), Decimal("400")],
            "encumbrance_end_date": [
                datetime.date(2026, 6, 30),
                datetime.date(2026, 6, 30),
                datetime.date(2027, 8, 1),
                datetime.date(2026, 6, 30),
            ],
            "is_encumbered": [False, False, True, False],
        }
    )

    # 2. Derivative trades: MNA netting vs gross non-MNA
    # mna1: (100-40) + (20-50) = +30 net payable
    # mna2: (10-60) = -50 -> +50 net receivable
    # non-MNA t4: payable=25, receivable=10 -> gross 25 outflow, 10 inflow
    derivative_trades = ibis.memtable(
        {
            "trade_id": ["t1", "t2", "t3", "t4"],
            "mna_id": ["mna1", "mna1", "mna2", "none"],
            "has_eligible_mna": [True, True, True, False],
            "mtm_payable": [Decimal("100"), Decimal("20"), Decimal("10"), Decimal("25")],
            "mtm_receivable": [Decimal("40"), Decimal("50"), Decimal("60"), Decimal("10")],
        }
    )

    # 3. Deposit accounts: customer_group $250k FCS & $2m SME + Para 42 pool tainting
    # cg1 (retail): a1=200k, a2=300k -> total=500k -> FCS cap 250k -> insured 100k + 150k = 250k
    # cg2 (sme): a3=1.5m -> <2m -> SME eligible, FCS insured 250k
    # cg3 (sme): a4=2.5m -> >=2m -> NOT SME eligible, FCS insured 0
    # Pool tainting: prodA has a1 tainted (True) so a2 (60d) is ALSO tainted! prodB (a3=60d, a4=90d) untainted -> excluded_term_deposits = 1.5m + 2.5m = 4.0m
    deposit_accounts = ibis.memtable(
        {
            "account_id": ["a1", "a2", "a3", "a4"],
            "customer_group_id": ["cg1", "cg1", "cg2", "cg3"],
            "customer_type": ["retail", "retail", "sme", "sme"],
            "product_category": ["prodA", "prodA", "prodB", "prodB"],
            "balance": [Decimal("200000"), Decimal("300000"), Decimal("1500000"), Decimal("2500000")],
            "has_early_withdrawal_no_penalty": [True, False, False, False],
            "residual_days": [Decimal("10"), Decimal("60"), Decimal("60"), Decimal("90")],
        }
    )

    # 4. LCR Para 9(e) foreign sovereign HQLA1 per-currency cap
    # USD: hqla1=120+30=150, outflow=100 -> capped 100
    # EUR: hqla1=40, outflow=80 -> capped 40
    # JPY: hqla1=50, outflow=-10 -> capped 0
    ccy_hqla1_positions = ibis.memtable(
        {
            "currency": ["USD", "USD", "EUR", "JPY"],
            "hqla1_foreign_sov_amt": [Decimal("120"), Decimal("30"), Decimal("40"), Decimal("50")],
            "ccy_net_outflow": [Decimal("60"), Decimal("40"), Decimal("80"), Decimal("-10")],
        }
    )

    # 5. Daily collateral flows (rolling 30d window): days 1..35
    # Days 1..30: +1 each (sum=30); Days 31..35: -20 each -> at day 35: 25*1 + 5*(-20) = -75 -> abs=75
    flow_dates = [datetime.date(2026, 1, 1) + datetime.timedelta(days=i) for i in range(35)]
    flow_vals = [Decimal("1") if i < 30 else Decimal("-20") for i in range(35)]
    daily_collateral_flows = ibis.memtable(
        {"valuation_date": flow_dates, "net_collateral_flow": flow_vals}
    )

    # 6. Monthly net cash outflows (12m average)
    month_dates = [datetime.date(2025, m, 28) for m in range(7, 13)] + [
        datetime.date(2026, m, 28) for m in range(1, 7)
    ]
    month_vals = [Decimal(str(100 + 10 * i)) for i in range(12)]  # avg = 155
    monthly_net_outflows = ibis.memtable(
        {"month_end_date": month_dates, "net_cash_outflow": month_vals}
    )

    exprs = evaluate_aps210_upstream(
        positions,
        derivative_trades,
        deposit_accounts,
        ccy_hqla1_positions,
        daily_collateral_flows,
        monthly_net_outflows,
        reporting_date,
    )

    results = {k: Decimal(str(con.execute(v))) for k, v in exprs.items()}
    expected = {
        "pos_lcr_within_30d_amt": Decimal("100"),
        "pos_nsfr_lt_6m_amt": Decimal("300"),
        "pos_nsfr_6m_to_1y_amt": Decimal("300"),
        "pos_nsfr_ge_1y_amt": Decimal("400"),
        "pos_enc_ge_1y_amt": Decimal("300"),
        "deriv_net_outflow": Decimal("55"),
        "deriv_net_inflow": Decimal("60"),
        "sme_eligible_deposits": Decimal("1500000"),
        "fcs_insured_deposits": Decimal("500000"),
        "excluded_term_deposits": Decimal("4000000"),
        "capped_foreign_sov_hqla1": Decimal("140"),
        "max_30d_collateral_outflow": Decimal("75"),
        "avg_12m_net_outflow": Decimal("155"),
    }

    for k, exp in expected.items():
        got = results[k]
        assert got == exp, f"Mismatch for {k}: got {got}, expected {exp}"
        print(f"  OK  {k:30s} = {got}")

    print("ALL 13 UPSTREAM IBIS/DUCKDB CHECKS PASSED!")


if __name__ == "__main__":
    main()
