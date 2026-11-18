from __future__ import annotations

import sys
from decimal import Decimal
from pathlib import Path

import ibis

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "generated"))
from aps210_cashflows_ibis import evaluate_aps210_cashflows


def make_tables(
    cf_rows: list[dict],
    deriv_rows: list[dict],
    liab_rows: list[dict],
    loan_rows: list[dict],
):
    cf_schema = ibis.schema(
        {
            "id": "string",
            "currency": "string",
            "direction": "string",
            "amount": "decimal(38, 12)",
            "contractual_days": "decimal(38, 12)",
            "has_earliest_call": "boolean",
            "earliest_call_days": "decimal(38, 12)",
            "has_latest_extension": "boolean",
            "latest_extension_days": "decimal(38, 12)",
            "is_open_maturity": "boolean",
            "is_minimum_contractual_payment": "boolean",
            "is_non_financial_revenue": "boolean",
            "counted_in_hqla": "boolean",
            "is_performing": "boolean",
            "source": "string",
        }
    )
    deriv_schema = ibis.schema(
        {
            "counterparty": "string",
            "currency": "string",
            "has_valid_mna": "boolean",
            "is_same_day_fx_principal_exchange": "boolean",
            "payable": "decimal(38, 12)",
            "receivable": "decimal(38, 12)",
            "uncounted_hqla_collateral": "decimal(38, 12)",
        }
    )
    leg_schema = ibis.schema(
        {
            "id": "string",
            "amount": "decimal(38, 12)",
            "days": "decimal(38, 12)",
        }
    )
    return (
        ibis.memtable(cf_rows, schema=cf_schema),
        ibis.memtable(deriv_rows, schema=deriv_schema),
        ibis.memtable(liab_rows, schema=leg_schema),
        ibis.memtable(loan_rows, schema=leg_schema),
    )


def main() -> None:
    con = ibis.duckdb.connect()

    # Case 1: Exact mirror of Lean2Ibis/CashFlowsSpec.lean end-to-end test + Para 60 / Para 31 / Para 66 exclusions
    cf_rows = [
        {
            "id": "out1",
            "currency": "AUD",
            "direction": "outflow",
            "amount": Decimal("60"),
            "contractual_days": Decimal("5"),
            "has_earliest_call": False,
            "earliest_call_days": Decimal("0"),
            "has_latest_extension": False,
            "latest_extension_days": Decimal("0"),
            "is_open_maturity": False,
            "is_minimum_contractual_payment": False,
            "is_non_financial_revenue": False,
            "counted_in_hqla": False,
            "is_performing": True,
            "source": "securitisation_spv",
        },
        {
            "id": "in1",
            "currency": "AUD",
            "direction": "inflow",
            "amount": Decimal("40"),
            "contractual_days": Decimal("20"),
            "has_earliest_call": False,
            "earliest_call_days": Decimal("0"),
            "has_latest_extension": False,
            "latest_extension_days": Decimal("0"),
            "is_open_maturity": False,
            "is_minimum_contractual_payment": False,
            "is_non_financial_revenue": False,
            "counted_in_hqla": False,
            "is_performing": True,
            "source": "standard",
        },
        {
            "id": "out2",
            "currency": "AUD",
            "direction": "outflow",
            "amount": Decimal("50"),
            "contractual_days": Decimal("90"),
            "has_earliest_call": False,
            "earliest_call_days": Decimal("0"),
            "has_latest_extension": False,
            "latest_extension_days": Decimal("0"),
            "is_open_maturity": False,
            "is_minimum_contractual_payment": False,
            "is_non_financial_revenue": False,
            "counted_in_hqla": False,
            "is_performing": True,
            "source": "standard",
        },
        # Excluded from LCR inflows (open maturity non-min, extension > 30d, HQLA double-count, non-fin rev)
        {
            "id": "open-full",
            "currency": "AUD",
            "direction": "inflow",
            "amount": Decimal("999"),
            "contractual_days": Decimal("10"),
            "has_earliest_call": False,
            "earliest_call_days": Decimal("0"),
            "has_latest_extension": False,
            "latest_extension_days": Decimal("0"),
            "is_open_maturity": True,
            "is_minimum_contractual_payment": False,
            "is_non_financial_revenue": False,
            "counted_in_hqla": False,
            "is_performing": True,
            "source": "standard",
        },
        {
            "id": "ext-late",
            "currency": "AUD",
            "direction": "inflow",
            "amount": Decimal("999"),
            "contractual_days": Decimal("20"),
            "has_earliest_call": False,
            "earliest_call_days": Decimal("0"),
            "has_latest_extension": True,
            "latest_extension_days": Decimal("400"),
            "is_open_maturity": False,
            "is_minimum_contractual_payment": False,
            "is_non_financial_revenue": False,
            "counted_in_hqla": False,
            "is_performing": True,
            "source": "standard",
        },
        {
            "id": "hqla-in",
            "currency": "AUD",
            "direction": "inflow",
            "amount": Decimal("999"),
            "contractual_days": Decimal("10"),
            "has_earliest_call": False,
            "earliest_call_days": Decimal("0"),
            "has_latest_extension": False,
            "latest_extension_days": Decimal("0"),
            "is_open_maturity": False,
            "is_minimum_contractual_payment": False,
            "is_non_financial_revenue": False,
            "counted_in_hqla": True,
            "is_performing": True,
            "source": "standard",
        },
    ]
    # Remove ext-late and hqla-in from ladder case check or include only Case 1 rows first
    base_cf = cf_rows[:4]
    deriv_rows = [
        {
            "counterparty": "cp1",
            "currency": "AUD",
            "has_valid_mna": True,
            "is_same_day_fx_principal_exchange": False,
            "payable": Decimal("80"),
            "receivable": Decimal("30"),
            "uncounted_hqla_collateral": Decimal("0"),
        },
        {
            "counterparty": "cp2",
            "currency": "AUD",
            "has_valid_mna": False,
            "is_same_day_fx_principal_exchange": True,
            "payable": Decimal("20"),
            "receivable": Decimal("50"),
            "uncounted_hqla_collateral": Decimal("0"),
        },
    ]
    liab_rows = [
        {"id": "l1", "amount": Decimal("100"), "days": Decimal("90")},
        {"id": "l2", "amount": Decimal("200"), "days": Decimal("240")},
        {"id": "l3", "amount": Decimal("300"), "days": Decimal("400")},
    ]
    loan_rows = [
        {"id": "a1", "amount": Decimal("100"), "days": Decimal("180")},
        {"id": "a2", "amount": Decimal("400"), "days": Decimal("730")},
    ]

    cf_tbl, d_tbl, liab_tbl, loan_tbl = make_tables(base_cf, deriv_rows, liab_rows, loan_rows)
    exprs = evaluate_aps210_cashflows(
        cf_tbl, d_tbl, liab_tbl, loan_tbl, Decimal("70"), False, "AUD"
    )
    actual = {k: con.execute(v) for k, v in exprs.items()}
    expected = {
        "min_cum_net_cf": Decimal("-70"),
        "survival_buffer_met": True,
        "sched_outflow_30d": Decimal("60"),
        "spv_outflow_30d": Decimal("60"),
        "sched_inflow_30d": Decimal("40"),
        "lcr_deriv_outflow": Decimal("50"),
        "lcr_deriv_inflow": Decimal("30"),
        "nsfr_asf_lt6m": Decimal("100"),
        "nsfr_asf_6m_to_1y": Decimal("200"),
        "nsfr_asf_ge1y": Decimal("300"),
        "nsfr_loan_lt1y": Decimal("100"),
        "nsfr_loan_ge1y": Decimal("400"),
    }
    for k, exp in expected.items():
        act = actual[k]
        if isinstance(exp, Decimal):
            assert Decimal(act) == exp, f"{k}: got {act}, expected {exp}"
        else:
            assert bool(act) == exp, f"{k}: got {act}, expected {exp}"
    print("PASS cashflows_case1_buffer_70:", actual)

    # Case 2: Buffer 60 < 70 fails survival_buffer_met on base_cf
    cf_tbl_fail, d_tbl_fail, liab_tbl_fail, loan_tbl_fail = make_tables(
        base_cf, deriv_rows, liab_rows, loan_rows
    )
    exprs_fail = evaluate_aps210_cashflows(
        cf_tbl_fail, d_tbl_fail, liab_tbl_fail, loan_tbl_fail, Decimal("60"), False, "AUD"
    )
    actual_fail = {k: con.execute(v) for k, v in exprs_fail.items()}
    assert Decimal(actual_fail["min_cum_net_cf"]) == Decimal("-70")
    assert bool(actual_fail["survival_buffer_met"]) is False
    print("PASS cashflows_case2_buffer_60_fails:", actual_fail)

    # Case 3: ext-late (effective 400d > 30d) and hqla-in (countedInHqla=True) stay excluded from sched_inflow_30d
    cf_tbl2, d_tbl2, liab_tbl2, loan_tbl2 = make_tables(cf_rows, deriv_rows, liab_rows, loan_rows)
    exprs2 = evaluate_aps210_cashflows(
        cf_tbl2, d_tbl2, liab_tbl2, loan_tbl2, Decimal("60"), False, "AUD"
    )
    actual2 = {k: con.execute(v) for k, v in exprs2.items()}
    assert Decimal(actual2["sched_inflow_30d"]) == Decimal("40")
    print("PASS cashflows_case3_lcr_inflow_exclusions:", actual2)


if __name__ == "__main__":
    main()
