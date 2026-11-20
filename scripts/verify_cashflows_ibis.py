from __future__ import annotations

import datetime
import sys
from decimal import Decimal
from pathlib import Path

import ibis
import pandas as pd

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "generated"))
from aps210_cashflows_ibis import evaluate_aps210_cashflows

D = "decimal(38, 12)"

# Every table column of `evaluate_aps210_cashflows`, in the generated signature's table order.
SCHEMAS = {
    "cash_flows": {
        "id": "string",
        "currency": "string",
        "side": "string",  # "we_owe" / "owed_to_us"
        "amount": D,
        "settlement_known": "boolean",
        "settlement_days": D,
        "options_known": "boolean",
        "performing": "string",  # reading: "true" / "false" / "unknown" / "refused"
        "is_open_maturity": "boolean",
        "is_minimum_contractual_payment": "boolean",
        "is_non_financial_revenue": "boolean",
        "counted_in_hqla": "boolean",
        "source": "string",  # "standard" / "securitisation_spv" / "derivative"
        "netting_set_id": "string",
        "has_valid_mna": "boolean",
        "is_same_day_fx_principal_exchange": "boolean",
    },
    "cash_flow_options": {
        "id": "string",
        "holder": "string",  # "us" / "counterparty"
        "kind": "string",  # "early_redemption" / "extension"
        "resulting_days": D,
    },
    "collateral": {
        "obligation_id": "string",
        "netting_set_id": "string",
        "side": "string",
        "amount": D,
        "currency": "string",
        "settlement_known": "boolean",
        "settlement_days": D,
        "eligible_for_offset": "string",  # reading
        "already_counted_in_hqla": "string",  # reading
    },
    "nsfr_liability_legs": {"id": "string", "amount": D, "days": D},
    "nsfr_loan_legs": {"id": "string", "amount": D, "days": D},
}

SCALAR_TYPES = {
    "reporting_date": "date",
    "initial_liquid_buffer": "rat",
    "use_currency_filter": "bool",
    "target_currency": "str",
}


def flow(fid, side, amount, days, **kw):
    """A known-dated, known-option, performing standard AUD flow; `kw` overrides columns."""
    row = {
        "id": fid,
        "currency": "AUD",
        "side": side,
        "amount": Decimal(amount),
        "settlement_known": True,
        "settlement_days": Decimal(days),
        "options_known": True,
        "performing": "true",
        "is_open_maturity": False,
        "is_minimum_contractual_payment": False,
        "is_non_financial_revenue": False,
        "counted_in_hqla": False,
        "source": "standard",
        "netting_set_id": "",
        "has_valid_mna": False,
        "is_same_day_fx_principal_exchange": False,
    }
    unknown = set(kw) - set(row)
    assert not unknown, unknown
    return {**row, **kw}


def deriv(fid, side, amount, days, netting_set_id, mna, fx):
    return flow(
        fid,
        side,
        amount,
        days,
        source="derivative",
        netting_set_id=netting_set_id,
        has_valid_mna=mna,
        is_same_day_fx_principal_exchange=fx,
    )


def option(fid, holder, kind, days):
    return {"id": fid, "holder": holder, "kind": kind, "resulting_days": Decimal(days)}


def coll(obligation_id, netting_set_id, side, amount, days, eligible, counted):
    """Collateral movement; `days=None` means the settlement date is not known."""
    return {
        "obligation_id": obligation_id,
        "netting_set_id": netting_set_id,
        "side": side,
        "amount": Decimal(amount),
        "currency": "AUD",
        "settlement_known": days is not None,
        "settlement_days": Decimal(days if days is not None else 0),
        "eligible_for_offset": eligible,
        "already_counted_in_hqla": counted,
    }


def leg(lid, amount, days):
    return {"id": lid, "amount": Decimal(amount), "days": Decimal(days)}


def tables(flows=(), options=(), collateral=(), liab=(), loans=()):
    return {
        "cash_flows": list(flows),
        "cash_flow_options": list(options),
        "collateral": list(collateral),
        "nsfr_liability_legs": list(liab),
        "nsfr_loan_legs": list(loans),
    }


def scalars(buffer, reporting_date=datetime.date(2026, 6, 30), currency=None):
    return {
        "reporting_date": reporting_date,
        "initial_liquid_buffer": Decimal(buffer),
        "use_currency_filter": currency is not None,
        "target_currency": currency or "AUD",
    }


BASE_FLOWS = [
    flow("out1", "we_owe", 60, 5, source="securitisation_spv"),
    flow("in1", "owed_to_us", 40, 20),
    flow("out2", "we_owe", 50, 90),
    # Para 60: open maturity without a minimum contractual payment -> not an inflow
    flow("open-full", "owed_to_us", 999, 10, is_open_maturity=True),
    # MNA netting set: 80 - 30 = 50 outflow; same-day FX principal exchange: 20 - 50 = 30 inflow
    deriv("d1p", "we_owe", 80, 5, "cp1", True, False),
    deriv("d1r", "owed_to_us", 30, 5, "cp1", True, False),
    deriv("d2p", "we_owe", 20, 3, "cp2", False, True),
    deriv("d2r", "owed_to_us", 50, 3, "cp2", False, True),
]
BASE_LIAB = [leg("l1", 100, 90), leg("l2", 200, 240), leg("l3", 300, 400)]
BASE_LOANS = [leg("a1", 100, 180), leg("a2", 400, 730)]

# Ladder (derivatives included): day 3: +30; days 5: -60 -80 +30; day 20: +40; day 90: -50
# -> cumulative 1-7d: -80, 8-30d: -40, 31d-6m: -90.
BASE_EXPECTED = {
    "min_cum_net_cf": Decimal(-90),
    "sched_outflow_30d": Decimal(60),
    "spv_outflow_30d": Decimal(60),
    "sched_inflow_30d": Decimal(40),
    "lcr_deriv_outflow": Decimal(50),
    "lcr_deriv_inflow": Decimal(30),
    "nsfr_asf_lt6m": Decimal(100),
    "nsfr_asf_6m_to_1y": Decimal(200),
    "nsfr_asf_ge1y": Decimal(300),
    "nsfr_loan_lt1y": Decimal(100),
    "nsfr_loan_ge1y": Decimal(400),
}

# (name, tables, scalars, expected). Expected values are from the Lean domain
# (`projectCashFlowLadder`, `deriveLcrDerivativeOutflowsAndInflows`, `split*Cal`).
SCENARIOS = [
    (
        "base_buffer_90",
        tables(BASE_FLOWS, liab=BASE_LIAB, loans=BASE_LOANS),
        scalars(90),
        {**BASE_EXPECTED, "survival_buffer_met": True},
    ),
    (
        "base_buffer_89_fails",
        tables(BASE_FLOWS, liab=BASE_LIAB, loans=BASE_LOANS),
        scalars(89),
        {**BASE_EXPECTED, "survival_buffer_met": False},
    ),
    # Para 60: counterparty extension beyond 30 days; para 31: counted in HQLA; para 66:
    # non-financial revenue -> none is an inflow.
    (
        "lcr_inflow_exclusions",
        tables(
            BASE_FLOWS
            + [
                flow("ext-late", "owed_to_us", 999, 20),
                flow("hqla-in", "owed_to_us", 999, 10, counted_in_hqla=True),
                flow("non-fin", "owed_to_us", 999, 10, is_non_financial_revenue=True),
                flow(
                    "open-min",
                    "owed_to_us",
                    7,
                    10,
                    is_open_maturity=True,
                    is_minimum_contractual_payment=True,
                ),
            ],
            options=[option("ext-late", "counterparty", "extension", 400)],
        ),
        scalars(0),
        {"sched_inflow_30d": Decimal(47)},
    ),
    # Lean2Ibis/CashFlowsSpec.lean end-to-end portfolio.
    (
        "spec_end_to_end",
        tables(
            [
                flow("out1", "we_owe", 60, 5, source="securitisation_spv"),
                flow("in1", "owed_to_us", 40, 20),
                flow("out2", "we_owe", 50, 90),
                flow("ext", "owed_to_us", 999, 20),
                flow("unk", "owed_to_us", 77, 5, options_known=False),
                deriv("d1p", "we_owe", 80, 5, "cp1", True, False),
                deriv("d1r", "owed_to_us", 30, 5, "cp1", True, False),
                deriv("d2p", "we_owe", 20, 3, "cp2", False, True),
                deriv("d2r", "owed_to_us", 50, 3, "cp2", False, True),
                deriv("g1", "we_owe", 40, 5, "cp3", False, False),
            ],
            options=[
                option("out2", "counterparty", "early_redemption", 10),
                option("ext", "counterparty", "extension", 40),
            ],
            collateral=[
                coll("d1p", "cp1", "we_owe", 15, 5, "false", "false"),
                coll("g1", "cp3", "owed_to_us", 10, 5, "true", "false"),
                coll("x", "cp9", "we_owe", 5, None, "false", "false"),
            ],
            liab=BASE_LIAB,
            loans=BASE_LOANS,
        ),
        scalars(130),
        {
            "min_cum_net_cf": Decimal(-130),
            "survival_buffer_met": True,
            "sched_outflow_30d": Decimal(110),
            "spv_outflow_30d": Decimal(60),
            "sched_inflow_30d": Decimal(40),
            "lcr_deriv_outflow": Decimal(100),
            "lcr_deriv_inflow": Decimal(30),
        },
    ),
    # Directed collateral on a gross derivative payment of 40 (Att A fn 5, para 31):
    # only collateral owed to us, known eligible, known not in HQLA and dated within 30 days
    # offsets the payment it secures; collateral we owe is an outflow unless dated after 30 days.
    (
        "collateral_direction",
        tables(
            [
                deriv("g1", "we_owe", 40, 5, "s1", False, False),
                deriv("g2", "we_owe", 40, 5, "s2", False, False),
                deriv("g3", "we_owe", 40, 5, "s3", False, False),
                deriv("g4", "we_owe", 40, 5, "s4", False, False),
                deriv("g5", "we_owe", 40, 5, "s5", False, False),
            ],
            collateral=[
                coll("g1", "s1", "owed_to_us", 10, 5, "true", "false"),  # offsets: 30
                coll("g2", "s2", "owed_to_us", 10, 5, "unknown", "false"),  # 40
                coll("g3", "s3", "owed_to_us", 10, 5, "true", "unknown"),  # 40
                coll("g4", "s4", "owed_to_us", 10, None, "true", "false"),  # 40
                coll("g5", "s5", "owed_to_us", 50, 30, "true", "false"),  # 0
                coll("c1", "s6", "we_owe", 7, None, "false", "false"),  # outflow 7
                coll("c2", "s7", "we_owe", 9, 31, "false", "false"),  # beyond 30 days
                coll("c3", "s8", "we_owe", 3, 30, "refused", "refused"),  # outflow 3
            ],
        ),
        scalars(0),
        {"lcr_deriv_outflow": Decimal(160), "lcr_deriv_inflow": Decimal(0)},
    ),
    # Netted set: collateral we owe adds to the net outflow; eligible collateral owed to us
    # reduces it but never turns it into an inflow.
    (
        "collateral_in_netted_set",
        tables(
            [
                deriv("p", "we_owe", 50, 5, "n1", True, False),
                deriv("r", "owed_to_us", 40, 5, "n1", True, False),
            ],
            collateral=[
                coll("p", "n1", "we_owe", 5, 5, "false", "false"),
                coll("r", "n1", "owed_to_us", 100, 5, "true", "false"),
            ],
        ),
        scalars(0),
        {"lcr_deriv_outflow": Decimal(0), "lcr_deriv_inflow": Decimal(0)},
    ),
    # Att A paras 45 & 60: LCR counts only the counterparty's options; unknown options
    # make what we owe due now and what is owed to us never arrive.
    (
        "options_and_unknown_options",
        tables(
            [
                flow("cp-call", "we_owe", 1, 90),  # counterparty call -> day 10: in
                flow("our-call", "we_owe", 2, 90),  # our call does not count: out
                flow(
                    "cp-ext", "owed_to_us", 4, 20
                ),  # counterparty extension -> day 40: out
                flow(
                    "our-ext", "owed_to_us", 8, 20
                ),  # our extension does not count: in
                flow("unk-owe", "we_owe", 16, 300, options_known=False),  # due now: in
                flow(
                    "unk-owed", "owed_to_us", 32, 5, options_known=False
                ),  # never: out
                flow(
                    "unk-date", "we_owe", 64, 300, settlement_known=False
                ),  # due now: in
                flow(
                    "both", "we_owe", 128, 90
                ),  # call at 31 and at 30 -> earliest 30: in
            ],
            options=[
                option("cp-call", "counterparty", "early_redemption", 10),
                option("our-call", "us", "early_redemption", 10),
                option("cp-ext", "counterparty", "extension", 40),
                option("our-ext", "us", "extension", 40),
                option("both", "counterparty", "early_redemption", 31),
                option("both", "counterparty", "early_redemption", 30),
                option("both", "counterparty", "extension", 500),
            ],
        ),
        scalars(1000),
        {
            "sched_outflow_30d": Decimal(1 + 16 + 64 + 128),
            "sched_inflow_30d": Decimal(8),
        },
    ),
    # Para 54: only an affirmatively performing flow owed to us is an inflow or in the ladder.
    (
        "performing_readings",
        tables(
            [
                flow("t", "owed_to_us", 1, 10, performing="true"),
                flow("f", "owed_to_us", 2, 10, performing="false"),
                flow("u", "owed_to_us", 4, 10, performing="unknown"),
                flow("r", "owed_to_us", 8, 10, performing="refused"),
                flow("o", "we_owe", 10, 10, performing="unknown"),
            ],
        ),
        scalars(9),
        {
            "sched_inflow_30d": Decimal(1),
            "sched_outflow_30d": Decimal(10),
            "min_cum_net_cf": Decimal(-9),
            "survival_buffer_met": True,
        },
    ),
    # Ladder currency filter: the USD outflow is outside the AUD ladder.
    (
        "ladder_currency_filter",
        tables(
            [
                flow("aud", "we_owe", 10, 3),
                flow("usd", "we_owe", 1000, 3, currency="USD"),
            ],
        ),
        scalars(10, currency="AUD"),
        {"min_cum_net_cf": Decimal(-10), "survival_buffer_met": True},
    ),
    # Calendar 6m / 1y boundaries from 2026-08-31: +6m = 2027-02-28 (181 days), +12m = 365 days.
    (
        "nsfr_calendar_boundaries",
        tables(
            liab=[
                leg("l180", 1, 180),
                leg("l181", 2, 181),
                leg("l364", 4, 364),
                leg("l365", 8, 365),
            ],
            loans=[leg("a364", 16, 364), leg("a365", 32, 365)],
        ),
        scalars(0, reporting_date=datetime.date(2026, 8, 31)),
        {
            "nsfr_asf_lt6m": Decimal(1),
            "nsfr_asf_6m_to_1y": Decimal(6),
            "nsfr_asf_ge1y": Decimal(8),
            "nsfr_loan_lt1y": Decimal(16),
            "nsfr_loan_ge1y": Decimal(32),
        },
    ),
    # Leap year: from 2024-02-29, +12m = 2025-02-28 (365 days), +6m = 2024-08-29 (182 days).
    (
        "nsfr_leap_year_boundaries",
        tables(liab=[leg("l181", 1, 181), leg("l182", 2, 182), leg("l365", 4, 365)]),
        scalars(0, reporting_date=datetime.date(2024, 2, 29)),
        {
            "nsfr_asf_lt6m": Decimal(1),
            "nsfr_asf_6m_to_1y": Decimal(2),
            "nsfr_asf_ge1y": Decimal(4),
        },
    ),
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


def main() -> None:
    con = ibis.duckdb.connect()
    for name, tbls, scal, expected in SCENARIOS:
        args = [memtable(t, tbls[t]) for t in SCHEMAS]
        exprs = evaluate_aps210_cashflows(*args, **scal)
        actual = {k: con.execute(v) for k, v in exprs.items()}
        for k, exp in expected.items():
            act = actual[k]
            if isinstance(exp, bool):
                assert bool(act) is exp, f"[{name}] {k}: got {act}, expected {exp}"
            else:
                assert Decimal(act) == exp, f"[{name}] {k}: got {act}, expected {exp}"
        print(f"PASS cashflows_{name}:", actual)
    print(f"ALL {len(SCENARIOS)} CASH-FLOW IBIS/DUCKDB SCENARIOS PASSED")


if __name__ == "__main__":
    main()
