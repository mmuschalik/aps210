from decimal import Decimal

import ibis
import pandas as pd

from generated.aps210_lcr_ibis import evaluate_lcr

D = "decimal(38, 12)"

# Every table column of `evaluate_lcr`, in the generated signature's table order.
SCHEMAS = {
    "hqla_assets": {
        "category": "string",
        "currency": "string",
        "market_value": D,
        "convertible_to_cash_anytime": "boolean",
        "unencumbered_and_controlled": "boolean",
        "control_evidenced": "boolean",
        "not_pledged_or_operational_cost": "boolean",
        "not_rehypothecated_if_received_in_sft": "boolean",
        "freely_transferable_at_level2": "boolean",
        "level2_transfer_group": "string",
        "is_host_recognised_ala": "boolean",
        "issuer_restriction_satisfied": "boolean",
        "recognised_by_regulator": "boolean",
        "ineligible_days_known": "boolean",
        "days_since_ineligible": D,
        "hqla2a_diversified": "boolean",
        "price_decline_known": "boolean",
        "max_30d_price_decline": D,
        "haircut_increase_known": "boolean",
        "max_30d_haircut_increase": D,
        "is_non_zero_rw_foreign_sov_in_foreign_currency": "boolean",
    },
    "level2_transferable_caps": {"level2_transfer_group": "string", "cap": D},
    "retail_deposits": {
        "customer_id": "string",
        "currency": "string",
        "balance": D,
        "guarantee_limit": D,
        "is_intermediated": "boolean",
        "intermediated_look_through_met": "boolean",
        "notice_deposit_called": "boolean",
        "residual_maturity_or_notice_gt_30d": "boolean",
        "no_withdrawal_right_or_material_penalty": "boolean",
        "no_legal_withdrawal_right_within_30d": "boolean",
        "category_tainted_by_early_withdrawal": "boolean",
        "host_jurisdiction_applies_basel": "boolean",
        "has_host_override": "boolean",
        "host_override_rate": D,
        "is_stable": "boolean",
        # Scorecard readings: "true" / "false" / "unknown" / "refused"; anything but "false" scores.
        "is_online_account": "string",
        "no_established_relationship": "string",
        "is_heavily_rate_driven": "string",
    },
    "other_outflows": {
        "category": "string",
        "currency": "string",
        "amount": D,
        "custom_rate": D,
        "has_apra_alt_amount": "boolean",
        "apra_alt_amount": D,
        "rollover_assumed": D,
    },
    "inflows": {
        "category": "string",
        "currency": "string",
        "amount": D,
        # Reading: only "true" counts.
        "is_fully_performing": "string",
        "counted_in_hqla": "boolean",
    },
    "foreign_sov_currency_caps": {"currency": "string", "cap": D},
}

SCALAR_TYPES = {
    "is_locally_incorporated": "bool",
    "is_aud_only": "bool",
    "is_level2": "bool",
    "required_min_rate": "rat",
    "delta_hqla1": "rat",
    "delta_hqla2a": "rat",
    "delta_hqla2b": "rat",
    "clf_recognised": "rat",
    "foreign_ho_facility_eligible": "rat",
    "has_apra_inflow_cap_override": "bool",
    "apra_inflow_cap_override": "rat",
    "internal_lcr_buffer": "rat",
}

# Canonical rows: every column stated; scenarios override what they test.
ELIGIBLE_HQLA1 = {
    "category": "hqla1",
    "currency": "AUD",
    "market_value": Decimal(0),
    "convertible_to_cash_anytime": True,
    "unencumbered_and_controlled": True,
    "control_evidenced": True,
    "not_pledged_or_operational_cost": True,
    "not_rehypothecated_if_received_in_sft": True,
    "freely_transferable_at_level2": True,
    "level2_transfer_group": "parent",
    "is_host_recognised_ala": False,
    "issuer_restriction_satisfied": True,
    "recognised_by_regulator": True,
    "ineligible_days_known": False,
    "days_since_ineligible": Decimal(0),
    "hqla2a_diversified": True,
    "price_decline_known": True,
    "max_30d_price_decline": Decimal(0),
    "haircut_increase_known": True,
    "max_30d_haircut_increase": Decimal(0),
    "is_non_zero_rw_foreign_sov_in_foreign_currency": False,
}

STABLE_RETAIL = {
    "customer_id": "c1",
    "currency": "AUD",
    "balance": Decimal(0),
    "guarantee_limit": Decimal(250000),
    "is_intermediated": False,
    "intermediated_look_through_met": False,
    "notice_deposit_called": False,
    "residual_maturity_or_notice_gt_30d": False,
    "no_withdrawal_right_or_material_penalty": False,
    "no_legal_withdrawal_right_within_30d": False,
    "category_tainted_by_early_withdrawal": False,
    "host_jurisdiction_applies_basel": False,
    "has_host_override": False,
    "host_override_rate": Decimal(0),
    "is_stable": True,
    "is_online_account": "false",
    "no_established_relationship": "false",
    "is_heavily_rate_driven": "false",
}

OUTFLOW = {
    "category": "non_op_other",
    "currency": "AUD",
    "amount": Decimal(0),
    "custom_rate": Decimal(0),
    "has_apra_alt_amount": False,
    "apra_alt_amount": Decimal(0),
    "rollover_assumed": Decimal(0),
}

INFLOW = {
    "category": "fi_and_central_bank",
    "currency": "AUD",
    "amount": Decimal(0),
    "is_fully_performing": "true",
    "counted_in_hqla": False,
}

BASE_SCALARS = {
    "is_locally_incorporated": True,
    "is_aud_only": False,
    "is_level2": False,
    "required_min_rate": Decimal("1.00"),
    "delta_hqla1": Decimal(0),
    "delta_hqla2a": Decimal(0),
    "delta_hqla2b": Decimal(0),
    "clf_recognised": Decimal(0),
    "foreign_ho_facility_eligible": Decimal(0),
    "has_apra_inflow_cap_override": False,
    "apra_inflow_cap_override": Decimal(0),
    "internal_lcr_buffer": Decimal(0),
}


def hqla(**kw):
    return {**ELIGIBLE_HQLA1, **kw}


def retail(**kw):
    return {**STABLE_RETAIL, **kw}


def outflow(**kw):
    return {**OUTFLOW, **kw}


def inflow(**kw):
    return {**INFLOW, **kw}


def tables(
    hqla_assets=(), caps=(), retail_deposits=(), outflows=(), inflows=(), fx_caps=()
):
    return {
        "hqla_assets": list(hqla_assets),
        "level2_transferable_caps": [
            {"level2_transfer_group": g, "cap": Decimal(c)} for g, c in caps
        ],
        "retail_deposits": list(retail_deposits),
        "other_outflows": list(outflows),
        "inflows": list(inflows),
        "foreign_sov_currency_caps": [
            {"currency": c, "cap": Decimal(v)} for c, v in fx_caps
        ],
    }


def scalars(**kw):
    unknown = set(kw) - set(BASE_SCALARS)
    assert not unknown, unknown
    return {**BASE_SCALARS, **kw}


# (name, tables, scalars, expected). Expected values are from the Lean domain (`evaluateLcr`).
SCENARIOS = [
    # Foreign ADI 40% min LCR + 50% head office facility (APS210/Tests.lean #5).
    (
        "foreign_adi_ho_facility",
        tables(
            hqla_assets=[hqla(market_value=Decimal(22))],
            outflows=[outflow(amount=Decimal(100))],
        ),
        scalars(
            is_locally_incorporated=False,
            required_min_rate=Decimal("0.40"),
            foreign_ho_facility_eligible=Decimal(30),
        ),
        {
            "ho_facility_recognised": Decimal(20),
            "lcr_numerator": Decimal(42),
            "compliant": True,
        },
    ),
    # AUD-only filter + retail scorecard (Tables 1 & 2) + 75% inflow cap + HQLA2A haircut.
    (
        "aud_scorecard_and_75pct_cap",
        tables(
            hqla_assets=[
                hqla(market_value=Decimal(50)),
                hqla(category="hqla2a", market_value=Decimal(20)),  # 20 * 0.85 = 17
                hqla(currency="USD", market_value=Decimal(1000)),  # filtered out
            ],
            retail_deposits=[
                # Stable: 100 * 5% = 5
                retail(customer_id="c1", balance=Decimal(100)),
                # Less stable, online (+2) and no relationship (+1) = 3 -> 25%: 25
                retail(
                    customer_id="c2",
                    balance=Decimal(100),
                    is_stable=False,
                    is_online_account="true",
                    no_established_relationship="true",
                ),
            ],
            outflows=[
                outflow(
                    category="committed_liquidity_non_fin_corp_sov_cb_pse",
                    amount=Decimal(100),
                ),  # 30
                outflow(currency="USD", amount=Decimal(500)),  # filtered out
            ],
            # 100 gross inflow -> capped at 75% of (5 + 25 + 30 = 60) = 45
            inflows=[inflow(amount=Decimal(100))],
        ),
        scalars(is_aud_only=True),
        {
            "stock_of_hqla": Decimal(67),
            "gross_outflows": Decimal(60),
            "capped_inflows": Decimal(45),
            "net_cash_outflows": Decimal(15),
            "compliant": True,
        },
    ),
    # $500k stable deposit split at the $250k FCS limit (250k*5% + 250k*10% = 37,500)
    # + Level 2 non-transferable HQLA limited by its group's cap.
    (
        "fcs_500k_split_and_l2_group_cap",
        tables(
            hqla_assets=[
                hqla(
                    market_value=Decimal(100000),
                    freely_transferable_at_level2=False,
                    level2_transfer_group="sub_nz",
                )
            ],
            caps=[("sub_nz", 40000)],
            retail_deposits=[retail(balance=Decimal(500000))],
        ),
        scalars(is_level2=True),
        {
            "stock_of_hqla": Decimal(40000),
            "gross_outflows": Decimal(37500),
            "compliant": True,
        },
    ),
    # Called intermediated notice deposit with failed look-through -> 100% (not 0).
    (
        "called_intermediated_deposit",
        tables(
            retail_deposits=[
                retail(
                    balance=Decimal(100),
                    is_intermediated=True,
                    intermediated_look_through_met=False,
                    notice_deposit_called=True,
                    residual_maturity_or_notice_gt_30d=True,
                    no_withdrawal_right_or_material_penalty=True,
                )
            ]
        ),
        scalars(),
        {"gross_outflows": Decimal(100), "compliant": False},
    ),
    # Stricter APRA inflow cap (20) + para 9(e) foreign-sovereign HQLA1 currency cap.
    (
        "apra_inflow_cap_and_para9e",
        tables(
            hqla_assets=[
                hqla(
                    currency="USD",
                    market_value=Decimal(120),
                    is_non_zero_rw_foreign_sov_in_foreign_currency=True,
                ),
                # EUR absent from the caps -> capped at 0
                hqla(
                    currency="EUR",
                    market_value=Decimal(50),
                    is_non_zero_rw_foreign_sov_in_foreign_currency=True,
                ),
            ],
            outflows=[outflow(amount=Decimal(100))],
            inflows=[inflow(amount=Decimal(100))],
            fx_caps=[("USD", 60)],
        ),
        scalars(
            has_apra_inflow_cap_override=True, apra_inflow_cap_override=Decimal(20)
        ),
        {
            "stock_of_hqla": Decimal(60),
            "capped_inflows": Decimal(20),
            "net_cash_outflows": Decimal(80),
        },
    ),
    # Paras 10 & 12: HQLA2 needs both stress readings known and within the limit; HQLA1 needs none.
    (
        "hqla2_stress_readings",
        tables(
            hqla_assets=[
                hqla(
                    market_value=Decimal(100), price_decline_known=False
                ),  # HQLA1: 100
                hqla(
                    category="hqla2a",
                    market_value=Decimal(100),
                    price_decline_known=False,
                ),  # 0
                hqla(
                    category="hqla2a",
                    market_value=Decimal(100),
                    haircut_increase_known=False,
                ),  # 0
                hqla(
                    category="hqla2a",
                    market_value=Decimal(20),
                    max_30d_price_decline=Decimal("0.10"),
                    max_30d_haircut_increase=Decimal("0.10"),
                ),  # at the 10% limit: 17
                hqla(
                    category="hqla2a",
                    market_value=Decimal(100),
                    max_30d_price_decline=Decimal("0.100001"),
                ),  # 0
                hqla(
                    category="hqla2b_equity",
                    market_value=Decimal(2),
                    max_30d_price_decline=Decimal("0.40"),
                ),  # at the 40% limit: 1
                hqla(
                    category="hqla2b_rmbs",
                    market_value=Decimal(100),
                    max_30d_haircut_increase=Decimal("0.21"),
                ),  # 0
            ],
            outflows=[outflow(amount=Decimal(100))],
        ),
        scalars(),
        {"stock_of_hqla": Decimal(118), "compliant": True},
    ),
    # Para 25: grace only with a known day count of at most 30.
    (
        "para25_grace_days",
        tables(
            hqla_assets=[
                hqla(
                    market_value=Decimal(30),
                    issuer_restriction_satisfied=False,
                    ineligible_days_known=True,
                    days_since_ineligible=Decimal(30),
                ),  # 30
                hqla(
                    market_value=Decimal(31),
                    issuer_restriction_satisfied=False,
                    ineligible_days_known=True,
                    days_since_ineligible=Decimal(31),
                ),  # 0
                hqla(
                    market_value=Decimal(50),
                    issuer_restriction_satisfied=False,
                    ineligible_days_known=False,
                    days_since_ineligible=Decimal(1),
                ),  # unknown day count: 0
            ],
            outflows=[outflow(amount=Decimal(30))],
        ),
        scalars(),
        {"stock_of_hqla": Decimal(30), "compliant": True},
    ),
    # Para 29: one cap per Level 2 transfer group, applied post-haircut, allocated pro rata;
    # a group without a cap is capped at 0; transferable assets are not capped.
    (
        "l2_group_cap_post_haircut",
        tables(
            hqla_assets=[
                hqla(
                    market_value=Decimal(100),
                    freely_transferable_at_level2=False,
                    level2_transfer_group="g1",
                ),
                hqla(
                    category="hqla2a",
                    market_value=Decimal(100),
                    freely_transferable_at_level2=False,
                    level2_transfer_group="g1",
                ),  # g1 post-haircut 185, cap 92.5 -> 50 + 42.5
                hqla(
                    market_value=Decimal(40),
                    freely_transferable_at_level2=False,
                    level2_transfer_group="g2",
                ),  # no cap for g2 -> 0
                hqla(
                    market_value=Decimal(25),
                    freely_transferable_at_level2=True,
                    level2_transfer_group="g2",
                ),  # transferable -> 25
            ],
            caps=[("g1", "92.5"), ("g1", 200)],  # lowest cap of a group applies
            outflows=[outflow(amount=Decimal(100))],
        ),
        scalars(is_level2=True),
        {"stock_of_hqla": Decimal("117.5"), "compliant": True},
    ),
    # Same assets at Level 1: no Level 2 cap (only the 40% HQLA2 cap: 100 + 85 - (85 - 200/3)).
    (
        "l2_group_cap_ignored_at_level1",
        tables(
            hqla_assets=[
                hqla(
                    market_value=Decimal(100),
                    freely_transferable_at_level2=False,
                    level2_transfer_group="g1",
                ),
                hqla(
                    category="hqla2a",
                    market_value=Decimal(100),
                    freely_transferable_at_level2=False,
                    level2_transfer_group="g1",
                ),
            ],
            caps=[("g1", 0)],
            outflows=[outflow(amount=Decimal(100))],
        ),
        scalars(),
        {"stock_of_hqla": Decimal(500) / 3, "compliant": True},
    ),
    # APG 210: the FCS limit applies to a customer's aggregate balance, insured share pro rata.
    # c1: 200k + 100k stable -> 250k*5% + 50k*10% = 17,500.
    # c2: 100k less stable + 200k stable -> aggregate above the limit: 100k*10% (score 2) = 10,000;
    #     stable insured 200k*250/300 at 5% + remainder at 10% = 11,666.67 (exact 35000/3).
    (
        "fcs_per_customer_aggregate",
        tables(
            retail_deposits=[
                retail(customer_id="c1", balance=Decimal(200000)),
                retail(customer_id="c1", balance=Decimal(100000)),
                retail(customer_id="c2", balance=Decimal(100000), is_stable=False),
                retail(customer_id="c2", balance=Decimal(200000)),
            ]
        ),
        scalars(),
        {"gross_outflows": Decimal(17500) + Decimal(10000) + Decimal(35000) / 3},
    ),
    # Scorecard readings: unknown / refused score as present.
    (
        "scorecard_unknown_scores",
        tables(
            retail_deposits=[
                # unknown online (+2) + refused relationship (+1) = 3 -> 25%
                retail(
                    customer_id="c1",
                    balance=Decimal(100),
                    is_stable=False,
                    is_online_account="unknown",
                    no_established_relationship="refused",
                ),
                # unknown relationship (+1) + unknown rate-driven (+1) = 2 -> 10%
                retail(
                    customer_id="c2",
                    balance=Decimal(100),
                    is_stable=False,
                    no_established_relationship="unknown",
                    is_heavily_rate_driven="unknown",
                ),
            ]
        ),
        scalars(),
        {"gross_outflows": Decimal(35)},
    ),
    # Para 54: only an affirmative performing reading contributes an inflow.
    (
        "inflow_performing_readings",
        tables(
            outflows=[outflow(amount=Decimal(1000))],
            inflows=[
                inflow(amount=Decimal(100), is_fully_performing="true"),
                inflow(amount=Decimal(200), is_fully_performing="false"),
                inflow(amount=Decimal(300), is_fully_performing="unknown"),
                inflow(amount=Decimal(400), is_fully_performing="refused"),
            ],
        ),
        scalars(),
        {"capped_inflows": Decimal(100), "net_cash_outflows": Decimal(900)},
    ),
    # Forward-starting lending: max(committed - rollover, 0) at 100%.
    (
        "forward_starting_lending",
        tables(
            outflows=[
                outflow(
                    category="forward_starting_lending",
                    amount=Decimal(100),
                    rollover_assumed=Decimal(30),
                ),  # 70
                outflow(
                    category="forward_starting_lending",
                    amount=Decimal(100),
                    rollover_assumed=Decimal(150),
                ),  # 0
            ]
        ),
        scalars(),
        {"gross_outflows": Decimal(70)},
    ),
    # An empty calculation (no HQLA, no outflows) is not compliant.
    ("empty_calculation", tables(), scalars(), {"compliant": False}),
    # No outflows but positive HQLA: compliant.
    (
        "no_outflows_positive_hqla",
        tables(hqla_assets=[hqla(market_value=Decimal(1))]),
        scalars(),
        {"net_cash_outflows": Decimal(0), "compliant": True},
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


def run(con, tbls, scal):
    args = [memtable(name, tbls[name]) for name in SCHEMAS]
    return {k: con.execute(v) for k, v in evaluate_lcr(*args, **scal).items()}


def check(name, res, expected):
    for k, exp in expected.items():
        got = res[k]
        if isinstance(exp, bool):
            assert bool(got) is exp, f"[{name}] {k}: got {got}, expected {exp}"
        else:
            assert abs(Decimal(got) - exp) <= Decimal("1e-9"), (
                f"[{name}] {k}: got {got}, expected {exp}"
            )


def main():
    con = ibis.duckdb.connect()
    for name, tbls, scal, expected in SCENARIOS:
        res = run(con, tbls, scal)
        check(name, res, expected)
        print(f"PASS lcr_{name}:", res)
    print(f"ALL {len(SCENARIOS)} LCR IBIS/DUCKDB SCENARIOS PASSED")


if __name__ == "__main__":
    main()
