from decimal import Decimal

import ibis
import pandas as pd

from generated.aps210_lcr_ibis import evaluate_lcr

HQLA_SCHEMA = ibis.schema(
    {
        "category": "string",
        "currency": "string",
        "market_value": "decimal(38, 12)",
        "convertible_to_cash_anytime": "boolean",
        "unencumbered_and_controlled": "boolean",
        "control_evidenced": "boolean",
        "not_pledged_or_operational_cost": "boolean",
        "not_rehypothecated_if_received_in_sft": "boolean",
        "freely_transferable_at_level2": "boolean",
        "level2_transferable_cap": "decimal(38, 12)",
        "is_host_recognised_ala": "boolean",
        "issuer_restriction_satisfied": "boolean",
        "recognised_by_regulator": "boolean",
        "became_ineligible_within_30_days": "boolean",
        "days_since_ineligible": "decimal(38, 12)",
        "hqla2a_diversified": "boolean",
        "max_30d_price_decline": "decimal(38, 12)",
        "max_30d_haircut_increase": "decimal(38, 12)",
    }
)

RETAIL_SCHEMA = ibis.schema(
    {
        "currency": "string",
        "balance": "decimal(38, 12)",
        "guarantee_limit": "decimal(38, 12)",
        "is_intermediated": "boolean",
        "intermediated_look_through_met": "boolean",
        "notice_deposit_called": "boolean",
        "residual_maturity_or_notice_gt_30d": "boolean",
        "no_withdrawal_right_or_material_penalty": "boolean",
        "category_tainted_by_early_withdrawal": "boolean",
        "has_host_override": "boolean",
        "host_override_rate": "decimal(38, 12)",
        "is_stable": "boolean",
        "above_guarantee_limit_or_250k": "boolean",
        "is_online_account": "boolean",
        "no_established_relationship": "boolean",
        "is_heavily_rate_driven": "boolean",
    }
)

OUTFLOW_SCHEMA = ibis.schema(
    {
        "category": "string",
        "currency": "string",
        "amount": "decimal(38, 12)",
        "custom_rate": "decimal(38, 12)",
    }
)

INFLOW_SCHEMA = ibis.schema(
    {
        "category": "string",
        "currency": "string",
        "amount": "decimal(38, 12)",
        "is_fully_performing": "boolean",
        "counted_in_hqla": "boolean",
    }
)


def make_tables(con, hqla, retail, outflows, inflows):
    return (
        ibis.memtable(
            pd.DataFrame(hqla, columns=list(HQLA_SCHEMA.names)), schema=HQLA_SCHEMA
        ),
        ibis.memtable(
            pd.DataFrame(retail, columns=list(RETAIL_SCHEMA.names)),
            schema=RETAIL_SCHEMA,
        ),
        ibis.memtable(
            pd.DataFrame(outflows, columns=list(OUTFLOW_SCHEMA.names)),
            schema=OUTFLOW_SCHEMA,
        ),
        ibis.memtable(
            pd.DataFrame(inflows, columns=list(INFLOW_SCHEMA.names)),
            schema=INFLOW_SCHEMA,
        ),
    )


def main():
    con = ibis.duckdb.connect()

    # Case 1: Foreign ADI 40% min LCR + 50% Head Office Facility (matches APS210/Tests.lean #5)
    hqla, retail, outf, inf = make_tables(
        con,
        hqla=[
            (
                "hqla1",
                "AUD",
                Decimal(22),
                True,
                True,
                True,
                True,
                True,
                True,
                Decimal(0),
                False,
                True,
                True,
                False,
                Decimal(0),
                True,
                Decimal(0),
                Decimal(0),
            )
        ],
        retail=[],
        outflows=[("non_op_other", "AUD", Decimal(100), Decimal(0))],
        inflows=[],
    )
    res1 = {
        k: con.execute(v)
        for k, v in evaluate_lcr(
            hqla,
            retail,
            outf,
            inf,
            is_aud_only=False,
            is_level2=False,
            required_min_rate=Decimal("0.40"),
            delta_hqla1=Decimal(0),
            delta_hqla2a=Decimal(0),
            delta_hqla2b=Decimal(0),
            clf_recognised=Decimal(0),
            foreign_ho_facility_eligible=Decimal(30),
        ).items()
    }
    assert Decimal(res1["ho_facility_recognised"]) == Decimal(20), res1
    assert Decimal(res1["lcr_numerator"]) == Decimal(42), res1
    assert bool(res1["compliant"]) is True, res1
    print("PASS lcr_foreign_adi_ho_facility:", res1)

    # Case 2: AUD-only filter + Retail Scorecard (Tables 1 & 2) + 75% Inflow Cap + HQLA2A Haircut
    hqla2, retail2, outf2, inf2 = make_tables(
        con,
        hqla=[
            (
                "hqla1",
                "AUD",
                Decimal(50),
                True,
                True,
                True,
                True,
                True,
                True,
                Decimal(0),
                False,
                True,
                True,
                False,
                Decimal(0),
                True,
                Decimal(0),
                Decimal(0),
            ),
            (
                "hqla2a",
                "AUD",
                Decimal(20),
                True,
                True,
                True,
                True,
                True,
                True,
                Decimal(0),
                False,
                True,
                True,
                False,
                Decimal(0),
                True,
                Decimal(0),
                Decimal(0),
            ),  # 20*0.85 = 17
            (
                "hqla1",
                "USD",
                Decimal(1000),
                True,
                True,
                True,
                True,
                True,
                True,
                Decimal(0),
                False,
                True,
                True,
                False,
                Decimal(0),
                True,
                Decimal(0),
                Decimal(0),
            ),  # filtered out
        ],
        retail=[
            # Stable retail: 100 * 5% = 5
            (
                "AUD",
                Decimal(100),
                Decimal(250000),
                False,
                True,
                False,
                False,
                False,
                False,
                False,
                Decimal(0),
                True,
                False,
                False,
                False,
                False,
            ),
            # Less stable scorecard >= 3 (online +2, >250k +2 = 4): 100 * 25% = 25
            (
                "AUD",
                Decimal(100),
                Decimal(250000),
                False,
                True,
                False,
                False,
                False,
                False,
                False,
                Decimal(0),
                False,
                True,
                True,
                False,
                False,
            ),
        ],
        outflows=[
            (
                "committed_liquidity_non_fin_corp_sov_cb_pse",
                "AUD",
                Decimal(100),
                Decimal(0),
            ),  # 30
            ("non_op_other", "USD", Decimal(500), Decimal(0)),  # filtered out
        ],
        inflows=[
            # 100 * 100% = 100 gross inflow -> capped at 75% of (5 + 25 + 30 = 60) = 45
            ("fi_and_central_bank", "AUD", Decimal(100), True, False),
        ],
    )
    res2 = {
        k: con.execute(v)
        for k, v in evaluate_lcr(
            hqla2,
            retail2,
            outf2,
            inf2,
            is_aud_only=True,
            is_level2=False,
            required_min_rate=Decimal("1.00"),
            delta_hqla1=Decimal(0),
            delta_hqla2a=Decimal(0),
            delta_hqla2b=Decimal(0),
            clf_recognised=Decimal(0),
            foreign_ho_facility_eligible=Decimal(0),
        ).items()
    }
    # Stock = 50 + 17 = 67; Gross outflows = 60; Capped inflows = 45; Net outflows = 15; 67 >= 15 -> compliant
    assert Decimal(res2["stock_of_hqla"]) == Decimal(67), res2
    assert Decimal(res2["gross_outflows"]) == Decimal(60), res2
    assert Decimal(res2["capped_inflows"]) == Decimal(45), res2
    assert Decimal(res2["net_cash_outflows"]) == Decimal(15), res2
    assert bool(res2["compliant"]) is True, res2
    print("PASS lcr_aud_scorecard_and_75pct_cap:", res2)

    # Case 3: $500k stable deposit split at $250k FCS limit ($250k*5% + $250k*10% = $37,500)
    #         + Level 2 non-transferable HQLA capped at level2_transferable_cap
    hqla3, retail3, outf3, inf3 = make_tables(
        con,
        hqla=[
            (
                "hqla1",
                "AUD",
                Decimal(100000),
                True,
                True,
                True,
                True,
                True,
                False,
                Decimal(40000),
                False,
                True,
                True,
                False,
                Decimal(0),
                True,
                Decimal(0),
                Decimal(0),
            ),
        ],
        retail=[
            (
                "AUD",
                Decimal(500000),
                Decimal(250000),
                False,
                True,
                False,
                False,
                False,
                False,
                False,
                Decimal(0),
                True,
                False,
                False,
                False,
                False,
            ),
        ],
        outflows=[],
        inflows=[],
    )
    res3 = {
        k: con.execute(v)
        for k, v in evaluate_lcr(
            hqla3,
            retail3,
            outf3,
            inf3,
            is_aud_only=False,
            is_level2=True,
            required_min_rate=Decimal("1.00"),
            delta_hqla1=Decimal(0),
            delta_hqla2a=Decimal(0),
            delta_hqla2b=Decimal(0),
            clf_recognised=Decimal(0),
            foreign_ho_facility_eligible=Decimal(0),
        ).items()
    }
    assert Decimal(res3["stock_of_hqla"]) == Decimal(40000), res3
    assert Decimal(res3["gross_outflows"]) == Decimal(37500), res3
    assert bool(res3["compliant"]) is True, res3
    print("PASS lcr_500k_fcs_split_and_l2_cap:", res3)

    # Case 4: Called intermediated notice deposit with failed look-through -> $100 outflow (not $0)
    hqla4, retail4, outf4, inf4 = make_tables(
        con,
        hqla=[],
        retail=[
            (
                "AUD",
                Decimal(100),
                Decimal(250000),
                True,  # is_intermediated
                False,  # intermediated_look_through_met = False
                True,  # notice_deposit_called = True
                True,  # residual_maturity_or_notice_gt_30d
                True,  # no_withdrawal_right_or_material_penalty
                False,  # category_tainted_by_early_withdrawal
                False,
                Decimal(0),
                True,
                False,
                False,
                False,
                False,
            ),
        ],
        outflows=[],
        inflows=[],
    )
    res4 = {
        k: con.execute(v)
        for k, v in evaluate_lcr(
            hqla4,
            retail4,
            outf4,
            inf4,
            is_aud_only=False,
            is_level2=False,
            required_min_rate=Decimal("1.00"),
            delta_hqla1=Decimal(0),
            delta_hqla2a=Decimal(0),
            delta_hqla2b=Decimal(0),
            clf_recognised=Decimal(0),
            foreign_ho_facility_eligible=Decimal(0),
        ).items()
    }
    assert Decimal(res4["gross_outflows"]) == Decimal(100), res4
    print("PASS lcr_called_intermediated_deposit:", res4)


if __name__ == "__main__":
    main()
