from decimal import Decimal

import ibis
import pandas as pd

from generated.aps210_mlh_ibis import evaluate_mlh


def run_case(
    con, name, assets, liabilities, placements, min_rate, trigger_ratio, expected
):
    asset_cols = [
        "category",
        "market_value",
        "unencumbered_or_apra_approved",
        "rba_repo_eligible",
        "non_subordinated",
        "liquidatable_within_two_business_days",
        "liquidation_days_ok",
    ]
    liab_cols = [
        "is_on_balance_sheet_or_irrevocable",
        "apra_approved_exclusion",
        "amount",
    ]
    assets_tbl = ibis.memtable(
        pd.DataFrame(assets, columns=asset_cols),
        schema=ibis.schema(
            {
                "category": "string",
                "market_value": "decimal(38, 12)",
                "unencumbered_or_apra_approved": "boolean",
                "rba_repo_eligible": "boolean",
                "non_subordinated": "boolean",
                "liquidatable_within_two_business_days": "boolean",
                "liquidation_days_ok": "boolean",
            }
        ),
    )
    liab_tbl = ibis.memtable(
        pd.DataFrame(liabilities, columns=liab_cols),
        schema=ibis.schema(
            {
                "is_on_balance_sheet_or_irrevocable": "boolean",
                "apra_approved_exclusion": "boolean",
                "amount": "decimal(38, 12)",
            }
        ),
    )
    res = evaluate_mlh(assets_tbl, liab_tbl, placements, min_rate, trigger_ratio)
    actual = {k: con.execute(v) for k, v in res.items()}
    for k, exp_v in expected.items():
        act_v = actual[k]
        if isinstance(exp_v, Decimal):
            assert Decimal(act_v) == exp_v, (
                f"[{name}] {k}: got {act_v}, expected {exp_v}"
            )
        else:
            assert bool(act_v) == exp_v, f"[{name}] {k}: got {act_v}, expected {exp_v}"
    print(f"PASS {name}: {actual}")


def main():
    con = ibis.duckdb.connect()

    # Case 1: Matches APS210/Tests.lean Case 6 (para 2(f) netting & para 3 RBA repo check)
    run_case(
        con,
        "test6_netting_and_rba",
        assets=[
            (
                "commonwealth_and_semi_govt_securities",
                Decimal(8),
                True,
                True,
                True,
                True,
                True,
            ),
            (
                "net_deposits_with_other_adis_two_business_days",
                Decimal(5),
                True,
                True,
                True,
                True,
                True,
            ),
            (
                "adi_bank_bills_cds_and_debt",
                Decimal(10),
                True,
                False,
                True,
                True,
                True,
            ),
        ],
        liabilities=[
            (True, False, Decimal(100)),
            (True, True, Decimal(50)),  # APRA-excluded
        ],
        placements=Decimal(3),
        min_rate=Decimal("0.09"),
        trigger_ratio=Decimal("0.10"),
        expected={
            "eligible_liquid_assets": Decimal(10),
            "total_liabilities": Decimal(100),
            "compliant": True,
            "trigger_ratio_valid": True,
            "above_trigger_ratio": True,
        },
    )

    # Case 2: Placements > 2(f) deposits (clamped at 0), compliant=False (< 9%)
    run_case(
        con,
        "clamped_2f_and_breach",
        assets=[
            (
                "notes_and_coin_and_settlement_funds",
                Decimal(8),
                True,
                False,
                False,
                True,
                True,
            ),
            (
                "net_deposits_with_other_adis_two_business_days",
                Decimal(2),
                True,
                True,
                True,
                True,
                True,
            ),
        ],
        liabilities=[(True, False, Decimal(100))],
        placements=Decimal(5),
        min_rate=Decimal("0.09"),
        trigger_ratio=Decimal("0.10"),
        expected={
            "eligible_liquid_assets": Decimal(8),
            "total_liabilities": Decimal(100),
            "compliant": False,
            "trigger_ratio_valid": True,
            "above_trigger_ratio": False,
        },
    )

    # Case 3: Empty / zero liabilities edge case
    run_case(
        con,
        "zero_liabilities",
        assets=[],
        liabilities=[],
        placements=Decimal(0),
        min_rate=Decimal("0.09"),
        trigger_ratio=Decimal("0.10"),
        expected={
            "eligible_liquid_assets": Decimal(0),
            "total_liabilities": Decimal(0),
            "compliant": True,
            "trigger_ratio_valid": True,
            "above_trigger_ratio": True,
        },
    )


if __name__ == "__main__":
    main()
