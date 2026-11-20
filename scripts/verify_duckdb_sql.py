from decimal import Decimal

import duckdb
import pandas as pd


def main():
    with open("generated/aps210_mlh_duckdb.sql") as f:
        sql = f.read()

    con = duckdb.connect()
    con.register(
        "assets",
        pd.DataFrame(
            [
                (
                    "commonwealth_and_semi_govt_securities",
                    Decimal(8),
                    True,
                    True,
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
                    True,
                    True,
                ),
            ],
            columns=[
                "category",
                "market_value",
                "unencumbered_or_apra_approved",
                "rba_repo_eligible",
                "non_subordinated",
                "is_debt_security",
                "liquidatable_within_two_business_days",
                "liquidation_days_ok",
                "apra_approved",
            ],
        ),
    )
    con.register(
        "liabilities",
        pd.DataFrame(
            [
                (True, False, Decimal(100)),
                (True, True, Decimal(50)),
            ],
            columns=[
                "is_on_balance_sheet_or_irrevocable",
                "apra_approved_exclusion",
                "amount",
            ],
        ),
    )

    # Bind named parameters via DuckDB prepared/parameterized dict
    row = con.execute(
        sql.replace(":placements_by_other_adis", "$placements_by_other_adis")
        .replace(":required_min_rate", "$required_min_rate")
        .replace(":trigger_ratio", "$trigger_ratio"),
        {
            "placements_by_other_adis": Decimal(3),
            "required_min_rate": Decimal("0.09"),
            "trigger_ratio": Decimal("0.10"),
        },
    ).fetchone()

    cols = [
        "eligible_liquid_assets",
        "total_liabilities",
        "compliant",
        "trigger_ratio_valid",
        "above_trigger_ratio",
    ]
    res = dict(zip(cols, row))
    assert res["eligible_liquid_assets"] == Decimal(10), res
    assert res["total_liabilities"] == Decimal(100), res
    assert res["compliant"] is True, res
    assert res["trigger_ratio_valid"] is True, res
    assert res["above_trigger_ratio"] is True, res
    print("PASS direct DuckDB SQL:", res)


if __name__ == "__main__":
    main()
