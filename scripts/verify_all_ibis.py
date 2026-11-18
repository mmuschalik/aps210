from decimal import Decimal
import pandas as pd
import ibis
from generated.aps210_nsfr_ibis import evaluate_nsfr
from generated.aps210_external_ibis import evaluate_external_rules
from generated.aps210_compliance_ibis import evaluate_compliance_gate


ASF_SCHEMA = ibis.schema(
    {
        "category": "string",
        "carrying_value": "decimal(38, 12)",
        "custom_factor": "decimal(38, 12)",
        "is_interdependent_approved": "boolean",
    }
)

RSF_SCHEMA = ibis.schema(
    {
        "category": "string",
        "carrying_value": "decimal(38, 12)",
        "encumbrance": "string",
        "custom_factor": "decimal(38, 12)",
        "on_behalf_of_customer_no_guarantee": "boolean",
        "has_exceptional_cb_factor": "boolean",
        "exceptional_cb_factor": "decimal(38, 12)",
        "is_interdependent_approved": "boolean",
        "excluded_from_balance_sheet_rsf": "boolean",
    }
)

OBS_SCHEMA = ibis.schema(
    {
        "category": "string",
        "amount": "decimal(38, 12)",
    }
)


def main():
    con = ibis.duckdb.connect()

    # 1. NSFR: matches APS210/Tests.lean #7 (CLF 10% cap, customer IM exemption, OBS Table 1, derivatives)
    asf_tbl = ibis.memtable(
        pd.DataFrame(
            [
                ("reg_cap_ex_t2_lt1y", Decimal("30"), Decimal("0"), False),
                ("stable_retail_sme_lt1y", Decimal("100"), Decimal("0"), False),
            ],
            columns=list(ASF_SCHEMA.names),
        ),
        schema=ASF_SCHEMA,
    )
    rsf_tbl = ibis.memtable(
        pd.DataFrame(
            [
                (
                    "std_res_mortgage_lvr_le80_ge1y",
                    Decimal("100"),
                    "unencumbered",
                    Decimal("0"),
                    False,
                    False,
                    Decimal("0"),
                    False,
                    False,
                ),
                (
                    "unencumbered_hqla1_5pct",
                    Decimal("20"),
                    "unencumbered",
                    Decimal("0"),
                    False,
                    False,
                    Decimal("0"),
                    False,
                    False,
                ),
                (
                    "initial_margin_ccp_default",
                    Decimal("50"),
                    "one_year_or_more",
                    Decimal("0"),
                    True,
                    False,
                    Decimal("0"),
                    False,
                    False,
                ),
                (
                    "clf_third_party_or_host_ala",
                    Decimal("15"),
                    "unencumbered",
                    Decimal("0"),
                    False,
                    False,
                    Decimal("0"),
                    False,
                    False,
                ),
                (
                    "self_securitised_clf",
                    Decimal("10"),
                    "unencumbered",
                    Decimal("0"),
                    False,
                    False,
                    Decimal("0"),
                    False,
                    False,
                ),
            ],
            columns=list(RSF_SCHEMA.names),
        ),
        schema=RSF_SCHEMA,
    )
    obs_tbl = ibis.memtable(
        pd.DataFrame(
            [
                ("irrev_or_cond_rev_facility", Decimal("100")),
                ("uncond_rev_facility", Decimal("100")),
            ],
            columns=list(OBS_SCHEMA.names),
        ),
        schema=OBS_SCHEMA,
    )

    nsfr_res = {
        k: con.execute(v)
        for k, v in evaluate_nsfr(
            asf_tbl,
            rsf_tbl,
            obs_tbl,
            required_min_rate=Decimal("1.00"),
            gross_deriv_liabilities=Decimal("10"),
            variation_margin_posted=Decimal("5"),
            gross_deriv_assets=Decimal("8"),
            eligible_cash_vm_received=Decimal("0"),
            has_clf_limit=True,
            recognised_clf_limit=Decimal("20"),
        ).items()
    }
    assert Decimal(nsfr_res["total_asf"]) == Decimal("125"), nsfr_res
    assert Decimal(nsfr_res["total_rsf"]) == Decimal("84"), nsfr_res
    assert bool(nsfr_res["compliant"]) is True, nsfr_res
    print("PASS nsfr_clf_cap_and_derivatives:", nsfr_res)

    # 2. External standards rules
    ext_res = {
        k: con.execute(v)
        for k, v in evaluate_external_rules(
            credit_rating_grade="grade1",
            is_aus_govt_or_rba_aud=False,
            is_zero_rw_inst_or_mdb=False,
            is_short_term_bank=False,
            to_natural_person=True,
            is_standard_loan_aps112=True,
            lvr_pct=Decimal("75"),
            residual_maturity_ge_1y=True,
            days_past_due=Decimal("10"),
            unlikely_to_pay=False,
            sme_business_exposure_aud=Decimal("1400000"),
        ).items()
    }
    assert Decimal(ext_res["sovereign_rw_pct"]) == Decimal("0"), ext_res
    assert Decimal(ext_res["domestic_pse_rw_pct"]) == Decimal("20"), ext_res
    assert Decimal(ext_res["bank_rw_pct"]) == Decimal("20"), ext_res
    assert bool(ext_res["qualifies_65pct_res_mortgage"]) is True, ext_res
    assert bool(ext_res["is_performing_aps220"]) is True, ext_res
    assert bool(ext_res["qualifies_sme_aps113_para40"]) is True, ext_res
    print("PASS external_rules:", ext_res)

    # 3. Compliance gate
    comp_res = {
        k: con.execute(v)
        for k, v in evaluate_compliance_gate(
            is_locally_incorporated=True,
            is_lcr_adi=True,
            apra_higher_min_lcr_aud=Decimal("0"),
            apra_higher_min_lcr_all=Decimal("0"),
            apra_higher_min_mlh=Decimal("0"),
            apra_higher_min_nsfr=Decimal("0"),
            base_governance_ok=True,
            stress_testing_ok=True,
            loc_assessed_annually=False,
            loc_outage_min_days=Decimal("0"),
            in_financial_stress=False,
            notified_apra_immediately=False,
            lcr_aud_compliant=True,
            lcr_all_compliant=True,
            mlh_compliant=False,
            mlh_trigger_valid=False,
            nsfr_compliant=True,
            offshore_subs_all_ge_100pct=True,
            securitisation_lcr_ok=True,
        ).items()
    }
    assert bool(comp_res["overall_compliant"]) is True, comp_res
    assert Decimal(comp_res["required_aud_lcr"]) == Decimal("1.00"), comp_res
    assert Decimal(comp_res["required_nsfr"]) == Decimal("1.00"), comp_res
    print("PASS compliance_gate:", comp_res)


if __name__ == "__main__":
    main()
