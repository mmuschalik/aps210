from decimal import Decimal

import ibis
import pandas as pd

from generated.aps210_compliance_ibis import evaluate_compliance_gate
from generated.aps210_external_ibis import evaluate_external_rules
from generated.aps210_nsfr_ibis import evaluate_nsfr

D = "decimal(38, 12)"

# Every table column of `evaluate_nsfr`, in the generated signature's table order.
SCHEMAS = {
    "asf_items": {
        "category": "string",
        "carrying_value": D,
        "custom_factor": D,
        "is_interdependent_approved": "boolean",
    },
    "rsf_assets": {
        "category": "string",
        "carrying_value": D,
        "encumbrance": "string",
        "custom_factor": D,
        "on_behalf_of_customer_no_guarantee": "boolean",
        "has_exceptional_cb_factor": "boolean",
        "exceptional_cb_factor": D,
        "is_interdependent_approved": "boolean",
        "excluded_from_balance_sheet_rsf": "boolean",
        "has_excess_clf_fallback_override": "boolean",
        "excess_clf_fallback_factor": D,
    },
    "obs_exposures": {"category": "string", "amount": D},
    "deriv_contracts": {
        "netting_set_id": "string",
        "mtm": D,
        "netting_eligible": "boolean",
    },
    "deriv_vm": {
        "netting_set_id": "string",
        "cash_received": D,
        "vm_known": "boolean",
        "is_cash": "boolean",
        "not_segregated_if_non_qccp": "boolean",
        "calculated_and_exchanged_daily_mtm": "boolean",
        "in_contract_mna_or_ccp_currency": "boolean",
        "in_same_settlement_currency_as_derivative": "boolean",
        "fully_extinguishes_mtm_exposure": "boolean",
    },
}

SCALAR_TYPES = {
    "required_min_rate": "rat",
    "variation_margin_posted": "rat",
    "has_clf_limit": "bool",
    "recognised_clf_limit": "rat",
    "host_ala_in_lcr_limit": "rat",
    "internal_nsfr_buffer": "rat",
}

# Canonical rows: every column stated; scenarios override what they test.
ASF = {
    "category": "reg_cap_ex_t2_lt1y",
    "carrying_value": Decimal(0),
    "custom_factor": Decimal(0),
    "is_interdependent_approved": False,
}

RSF = {
    "category": "all_other_assets",
    "carrying_value": Decimal(0),
    "encumbrance": "unencumbered",
    "custom_factor": Decimal(0),
    "on_behalf_of_customer_no_guarantee": False,
    "has_exceptional_cb_factor": False,
    "exceptional_cb_factor": Decimal(0),
    "is_interdependent_approved": False,
    "excluded_from_balance_sheet_rsf": False,
    "has_excess_clf_fallback_override": False,
    "excess_clf_fallback_factor": Decimal(0),
}

ELIGIBLE_VM = {
    "netting_set_id": "",
    "cash_received": Decimal(0),
    "vm_known": True,
    "is_cash": True,
    "not_segregated_if_non_qccp": True,
    "calculated_and_exchanged_daily_mtm": True,
    "in_contract_mna_or_ccp_currency": True,
    "in_same_settlement_currency_as_derivative": True,
    "fully_extinguishes_mtm_exposure": True,
}

BASE_SCALARS = {
    "required_min_rate": Decimal("1.00"),
    "variation_margin_posted": Decimal(0),
    "has_clf_limit": False,
    "recognised_clf_limit": Decimal(0),
    "host_ala_in_lcr_limit": Decimal(0),
    "internal_nsfr_buffer": Decimal(0),
}


def asf(category, value, **kw):
    return {**ASF, "category": category, "carrying_value": Decimal(value), **kw}


def rsf(category, value, **kw):
    return {**RSF, "category": category, "carrying_value": Decimal(value), **kw}


def obs(category, amount):
    return {"category": category, "amount": Decimal(amount)}


def contract(netting_set_id, mtm, netting_eligible):
    return {
        "netting_set_id": netting_set_id,
        "mtm": Decimal(mtm),
        "netting_eligible": netting_eligible,
    }


def vm(netting_set_id, cash_received, **kw):
    return {
        **ELIGIBLE_VM,
        "netting_set_id": netting_set_id,
        "cash_received": Decimal(cash_received),
        **kw,
    }


def tables(asf_items=(), rsf_assets=(), obs_exposures=(), contracts=(), vms=()):
    return {
        "asf_items": list(asf_items),
        "rsf_assets": list(rsf_assets),
        "obs_exposures": list(obs_exposures),
        "deriv_contracts": list(contracts),
        "deriv_vm": list(vms),
    }


def scalars(**kw):
    unknown = set(kw) - set(BASE_SCALARS)
    assert not unknown, unknown
    return {**BASE_SCALARS, **kw}


TESTS_LEAN_7_ASF = [asf("reg_cap_ex_t2_lt1y", 30), asf("stable_retail_sme_lt1y", 100)]
TESTS_LEAN_7_OBS = [
    obs("irrev_or_cond_rev_facility", 100),
    obs("uncond_rev_facility", 100),
]

# (name, tables, scalars, expected). Expected values are from the Lean domain (`evaluateNsfr`).
NSFR_SCENARIOS = [
    # APS210/Tests.lean #7: CLF 10% cap, customer IM exemption, OBS Table 1, derivatives
    # (gross assets 8, gross liabilities 10, VM posted 5 -> 3 + 20% * 10 = 5).
    (
        "clf_cap_and_derivatives",
        tables(
            TESTS_LEAN_7_ASF,
            [
                rsf("std_res_mortgage_lvr_le80_ge1y", 100),
                rsf("unencumbered_hqla1_5pct", 20),
                rsf(
                    "initial_margin_ccp_default",
                    50,
                    encumbrance="one_year_or_more",
                    on_behalf_of_customer_no_guarantee=True,
                ),
                rsf("clf_third_party_debt", 15),
                rsf("self_securitised_clf", 10),
            ],
            TESTS_LEAN_7_OBS,
            [contract("ns_a", 8, False), contract("ns_b", -10, False)],
        ),
        scalars(
            variation_margin_posted=Decimal(5),
            has_clf_limit=True,
            recognised_clf_limit=Decimal(20),
        ),
        {
            "total_asf": Decimal(125),
            "total_rsf": Decimal("83.25"),
            "net_deriv_rsf": Decimal(3),
            "deriv_addon_rsf": Decimal(2),
            "compliant": True,
        },
    ),
    # No CLF limit -> CLF limit 0: third-party CLF debt at 85% (para 36(c)), not 10%.
    (
        "missing_clf_limit_uses_zero",
        tables(
            TESTS_LEAN_7_ASF,
            [rsf("clf_third_party_debt", 100)],
            TESTS_LEAN_7_OBS,
        ),
        scalars(has_clf_limit=False, recognised_clf_limit=Decimal(999)),
        {"total_rsf": Decimal(91)},
    ),
    # Contract-level replacement cost only (no aggregate overrides).
    (
        "deriv_contract_level_only",
        tables(contracts=[contract("ns1", 20, True)]),
        scalars(),
        {"total_rsf": Decimal(20), "net_deriv_rsf": Decimal(20)},
    ),
    # Mixed netting-set eligibility: one ineligible contract -> the set is taken gross.
    (
        "deriv_mixed_mna",
        tables(
            contracts=[
                contract("ns1", 100, True),
                contract("ns1", -90, True),
                contract("ns1", -50, False),
            ]
        ),
        scalars(),
        {"total_rsf": Decimal(28), "deriv_addon_rsf": Decimal(28)},
    ),
    # Para 32: separate pools. Third-party debt limited by the CLF (60): 6 + 40*85% = 40;
    # host ALA by its own limit (30): 3 + 20*85% = 20; self-securitised by CLF less third-party
    # debt (0): 50*85% = 42.5.
    (
        "clf_separate_pools_exhausted",
        tables(
            rsf_assets=[
                rsf("clf_third_party_debt", 100),
                rsf("host_jurisdiction_ala", 50),
                rsf("self_securitised_clf", 50),
            ]
        ),
        scalars(
            has_clf_limit=True,
            recognised_clf_limit=Decimal(60),
            host_ala_in_lcr_limit=Decimal(30),
        ),
        {"total_rsf": Decimal("102.5")},
    ),
    # Third-party debt 20 inside the CLF (60): 2; self-securitised gets the remaining 40:
    # 4 + 10*85% = 12.5; host ALA does not draw on the CLF (limit 0): 10*85% = 8.5.
    (
        "clf_separate_pools_remaining",
        tables(
            rsf_assets=[
                rsf("clf_third_party_debt", 20),
                rsf("self_securitised_clf", 50),
                rsf("host_jurisdiction_ala", 10),
            ]
        ),
        scalars(
            has_clf_limit=True,
            recognised_clf_limit=Decimal(60),
            host_ala_in_lcr_limit=Decimal(0),
        ),
        {"total_rsf": Decimal(23)},
    ),
    # Greedy pool allocation favours the highest fallback factor (custom 100% before default 85%).
    (
        "clf_pool_greedy_by_fallback",
        tables(
            rsf_assets=[
                rsf("clf_third_party_debt", 50),
                rsf(
                    "clf_third_party_debt",
                    50,
                    has_excess_clf_fallback_override=True,
                    excess_clf_fallback_factor=Decimal(1),
                ),
            ]
        ),
        scalars(has_clf_limit=True, recognised_clf_limit=Decimal(50)),
        {"total_rsf": Decimal("47.5")},
    ),
    # Para 28: VM offsets only its own netting set, capped at that set's positive RC (ns1: 100 ->
    # 0, the excess 50 does not reach ns2); VM that does not fully extinguish the exposure, VM with
    # unknown eligibility and VM on a set without contracts give no offset.
    (
        "vm_per_netting_set",
        tables(
            contracts=[
                contract("ns1", 100, True),
                contract("ns2", 50, True),
                contract("ns3", 40, False),
                contract("ns4", 30, True),
            ],
            vms=[
                vm("ns1", 150),
                vm("ns3", 40, fully_extinguishes_mtm_exposure=False),
                vm("ns4", 30, vm_known=False),
                vm("ns9", 500),
            ],
        ),
        scalars(),
        {"net_deriv_rsf": Decimal(120), "total_rsf": Decimal(120)},
    ),
    # VM offset, then derivative liabilities net of VM posted reduce the net derivative RSF.
    (
        "vm_and_liabilities",
        tables(
            contracts=[
                contract("ns1", 100, True),
                contract("ns1", -20, True),
                contract("ns2", -60, True),
            ],
            vms=[vm("ns1", 30)],
        ),
        scalars(variation_margin_posted=Decimal(10)),
        {
            "net_deriv_rsf": Decimal(0),
            "deriv_addon_rsf": Decimal(12),
            "total_rsf": Decimal(12),
        },
    ),
    # An empty calculation (ASF = RSF = 0) is not compliant; positive ASF with RSF = 0 is.
    ("empty_calculation", tables(), scalars(), {"compliant": False}),
    (
        "asf_without_rsf",
        tables(asf_items=[asf("reg_cap_ex_t2_lt1y", 1)]),
        scalars(),
        {"total_rsf": Decimal(0), "compliant": True},
    ),
]

BASE_GATE = {
    "is_locally_incorporated": True,
    "is_lcr_adi": True,
    "apra_higher_min_lcr_aud": Decimal(0),
    "apra_higher_min_lcr_all": Decimal(0),
    "apra_higher_min_mlh": Decimal(0),
    "apra_higher_min_nsfr": Decimal(0),
    "base_governance_ok": True,
    "stress_testing_ok": True,
    "loc_assessed_annually": False,
    "loc_outage_min_days": Decimal(0),
    "loc_covers_tz_holidays": True,
    "in_financial_stress": False,
    "experiencing_severe_stress": False,
    "notified_apra_immediately": False,
    "lcr_aud_compliant": True,
    "lcr_all_compliant": True,
    "mlh_compliant": False,
    "mlh_trigger_valid": False,
    "nsfr_compliant": True,
    "has_level2_group": False,
    "lcr_aud_l2_compliant": False,
    "lcr_all_l2_compliant": False,
    "mlh_l2_compliant": False,
    "nsfr_l2_compliant": False,
    "has_buffer_policy": True,
    "buffer_policy_waived_during_stress": False,
    "all_buffers_met": True,
    "has_custom_buffer_floor": False,
    "custom_buffer_floor_met": True,
    "exclude_lcr_aud": False,
    "exclude_lcr_all": False,
    "exclude_mlh": False,
    "exclude_nsfr": False,
    "has_override_lcr_aud": False,
    "override_lcr_aud": Decimal(0),
    "has_override_lcr_all": False,
    "override_lcr_all": Decimal(0),
    "has_override_mlh": False,
    "override_mlh": Decimal(0),
    "has_override_nsfr": False,
    "override_nsfr": Decimal(0),
    "offshore_subs_all_ge_100pct": True,
    "securitisation_lcr_ok": True,
    "spv_excluded_from_mlh_nsfr": False,
    "may_exclude_spv_from_mlh_nsfr": True,
    "has_apra_adj_or_excl": False,
    "apra_adj_complied": True,
    "seeks_prior_discretion": False,
    "notified_apra_discretion": True,
    "cf_survival_met": True,
    "exclude_lcr_aud_approved": False,
    "exclude_lcr_all_approved": False,
    "exclude_mlh_approved": False,
    "exclude_nsfr_approved": False,
    "override_lcr_aud_approved": False,
    "override_lcr_all_approved": False,
    "override_mlh_approved": False,
    "override_nsfr_approved": False,
}

STRESS = {
    "in_financial_stress": True,
    "experiencing_severe_stress": True,
    "notified_apra_immediately": True,
}

MLH_ADI = {"is_lcr_adi": False, "mlh_compliant": True, "mlh_trigger_valid": True}


def gate(**kw):
    unknown = set(kw) - set(BASE_GATE)
    assert not unknown, unknown
    return {**BASE_GATE, **kw}


# (name, scalars, expected). Expected values are from the Lean domain (`evaluateAdiCompliance`).
GATE_SCENARIOS = [
    (
        "baseline",
        gate(),
        {
            "overall_compliant": True,
            "required_aud_lcr": Decimal("1.00"),
            "required_nsfr": Decimal("1.00"),
        },
    ),
    # Para 71: an exclusion without APRA approval evidence is not applied.
    (
        "unapproved_nsfr_exclusion",
        gate(nsfr_compliant=False, exclude_nsfr=True),
        {"required_nsfr": Decimal("1.00"), "overall_compliant": False},
    ),
    (
        "approved_nsfr_exclusion",
        gate(nsfr_compliant=False, exclude_nsfr=True, exclude_nsfr_approved=True),
        {"required_nsfr": Decimal(0), "overall_compliant": True},
    ),
    # Para 71: an override without APRA approval evidence is not applied.
    (
        "unapproved_lcr_override",
        gate(has_override_lcr_aud=True, override_lcr_aud=Decimal("0.50")),
        {"required_aud_lcr": Decimal("1.00")},
    ),
    (
        "approved_lcr_override",
        gate(
            has_override_lcr_aud=True,
            override_lcr_aud=Decimal("0.50"),
            override_lcr_aud_approved=True,
        ),
        {"required_aud_lcr": Decimal("0.50")},
    ),
    # Paras 54-55: the stress exception covers the LCR only; MLH below its minimum under
    # stress is non-compliant.
    (
        "lcr_below_min_under_stress",
        gate(lcr_aud_compliant=False, all_buffers_met=False, **STRESS),
        {"quantitative_compliant": False},
    ),
    (
        "lcr_below_min_under_stress_waived_buffers",
        gate(
            lcr_aud_compliant=False,
            all_buffers_met=False,
            buffer_policy_waived_during_stress=True,
            **STRESS,
        ),
        {"overall_compliant": True},
    ),
    ("mlh_adi_compliant", gate(**MLH_ADI), {"overall_compliant": True}),
    (
        "mlh_below_min_under_stress",
        gate(**{**MLH_ADI, "mlh_compliant": False}, **STRESS),
        {"overall_compliant": True},
    ),
    (
        "mlh_below_min_under_stress_unnotified",
        gate(
            **{**MLH_ADI, "mlh_compliant": False},
            in_financial_stress=True,
            experiencing_severe_stress=True,
            notified_apra_immediately=False,
        ),
        {"overall_compliant": False},
    ),
    (
        "mlh_unapproved_exclusion",
        gate(**{**MLH_ADI, "mlh_compliant": False}, exclude_mlh=True),
        {"required_mlh": Decimal("0.09"), "overall_compliant": False},
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


def run_nsfr(con, tbls, scal):
    args = [memtable(name, tbls[name]) for name in SCHEMAS]
    return {k: con.execute(v) for k, v in evaluate_nsfr(*args, **scal).items()}


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

    # 1. NSFR
    for name, tbls, scal, expected in NSFR_SCENARIOS:
        res = run_nsfr(con, tbls, scal)
        check(name, res, expected)
        print(f"PASS nsfr_{name}:", res)

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
            lvr_pct=Decimal(75),
            residual_maturity_ge_1y=True,
            days_past_due=Decimal(10),
            unlikely_to_pay=False,
            sme_business_exposure_aud=Decimal(1400000),
        ).items()
    }
    assert Decimal(ext_res["sovereign_rw_pct"]) == Decimal(0), ext_res
    assert Decimal(ext_res["domestic_pse_rw_pct"]) == Decimal(20), ext_res
    assert Decimal(ext_res["bank_rw_pct"]) == Decimal(20), ext_res
    assert bool(ext_res["qualifies_65pct_res_mortgage"]) is True, ext_res
    assert bool(ext_res["is_performing_aps220"]) is True, ext_res
    assert bool(ext_res["qualifies_sme_aps113_para40"]) is True, ext_res
    print("PASS external_rules:", ext_res)

    # 3. Compliance gate
    for name, scal, expected in GATE_SCENARIOS:
        res = {k: con.execute(v) for k, v in evaluate_compliance_gate(**scal).items()}
        check(name, res, expected)
        print(f"PASS compliance_gate_{name}:", res)

    print(
        f"ALL {len(NSFR_SCENARIOS)} NSFR + 1 EXTERNAL + {len(GATE_SCENARIOS)} "
        "COMPLIANCE GATE SCENARIOS PASSED"
    )


if __name__ == "__main__":
    main()
