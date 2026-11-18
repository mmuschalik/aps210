import Lean2Ibis.IR
import APS210.ExternalStandards

namespace Lean2Ibis.ExternalAndComplianceSpec

/-- External Standards (APS 112/113/220/110/120) scalar classification & eligibility program. -/
def externalProgram : Program :=
  { name := "evaluate_external_rules"
    tableParams := []
    scalarParams :=
      [ ("credit_rating_grade", .str)
      , ("is_aus_govt_or_rba_aud", .bool)
      , ("is_zero_rw_inst_or_mdb", .bool)
      , ("is_short_term_bank", .bool)
      , ("to_natural_person", .bool)
      , ("is_standard_loan_aps112", .bool)
      , ("lvr_pct", .rat)
      , ("residual_maturity_ge_1y", .bool)
      , ("days_past_due", .rat)
      , ("unlikely_to_pay", .bool)
      , ("sme_business_exposure_aud", .rat) ]
    lets :=
      [ ("sovereign_rw_pct", .scalar
          (.ite (.binOp .or (.col "is_aus_govt_or_rba_aud") (.col "is_zero_rw_inst_or_mdb"))
            (.litRat 0 1)
            (.caseStr (.col "credit_rating_grade")
              [ ("grade1", .litRat 0 1)
              , ("grade2", .litRat 20 1)
              , ("grade3", .litRat 50 1)
              , ("grade4", .litRat 100 1)
              , ("grade5", .litRat 100 1)
              , ("grade6", .litRat 150 1) ]
              (.litRat 100 1))))
      , ("domestic_pse_rw_pct", .scalar
          (.caseStr (.col "credit_rating_grade")
            [ ("grade1", .litRat 20 1)
            , ("grade2", .litRat 50 1)
            , ("grade3", .litRat 50 1)
            , ("grade4", .litRat 100 1)
            , ("grade5", .litRat 100 1)
            , ("grade6", .litRat 150 1) ]
            (.litRat 50 1)))
      , ("bank_rw_pct", .scalar
          (.ite (.col "is_short_term_bank")
            (.caseStr (.col "credit_rating_grade")
              [ ("grade1", .litRat 20 1), ("grade2", .litRat 20 1), ("grade3", .litRat 20 1)
              , ("grade4", .litRat 50 1), ("grade5", .litRat 50 1), ("grade6", .litRat 150 1) ]
              (.litRat 20 1))
            (.caseStr (.col "credit_rating_grade")
              [ ("grade1", .litRat 20 1), ("grade2", .litRat 30 1), ("grade3", .litRat 50 1)
              , ("grade4", .litRat 100 1), ("grade5", .litRat 100 1), ("grade6", .litRat 150 1) ]
              (.litRat 50 1))))
      , ("qualifies_65pct_res_mortgage", .scalar
          (.binOp .and (.col "to_natural_person")
            (.binOp .and (.col "is_standard_loan_aps112")
              (.binOp .and (.binOp .le (.col "lvr_pct") (.litRat 80 1))
                (.col "residual_maturity_ge_1y")))))
      , ("is_performing_aps220", .scalar
          (.binOp .and (.binOp .lt (.col "days_past_due") (.litRat 90 1))
            (.unOp .not (.col "unlikely_to_pay"))))
      , ("qualifies_sme_aps113_para40", .scalar
          (.binOp .and (.binOp .le (.litRat 0 1) (.col "sme_business_exposure_aud"))
            (.binOp .lt (.col "sme_business_exposure_aud") (.litRat 1500000 1)))) ]
    outputs :=
      [ ("sovereign_rw_pct", .col "sovereign_rw_pct")
      , ("domestic_pse_rw_pct", .col "domestic_pse_rw_pct")
      , ("bank_rw_pct", .col "bank_rw_pct")
      , ("qualifies_65pct_res_mortgage", .col "qualifies_65pct_res_mortgage")
      , ("is_performing_aps220", .col "is_performing_aps220")
      , ("qualifies_sme_aps113_para40", .col "qualifies_sme_aps113_para40") ] }

/-- Top-level APS 210 compliance & minimum-threshold gate in the Lean-to-Ibis IR. -/
def complianceGateProgram : Program :=
  { name := "evaluate_compliance_gate"
    tableParams := []
    scalarParams :=
      [ ("is_locally_incorporated", .bool)
      , ("is_lcr_adi", .bool)
      , ("apra_higher_min_lcr_aud", .rat)
      , ("apra_higher_min_lcr_all", .rat)
      , ("apra_higher_min_mlh", .rat)
      , ("apra_higher_min_nsfr", .rat)
      , ("base_governance_ok", .bool)
      , ("stress_testing_ok", .bool)
      , ("loc_assessed_annually", .bool)
      , ("loc_outage_min_days", .rat)
      , ("in_financial_stress", .bool)
      , ("notified_apra_immediately", .bool)
      , ("lcr_aud_compliant", .bool)
      , ("lcr_all_compliant", .bool)
      , ("mlh_compliant", .bool)
      , ("mlh_trigger_valid", .bool)
      , ("nsfr_compliant", .bool)
      , ("offshore_subs_all_ge_100pct", .bool)
      , ("securitisation_lcr_ok", .bool) ]
    lets :=
      [ ("required_aud_lcr", .scalar
          (.binOp .max (.litRat 100 100) (.col "apra_higher_min_lcr_aud")))
      , ("required_all_lcr", .scalar
          (.ite (.col "is_locally_incorporated")
            (.binOp .max (.litRat 100 100) (.col "apra_higher_min_lcr_all"))
            (.binOp .max (.litRat 40 100) (.col "apra_higher_min_lcr_all"))))
      , ("required_mlh", .scalar
          (.binOp .max (.litRat 9 100) (.col "apra_higher_min_mlh")))
      , ("required_nsfr", .scalar
          (.binOp .max (.litRat 100 100) (.col "apra_higher_min_nsfr")))
      , ("loc_ok", .scalar
          (.binOp .or (.col "is_locally_incorporated")
            (.binOp .and (.col "loc_assessed_annually") (.binOp .le (.litRat 3 1) (.col "loc_outage_min_days")))))
      , ("stress_ok", .scalar
          (.binOp .or (.unOp .not (.col "is_lcr_adi")) (.col "stress_testing_ok")))
      , ("qualitative_compliant", .scalar
          (.binOp .and (.col "base_governance_ok")
            (.binOp .and (.col "stress_ok") (.col "loc_ok"))))
      , ("stress_breach_permitted", .scalar
          (.binOp .and (.col "in_financial_stress") (.col "notified_apra_immediately")))
      , ("lcr_ok", .scalar
          (.binOp .or (.unOp .not (.col "is_lcr_adi"))
            (.binOp .and
              (.binOp .or (.unOp .not (.col "is_locally_incorporated"))
                (.binOp .or (.col "lcr_aud_compliant") (.col "stress_breach_permitted")))
              (.binOp .or (.col "lcr_all_compliant") (.col "stress_breach_permitted")))))
      , ("mlh_ok", .scalar
          (.binOp .or (.col "is_lcr_adi")
            (.binOp .and (.col "mlh_trigger_valid")
              (.binOp .or (.col "mlh_compliant") (.col "stress_breach_permitted")))))
      , ("nsfr_ok", .scalar
          (.binOp .or (.unOp .not (.binOp .and (.col "is_locally_incorporated") (.col "is_lcr_adi")))
            (.col "nsfr_compliant")))
      , ("quantitative_compliant", .scalar
          (.binOp .and (.col "lcr_ok")
            (.binOp .and (.col "mlh_ok")
              (.binOp .and (.col "nsfr_ok")
                (.binOp .and (.col "offshore_subs_all_ge_100pct") (.col "securitisation_lcr_ok"))))))
      , ("overall_compliant", .scalar
          (.binOp .and (.col "qualitative_compliant") (.col "quantitative_compliant"))) ]
    outputs :=
      [ ("required_aud_lcr", .col "required_aud_lcr")
      , ("required_all_lcr", .col "required_all_lcr")
      , ("required_mlh", .col "required_mlh")
      , ("required_nsfr", .col "required_nsfr")
      , ("qualitative_compliant", .col "qualitative_compliant")
      , ("quantitative_compliant", .col "quantitative_compliant")
      , ("overall_compliant", .col "overall_compliant") ] }

end Lean2Ibis.ExternalAndComplianceSpec
