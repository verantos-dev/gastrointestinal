fu_dictionary <- c(
  "follow_up_years"                   = "Duration of follow-up (years)",
  "follow_up_ce_months"               = "Duration of continuous enrollment (months) (with allowable gap of 45 days)",
  "specialties_grouped_baseline"      = "Distribution of physician specialty",
  "max_physician_documented_asthma"   = "Maximum physician-documented asthma severity"
)

demographics_dictionary <- c(
  "age_at_index"                      = "Age, years (at index)",
  "age_at_index_group"                = "Age category, years (at index)",
  "Female"                            = "Female",
  "race_group"                        = "Race",
  "ethnicity_group"                   = "Ethnicity"
)

lifestyle_factors_dictionary <- c(
  "smoking"                           = "Documentation of smoking"
)

clinical_characteristics_dictionary <- c(
  "exacerbation"                      = "History of exacerbations",
  "exacerbation_baseline_n"           = "History of (any) exacerbations",
  "inpatient_exacerbation_baseline"   = "Inpatient excerbation",
  "any_biologics"                     = "Biologics received"
)

clinical_characteristics_dictionary_with_ex_types <- c(
  "exacerbation"                      = "History of exacerbations",
  "Outpatient"                        = "History of outpatient exacerbations",
  "Inpatient"                         = "History of inpatient exacerbations",
  "ED/Urgent Care"                    = "History of ED/Urgent Care exacerbations",
  "inpatient_exacerbation_baseline"   = "Inpatient excerbation",
  "inpatient_exacerbation_baseline_count" = "Inpatient exacerbation counts",
  "any_biologics"                     = "Biologics received"
)

treatments_dictionary <- c(
  "Benralizumab"  = "Benralizumab",
  "Dupilumab"     = "Dupilumab",
  "Mepolizumab"   = "Mepolizumab",
  "Omalizumab"    = "Omalizumab",
  "Reslizumab"    = "Reslizumab",
  "Tezepelumab"   = "Tezepelumab"
)


completeness_dictionary <- c(
  "two_sources"  = "Patient-years that have 2+ data sources",
  "three_sources"     = "Patient-years that have 3+ data sources"

)

observation_tables_dictionary <- tibble::tribble(
  ~ "table", ~ "fields",
  "condition_occurrence",                c("condition_start_date", "condition_end_date"),
  "device_exposure",                     c("device_exposure_start_date", "device_exposure_end_date"),
  "drug_exposure",                       c("drug_exposure_start_date", "drug_exposure_end_date"),
  "measurement",                         c("measurement_date"),
  "observation",                         c("observation_date"),
  #"payer_plan_period",                   c("payer_plan_period_start_date","payer_plan_period_end_date"),
  "procedure_occurrence",                c("procedure_date"),
  "visit_occurrence",                    c("visit_start_date", "visit_end_date")
)

tezepelumab_code_list <- c(
  '2587790','2587791','2587795','2587797',
  '2587801','2587804','2587807','2587794',
  '2587800','2587803','2587806','J2356',
  '2628921','2628924','2628920','2628923',
  '55513012301','55513011299','55513011296',
  '55513011201','55513010099','55513010096',
  '55513010001','2587789','2587792','2587793',
  '2587799','2587802','2587805','2628919',
  '2628922','2587798','2587796'
)

age_group_levels <- c("12 to 17",
                      "18 to 29",
                      "30 to 39",
                      "40 to 49",
                      "50 to 59",
                      "60 to 69",
                      "70 to 79",
                      "≥80")

race_levels <- c("White",
                 "Black or African American",
                 "Asian",
                 "Other",
                 "Unknown")

ethnicity_levels <- c("Hispanic or Latino",
                      "Not Hispanic or Latino",
                      # "Other",
                      "Unknown")

gender_levels <- c("Male", "Female", "Unknown")

specialty_levels <- c("Allergist (no pulmonologist but with or without an additional different type of provider)",
                      "Pulmonologist (no allergist but with or without an additional different type of provider)",
                      "Allergist and pulmonologist (with or without a different type of provider)",
                      "General practitioner (no pulmonologist, no allergist, but with or without a different type of provider)",
                      "Other (no pulmonologist, no allergist, no general practitioner)",
                      "Unknown")

severity_levels <- c("Mild", "Mild-to-moderate", "Moderate", "Moderate-to-severe", "Severe", "Not documented")

visit_type_filter_dictionary <- list(
  "any"                 = c("Inpatient Visit", "Emergency Room and Inpatient Visit", "Emergency Room Visit", "Urgent Care Facility", "Urgent Care Visit", "Outpatient Visit"),
  "inpatient"           = c("Inpatient Visit", "Emergency Room and Inpatient Visit"),
  "ed_urgent"           = c("Emergency Room Visit", "Urgent Care Facility", "Urgent Care Visit"),
  "ed_urgent_inpatient" = c("Inpatient Visit", "Emergency Room and Inpatient Visit", "Emergency Room Visit", "Urgent Care Facility", "Urgent Care Visit"),
  "outpatient"          = c("Outpatient Visit"),
  "clinic_treated"      = c("Outpatient Visit"),
  "urgent_only"         = c("Urgent Care Facility", "Urgent Care Visit"),
  "ed_only"             = c("Emergency Room Visit")
)

exacerbation_order <- c("Inpatient",
                        "Inpatient Visit",
                        "Emergency Room and Inpatient Visit",
                        "ED/Urgent Care",
                        "Emergency Room Visit",
                        "Urgent Care Visit",
                        "Outpatient",
                        "Outpatient Visit")

tables_dictionary <- tibble::tribble(
  ~ "table_name", ~ "date_fields", ~ "vocabulary_fields", ~"concept_code_field",
  "condition_occurrence",                c("condition_start_date", "condition_end_date"),  "condition_vocabulary_id", "condition_concept_code",
  "device_exposure",                     c("device_exposure_start_date", "device_exposure_end_date"), "device_vocabulary_id", "device_concept_code",
  "drug_exposure",                       c("drug_exposure_start_date", "drug_exposure_end_date"), "drug_vocabulary_id", "drug_concept_code",
  "measurement",                         c("measurement_date"), "measurement_vocabulary_id", "measurement_concept_code",
  "observation",                         c("observation_date"), "observation_vocabulary_id", "observation_concept_code",
  "payer_plan_period",                   c("payer_plan_period_start_date","payer_plan_period_end_date"), "payer_vocabulary_id", "payer_concept_code",
  "procedure_occurrence",                c("procedure_date"), "procedure_vocabulary_id", "procedure_concept_code",
  "visit_occurrence",                    c("visit_start_date", "visit_end_date"), as.character(NA), as.character(NA),
  "person",                              c(), as.character(NA), as.character(NA),
  "death",                               c("death_date"), "death_type_vocabulary_id", "death_type_concept_code",
  "observation_period",                  c("observation_period_start_date", "observation_period_end_date"), as.character(NA), as.character(NA),
  "provider",                            c(), as.character(NA), as.character(NA),
  "registry_inclusion_person",           c("cohort_eligibility_date"), as.character(NA), as.character(NA),
)


# snowflake_tables_dictionary <- tibble::tribble(
#   ~ table,                               ~ date_start,                   ~concept_field,          ~source_concept_field,  ~ join_to_visit_occurrence, ~optimized_concepts, ~code_field, ~ vocabulary_field, ~fields_to_pull,
#   "condition_occurrence",                "condition_start_date",         "condition_concept_id",  "condition_source_concept_id",     TRUE,  TRUE,   "condition_concept_code",   "condition_vocabulary_id",  c("person_id",
#                                                                                                                                                                                 "condition_occurrence_id",
#                                                                                                                                                                                 "visit_occurrence_id",
#                                                                                                                                                                                 "condition_concept_code",
#                                                                                                                                                                                 "condition_concept_id",
#                                                                                                                                                                                 "condition_vocabulary_id",
#                                                                                                                                                                                 "condition_concept_name",
#                                                                                                                                                                                 "condition_start_date",
#                                                                                                                                                                                 "condition_status_concept_id",
#                                                                                                                                                                                 "condition_status_concept_code",
#                                                                                                                                                                                 "condition_status_concept_name",
#                                                                                                                                                                                 "condition_status_vocabulary_id",
#                                                                                                                                                                                 "condition_type_concept_id",
#                                                                                                                                                                                 "condition_type_concept_code",
#                                                                                                                                                                                 "condition_type_concept_name",
#                                                                                                                                                                                 "condition_type_vocabulary_id",
#                                                                                                                                                                                 "condition_status_source_value",
#                                                                                                                                                                                 "condition_status_source_vocabulary_id",
#                                                                                                                                                                                 "condition_status_source_concept_name",
#                                                                                                                                                                                 "condition_status_source_concept_code",
#                                                                                                                                                                                 "condition_source_concept_id",
#                                                                                                                                                                                 "condition_type_concept_name",
#                                                                                                                                                                                 "is_experienced",
#                                                                                                                                                                                 "is_current",
#                                                                                                                                                                                 "severity_id",
#                                                                                                                                                                                 "dataset_id"),
#   "device_exposure",                     "device_exposure_start_date",   "device_exposure_concept_id", "device_exposure_source_concept_id", TRUE,  TRUE,   "device_concept_code",      "device_vocabulary_id",      c("person_id",
#                                                                                                                                                                                  "visit_occurrence_id",
#                                                                                                                                                                                  "device_exposure_id",
#                                                                                                                                                                                  "visit_occurrence_id,",
#                                                                                                                                                                                  "device_concept_code",
#                                                                                                                                                                                  "device_concept_Id",
#                                                                                                                                                                                  "device_vocabulary_id",
#                                                                                                                                                                                  "device_concept_name",
#                                                                                                                                                                                  "device_exposure_start_date",
#                                                                                                                                                                                  "is_experienced",
#                                                                                                                                                                                  "is_current",
#                                                                                                                                                                                  "dataset_id"),
#   "drug_exposure",                      "drug_exposure_start_date",      "drug_concept_id", "drug_source_concept_id",           TRUE,  TRUE,   "drug_concept_code",        "drug_vocabulary_id",        c("person_id",
#                                                                                                                                                                                  "visit_occurrence_id",
#                                                                                                                                                                                  "drug_concept_code",
#                                                                                                                                                                                  "drug_source_concept_code",
#                                                                                                                                                                                  "drug_concept_id",
#                                                                                                                                                                                  "drug_vocabulary_id",
#                                                                                                                                                                                  "drug_source_vocabulary_id",
#                                                                                                                                                                                  "drug_concept_name",
#                                                                                                                                                                                  "drug_source_concept_code",
#                                                                                                                                                                                  "concept_group_name",
#                                                                                                                                                                                  "drug_exposure_start_date",
#                                                                                                                                                                                  "is_experienced",
#                                                                                                                                                                                  "is_current",
#                                                                                                                                                                                  "days_supply",
#                                                                                                                                                                                  "quantity",
#                                                                                                                                                                                  "dataset_id"),
#   "measurement",                         "measurement_date",             "measurement_concept_id",  "measurement_source_concept_id",   TRUE,  TRUE,   "measurement_concept_code", "measurement_vocabulary_id", c("person_id",
#                                                                                                                                                                                  "measurement_id",
#                                                                                                                                                                                  "visit_occurrence_id",
#                                                                                                                                                                                  "measurement_concept_code",
#                                                                                                                                                                                  "measurement_concept_id",
#                                                                                                                                                                                  "measurement_vocabulary_id",
#                                                                                                                                                                                  "measurement_concept_name",
#                                                                                                                                                                                  "measurement_date",
#                                                                                                                                                                                  "unit_concept_name",
#                                                                                                                                                                                  "value_as_number",
#                                                                                                                                                                                  "is_experienced",
#                                                                                                                                                                                  "is_current",
#                                                                                                                                                                                  "dataset_id"),
#   "observation",                         "observation_date",             "observation_concept_id", "observation_source_concept_id",    TRUE,  TRUE,   "observation_concept_code", "observation_vocabulary_id", c("person_id",
#                                                                                                                                                                                  "observation_id",
#                                                                                                                                                                                  "visit_occurrence_id",
#                                                                                                                                                                                  "observation_concept_code",
#                                                                                                                                                                                  "observation_concept_id",
#                                                                                                                                                                                  "observation_concept_name",
#                                                                                                                                                                                  "observation_vocabulary_id",
#                                                                                                                                                                                  "observation_date",
#                                                                                                                                                                                  "is_experienced",
#                                                                                                                                                                                  "is_current",
#                                                                                                                                                                                  "dataset_id"),
#   "payer_plan_period",                   "payer_plan_period_start_date", "payer_concept_id",  "payer_source_concept_id",         FALSE, FALSE,  "payer_concept_code",       "payer_vocabulary_id",       c("person_id",
#                                                                                                                                                                                  "payer_plan_period_id",
#                                                                                                                                                                                  "payer_concept_code",
#                                                                                                                                                                                  "payer_concept_id",
#                                                                                                                                                                                  "payer_concept_name",
#                                                                                                                                                                                  "payer_vocabulary_id",
#                                                                                                                                                                                  "payer_plan_period_start_date",
#                                                                                                                                                                                  "is_experienced",
#                                                                                                                                                                                  "is_current",
#                                                                                                                                                                                  "dataset_id"),
#   "procedure_occurrence",                "procedure_date",               "procedure_concept_id",  "procedure_source_concept_id",      TRUE,  TRUE,   "procedure_concept_code",   "procedure_vocabulary_id",   c("person_id",
#                                                                                                                                                                                  "procedure_occurrence_id",
#                                                                                                                                                                                  "visit_occurrence_id",
#                                                                                                                                                                                  "procedure_concept_code",
#                                                                                                                                                                                  "procedure_concept_id",
#                                                                                                                                                                                  "procedure_concept_name",
#                                                                                                                                                                                  "procedure_vocabulary_id",
#                                                                                                                                                                                  "procedure_date",
#                                                                                                                                                                                  "is_experienced",
#                                                                                                                                                                                  "is_current",
#                                                                                                                                                                                  "dataset_id"),
#   "visit_occurrence",                    "visit_start_date",             "visit_concept_id",      "visit_source_concept_id",      FALSE,  FALSE, "visit_concept_code",       "visit_vocabulary_id",       c("person_id",
#                                                                                                                                                                                  "visit_occurrence_id",
#                                                                                                                                                                                  "visit_start_date",
#                                                                                                                                                                                  "visit_concept_code",
#                                                                                                                                                                                  "visit_concept_name",
#                                                                                                                                                                                  "visit_source_concept_name",
#                                                                                                                                                                                  "visit_type_concept_id",
#                                                                                                                                                                                  "visit_type_concept_name",
#                                                                                                                                                                                  "visit_concept_id",
#                                                                                                                                                                                  "is_experienced",
#                                                                                                                                                                                  "is_current")
# )
#
#
