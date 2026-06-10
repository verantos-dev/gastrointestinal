# Initialize ===================================================================
# https://verantos.atlassian.net/browse/SALES-101

# cohort and AD were created in research assistant - pulling in here

dataset <- read.csv(here::here("data/SCIENCE-101/dataset.csv"))

data_dict <- read.csv(here::here("data/SCIENCE-101/data_dictionary.csv"))

# create tables ================================================================

# build list of labels from data dictionary
label_list <- data_dict %>%
  filter(COLUMN_NAME_corrected %in% names(dataset)) %>%
  select(COLUMN_NAME_corrected, NAME) %>%
  deframe() %>%          # named character vector: name = col, value = label
  as.list()

table_demo <- dataset %>%
  mutate(index_year = year(mdy(index_date)),
         gender = factor(case_when(gender == "FEMALE" ~ "Female",
                                   gender == "MALE" ~ "Male",
                                   TRUE ~ "Unknown/Other"),
                         levels = c("Male", "Female", "Unknown/Other")),
         race = factor(case_when(race == "No matching concept" ~ "Unknown/Other",
                                   TRUE ~ race),
                         levels = c("White", "Black or African American", "Asian", "Unknown/Other")),
         ethnicity = factor(case_when(ethnicity == "No matching concept" ~ "Unknown",
                                 TRUE ~ ethnicity),
                       levels = c("Not Hispanic or Latino", "Hispanic or Latino", "Unknown"))) %>%
  tbl_summary(include = c(index_year,
                          age_index,
                          gender,
                          race,
                          ethnicity),
            label = list(index_year ~ "Year of index",
                         age_index ~ "Age at index",
                         gender ~ "Sex",
                         race ~ "Race",
                         ethnicity ~ "Ethnicity"),
            type = list(index_year ~ "categorical"))
# bunch of index dates prior to 2015 implemented in RA - waiting to see what i should do
table_demo


# helper: merge the 30-day and 60-day versions of a set of variables
# into one table with side-by-side spanning columns
merge_pre_proc <- function(data, stems, labels) {
  make_tbl <- function(window) {
    data %>%
      select(all_of(paste0(stems, window))) %>%
      rename_with(~ sub("_(30|60)d$", "", .x)) %>%   # strip window so 30d/60d align
      tbl_summary(label = labels)
  }
  tbl_merge(
    list(make_tbl("_30d"), make_tbl("_60d")),
    tab_spanner = c("**30 days pre-procedure**", "**60 days pre-procedure**")
  )
}

table_labs_indices <- merge_pre_proc(
  dataset,
  stems = c("crp_or_calprotectin_pre_fibrosis_procedure",
            "cdai_or_hbi_pre_fibrosis_procedure"),
  labels = list(
    crp_or_calprotectin_pre_fibrosis_procedure ~ "CRP or calprotectin",
    cdai_or_hbi_pre_fibrosis_procedure          ~ "CDAI or HBI"
  )
)

table_symptoms <- merge_pre_proc(
  dataset,
  stems = c("symptom_abdominal_mass_pre_fibrosis_proc",
            "symptom_abdominal_pain_pre_fibrosis_proc",
            "symptom_arthritis_pre_fibrosis_proc",
            "symptom_diarrhea_pre_fibrosis_proc",
            "symptom_erythema_nodosum_pre_fibrosis_proc",
            "symptom_fatigue_pre_fibrosis_proc",
            "symptom_iritis_pre_fibrosis_proc",
            "symptom_perianal_disease_pre_fibrosis_proc",
            "symptom_pyoderma_pre_fibrosis_proc"),
  labels = list(
    symptom_abdominal_mass_pre_fibrosis_proc    ~ "Abdominal mass",
    symptom_abdominal_pain_pre_fibrosis_proc    ~ "Abdominal pain",
    symptom_arthritis_pre_fibrosis_proc         ~ "Arthritis",
    symptom_diarrhea_pre_fibrosis_proc          ~ "Diarrhea",
    symptom_erythema_nodosum_pre_fibrosis_proc  ~ "Erythema nodosum",
    symptom_fatigue_pre_fibrosis_proc           ~ "Fatigue",
    symptom_iritis_pre_fibrosis_proc            ~ "Iritis",
    symptom_perianal_disease_pre_fibrosis_proc  ~ "Perianal disease",
    symptom_pyoderma_pre_fibrosis_proc          ~ "Pyoderma"
  )
)

table_ip_flare <- dataset %>%
  tbl_summary(include = inpatient_hosp_crohns_flare,
              label = label_list)

table_2 <- tbl_stack(tbls = list(table_labs_indices, table_symptoms),
                     group_header = c("Laboratory measures and clinical scoring measures",
                                      "CDAI-related symptoms")) %>%
  as_gt() %>%
  gt::tab_style(
    style = gt::cell_text(weight = "bold"),
    locations = gt::cells_row_groups(groups = everything())
  )

table_2

sales_101 <- list(table_demo, table_2, table_ip_flare)
write_rds(sales_101, "data/SCIENCE-101/sales_101.rds")
# FOR RMD REPORT:
# add definition of index
# add list of procedure codes/types included
# add description of how inpatient hospital stay with flare was defined

# QC cohort inclusion =============================================
# ---- parameters -------------------------------------------------------------
db_name_export <- "EXPORT"
schema_name    <- "IBD_PRAGMATIC_REGISTRY_2026Q1"
study_start    <- as.Date("2015-01-01")
study_end      <- as.Date("2026-01-08")
sandbox_schema <- paste0(toupper(Sys.info()["user"]), "_SANDBOX")

crohns_group_id <- "2146300387770138038-Verantos"
uc_group_id     <- "9056166393484755007-Verantos"
ibd_unspec_ids  <- c(4074815L, 4341633L)   # IBD (24526004), Indeterminate colitis (235746007)

# The generated SQL had a bug where the CONDITION_START_DATE study-period filter
# was NOT being enforced. Set FALSE to mirror the SQL *as it actually ran*
# (apples-to-apples reconciliation). Flip to TRUE once the generator is fixed.
enforce_study_window <- TRUE
apply_window <- function(tbl) {
  if (enforce_study_window) {
    dplyr::filter(tbl, condition_start_date >= study_start,
                  condition_start_date <= study_end)
  } else {
    tbl
  }
}

con <- verantos::snowflake_connect(
  db_name      = "ANALYSIS",
  db_warehouse = "DEV_WH",
  db_role      = "DATA_DEV"
)

# convenience tbls
co_tbl     <- tbl(con, in_catalog(db_name_export, schema_name, "CONDITION_OCCURRENCE")) %>% rename_with(tolower)
person_tbl <- tbl(con, in_catalog(db_name_export, schema_name, "PERSON")) %>% rename_with(tolower)
cgc_tbl    <- tbl(con, in_catalog("TOOLING", "PLATFORM", "ALL_CONCEPT_GROUP_CONCEPT")) %>% rename_with(tolower)

group_ids <- function(group_id) {
  cgc_tbl %>% filter(omop_concept_group_id == group_id) %>% distinct(concept_id)
}

# =============================================================================
# (A) RECONSTRUCT EACH ANALYTICAL VARIABLE IN R
# =============================================================================

# --- person spine + derived birth date (mirror DERIVED_BIRTH_DATE waterfall) -
# NOTE: person_id is a 64-bit id (~19 digits) and MUST be cast to character
# INSIDE the lazy query (TO_CHAR in Snowflake) so it never passes through an R
# double. Casting after collect() is too late — precision is already lost.
person <- person_tbl %>%
  mutate(person_id = as.character(person_id)) %>%
  select(person_id, birth_datetime, day_of_birth, month_of_birth, year_of_birth,
         gender_concept_name, race_concept_name, ethnicity_concept_name) %>%
  collect()

build_derived_birth <- function(bd, d, m, y) {
  # Snowflake may hand back numeric/character; %04d needs integers
  d <- suppressWarnings(as.integer(d))
  m <- suppressWarnings(as.integer(m))
  y <- suppressWarnings(as.integer(y))
  out <- as.Date(bd)
  # full d/m/y
  i <- is.na(out) & !is.na(d) & !is.na(m) & !is.na(y)
  out[i] <- as.Date(sprintf("%04d-%02d-%02d", y[i], m[i], d[i]))
  # d + y (month -> June)
  i <- is.na(out) & !is.na(d) & is.na(m) & !is.na(y)
  out[i] <- as.Date(sprintf("%04d-06-%02d", y[i], d[i]))
  # m + y (day -> 15)
  i <- is.na(out) & !is.na(m) & is.na(d) & !is.na(y)
  out[i] <- as.Date(sprintf("%04d-%02d-15", y[i], m[i]))
  # y only -> June 15
  i <- is.na(out) & is.na(m) & is.na(d) & !is.na(y)
  out[i] <- as.Date(sprintf("%04d-06-15", y[i]))
  out
}

person <- person %>%
  mutate(
    birth_dt           = as.Date(birth_datetime),
    derived_birth_date = build_derived_birth(birth_datetime, day_of_birth, month_of_birth, year_of_birth)
  )

# --- AV1: crohns_disease_index_date ------------------------------------------
crohns_index_r <- co_tbl %>%
  inner_join(group_ids(crohns_group_id), by = c("condition_concept_id" = "concept_id")) %>%
  apply_window() %>%
  group_by(person_id) %>%
  summarise(index_date = min(condition_start_date, na.rm = TRUE), .groups = "drop") %>%
  mutate(person_id = as.character(person_id)) %>%   # cast in-DB, before collect()
  collect() %>%
  mutate(index_date = as.Date(index_date))

cli_alert_info("AV1 crohns index: {nrow(crohns_index_r)} patients")

# --- AV2: age_gte_18_at_index_v2 (year-based, BIRTH_DATETIME only) -----------
mmdd <- function(d) as.integer(format(d, "%m%d"))
age_gte18_r <- crohns_index_r %>%
  inner_join(person %>% select(person_id, birth_dt, derived_birth_date), by = "person_id") %>%
  mutate(
    # SQL: DATEDIFF(year, birth, index) - (MMDD(birth) > MMDD(index) ? 1 : 0)
    age_v2_gate = (as.integer(format(index_date, "%Y")) - as.integer(format(birth_dt, "%Y"))) -
      ifelse(mmdd(birth_dt) > mmdd(index_date), 1L, 0L),
    flag_gate   = !is.na(age_v2_gate) & age_v2_gate >= 18,   # null birth_dt -> FALSE
    # SQL INDEX_DATE CTE: day-based on DERIVED birth date
    age_derived = floor(as.numeric(difftime(index_date, derived_birth_date, units = "days")) / 365.25)
  )

cli_alert_info("AV2 age gate TRUE: {sum(age_gte18_r$flag_gate)} of {nrow(age_gte18_r)}")

# --- AV3: ulcerative_colitis_any_history -------------------------------------
uc_persons <- co_tbl %>%
  inner_join(group_ids(uc_group_id), by = c("condition_concept_id" = "concept_id")) %>%
  apply_window() %>%
  distinct(person_id) %>%
  mutate(person_id = as.character(person_id)) %>%   # cast in-DB, before collect()
  collect()

# --- AV4: ibd_unspecified_any_history ----------------------------------------
ibd_unspec_persons <- co_tbl %>%
  filter(condition_concept_id %in% ibd_unspec_ids) %>%
  apply_window() %>%
  distinct(person_id) %>%
  mutate(person_id = as.character(person_id)) %>%   # cast in-DB, before collect()
  collect()

cli_alert_info("AV3 UC history: {nrow(uc_persons)} | AV4 IBD-unspec history: {nrow(ibd_unspec_persons)}")

# =============================================================================
# (B) RECONSTRUCT THE FINAL COHORT IN R
#     INCLUDE = age gate TRUE; EXCLUDE = UC or IBD-unspec; FINAL = INCLUDE \ EXCLUDE
#     with non-null index date (and, per INDEX_DATE CTE, non-null derived birth).
# =============================================================================
include_ids <- age_gte18_r %>% filter(flag_gate) %>% pull(person_id)
exclude_ids <- union(uc_persons$person_id, ibd_unspec_persons$person_id)

final_r <- person %>%
  filter(person_id %in% include_ids, !person_id %in% exclude_ids) %>%
  inner_join(crohns_index_r, by = "person_id") %>%          # index_date not null
  filter(!is.na(derived_birth_date)) %>%                    # INDEX_DATE CTE guard
  mutate(age_at_index = floor(as.numeric(difftime(index_date, derived_birth_date, units = "days")) / 365.25)) %>%
  distinct(person_id, index_date, age_at_index,
           gender_concept_name, race_concept_name, ethnicity_concept_name)

cli_h2("(B) R final cohort")
cli_alert_info("R cohort: {nrow(final_r)} patients")

# =============================================================================
# (C) LOAD THE SAVED SQL COHORT OUTPUT AND RECONCILE
#     The cohort produced by the SQL definition is saved to CSV
#     (FINAL_SUBJECTS_STATEMENT columns: person_id, index_date, age_at_index,
#      gender_concept_name, race_concept_name, ethnicity_concept_name).
# =============================================================================
cohort_csv_path <- here::here("data", "SCIENCE-101", "ra_cohort_crohns_reprocess.csv")
stopifnot(file.exists(cohort_csv_path))

# Read EVERYTHING as character so the 64-bit subject_id keeps all 19 digits
# (readr would otherwise guess double and round it). Coerce the rest afterward.
sql_final <- readr::read_csv(cohort_csv_path, show_col_types = FALSE,
                             col_types = readr::cols(.default = readr::col_character())) %>%
  rename_with(tolower) %>%
  mutate(
    person_id    = subject_id,                 # exact string id (no double round-trip)
    index_date   = lubridate::mdy(index_date), # "mm/dd/yy"
    age_at_index = as.integer(age_at_index)
  )

# person_id is already an exact character string on the R side (cast in-DB above).

cli_alert_info("Loaded SQL cohort CSV: {nrow(sql_final)} rows from {basename(cohort_csv_path)}")

cli_h2("(C) Reconcile R vs SQL cohort")
cli_alert_info("SQL cohort: {nrow(sql_final)} | R cohort: {nrow(final_r)}")

only_in_sql <- anti_join(sql_final, final_r, by = "person_id")
only_in_r   <- anti_join(final_r, sql_final, by = "person_id")
cli_alert_info("Persons only in SQL: {nrow(only_in_sql)} | only in R: {nrow(only_in_r)}")

# index_date and age_at_index agreement on the shared persons
shared <- inner_join(
  sql_final %>% select(person_id, index_date, age_at_index) %>%
    rename(index_date_sql = index_date, age_sql = age_at_index),
  final_r %>% select(person_id, index_date, age_at_index) %>%
    rename(index_date_r = index_date, age_r = age_at_index),
  by = "person_id"
) %>%
  mutate(index_match = index_date_sql == index_date_r,
         age_match   = age_sql == age_r)

cli_alert_info("Shared persons: {nrow(shared)} | index_date matches: {sum(shared$index_match)} | age matches: {sum(shared$age_match)}")
if (any(!shared$index_match)) print(head(shared %>% filter(!index_match), 20))
if (any(!shared$age_match))   print(head(shared %>% filter(!age_match), 20))

if (nrow(only_in_sql) == 0 && nrow(only_in_r) == 0 && all(shared$index_match) && all(shared$age_match)) {
  cli_alert_success("EXACT MATCH: identical cohort membership, index dates and ages")
} else {
  cli_alert_danger("Differences found — see breakdowns above and gotcha checks below")
}

# =============================================================================
# (D) GOTCHA CHECKS — "is this doing what I want?"
# =============================================================================
cli_h2("(D) Structural checks")

# G1: inclusion-gate age vs reported derived age — how often do they disagree?
age_compare <- age_gte18_r %>%
  filter(flag_gate, !person_id %in% exclude_ids, !is.na(derived_birth_date)) %>%
  mutate(disagree = age_v2_gate != age_derived)
cli_alert_info("G1 gate-age vs derived-age disagreements among included: {sum(age_compare$disagree, na.rm = TRUE)}")
# the dangerous case: admitted by gate (>=18) but derived age reported < 18
reported_under18 <- age_compare %>% filter(age_derived < 18)
if (nrow(reported_under18)) {
  cli_alert_danger("G1 {nrow(reported_under18)} included patients have REPORTED age_at_index < 18 (gate used birth_datetime, report uses derived date)")
  print(head(reported_under18 %>% select(person_id, birth_dt, derived_birth_date, index_date, age_v2_gate, age_derived), 20))
} else {
  cli_alert_success("G1 no included patient has reported age_at_index < 18")
}

# G2: patients with a Crohn's index but NULL birth_datetime (dropped by the gate)
null_bd <- crohns_index_r %>%
  inner_join(person %>% select(person_id, birth_dt, derived_birth_date), by = "person_id") %>%
  filter(is.na(birth_dt))
null_bd_recoverable <- null_bd %>%
  filter(!is.na(derived_birth_date)) %>%
  mutate(age_derived = floor(as.numeric(difftime(index_date, derived_birth_date, units = "days")) / 365.25)) %>%
  filter(age_derived >= 18)
cli_alert_info("G2 Crohn's patients with NULL birth_datetime: {nrow(null_bd)}")
cli_alert_warning("G2 of those, would be >=18 via derived birth date but are EXCLUDED by the gate: {nrow(null_bd_recoverable)}")

# G3: confirm exclusion is any-history (not index-anchored) — informational
cli_alert_info("G3 exclusion is any-history over [{study_start}, {study_end}], not index-anchored (by design in SQL)")

# G4: EXCEPT behaves as person-level set difference (demographics from PERSON
# are identical between INCLUDE and EXCLUDE, so no row survives for an excluded
# person). Verify no excluded person leaks into the SQL cohort.
leaked <- sql_final %>% filter(person_id %in% as.character(exclude_ids))
if (nrow(leaked) == 0) {
  cli_alert_success("G4 no excluded (UC/IBD-unspec) person present in SQL cohort")
} else {
  cli_alert_danger("G4 {nrow(leaked)} excluded persons leaked into SQL cohort")
}

# basic assertions on the SQL output
cli_h2("Assertions on SQL cohort output")
chk <- list(
  unique_person_rows = nrow(sql_final) == dplyr::n_distinct(sql_final$person_id),
  index_not_null     = all(!is.na(sql_final$index_date)),
  age_non_negative   = all(sql_final$age_at_index >= 0, na.rm = TRUE)
)
# Only assert the window when it's supposed to be enforced; with the SQL bug
# active (enforce_study_window = FALSE) out-of-window index dates are expected.
if (enforce_study_window) {
  chk$index_in_window <- all(sql_final$index_date >= study_start & sql_final$index_date <= study_end)
} else {
  oob <- sum(sql_final$index_date < study_start | sql_final$index_date > study_end)
  cli_alert_warning("Window NOT enforced (matching SQL bug): {oob} cohort rows have an out-of-window index date")
}
for (nm in names(chk)) {
  if (isTRUE(chk[[nm]])) cli_alert_success("PASS: {nm}") else cli_alert_danger("FAIL: {nm}")
}

cli_h1("IBD cohort validation complete")


# PROCEDURES VALIDATION ========================================================
# ---- parameters -------------------------------------------------------------
db_name_export  <- "EXPORT"
schema_name     <- "IBD_PRAGMATIC_REGISTRY_2026Q1"
dataset         <- paste(db_name_export, schema_name, sep = ".")  # {dataset}
study_start     <- as.Date("2015-01-01")                          # {study_start}
study_end       <- as.Date("2026-01-08")                          # {study_end}
km_db           <- "KNOWLEDGE_MANAGEMENT"
km_schema       <- "OMOP_METADATA"
sandbox_schema  <- paste0(toupper(Sys.info()["user"]), "_SANDBOX")
index_src_table <- "FIBROSIS_INDEX_SRC"
sql_out_table   <- "FIBROSIS_PROC_SQL"

if (!exists("con")) {
  con <- verantos::snowflake_connect(db_name = "ANALYSIS", db_warehouse = "DEV_WH", db_role = "DATA_DEV")
}
if (!exists("km")) {
  km <- verantos::snowflake_connect(db_name = "KNOWLEDGE_MANAGEMENT", db_warehouse = "DEV_WH",
                                    db_role = "DATA_DEV", use_schema = "OMOP_METADATA")
}

# ---- index source = validated cohort ----------------------------------------
if (!exists("final_r")) {
  cli_abort(c("`final_r` not found.",
              "i" = "Run validate_ibd_cohort.R first so the validated cohort index dates are available."))
}
index_dates <- final_r %>%
  transmute(person_id = as.character(person_id), value = as.Date(index_date))

cli_alert_info("Index source = validated cohort: {nrow(index_dates)} subjects")

# =============================================================================
# (0) CONCEPT CATALOG CROSS-CHECK
# =============================================================================
fibrosis_concept_ids <- tibble::tribble(
  ~concept_id, ~procedure_category,
  2109120L,  "Strictureplasty",
  4120978L,  "Strictureplasty",
  4018004L,  "Strictureplasty",
  4123904L,  "Endoscopic balloon dilation",
  4125168L,  "Endoscopic balloon dilation",
  46257755L, "Endoscopic balloon dilation",
  2109202L,  "Endoscopic balloon dilation",
  46257754L, "Endoscopic balloon dilation",
  2109189L,  "Endoscopic balloon dilation",
  3657464L,  "Endoscopic balloon dilation",
  2109122L, "Crohns resection", 2109123L, "Crohns resection",
  2109029L, "Crohns resection", 2109028L, "Crohns resection",
  2109030L, "Crohns resection", 2109075L, "Crohns resection",
  2109064L, "Crohns resection", 2109063L, "Crohns resection",
  2810885L, "Crohns resection", 2815575L, "Crohns resection",
  2861646L, "Crohns resection", 2861647L, "Crohns resection",
  2843192L, "Crohns resection", 2856215L, "Crohns resection",
  2875022L, "Crohns resection", 2896128L, "Crohns resection",
  2861648L, "Crohns resection", 2753168L, "Crohns resection",
  2753169L, "Crohns resection", 2753170L, "Crohns resection",
  2753171L, "Crohns resection", 2753156L, "Crohns resection",
  2753157L, "Crohns resection", 2753158L, "Crohns resection",
  2753159L, "Crohns resection", 2002762L, "Crohns resection",
  2002747L, "Crohns resection", 44510657L, "Crohns resection",
  44510664L, "Crohns resection", 4163693L, "Crohns resection",
  1075436L, "Crohns resection", 1073488L, "Crohns resection",
  4017464L, "Crohns resection", 4017602L, "Crohns resection",
  44811323L, "Crohns resection", 4018274L, "Crohns resection",
  4136779L, "Crohns resection", 40491370L, "Crohns resection",
  40486935L, "Crohns resection", 4201147L, "Crohns resection",
  4066651L, "Crohns resection", 4066652L, "Crohns resection",
  4068264L, "Crohns resection", 4068028L, "Crohns resection",
  4146616L, "Crohns resection", 44809645L, "Crohns resection",
  44807783L, "Crohns resection", 37163764L, "Crohns resection",
  44809642L, "Crohns resection", 44809643L, "Crohns resection",
  4179797L, "Crohns resection", 44809644L, "Crohns resection",
  4250795L, "Crohns resection", 44813904L, "Crohns resection",
  37163763L, "Crohns resection", 44809619L, "Crohns resection",
  4075872L, "Crohns resection", 4264149L, "Crohns resection",
  4233412L, "Crohns resection", 42538033L, "Crohns resection",
  4199951L, "Crohns resection", 4018022L, "Crohns resection",
  44811330L, "Crohns resection", 40487481L, "Crohns resection",
  4018279L, "Crohns resection", 4144205L, "Crohns resection",
  4292716L, "Crohns resection"
)

cli_alert_info("Inline concept set: {nrow(fibrosis_concept_ids)} ids; duplicates: {sum(duplicated(fibrosis_concept_ids$concept_id))}")

concept_catalog <- tbl(km, in_catalog(km_db, km_schema, "CONCEPT")) %>%
  rename_with(tolower) %>%
  filter(concept_id %in% !!fibrosis_concept_ids$concept_id) %>%
  select(concept_id, concept_name, domain_id, vocabulary_id, standard_concept, invalid_reason) %>%
  collect()

concept_check <- fibrosis_concept_ids %>%
  left_join(concept_catalog, by = "concept_id") %>%
  mutate(found = !is.na(concept_name),
         is_procedure = domain_id == "Procedure",
         is_deprecated = !is.na(invalid_reason),
         is_standard = standard_concept == "S")

concept_flags <- concept_check %>% filter(!found | !is_procedure | is_deprecated)

cli_h2("(0) Concept catalog cross-check")
cli_alert_info("Resolved {sum(concept_check$found)}/{nrow(concept_check)} ids | non-Procedure: {sum(!concept_check$is_procedure, na.rm = TRUE)} | deprecated: {sum(concept_check$is_deprecated, na.rm = TRUE)} | non-standard: {sum(!concept_check$is_standard, na.rm = TRUE)}")
if (nrow(concept_flags)) {
  cli_alert_warning("Concepts to review:")
  print(concept_flags %>% select(concept_id, procedure_category, concept_name, domain_id, standard_concept, invalid_reason))
} else {
  cli_alert_success("All inline concept ids resolve to valid Procedure-domain concepts")
}

group_membership <- tbl(con, in_catalog("TOOLING", "PLATFORM", "ALL_CONCEPT_GROUP_CONCEPT")) %>%
  rename_with(tolower) %>%
  filter(concept_id %in% !!fibrosis_concept_ids$concept_id) %>%
  count(omop_concept_group_id, name = "n_concepts") %>%
  collect() %>%
  arrange(desc(n_concepts))
cli_alert_info("Inline ids appearing in existing concept groups: {nrow(group_membership)} group(s)")
if (nrow(group_membership)) print(group_membership)

# =============================================================================
# (1) INDEPENDENT R RECONSTRUCTION (grain-preserving: SQL has no DISTINCT)
# =============================================================================
procedures_raw <- tbl(con, in_catalog(db_name_export, schema_name, "PROCEDURE_OCCURRENCE")) %>%
  rename_with(tolower) %>%
  filter(procedure_concept_id %in% !!fibrosis_concept_ids$concept_id) %>%
  filter(procedure_date >= study_start & procedure_date <= study_end) %>%
  mutate(person_id = as.character(person_id)) %>%        # cast in-DB, before collect()
  select(person_id, procedure_concept_id, procedure_date) %>%
  collect() %>%
  mutate(procedure_date = as.Date(procedure_date))

r_result <- procedures_raw %>%
  inner_join(index_dates, by = "person_id") %>%
  filter(procedure_date >= value) %>%
  left_join(fibrosis_concept_ids, by = c("procedure_concept_id" = "concept_id")) %>%
  transmute(person_id, value = procedure_date, procedure_category, procedure_concept_id)

cli_h2("(1) R reconstruction")
cli_alert_info("R rows: {nrow(r_result)} | distinct persons: {dplyr::n_distinct(r_result$person_id)}")
print(r_result %>% count(procedure_category, name = "r_rows"))

# =============================================================================
# (2) RUN THE REAL SQL LIVE AND RECONCILE
#     a) push the cohort index to a sandbox table (so {index_date_source} is the
#        validated cohort); b) fill placeholders in your verbatim SQL;
#     c) materialize; d) pull back casting person_id to char in-DB.
# =============================================================================

# a) index source table — Parquet stage + COPY (fast for ~105k rows).
#    person_id written as text; TO_NUMBER in the SQL so the join to
#    PROCEDURE_OCCURRENCE.person_id [NUMBER] is number=number.
#    Lands in ANALYSIS.<schema>.<table> (same idiom as get_optum_diagnosis).
verantos::upload_to_snowflake_parquet(
  index_dates %>% dplyr::as_tibble(),
  index_src_table,
  sandbox_schema
)
# The uploaded table stores R column names as lowercase, case-sensitive
# identifiers ("person_id", "value"), so they MUST be double-quoted here —
# unquoted Snowflake would fold them to PERSON_ID/VALUE and fail to find them.
index_date_source <- glue::glue(
  'SELECT TO_NUMBER("person_id") AS person_id, "value" AS value FROM ANALYSIS.{sandbox_schema}.{index_src_table}'
)

# b) your fibrosis SQL, verbatim, with the 4 placeholders left intact
sql_template <- r"(
WITH
    index_dates AS ( {index_date_source} ),
    fibrosis_concept_ids AS (
        SELECT concept_id, procedure_category
        FROM (VALUES
            (2109120,  'Strictureplasty'),
            (4120978,  'Strictureplasty'),
            (4018004,  'Strictureplasty'),
            (4123904,  'Endoscopic balloon dilation'),
            (4125168,  'Endoscopic balloon dilation'),
            (46257755, 'Endoscopic balloon dilation'),
            (2109202,  'Endoscopic balloon dilation'),
            (46257754, 'Endoscopic balloon dilation'),
            (2109189,  'Endoscopic balloon dilation'),
            (3657464,  'Endoscopic balloon dilation'),
            (2109122,  'Crohns resection'),
            (2109123,  'Crohns resection'),
            (2109029,  'Crohns resection'),
            (2109028,  'Crohns resection'),
            (2109030,  'Crohns resection'),
            (2109075,  'Crohns resection'),
            (2109064,  'Crohns resection'),
            (2109063,  'Crohns resection'),
            (2810885,  'Crohns resection'),
            (2815575,  'Crohns resection'),
            (2861646,  'Crohns resection'),
            (2861647,  'Crohns resection'),
            (2843192,  'Crohns resection'),
            (2856215,  'Crohns resection'),
            (2875022,  'Crohns resection'),
            (2896128,  'Crohns resection'),
            (2861648,  'Crohns resection'),
            (2753168,  'Crohns resection'),
            (2753169,  'Crohns resection'),
            (2753170,  'Crohns resection'),
            (2753171,  'Crohns resection'),
            (2753156,  'Crohns resection'),
            (2753157,  'Crohns resection'),
            (2753158,  'Crohns resection'),
            (2753159,  'Crohns resection'),
            (2002762,  'Crohns resection'),
            (2002747,  'Crohns resection'),
            (44510657, 'Crohns resection'),
            (44510664, 'Crohns resection'),
            (4163693,  'Crohns resection'),
            (1075436,  'Crohns resection'),
            (1073488,  'Crohns resection'),
            (4017464,  'Crohns resection'),
            (4017602,  'Crohns resection'),
            (44811323, 'Crohns resection'),
            (4018274,  'Crohns resection'),
            (4136779,  'Crohns resection'),
            (40491370, 'Crohns resection'),
            (40486935, 'Crohns resection'),
            (4201147,  'Crohns resection'),
            (4066651,  'Crohns resection'),
            (4066652,  'Crohns resection'),
            (4068264,  'Crohns resection'),
            (4068028,  'Crohns resection'),
            (4146616,  'Crohns resection'),
            (44809645, 'Crohns resection'),
            (44807783, 'Crohns resection'),
            (37163764, 'Crohns resection'),
            (44809642, 'Crohns resection'),
            (44809643, 'Crohns resection'),
            (4179797,  'Crohns resection'),
            (44809644, 'Crohns resection'),
            (4250795,  'Crohns resection'),
            (44813904, 'Crohns resection'),
            (37163763, 'Crohns resection'),
            (44809619, 'Crohns resection'),
            (4075872,  'Crohns resection'),
            (4264149,  'Crohns resection'),
            (4233412,  'Crohns resection'),
            (42538033, 'Crohns resection'),
            (4199951,  'Crohns resection'),
            (4018022,  'Crohns resection'),
            (44811330, 'Crohns resection'),
            (40487481, 'Crohns resection'),
            (4018279,  'Crohns resection'),
            (4144205,  'Crohns resection'),
            (4292716,  'Crohns resection')
        ) AS t(concept_id, procedure_category)
    ),
    qualifying_procedures AS (
        SELECT
            po.person_id,
            po.procedure_date AS value
        FROM {dataset}.PROCEDURE_OCCURRENCE po
        INNER JOIN index_dates idx
            ON idx.person_id = po.person_id
        INNER JOIN fibrosis_concept_ids fc
            ON fc.concept_id = po.procedure_concept_id
        WHERE po.procedure_date BETWEEN DATE('{study_start}') AND DATE('{study_end}')
          AND po.procedure_date >= idx.value
    )
SELECT
    person_id,
    value
FROM qualifying_procedures
)"

fibrosis_sql <- sql_template
fibrosis_sql <- gsub("{index_date_source}", index_date_source, fibrosis_sql, fixed = TRUE)
fibrosis_sql <- gsub("{dataset}",     dataset,                  fibrosis_sql, fixed = TRUE)
fibrosis_sql <- gsub("{study_start}", as.character(study_start), fibrosis_sql, fixed = TRUE)
fibrosis_sql <- gsub("{study_end}",   as.character(study_end),   fibrosis_sql, fixed = TRUE)

# c) materialize
DBI::dbExecute(con, glue::glue(
  "CREATE OR REPLACE TABLE ANALYSIS.{sandbox_schema}.{sql_out_table} AS ({fibrosis_sql})"
))

# d) pull back — cast person_id to char in-DB so the 64-bit id stays exact
sql_result <- tbl(con, in_catalog("ANALYSIS", sandbox_schema, sql_out_table)) %>%
  rename_with(tolower) %>%
  mutate(person_id = as.character(person_id)) %>%
  collect() %>%
  mutate(value = as.Date(value))

cli_h2("(2) Reconcile R vs SQL")
cli_alert_info("SQL rows: {nrow(sql_result)} | distinct persons: {dplyr::n_distinct(sql_result$person_id)}")
cli_alert_info("R rows:   {nrow(r_result)} | distinct persons: {dplyr::n_distinct(r_result$person_id)}")

# No DISTINCT in SQL -> reconcile on counts per (person_id, value), not anti_join.
r_counts   <- r_result   %>% count(person_id, value, name = "r_n")
sql_counts <- sql_result %>% count(person_id, value, name = "sql_n")

reconcile <- full_join(r_counts, sql_counts, by = c("person_id", "value")) %>%
  mutate(across(c(r_n, sql_n), ~ tidyr::replace_na(.x, 0L)),
         delta = r_n - sql_n)

mismatches <- reconcile %>% filter(delta != 0)
if (nrow(mismatches) == 0) {
  cli_alert_success("EXACT MATCH: identical (person_id, value) multiset in R and SQL")
} else {
  cli_alert_danger("{nrow(mismatches)} (person_id, value) keys differ")
  cli_alert_info("Rows only in R: {sum(pmax(mismatches$delta, 0))} | only in SQL: {sum(pmax(-mismatches$delta, 0))}")
  print(head(mismatches, 20))
}

# =============================================================================
# (3) STANDALONE ASSERTIONS on the SQL output
# =============================================================================
cli_h2("(3) Assertions on SQL output")

sql_checked <- sql_result %>%
  left_join(index_dates, by = "person_id", suffix = c("", "_idx")) %>%
  rename(index_date = value_idx)

chk <- list(
  on_or_after_index   = all(sql_checked$value >= sql_checked$index_date, na.rm = TRUE),
  within_study_window = all(sql_result$value >= study_start & sql_result$value <= study_end, na.rm = TRUE),
  persons_in_cohort   = all(sql_result$person_id %in% index_dates$person_id),
  no_missing_dates    = all(!is.na(sql_result$value)),
  no_missing_person   = all(!is.na(sql_result$person_id))
)
for (nm in names(chk)) {
  if (isTRUE(chk[[nm]])) cli_alert_success("PASS: {nm}") else cli_alert_danger("FAIL: {nm}")
}

# Grain note: SQL emits one row per qualifying procedure (no DISTINCT). Same-date
# multi-procedure patients legitimately produce >1 row per (person_id, value).
dup_same_date <- sql_result %>% count(person_id, value) %>% filter(n > 1)
cli_alert_info("(person_id, value) pairs appearing >1x in SQL: {nrow(dup_same_date)} (expected if same-date multi-procedure)")

cli_h1("fibrosis_related_procedures validation complete")

# SYMPTOMS VALIDATION ==========================================================
# patient spine + procedure anchor (clean character ids)
spine     <- final_r %>% transmute(person_id = as.character(person_id))
proc_dates <- r_result %>% transmute(person_id = as.character(person_id),
                                     procedure_date = as.Date(value)) %>% distinct()

# index_date_source already defined in the fibrosis script — reuse it as-is.
av_fibrosis <- glue::glue("ANALYSIS.{sandbox_schema}.FIBROSIS_PROC_SQL")

# Only patients with a fibrosis procedure can ever match, so restrict symptom
# pulls to them in-DB (NUMBER=NUMBER semi-join, before casting id to char).
# Keeps the diarrhea collect small without changing the result.
proc_persons_tbl <- tbl(con, in_catalog("ANALYSIS", sandbox_schema, "FIBROSIS_PROC_SQL")) %>%
  rename_with(tolower) %>% distinct(person_id)

# -----------------------------------------------------------------------------
# Generic validator: given reconstructed symptom events (person_id, event_date),
# the window N, the filled SQL, reconcile + audit gaps.
# -----------------------------------------------------------------------------
validate_symptom <- function(label, events_r, window_days, symptom_sql, out_table) {
  cli_h1("{label}  (1-{window_days} days before procedure)")

  # ---- (1) R reconstruction ----
  # all symptom×procedure pairs for the same patient, with day gap
  pairs <- events_r %>%
    inner_join(proc_dates, by = "person_id", relationship = "many-to-many") %>%
    mutate(gap = as.integer(procedure_date - event_date))
  matched <- pairs %>% filter(gap >= 1, gap <= window_days)

  flag_r <- spine %>%
    mutate(value_r = person_id %in% unique(matched$person_id))
  cli_alert_info("R: {sum(flag_r$value_r)} TRUE of {nrow(flag_r)} ({dplyr::n_distinct(events_r$person_id)} patients have any in-window-eligible symptom record)")

  # ---- (2) run the real SQL live + reconcile ----
  DBI::dbExecute(con, glue::glue(
    "CREATE OR REPLACE TABLE ANALYSIS.{sandbox_schema}.{out_table} AS ({symptom_sql})"
  ))
  flag_sql <- tbl(con, in_catalog("ANALYSIS", sandbox_schema, out_table)) %>%
    rename_with(tolower) %>%
    mutate(person_id = as.character(person_id)) %>%
    collect() %>%
    transmute(person_id, value_sql = as.logical(value))

  recon <- full_join(flag_r, flag_sql, by = "person_id") %>%
    mutate(across(c(value_r, value_sql), ~ tidyr::replace_na(.x, FALSE)))

  cli_h2("Reconcile per-patient flag")
  cli_alert_info("SQL TRUE: {sum(recon$value_sql)} | R TRUE: {sum(recon$value_r)}")
  disagree <- recon %>% filter(value_r != value_sql)
  if (nrow(disagree) == 0) {
    cli_alert_success("EXACT MATCH: identical TRUE/FALSE flag for every patient")
  } else {
    cli_alert_danger("{nrow(disagree)} patients disagree (R-only TRUE: {sum(disagree$value_r)}, SQL-only TRUE: {sum(disagree$value_sql)})")
    print(head(disagree, 20))
  }

  # ---- (3) GAP AUDIT — the core question ----
  cli_h2("Gap audit: are matches really 1-{window_days} days before a procedure?")
  if (nrow(matched) == 0) {
    cli_alert_warning("No matched pairs to audit.")
  } else {
    cli_alert_info("Matched pairs: {nrow(matched)} | gap range observed: [{min(matched$gap)}, {max(matched$gap)}] days")
    a_lower <- all(matched$gap >= 1)
    a_upper <- all(matched$gap <= window_days)
    if (a_lower) cli_alert_success("PASS: every match is >=1 day before the procedure (procedure day itself excluded)")
    else cli_alert_danger("FAIL: {sum(matched$gap < 1)} matches are on/after the procedure date")
    if (a_upper) cli_alert_success("PASS: every match is <={window_days} days before the procedure")
    else cli_alert_danger("FAIL: {sum(matched$gap > window_days)} matches exceed {window_days} days")

    # boundary counts: confirm 1 and N are included, 0 and N+1 are absent in matches
    bcount <- pairs %>%
      mutate(bucket = case_when(
        gap <= 0                         ~ "<=0 (on/after proc)",
        gap == 1                         ~ "1 (boundary)",
        gap == window_days               ~ paste0(window_days, " (boundary)"),
        gap > window_days                ~ paste0(">", window_days, " (too early)"),
        TRUE                             ~ "2..N-1 (interior)"
      )) %>%
      count(bucket, name = "pairs")
    print(bcount)

    # sample TRUE patients: show the closest qualifying symptom for eyeballing
    cli_alert_info("Sample matched (symptom -> procedure) pairs:")
    print(matched %>%
            group_by(person_id) %>% slice_min(gap, n = 1, with_ties = FALSE) %>% ungroup() %>%
            arrange(gap) %>% transmute(person_id, event_date, procedure_date, gap_days = gap) %>%
            head(10))
  }

  # excluded-but-has-symptoms: patients with symptom events whose nearest
  # pre-procedure gap falls OUTSIDE [1,N] (or have no procedure) -> correctly FALSE
  near_miss <- pairs %>%
    filter(!person_id %in% unique(matched$person_id)) %>%
    group_by(person_id) %>%
    summarise(nearest_pre = suppressWarnings(min(gap[gap >= 1], na.rm = TRUE)),
              any_post = any(gap <= 0), .groups = "drop") %>%
    mutate(nearest_pre = ifelse(is.infinite(nearest_pre), NA_integer_, nearest_pre))
  cli_alert_info("Patients with symptom records but NOT flagged: {nrow(near_miss)} (excluded because nearest pre-proc gap > {window_days}, or only on/after procedure)")
  if (nrow(near_miss)) print(head(near_miss %>% arrange(nearest_pre), 10))

  invisible(list(flag_r = flag_r, flag_sql = flag_sql, matched = matched, recon = recon))
}

# =============================================================================
# SYMPTOM 1: abdominal mass, 30 days  (inline concept ids, CONDITION only)
# =============================================================================
abdom_ids <- c(192438L, 4223207L, 4139255L, 37311117L, 37311118L, 37311119L, 37311120L)

cli_h2("(0) abdominal mass concept check")
abdom_cat <- tbl(km, in_catalog(km_db, km_schema, "CONCEPT")) %>%
  rename_with(tolower) %>% filter(concept_id %in% !!abdom_ids) %>%
  select(concept_id, concept_name, domain_id, standard_concept, invalid_reason) %>% collect()
print(abdom_cat)
cli_alert_info("Resolved {nrow(abdom_cat)}/{length(abdom_ids)} | non-Condition domain: {sum(abdom_cat$domain_id != 'Condition')} | deprecated: {sum(!is.na(abdom_cat$invalid_reason))}")

abdom_events <- tbl(con, in_catalog(db_name_export, schema_name, "CONDITION_OCCURRENCE")) %>%
  rename_with(tolower) %>%
  filter(condition_concept_id %in% !!abdom_ids,
         condition_start_date >= study_start, condition_start_date <= study_end) %>%
  semi_join(proc_persons_tbl, by = "person_id") %>%
  mutate(person_id = as.character(person_id)) %>%
  transmute(person_id, event_date = condition_start_date) %>%
  collect() %>% mutate(event_date = as.Date(event_date))

abdom_sql <- r"(
WITH
index_dates AS ( {index_date_source} ),
procedure_dates AS ( SELECT fp.person_id, fp.value AS procedure_date FROM {av_fib} fp ),
abdom_mass_condition AS (
    SELECT co.person_id, co.condition_start_date AS event_date
    FROM {dataset}.CONDITION_OCCURRENCE co
    WHERE co.CONDITION_CONCEPT_ID IN (192438,4223207,4139255,37311117,37311118,37311119,37311120)
      AND co.CONDITION_START_DATE BETWEEN DATE('{study_start}') AND DATE('{study_end}')
),
matched_pairs AS (
    SELECT DISTINCT ac.person_id
    FROM abdom_mass_condition ac
    INNER JOIN procedure_dates pd
        ON pd.person_id = ac.person_id
        AND ac.event_date BETWEEN DATEADD('day',-30,pd.procedure_date) AND DATEADD('day',-1,pd.procedure_date)
)
SELECT idx.person_id, CASE WHEN mp.person_id IS NOT NULL THEN TRUE ELSE FALSE END AS value
FROM index_dates idx LEFT JOIN matched_pairs mp ON mp.person_id = idx.person_id
)"
abdom_sql <- gsub("{index_date_source}", index_date_source, abdom_sql, fixed = TRUE)
abdom_sql <- gsub("{av_fib}",       av_fibrosis,            abdom_sql, fixed = TRUE)
abdom_sql <- gsub("{dataset}",      dataset,                abdom_sql, fixed = TRUE)
abdom_sql <- gsub("{study_start}",  as.character(study_start), abdom_sql, fixed = TRUE)
abdom_sql <- gsub("{study_end}",    as.character(study_end),   abdom_sql, fixed = TRUE)

res_abdom <- validate_symptom("symptom_abdominal_mass_pre_fibrosis_proc_30d",
                              abdom_events, 30L, abdom_sql, "SYMPTOM_ABDOM_MASS_30D_SQL")

# =============================================================================
# SYMPTOM 2: diarrhea / stool frequency, 60 days
#   concept group 6059529768801722246-Verantos, CONDITION + OBSERVATION
# =============================================================================
diarrhea_group <- "6059529768801722246-Verantos"
diarrhea_ids <- tbl(con, in_catalog("TOOLING","PLATFORM","ALL_CONCEPT_GROUP_CONCEPT")) %>%
  rename_with(tolower) %>% filter(omop_concept_group_id == diarrhea_group) %>%
  distinct(concept_id) %>% collect() %>% pull(concept_id)

cli_h2("(0) diarrhea concept group check")
cli_alert_info("Group {diarrhea_group}: {length(diarrhea_ids)} concept ids")
diarrhea_domains <- tbl(km, in_catalog(km_db, km_schema, "CONCEPT")) %>%
  rename_with(tolower) %>% filter(concept_id %in% !!diarrhea_ids) %>%
  count(domain_id, name = "n") %>% collect()
print(diarrhea_domains)

diarrhea_co <- tbl(con, in_catalog(db_name_export, schema_name, "CONDITION_OCCURRENCE")) %>%
  rename_with(tolower) %>%
  filter(condition_concept_id %in% !!diarrhea_ids,
         condition_start_date >= study_start, condition_start_date <= study_end) %>%
  semi_join(proc_persons_tbl, by = "person_id") %>%
  mutate(person_id = as.character(person_id)) %>%
  transmute(person_id, event_date = condition_start_date) %>% collect()
diarrhea_obs <- tbl(con, in_catalog(db_name_export, schema_name, "OBSERVATION")) %>%
  rename_with(tolower) %>%
  filter(observation_concept_id %in% !!diarrhea_ids,
         observation_date >= study_start, observation_date <= study_end) %>%
  semi_join(proc_persons_tbl, by = "person_id") %>%
  mutate(person_id = as.character(person_id)) %>%
  transmute(person_id, event_date = observation_date) %>% collect()
diarrhea_events <- bind_rows(diarrhea_co, diarrhea_obs) %>% mutate(event_date = as.Date(event_date))
cli_alert_info("diarrhea events: {nrow(diarrhea_co)} condition + {nrow(diarrhea_obs)} observation = {nrow(diarrhea_events)}")

diarrhea_sql <- r"(
WITH
index_dates AS ( {index_date_source} ),
procedure_dates AS ( SELECT fp.person_id, fp.value AS procedure_date FROM {av_fib} fp ),
diarrhea_condition AS (
    SELECT co.person_id, co.condition_start_date AS event_date
    FROM {dataset}.CONDITION_OCCURRENCE co
    INNER JOIN TOOLING.PLATFORM.ALL_CONCEPT_GROUP_CONCEPT g
        ON g.CONCEPT_ID = co.CONDITION_CONCEPT_ID AND g.OMOP_CONCEPT_GROUP_ID = '6059529768801722246-Verantos'
    WHERE co.CONDITION_START_DATE BETWEEN DATE('{study_start}') AND DATE('{study_end}')
),
diarrhea_observation AS (
    SELECT o.person_id, o.observation_date AS event_date
    FROM {dataset}.OBSERVATION o
    INNER JOIN TOOLING.PLATFORM.ALL_CONCEPT_GROUP_CONCEPT g
        ON g.CONCEPT_ID = o.OBSERVATION_CONCEPT_ID AND g.OMOP_CONCEPT_GROUP_ID = '6059529768801722246-Verantos'
    WHERE o.OBSERVATION_DATE BETWEEN DATE('{study_start}') AND DATE('{study_end}')
),
all_symptom_events AS (
    SELECT person_id, event_date FROM diarrhea_condition
    UNION ALL
    SELECT person_id, event_date FROM diarrhea_observation
),
matched_pairs AS (
    SELECT DISTINCT se.person_id
    FROM all_symptom_events se
    INNER JOIN procedure_dates pd
        ON pd.person_id = se.person_id
        AND se.event_date BETWEEN DATEADD('day',-60,pd.procedure_date) AND DATEADD('day',-1,pd.procedure_date)
)
SELECT idx.person_id, CASE WHEN mp.person_id IS NOT NULL THEN TRUE ELSE FALSE END AS value
FROM index_dates idx LEFT JOIN matched_pairs mp ON mp.person_id = idx.person_id
)"
diarrhea_sql <- gsub("{index_date_source}", index_date_source, diarrhea_sql, fixed = TRUE)
diarrhea_sql <- gsub("{av_fib}",       av_fibrosis,            diarrhea_sql, fixed = TRUE)
diarrhea_sql <- gsub("{dataset}",      dataset,                diarrhea_sql, fixed = TRUE)
diarrhea_sql <- gsub("{study_start}",  as.character(study_start), diarrhea_sql, fixed = TRUE)
diarrhea_sql <- gsub("{study_end}",    as.character(study_end),   diarrhea_sql, fixed = TRUE)

res_diarrhea <- validate_symptom("symptom_diarrhea_pre_fibrosis_proc_60d",
                                 diarrhea_events, 60L, diarrhea_sql, "SYMPTOM_DIARRHEA_60D_SQL")

cli_h1("symptom window validation complete")

# Build some additional tables for report using data loaded in during QC, since we have it
# =============================================================================
# Cohort attrition table — built from objects already in the session
# (person, crohns_index_r, include_ids, uc_persons, ibd_unspec_persons, final_r)
# =============================================================================

# id sets (character, exact)
crohns_ids  <- unique(as.character(crohns_index_r$person_id))
age_ids     <- unique(as.character(include_ids))                  # age >=18 (subset of Crohn's)
uc_ids      <- unique(as.character(uc_persons$person_id))
ibd_ids     <- unique(as.character(ibd_unspec_persons$person_id))
nobirth_ids <- person %>% filter(is.na(derived_birth_date)) %>% pull(person_id) %>% as.character()

# sequential funnel
s1 <- crohns_ids                       # >=1 Crohn's dx in study window
s2 <- intersect(s1, age_ids)           # age >=18 at index (INCLUDE)
s3 <- setdiff(s2, uc_ids)              # exclude UC history
s4 <- setdiff(s3, ibd_ids)             # exclude IBD-unspecified history -> FINAL

# sanity: terminal step must equal the validated cohort (the non-null-index
# guard removes no one because the age gate already requires a birth date)
stopifnot(length(setdiff(s4, nobirth_ids)) == nrow(final_r),
          length(s4) == nrow(final_r))

attrition <- tibble::tibble(
  step = c(
    "All patients in registry (PERSON)",
    "≥ 1 Crohn's disease diagnosis in study period",
    "Age ≥ 18 at index date",
    "Exclude: ulcerative colitis history (any time in period)",
    "Exclude: IBD-unspecified / indeterminate colitis history  →  Final cohort"
  ),
  n_remaining = c(nrow(person), length(s1), length(s2), length(s3), length(s4))
) %>%
  mutate(
    n_excluded   = dplyr::lag(n_remaining) - n_remaining,
    pct_of_crohns = round(100 * n_remaining / length(s1), 1)   # % of Crohn's-indexed pop
  )
attrition$pct_of_crohns[1] <- NA_real_   # registry total isn't a meaningful % of Crohn's-indexed

print(attrition)

# ---- rendered table ---------------------------------------------------------
attrition_gt <- attrition %>%
  gt() %>%
  tab_header(
    title = "Crohn's Cohort Attrition",
    subtitle = "IBD Pragmatic Registry 2026Q1  |  study period 2015-01-01 to 2026-01-08"
  ) %>%
  cols_label(
    step = "Step",
    n_remaining = "Patients remaining",
    n_excluded = "Excluded at step",
    pct_of_crohns = "% of Crohn's-indexed"
  ) %>%
  fmt_number(c(n_remaining, n_excluded), decimals = 0, use_seps = TRUE) %>%
  fmt_number(pct_of_crohns, decimals = 1) %>%
  sub_missing(columns = c(n_excluded, pct_of_crohns), missing_text = "—") %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_body(rows = c(1))) %>%
  cols_align("left", columns = step)

attrition_gt

# =============================================================================
# Fibrosis-related procedures summary table — built from session objects
#   r_result : validated fibrosis_related_procedures (person_id, value=date,
#              procedure_category) — one row per qualifying procedure occurrence
#   final_r  : validated cohort (denominator)
#
# Shows: patients with ANY fibrosis-related procedure, then the breakdown by
# subgroup (resection / endoscopic balloon dilation / strictureplasty).
# NOTE: subgroups are NOT mutually exclusive — a patient with >1 procedure type
# is counted in each, so subgroup patient counts do not sum to the total.
# =============================================================================
# per-patient subgroup flags from the validated procedure rows
proc_flags <- r_result %>%
  transmute(
    person_id = as.character(person_id),
    cat = dplyr::recode(procedure_category,
                        "Crohns resection"            = "resection",
                        "Endoscopic balloon dilation" = "ebd",
                        "Strictureplasty"             = "strictureplasty")
  ) %>%
  distinct() %>%
  mutate(flag = TRUE) %>%
  pivot_wider(names_from = cat, values_from = flag, values_fill = FALSE)

# attach to the full cohort so percentages are out of cohort N
proc_patient <- final_r %>%
  transmute(person_id = as.character(person_id)) %>%
  left_join(proc_flags, by = "person_id") %>%
  mutate(across(c(resection, ebd, strictureplasty), ~ tidyr::replace_na(.x, FALSE)),
         any_fibrosis_procedure = resection | ebd | strictureplasty)

# ---- table (same idiom as table_ip_flare) -----------------------------------
table_procedures <- proc_patient %>%
  tbl_summary(
    include = c(any_fibrosis_procedure, resection, ebd, strictureplasty),
    label = list(
      any_fibrosis_procedure ~ "Any stricture-related procedure",
      resection              ~ "Intestinal resection (Crohn's-relevant)",
      ebd                    ~ "Endoscopic balloon dilation",
      strictureplasty        ~ "Strictureplasty"
    )
  ) %>%
  modify_footnote(
    all_stat_cols() ~ "n (%). Procedure subgroups are not mutually exclusive; a patient with more than one procedure type is counted in each, so subgroup counts do not sum to 'Any'."
  )

table_procedures


sales_101 <- readr::read_rds("data/SCIENCE-101/sales_101.rds")
sales_101 <- c(sales_101, list(attrition_gt, table_procedures))          # add the gt table
readr::write_rds(sales_101, "data/SCIENCE-101/sales_101.rds")



