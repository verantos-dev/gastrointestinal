# =============================================================================
# SALES-101 QC  —  source this BEFORE building the report tables
# https://verantos.atlassian.net/browse/SALES-101
#
# Everything is anchored to the NEW inclusive procedure list (2,464 codes).
# On completion the session holds the objects the tables need:
#   final_r, person, crohns_index_r, include_ids, uc_persons, ibd_unspec_persons   (cohort/attrition)
#   r_result                                                                       (fibrosis subclassifications)
#   GI_PROC_SQL (sandbox), r_gi, proc_dates, proc_persons_tbl, av_fibrosis, spine  (inclusive anchor)
#
# Sections:
#   (A) cohort reconstruction + reconcile
#   (B) fibrosis subclassification procedures  -> r_result   (resection/EBD/strictureplasty labels)
#   (C) INCLUSIVE procedures                   -> GI_PROC_SQL, r_gi; reconcile vs dataset any_gi_procedure_ever
#   (D) symptom validation, anchored to the inclusive procedures
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(dbplyr); library(tidyr); library(stringr)
  library(lubridate); library(readr); library(glue); library(DBI); library(cli); library(tibble)
})
options(scipen = 9999)

# ---- parameters / connections -----------------------------------------------
db_name_export <- "EXPORT"
schema_name    <- "IBD_PRAGMATIC_REGISTRY_2026Q1"
study_start    <- as.Date("2015-01-01")
study_end      <- as.Date("2026-01-08")
km_db          <- "KNOWLEDGE_MANAGEMENT"
km_schema      <- "OMOP_METADATA"
sandbox_schema <- paste0(toupper(Sys.info()["user"]), "_SANDBOX")

crohns_group_id <- "2146300387770138038-Verantos"
uc_group_id     <- "9056166393484755007-Verantos"
ibd_unspec_ids  <- c(4074815L, 4341633L)
diarrhea_group  <- "6059529768801722246-Verantos"
abdom_ids       <- c(192438L, 4223207L, 4139255L, 37311117L, 37311118L, 37311119L, 37311120L)

cohort_csv_path    <- here::here("data", "SCIENCE-101", "ra_cohort_crohns_reprocess.csv")
dataset_path       <- here::here("data", "SCIENCE-101", "dataset.csv")
inclusive_ids_path <- here::here("data", "SCIENCE-101", "final_inclusive_concept_ids.csv")
index_src_table    <- "FIBROSIS_INDEX_SRC"
gi_out_table       <- "GI_PROC_SQL"
stopifnot(file.exists(cohort_csv_path), file.exists(dataset_path), file.exists(inclusive_ids_path))

enforce_study_window <- TRUE
apply_window <- function(tbl) {
  if (enforce_study_window) dplyr::filter(tbl, condition_start_date >= study_start,
                                          condition_start_date <= study_end) else tbl
}

con <- verantos::snowflake_connect(db_name = "ANALYSIS", db_warehouse = "DEV_WH", db_role = "DATA_DEV")
km  <- verantos::snowflake_connect(db_name = "KNOWLEDGE_MANAGEMENT", db_warehouse = "DEV_WH",
                                   db_role = "DATA_DEV", use_schema = "OMOP_METADATA")

co_tbl     <- tbl(con, in_catalog(db_name_export, schema_name, "CONDITION_OCCURRENCE")) %>% rename_with(tolower)
person_tbl <- tbl(con, in_catalog(db_name_export, schema_name, "PERSON")) %>% rename_with(tolower)
cgc_tbl    <- tbl(con, in_catalog("TOOLING", "PLATFORM", "ALL_CONCEPT_GROUP_CONCEPT")) %>% rename_with(tolower)
group_ids  <- function(g) cgc_tbl %>% filter(omop_concept_group_id == g) %>% distinct(concept_id)

# =============================================================================
# (A) COHORT  (unchanged — cohort/attrition are identical to before)
# =============================================================================
person <- person_tbl %>%
  mutate(person_id = as.character(person_id)) %>%
  select(person_id, birth_datetime, day_of_birth, month_of_birth, year_of_birth,
         gender_concept_name, race_concept_name, ethnicity_concept_name) %>%
  collect()

build_derived_birth <- function(bd, d, m, y) {
  d <- suppressWarnings(as.integer(d)); m <- suppressWarnings(as.integer(m)); y <- suppressWarnings(as.integer(y))
  out <- as.Date(bd)
  i <- is.na(out) & !is.na(d) & !is.na(m) & !is.na(y); out[i] <- as.Date(sprintf("%04d-%02d-%02d", y[i], m[i], d[i]))
  i <- is.na(out) & !is.na(d) &  is.na(m) & !is.na(y); out[i] <- as.Date(sprintf("%04d-06-%02d", y[i], d[i]))
  i <- is.na(out) & !is.na(m) &  is.na(d) & !is.na(y); out[i] <- as.Date(sprintf("%04d-%02d-15", y[i], m[i]))
  i <- is.na(out) &  is.na(m) &  is.na(d) & !is.na(y); out[i] <- as.Date(sprintf("%04d-06-15", y[i]))
  out
}
person <- person %>% mutate(birth_dt = as.Date(birth_datetime),
                            derived_birth_date = build_derived_birth(birth_datetime, day_of_birth, month_of_birth, year_of_birth))

crohns_index_r <- co_tbl %>%
  inner_join(group_ids(crohns_group_id), by = c("condition_concept_id" = "concept_id")) %>%
  apply_window() %>% group_by(person_id) %>%
  summarise(index_date = min(condition_start_date, na.rm = TRUE), .groups = "drop") %>%
  mutate(person_id = as.character(person_id)) %>% collect() %>% mutate(index_date = as.Date(index_date))

mmdd <- function(d) as.integer(format(d, "%m%d"))
age_gte18_r <- crohns_index_r %>%
  inner_join(person %>% select(person_id, birth_dt, derived_birth_date), by = "person_id") %>%
  mutate(age_v2_gate = (as.integer(format(index_date, "%Y")) - as.integer(format(birth_dt, "%Y"))) -
           ifelse(mmdd(birth_dt) > mmdd(index_date), 1L, 0L),
         flag_gate = !is.na(age_v2_gate) & age_v2_gate >= 18)

uc_persons <- co_tbl %>% inner_join(group_ids(uc_group_id), by = c("condition_concept_id" = "concept_id")) %>%
  apply_window() %>% distinct(person_id) %>% mutate(person_id = as.character(person_id)) %>% collect()
ibd_unspec_persons <- co_tbl %>% filter(condition_concept_id %in% ibd_unspec_ids) %>%
  apply_window() %>% distinct(person_id) %>% mutate(person_id = as.character(person_id)) %>% collect()

include_ids <- age_gte18_r %>% filter(flag_gate) %>% pull(person_id)
exclude_ids <- union(uc_persons$person_id, ibd_unspec_persons$person_id)

final_r <- person %>%
  filter(person_id %in% include_ids, !person_id %in% exclude_ids) %>%
  inner_join(crohns_index_r, by = "person_id") %>%
  filter(!is.na(derived_birth_date)) %>%
  mutate(age_at_index = floor(as.numeric(difftime(index_date, derived_birth_date, units = "days")) / 365.25)) %>%
  distinct(person_id, index_date, age_at_index, gender_concept_name, race_concept_name, ethnicity_concept_name)

sql_final <- readr::read_csv(cohort_csv_path, show_col_types = FALSE,
                             col_types = readr::cols(.default = readr::col_character())) %>%
  rename_with(tolower) %>%
  mutate(person_id = subject_id, index_date = lubridate::mdy(index_date), age_at_index = as.integer(age_at_index))

cli_h2("(A) Cohort reconcile")
cli_alert_info("R cohort: {nrow(final_r)} | SQL cohort: {nrow(sql_final)} | only-in-R: {nrow(anti_join(final_r, sql_final, by='person_id'))} | only-in-SQL: {nrow(anti_join(sql_final, final_r, by='person_id'))}")

# index source staged for the live procedure SQL (quoted-lowercase columns)
index_dates <- final_r %>% transmute(person_id = as.character(person_id), value = as.Date(index_date))
verantos::upload_to_snowflake_parquet(index_dates %>% as_tibble(), index_src_table, sandbox_schema)
index_date_source <- glue::glue(
  'SELECT TO_NUMBER("person_id") AS person_id, "value" AS value FROM ANALYSIS.{sandbox_schema}.{index_src_table}')

# =============================================================================
# (B) FIBROSIS SUBCLASSIFICATIONS  -> r_result  (resection / EBD / strictureplasty)
#     Used ONLY to label the three subgroups in table_procedures.
# =============================================================================
fibrosis_concept_ids <- tibble::tribble(
  ~concept_id, ~procedure_category,
  2109120L,"Strictureplasty", 4120978L,"Strictureplasty", 4018004L,"Strictureplasty",
  4123904L,"Endoscopic balloon dilation", 4125168L,"Endoscopic balloon dilation",
  46257755L,"Endoscopic balloon dilation", 2109202L,"Endoscopic balloon dilation",
  46257754L,"Endoscopic balloon dilation", 2109189L,"Endoscopic balloon dilation",
  3657464L,"Endoscopic balloon dilation",
  2109122L,"Crohns resection",2109123L,"Crohns resection",2109029L,"Crohns resection",2109028L,"Crohns resection",
  2109030L,"Crohns resection",2109075L,"Crohns resection",2109064L,"Crohns resection",2109063L,"Crohns resection",
  2810885L,"Crohns resection",2815575L,"Crohns resection",2861646L,"Crohns resection",2861647L,"Crohns resection",
  2843192L,"Crohns resection",2856215L,"Crohns resection",2875022L,"Crohns resection",2896128L,"Crohns resection",
  2861648L,"Crohns resection",2753168L,"Crohns resection",2753169L,"Crohns resection",2753170L,"Crohns resection",
  2753171L,"Crohns resection",2753156L,"Crohns resection",2753157L,"Crohns resection",2753158L,"Crohns resection",
  2753159L,"Crohns resection",2002762L,"Crohns resection",2002747L,"Crohns resection",44510657L,"Crohns resection",
  44510664L,"Crohns resection",4163693L,"Crohns resection",1075436L,"Crohns resection",1073488L,"Crohns resection",
  4017464L,"Crohns resection",4017602L,"Crohns resection",44811323L,"Crohns resection",4018274L,"Crohns resection",
  4136779L,"Crohns resection",40491370L,"Crohns resection",40486935L,"Crohns resection",4201147L,"Crohns resection",
  4066651L,"Crohns resection",4066652L,"Crohns resection",4068264L,"Crohns resection",4068028L,"Crohns resection",
  4146616L,"Crohns resection",44809645L,"Crohns resection",44807783L,"Crohns resection",37163764L,"Crohns resection",
  44809642L,"Crohns resection",44809643L,"Crohns resection",4179797L,"Crohns resection",44809644L,"Crohns resection",
  4250795L,"Crohns resection",44813904L,"Crohns resection",37163763L,"Crohns resection",44809619L,"Crohns resection",
  4075872L,"Crohns resection",4264149L,"Crohns resection",4233412L,"Crohns resection",42538033L,"Crohns resection",
  4199951L,"Crohns resection",4018022L,"Crohns resection",44811330L,"Crohns resection",40487481L,"Crohns resection",
  4018279L,"Crohns resection",4144205L,"Crohns resection",4292716L,"Crohns resection"
)

r_result <- tbl(con, in_catalog(db_name_export, schema_name, "PROCEDURE_OCCURRENCE")) %>%
  rename_with(tolower) %>%
  filter(procedure_concept_id %in% !!fibrosis_concept_ids$concept_id,
         procedure_date >= study_start & procedure_date <= study_end) %>%
  mutate(person_id = as.character(person_id)) %>%
  select(person_id, procedure_concept_id, procedure_date) %>%
  collect() %>% mutate(procedure_date = as.Date(procedure_date)) %>%
  inner_join(index_dates, by = "person_id") %>%
  filter(procedure_date >= value) %>%
  left_join(fibrosis_concept_ids, by = c("procedure_concept_id" = "concept_id")) %>%
  transmute(person_id, value = procedure_date, procedure_category, procedure_concept_id)
cli_h2("(B) Fibrosis subclassifications")
print(r_result %>% count(procedure_category, name = "rows"))

# =============================================================================
# (C) INCLUSIVE PROCEDURES  -> GI_PROC_SQL, r_gi ; reconcile vs dataset any_gi_procedure_ever
# =============================================================================
# format-agnostic: pull every integer run, so this works whether the file is the
# comma-separated .txt OR a .csv with a header / extra (TRUE/FALSE) columns.
.raw_ids      <- readr::read_file(inclusive_ids_path)
inclusive_ids <- as.integer(unlist(regmatches(.raw_ids, gregexpr("[0-9]+", .raw_ids))))
inclusive_ids <- unique(inclusive_ids[!is.na(inclusive_ids)])
stopifnot("No concept ids parsed from inclusive_ids_path — check the file contents/path" =
            length(inclusive_ids) > 0)
cli_alert_info("Inclusive procedure concept ids: {length(inclusive_ids)} (e.g. {paste(head(inclusive_ids,3), collapse=', ')})")

inc_cat <- tbl(km, in_catalog(km_db, km_schema, "CONCEPT")) %>% rename_with(tolower) %>%
  filter(concept_id %in% !!inclusive_ids) %>%
  select(concept_id, domain_id, standard_concept, invalid_reason, concept_class_id) %>% collect()
cli_h2("(C) Inclusive catalog cross-check")
cli_alert_info("Resolved {nrow(inc_cat)}/{length(inclusive_ids)} | non-Procedure: {sum(inc_cat$domain_id!='Procedure',na.rm=TRUE)} | deprecated: {sum(!is.na(inc_cat$invalid_reason))} | Hierarchy(non-leaf): {sum(grepl('Hierarchy',inc_cat$concept_class_id,ignore.case=TRUE))}")

proc_inclusive_raw <- tbl(con, in_catalog(db_name_export, schema_name, "PROCEDURE_OCCURRENCE")) %>%
  rename_with(tolower) %>%
  filter(procedure_concept_id %in% !!inclusive_ids,
         procedure_date >= study_start & procedure_date <= study_end) %>%
  mutate(person_id = as.character(person_id)) %>%
  select(person_id, procedure_concept_id, procedure_date) %>%
  collect() %>% mutate(procedure_date = as.Date(procedure_date))

r_gi <- proc_inclusive_raw %>% inner_join(index_dates, by = "person_id") %>%
  filter(procedure_date >= value) %>% transmute(person_id, value = procedure_date)

in_list <- paste(inclusive_ids, collapse = ",")
gi_sql <- glue::glue(
  "WITH index_dates AS ( {index_date_source} )
   SELECT po.person_id, po.procedure_date AS value
   FROM {dataset}.PROCEDURE_OCCURRENCE po
   INNER JOIN index_dates idx ON idx.person_id = po.person_id
   WHERE po.procedure_date BETWEEN DATE('{study_start}') AND DATE('{study_end}')
     AND po.procedure_date >= idx.value
     AND po.procedure_concept_id IN ({in_list})",
  dataset = paste(db_name_export, schema_name, sep = "."))
DBI::dbExecute(con, glue::glue("CREATE OR REPLACE TABLE ANALYSIS.{sandbox_schema}.{gi_out_table} AS ({gi_sql})"))

sql_gi <- tbl(con, in_catalog("ANALYSIS", sandbox_schema, gi_out_table)) %>%
  rename_with(tolower) %>% mutate(person_id = as.character(person_id)) %>%
  collect() %>% mutate(value = as.Date(value))

recon <- full_join(r_gi %>% count(person_id, value, name = "r_n"),
                   sql_gi %>% count(person_id, value, name = "sql_n"),
                   by = c("person_id", "value")) %>%
  mutate(across(c(r_n, sql_n), ~ tidyr::replace_na(.x, 0L)), delta = r_n - sql_n)
if (sum(recon$delta != 0) == 0) cli_alert_success("Inclusive R vs SQL: EXACT MATCH ({nrow(r_gi)} rows)")


ds_anygi <- readr::read_csv(dataset_path, col_types = readr::cols(.default = readr::col_character()), show_col_types = FALSE) %>%
  transmute(person_id = patient_id, any_gi_dataset = tolower(any_gi_procedure_ever) == "true")
recon_anygi <- full_join(ds_anygi,
                         final_r %>% transmute(person_id = as.character(person_id),
                                               any_gi_r = person_id %in% unique(r_gi$person_id)),
                         by = "person_id") %>%
  mutate(across(c(any_gi_dataset, any_gi_r), ~ tidyr::replace_na(.x, FALSE)))
disagree <- recon_anygi %>% filter(any_gi_dataset != any_gi_r)
cli_h2("(C) any_gi_procedure_ever : dataset vs reconstruction")
cli_alert_info("dataset TRUE: {sum(recon_anygi$any_gi_dataset)} | R TRUE: {sum(recon_anygi$any_gi_r)} | disagree: {nrow(disagree)}")
if (nrow(disagree)) {
  bad <- disagree %>% filter(!grepl('^[0-9]+$', person_id))
  if (nrow(bad)) cli_alert_warning("{nrow(bad)} disagreements are malformed (Excel) ids")
  print(head(disagree %>% filter(grepl('^[0-9]+$', person_id)), 20))
} else cli_alert_success("any_gi_procedure_ever agrees for every patient")

# =============================================================================
# (D) SYMPTOM VALIDATION  — anchored to the INCLUSIVE procedures
# =============================================================================
spine        <- final_r %>% transmute(person_id = as.character(person_id))
proc_dates   <- r_gi %>% transmute(person_id = as.character(person_id), procedure_date = as.Date(value)) %>% distinct()
av_fibrosis  <- glue::glue("ANALYSIS.{sandbox_schema}.{gi_out_table}")
proc_persons_tbl <- tbl(con, in_catalog("ANALYSIS", sandbox_schema, gi_out_table)) %>% rename_with(tolower) %>% distinct(person_id)
dataset_dot  <- paste(db_name_export, schema_name, sep = ".")

validate_symptom <- function(label, events_r, window_days, symptom_sql, out_table) {
  cli_h1("{label}  (1-{window_days} days before procedure)")
  pairs   <- events_r %>% inner_join(proc_dates, by = "person_id", relationship = "many-to-many") %>%
    mutate(gap = as.integer(procedure_date - event_date))
  matched <- pairs %>% filter(gap >= 1, gap <= window_days)
  flag_r  <- spine %>% mutate(value_r = person_id %in% unique(matched$person_id))
  DBI::dbExecute(con, glue::glue("CREATE OR REPLACE TABLE ANALYSIS.{sandbox_schema}.{out_table} AS ({symptom_sql})"))
  flag_sql <- tbl(con, in_catalog("ANALYSIS", sandbox_schema, out_table)) %>% rename_with(tolower) %>%
    mutate(person_id = as.character(person_id)) %>% collect() %>% transmute(person_id, value_sql = as.logical(value))
  recon <- full_join(flag_r, flag_sql, by = "person_id") %>% mutate(across(c(value_r, value_sql), ~ tidyr::replace_na(.x, FALSE)))
  dis <- recon %>% filter(value_r != value_sql)
  cli_alert_info("SQL TRUE: {sum(recon$value_sql)} | R TRUE: {sum(recon$value_r)} | disagree: {nrow(dis)}")
  if (nrow(matched)) cli_alert_info("gap range: [{min(matched$gap)}, {max(matched$gap)}] days (expect [1, {window_days}])")
  if (nrow(dis) == 0) cli_alert_success("flag matches for every patient") else cli_alert_danger("{nrow(dis)} disagree")
  invisible(recon)
}

# abdominal mass, 30d
abdom_events <- tbl(con, in_catalog(db_name_export, schema_name, "CONDITION_OCCURRENCE")) %>% rename_with(tolower) %>%
  filter(condition_concept_id %in% !!abdom_ids, condition_start_date >= study_start, condition_start_date <= study_end) %>%
  semi_join(proc_persons_tbl, by = "person_id") %>% mutate(person_id = as.character(person_id)) %>%
  transmute(person_id, event_date = condition_start_date) %>% collect() %>% mutate(event_date = as.Date(event_date))
abdom_sql <- r"(
WITH index_dates AS ( {index_date_source} ),
procedure_dates AS ( SELECT fp.person_id, fp.value AS procedure_date FROM {av_fib} fp ),
abdom AS (SELECT co.person_id, co.condition_start_date AS event_date FROM {dataset}.CONDITION_OCCURRENCE co
          WHERE co.CONDITION_CONCEPT_ID IN (192438,4223207,4139255,37311117,37311118,37311119,37311120)
            AND co.CONDITION_START_DATE BETWEEN DATE('{study_start}') AND DATE('{study_end}')),
matched AS (SELECT DISTINCT a.person_id FROM abdom a INNER JOIN procedure_dates pd
            ON pd.person_id=a.person_id AND a.event_date BETWEEN DATEADD('day',-30,pd.procedure_date) AND DATEADD('day',-1,pd.procedure_date))
SELECT idx.person_id, CASE WHEN m.person_id IS NOT NULL THEN TRUE ELSE FALSE END AS value
FROM index_dates idx LEFT JOIN matched m ON m.person_id=idx.person_id)"
abdom_sql <- gsub("{index_date_source}", index_date_source, abdom_sql, fixed = TRUE)
abdom_sql <- gsub("{av_fib}", av_fibrosis, abdom_sql, fixed = TRUE)
abdom_sql <- gsub("{dataset}", dataset_dot, abdom_sql, fixed = TRUE)
abdom_sql <- gsub("{study_start}", as.character(study_start), abdom_sql, fixed = TRUE)
abdom_sql <- gsub("{study_end}",   as.character(study_end),   abdom_sql, fixed = TRUE)
validate_symptom("symptom_abdominal_mass (30d)", abdom_events, 30L, abdom_sql, "SYMPTOM_ABDOM_MASS_30D_SQL")

# diarrhea, 60d (condition + observation)
diarrhea_ids <- group_ids(diarrhea_group) %>% collect() %>% pull(concept_id)
diarrhea_co <- tbl(con, in_catalog(db_name_export, schema_name, "CONDITION_OCCURRENCE")) %>% rename_with(tolower) %>%
  filter(condition_concept_id %in% !!diarrhea_ids, condition_start_date >= study_start, condition_start_date <= study_end) %>%
  semi_join(proc_persons_tbl, by = "person_id") %>% mutate(person_id = as.character(person_id)) %>%
  transmute(person_id, event_date = condition_start_date) %>% collect()
diarrhea_obs <- tbl(con, in_catalog(db_name_export, schema_name, "OBSERVATION")) %>% rename_with(tolower) %>%
  filter(observation_concept_id %in% !!diarrhea_ids, observation_date >= study_start, observation_date <= study_end) %>%
  semi_join(proc_persons_tbl, by = "person_id") %>% mutate(person_id = as.character(person_id)) %>%
  transmute(person_id, event_date = observation_date) %>% collect()
diarrhea_events <- bind_rows(diarrhea_co, diarrhea_obs) %>% mutate(event_date = as.Date(event_date))
diarrhea_sql <- r"(
WITH index_dates AS ( {index_date_source} ),
procedure_dates AS ( SELECT fp.person_id, fp.value AS procedure_date FROM {av_fib} fp ),
dc AS (SELECT co.person_id, co.condition_start_date AS event_date FROM {dataset}.CONDITION_OCCURRENCE co
       INNER JOIN TOOLING.PLATFORM.ALL_CONCEPT_GROUP_CONCEPT g ON g.CONCEPT_ID=co.CONDITION_CONCEPT_ID AND g.OMOP_CONCEPT_GROUP_ID='6059529768801722246-Verantos'
       WHERE co.CONDITION_START_DATE BETWEEN DATE('{study_start}') AND DATE('{study_end}')),
dobs AS (SELECT o.person_id, o.observation_date AS event_date FROM {dataset}.OBSERVATION o
         INNER JOIN TOOLING.PLATFORM.ALL_CONCEPT_GROUP_CONCEPT g ON g.CONCEPT_ID=o.OBSERVATION_CONCEPT_ID AND g.OMOP_CONCEPT_GROUP_ID='6059529768801722246-Verantos'
         WHERE o.OBSERVATION_DATE BETWEEN DATE('{study_start}') AND DATE('{study_end}')),
ev AS (SELECT person_id,event_date FROM dc UNION ALL SELECT person_id,event_date FROM dobs),
matched AS (SELECT DISTINCT e.person_id FROM ev e INNER JOIN procedure_dates pd
            ON pd.person_id=e.person_id AND e.event_date BETWEEN DATEADD('day',-60,pd.procedure_date) AND DATEADD('day',-1,pd.procedure_date))
SELECT idx.person_id, CASE WHEN m.person_id IS NOT NULL THEN TRUE ELSE FALSE END AS value
FROM index_dates idx LEFT JOIN matched m ON m.person_id=idx.person_id)"
diarrhea_sql <- gsub("{index_date_source}", index_date_source, diarrhea_sql, fixed = TRUE)
diarrhea_sql <- gsub("{av_fib}", av_fibrosis, diarrhea_sql, fixed = TRUE)
diarrhea_sql <- gsub("{dataset}", dataset_dot, diarrhea_sql, fixed = TRUE)
diarrhea_sql <- gsub("{study_start}", as.character(study_start), diarrhea_sql, fixed = TRUE)
diarrhea_sql <- gsub("{study_end}",   as.character(study_end),   diarrhea_sql, fixed = TRUE)
validate_symptom("symptom_diarrhea (60d)", diarrhea_events, 60L, diarrhea_sql, "SYMPTOM_DIARRHEA_60D_SQL")

cli_h1("SALES-101 QC complete — objects ready for the report tables")
