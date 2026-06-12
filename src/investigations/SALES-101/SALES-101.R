# Initialize ===================================================================
# https://verantos.atlassian.net/browse/SALES-101

# cohort and AD were created in research assistant - pulling in here

dataset <- read.csv(here::here("data/SCIENCE-101/dataset.csv"))

dataset <- dataset %>%
  mutate(across(where(~ is.character(.x) && all(.x %in% c("true", "false", NA))),
                as.logical))

data_dict <- read.csv(here::here("data/SCIENCE-101/data_dictionary.csv"))

# create tables ================================================================

# build list of labels from data dictionary
label_list <- data_dict %>%
  filter(COLUMN_NAME %in% names(dataset)) %>%
  select(COLUMN_NAME, NAME) %>%
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
  tbl_summary(include = c(index_year, age_at_index, gender, race, ethnicity),
              label = list(index_year ~ "Year of index",
                           age_at_index ~ "Age at index",
                           gender ~ "Sex",
                           race ~ "Race",
                           ethnicity ~ "Ethnicity"),
              type = list(index_year ~ "categorical"))
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

proc_ever <- dataset %>% filter(any_gi_procedure_ever)   # 5,049 patients

table_labs_indices <- merge_pre_proc(
  proc_ever,
  stems = c("crp_or_calprotectin_pre_fibrosis_procedure",
            "cdai_or_hbi_pre_fibrosis_procedure"),
  labels = list(
    crp_or_calprotectin_pre_fibrosis_procedure ~ "CRP or calprotectin",
    cdai_or_hbi_pre_fibrosis_procedure          ~ "CDAI or HBI"
  )
)

table_symptoms <- merge_pre_proc(
  proc_ever,
  stems = c("symptom_abdominal_mass_pre_fibrosis_proc",
            "symptom_abdominal_pain_pre_fibrosis_proc",
            "symptom_arthritis_pre_fibrosis_proc",
            "symptom_diarrhea_pre_fibrosis_proc",
            "symptom_erythema_nodosum_pre_fibrosis_proc",
            "symptom_fatigue_pre_fibrosis_proc",
            "symptom_iritis_uveitis_pre_fibrosis_proc",   # renamed
            "symptom_perianal_pre_fibrosis_proc",         # renamed
            "symptom_pyoderma_pre_fibrosis_proc"),
  labels = list(
    symptom_abdominal_mass_pre_fibrosis_proc    ~ "Abdominal mass",
    symptom_abdominal_pain_pre_fibrosis_proc    ~ "Abdominal pain",
    symptom_arthritis_pre_fibrosis_proc         ~ "Arthritis",
    symptom_diarrhea_pre_fibrosis_proc          ~ "Diarrhea",
    symptom_erythema_nodosum_pre_fibrosis_proc  ~ "Erythema nodosum",
    symptom_fatigue_pre_fibrosis_proc           ~ "Fatigue",
    symptom_iritis_uveitis_pre_fibrosis_proc    ~ "Iritis/uveitis",
    symptom_perianal_pre_fibrosis_proc          ~ "Perianal disease",
    symptom_pyoderma_pre_fibrosis_proc          ~ "Pyoderma"
  )
)

table_2 <- tbl_stack(tbls = list(table_labs_indices, table_symptoms),
                     group_header = c("Laboratory measures and clinical scoring measures",
                                      "CDAI-related symptoms")) %>%
  as_gt() %>%
  gt::tab_style(
    style = gt::cell_text(weight = "bold"),
    locations = gt::cells_row_groups(groups = everything())
  )
table_2

table_symptoms_post_index <- dataset %>%
  tbl_summary(
    include = c(symptom_abdominal_mass_post_index,
                symptom_abdominal_pain_post_index,
                symptom_arthritis_post_index,
                symptom_diarrhea_post_index,
                symptom_erythema_nodosum_post_index,
                symptom_fatigue_post_index,
                symptom_iritis_uveitis_post_index,
                symptom_perianal_post_index,
                symptom_pyoderma_post_index),
    label = list(
      symptom_abdominal_mass_post_index   ~ "Abdominal mass",
      symptom_abdominal_pain_post_index   ~ "Abdominal pain",
      symptom_arthritis_post_index        ~ "Arthritis",
      symptom_diarrhea_post_index         ~ "Diarrhea",
      symptom_erythema_nodosum_post_index ~ "Erythema nodosum",
      symptom_fatigue_post_index          ~ "Fatigue",
      symptom_iritis_uveitis_post_index   ~ "Iritis/uveitis",
      symptom_perianal_post_index         ~ "Perianal disease",
      symptom_pyoderma_post_index         ~ "Pyoderma"
    )
  )
table_symptoms_post_index

table_ip_flare <- dataset %>%
  tbl_summary(include = c(inpatient_hosp_crohns_flare),
              label = label_list)
table_ip_flare

# first write of the report list (table_procedures appended later, after the
# procedures validation produces r_result)
sales_101 <- list(table_demo, table_2, table_ip_flare, table_symptoms_post_index)
write_rds(sales_101, "data/SCIENCE-101/sales_101.rds")

source(here::here("src/investigations/SALES-101/SALES-101_QC.R"))

# overall flag from the regenerated dataset — read patient_id as character so the
# 19-digit id joins exactly to r_result (read.csv would round it to a double)
proc_overall <- readr::read_csv(
  here::here("data/SCIENCE-101/dataset.csv"),
  col_types = readr::cols(.default = readr::col_character()),
  show_col_types = FALSE
) %>%
  transmute(person_id = patient_id,
            any_gi_procedure = tolower(any_gi_procedure_ever) == "true")

# fibrosis-specific subgroup flags from the validated procedures (r_result)
proc_flags <- r_result %>%
  transmute(person_id = as.character(person_id),
            cat = dplyr::recode(procedure_category,
                                "Crohns resection"            = "resection",
                                "Endoscopic balloon dilation" = "ebd",
                                "Strictureplasty"             = "strictureplasty")) %>%
  distinct() %>%
  mutate(flag = TRUE) %>%
  tidyr::pivot_wider(names_from = cat, values_from = flag, values_fill = FALSE)
for (cc in c("resection", "ebd", "strictureplasty"))
  if (!cc %in% names(proc_flags)) proc_flags[[cc]] <- FALSE

proc_patient <- proc_overall %>%
  left_join(proc_flags, by = "person_id") %>%
  mutate(across(c(resection, ebd, strictureplasty), ~ tidyr::replace_na(.x, FALSE)))

table_procedures <- proc_patient %>%
  tbl_summary(
    include = c(any_gi_procedure, resection, ebd, strictureplasty),
    label = list(
      any_gi_procedure ~ "Any GI-related procedure",
      resection        ~ "Intestinal resection",
      ebd              ~ "Endoscopic balloon dilation",
      strictureplasty  ~ "Strictureplasty"
    )
  ) %>%
  modify_footnote(
    all_stat_cols() ~ paste(
      "n (%) of the full cohort. 'Any GI-related procedure' uses the inclusive procedure code list;",
      "the three subclassifications reflect the stricture-specific procedure types, are not mutually",
      "exclusive, and need not sum to the overall total."
    )
  )
table_procedures

# =============================================================================
# Cohort attrition table — from QC objects (run sales_101_QC.R first):
#   person, crohns_index_r, include_ids, uc_persons, ibd_unspec_persons, final_r
# =============================================================================
crohns_ids  <- unique(as.character(crohns_index_r$person_id))
age_ids     <- unique(as.character(include_ids))
uc_ids      <- unique(as.character(uc_persons$person_id))
ibd_ids     <- unique(as.character(ibd_unspec_persons$person_id))
nobirth_ids <- person %>% filter(is.na(derived_birth_date)) %>% pull(person_id) %>% as.character()

s1 <- crohns_ids                       # >=1 Crohn's dx in study window
s2 <- intersect(s1, age_ids)           # age >=18 at index
s3 <- setdiff(s2, uc_ids)              # exclude UC history
s4 <- setdiff(s3, ibd_ids)             # exclude IBD-unspecified history -> FINAL
stopifnot(length(setdiff(s4, nobirth_ids)) == nrow(final_r), length(s4) == nrow(final_r))

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
  mutate(n_excluded = dplyr::lag(n_remaining) - n_remaining,
         pct_of_crohns = round(100 * n_remaining / length(s1), 1))
attrition$pct_of_crohns[1] <- NA_real_

attrition_gt <- attrition %>%
  gt() %>%
  tab_header(title = "Crohn's Cohort Attrition",
             subtitle = "IBD Pragmatic Registry 2026Q1  |  study period 2015-01-01 to 2026-01-08") %>%
  cols_label(step = "Step", n_remaining = "Patients remaining",
             n_excluded = "Excluded at step", pct_of_crohns = "% of Crohn's-indexed") %>%
  fmt_number(c(n_remaining, n_excluded), decimals = 0, use_seps = TRUE) %>%
  fmt_number(pct_of_crohns, decimals = 1) %>%
  sub_missing(columns = c(n_excluded, pct_of_crohns), missing_text = "—") %>%
  tab_style(style = cell_text(weight = "bold"), locations = cells_body(rows = c(1))) %>%
  cols_align("left", columns = step)
attrition_gt

# =============================================================================
# final report list
# =============================================================================
sales_101 <- readr::read_rds("data/SCIENCE-101/sales_101.rds")
sales_101 <- c(sales_101, list(attrition_gt, table_procedures))
write_rds(sales_101, "data/SCIENCE-101/sales_101.rds")

