# Install and load packages ====

# Package names
packages <- c(
  "RPostgres",
  "DBI",
  "tidyverse",
  "dbplyr",
  "lubridate",
  "DescTools",
  "janitor",
  "ggplot2",
  "plotly",
  "gt",
  "table1",
  "devtools",
  "default",
  "tictoc",
  "rairtable",
  "whereami",
  "openxlsx",
  "huxtable",
  "gtsummary"
)

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())

if (any(installed_packages == FALSE)) {
  renv::install(packages[!installed_packages], type = "binary", prompt = FALSE, verbose = TRUE)
}

# bstfun is a gtsummary helper not on CRAN
if (!"bstfun" %in% rownames(installed.packages())) {
  remotes::install_github("ddsjoberg/bstfun")
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))

# Change default args
default::default(arrange) <- list(.by_group = TRUE) # always include group cols

# install.packages("devtools")
remotes::install_git("git@github.com:verantos-dev/verantos.git", force = TRUE)
library(verantos)
readRenviron(here::here(".env"))

# Study dates -----------------------------------------------------------------

study_start_date <- as.Date("2014-01-01") # baseline start
study_end_date <- as.Date("2024-08-01")


# VPN connection required =====================================================

## code lists -----------------------------------------------------------------

tic_wrapper <- function(text = "outpatient", run, date_col = "exacerbation_date"){
  tictoc::tic(text)

  output <- run

  tictoc::toc()

  if(output %>% nrow() > 0){
    expect_date_within_period(output, date_col)
  }
  return(output)

}

get_km_code_list <- function(strip_icd9_10 = TRUE, concept_filter = NULL) {

  if(!is.null(concept_filter)){
    concept_filter = tolower(concept_filter)
  }

  tbl(snowflake_connect(
    db_name = "KNOWLEDGE_MANAGEMENT",
    db_warehouse = "DEV_WH",
    db_role = "DATA_DEV",
    use_schema = "OMOP_METADATA"
  ),
  in_schema("OMOP_METADATA", "INSIGHTS_CODE_LIST_NEW")) %>%
    {`if` (!is.null(concept_filter),
           filter(., grepl(concept_filter, tolower(CONCEPT_GROUP_NAME))),
           .)} %>%
    {`if` (strip_icd9_10,
           mutate(., CONCEPT_CODE = if_else(
             VOCABULARY_ID %in% c("ICD10CM", "ICD9CM"),
             str_remove_all(CONCEPT_CODE, "\\."),
             CONCEPT_CODE)),
           .)} %>%
    rename(CODE_LIST_CONCEPT_NAME = CONCEPT_NAME)
}

if(FALSE){
  source(here::here("initialize/airtable.R"))
} else {
  insights_code_list <- get_km_code_list() %>%
    rename_with(tolower) %>%
    select(-deprecated_do_not_use_ics_dose_classification)
}

## definitions ----------------------------------------------------------------
source(here::here("initialize/definitions.R"))

## functions ------------------------------------------------------------------
use_client <- function(client = c("providence", "vanderbilt", "vanderbilt - teze", "providence - teze", "asthma registry", "asthma registry old")) {
  client <- match.arg(client)
  assign("client", client, envir = .GlobalEnv)
  source(here::here("config.R"))
  source(here::here("initialize/database_connection.R"))
  cli::cli_alert_success("Using client {.val {client}}")
}

to_natural_list <- function(vec) {
  if (length(vec) < 2) {
    return(toString(vec))
  }
  paste0(
    paste(head(vec, -1), collapse = ", "),
    " and ",
    tail(vec, 1)
  )
}

rndr = function(x, ...){
  y = render.default(x, ...)
  if(is.logical(x)) y[[2]] else y
}

# renders rmd with jobs so you can simultaneously work while running report
render_with_jobs <- function(){

  rstudioapi::verifyAvailable()
  jobs_file <- tempFile(tmpdir = "/tmp", fileext  = ".R")
  rmd_to_render <- rstudioapi::selectFile(caption = "Choose an Rmd file...",
                                          filter  = "Rmd files (*.Rmd)")

  if(is.null(rmd_to_render)){
    stop("You must choose an Rmd file to proceed!")
  }
  cat(paste0('rmarkdown::render("',rmd_to_render,'")'), file = jobs_file)
  rstudioapi::jobRunScript(path = jobs_file,
                           name = basename(rmd_to_render),
                           workingDir = getwd())

  Sys.sleep(5)
  file.remove(jobs_file)
}

get_code_list <- function(con, schema_name, strip_icd10 = TRUE, use_local = FALSE) {

  if (use_local & file.exists(here::here("data/code list-Table 1.csv"))) {
    # use a downloaded/exported version of the code list instead of the online version
    return(read_csv("data/code list-Table 1.csv",
                    col_types = cols(...6 = col_skip())) %>%
             distinct())
  }

  # New codes to add
  new_codes <- tribble(
    ~name,                                        ~vocabulary_id, ~concept_code, ~concept_name,                                         ~ics_dose_classification,
    "total immunoglobulin E (IgE) level (IU/mL)", "LOINC",        "76488-6",     "IgE [Mass/volume] in Serum or Plasma by Immunoassay", NA_character_,
    "total immunoglobulin E (IgE) level (IU/mL)", "SNOMED",       "41960005",    "Immunoglobulin E measurement",NA_character_,
    "biologic-mepolizumab",	"RxNorm",	"1720597",	"mepolizumab",NA_character_,
    "biologic-omalizumab",	"RxNorm",	"302379",	"omalizumab",NA_character_,
    "FEV1/FVC ratio percent predicted - pre-bronchodilator",	"PSJH",	"12628",	"FEV1/FVC % Predicted",NA_character_
    #"asthma", "SNOMED", "707980005", "Acute severe exacerbation of moderate persistent asthma", NA_character_
  )

  tbl(con, in_schema(schema_name, "code_list")) %>%
    union(
      new_codes,
      copy = TRUE
    )  %>%
    {`if` (strip_icd10,
           mutate(., concept_code = if_else(
             vocabulary_id == "ICD10CM",
             str_remove_all(concept_code, "\\."),
             concept_code)),
           .)} %>%
    # trim trailing whitespace
    mutate(
      concept_code = trimws(concept_code, "right"),
      concept_name = trimws(concept_name, "right")
    )
}

writeDataFrameToDB <-
  function(data_frame,
           connection = con,
           target_schema = schema_name,
           target_table = deparse(substitute(data_frame))) {
    dbWriteTable(
      conn = connection,
      name = Id(schema = target_schema, table = target_table),
      value = data_frame,
      copy = TRUE,
      row.names = FALSE,
      overwrite = TRUE,
    )
  }

# Test method for verifying the date is within the study period
expect_date_within_period <- function(df, date_column, period = c(study_start_date, study_end_date)) {
  testthat::test_that(
    "test that the dates do not exceed the study period",
    {
      testthat::expect_gte(
        df %>%
          ungroup %>%
          summarize(min = min(!!rlang::sym(date_column), na.rm = T)) %>% pull(),
        period[[1]]
      )

      if(!is.na(period[[2]])){
        testthat::expect_lte(
          df %>%
            ungroup %>%
            summarize(max = max(!!rlang::sym(date_column), na.rm = T)) %>% pull(),
          period[[2]]
        )
      }

    })
}



# For OCS maintenance and all control defined severe asthma meds,
# This function finds the window start date such that no two meds dates
# are within 7 days of each other, but intervening dates do not cause
# "reanchoring". Rather, the 7-day window restarts on the 8th day and
# a new window start date can be chosen.
# This function does not filter so that max days_supply can be gathered
# for each 7-day window per person.
find_window_start <- function(df, datecol) {
  datecol <- enquo(datecol)
  result_dates <- as.Date(character(0))  # Initialize an empty date vector
  exclusion_date <- df %>% pluck(rlang::as_name(datecol)) %>% first()

  for (i in 1:nrow(df)) {
    current_date <- pluck(df, rlang::as_name(datecol))[i]
    if (current_date >= exclusion_date) {
      result_dates <- c(result_dates, current_date)
      exclusion_date <- current_date + 7
    }
  }

  df %>%
    mutate(window_start = if_else(!!datecol %in% result_dates, !!datecol, NA)) %>%
    group_by(person_id) %>%
    fill(window_start, .direction = c("down"))
}

get_person_table <- function(con, schema_name, snowflake = snowflake, testing){

  person <-
    tbl(con, in_schema(schema_name, if_else(snowflake, "PERSON", "person"))) %>%
    {`if` (snowflake,
           rename_with(., ~ tolower(.)),
           .)} %>%
    {
      `if` (testing,
            head(., n = 100),
            .)
    }

  return(person)
}

get_visit_occurrence <-
  function(con,
           schema_name) {

    tbl(con, in_schema(schema_name, "VISIT_OCCURRENCE")) %>%
      rename_with(tolower)
  }

get_insights_code_list <- function(strip_icd9_10 = TRUE, collect = FALSE) {

  code_list <- tbl(
    snowflake_connect(
      connection_name = "km",
      db_name = "KNOWLEDGE_MANAGEMENT",
      db_warehouse = "DEV_WH",
      db_role = "DATA_DEV",
      use_schema = "OMOP_METADATA"
    ),
    in_schema("OMOP_METADATA", "INSIGHTS_CODE_LIST")
  ) %>%
    rename_with(tolower) %>%
    {`if` (strip_icd9_10,
           dplyr::mutate(.,
                         concept_code = if_else(
                           vocabulary_id %in% c("ICD10CM","ICD9CM"),
                           str_remove_all(concept_code, "\\."),
                           concept_code)
           ))
    }

  if(collect){
    code_list <- code_list %>%
      collect()
  }

  return(code_list)

}


quick_count <- function(df, ..., today = TRUE, add_size = FALSE) {
  grouping_columns <- enquos(...)
  if ("PERSON_ID" %in% colnames(df)) {
    person_id_col <- rlang::sym("PERSON_ID")
  } else if ("person_id" %in% colnames(df)) {
    person_id_col <- rlang::sym("person_id")
  } else {
    cli::cli_abort("{.val person_id} or {.val PERSON_ID} not found; exiting.")
  }
  obj.size = format(object.size(df), units = "auto")

  if (sum(pull(count(df))) == 0) {
    df <- df %>% collect() %>% select(!!!grouping_columns) %>% add_row() %>% mutate(n = 0, n_pat = 0)
  } else {
    if (length(grouping_columns) > 0) {
      df <- df %>% group_by(!!!grouping_columns)
    }
    df <- df %>%
      summarise(n = n(), n_pat = n_distinct(!!person_id_col), .groups = "keep")
  }

  df %>%
    {`if` (today, mutate(., today = today()), .)} %>%
    {`if` (add_size, mutate(., obj.size = obj.size), .)}
}

get_condition_occurrence <- function(con, schema_name, strip_icd9_10 = TRUE) {

  tbl(con, in_schema(schema_name, "CONDITION_OCCURRENCE")) %>%
    rename_with(tolower) %>%
    {`if` (strip_icd9_10,
           dplyr::mutate(.,
                         condition_concept_code = if_else(
                           condition_vocabulary_id %in% c("ICD10CM","ICD9CM"),
                           str_remove_all(condition_concept_code, "\\."),
                           condition_concept_code)
           ))
    } %>%
    left_join(
      get_study_optimized_concept_attribute(con, schema_name),
      by = c(
        "condition_vocabulary_id" = "concept_vocabulary_id",
        "condition_concept_code" = "concept_code"
      ),
      copy = TRUE
    ) %>%
    filter(
      (
        # curated records must be experienced and optimized
        grepl("^CURATED", dataset_id) &
          is_experienced == TRUE &
          optimized_experienced == TRUE
      ) |
        !grepl("^CURATED", dataset_id)
    ) %>%
    left_join(
      get_visit_occurrence(con, schema_name) %>%
        select(person_id, visit_occurrence_id, visit_start_date),
      by = c(
        "person_id", "visit_occurrence_id"
      )
    ) %>%
    mutate(condition_start_date = coalesce(condition_start_date, visit_start_date)) %>%
    # {`if` (STUDY_PERIOD_ONLY,
    #        filter(., condition_start_date >= study_start_date &
    #                 condition_start_date <= study_end_date),
    #        .)} %>%
    dplyr::select(-visit_start_date) %>%
    distinct

}

get_observation <- function(con, schema_name) {

  tbl(con, in_schema(schema_name,"OBSERVATION")) %>%
    rename_with(tolower) %>%
    mutate(
      observation_concept_code = if_else(
        observation_vocabulary_id %in% c("ICD10CM","ICD9CM"),
        str_remove_all(observation_concept_code, "\\."),
        observation_concept_code)
    ) %>%
    left_join(
      get_study_optimized_concept_attribute(con, schema_name),
      by = c(
        "observation_vocabulary_id" = "concept_vocabulary_id",
        "observation_concept_code" = "concept_code"
      ),
      copy = TRUE
    )  %>%
    filter(
      (
        grepl("^CURATED", dataset_id) &
          is_experienced == TRUE &
          optimized_experienced == TRUE
      ) |
        !grepl("^CURATED", dataset_id)
    ) %>%
    left_join(
      get_visit_occurrence(con, schema_name) %>%
        select(person_id, visit_occurrence_id, visit_start_date),
      by = c(
        "person_id", "visit_occurrence_id"
      )
    ) %>%
    mutate(observation_date = coalesce(observation_date, visit_start_date)) %>%
    {`if` (STUDY_PERIOD_ONLY,
           filter(., observation_date >= study_start_date &
                    observation_date <= study_end_date),
           .)} %>%
    distinct()

}

calculate_hru <- function(study_cohort,
                          hru_df,
                          groups = asthma_dictionary,
                          #group_labels = unlist(unname(asthma_control_group_dictionary)),
                          quiet = TRUE) {

  # lists for storing output variables
  output = list()
  final_output = list()
  interim_output = list()
  group_labels = unlist(groups[[3]])

  ## loop over asthma control groups ----
  for (j in seq_along(groups %>% row_number())) {

    if (!quiet) cli::cli_text("{.val {j}}) {groups[[2]][[j]]}")

    field = groups[[1]][[j]]
    group_temp = groups[[2]][[j]]

    study_cohort_temp <- study_cohort %>%
      filter(!!sym(field) %in% group_temp) %>%
      select(person_id, follow_up_start_date, follow_up_end_date) %>%
      # Follow up begins one day after index HM 05/03/2024
      mutate(person_time_at_risk = as.numeric(difftime(follow_up_end_date,follow_up_start_date, unit = "days")),
             total_person_time_at_risk = sum(person_time_at_risk)/365.25
      )

    cohort_n <- length(study_cohort_temp$person_id)

    ## loop over hru_groups ----
    # `hru_groups` defined in "common_utils/definitions.R"
    for (i in seq_along(hru_groups)) {
      if (!quiet) cli::cli_text("{.val {j}}.{.val {i}}. {hru_groups[i]}")

      utilization_group <- gsub(":", "_", hru_groups[i])
      utilization <- strsplit(hru_groups[i], ":")[[1]][1]
      cause <- strsplit(hru_groups[i], ":")[[1]][2]

      hru_base_df_temp <- study_cohort_temp %>%
        inner_join(hru_df,
                   join_by(person_id),
                   relationship = "many-to-many"
        )

      #hru_base_df_temp %>% count(utilization_source, utilization_type, allcause)

      if (utilization == "total") {

        hru_base_df_temp <- hru_base_df_temp %>%
          filter(!!rlang::sym(cause)) %>%
          mutate(utilization_group = !!utilization_group)

      } else {

        hru_base_df_temp <- hru_base_df_temp %>%
          filter(!!rlang::sym(cause)) %>%
          filter(utilization_type == !!utilization) %>%
          mutate(utilization_group = !!utilization_group)

        #hru_base_df_temp %>% count(utilization_source, utilization_type, allcause)

      }

      if (nrow(hru_base_df_temp) == 0) {

        output[[i]] <- tibble(cohort = utilization_group,
                              utilization_n = NA,
                              total_person_time_at_risk = NA,
                              event_rate = NA)

      } else {

        output[[i]] <- hru_base_df_temp %>%
          mutate(
            cohort = utilization_group,
            utilization_n = n_distinct(person_id, visit_start_date, utilization_source), # Every utilization is a distinct HRU.
            # utilization_n = n_distinct(person_id, visit_start_date, utilization_type=="outpatient_prescription"), # Only prescriptions are distinct utilizations.
            event_rate = (utilization_n / total_person_time_at_risk),
            .after = utilization_group
          ) %>%
          distinct(cohort,
                   utilization_n,
                   total_person_time_at_risk,
                   event_rate)
        # print(output[[i]])
      }

      interim_output[[j]] <- data.frame(
        cohort = group_labels[j],
        utilization_n = NA,
        total_person_time_at_risk = NA,
        event_rate = NA
      )

      # cli::cli_text("{.val {j}}.{.val {i}}. {hru_groups[i]}: n = {.val {output[[i]]$utilization_n[1]}}")

      interim_output_v2 <- do.call(rbind, output)
      interim_output_v2 <- interim_output_v2 %>%
        fill(total_person_time_at_risk, .direction = "downup") %>%
        mutate(across(c(utilization_n, event_rate), ~replace_na(., FALSE)))
    } # end i

    if (all(is.na(interim_output_v2$total_person_time_at_risk))) {

      interim_output_v2$event_rate <- NA

    } else {

      ci_df <- epitools::pois.exact(interim_output_v2$utilization_n, interim_output_v2$total_person_time_at_risk,
                                    conf.level = 0.95) %>% as_tibble()

      ci_df$lower <- round(ci_df$lower*100,digits=2)
      ci_df$upper <- round(ci_df$upper*100,digits=2)

      interim_output_v2$event_rate <- paste0(format(round(interim_output_v2$event_rate*100, digits=2), nsmall=2, trim=TRUE), " (",
                                             ci_df$lower, "-",
                                             ci_df$upper, ")")
    }

    interim_output[[j]] <- rbind(
      interim_output[[j]],
      interim_output_v2
    )

  } # end j

  do.call(rbind, interim_output)

}

get_contiguous_enrollment <- function(con, schema_name, snowflake, allowable_gap = 45){

  tbl(con, in_schema(
    schema_name,
    if_else(snowflake, "PAYER_PLAN_PERIOD", "payer_plan_period")
  )) %>%
    {
      `if` (snowflake,
            rename_with(., ~ tolower(.)),
            .)
    } %>%
    distinct(
      person_id,
      plan_concept_name,
      payer_plan_period_start_date,
      payer_plan_period_end_date,
      dataset_id
    ) %>%
    mutate(
      payer_plan_period_start_date = coalesce(payer_plan_period_start_date, payer_plan_period_end_date),
      payer_plan_period_end_date = coalesce(payer_plan_period_end_date, payer_plan_period_start_date)
    ) %>%
    collect() %>%
    distinct(person_id,
             payer_plan_period_start_date,
             payer_plan_period_end_date) %>%
    group_by(person_id) %>%
    arrange(payer_plan_period_start_date,
            payer_plan_period_end_date,
            .by_group = TRUE) %>%
    mutate(
      cum_end_date = as.Date(cummax(as.numeric(
        lag(
          payer_plan_period_end_date,
          default = first(payer_plan_period_end_date)
        )
      )), origin = "1970-01-01"),
      plan_index = cumsum(payer_plan_period_start_date > cum_end_date + allowable_gap)
    ) %>%
    group_by(person_id, plan_index) %>%
    mutate(
      payer_plan_period_start_date = min(payer_plan_period_start_date, na.rm = TRUE),
      payer_plan_period_end_date = max(payer_plan_period_end_date, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    distinct(person_id,
             payer_plan_period_start_date,
             payer_plan_period_end_date)

}

make_base_exacerbation_table <- function(con, schema_name, snowflake, testing) {
  # Find asthma diagnoses from condition_occurrence
  get_condition_occurrence(con, schema_name, snowflake, testing) %>%
    select(
      person_id,
      condition_occurrence_id,
      visit_occurrence_id,
      condition_concept_code,
      condition_vocabulary_id,
      condition_concept_name,
      condition_start_date,
      condition_status_concept_code,
      condition_status_concept_name,
      condition_status_vocabulary_id,
      condition_type_concept_code,
      condition_type_concept_name,
      condition_type_vocabulary_id,
      dataset_id,
      is_experienced,
      optimized_experienced
    ) %>%
    # join to visit_occurrence
    inner_join(
      get_visit_occurrence(con, schema_name) %>%
        select(
          person_id,
          dataset_id,
          visit_concept_name,
          visit_source_concept_name, # added for Vanderbilt
          visit_occurrence_id,
          visit_start_date,
          visit_end_date,
          visit_type_concept_name
        ),
      by = c("person_id", "visit_occurrence_id", "dataset_id")
    ) %>%
    # filter to baseline period - this was previously done inside inner join above
    # however, there are some visits with associated condition start dates, etc.
    # do we trust visit start date more? no...
    {`if` (STUDY_PERIOD_ONLY,
           # condition start date already combined with visit_start_date
           # in get_condition_occurrence
           filter(., condition_start_date >= study_start_date &
                    condition_start_date <= study_end_date),
           .)} %>%
    collect() %>%
    # join to asthma codes
    left_join(
      insights_code_list %>%
        filter(name == "asthma"),
      by = c("condition_vocabulary_id" = "vocabulary_id",
             "condition_concept_code" = "concept_code"),
      copy = TRUE
    )

}

get_drug_exposure <- function(con, schema_name, snowflake, testing) {
  # there are no ICD10CM codes in drug_exposure table
  get_meds(con, schema_name, snowflake, testing) %>%
    collect() %>%
    # filter to baseline period
    inner_join(
      insights_code_list %>%
        distinct(vocabulary_id, concept_code, name),
      by = c(
        "drug_vocabulary_id" = "vocabulary_id",
        "drug_concept_code" = "concept_code"),
      relationship = "many-to-many")
}

get_meds <- function(con, schema_name, snowflake, testing) {

  # there are no ICD10CM or ICD9CM codes in drug_exposure table
  tbl(con, in_schema(schema_name,  if_else(snowflake, "DRUG_EXPOSURE", "drug_exposure"))) %>%
    {`if` (snowflake,
           rename_with(., ~ tolower(.)),
           .)} %>%
    {
      `if` (testing,
            head(., n = 100000),
            .)
    } %>%
    # curated records must be experienced and optimized
    left_join(
      get_study_optimized_concept_attribute(con, schema_name),
      join_by(
        drug_vocabulary_id == concept_vocabulary_id,
        drug_concept_code == concept_code
      ),
      copy = TRUE
    ) %>%
    filter(
      (
        grepl("^CURATED", dataset_id) &
          is_experienced == TRUE &
          optimized_experienced == TRUE
      ) |
        !grepl("^CURATED", dataset_id)
    ) %>%
    left_join(
      get_visit_occurrence(con, schema_name) %>%
        distinct(person_id, visit_occurrence_id, visit_start_date),
      by = join_by(person_id, visit_occurrence_id)
    ) %>%
    mutate(drug_exposure_start_date = coalesce(drug_exposure_start_date, visit_start_date)) %>%
    {`if` (STUDY_PERIOD_ONLY,
           filter(., drug_exposure_start_date >= study_start_date &
                    drug_exposure_start_date <= study_end_date),
           .)} %>%
    filter(!is.na(drug_exposure_start_date)) %>%
    select(
      person_id,
      dataset_id,
      drug_exposure_start_date,
      drug_concept_name,
      drug_concept_code,
      drug_vocabulary_id,
      drug_source_concept_code,
      drug_source_concept_name,
      drug_source_value,
      days_supply,
      quantity,
      is_experienced,
      optimized_experienced
    )
}

get_measurement <- function(con, schema_name, snowflake, testing) {

  tbl(con, in_schema(schema_name, if_else(snowflake, "MEASUREMENT", "measurement"))) %>%
    {`if` (snowflake,
           rename_with(., ~ tolower(.)),
           .)} %>%
    {
      `if` (testing,
            head(., n = 100),
            .)
    } %>%
    mutate(
      measurement_concept_code = if_else(
        measurement_vocabulary_id == "ICD10CM",
        str_remove_all(measurement_concept_code, "\\."),
        measurement_concept_code
      )
    ) %>%
    left_join(
      get_study_optimized_concept_attribute(con, schema_name) %>%
        distinct(optimized_experienced, concept_code, concept_vocabulary_id),
      join_by(
        measurement_vocabulary_id == concept_vocabulary_id,
        measurement_concept_code == concept_code
      ),
      copy = TRUE
    ) %>%
    filter(
      (
        grepl("^CURATED", dataset_id) &
          is_experienced == TRUE &
          optimized_experienced == TRUE
      ) |
        !grepl("^CURATED", dataset_id)
    ) %>%
    left_join(
      get_visit_occurrence(con, schema_name) %>%
        distinct(person_id, visit_occurrence_id, visit_start_date),
      by = join_by(person_id, visit_occurrence_id)
    ) %>%
    mutate(measurement_date = coalesce(measurement_date, visit_start_date)) %>%
    {`if` (STUDY_PERIOD_ONLY,
           filter(., measurement_date >= study_start_date &
                    measurement_date <= study_end_date),
           .)} %>%
    distinct(
      person_id,
      measurement_date,
      measurement_vocabulary_id,
      measurement_concept_code,
      measurement_concept_name,
      value_as_number,
      unit_concept_name,
      measurement_source_concept_code,
      dataset_id,
      is_experienced,
      optimized_experienced
    ) %>%
    collect() %>%
    # previously left join, unnecessary when
    # only looking for blood eosinophil and IgE levels
    inner_join(
      insights_code_list,
      join_by(
        measurement_vocabulary_id == vocabulary_id,
        measurement_concept_code == concept_code
      ),
      relationship = "many-to-many"
    )
}

get_patient_years <-
  function(schema_name = "trust_dataset_a6",
           table = c(
             "observation",
             "condition_occurrence",
             "device_exposure",
             "drug_exposure",
             "measurement",
             "observation",
             "procedure_occurrence"
           ),
           start_date_column = "observation_date",
           end_date_column = as.character(NA),
           vocabulary_id_column = "observation_vocabulary_id",
           snowflake,
           testing = FALSE) {
    table <- match.arg(table)

    tictoc::tic(glue::glue(schema_name, ": ", table))

    start_date_column <- sym(start_date_column)
    vocabulary_id_column <- sym(vocabulary_id_column)

    if (is.na(end_date_column)) {
      output <- tbl(con, in_schema(schema_name, if_else(snowflake, toupper(table), table))) %>%
        rename_with(tolower) %>%
        filter(!is.na(!!start_date_column)) %>%
        {
          `if` (testing,
                head(., n = 100),
                .)
        } %>%
        mutate(
          data_element = case_when
          (
            grepl("CLAIM", dataset_id) &
              (is.na(visit_occurrence_id)) &
              !!vocabulary_id_column == "NDC" ~ "Pharmacy Claim",
            grepl("CLAIM", dataset_id) ~ "Medical Claim",
            grepl("STRUCTURED", dataset_id) ~ "EHR Structured",
            grepl("CURATED", dataset_id) ~ "EHR Unstructured",
            TRUE ~ as.character(NA)
          )
        ) %>%
        distinct(person_id, !!start_date_column,
                 data_element) %>%
        collect() %>%
        mutate(year = as.integer(str_sub(!!start_date_column, 1, 4))) %>%
        filter(!is.na(year)) %>%
        distinct(person_id, year, data_element)


    } else {

      end_date_column <- sym(end_date_column)

      output <- rbind(
        tbl(con, in_schema(schema_name, if_else(snowflake, toupper(table), table))) %>%
          rename_with(tolower) %>%
          filter(!is.na(!!start_date_column)) %>%
          {
            `if` (testing,
                  head(., n = 100),
                  .)
          } %>%
          mutate(
            data_element = case_when
            (
              grepl("CLAIM", dataset_id) &
                (is.na(visit_occurrence_id)) &
                !!vocabulary_id_column == "NDC" ~ "Pharmacy Claim",
              grepl("CLAIM", dataset_id) ~ "Medical Claim",
              grepl("STRUCTURED", dataset_id) ~ "EHR Structured",
              grepl("CURATED", dataset_id) ~ "EHR Unstructured",
              TRUE ~ as.character(NA)
            )
          ) %>%
          distinct(person_id, !!start_date_column, data_element) %>%
          collect() %>%
          mutate(year = as.integer(str_sub(!!start_date_column, 1, 4
          ))) %>%
          filter(!is.na(year)) %>%
          distinct(person_id, year, data_element),
        tbl(con, in_schema(schema_name, if_else(snowflake, toupper(table), table))) %>%
          rename_with(tolower) %>%
          filter(!is.na(!!end_date_column)) %>%
          {
            `if` (testing,
                  head(., n = 100),
                  .)
          } %>%
          mutate(
            data_element = case_when
            (
              grepl("CLAIM", dataset_id) &
                (is.na(visit_occurrence_id)) &
                !!vocabulary_id_column == "NDC" ~ "Pharmacy Claim",
              grepl("CLAIM", dataset_id) ~ "Medical Claim",
              grepl("STRUCTURED", dataset_id) ~ "EHR Structured",
              grepl("CURATED", dataset_id) ~ "EHR Unstructured",
              TRUE ~ as.character(NA)
            )
          ) %>%
          distinct(person_id, !!end_date_column,
                   data_element) %>%
          collect() %>%
          mutate(year = as.integer(str_sub(
            !!end_date_column, 1, 4
          ))) %>%
          filter(!is.na(year)) %>%
          distinct(person_id, year, data_element)
      )

    }

    tictoc::toc()
    return(output)

  }

make_has <- function(df, has_col, prefix = c("has", "baseline_has"), n = 1, print_for_dictionary = FALSE) {
  prefix <- match.arg(prefix)
  has_col <- enquo(has_col)
  v <- df %>% distinct(!!has_col) %>% pull()

  for (i in seq_along(v)) {
    varname = make_col_name(v[i], prefix = prefix)
    if (print_for_dictionary) {
      cli::cli_text("{.val {varname}} = {.val {stringr::str_to_sentence(v[i])}}")
    } else {
      cli::cli_text("{.val {i}}: {.field {v[i]}}")
    }

    # Look for any occurrence of the comorbidity per patient
    df <- df %>%
      group_by(person_id) %>%
      # mutate(!!sym(varname) := any(!!has_col == v[i])) %>%
      mutate(!!sym(varname) := sum(!!has_col == v[i]) >= n) %>%
      ungroup()
  }
  return(df)
}

make_col_name <- function(s, prefix = c("has", "baseline_has")) {
  # if(prefix != "") prefix <- match.arg(prefix)
  gsub("[[:punct:]]+|[[:space:]]+","_", paste(prefix, tolower(s), sep="_"))
  # gsub("_$","", gsub("[[:punct:]]+|[[:space:]]+","_", paste(prefix, tolower(s), sep="_")))
}


get_exacerbation_secondary <- function(con,
                                       db_name,
                                       schema_name,
                                       exacerbation_type = c("ED", "Outpatient", "Inpatient", "ED / Urgent care"),
                                       dataset_id_filter = "CLAIMS") {
  visit_concept_ids <- verantos::exacerbation_dictionary %>%
    dplyr::filter(name == exacerbation_type) %>%
    dplyr::pull(visit_concept_id) %>%
    base::unlist()

  curated = if_else(grepl("CURATED", dataset_id_filter), TRUE, FALSE)

  exacerbation <- get_snowflake_table(
    con,
    schema_name,
    db_name = db_name,
    table = "condition_occurrence",
    concept_filter = "Asthma exacerbation",
    dataset_id_filter = dataset_id_filter
  ) %>%
    dplyr::filter(visit_concept_id %in% visit_concept_ids) %>%
    {
      if (curated) {
        filter(
          .,
          (
            grepl("CURATED", dataset_id_filter) &
              is_experienced == TRUE & is_current == TRUE
          ) |
            !grepl("CURATED", dataset_id_filter)
        )
      } else {
        .
      }
    } %>%
    dplyr::distinct() %>%
    dplyr::collect() %>%
    dplyr::mutate(exacerbation_date = condition_start_date) %>%
    dplyr::select(dplyr::any_of(
      base::c(
        "person_id",
        "dataset_id",
        "exacerbation_date",
        "visit_occurrence_id",
        "visit_concept_name",
        "visit_concept_id",
        "visit_type_concept_name"
      )
    ))

  return(exacerbation)

}

process_patient_events <- function(patient_data, gap = 14) {
  # Sort data by event_date
  patient_data <- patient_data %>% arrange(exacerbation_date)

  # Initialize an empty result set
  result <- data.frame()

  # Initialize remaining data
  remaining_data <- patient_data

  while (nrow(remaining_data) > 0) {
    # Get the first event in the remaining data
    anchor_event <- remaining_data[1, ]

    # Define the static 14-day window starting from the current anchor event
    window_end <- anchor_event$exacerbation_date + (gap - 1)

    # Filter events within the current 14-day window
    events_in_window <- remaining_data %>%
      filter(exacerbation_date >= anchor_event$exacerbation_date & exacerbation_date <= window_end)

    events_in_window

    # Select the event with the highest factor in this window
    top_event <- events_in_window %>%
      arrange(exacerbation_type) %>%
      # cast all dates in group the min date as this is the date that needs to
      # be used, "the first in the window"
      mutate(exacerbation_date = min(exacerbation_date)) %>%
      slice_min(order_by = tibble(exacerbation_type, exacerbation_date), with_ties = FALSE)

    top_event
    # Add the top event to the result set
    result <- bind_rows(result, top_event)

    # Remove events already processed (within the current 14-day window)
    remaining_data <- remaining_data %>%
      filter(exacerbation_date > window_end)
  }

  return(result)
}

calculate_ics_dose_categories <- function(df) {
  df %>%
    mutate(
      average_daily_dose_mcg = ((quantity * mcg_per_quantity) / days_supply),
      average_daily_dose_category = dplyr::case_when(
        concept_group_name == "ICS - beclomethasone" & average_daily_dose_mcg <= 200 ~ "Low",
        concept_group_name == "ICS - beclomethasone" & average_daily_dose_mcg > 200 & average_daily_dose_mcg <= 400 ~ "Medium",
        concept_group_name == "ICS - beclomethasone" & average_daily_dose_mcg > 400 ~ "High",
        concept_group_name %in% c("ICS - budesonide", "ICS/LABA - budesonide/formoterol") & average_daily_dose_mcg <= 400 ~ "Low",
        concept_group_name %in% c("ICS - budesonide", "ICS/LABA - budesonide/formoterol") & average_daily_dose_mcg > 400 & average_daily_dose_mcg <= 800 ~ "Medium",
        concept_group_name %in% c("ICS - budesonide", "ICS/LABA - budesonide/formoterol") & average_daily_dose_mcg > 800 ~ "High",
        concept_group_name == "ICS - ciclesonide" & average_daily_dose_mcg <= 160 ~ "Low",
        concept_group_name == "ICS - ciclesonide" & average_daily_dose_mcg > 160 & average_daily_dose_mcg <= 320 ~ "Medium",
        concept_group_name == "ICS - ciclesonide" & average_daily_dose_mcg > 320 ~ "High",
        concept_group_name == "ICS - flunisolide" & mcg_per_puff == 250 & average_daily_dose_mcg <= 1000 ~ "Low",
        concept_group_name == "ICS - flunisolide" & mcg_per_puff == 250 & average_daily_dose_mcg > 1000 & average_daily_dose_mcg <= 2000 ~ "Medium",
        concept_group_name == "ICS - flunisolide" & mcg_per_puff == 250 & average_daily_dose_mcg > 2000 ~ "High",
        concept_group_name == "ICS - flunisolide" & mcg_per_puff == 80 & average_daily_dose_mcg <= 320 ~ "Low",
        concept_group_name == "ICS - flunisolide" & mcg_per_puff == 80 & average_daily_dose_mcg > 320 & average_daily_dose_mcg <= 640 ~ "Medium",
        concept_group_name == "ICS - flunisolide" & mcg_per_puff == 80 & average_daily_dose_mcg > 640 ~ "High",
        concept_group_name %in% c("ICS - fluticasone furoate", "ICS/LABA - fluticasone furoate/vilanterol", "ICS/LAMA/LABA - fluticasone furoate/umeclidinium/vilanterol") &
          average_daily_dose_mcg <= 100 ~ "Medium",
        concept_group_name %in% c("ICS - fluticasone furoate","ICS/LABA - fluticasone furoate/vilanterol","ICS/LAMA/LABA - fluticasone furoate/umeclidinium/vilanterol") &
          average_daily_dose_mcg > 100 ~ "High",
        concept_group_name %in% c("ICS - fluticasone propionate","ICS/LABA - fluticasone propionate/salmeterol") & average_daily_dose_mcg <= 250 ~ "Low",
        concept_group_name %in% c("ICS - fluticasone propionate","ICS/LABA - fluticasone propionate/salmeterol") & average_daily_dose_mcg > 250 & average_daily_dose_mcg <= 500 ~ "Medium",
        concept_group_name %in% c("ICS - fluticasone propionate","ICS/LABA - fluticasone propionate/salmeterol") & average_daily_dose_mcg > 500 ~ "High",
        concept_group_name %in% c("ICS - mometasone metered dose inhaler","ICS/LABA - mometasone/formoterol metered dose inhaler") & average_daily_dose_mcg <= 400 ~ "Medium",
        concept_group_name %in% c("ICS - mometasone metered dose inhaler", "ICS/LABA - mometasone/formoterol metered dose inhaler") & average_daily_dose_mcg > 400 ~ "High",
        concept_group_name %in% c("ICS - mometasone dry powder inhaler") & average_daily_dose_mcg <= 200 ~ "Low",
        concept_group_name %in% c("ICS - mometasone dry powder inhaler") & average_daily_dose_mcg > 200 & average_daily_dose_mcg <= 400 ~ "Medium",  # Fixed: was 00 instead of 400
        concept_group_name %in% c("ICS - mometasone dry powder inhaler") & average_daily_dose_mcg > 400 ~ "High",
        TRUE ~ as.character(NA)
      )
    )

}

optum_mapping <- c(
  "person_id"                = "ptid",
  "days_supply"              = "days_sup",
  "drug_exposure_start_date" = "fill_dt",
  "drug_concept_code"        = "ndc",
  "condition_occurrence_id"  = "clmid",
  "condition_occurrence_id"  = "encid",
  "condition_start_date"     = "diag_date",
  "condition_start_date"     = "fst_dt",
  "condition_concept_code"   = "diag",
  "condition_concept_code"   = "diagnosis_cd",
  "condition_vocabulary_id"  = "diagnosis_cd_type",
  "condition_vocabulary_id"  = "icd_flag",
  "drug_exposure_start_date" = "rxdate",
  "quantity"                 = "quantity_of_dose",
  "year_of_birth"            = "birth_yr"
)

# Helper function to check if table exists and has data
table_exists_and_has_data <- function(con, db_name, schema_name,
                                      table_name) {

  tables <- dplyr::tbl(con, dbplyr::in_catalog(db_name, "INFORMATION_SCHEMA", "TABLES"))
  res <- tables %>%
    dplyr::filter(TABLE_TYPE == "BASE TABLE") %>%
    dplyr::filter(TABLE_SCHEMA == toupper(schema_name)) %>%
    dplyr::filter(TABLE_NAME == toupper(table_name)) %>%
    dplyr::select(ROW_COUNT) %>%
    dplyr::collect()

  return (nrow(res) > 0 && res$ROW_COUNT > 0)
}


get_encounter_count <- function(schema_name, testing = TRUE){

  cli::cli_h2("Capturing encounter counts: {.var {schema_name}}")

  tbl(
    con,
    in_catalog(db_name, schema_name, "VISIT_OCCURRENCE")
  ) %>%
    rename_with(tolower) %>%
    {
      if(testing) head(., 100) else .
    } %>%
    mutate(provider_id = coalesce(provider_id,"0"),
           visit_start_date = coalesce(visit_start_date, visit_end_date),
           visit_end_date = coalesce(visit_end_date, visit_start_date)) %>%
    select(visit_occurrence_id, person_id, provider_id, visit_start_date, visit_end_date) %>%
    collect() %>%
    # engineering is capturing the unique_id via a coalesce, which ...
    mutate(unique_id = paste(person_id, provider_id, visit_start_date, visit_end_date, sep = "")) %>%
    distinct(unique_id) %>%
    count() %>%
    pull(n)

  cli::cli_alert_success("Encounter counts captured")

}

# tables_list <- c(
#   "visit_occurrence",
#   "condition_occurrence",
#   "drug_exposure",
#   "procedure_occurrence",
#   "device_exposure",
#   "measurement",
#   "observation",
#   "death",
#   "care_site",
#   "payer_plan_period",
#   # optum data model
#   "enc",
#
#   "clm_mbr_dtl",
#   "diag",
# )

get_person_count <- function(schema_name, testing = TRUE){

  cli::cli_h2("Capturing patient count: {.var {schema_name}}")

  count <- tbl(
    con,
    in_catalog(db_name, schema_name, if(!grepl("OPTUM", schema_name)) "PERSON" else "PT")) %>%
    rename_with(tolower) %>%
    {
      if(testing) head(., test_count) else .
    } %>%
    distinct(across(any_of(c("person_id", "ptid")))) %>%
    collect() %>%
    nrow()

  cli::cli_alert_success("Patient counts captured")

  return(count)
}
