readRenviron(".env")
source(here::here("initialize/initialize.R"))

{

  options(scipen = 9999)

  db_name = "EXPORT"

  # removing pointer to registry schema in analysis
  # going to integrate in_catalog and point to EXPORT
  con = verantos::snowflake_connect(
    db_name = "ANALYSIS",
    db_warehouse = "DEV_WH",
    db_role = "DATA_DEV"
  )

  km = verantos::snowflake_connect(
    db_name = "KNOWLEDGE_MANAGEMENT",
    db_warehouse = "DEV_WH",
    db_role = "DATA_DEV",
    use_schema = "OMOP_METADATA"
  )
}
