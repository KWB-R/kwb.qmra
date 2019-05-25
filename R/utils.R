# add_pathogen_name ------------------------------------------------------------
add_pathogen_name <- function(data, config)
{
  pathogen_names <- config$doseresponse[, pathogen_columns()]
  dplyr::inner_join(data, pathogen_names, by = "PathogenID")
}

# add_scheme_name --------------------------------------------------------------
add_scheme_name <- function(data, config)
{
  treatment_names <- unique(config$treatment$schemes[, scheme_columns()])
  dplyr::inner_join(data, treatment_names, by = "TreatmentSchemeID")
}

# add_treatment_name -----------------------------------------------------------
add_treatment_name <- function(data, config)
{
  treatment_names <- unique(config$treatment$processes[, treatment_columns()])
  dplyr::inner_join(data, treatment_names, by = "TreatmentID")
}

# id_columns_to_integer --------------------------------------------------------
id_columns_to_integer <- function(data)
{
  # Which ID columns require conversion?
  to_convert <- grepl("ID$", names(data)) & ! sapply(data, is.integer)
  
  data[to_convert] <- lapply(data[to_convert], as.integer)
  
  data
}

# pathogen_columns -------------------------------------------------------------
pathogen_columns <- function()
{
  c("PathogenGroup", "PathogenID", "PathogenName")
}

# scheme_columns ---------------------------------------------------------------
scheme_columns <- function()
{
  c("TreatmentSchemeID", "TreatmentSchemeName")
}

# treatment_columns ------------------------------------------------------------
treatment_columns <- function()
{
  c("TreatmentID", "TreatmentName")
}
