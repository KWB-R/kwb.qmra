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

# get_exposure_value_or_stop ---------------------------------------------------
get_exposure_value_or_stop <- function(config, name)
{
  # HARD CODED: "name" MUST be of type value!
  value <- config$exposure$value[config$exposure$name == name]

  if (is.na(value)) {
    stop(sprintf(get_stop_text("must_be_value"), name), call. = FALSE)
  }
  
  value
}

# id_columns_to_integer --------------------------------------------------------
id_columns_to_integer <- function(data)
{
  # Which ID columns require conversion?
  to_convert <- grepl("ID$", names(data)) & ! sapply(data, is.integer)
  
  data[to_convert] <- lapply(data[to_convert], as.integer)
  
  data
}

# number_of_exposures ----------------------------------------------------------

#' Helper function: gets number of exposures from config
#' @param config as retrieved by config_read() 
#' @return number of exposures
#' @keywords internal
number_of_exposures <- function(config)
{
  # HARD CODED: number_of_exposures MUST be of type value!      
  get_exposure_value_or_stop(config, "number_of_exposures")
}

# number_of_repeatings ---------------------------------------------------------

#' Helper function: gets number of repeatings from config
#' @param config as retrieved by config_read() 
#' @return number of repeatings (used for bayesian analysis)
#' @keywords internal
number_of_repeatings <- function(config)
{
  # HARD CODED: number_of_repeatings MUST be of type value!      
  get_exposure_value_or_stop(config, "number_of_repeatings")
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

