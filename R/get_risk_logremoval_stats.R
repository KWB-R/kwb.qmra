# get_risk_logremoval_stats ----------------------------------------------------

#' @keywords internal
#' @noRd
get_risk_logremoval_stats <- function(data)
{
  data %>% 
    group_by_treatment_and_pathogen_group(lean = FALSE) %>% 
    summarise_logreduction()
}

# group_by_treatment_and_pathogen_group ----------------------------------------
#' @keywords internal
#' @noRd
group_by_treatment_and_pathogen_group <- function(data, lean)
{
  if (lean) {
    dplyr::group_by(
      data,
      .data$TreatmentSchemeID, 
      .data$TreatmentID, 
      .data$PathogenGroup
    )
  } else {
    dplyr::group_by(
      data,
      .data$TreatmentSchemeID, 
      .data$TreatmentSchemeName, 
      .data$TreatmentID, 
      .data$TreatmentName, 
      .data$PathogenGroup
    )
  }
}

# summarise_logreduction -------------------------------------------------------
#' @importFrom stats quantile
#' @keywords internal
summarise_logreduction <- function(data)
{
  dplyr::summarise( 
    data,
    min = min(.data$logreduction), 
    p05 = stats::quantile(.data$logreduction, probs = 0.05),
    p25 =  stats::quantile(.data$logreduction, probs = 0.25),
    mean = mean(.data$logreduction), 
    median = median(.data$logreduction), 
    p75 =  stats::quantile(.data$logreduction, probs = 0.75),
    p95 = stats::quantile(.data$logreduction, probs = 0.95),
    max = max(.data$logreduction)
  )
}

# get_risk_logremoval_stats_lean -----------------------------------------------

#' "Lean" version of get_risk_logremoval_stats()
#' @param data_lean list element "events" (as returned after running 
#' kwb.qmra::simulate_risk(config, lean = TRUE))
#' @param config config as returned by kwb.qmra::config_read()
#' This function returns the same as get_risk_total_stats() but does not require
#' the "Name" columns. Instead, names are merged by ID from the metadata tables
#' given in "config".
#' @importFrom kwb.utils moveColumnsToFront
#' @keywords internal
#' @noRd
get_risk_logremoval_stats_lean <- function(data_lean, config)
{
  data_lean %>% 
    group_by_treatment_and_pathogen_group(lean = TRUE) %>% 
    summarise_logreduction() %>% 
    add_scheme_name(config) %>%
    add_treatment_name(config) %>%
    kwb.utils::moveColumnsToFront(c(
      scheme_columns(), treatment_columns(), "PathogenGroup"
    ))
}
