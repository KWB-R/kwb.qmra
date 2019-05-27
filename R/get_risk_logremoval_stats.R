# get_risk_logremoval_stats ----------------------------------------------------
#' @keywords internal
get_risk_logremoval_stats <- function(data)
{
  data %>% 
    group_by_treatment_and_pathogen_group(lean = FALSE) %>% 
    summarise_logreduction()
}

# group_by_treatment_and_pathogen_group ----------------------------------------
#' @keywords internal
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
#' @keywords internal
summarise_logreduction <- function(data)
{
  dplyr::summarise( 
    data,
    min = min(.data$logreduction), 
    p05 = quantile(.data$logreduction, probs = 0.05),
    p25 =  quantile(.data$logreduction, probs = 0.25),
    mean = mean(.data$logreduction), 
    median = median(.data$logreduction), 
    p75 =  quantile(.data$logreduction, probs = 0.75),
    p95 = quantile(.data$logreduction, probs = 0.95),
    max = max(.data$logreduction)
  )
}

# get_risk_logremoval_stats_lean -----------------------------------------------

#' "Lean" version of get_risk_logremoval_stats()
#' @importFrom kwb.utils moveColumnsToFront
#' @keywords internal
get_risk_logremoval_stats_lean <- function(data, config)
{
  data %>% 
    group_by_treatment_and_pathogen_group(lean = TRUE) %>% 
    summarise_logreduction() %>% 
    add_scheme_name(config) %>%
    add_treatment_name(config) %>%
    kwb.utils::moveColumnsToFront(c(
      scheme_columns(), treatment_columns(), "PathogenGroup"
    ))
}
