# get_risk_logremoval_stats ----------------------------------------------------
get_risk_logremoval_stats <- function(data)
{
  data %>% 
    dplyr::group_by(
      .data$TreatmentSchemeID, 
      .data$TreatmentSchemeName, 
      .data$TreatmentID, 
      .data$TreatmentName, 
      .data$PathogenGroup
    ) %>% 
    dplyr::summarise( 
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
#' 
get_risk_logremoval_stats_lean <- function(data, config)
{
  data %>% 
    dplyr::group_by(
      .data$TreatmentSchemeID, 
      .data$TreatmentID, 
      .data$PathogenGroup
    ) %>% 
    dplyr::summarise( 
      min = min(.data$logreduction), 
      p05 = quantile(.data$logreduction, probs = 0.05),
      p25 =  quantile(.data$logreduction, probs = 0.25),
      mean = mean(.data$logreduction), 
      median = median(.data$logreduction), 
      p75 =  quantile(.data$logreduction, probs = 0.75),
      p95 = quantile(.data$logreduction, probs = 0.95),
      max = max(.data$logreduction)
    ) %>% 
    kwb.qmra:::add_scheme_name(config) %>%
    kwb.qmra:::add_treatment_name(config) %>%
    kwb.utils::moveColumnsToFront(c(
      scheme_columns(), treatment_columns(), "PathogenGroup"
    ))
}
