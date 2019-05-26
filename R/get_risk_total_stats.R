# get_risk_total_stats ---------------------------------------------------------
get_risk_total_stats <- function(data)
{
  data %>% 
    gather_total_risk(lean = FALSE) %>% 
    group_by_treatment_and_pathogen(lean = FALSE) %>% 
    summarise_value()
}

# gather_total_risk ------------------------------------------------------------
gather_total_risk <- function(data, lean)
{
  if (lean) {
    tidyr::gather(
      data, 
      key = "key", 
      value = "value",  
      - .data$repeatID, 
      - .data$TreatmentSchemeID, 
      - .data$PathogenID
    )
  } else {
    tidyr::gather(
      data = data, 
      key = "key", 
      value = "value",  
      - .data$TreatmentSchemeID, 
      - .data$TreatmentSchemeName, 
      - .data$PathogenGroup, 
      - .data$repeatID, 
      .data$events, 
      - .data$PathogenID, 
      - .data$PathogenName
    )
  }
}

# group_by_treatment_and_pathogen ----------------------------------------------
group_by_treatment_and_pathogen <- function(data, lean)
{
  if (lean) {
    dplyr::group_by(
      data,
      .data$TreatmentSchemeID, 
      .data$PathogenID,
      .data$key
    )     
  } else {
    dplyr::group_by(
      data,
      .data$TreatmentSchemeID, 
      .data$TreatmentSchemeName, 
      .data$PathogenGroup, 
      .data$PathogenID, 
      .data$PathogenName, 
      .data$key
    )
  }
}

# summarise_value --------------------------------------------------------------
summarise_value <- function(data)
{
  dplyr::summarise(
    data,
    min = min(.data$value), 
    p05 = quantile(.data$value, probs = 0.05),
    p25 =  quantile(.data$value, probs = 0.25),
    mean = mean(.data$value), 
    median = median(.data$value), 
    p75 =  quantile(.data$value, probs = 0.75),
    p95 = quantile(.data$value, probs = 0.95),
    max = max(.data$value)
  )
}

# get_risk_total_stats_lean ----------------------------------------------------

#' Lean version of get_risk_total_stats()
#' 
#' This function returns the same as get_risk_total_stats() but does not require
#' the "Name" columns. Instead, names are merged by ID from the metadata tables
#' given in "config".
#' 
get_risk_total_stats_lean <- function(data_lean, config)
{
  data_lean %>% 
    gather_total_risk(lean = TRUE) %>%
    group_by_treatment_and_pathogen(lean = TRUE) %>% 
    summarise_value() %>%
    add_scheme_name(config) %>%
    add_pathogen_name(config) %>%
    dplyr::arrange(.data$TreatmentSchemeID, .data$PathogenGroup) %>%
    kwb.utils::moveColumnsToFront(c(
      scheme_columns(), pathogen_columns(), "key"
    ))
}
