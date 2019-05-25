# get_risk_total_stats ---------------------------------------------------------
get_risk_total_stats <- function(data)
{
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
  ) %>% 
    dplyr::group_by(
      .data$TreatmentSchemeID, 
      .data$TreatmentSchemeName, 
      .data$PathogenGroup, 
      .data$PathogenID, 
      .data$PathogenName, 
      key
    ) %>% 
    dplyr::summarise( 
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
