#' Backcalculate risk: infection (helper function)
#' @param infectionRisk_perDay no default. will be set by function backcalc_infectionRisk_perDay()
#' @param exposure_daysPerYear exposure days per year 
#' @param target_infectionRisk_perYear target infection risk per per year (default: 
#' 1/10000) 
#' @return absolute offset between "target_infectionRisk_perYear" and 
#' "infectionRisk_perYear"
#' @keywords internal

backcalcHelper_infectionRisk_perDay <- function(
  infectionRisk_perDay, 
  exposure_daysPerYear, 
  target_infectionRisk_perYear = 1/10000
)
{
  infectionRisk_perYear <- 1 - (1 - infectionRisk_perDay)^exposure_daysPerYear
  abs(target_infectionRisk_perYear - infectionRisk_perYear)
}

#' Backcalculate risk: infection (using optimise() function)
#' @description Based on exposure days per year and target infection risk per year 
#' the acceptable daily infection risk is backcalculated
#' @param target_infectionRisk_perYear target infection risk per per year (default: 
#' 1/10000) 
#' @param exposures_daysPerYear exposure days per year (default: 1 to 365) 
#' @return acceptable daily infection risk for given exposures per year and target
#' infection risk per year 
#' @export

backcalc_infectionRisk_perDay <- function(
  target_infectionRisk_perYear = 1/10000,
  exposures_daysPerYear = 1:365
)
{
  infectionRisk_perDay <- vector(mode = "integer")
  
  expLength <- length(exposures_daysPerYear)
  
  for (exp_ind in seq_len(expLength)) {
    
    infectionRisk_perDay[exp_ind] <- stats::optimise(
      f = backcalcHelper_infectionRisk_perDay, 
      interval = c(1E-15, 1E-02),  
      tol =  .Machine$double.eps,
      exposure_daysPerYear = exposures_daysPerYear[exp_ind],
      target_infectionRisk_perYear = target_infectionRisk_perYear
    )$minimum
  }
  
  data.frame(
    target_infectionRisk_perYear = rep(target_infectionRisk_perYear, expLength), 
    exposures_daysPerYear = exposures_daysPerYear, 
    infectionRisk_perDay = infectionRisk_perDay
  )
}

if (FALSE)
{
  tmp <- backcalc_infectionRisk_perDay()
  plot(tmp$exposures_daysPerYear, -tmp$infectionRisk_perDay)
}
