#' Risk calculation: infection
#' 
#' @param inflow_orgPerLitre concentration of microbiological parameter in
#'   inflow to water treatment plant (default: 10 Org/L)
#' @param  treatment_logRemoval reduction of microbiological concentration in
#'   water treatment plant (default: 5.8 log)
#' @param exposure_daysPerYear exposure days per year (default: 365)
#' @param doseresponse_modelType dose response model to be used: "dr.expo" for
#'   exponential or "dr.betapoisson" for beta-poisson model
#' @param  waterConsumption_LitrePerDay daily water consumption (default: 1
#'   L/day)
#' @param target_infectionRisk_perYear NOT IMPLEMENTED YET!!!! target infection
#'   risk per per year. Only used if one of the following input parameters (i.e.
#'   "inflow_orgPerLitre", "treatment_logRemoval", "exposure_daysPerYear",
#'   "waterConsumption_LitrePerDay") is not defined (default: 1/10000).
#' @param ... additional parameters used for dose response modelling. Depends on
#'   used dose-response model
#' @return list with input parameters and calculated infection risk
#' @export
#' @seealso \code{\link{dr.expo}} for exponential or
#'   \code{\link{dr.betapoisson}} for beta-poisson dose-response model

calc_infection_risk <- function(
  inflow_orgPerLitre = 10,
  treatment_logRemoval = 5.8, 
  exposure_daysPerYear = 365,
  doseresponse_modelType = "dr.expo",
  waterConsumption_LitrePerDay = 1,
  target_infectionRisk_perYear = 1/10000,
  ...
) 
{
  outflow_orgPerLitre <- inflow_orgPerLitre / (10 ^ treatment_logRemoval)
  
  exposure_orgPerDay <-  outflow_orgPerLitre * waterConsumption_LitrePerDay
  
  doseresponse_modelType <- get(doseresponse_modelType)
  
  doseresponseModel <- function(dose,...) {
    doseresponse_modelType(dose, ...)
  }
  
  lowExposure <- exposure_orgPerDay < 1
  
  infectionRisk_perDay <- rep(NA, length(exposure_orgPerDay))
  
  if (any(lowExposure)) {
    doseresponse <- doseresponseModel(dose = 1, ...)
    infectionRisk_perDay[lowExposure] <- exposure_orgPerDay[lowExposure] * 
      doseresponse$infectionProbability
  } 
  
  if (any(! lowExposure)) { 
    doseresponse <- doseresponseModel(dose = exposure_orgPerDay[! lowExposure], ...)
    infectionRisk_perDay[! lowExposure] <- doseresponse$infectionProbability
  }
  
  events <- data.frame(
    inflow_orgPerLitre = inflow_orgPerLitre, 
    treatment_logRemoval = treatment_logRemoval,
    outflow_orgPerLitre  = outflow_orgPerLitre, 
    exposure_orgPerDay = exposure_orgPerDay,
    infectionProbability = doseresponse$infectionProbability,
    infectionRisk_perDay = infectionRisk_perDay, 
    exposure_daysPerYear = exposure_daysPerYear,
    infectionRisk_perYear = 1 - prod(
      1 - rep(infectionRisk_perDay, exposure_daysPerYear)
    )
  )
  
  list(
    events = events, 
    doseresponse = doseresponse
  )
}

#' Risk calculation: health
#' 
#' @param infectionRisk_perYear as retrieved by
#'   calc_infection_risk()$events$infectionRisk_perYear or user defined input
#'   (default: 9.5 * 10 ^ - 4 infections/year)
#' @param infection_to_illness probability of illness given infection (default:
#'   0.7, i.e. 70 percent illness probability giving infection)
#' @param diseaseBurden_dalyPerCase disabled adjusted life years per case. Value
#'   depends on multiple parameters such as illness type (default: 1.5*10 ^ -3)
#' @param fraction_population fraction of population at risk of getting the
#'   illness (default: 1, i.e. 100 percent of population can possibly get the
#'   illness, worst-case assumption assuming no prior immunization!)
#' @param target_dalyPerYearPerPerson target disabled adjusted life years (DALY)
#'   per person per year (default: 1/1000000 DALY per per person per year, WHO
#'   standard)
#' @return data frame with input parameters and calculated health risk outputs
#' @export
#' @seealso \code{\link{calc_infection_risk}} for infection risk input

calc_health_risk <- function(
  infectionRisk_perYear = 9.5 * 10 ^ -4, 
  infection_to_illness = 0.7, 
  diseaseBurden_dalyPerCase = 1.5*10 ^ -3,
  fraction_population = 1, 
  target_dalyPerYearPerPerson = 1/1000000
) 
{
  if (! is.null(infectionRisk_perYear)) {
    
    illnessRisk_perYear <- infectionRisk_perYear * infection_to_illness
    
    data.frame(
      infectionRisk_perYear = infectionRisk_perYear, 
      infection_to_illness = infection_to_illness,
      illnessRisk_perYear = illnessRisk_perYear, 
      diseaseBurden_dalyPerCase = diseaseBurden_dalyPerCase,
      fraction_population = fraction_population, 
      healthRisk_dalyPerYearPerPerson = illnessRisk_perYear * 
        diseaseBurden_dalyPerCase * fraction_population
    )
    
  } else {
    
    illnessRisk_perYear <- target_dalyPerYearPerPerson * fraction_population * 
      diseaseBurden_dalyPerCase / target_dalyPerYearPerPerson 
    
    data.frame(
      infectionRisk_perYear = illnessRisk_perYear / infection_to_illness, 
      infection_to_illness = infection_to_illness,
      illnessRisk_perYear = illnessRisk_perYear, 
      diseaseBurden_dalyPerCase = diseaseBurden_dalyPerCase,
      fraction_population = fraction_population, 
      healthRisk_dalyPerYearPerPerson = target_dalyPerYearPerPerson
    )
  }
}
