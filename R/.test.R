library(kwb.qmra)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(readxl)
library(tidyr)
library(EnvStats)

  #library(EnvStats)
  #
  # dr_model <- "expo"
  #
  # # exposed_days_per_year <- 1000
  # # EnvStats::rtri()
  # # rlnorm(n = exposed_days_per_year,meanlog = 5, sdlog = 0)
  # #
  # #
  # # water_inflow <- runif(exposed_days_per_year,
  # #                       min = 10,
  # #                       max = 100)

  ### Recalculate Wolfgangs Risk assessment for Braunschweig
  exposure_daysPerYear <- 300


  inflow_orgPerLitre <- mean(rlnorm(n = exposure_daysPerYear,
                                    meanlog = 17.26,
                                    sdlog = 1) * EnvStats::rtri(n = exposure_daysPerYear,
                                                      min = 0.1,
                                                      max = 1,
                                                      mode = 0.55) / 10 ^5 )


 risk_inf <-  calc_infection_risk(inflow_orgPerLitre = inflow_orgPerLitre,
                      exposure_daysPerYear = exposure_daysPerYear,
                      doseresponse_modelType = "dr.betapoisson",
                      alpha = 0.145,
                      N50 = 896)

 risk_health <- calc_health_risk(infectionRisk_perYear = risk_inf$events$infectionRisk_perYear)

 risk_inf
 risk_health

}


