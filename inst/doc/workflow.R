## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ------------------------------------------------------------------------
library(kwb.qmra)

## ------------------------------------------------------------------------
#### DEFINE DIRECTORY ################
confDir <- system.file("extdata/configs/dummy", package = "kwb.qmra")

#### LOAD ############################
config <- config_read(confDir) 

## ------------------------------------------------------------------------
risk <- simulate_risk(config)

## ----fig.width=10,fig.asp=1----------------------------------------------


plot_inflow(risk)

plot_reduction(risk)

plot_effluent(risk)

plot_event_volume(risk)

plot_doseresponse(risk)

### Exposure: effluent conc * volume #####
plot_event_exposure(risk)

#### Dose: based on exposure discrete dose is calculated by using rpois(), for 
#### details see: simulate_risk() function
plot_event_dose(risk)

#### RISK PER EVENT ######################
plot_event_infectionProb(risk)
plot_event_illnessProb(risk)
plot_event_dalys(risk)

#### RISK TOTAL ##########################
plot_total_infectionProb(risk)
plot_total_illnessProb(risk)
plot_total_dalys(risk)

## ----eval=FALSE----------------------------------------------------------
#  set.seed(seed = 1)
#  confDirs <- system.file("extdata/configs", package = "kwb.qmra")
#  report_workflow(confDirs = confDirs)

