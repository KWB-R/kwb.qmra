## ------------------------------------------------------------------------
library(kwb.qmra)

## ------------------------------------------------------------------------
#### DEFINE DIRECTORY ################
confDir <- system.file("extdata/configs/dummy", package = "kwb.qmra")
confDir

## ---- message = FALSE, warnings = FALSE----------------------------------
#### LOAD ############################
config <- config_read(confDir) 

## ----echo = FALSE, message = FALSE, warnings = FALSE---------------------
simulated <- config$inflow[config$inflow$simulate == 1, 1:3]
knitr::kable(simulated,caption = "Simulated pathogens for QMRA (defined in: 'inflow.csv' with 
             simulated = 1):")

knitr::kable(config$inflow[config$inflow$PathogenName %in% simulated$PathogenName,
 c("PathogenID","PathogenName", "PathogenGroup", "type", "min", "max")], 
 caption = "Inflow concentrations (defined in: 'inflow.csv') for pathogens used for QMRA:")

knitr::kable(config$treatment$schemes,
 caption = "Treatment schemes (defined in: 'treatment_schemes.csv') used for QMRA:")

knitr::kable(config$treatment$processes[config$treatment$processes$TreatmentID %in% config$treatment$schemes$TreatmentID,
c("TreatmentID","TreatmentName", "TreatmentGroup", "PathogenGroup", "type", "min", "max")], 
 caption = "Treatment processes (defined in: 'treatment_proecesses.csv') and assumed log-reductions used for QMRA (from [WHO, 2011](http://apps.who.int/iris/bitstream/10665/44584/1/9789241548151_eng.pdf#page=162)):")


exposure <- list(number_of_repeatings = as.numeric(config$exposure[config$exposure$name == "number_of_repeatings","value"]),
                 number_of_exposures =  as.numeric(config$exposure[config$exposure$name == "number_of_exposures","value"]),
                 volume_perEvent = config$exposure[config$exposure$name == "volume_perEvent",]
                 )


knitr::kable(exposure$volume_perEvent[,c("name", "type", "min", "max", "mode")],
 caption = "Ingested volume per event (defined in: row 3 'volume_perEvent' in 'exposure.csv') (source: own assumption)):")


volume_per_event <- kwb.qmra::create_random_distribution(type = exposure$volume_perEvent$type, 
                                     number_of_repeatings = exposure$number_of_repeatings,
                                     number_of_events = exposure$number_of_exposures, 
                                     value = exposure$volume_perEvent$value, 
                                     min = exposure$volume_perEvent$min, 
                                     max = exposure$volume_perEvent$max, 
                                     mode = exposure$volume_perEvent$mode, 
                                     mean = exposure$volume_perEvent$mean,debug = FALSE)


knitr::kable(config$doseresponse[config$doseresponse$PathogenName %in% simulated$PathogenName,], 
              caption = "Dose-response models (defined in: 'doseresponse.csv') used for QMRA (from [QMRAwiki](http://qmrawiki.canr.msu.edu/index.php/Quantitative_Microbial_Risk_Assessment_(QMRA)_Wiki)):")

knitr::kable(config$health[config$health$PathogenName %in% simulated$PathogenName,], 
              caption = "Health parameters (defined in: 'health.csv') for simulated pathogens (from [WHO, 2011](http://apps.who.int/iris/bitstream/10665/44584/1/9789241548151_eng.pdf#page=132)):")



## ------------------------------------------------------------------------
risk <- kwb.qmra::simulate_risk(config)

## ------------------------------------------------------------------------
str(risk$input)

## ------------------------------------------------------------------------
str(risk$output)

## ---- fig.width = 12,  fig.cap="Simulated inflow concentrations", warning= FALSE----
kwb.qmra::plot_inflow(risk)

## ---- fig.width = 12, fig.cap="Simulated reductions in the treatment plant", warning= FALSE----
kwb.qmra::plot_reduction(risk)

## ---- fig.width = 12, fig.cap="Simulated effluent concentrations", warning= FALSE----
kwb.qmra::plot_effluent(risk)

## ---- fig.width = 12, fig.cap="Simulated dose per event", warning= FALSE----
kwb.qmra::plot_event_dose(risk)

## ---- fig.width = 12, fig.cap="Simulated ingested volume per event", warning= FALSE----
kwb.qmra::plot_event_volume(risk)

## ---- fig.width = 12, fig.cap="Simulated infection probability per event", warning= FALSE----
kwb.qmra::plot_event_infectionProb(risk)

## ---- fig.width = 12, fig.cap="Simulated illness probability per event", warning= FALSE----
kwb.qmra::plot_event_illnessProb(risk)

## ---- fig.width = 12, fig.cap="Simulated DALYs per event", warning= FALSE----
kwb.qmra::plot_event_dalys(risk)

## ---- fig.width = 12, fig.cap="Simulated total infection probability (for all events)", warning= FALSE----
kwb.qmra::plot_total_infectionProb(risk)

## ---- fig.width = 12, fig.cap="Simulated total illness probability (for all events", warning= FALSE----
kwb.qmra::plot_total_illnessProb(risk)

## ---- fig.width = 12, fig.cap="Simulated total DALYs (for all events)", warning= FALSE----
kwb.qmra::plot_total_dalys(risk)

## ---- echo = FALSE-------------------------------------------------------
knitr::kable(risk$output$total[risk$output$total$repeatID==1, ], 
             caption = "Total risk (for first repeat of random generation)")

## ----eval = FALSE--------------------------------------------------------
#  confDirs <- system.file("extdata/configs/", package = "kwb.qmra")
#  kwb.qmra::report_workflow(confDirs)
#  

## ----eval = FALSE--------------------------------------------------------
#  
#  

