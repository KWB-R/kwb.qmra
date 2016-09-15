library(kwb.qmra)
#library(ggplot2)

### Create configuration files 
if (FALSE) {
config_write_dummy()
config_write_dummy("C:/Users/mrustl/Documents/WC_Server/R_Development/trunk/RPackages/kwb.qmra/inst/extdata/configs/dummy")
}


################################################################################
#### 1) CONFIGURATION
################################################################################

confDirs <- dir("C:/Users/mrustl/Desktop/QMRA_configs",full.names = TRUE)


#### DEFINE DIRECTORY ################
configDir <- system.file("extdata/configs/dummy", package = "kwb.qmra")
config <- config_read(configDir)

config_write(config, 
             confName = "dummy1", 
             confDir = system.file("extdata/configs", package = "kwb.qmra"),
             zipFiles = FALSE)
#### LOAD ############################
config <- config_read(confDir = confDirs[2]) 

################################################################################
#### 2) SIMULATE RISK
################################################################################

knitr::knit(input = "C:/Users/mrustl/Documents/WC_Server/R_Development/trunk/RPackages/kwb.qmra/inst/extdata/report/workflow.Rmd",
            output = "C:/Users/mrustl/Documents/WC_Server/R_Development/trunk/RPackages/kwb.qmra/inst/extdata/report/workflow.md")

risk <- simulate_risk(config)
#inflow <- simulate_inflow(config)

################################################################################
#### 3) VISUALIZE
################################################################################


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

################################################################################
#### 4) Create report
################################################################################
set.seed(seed = 1)
report_workflow(confDirs = "C:/Users/mrustl/Desktop/QMRA_configs")

