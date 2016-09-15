#' Config: create dummy configuration
#' @param confDir directory to save the dummy configuration files (Default:  
#' system.file('extdata/config', package = 'kwb.qmra')")
#' @return writes dummy configuration in confDir 
#' @export
#' @importFrom utils write.csv 
config_write_dummy <- function(confDir = system.file("extdata/configs/dummy", 
                                package = "kwb.qmra")) {
  
  print(sprintf("Writing dummy configuration file in folder: %s", confDir))
  
  ### 1) Exposure exposure

  exposure <- data.frame(name = c("number_of_repeatings", "number_of_exposures", "volume_perEvent"),
                         type = c("value", "value", "triangle"),
                         value = c(10, 365, NA),
                         min = c(NA_real_, NA_real_, 0.5),
                         max = c(NA_real_, NA_real_, 3),
                         mode = c(NA_real_, NA_real_, 1.5),
                         mean = c(NA_real_, NA_real_, 2),
                         sd = c(NA_real_, NA_real_, NA_real_),
                         meanlog = c(NA_real_, NA_real_, NA_real_),
                         sdlog = c(NA_real_,NA_real_,NA_real_))
  write.csv(exposure, 
            file = file.path(confDir, "exposure.csv"),
            row.names = FALSE)
  ### 2) Inflow 
  dr.db_download() %>% 
    dplyr::select_(~PathogenID, 
                   ~PathogenName,
                   ~PathogenGroup) %>%
    dplyr::mutate(simulate = ifelse(PathogenID %in% c(3,32,36), 
                             1, 
                             0),
           type = "uniform", 
           value = NA_real_,
           min = 10, 
           max = 10000, 
           mode = NA_real_, 
           mean = NA_real_, 
           sd = NA_real_, 
           meanlog = NA_real_, 
           sdlog = NA_real_) %>% 
    write.csv(file = file.path(confDir, "inflow.csv"),
              row.names = FALSE)
  
  
  ### 3) Treatment
  treatment <- who_getTreatment()
  ### 3.1) Processes 
  
  treatment_processes <- treatment$untidy %>% 
    dplyr::rename_(min = ~LogReduction_Minimum, 
           max = ~LogReduction_Maximum) %>%
    dplyr::mutate(type = "uniform", 
           value = NA_real_,
           mode = NA_real_, 
           mean = NA_real_, 
           sd = NA_real_, 
           meanlog = NA_real_, 
           sdlog = NA_real_) %>% 
    dplyr::select_(~TreatmentID, 
                   ~TreatmentName, 
                   ~TreatmentGroup, 
                   ~PathogenGroup,
                   ~type,
                   ~value,
                   ~min, 
                   ~max,
                   ~mode, 
                   ~mean, 
                   ~sd, 
                   ~meanlog, 
                   ~sdlog) %>% 
    dplyr::arrange_(~TreatmentID) %>% 
    write.csv(file = file.path(confDir, "treatment_processes.csv"),
              row.names = FALSE)
  
  ### 3.2) Schemes
  
  treatment_schemes <- cbind(data.frame(TreatmentSchemeID = c(rep(1,2), rep(2,3)),
                                        TreatmentSchemeName = c(rep("Berlin (BF + Slow sand)",2),
                                                                rep("Depth & surface filtration",3))), 
                             rbind(treatment$schemes[treatment$schemes$TreatmentID %in% c(9,8),],
                                   treatment$schemes[treatment$schemes$TreatmentID %in% c(1,5,15),]))
  
  
  write.csv(treatment_schemes , 
            file = file.path(confDir, "treatment_schemes.csv"),
            row.names = FALSE)
  
  # 4) Dose-response config 
  dr.db_download() %>%  dplyr::select_(~PathogenID,
                                       ~PathogenName,
                                       ~PathogenGroup,
                                       ~`Best fit model*`,
                                       ~k,
                                       ~alpha,
                                       ~N50,
                                       ~`Host type`,
                                       ~`Dose units`,
                                       ~ Route,
                                       ~Response,
                                       ~Reference,
                                       ~Link) %>% 
              write.csv(file = file.path(confDir, "doseresponse.csv"),
            row.names = FALSE)
  
  # 5) Health config
  dr.db_download() %>%  dplyr::select_(~PathogenID,
                                       ~PathogenName) %>%
    dplyr::mutate(infection_to_illness = ifelse(PathogenID == 3, 
                                                            0.7, 
                                                            ifelse(PathogenID == 32,
                                                                   0.03,
                                                                   ifelse(PathogenID == 36, 
                                                                          0.3, 
                                                                          NA))),
                              dalys_per_case = ifelse(PathogenID == 3, 
                                                            4.6*10^-3, 
                                                            ifelse(PathogenID == 32,
                                                                   1.4*10 ^ -2,
                                                                   ifelse(PathogenID == 36, 
                                                                          1.5*10 ^ -3, 
                                                                          NA)))) %>% 
    write.csv(file = file.path(confDir, "health.csv"),
              row.names = FALSE)
}

#' Config: create configuration
#' @param config a configuration as retrieved by config_read()
#' @param confName name of configuration 
#' @param confDir directory to save the configuration files (Default:  
#' tempdir()")
#' @param zipFiles should also zipfile be created in folder confDir (default: TRUE) 
#' @return writes configuration in confDir subfolder defined in confName 
#' @export
#' @importFrom utils zip
config_write <- function(config,
                         confName = "dummy",
                         confDir = tempdir(),
                         zipFiles = TRUE) {
  
  exportDir <- file.path(confDir, confName)
  
  dir.create(exportDir)
  
  write.csv(config$inflow, file = file.path(exportDir,"inflow.csv"),)
  write.csv(config$treatment$processes, file = file.path(exportDir,"treatment_processes.csv"))
  write.csv(config$treatment$schemes, file = file.path(exportDir,"treatment_schemes.csv"))
  write.csv(config$health, file = file.path(exportDir,"health.csv"))
  write.csv(config$exposure,  file = file.path(exportDir,"exposure.csv"))
  write.csv(config$doseresponse,  file = file.path(exportDir,"doseresponse.csv"))
  
  if (zipFiles) {
    zip(zipfile = file.path(confDir, confName),
        files = dir(exportDir,
                    full.names = TRUE))}
}

#' Config: read configuration
#' @param confDir directory to read configuration files (Default:  
#' system.file('extdata/configs/dummy', package = 'kwb.qmra')")
#' @return stores configuration in R list structure 
#' @importFrom readr read_csv
#' @export
config_read <- function(confDir = system.file("extdata/configs/dummy", 
                                             package = "kwb.qmra")) {
  
  
  exposure <- readr::read_csv(file.path(confDir, "exposure.csv"))
  
  inflow <- readr::read_csv(file.path(confDir, "inflow.csv"))
  
  treatment_processes <- readr::read_csv(file.path(confDir, "treatment_processes.csv"))
  
  treatment_schemes <- readr::read_csv(file.path(confDir, "treatment_schemes.csv"))
  
  doseresponse <- readr::read_csv(file.path(confDir, "doseresponse.csv"))
  
  health <- readr::read_csv(file.path(confDir, "health.csv"))
  
  config <- list(exposure = exposure,
                 inflow = inflow,
                 treatment = list(processes = treatment_processes, 
                                  schemes = treatment_schemes), 
                 doseresponse = doseresponse,
                 health = health)
  
  return(config)
  
  
}
