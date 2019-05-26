#' Config: create dummy configuration
#' 
#' @param confDir directory to save the dummy configuration files (Default:  
#' system.file('extdata/config', package = 'kwb.qmra')")
#' @return writes dummy configuration in confDir 
#' @export
#' @importFrom utils write.csv 
#' 
config_write_dummy <- function(
  confDir = system.file("extdata/configs/dummy", package = "kwb.qmra")
)
{
  # Helper function to write a configuration to a CSV file
  write_config_csv <- function(x, filename) {
    write.csv(x, file = file.path(confDir, filename), row.names = FALSE)
  }
  
  cat(sprintf("Writing dummy configuration file in folder: %s", confDir))
  
  ### 1) Exposure exposure
  data.frame(
    name = c("number_of_repeatings", "number_of_exposures", "volume_perEvent"),
    type = c("value", "value", "triangle"),
    value = c(10, 365, NA),
    min = c(NA_real_, NA_real_, 0.5),
    max = c(NA_real_, NA_real_, 3),
    mode = c(NA_real_, NA_real_, 1.5),
    mean = c(NA_real_, NA_real_, 2),
    sd = c(NA_real_, NA_real_, NA_real_),
    meanlog = c(NA_real_, NA_real_, NA_real_),
    sdlog = c(NA_real_,NA_real_,NA_real_)
  ) %>%
    write_config_csv("exposure.csv")
  
  ### 2) Inflow 
  dr.db_download() %>% 
    dplyr::select(
      .data$PathogenID, 
      .data$PathogenName,
      .data$PathogenGroup
    ) %>%
    dplyr::mutate(
      simulate = ifelse(.data$PathogenID %in% c(3,32,36), 1, 0),
      type = "uniform", 
      value = NA_real_,
      min = 10, 
      max = 10000, 
      mode = NA_real_, 
      mean = NA_real_, 
      sd = NA_real_, 
      meanlog = NA_real_, 
      sdlog = NA_real_) %>% 
    write_config_csv("inflow.csv")
  
  ### 3) Treatment
  treatment <- who_getTreatment()
  
  ### 3.1) Processes 
  treatment$untidy %>% 
    dplyr::rename(
      min = .data$LogReduction_Minimum, 
      max = .data$LogReduction_Maximum
    ) %>%
    dplyr::mutate(
      type = "uniform", 
      value = NA_real_,
      mode = NA_real_, 
      mean = NA_real_, 
      sd = NA_real_, 
      meanlog = NA_real_, 
      sdlog = NA_real_
    ) %>% 
    dplyr::select(
      .data$TreatmentID, 
      .data$TreatmentName, 
      .data$TreatmentGroup, 
      .data$PathogenGroup,
      .data$type,
      .data$value,
      .data$min, 
      .data$max,
      .data$mode, 
      .data$mean, 
      .data$sd, 
      .data$meanlog, 
      .data$sdlog
    ) %>% 
    dplyr::arrange(.data$TreatmentID) %>% 
    write_config_csv("treatment_processes.csv")
  
  ### 3.2) Schemes
  schemes <- treatment$schemes
  data.frame(
    TreatmentSchemeID = c(rep(1, 2), rep(2, 3)),
    TreatmentSchemeName = c(
      rep("Berlin (BF + Slow sand)", 2),
      rep("Depth & surface filtration", 3)
    )
  ) %>% cbind(rbind(
    schemes[schemes$TreatmentID %in% c(9, 8), ],
    schemes[schemes$TreatmentID %in% c(1, 5, 15), ]
  )) %>%
    write_config_csv("treatment_schemes.csv")
  
  # 4) Dose-response config 
  dr.db_download() %>%
    dplyr::select(
      .data$PathogenID,
      .data$PathogenName,
      .data$PathogenGroup,
      .data$`Best fit model*`,
      .data$k,
      .data$alpha,
      .data$N50,
      .data$`Host type`,
      .data$`Dose units`,
      .data$Route,
      .data$Response,
      .data$Reference,
      .data$Link
    ) %>% 
    write_config_csv("doseresponse.csv")
  
  # 5) Health config
  dr.db_download() %>%
    dplyr::select(
      .data$PathogenID, 
      .data$PathogenName
    ) %>%
    dplyr::mutate(
      infection_to_illness = ifelse(
        .data$PathogenID == 3, 0.7, ifelse(
          .data$PathogenID == 32, 0.03, ifelse(
            .data$PathogenID == 36, 0.3, NA
          )
        )
      ),
      dalys_per_case = ifelse(
        .data$PathogenID == 3, 4.6 * 10^-3, ifelse(
          .data$PathogenID == 32, 1.4 * 10 ^ -2, ifelse(
            .data$PathogenID == 36, 1.5*10 ^ -3, NA
          )
        )
      )
    ) %>% 
    write_config_csv("health.csv")
}

#' Config: create configuration
#' 
#' @param config a configuration as retrieved by config_read()
#' @param confName name of configuration 
#' @param confDir directory to save the configuration files (Default:  
#' tempdir()")
#' @param zipFiles should also zipfile be created in folder confDir (default: TRUE) 
#' @return writes configuration in confDir subfolder defined in confName 
#' @export
#' @importFrom utils zip
#' 
config_write <- function(
  config, confName = "dummy", confDir = tempdir(), zipFiles = TRUE
)
{
  exportDir <- file.path(confDir, confName)
  
  dir.create(exportDir)
  
  # Helper function to write a config file
  write_config_csv <- function(x, filename) {
    write.csv(x, file = file.path(exportDir, filename))
  }
  
  write_config_csv(config$inflow, "inflow.csv")
  write_config_csv(config$treatment$processes, "treatment_processes.csv")
  write_config_csv(config$treatment$schemes, "treatment_schemes.csv")
  write_config_csv(config$health, "health.csv")
  write_config_csv(config$exposure, "exposure.csv")
  write_config_csv(config$doseresponse, "doseresponse.csv")
  
  if (zipFiles) zip(
    zipfile = file.path(confDir, confName),
    files = dir(exportDir, full.names = TRUE)
  )
}

#' Config: read configuration
#' 
#' @param confDir directory to read configuration files (Default:  
#' system.file('extdata/configs/dummy', package = 'kwb.qmra')")
#' @return stores configuration in R list structure 
#' @importFrom readr read_csv
#' @export
#' 
config_read <- function(
  confDir = system.file("extdata/configs/dummy", package = "kwb.qmra")
)
{
  # Helper function to read a config file
  read_config_csv <- function(filename) {
    readr::read_csv(file.path(confDir, filename))
  }
  
  list(
    exposure = read_config_csv("exposure.csv"),
    inflow = read_config_csv("inflow.csv"),
    treatment = list(
      processes = read_config_csv("treatment_processes.csv"), 
      schemes = read_config_csv("treatment_schemes.csv")
    ), 
    doseresponse = read_config_csv("doseresponse.csv"),
    health = read_config_csv("health.csv")
  )
}
