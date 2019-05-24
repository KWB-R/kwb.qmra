#' Helper function: gets number of exposures from config
#' @param config as retrieved by config_read() 
#' @return number of exposures
#' @keywords internal
number_of_exposures <- function(config) {
  ### HARD CODED: number_of_exposures MUST be of type value!    
  exposures <- config$exposure$value[config$exposure$name == "number_of_exposures"]
  
  if (is.na(exposures)) {
    stop("number_of_exposures in configuration 'exposure' MUST be of 'type'='value' 
         and column 'value' has to be numeric") }
  
  return(exposures)
}


#' Helper function: gets number of repeatings from config
#' @param config as retrieved by config_read() 
#' @return number of repeatings (used for bayesian analysis)
#' @keywords internal
number_of_repeatings <- function(config) {
  ### HARD CODED: number_of_exposures MUST be of type value!    
  repeatings <- config$exposure$value[config$exposure$name == "number_of_repeatings"]
  
  if (is.na(repeatings)) {
    stop("number_of_repeatings in configuration 'exposure' MUST be of 'type'='value' 
         and column 'value' has to be numeric") }
  
  return(repeatings)
}

#' Simulate: inflow
#' @param config as retrieved by config_read() 
#' @param debug print debug information (default: TRUE)
#' @return list with parameters of user defined random distribution and 
#' corresponding values
#' @export

simulate_inflow <- function(config, debug = TRUE) {
  
  events <- number_of_exposures(config)
  repeatings <- number_of_repeatings(config)
  
  ### Only pathogens to be simulated
  inflow_simulate <- config$inflow[config$inflow$simulate == 1,]
  inflow_events <- data.frame()
  inflow_paras <- data.frame()
  for (patho_id in inflow_simulate$PathogenID) {
    
    inflow_tmp <- config$inflow[config$inflow$PathogenID == patho_id,] 
    if (debug) cat(sprintf("Simulated pathogen: %s\n", inflow_tmp$PathogenName))
    inflow_tmp_random <- generate_random_values(config = inflow_tmp,
                                                number_of_repeatings = repeatings,
                                                number_of_events = events,
                                                debug = debug)
    
    inflow_tmp_events <- cbind(inflow_tmp_random$events, 
                               inflow_tmp[,c("PathogenID", 
                                             "PathogenName", 
                                             "PathogenGroup")])
    
    inflow_tmp_paras <- cbind(inflow_tmp_random$paras, 
                              inflow_tmp[,c("PathogenID", 
                                            "PathogenName", 
                                            "PathogenGroup")])
    
    if (patho_id == inflow_simulate$PathogenID[1]) {
      inflow_events <- inflow_tmp_events
      inflow_paras <- inflow_tmp_paras
    } else {
      inflow_events <- rbind(inflow_events, inflow_tmp_events)
      inflow_paras <- plyr::rbind.fill(inflow_paras, inflow_tmp_paras)
    }
    
  }
  names(inflow_events)[names(inflow_events) == "values"] <- "inflow"
  return(inflow = list(events = inflow_events , 
                       paras = inflow_paras))
}

#' Simulate: treatment
#' @param config as retrieved by config_read() 
#' @param wide if TRUE results will be converted to wide format (default: 
#' FALSE)
#' @param debug print debug information (default: TRUE)
#' @return list with parameters of user defined random distribution and 
#' corresponding values
#' @importFrom tidyr spread 
#' @export

simulate_treatment <- function(config, wide = FALSE, debug = TRUE, minimal = FALSE)
{
  events <- number_of_exposures(config)
  repeatings <- number_of_repeatings(config)
  
  pathoGroups <- unique(config$inflow$PathogenGroup[config$inflow$simulate == 1])
  treatmentIDs <- unique(config$treatment$schemes$TreatmentID)
  
  treatment_required <- config$treatment$processes$TreatmentID %in% treatmentIDs
  pathogen_required <- config$treatment$processes$PathogenGroup %in% pathoGroups
  
  condition <- treatment_required & pathogen_required

  treatment_processes_simulate <- config$treatment$processes[condition, ]
  
  treatment_events <- data.frame()
  treatment_paras <- data.frame()
  
  for (i in seq_len(nrow(treatment_processes_simulate))) {
    
    treatment_tmp <- treatment_processes_simulate[i, ]
    
    if (debug) cat(sprintf(
      "Simulated treatment: %s for %s\n", 
      treatment_tmp$TreatmentName, 
      treatment_tmp$PathogenGroup
    ))
    
    treatment_tmp_random <- generate_random_values(
      config = treatment_tmp, 
      number_of_repeatings = repeatings,
      number_of_events = events,
      debug = debug
    )
    
    treatment_tmp_events <- cbind(
      treatment_tmp_random$events, 
      treatment_tmp[, c("TreatmentID", "PathogenGroup")],
      row.names = NULL
    )
    
    treatment_tmp_paras <- cbind(
      treatment_tmp_random$paras, 
      treatment_tmp[, c("TreatmentID", "PathogenGroup")],
      row.names = NULL
    )
    
    if (i == 1) {
      
      treatment_events <- treatment_tmp_events
      treatment_paras <- treatment_tmp_paras
      
    } else {
      
      treatment_events <- rbind(treatment_events, treatment_tmp_events)
      treatment_paras <- plyr::rbind.fill(treatment_paras, treatment_tmp_paras)
    }
    
  } # next i

  is_value_column <- names(treatment_events) == "values"
  names(treatment_events)[is_value_column] <- "logreduction"

  treatment_events <- dplyr::left_join(treatment_events, config$treatment$schemes)
  
  # Return only the result that is required by the web app if "minimal" is TRUE
  if (minimal) {
    
    return(list(events_long = treatment_events))
  } 
  
  if (wide) {
    
    treatment_events_wide <- tidyr::spread(
      data = treatment_events,
      key = .data$TreatmentID,
      value = .data$logreduction
    )
    
    schemes_events_wide <- get_scheme_events_wide(config, treatment_events_wide)
  }
  
  # Create and return further results only if "minimal" is FALSE
  lookup_treatmentNames <- config$treatment$processes[, c("TreatmentID", "TreatmentName")] %>%
    dplyr::group_by(.data$TreatmentID) %>% 
    dplyr::slice(1)
  
  treatment_paras <- dplyr::left_join(treatment_paras, lookup_treatmentNames)
  
  if (wide) list(
    
    events_long = treatment_events,
    events_wide = treatment_events_wide,
    schemes_events_wide = schemes_events_wide,
    schemes = config$treatment$schemes,
    paras = treatment_paras
    
  ) else list(
    
    events_long = treatment_events,
    schemes = config$treatment$schemes,
    paras = treatment_paras
  )
}

# get_scheme_events_wide -------------------------------------------------------
get_scheme_events_wide <- function(config, treatment_events_wide)
{
  schemes <- config$treatment$schemes
  
  schemeIDs <- unique(schemes$TreatmentSchemeID)
  
  schemes_events_wide <- data.frame()

  for (schemeID in schemeIDs) {
    
    is_scheme <- schemes$TreatmentSchemeID == schemeID
    
    scheme_treatmentIDs <- unique(schemes$TreatmentID[is_scheme])
    
    event_row_sums <- rowSums(
      treatment_events_wide[, as.character(scheme_treatmentIDs), drop = FALSE]
    )

    if (schemeID == schemeIDs[1]) {
      
      schemes_events_wide <- cbind(
        treatment_events_wide[, c("eventID", "PathogenGroup")],
        event_row_sums,
        row.names = NULL
      )
      
    } else {
      
      schemes_events_wide <- cbind(
        schemes_events_wide,
        event_row_sums,
        row.names = NULL
      )
    }
  }
  
  stats::setNames(schemes_events_wide, c(
    "eventID", "PathogenGroup", paste0("scheme_", schemeIDs)
  ))
}

#' Helper function: poisson distribution based on exposure per event 
#' @param exposure_perEvent exposed organisms per event
#' @return dose per event based on poisson process
#' @export
#' @importFrom stats rpois
poisson_dose <- function(exposure_perEvent) {
  sapply(exposure_perEvent, 
         FUN = function(exposure) {rpois(n = 1, 
                                         lambda = exposure)})
}


#' Simulate: exposure
#' @param config as retrieved by config_read() 
#' @param debug print debug information (default: TRUE)
#' @return list with parameters of user defined exposure scenario (number of events 
#' and volumes per Event)
#' @export

simulate_exposure <- function(config, debug = TRUE) {
  
  events <- number_of_exposures(config)
  repeatings <- number_of_repeatings(config)
  
  volume_perEvent <- config$exposure[config$exposure$name == "volume_perEvent",]
  
  if (debug) {
    cat(sprintf("Simulated exposure: volume per event\n")) }
  volumes <-  generate_random_values(config = volume_perEvent, 
                                     number_of_repeatings = repeatings,
                                     number_of_events = events,
                                     debug = debug)
  
  colnames(volumes$events)[names(volumes$events) == "values"] <- "volume_perEvent" 
  
  list(
    #number_of_events = events,
    volumes = volumes["events"]
  )
}


#' Simulate: risk
#' @param config as retrieved by config_read() 
#' @param usePoisson should a poisson proccess (see function dose_perEvent()) be
#' used to calculate the dose_perEvent (TRUE) or just the exposure_perEvent 
#' column (FALSE), (default: TRUE)
#' @param debug print debug information (default: TRUE)
#' @return list with parameters of user defined random distribution and 
#' corresponding values
#' @importFrom stats median
#' @import dplyr
#' @export

simulate_risk <- function(config, usePoisson = TRUE, debug = TRUE, minimal = FALSE)
{
  #kwb.utils::assignPackageObjects("kwb.qmra")
  cat("### STEP 0: BASIC CONFIGURATION #############################################\n\n")
  
  simulated_pathogens <- config$inflow$PathogenName[config$inflow$simulate == 1]
  
  cat(sprintf(
    "Simulated %d pathogen(s): %s\n", 
    length(simulated_pathogens),
    paste(simulated_pathogens, collapse = ", ")
  ))
  cat(sprintf("Number of random distribution repeatings: %d\n", number_of_repeatings(config)))
  cat(sprintf("Number of exposure events: %d\n\n", number_of_exposures(config)))
  
  cat("### STEP 1: INFLOW ##########################################################\n\n")
  inflow <- simulate_inflow(config, debug)
  
  cat("\n### STEP 2: TREATMENT SCHEMES #############################################\n\n")
  treatment <- simulate_treatment(config, debug = debug, minimal = minimal)
  
  tbl_reduction <- treatment$events_long %>%  
    dplyr::group_by(.data$TreatmentSchemeID,
                    .data$TreatmentSchemeName,
                    .data$PathogenGroup, 
                    .data$eventID,
                    .data$repeatID) %>% 
    dplyr::summarise(logreduction = sum(.data$logreduction))
  
  
  
  cat("\n### STEP 3: Exposure ######################################################\n\n")
  exposure <- simulate_exposure(config, debug)
  
  tbl_risk <-  dplyr::right_join(tbl_reduction, 
                                 inflow$events) %>% 
    dplyr::mutate(effluent = 10 ^ (log10(.data$inflow) - .data$logreduction)) %>% 
    dplyr::left_join(exposure$volumes$events) %>% 
    dplyr::mutate(exposure_perEvent = .data$effluent * .data$volume_perEvent)
  
  
  if (usePoisson) { 
    tbl_risk$dose_perEvent <- poisson_dose(tbl_risk$exposure_perEvent) 
  } else {
    tbl_risk$dose_perEvent <- tbl_risk$exposure_perEvent
  }
  
  cat("\n### STEP 4: DOSE RESPONSE #################################################\n\n")
  
  simulated_pathogenIDs <- config$inflow$PathogenID[config$inflow$simulate == 1]
  
  
  indices <- config$doseresponse$PathogenID %in%  simulated_pathogenIDs
  
  paras <- config$doseresponse[config$doseresponse$PathogenID %in% simulated_pathogenIDs,]
  if(debug) print(paras)
  doseresponse <- list(response = dr.db_model(dr.db = config$doseresponse[indices,]),
                       paras = paras)
  
  tbl_risk$infectionProb_per_event <- NA
  
  for (pathogenID in unique(tbl_risk$PathogenID)) {
    
    
    cond <- tbl_risk$PathogenID == pathogenID  
    
    dose <- tbl_risk$dose_perEvent[cond]
    
    dr_model <- config$doseresponse[config$doseresponse$PathogenID == pathogenID,]
    
    if (is.na(dr_model$k) & !is.na(dr_model$alpha) & !is.na(dr_model$N50)) {
      tbl_risk$infectionProb_per_event[cond] <- dr.betapoisson(dose = dose,
                                                               alpha = dr_model$alpha, 
                                                               N50 = dr_model$N50)$infectionProbability
    } else if (!is.na(dr_model$k) & is.na(dr_model$alpha) & is.na(dr_model$N50)) {
      tbl_risk$infectionProb_per_event[cond] <- dr.expo(dose = dose,
                                                        k = dr_model$k)$infectionProbability
    } else {
      
      stop(
        "Doseresponse configuration incomplete for pathogen ", 
        config$health$PathogenName[config$health$PathogenID == pathogenID], ".\n",
        "Define required parameter(s), either k (for exponential model) or\n",
        "alpha & N50 (for beta-poisson model) "
      )
    }
  }
  
  cat("\n### STEP 5: Health        #################################################\n\n")
  
  health <- config$health[config$health$PathogenID %in% simulated_pathogenIDs,]
  
  if(debug) print(health)
  
  tbl_risk <- tbl_risk %>% 
    dplyr::left_join(config$health) %>% 
    dplyr::mutate(
      illnessProb_per_event = .data$infectionProb_per_event * .data$infection_to_illness,
      dalys_per_event = .data$illnessProb_per_event * .data$dalys_per_case
    ) %>% 
    dplyr::ungroup()
  
  tbl_risk_total <- tbl_risk %>%
    dplyr::group_by(
      .data$repeatID, 
      .data$TreatmentSchemeID, 
      .data$TreatmentSchemeName,
      .data$PathogenID, 
      .data$PathogenName, 
      .data$PathogenGroup
    ) %>% 
    dplyr::summarise(
      events = dplyr::n(), 
      inflow_median = median(.data$inflow), 
      logreduction_median = median(.data$logreduction), 
      volume_sum = sum(.data$volume_perEvent), 
      exposure_sum = sum(.data$exposure_perEvent),
      dose_sum = sum(.data$dose_perEvent),
      infectionProb_sum = 1 - prod( 1 - .data$infectionProb_per_event),
      illnessProb_sum = 1 - prod(1 - .data$illnessProb_per_event),
      dalys_sum = sum(.data$dalys_per_event)
    )
  
  if (minimal) list(
    
    input = list(treatment = treatment["events_long"]), 
    output = list(total = tbl_risk_total)
    
  ) else list(
    
    input = list(
      inflow = inflow["events"],
      treatment = treatment["events_long"],
      exposure = exposure["volumes"],
      doseresponse = doseresponse["paras"],
      health = health
    ), 
    output = list(
      events = tbl_risk, 
      total = tbl_risk_total
    )
  )
}
