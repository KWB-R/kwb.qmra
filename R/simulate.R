# number_of_exposures ----------------------------------------------------------

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

# number_of_repeatings ---------------------------------------------------------

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

# simulate_inflow --------------------------------------------------------------

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

# simulate_treatment -----------------------------------------------------------

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
  pathoGroups <- unique(config$inflow$PathogenGroup[config$inflow$simulate == 1])
  treatmentIDs <- unique(config$treatment$schemes$TreatmentID)
  
  treatment_wanted <- config$treatment$processes$TreatmentID %in% treatmentIDs
  pathogen_wanted <- config$treatment$processes$PathogenGroup %in% pathoGroups

  processes <- config$treatment$processes[treatment_wanted & pathogen_wanted, ]
  
  treatment_data <- get_treatment_data(
    processes = processes, 
    repeatings = number_of_repeatings(config), 
    n_events = number_of_exposures(config), 
    debug = debug,
    include_paras = ! minimal
  )

  treatment_events <- dplyr::left_join(treatment_data$events, config$treatment$schemes)
  
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
  
  treatment_paras <- dplyr::left_join(treatment_data$paras, lookup_treatmentNames)
  
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

# simulate_treatment_lean ------------------------------------------------------
simulate_treatment_lean <- function(config, debug = TRUE)
{
  pathoGroups <- unique(config$inflow$PathogenGroup[config$inflow$simulate == 1])
  treatmentIDs <- unique(config$treatment$schemes$TreatmentID)
  
  treatment_wanted <- config$treatment$processes$TreatmentID %in% treatmentIDs
  pathogen_wanted <- config$treatment$processes$PathogenGroup %in% pathoGroups
  
  processes <- config$treatment$processes[treatment_wanted & pathogen_wanted, ]
  
  repeatings = number_of_repeatings(config)
  n_events = number_of_exposures(config)

  # Provide row indices
  indices <- seq_len(nrow(processes))

  # Build data frame "events"
  events <- do.call(rbind, lapply(indices, function(i) {
    
    treatment <- processes[i, ]

    # Create random values for each process
    random_values <- generate_random_values(
      config = treatment, 
      number_of_repeatings = repeatings,
      number_of_events = n_events,
      debug = debug
    )

    cbind(
      random_values$events, 
      treatment[, c("TreatmentID", "PathogenGroup")], 
      row.names = NULL
    )
  }))
  
  # Rename column "values" to "logreduction" in events
  names(events)[names(events) == "values"] <- "logreduction"

  list(events_long = dplyr::left_join(events, config$treatment$schemes))
}

# get_treatment_data -----------------------------------------------------------
get_treatment_data <- function(
  processes, repeatings, n_events, debug = TRUE, 
  include_paras = TRUE
)
{
  # Helper function to select a treatment from "processes" and report about it
  get_treatment <- function(i) {
    treatment <- processes[i, ]
    if (debug) cat(sprintf(
      "Simulated treatment: %s for %s\n", 
      treatment$TreatmentName, 
      treatment$PathogenGroup
    ))
    treatment[, c("TreatmentID", "PathogenGroup")]
  }
  
  # Provide row indices
  indices <- seq_len(nrow(processes))
  
  # Create random values for each process
  random_list <- lapply(indices, function(i) {
    generate_random_values(
      config = processes[i, ], 
      number_of_repeatings = repeatings,
      number_of_events = n_events,
      debug = debug
    )
  })

  # Build data frame "events"
  events <- do.call(rbind, lapply(indices, function(i) {
    cbind(random_list[[i]]$events, get_treatment(i), row.names = NULL)
  }))
  
  # Rename column "values" to "logreduction" in events
  names(events)[names(events) == "values"] <- "logreduction"
  
  # Build data frame "paras" if requested
  if (include_paras) list(
    
    events = events,
    paras = do.call(plyr::rbind.fill, lapply(indices, function(i) {
      cbind(random_list[[i]]$paras, get_treatment(i), row.names = NULL)
    }))
    
  ) else list(
    
    events = events
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

# poisson_dose -----------------------------------------------------------------

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

# simulate_exposure ------------------------------------------------------------

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

# simulate_risk ----------------------------------------------------------------

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
  #treatment <- simulate_treatment(config, debug = debug, minimal = minimal)
  treatment <- simulate_treatment_lean(config, debug = debug)
  
  tbl_reduction <- treatment$events_long %>%
    dplyr::group_by(.data$TreatmentSchemeID,
                    .data$TreatmentSchemeName,
                    .data$PathogenGroup, 
                    .data$eventID,
                    .data$repeatID) %>% 
    dplyr::summarise(logreduction = sum(.data$logreduction))

  cat("\n### STEP 3: Exposure ######################################################\n\n")
  exposure <- simulate_exposure(config, debug)
  
  tbl_risk <-  dplyr::right_join(tbl_reduction, inflow$events) %>% 
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

# simulate_risk_lean -----------------------------------------------------------
simulate_risk_lean <- function(config, usePoisson = TRUE, debug = TRUE)
{
  #kwb.utils::assignPackageObjects("kwb.qmra")
  cat("# STEP 0: BASIC CONFIGURATION\n\n")
  
  is_simulated <- config$inflow$simulate == 1
  pathogens <- config$inflow$PathogenName[is_simulated]
  
  cat(sprintf(
    "Simulated %d pathogen(s): %s\n", 
    length(pathogens),
    paste(pathogens, collapse = ", ")
  ))
  
  n_repeatings <- number_of_repeatings(config)
  n_exposures <- number_of_exposures(config)
  
  cat(sprintf("Number of random distribution repeatings: %d\n", n_repeatings))
  cat(sprintf("Number of exposure events: %d\n\n", n_exposures))
  
  cat("# STEP 1: INFLOW\n\n")
  inflow <- simulate_inflow(config, debug)
  
  cat("\n# STEP 2: TREATMENT SCHEMES\n\n")
  treatment <- simulate_treatment_lean(config, debug = debug)
  
  tbl_reduction <- treatment$events_long %>%
    dplyr::group_by(
      .data$TreatmentSchemeID,
      .data$TreatmentSchemeName,
      .data$PathogenGroup, 
      .data$eventID,
      .data$repeatID
    ) %>% 
    dplyr::summarise(logreduction = sum(.data$logreduction))
  
  cat("\n### STEP 3: Exposure\n\n")
  exposure <- simulate_exposure(config, debug)
  
  tbl_risk <- dplyr::right_join(tbl_reduction, inflow$events) %>% 
    dplyr::mutate(effluent = 10 ^ (log10(.data$inflow) - .data$logreduction)) %>% 
    dplyr::left_join(exposure$volumes$events) %>% 
    dplyr::mutate(exposure_perEvent = .data$effluent * .data$volume_perEvent)

  tbl_risk$dose_perEvent <- if (usePoisson) { 
    poisson_dose(tbl_risk$exposure_perEvent) 
  } else {
    tbl_risk$exposure_perEvent
  }
  
  cat("\n# STEP 4: DOSE RESPONSE\n\n")
  
  pathogenIDs <- config$inflow$PathogenID[is_simulated]
  paras <- config$doseresponse[config$doseresponse$PathogenID %in% pathogenIDs, ]
  if (debug) {
    print(paras)
  }
  
  doseresponse <- list(
    response = dr.db_model(dr.db = paras),
    paras = paras
  )
  
  tbl_risk$infectionProb_per_event <- NA
  
  for (pathogenID in unique(tbl_risk$PathogenID)) {
    
    condition <- tbl_risk$PathogenID == pathogenID
    
    dose <- tbl_risk$dose_perEvent[condition]
    
    this_pathogen <- config$doseresponse$PathogenID == pathogenID
    
    k <- config$doseresponse$k[this_pathogen]
    alpha <- config$doseresponse$alpha[this_pathogen]
    n50 <- config$doseresponse$N50[this_pathogen]
    
    values <- if (is.na(k) & ! is.na(alpha) & ! is.na(n50)) {
      
      dr.betapoisson(dose = dose, alpha = alpha, N50 = n50)
      
    } else if (! is.na(k) & is.na(alpha) & is.na(n50)) {
      
      dr.expo(dose = dose, k = k)
      
    } else {
      
      stop(
        "Doseresponse configuration incomplete for pathogen ", 
        config$health$PathogenName[config$health$PathogenID == pathogenID], ".\n",
        "Define required parameter(s), either k (for exponential model) or\n",
        "alpha & N50 (for beta-poisson model) "
      )
    }
    
    tbl_risk$infectionProb_per_event[condition] <- values$infectionProbability
  }
  
  cat("\n# STEP 5: Health\n\n")
  
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
  
  list(
    input = list(treatment = treatment["events_long"]), 
    output = list(total = tbl_risk_total)
  )
}
