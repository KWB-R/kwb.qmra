# simulate_inflow --------------------------------------------------------------

#' Simulate: inflow
#'
#' @param config as retrieved by config_read()
#' @param debug print debug information (default: TRUE)
#' @param lean if \code{TRUE}, only the "events" are returned (in a reduced
#'   version, i.e. without column \code{PathogenName} and with all ID columns
#'   being of class \code{integer}), otherwise a list with the events in element
#'   \code{events}" and the corresponding parameters in element \code{paras}.
#'   The default is \code{FALSE}, i.e. events and parameters are returned in a
#'   list.
#' @importFrom kwb.utils catIf catAndRun renameColumns removeColumns selectColumns
#' @return list with parameters of user defined random distribution and
#'   corresponding values
#' @export

simulate_inflow <- function(config, debug = TRUE, lean = FALSE)
{
  # Only pathogens to be simulated
  patho_ids <- config$inflow$PathogenID[config$inflow$simulate == 1]

  # Helper function to select a row from config$inflow
  get_inflow_config_row <- function(patho_id) {
    inflow_config <- config$inflow[config$inflow$PathogenID == patho_id, ] 
    kwb.utils::catIf(debug, sprintf(
      "Simulated pathogen: %s\n", inflow_config$PathogenName
    ))
    inflow_config
  }
  
  random_objects <- lapply(patho_ids, function(patho_id) {
    
    inflow_config_row <- get_inflow_config_row(patho_id)

    lapply(
      generate_random_values(
        config = inflow_config_row,
        number_of_repeatings = number_of_repeatings(config),
        number_of_events = number_of_exposures(config),
        debug = debug
      ), 
      cbind,
      inflow_config_row[, c("PathogenID", "PathogenName", "PathogenGroup")]
    )
  })

  events <- kwb.utils::catAndRun(
    "Providing inflow events", 
    do.call(rbind, lapply(random_objects, "[[", "events")) %>%
      kwb.utils::renameColumns(list(values = "inflow"))
  )
  
  if (lean) return(
    events %>%
      kwb.utils::removeColumns("PathogenName") %>%
      id_columns_to_integer()
  )
  
  list(
    events = events, 
    paras = kwb.utils::catAndRun(
      "Providing inflow paras", 
      do.call(plyr::rbind.fill, lapply(random_objects, "[[", "paras"))
    )
  )
}

# simulate_treatment -----------------------------------------------------------

#' Simulate: treatment
#' @param config as retrieved by config_read() 
#' @param wide if TRUE results will be converted to wide format (default: 
#' FALSE)
#' @param debug print debug information (default: TRUE)
#' @param lean lean (default: FALSE)
#' @return list with parameters of user defined random distribution and 
#' corresponding values
#' @importFrom tidyr spread
#' @export
#' 
simulate_treatment <- function(
  config, wide = FALSE, debug = TRUE, lean = FALSE
)
{
  inflow_config_simulated <- config$inflow %>%
    dplyr::filter(.data$simulate == 1)

  schemes_config <- config$treatment$schemes

  processes_simulated <- config$treatment$processes %>%
    dplyr::filter(
      .data$PathogenGroup %in% unique(inflow_config_simulated$PathogenGroup),
      .data$TreatmentID %in% unique(schemes_config$TreatmentID)
    )
  
  treatment_data <- get_treatment_data(
    processes = processes_simulated, 
    n_repeatings = number_of_repeatings(config), 
    n_events = number_of_exposures(config), 
    debug = debug,
    include_paras = ! lean
  )

  treatment_events <- treatment_data$events %>%
    dplyr::left_join(schemes_config)
  
  # Return only the result that is required by the web app if "lean" is TRUE
  if (lean) {
    
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
  
  # Create and return further results only if "lean" is FALSE
  treatment_id_to_name <- config$treatment$processes %>%
    dplyr::select(.data$TreatmentID, .data$TreatmentName) %>%
    dplyr::group_by(.data$TreatmentID) %>% 
    dplyr::slice(1)
  
  treatment_paras <- treatment_data$paras %>%
    dplyr::left_join(treatment_id_to_name)
  
  if (wide) list(
    
    events_long = treatment_events,
    events_wide = treatment_events_wide,
    schemes_events_wide = schemes_events_wide,
    schemes = schemes_config,
    paras = treatment_paras
    
  ) else list(
    
    events_long = treatment_events,
    schemes = schemes_config,
    paras = treatment_paras
  )
}

# simulate_treatment_lean ------------------------------------------------------
simulate_treatment_lean <- function(config, debug = TRUE)
{
  #kwb.utils::assignPackageObjects("kwb.qmra")
  simulated_inflows <- config$inflow %>%
    dplyr::filter(.data$simulate == 1) %>%
    kwb.utils::removeColumns(c(
      "simulate", "PathogenName", "ReferenceName", "ReferenceLink"
    )) %>% 
    id_columns_to_integer()
  
# Classes ???tbl_df???, ???tbl??? and 'data.frame':	3 obs. of  11 variables:
#  $ PathogenID   : int  3 32 34
#  $ PathogenGroup: chr  "Bacteria" "Viruses" "Protozoa"
#  $ type         : chr  "uniform" "uniform" "uniform"
#  $ value        : logi  NA NA NA
#  $ min          : num  100 50 1
#  $ max          : num  1e+06 5e+03 1e+04
#  $ mode         : logi  NA NA NA
#  $ mean         : logi  NA NA NA
#  $ sd           : logi  NA NA NA
#  $ meanlog      : logi  NA NA NA
#  $ sdlog        : logi  NA NA NA

  schemes <- config$treatment$schemes %>%
    kwb.utils::removeColumns(c("TreatmentSchemeName", "TreatmentName")) %>%
    id_columns_to_integer()
  
# Classes ???tbl_df???, ???tbl??? and 'data.frame':	8 obs. of  2 variables:
#  $ TreatmentSchemeID: int  5 5 5 5 5 5 5 5
#  $ TreatmentID      : int  16 17 19 20 8 9 15 14

  processes <- config$treatment$processes %>% 
    dplyr::filter(
      .data$TreatmentID %in% unique(schemes$TreatmentID), 
      .data$PathogenGroup %in% unique(simulated_inflows$PathogenGroup)
    ) %>% 
    kwb.utils::removeColumns(c(
      "TreatmentName", "TreatmentGroup", "ReferenceName", "ReferenceLink"
    )) %>% 
    id_columns_to_integer()
  
# Classes ???tbl_df???, ???tbl??? and 'data.frame':	24 obs. of  11 variables:
#  $ TreatmentID  : int  8 8 8 9 9 9 14 14 14 15 ...
#  $ PathogenGroup: chr  "Bacteria" "Protozoa" "Viruses" "Bacteria" ...
#  $ type         : chr  "uniform" "uniform" "uniform" "uniform" ...
#  $ value        : num  NA NA NA NA NA NA NA NA NA NA ...
#  $ min          : num  2 0.3 0.25 2 1 2.1 2 2 2 4 ...
#  $ max          : num  6 5 4 6 2 8.3 2 2 2 4 ...
#  $ mode         : logi  NA NA NA NA NA NA ...
#  $ mean         : logi  NA NA NA NA NA NA ...
#  $ sd           : logi  NA NA NA NA NA NA ...
#  $ meanlog      : logi  NA NA NA NA NA NA ...
#  $ sdlog        : logi  NA NA NA NA NA NA 

  n_repeatings <- number_of_repeatings(config)
  n_events <- number_of_exposures(config)

  # Provide row indices
  indices <- seq_len(nrow(processes))

  # Build data frame "events"
  events <- do.call(rbind, lapply(indices, function(i) {
    
    treatment <- processes[i, ]
    treatment_pathogen <- treatment[, c("TreatmentID", "PathogenGroup")]

    # Create random values for each process
    random_values <- generate_random_values(
      config = treatment, 
      number_of_repeatings = n_repeatings,
      number_of_events = n_events,
      debug = debug
    )

    cbind(random_values$events, treatment_pathogen, row.names = NULL)
  }))
  
# 'data.frame':	2640000 obs. of  5 variables:
#  $ repeatID     : int  1 1 1 1 1 1 1 1 1 1 ...
#  $ eventID      : int  1 2 3 4 5 6 7 8 9 10 ...
#  $ values       : num  5.26 5.54 5.22 2.91 5.3 ...
#  $ TreatmentID  : int  8 8 8 8 8 8 8 8 8 8 ...
#  $ PathogenGroup: chr  "Bacteria" "Bacteria" "Bacteria" "Bacteria" ...  

  events %>% 
    kwb.utils::renameColumns(list(values = "logreduction")) %>%
    dplyr::left_join(schemes, by = c("TreatmentID"))
}

# get_treatment_data -----------------------------------------------------------
get_treatment_data <- function(
  processes, n_repeatings, n_events, debug = TRUE, include_paras = TRUE
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
      number_of_repeatings = n_repeatings,
      number_of_events = n_events,
      debug = debug
    )
  })

  # Function to cbind random events and treatment
  cbind_events_treatment <- function(i) {
    cbind(random_list[[i]]$events, get_treatment(i), row.names = NULL)
  }
  
  # Build data frame "events"
  events <- lapply(indices, cbind_events_treatment) %>%
    do.call(what = rbind) %>%
    kwb.utils::renameColumns(list(values = "logreduction"))
  
  # Return the events if parameters are not requested
  if (! include_paras) {
    return(events)
  }
  
  # Function to cbind random events and treatment
  cbind_paras_treatment <- function(i) {
    cbind(random_list[[i]]$paras, get_treatment(i), row.names = NULL)
  }
  
  # Return a list with events and parameters
  list(
    events = events,
    paras = lapply(indices, cbind_paras_treatment) %>%
      do.call(what = plyr::rbind.fill)
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
poisson_dose <- function(exposure_perEvent)
{
  sapply(exposure_perEvent, stats::rpois, n = 1)
}

# simulate_exposure ------------------------------------------------------------

#' Simulate: exposure
#' @param config as retrieved by config_read() 
#' @param debug print debug information (default: TRUE)
#' @return list with parameters of user defined exposure scenario (number of events 
#' and volumes per Event)
#' @export

simulate_exposure <- function(config, debug = TRUE)
{
  kwb.utils::catIf(debug, sprintf("Simulated exposure: volume per event\n"))

  n_events <- number_of_exposures(config)
  
  volumes <-  generate_random_values(
    config = config$exposure[config$exposure$name == "volume_perEvent", ], 
    number_of_repeatings = number_of_repeatings(config),
    number_of_events = n_events,
    debug = debug
  )
  
  list(
    #number_of_events = n_events,
    volumes = list(
      events = kwb.utils::renameColumns(volumes$events, list(
        values = "volume_perEvent"
      ))
    )
  )
}

# simulate_risk ----------------------------------------------------------------

#' Simulate: risk
#' @param config as retrieved by config_read() 
#' @param usePoisson should a poisson proccess (see function dose_perEvent()) be
#' used to calculate the dose_perEvent (TRUE) or just the exposure_perEvent 
#' column (FALSE), (default: TRUE)
#' @param debug print debug information (default: TRUE)
#' @param lean if \code{TRUE}, a "lean" version of this function is called, see
#'   \code{kwb.qmra:::simulate_risk_lean}
#' @return list with parameters of user defined random distribution and 
#' corresponding values
#' @importFrom stats median
#' @importFrom  kwb.utils selectColumns
#' @import dplyr
#' @export
#' 
simulate_risk <- function(config, usePoisson = TRUE, debug = TRUE, lean = FALSE)
{
  #kwb.utils::assignPackageObjects("kwb.qmra")
  if (lean) {
    return(simulate_risk_lean(config, usePoisson, debug))
  }
  
  print_step(0, "basic configuration")
  
  print_basic_information(config)
  
  print_step(1, "inflow")
  
  inflow <- simulate_inflow(config, debug)
  
  print_step(2, "treatment schemes")
  
  treatment <- simulate_treatment(config, debug = debug)

  tbl_reduction <- treatment$events_long %>%
    dplyr::group_by(
      .data$TreatmentSchemeID,
      .data$TreatmentSchemeName,
      .data$PathogenGroup, 
      .data$eventID,
      .data$repeatID
    ) %>% 
    dplyr::summarise(logreduction = sum(.data$logreduction))
  
  print_step(3, "exposure")
  exposure <- simulate_exposure(config, debug)
  
  tbl_risk <-  dplyr::right_join(tbl_reduction, inflow$events) %>% 
    dplyr::mutate(effluent = 10 ^ (log10(.data$inflow) - .data$logreduction)) %>% 
    dplyr::left_join(exposure$volumes$events) %>% 
    dplyr::mutate(exposure_perEvent = .data$effluent * .data$volume_perEvent)

  tbl_risk$dose_perEvent <- if (usePoisson) { 
    poisson_dose(tbl_risk$exposure_perEvent) 
  } else {
    tbl_risk$exposure_perEvent
  }
  
  print_step(4, "dose response")
  
  simulated_pathogenIDs <- config$inflow$PathogenID[config$inflow$simulate == 1]

  indices <- config$doseresponse$PathogenID %in%  simulated_pathogenIDs
  
  paras <- config$doseresponse[config$doseresponse$PathogenID %in% simulated_pathogenIDs,]
  
  if (debug) print(paras)
  
  doseresponse <- list(
    response = dr.db_model(dr.db = config$doseresponse[indices,]),
    paras = paras
  )
  
  tbl_risk$infectionProb_per_event <- get_infection_prob(
    tbl_risk, 
    dose_response = config$doseresponse, 
    health = config$health
  )
  
  print_step(5, "health")
  
  health <- config$health[config$health$PathogenID %in% simulated_pathogenIDs,]
  
  if (debug) print(health)
  
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
  
  stats_total <- get_risk_total_stats(tbl_risk_total)
  
  stats_logremoval <- get_risk_logremoval_stats(tbl_risk)
  
  list(
    input = list(
      inflow = inflow["events"],
      treatment = treatment["events_long"],
      exposure = exposure["volumes"],
      doseresponse = doseresponse["paras"],
      health = health
    ), 
    output = list(
      events = tbl_risk, 
      total = tbl_risk_total,
      stats_total = stats_total,
      stats_logremoval = stats_logremoval
    )
  )
}

# simulate_risk_lean -----------------------------------------------------------
simulate_risk_lean <- function(config, usePoisson = TRUE, debug = TRUE)
{
  #kwb.utils::assignPackageObjects("kwb.qmra")
  #kwb.utils::assignArgumentDefaults(kwb.qmra:::simulate_risk_lean)
  #set.seed(123)
  print_step(0, "basic configuration")
  
  print_basic_information(config)

  print_step(1, "inflow")
  
  # Keep only required columns and convert IDs to integer
  inflow_events <- simulate_inflow(config, debug, lean = TRUE)
  
  # 'data.frame':	330000 obs. of  5 variables:
  #   $ repeatID     : int  1 1 1 1 1 1 1 1 1 1 ...
  #   $ eventID      : int  1 2 3 4 5 6 7 8 9 10 ...
  #   $ inflow       : num  287649 788326 409036 883029 940473 ...
  #   $ PathogenID   : int  3 3 3 3 3 3 3 3 3 3 ...
  #   $ PathogenGroup: chr  "Bacteria" "Bacteria" "Bacteria" "Bacteria" ...
  
  print_step(2, "treatment schemes")
  
  # Keep only required columns and convert IDs to integer
  events <- simulate_treatment_lean(config, debug = debug)

  # 'data.frame':	2640000 obs. of  6 variables:
  #   $ repeatID         : int  1 1 1 1 1 1 1 1 1 1 ...
  #   $ eventID          : int  1 2 3 4 5 6 7 8 9 10 ...
  #   $ logreduction     : num  4.58 3.55 2.8 5.71 2.38 ...
  #   $ TreatmentID      : int  8 8 8 8 8 8 8 8 8 8 ...
  #   $ PathogenGroup    : chr  "Bacteria" "Bacteria" "Bacteria" "Bacteria" ...
  #   $ TreatmentSchemeID: int  5 5 5 5 5 5 5 5 5 5 ..
  
  print_step(3, "exposure")
  
  exposure_volumes <- simulate_exposure(config, debug)$volumes$events
  
  # 'data.frame':	110000 obs. of  3 variables:
  #   $ repeatID       : int  1 1 1 1 1 1 1 1 1 1 ...
  #   $ eventID        : int  1 2 3 4 5 6 7 8 9 10 ...
  #   $ volume_perEvent: num  1e-05 1e-05 1e-05 1e-05 1e-05 1e-05 1e-05 1e-05 1e-05 1e-05 ...
  
  # Function that "exposure_perEvent" will be passed through
  poisson_or_not <- if (usePoisson) poisson_dose else identity
  
  tbl_risk <- events %>%
    dplyr::group_by(
      .data$TreatmentSchemeID,
      .data$PathogenGroup, 
      .data$eventID,
      .data$repeatID
    ) %>% 
    dplyr::summarise(logreduction = sum(.data$logreduction)) %>%
    dplyr::right_join(inflow_events, by = c("repeatID", "eventID", "PathogenGroup")) %>% 
    dplyr::mutate(effluent = 10 ^ (log10(.data$inflow) - .data$logreduction)) %>% 
    dplyr::left_join(exposure_volumes, by = c("repeatID", "eventID")) %>% 
    dplyr::mutate(
      exposure_perEvent = .data$effluent * .data$volume_perEvent,
      dose_perEvent = poisson_or_not(.data$exposure_perEvent)
    ) %>% 
    kwb.utils::removeColumns(c("PathogenGroup", "PathogenName", "effluent"))

  print_step(4, "dose response")
  
  tbl_risk$infectionProb_per_event <- get_infection_prob(
    tbl_risk, 
    dose_response = config$doseresponse, 
    health = config$health
  )

  print_step(5, "health")

  health_columns <- c("PathogenID", "infection_to_illness", "dalys_per_case")
  health <- kwb.utils::selectColumns(config$health, health_columns)
  
  total <- tbl_risk %>% 
    id_columns_to_integer() %>%
    dplyr::left_join(health, by = "PathogenID") %>% 
    dplyr::mutate(
      illnessProb_per_event = .data$infectionProb_per_event * 
        .data$infection_to_illness,
      dalys_per_event = .data$illnessProb_per_event * 
        .data$dalys_per_case
    ) %>% 
    dplyr::ungroup() %>%
    dplyr::group_by(
      .data$repeatID, 
      .data$TreatmentSchemeID, 
      .data$PathogenID
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
  
  stats_total <- get_risk_total_stats_lean(data_lean = total, 
                                          config = config)
  
  stats_logremoval <- get_risk_logremoval_stats_lean(data_lean = events,
                                                config = config)

  #bak <- list(events = events , total = total)
  list(events = events , 
       total = total, 
       stats_total = stats_total, 
       stats_logremoval = stats_logremoval
       )
}

# get_infection_prob -----------------------------------------------------------
get_infection_prob <- function(tbl_risk, dose_response, health)
{
  result_vector <- rep(NA_real_, nrow(tbl_risk))
  
  for (pathogenID in unique(tbl_risk$PathogenID)) {
    
    condition <- tbl_risk$PathogenID == pathogenID
    
    this_pathogen <- dose_response$PathogenID == pathogenID
    
    values <- get_dose_response_values(
      dose = tbl_risk$dose_perEvent[condition], 
      k = dose_response$k[this_pathogen], 
      n50 = dose_response$N50[this_pathogen],
      alpha = dose_response$alpha[this_pathogen],
      pathogen_name = health$PathogenName[health$PathogenID == pathogenID]
    )

    result_vector[condition] <- values$infectionProbability
  }
  
  result_vector
}

# get_dose_response_values -----------------------------------------------------
get_dose_response_values <- function(dose, k, alpha, n50, pathogen_name)
{
  stopifnot(length(k) == 1, length(alpha) == 1, length(n50) == 1)
  
  if (is.na(k) && ! is.na(alpha) && ! is.na(n50)) {
    
    dr.betapoisson(dose = dose, alpha = alpha, N50 = n50)
    
  } else if (! is.na(k) & is.na(alpha) & is.na(n50)) {
    
    dr.expo(dose = dose, k = k)
    
  } else {
    
    stop(sprintf(
      get_stop_text("doseresponse_config_incomplete"), pathogen_name
    ))
  }
}
