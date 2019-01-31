#' Helper function: distributon repeater
#' @param number_of_repeatings how often should the random distribution with the
#' same parameters be generated (default: 1)
#' @param  number_of_events number of events
#' @param func distribution function to be repeated (e.g. runif, rlnorm, rnorm)
#' @param ... further parameters passed to func
#' @return data.frame with columns repeatID, eventID and values
#' @export
#' @examples
#' distribution_repeater(
#'   number_of_repeatings = 2,
#'   number_of_events = 10,
#'   func = runif,
#'   min = 1,
#'   max = 10
#' )
distribution_repeater <- function(number_of_repeatings = 10,
                                  number_of_events = 365,
                                  func,
                                  ...) {
  repl_tmp <- vapply(seq_len(number_of_repeatings),
    function(x) {
      func(
        n = number_of_events,
        ...
      )
    },
    FUN.VALUE = numeric(number_of_events)
  )

  if (number_of_events == 1) repl_tmp <- t(repl_tmp)

  repl <- repl_tmp %>%
    as.data.frame() %>%
    cbind(eventID = seq_len(number_of_events))
  colnames(repl) <- c(seq_len(number_of_repeatings), "eventID")

  repl_list <- tidyr::gather_(
    data = repl,
    key_col = "repeatID",
    value_col = "values",
    gather_cols = as.character(seq_len(number_of_repeatings))
  )

  repl_list$repeatID <- as.integer(repl_list$repeatID)

  return(repl_list[, c("repeatID", "eventID", "values")])
}




#' Create random distribution
#' @param type "uniform" calls runif(), "loguniform" calls 
#' 10^runif(number_of_repeatings, log10_min, log10_max), "triangle" calls 
#' EnvStats::rtri(), "lognorm" calls rlnorm() and "norm" calls rnorm(), 
#' (default: "uniform") 
#' @param number_of_repeatings how often should the random distribution with the
#' same parameters be generated (default: 1)
#' @param  number_of_events number of events
#' @param value constant value (no random number), gets repeated number_of_events
#' times (if 'type' = 'value')
#' @param min minimum value (default: 10), only used if 'type' is "runif" or
#' "triangle"
#' @param max maximum value (default: 1000), only used if 'type' is "runif" or
#' "triangle"
#' @param log_zero_threshold  only used if 'type' is "loguniform"
#' and "min" value equal zero. In this case the zero is replaced by this value 
#' (default: 1E-10), i.e. with a lower limit of log10(1E-10) = -10
#' @param log10_min minimum value (default: ifelse(min > 0, log10(min), log10(1E-10)), 
#' only used if 'type' is "loguniform"
#' @param log10_max maximum value (default: ifelse(max > 0, log10(max), log10(1E-10)), 
#' only used if 'type' is "loguniform"
#' @param mean mean value (default: mean of min & max value), only used if 'type'
#' is "norm"
#' @param sdev standard deviation (default: (max-mean) / qnorm(0.975)),
#' only used if 'type' is "norm"
#' @param meanlog log mean value (default: mean of log min & max value), only
#' used if 'type' is "lognorm"
#' @param sdlog standard deviation (default: sd of log min & max value), only
#' used if 'type' is "lognorm"
#' @param mode (default: mean of min & max), only used if 'type' is "triangle"
#' @param debug print debug information (default: TRUE)
#' @return list with parameters of user defined random distribution and
#' corresponding values
#' @export
#' @importFrom stats sd qnorm runif rnorm rlnorm
#' @importFrom dplyr mutate
#' @importFrom rlang .data
#' @seealso for random triangle see \code{\link{rtri}}


create_random_distribution <- function(type = "uniform",
                                       number_of_repeatings = 1,
                                       number_of_events = 365,
                                       value = 10,
                                       min = 10,
                                       max = 1000,
                                       log_zero_threshold = 1E-10, 
                                       log10_min = ifelse(min > 0, 
                                                       log10(min), 
                                                       log10(log_zero_threshold)), 
                                       log10_max = ifelse(max > 0, 
                                                       log10(max), 
                                                       log10(log_zero_threshold)),
                                       mean = (min + max) / 2,
                                       sdev = (max - mean) / qnorm(0.975),
                                       meanlog = mean(log((min + max) / 2)),
                                       sdlog = sd(log((c(min, max)))),
                                       mode = (min + max) / 2,
                                       debug = TRUE) {
  if (type == "value") {
    if (debug) {
      cat(sprintf(
        "Replicate %d times constant value %f\n",
        number_of_events * number_of_repeatings,
        value
      ))
    }

    values <- rep(value,
      times = number_of_events * number_of_repeatings
    )

    events <- data.frame(
      repeatID = lapply(
        1:number_of_repeatings,
        function(x) {
          rep(x, number_of_events)
        }
      ) %>%
        unlist(),
      eventID = rep(1:number_of_events, number_of_repeatings),
      values = rep(value, number_of_events * number_of_repeatings)
    )

    paras <- data.frame(
      repeatings = number_of_repeatings,
      events = number_of_events,
      value = value
    )
  }
  else if (type == "uniform") {
    if (debug) {
      cat(sprintf(
        "Create %d random distribution(s): uniform (with parameters n: %d, min: %f, max: %f)\n",
        number_of_repeatings,
        number_of_events,
        min,
        max
      ))
    }




    events <- distribution_repeater(
      number_of_repeatings = number_of_repeatings,
      number_of_events = number_of_events,
      func = runif,
      min = min,
      max = max
    )


    paras <- data.frame(
      repeatings = number_of_repeatings,
      events = number_of_events,
      min = min,
      max = max
    )
  }
  else if (type == "loguniform") {
    if (debug) {
      cat(sprintf(
        "Create %d random distribution(s): loguniform (with parameters n: %d, min: %f, max: %f)\n",
        number_of_repeatings,
        number_of_events,
        log10_min,
        log10_max
      ))
    }


    events <- distribution_repeater(
      number_of_repeatings = number_of_repeatings,
      number_of_events = number_of_events,
      func = runif,
      min = logmin,
      max = logmax
    ) %>% 
      dplyr::mutate(values = 10^.data$values)


    paras <- data.frame(
      repeatings = number_of_repeatings,
      events = number_of_events,
      min = min,
      max = max
    )
  }
  else if (type == "norm") {
    if (debug) {
      cat(sprintf(
        "Create %d random distribution(s): norm (with parameters n: %d, mean: %f, sd: %f)\n",
        number_of_repeatings,
        number_of_events,
        mean,
        sdev
      ))
    }

    events <- distribution_repeater(
      number_of_repeatings = number_of_repeatings,
      number_of_events = number_of_events,
      func = rnorm,
      mean = mean,
      sd = sdev
    )

    paras <- data.frame(
      repeatings = number_of_repeatings,
      events = number_of_events,
      mean = mean,
      sd = sdev
    )
  }


  else if (type == "lognorm") {
    if (debug) {
      cat(sprintf(
        "Create %d random distribution(s): lognorm (with parameters n: %d, meanlog: %f, sdlog: %f)\n",
        number_of_repeatings,
        number_of_events,
        meanlog,
        sdlog
      ))
    }


    events <- distribution_repeater(
      number_of_repeatings = number_of_repeatings,
      number_of_events = number_of_events,
      func = rlnorm,
      meanlog = meanlog,
      sdlog = sdlog
    )


    paras <- data.frame(
      repeatings = number_of_repeatings,
      events = number_of_events,
      meanlog = meanlog,
      sdlog = sdlog
    )
  } else if (type == "triangle") {
    if (debug) {
      cat(sprintf(
        "Create %d random distribution(s): triangle (with parameters n: %d, min: %f, max: %f, mode = %f)\n",
        number_of_repeatings,
        number_of_events,
        min,
        max,
        mode
      ))
    }


    events <- distribution_repeater(
      number_of_repeatings = number_of_repeatings,
      number_of_events = number_of_events,
      func = EnvStats::rtri,
      min = min,
      max = max,
      mode = mode
    )

    paras <- data.frame(
      repeatings = number_of_repeatings,
      events = number_of_events,
      min = min,
      max = max,
      mode = mode
    )
  } else {
    stop(sprintf("Your value for parameter 'type' = %s is not an implemented distribution.
  Valid values of 'type' are: 'uniform', 'triangle', 'norm' or 'lognorm'", type))
  }

  paras <- data.frame(type = type, paras)

  dist <- list(
    events = events,
    paras = paras
  )

  return(dist)
}


#' Create random distribution based on configuration file
#' @param config as retrieved by config_read()
#' @param number_of_repeatings how often should the random distribution with the
#' same parameters be generated (default: 1)
#' @param number_of_events number of events
#' @param debug print debug information (default: TRUE)
#' @return list random distributions based on configuration file
#' @export

generate_random_values <- function(config,
                                   number_of_repeatings = 1,
                                   number_of_events,
                                   debug = TRUE) {
  #### Set defaults based on min/max for function create_random_distribution() if
  #### values are missing
  if (config$type == "norm") {
    if (is.na(config$mean)) config$mean <- (config$max + config$min) / 2
    if (is.na(config$sd)) config$sd <- sd(c(config$mean, config$max)) / 2
  }

  if (config$type == "lnorm") {
    if (is.na(config$meanlog)) config$meanlog <- mean(log((config$min + config$max) / 2))
    if (is.na(config$sdlog)) config$sdlog <- sd(log((c(config$min, config$max))))
  }


  if (config$type == "triangle") {
    if (is.na(config$mode)) {
      if (config$min == config$max) {
        cat("Distribution set from 'triangle' to 'uniform' because 'min' equals 'max'\n")
        config$type <- "uniform"
      } else {
        config$mode <- (config$min + config$max) / 2
      }
    }
  }


  random <- create_random_distribution(
    type = config$type,
    number_of_repeatings = number_of_repeatings,
    number_of_events = number_of_events,
    value = config$value,
    min = config$min,
    max = config$max,
    mean = config$mean,
    sdev = config$sd,
    meanlog = config$meanlog,
    sdlog = config$sdlog,
    mode = config$mode,
    debug = debug
  )



  return(random)
}
