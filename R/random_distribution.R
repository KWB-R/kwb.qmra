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

#' Default Min
#' @param org_min original minimum value
#' @param org_max original maximum value 
#' @param new_min new minimum value for replacement
#' @param f function apply on org_min and new_min to transform to the correct 
#' dimension (e.g. "log" in case of "lognorm" or "log10" in case of log10_norm),
#' (default: c())
#' @export
#' @examples 
#' default_min(org_min = 2, org_max = 100, new_min = 0.01, f = log10)
#' default_min(org_min = 0, org_max = 100, new_min = 0.01, f = log10)
#' default_min(org_min = 2, org_max = 100, new_min = 0.01, f = log)
#' default_min(org_min = 0, org_max = 100, new_min = 0.01, f = log)
default_min <- function(org_min, org_max, new_min, f = c) {
  ifelse(org_min > 0 && org_max > 1, 
         f(org_min), 
         ifelse(org_min == 0 && org_max < 1,
                0.01 * org_max, 
                f(new_min)
                )
         )
  
}

#' Default Max
#' @param org_max original maximum value 
#' @param new_max new maximum value for replacement
#' @param f function apply on "org_max" to transform to the correct 
#' dimension (e.g. "log" in case of "lognorm" or "log10" in case of log10_norm) 
#' (default: c())
#' @export
#' @examples 
#' default_max(org_max = 2, new_max = 0.01, f = log10)
#' default_max(org_max = 0, new_max = 0.01, f = log10)
#' default_max(org_max = 2, new_max = 0.01, f = log)
#' default_max(org_max = 0, new_max = 0.01, f = log)
default_max <- function(org_max, new_max, f = c) {

    ifelse(org_max > 0, f(org_max), f(new_max))
  
}

#' Create random distribution
#' @param type "uniform" calls runif(), "log10_uniform" calls 
#' 10^runif(number_of_events, log10_min, log10_max), "triangle" calls 
#' EnvStats::rtri(), "lognorm" calls rlnorm(), "norm" calls rnorm() and 
#' "log10_norm" calls 10^rnorm(number_of_events, mean = log10_mean, 
#' sdev = log10_sdev), (default: "uniform") 
#' @param number_of_repeatings how often should the random distribution with the
#' same parameters be generated (default: 1)
#' @param  number_of_events number of events
#' @param value constant value (no random number), gets repeated number_of_events
#' times (if 'type' = 'value')
#' @param min minimum value (default: 10), only used if 'type' is "runif" or
#' "triangle"
#' @param max maximum value (default: 1000), only used if 'type' is "runif" or
#' "triangle"
#' @param min_zero  only used if 'type' is "log10_uniform" or 
#' "log10_norm", "norm" or "lognorm" and "min" value equal zero. 
#' In this case the zero is replaced by this value (default: 0.01), see also 
#' \code{\link{default_min}}
#' @param log10_min minimum value (default: default_min(min, max, 
#' min_zero, f = log10)), only used if 'type' is "log10_uniform" or "log10_norm"
#' @param log10_max maximum value (default: ifelse(max > 0, log10(max), 
#' log10_zero_threshold), only used if 'type' is "log10_uniform" or "log10_norm"
#' @param log10_mean mean value (default: (log10_min + log10_max)/2), only used 
#' if 'type' is "log10_norm"
#' @param log10_sdev standard deviation (default: abs((log10_max- log10_mean) / 
#' qnorm(0.95)), only used if 'type' is "log10_norm"
#' @param mean mean value (default: (default_min(min, max, min_zero) / 
#' default_max(max, 10*min_zero)) / 2), only used if 'type' is "norm"
#' @param sdev standard deviation (default: abs((default_max(max, 10*min_zero) - 
#' mean) / qnorm(0.95))), only used if 'type' is "norm"
#' @param meanlog log mean value (default: mean(log((min + max) / 2))), only
#' used if 'type' is "lognorm"
#' @param sdlog standard deviation (default: abs(sd(c(default_min(min, max, 
#' min_zero, f = log)))) ), only used if 'type' is "lognorm"
#' @param mode (default: default_min(min, max, min_zero) + 
#' default_max(max, 10 * min_zero) / 2), only used if 'type' is "triangle"
#' @param debug print debug information (default: TRUE)
#' @return list with parameters of user defined random distribution and
#' corresponding values
#' @export
#' @importFrom stats sd qnorm runif rnorm rlnorm
#' @importFrom dplyr mutate
#' @importFrom rlang .data
#' @seealso for random triangle see \code{\link{rtri}}, for default 
#' min/max see \code{\link{default_min}} and \code{\link{default_max}} 
#' 


create_random_distribution <- function(type = "uniform",
  number_of_repeatings = 1,
  number_of_events = 365,
  value = 10,
  min = 10,
  max = 1000,
  min_zero = 0.01,
  log10_min = default_min(min, max, min_zero, f = log10), 
  log10_max = default_max(max, min_zero * 10, f = log10),
  log10_mean = (log10_min + log10_max) / 2,
  log10_sdev = abs((log10_max - log10_mean) / qnorm(0.95)),
  mean = (default_min(min, max, min_zero)/ default_max(max, 10*min_zero)) / 2,
  sdev = abs((default_max(max, 10*min_zero) - mean) / qnorm(0.95)),
  meanlog = mean(log(default_min(min, max, min_zero) + default_max(max, 10 * min_zero)) / 2),
  sdlog = abs(sd(c(default_min(min, max, min_zero, f = log), 
                   default_max(max, 10 * min_zero, f = log)))),
  mode = (default_min(min, max, min_zero) + default_max(max, 10 * min_zero)) / 2,
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
  else if (type == "log10_uniform") {
    if (debug) {
      cat(sprintf(
        "Create %d random distribution(s): 10^runif(with parameters n: %d, min: %f, max: %f)\n",
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
      min = log10_min,
      max = log10_max
    ) %>% 
      dplyr::mutate(values = 10^.data$values)


    paras <- data.frame(
      repeatings = number_of_repeatings,
      events = number_of_events,
      min = log10_min,
      max = log10_max
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

  else if (type == "log10_norm") {
    if (debug) {
      cat(sprintf(
        "Create %d random distribution(s): 10^rnorm(with parameters n: %d, mean: %f, sd: %f)\n",
        number_of_repeatings,
        number_of_events,
        log10_mean,
        log10_sdev
      ))
    }
    
    events <- distribution_repeater(
      number_of_repeatings = number_of_repeatings,
      number_of_events = number_of_events,
      func = rnorm,
      mean = log10_mean,
      sd = log10_sdev
    ) %>% 
      dplyr::mutate(values = 10^.data$values)
    
    paras <- data.frame(
      repeatings = number_of_repeatings,
      events = number_of_events,
      mean = log10_mean,
      sd = log10_sdev
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
  Valid values of 'type' are: 
  'uniform', 'triangle', 'norm', 'lognorm', 'log10_uniform' or 'log10_norm'", type))
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