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
distribution_repeater <- function(
  number_of_repeatings = 10, number_of_events = 365, func, ...
)
{
  seq_repeatings <- seq_len(number_of_repeatings)

  repl <- vapply(
    seq_repeatings, 
    function(i) func(n = number_of_events, ...), 
    FUN.VALUE = numeric(number_of_events)
  )

  if (number_of_events == 1) t(repl) else repl %>%
    as.data.frame() %>%
    cbind(eventID = seq_len(number_of_events)) %>%
    stats::setNames(c(seq_repeatings, "eventID")) %>%
    tidyr::gather(key = "repeatID", value = "values", -.data$eventID) %>%
    dplyr::mutate(repeatID = as.integer(.data$repeatID)) %>%
    dplyr::select(.data$repeatID, .data$eventID, .data$values)
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
default_min <- function(org_min, org_max, new_min, f = c)
{
  ifelse(
    org_min > 0 && org_max > 1, 
    f(org_min), 
    ifelse(org_min == 0 && org_max < 1, f(0.01 * org_max), f(new_min))
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
default_max <- function(org_max, new_max, f = c)
{
  ifelse(org_max > 0, f(org_max), f(new_max))
}

#' Helper function: get percentile 
#' 
#' @param percent_within_minmax percent of data point within min/max (default: 
#' 0.9 i.e. 90 percent)
#' @export
#' @examples 
#' get_percentile(0.9)
#' get_percentile(0.95)
#' 
get_percentile <- function(percent_within_minmax = 0.9)
{
  qnorm(percent_within_minmax + (1 - percent_within_minmax) / 2)
}

#' Create random distribution
#' @param type "uniform" calls \code{\link[stats]{runif}}, "log10_uniform" calls 
#' 10^\code{\link[stats]{runif}}(number_of_events, log10_min, log10_max), "triangle" 
#' calls \code{\link[EnvStats]{rtri}}, "lognorm" calls \code{\link[stats]{rlnorm}}, 
#' "norm" calls \code{\link[stats]{rnorm}} and "log10_norm" calls 
#' 10^\code{\link[stats]{rnorm}}(number_of_events, mean = log10_mean, 
#' sdev = log10_sdev), (default: "uniform") 
#' @param number_of_repeatings how often should the random distribution with the
#' same parameters be generated (default: 1)
#' @param  number_of_events number of events
#' @param value constant value (no random number), gets repeated number_of_events
#' times (if 'type' = 'value')
#' @param min minimum value (default: 10), only used if 'type' is "uniform" or
#' "triangle"
#' @param max maximum value (default: 1000), only used if 'type' is "uniform" or
#' "triangle"
#' @param percent_within_minmax percent of data point within min/max (default: 
#' 0.9 i.e. 90 percent,see also \code{\link{get_percentile}}
#' @param min_zero  only used if 'type' is "log10_uniform" or 
#' "log10_norm", "norm" or "lognorm" and "min" value equal zero. 
#' In this case the zero is replaced by this value (default: 0.01), see also 
#' \code{\link{default_min}}
#' @param log10_min minimum value (default: \code{\link{default_min}}(min, max, 
#' min_zero, f = log10)), only used if 'type' is "log10_uniform" or "log10_norm"
#' @param log10_max maximum value (default: \code{\link{default_max}}(max, 
#' min_zero * 10, f = log10)), only used if 'type' is "log10_uniform" or "log10_norm"
#' @param log10_mean mean value (default: (log10_min + log10_max)/2), only used 
#' if 'type' is "log10_norm"
#' @param log10_sdev standard deviation (default: abs((log10_max- log10_mean) / 
#' \code{\link{get_percentile}}(percent_within_minmax))), only used if 'type' is 
#' "log10_norm"
#' @param mean mean value (default: (\code{\link{default_min}}(min, max, min_zero) / 
#' \code{\link{default_max}}(max, 10*min_zero)) / 2), only used if 'type' is "norm"
#' @param sdev standard deviation (default: abs((log10_max - log10_mean) / 
#' \code{\link{get_percentile}}(percent_within_minmax))), only used if 'type' is 
#' "norm"
#' @param meanlog log mean value (default: 
#' mean(log( \code{\link{default_min}}(min, max, min_zero) +  
#' \code{\link{default_max}}(max, 10 * min_zero)) / 2)), only
#' used if 'type' is "lognorm"
#' @param sdlog standard deviation (default: abs(sd(c(
#' \code{\link{default_min}}(min, max, min_zero, f = log), 
#' \code{\link{default_max}}(max, 10 * min_zero, f = log)))), 
#' only used if 'type' is "lognorm"
#' @param mode (default: \code{\link{default_min}}(min, max, min_zero) + 
#' \code{\link{default_max}}(max, 10 * min_zero) / 2), only used if 'type' is 
#' "triangle"
#' @param debug print debug information (default: TRUE)
#' @return list with parameters of user defined random distribution and
#' corresponding values
#' @export
#' @importFrom stats sd qnorm runif rnorm rlnorm
#' @importFrom dplyr mutate
#' @importFrom rlang .data
#' @seealso for random triangle see \code{\link[EnvStats]{rtri}}, for default 
#' min/max see \code{\link{default_min}},  \code{\link{default_max}} and 
#' \code{\link{get_percentile}} 
#' 
create_random_distribution <- function(
  type = "uniform",
  number_of_repeatings = 1,
  number_of_events = 365,
  value = 10,
  min = 10,
  max = 1000,
  percent_within_minmax = 0.9,
  min_zero = 0.01,
  log10_min = default_min(min, max, min_zero, f = log10), 
  log10_max = default_max(max, min_zero * 10, f = log10),
  log10_mean = (log10_min + log10_max) / 2,
  log10_sdev = abs((log10_max - log10_mean) / get_percentile(percent_within_minmax)),
  mean = (default_min(min, max, min_zero) + default_max(max, 10*min_zero)) / 2,
  sdev = abs((default_max(max, 10*min_zero) - mean) / get_percentile(percent_within_minmax)),
  meanlog = mean(log(default_min(min, max, min_zero) + default_max(max, 10 * min_zero)) / 2),
  sdlog = abs(sd(c(default_min(min, max, min_zero, f = log), 
                   default_max(max, 10 * min_zero, f = log)))),
  mode = (default_min(min, max, min_zero) + default_max(max, 10 * min_zero)) / 2,
  debug = TRUE
)
{
  # Inline helper functions 
  
  # Print a message if debug is TRUE
  debug_formatted <- function(fmt, ...) if (debug) cat(sprintf(fmt, ...))

  # Consistent message text
  distribution_text <- function(type_text, ...) paste0(
    "Create ", number_of_repeatings, " random distribution(s): ",
    type_text, " (n: ", number_of_events, ..., ")\n"
  )

  # Creation of paramter data frame
  paras_data_frame <- function(...) data.frame(
    type = type, 
    repeatings = number_of_repeatings,
    events = number_of_events,
    ...
  )
  
  # Creation of event data frame
  event_data_frame <- function(...) distribution_repeater(
    number_of_repeatings = number_of_repeatings,
    number_of_events = number_of_events,
    ...
  )
  
  # Product of numbers, to be reused
  number_product <- number_of_events * number_of_repeatings
  
  if (type == "value") {
    
    debug_formatted(
      "Replicate %d times constant value %f\n", number_product, value
    )

    events <- data.frame(
      repeatID = unlist(lapply(
        seq_len(number_of_repeatings), rep, number_of_events
      )),
      eventID = rep(seq_len(number_of_events), number_of_repeatings),
      values = rep(value, number_product)
    )
    
    paras <- paras_data_frame(value = value)
    
  } else if (type == "uniform") {
    
    debug_formatted(
      distribution_text("uniform", ", min: %f, max: %f"), 
      min, max
    )
    
    events <- event_data_frame(func = runif, min = min, max = max)
    paras <- paras_data_frame(min = min, max = max)
    
  } else if (type == "log10_uniform") {
    
    debug_formatted(
      distribution_text("10^runif", ", min: %f, max: %f"), 
      log10_min, log10_max
    )

    events <- event_data_frame(
      func = runif, min = log10_min, max = log10_max
    ) %>% 
      dplyr::mutate(values = 10^.data$values)
    
    paras <- paras_data_frame(min = log10_min, max = log10_max)
    
  } else if (type == "norm") {
    
    debug_formatted(
      distribution_text("norm", ", mean: %f, sd: %f"), mean, sdev
    )
    
    events <- event_data_frame(func = rnorm, mean = mean, sd = sdev)
    paras <- paras_data_frame(mean = mean, sd = sdev)
    
  } else if (type == "log10_norm") {
    
    debug_formatted(
      distribution_text("10^rnorm ", ", mean: %f, sd: %f"),
      log10_mean, log10_sdev
    )
    
    events <- event_data_frame(
      func = rnorm, mean = log10_mean, sd = log10_sdev
    ) %>% 
      dplyr::mutate(values = 10^.data$values)
    
    paras <- paras_data_frame(mean = log10_mean, sd = log10_sdev)
    
  } else if (type == "lognorm") {
    
    debug_formatted(
      distribution_text("lognorm", ", meanlog: %f, sdlog: %f"), 
      meanlog, sdlog
    )
    
    events <- event_data_frame(func = rlnorm, meanlog = meanlog, sdlog = sdlog)
    paras <- paras_data_frame(meanlog = meanlog, sdlog = sdlog)
    
  } else if (type == "triangle") {
    
    debug_formatted(
      distribution_text("triangle", ", min: %f, max: %f, mode = %f"),
      min, max, mode
    )
    
    events <- event_data_frame(
      func = EnvStats::rtri, min = min, max = max, mode = mode
    )
    paras <- paras_data_frame(min = min, max = max, mode = mode)
    
  } else {
    
    stop(sprintf(get_stop_text("no_such_distribution"), type), call. = FALSE)
  }

  list(events = events, paras = paras)
}

#' Create random distribution based on configuration file
#' @param config as retrieved by config_read()
#' @param number_of_repeatings how often should the random distribution with the
#' same parameters be generated (default: 1)
#' @param number_of_events number of events
#' @param debug print debug information (default: TRUE)
#' @return list random distributions based on configuration file
#' @export

generate_random_values <- function(
  config, number_of_repeatings = 1, number_of_events, debug = TRUE
) 
{
  default <- function(x, default) {
    if (is.na(x)) default else x
  }
  
  # Set defaults based on min/max for function create_random_distribution() if
  # values are missing
  if (config$type == "norm") {
    config$mean <- default(config$mean, (config$max + config$min) / 2)
    config$sd <- default(config$sd, sd(c(config$mean, config$max)) / 2)
  }

  if (config$type == "lnorm") {
    config$meanlog <- default(config$meanlog, log((config$min + config$max) / 2))
    config$sdlog <- default(config$sdlog, sd(log((c(config$min, config$max)))))
  }

  if (config$type == "triangle") {
    if (is.na(config$mode)) {
      if (config$min == config$max) {
        cat(
          "Distribution set from 'triangle' to 'uniform' because 'min'", 
          "equals 'max'\n"
        )
        config$type <- "uniform"
      } else {
        config$mode <- (config$min + config$max) / 2
      }
    }
  }
  
  create_random_distribution(
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
}
