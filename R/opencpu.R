#' OpenCPU wrapper: import config from CSV and convert to JSON
#' 
#' @param confDir directory to read configuration files (Default:  
#' system.file('extdata/configs/dummy', package = 'kwb.qmra')")
#' @return stores configuration in JSON format
#' @export
#' @examples 
#' ### Example json config file
#' config_json <- kwb.qmra::opencpu_config_read()
#' head(config_json)
#' 
opencpu_config_read <- function(
  confDir = system.file("extdata/configs/dummy", package = "kwb.qmra")
)
{
  
  args_list <- list(
    confDir
  )
  
  output <- base::do.call(kwb.qmra::config_read, args_list)
  jsonlite::toJSON(output, pretty = TRUE)
}



# simulate_risk_opencpu --------------------------------------------------------

#' OpenCPU wrapper: Simulate risk using JSON format for config/results
#' @param config_json config json object as retrieved by opencpu_config_read() 
#' @param usePoisson should a poisson proccess (see function dose_perEvent()) be
#' used to calculate the dose_perEvent (TRUE) or just the exposure_perEvent 
#' column (FALSE), (default: TRUE)
#' @param debug print debug information (default: TRUE)
#' @param lean if \code{TRUE}, a "lean" version of this function is called, see
#'   \code{kwb.qmra:::simulate_risk_lean}, (default: TRUE)
#' @return JSON list with parameters of user defined random distribution and 
#' corresponding values
#' @importFrom jsonlite fromJSON toJSON
#' @export
#' @examples 
#' ### Example json config file
#' config_json <- kwb.qmra::opencpu_config_read()
#' risk_json <- kwb.qmra::opencpu_simulate_risk(config_json)
#' 
opencpu_simulate_risk <- function(config_json = opencpu_config_read(), 
                                  usePoisson = TRUE, 
                                  debug = TRUE, 
                                  lean = TRUE)
{
  
  config <- jsonlite::fromJSON(config_json)
  
    args_list <- list(
    config, 
    usePoisson, 
    debug, 
    lean
  )
  
  output <- base::do.call(simulate_risk, args_list)
  jsonlite::toJSON(output, pretty = TRUE)
}
