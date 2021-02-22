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

#' OpenCPU wrapper: run risk calculation and convert results to JSON format
#' @param config config object as retrieved by \code{kwb.qmra::config_read()}
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
#' ### Example simulation run
#' ## Read from JSON
#' config_json <- kwb.qmra::opencpu_config_read()
#' config <- jsonlite::fromJSON(config_json)
#' ## Optionally directly import from CSVs
#' # config <- kwb.qmra::config_read()
#' risk <- kwb.qmra::opencpu_simulate_risk(config)
#' risk_json <- jsonlite::toJSON(risk, pretty = TRUE)
#' writeLines(text = risk_json, "risk.json")
#' 
opencpu_simulate_risk <- function(config = config_read(), 
                                  usePoisson = TRUE, 
                                  debug = TRUE, 
                                  lean = TRUE)
{
  
    args_list <- list(
    config, 
    usePoisson, 
    debug, 
    lean
  )
  
 base::do.call(simulate_risk, args_list)
}
