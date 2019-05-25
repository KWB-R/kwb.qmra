# get_stop_text ----------------------------------------------------------------
get_stop_text <- function(keyword = NULL)
{
  texts <- list(
    doseresponse_config_incomplete = paste0(
      "Doseresponse configuration incomplete for pathogen %s.\n",
      "Define required parameter(s), either k (for exponential model) or\n",
      "alpha & N50 (for beta-poisson model)"
    ),
    must_be_value = paste0(
      "%s in configuration 'exposure' MUST be of 'type' ", 
      "= 'value' and column 'value' has to be numeric"
    )
  )
  
  if (is.null(keyword)) {
    return(texts)
  }
  
  if (! keyword %in% names(texts)) {
    stop(
      "No such keyword: ", keyword, ". Available keywords:\n",
      paste(names(texts), collapse = ", "), call. = FALSE
    )
  }
  
  texts[[keyword]]
}
