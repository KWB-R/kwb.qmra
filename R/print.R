# print_basic_information ------------------------------------------------------
print_basic_information <- function(config)
{
  print_simulated_pathogens(config)
  print_repeatings_exposures(config)
}

# print_simulated_pathogens ----------------------------------------------------
print_simulated_pathogens <- function(config)
{
  pathogens <- config$inflow$PathogenName[config$inflow$simulate == 1]
  
  cat(sprintf(
    "Simulated %d pathogen(s): %s\n", 
    length(pathogens),
    paste(pathogens, collapse = ", ")
  ))
}

# print_repeatings_exposures ---------------------------------------------------
print_repeatings_exposures <- function(config)
{
  n_repeatings <- number_of_repeatings(config)
  n_exposures <- number_of_exposures(config)
  
  cat(sprintf("Number of random distribution repeatings: %d\n", n_repeatings))
  cat(sprintf("Number of exposure events: %d\n", n_exposures))
}

# print_step -------------------------------------------------------------------
print_step <- function(number, name)
{
  cat(sprintf("\n# STEP %d: %s\n\n", number, toupper(name)))
}
