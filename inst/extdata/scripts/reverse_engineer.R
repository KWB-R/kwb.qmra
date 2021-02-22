#
# Source first to load functions below!
#

# Reverse engineering "Total Summary" ------------------------------------------
if (FALSE)
{
  # Input data returned by the simulation
  total <- risk$total
  
  # Create "lean" version of the input data
  total_lean <- kwb.utils::removeColumns(total, pattern = "(Name|Group)$") %>%
    kwb.qmra:::id_columns_to_integer()
  
  # What would we save in terms of object sizes?
  size_ratio(total_lean, total)
  
  # Call the original version of the summary function that needs all columns
  total_stats_old <- kwb.qmra:::get_risk_total_stats(total)
  
  # Call the new version of the summary function that accepts the lean version
  # of the input data. It requires the config object to add text columns
  total_stats_new <- kwb.qmra:::get_risk_total_stats_lean(total_lean, config)
  
  stopifnot(identical(
    structure(total_stats_old, groups = NULL),
    structure(total_stats_new, groups = NULL)
  ))
}

# Reverse engineering "Total Summary" ------------------------------------------
if (FALSE)
{
  # Get the "events" table
  events <- risk$events
  
  # Create a "lean" version of the "events" table
  events_lean <- events %>% 
    kwb.utils::removeColumns(c("TreatmentSchemeName", "TreatmentName")) %>%
    kwb.qmra:::id_columns_to_integer()
  
  # What would we save in terms of object sizes?
  size_ratio(events_lean, events)

  # Call the original version of the summary function that needs all columns
  system.time(logremoval_stats_old <- kwb.qmra:::get_risk_logremoval_stats(
    events
  ))

  # Call the new version of the summary function that accepts the lean version
  # of the input data. It requires the config object to add text columns
  system.time(logremoval_stats_new <- kwb.qmra:::get_risk_logremoval_stats_lean(
    data = events_lean, config
  ))

  stopifnot(identical(
    structure(logremoval_stats_old, groups = NULL),
    structure(logremoval_stats_new, groups = NULL)
  ))
}

# Functions --------------------------------------------------------------------

# size_ratio -------------------------------------------------------------------
size_ratio <- function(object, base_object)
{
  kwb.utils::percentage(byte_size(object), byte_size(base_object))
}

# byte_size --------------------------------------------------------------------
byte_size <- function(x)
{
  as.integer(object.size(x))
}
