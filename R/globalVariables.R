### Dirty workaround to get rid of "global variable warning" by using the 
### "tidyverse" packages (especially: dplyr::mutate with complicated calculations)
### Applied workaround as described by: 
### http://rmhogervorst.nl/cleancode/blog/2016/06/13/NSE_standard_evaluation_dplyr.html


globVars <- c("Key", 
              "LogReduction_Maximum",
              "LogReduction_Minimum",
              "PathogenID",
              "ReferenceID",
              "ReferenceLink",
              "ReferenceName",
              "dalys_per_case",
              "dalys_per_event",
              "dalys_sum",
              "dose_perEvent",
              "effluent",
              "exposure_perEvent",
              "illnessProb_per_event",
              "illnessProb_sum",
              "infectionProb_per_event",
              "infectionProb_sum",
              "infection_to_illness",
              "label",
              "logreduction",
              "n",
              "volume_perEvent")


globalVariables(names = globVars)