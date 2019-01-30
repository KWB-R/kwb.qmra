library(readxl)

who_pathogens <- readxl::read_excel(path = whoFile,
                                    sheet = "Pathogens") %>%  
  dplyr::left_join(.data$who_sources)

who_influent <-  readxl::read_excel(path = whoFile,
                                    sheet = "InfluentConcentrations") %>%  
  dplyr::left_join(.data$who_sources)