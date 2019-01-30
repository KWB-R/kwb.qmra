library(readxl)

who_influent <-  readxl::read_excel(path = whoFile,
                                    sheet = "InfluentConcentrations") %>%  
  dplyr::left_join(.data$who_sources)