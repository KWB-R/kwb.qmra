# library("profmem")
# options(profmem.threshold = 2000)
# p <- profmem({

#remotes::install_github("kwb-r/kwb.qmra@perf-1")

library(dplyr)
library(tidyr)

set.seed(123)

#setwd(kwb.utils::safePath(kwb.utils::desktop(), "../Documents/R-Development/RScripts/QMRA"))
setwd("~/Downloads/QMRA")

branch <- "master"

output_dir <- "output"

# temp <- tempdir()
# url <- "https://zenodo.org/record/159527/files/QMRA_OldFord.zip"
# t_path <- file.path(temp, basename(url))
# download.file(url, dest= t_path, mode="wb") 
# unzip(t_path, exdir = temp)
# 
scenarios_dir <- kwb.utils::safePath("scenarios")

cat(sprintf("### Available scenarios:\n%s",
            paste(dir(scenarios_dir), collapse = "\n")))

#### DEFINE DIRECTORY ################
confDir <- dir(scenarios_dir, full.names = TRUE)[3]

#### LOAD ############################
config <- kwb.qmra::config_read(confDir)

### 2.1) Only use one treatment scheme "1st-2nd-MBR-Cl" (instead of 5 different!)  
filter_condition <- config$treatment$schemes$TreatmentSchemeID == 5
config$treatment$schemes <- config$treatment$schemes[filter_condition,]

config$exposure$value[config$exposure$name == "number_of_repeatings"] <- 100

# 3) Perform QMRA
#risk <- kwb.qmra::simulate_risk(config, usePoisson = FALSE)
risk <- kwb.qmra::simulate_risk(config, usePoisson = FALSE, lean = TRUE)

# 4 Export model output
# modelName <- "qmra_risk"

stopifnot(file.exists(output_dir))

### 4.1) Risk per event (can be skipped!): 
# data.table::fwrite(risk$output$events,
#                                file = sprintf("%s/qmra_risk_perEvent.csv", 
#                                               output_dir))

### 4.2.1) Risk summed for each random run (for each random run and each pathogen group)											
# data.table::fwrite(risk$output$total,
#                                file = sprintf("%s/qmra_risk_total.csv", 
#                                               output_dir))

system.time(risk_total_stats <- kwb.qmra:::get_risk_total_stats_lean(
  risk$total, config
))

old_version <- FALSE

if (old_version) system.time(
  risk_total_stats <- tidyr::gather(
    data = risk$output$total, 
    key = "key", 
    value = "value",  
    -TreatmentSchemeID, 
    -TreatmentSchemeName, 
    -PathogenGroup, 
    -repeatID, 
    events, 
    -PathogenID, 
    -PathogenName
  ) %>% 
    dplyr::group_by(
      TreatmentSchemeID, TreatmentSchemeName, PathogenGroup, PathogenID, PathogenName, key
    ) %>% 
    dplyr::summarise( 
      min = min(value), 
      p05 = quantile(value, probs = 0.05),
      p25 =  quantile(value, probs = 0.25),
      mean = mean(value), 
      median = median(value), 
      p75 =  quantile(value, probs = 0.75),
      p95 = quantile(value, probs = 0.95),
      max = max(value)
    )
)

### 4.2.2) Risk statistics
# data.table::fwrite(
#   risk_total_stats,
#   file = file.path(output_dir, sprintf("qmra_risk_total_stats_%s.csv", branch))
# )

### 4.3.1) Risk log_removal (can be skipped!):
# 	data.table::fwrite(risk$input$treatment$events_long,
#                    file = sprintf("%s/qmra_risk_logremoval.csv", output_dir))

### 4.3.2) Risk log_removal statistics
system.time(risk_logremoval_stats <- kwb.qmra:::get_risk_logremoval_stats_lean(
  risk$events, config
))

if (old_version) system.time(
  risk_logremoval_stats <- risk$input$treatment$events_long %>% 
    dplyr::group_by(
      TreatmentSchemeID, TreatmentSchemeName, TreatmentID, TreatmentName, PathogenGroup
    ) %>% 
    dplyr::summarise( 
      min = min(logreduction), 
      p05 = quantile(logreduction, probs = 0.05),
      p25 =  quantile(logreduction, probs = 0.25),
      mean = mean(logreduction), 
      median = median(logreduction), 
      p75 =  quantile(logreduction, probs = 0.75),
      p95 = quantile(logreduction, probs = 0.95),
      max = max(logreduction)
    )
)

pryr::mem_used()

object.size(risk)

ungroup <- dplyr::ungroup
ungrouped_identical <- function(a, b) identical(ungroup(a), ungroup(b))

stopifnot(ungrouped_identical(risk_total_stats, readRDS("risk_total_stats.rds")))
stopifnot(ungrouped_identical(risk_logremoval_stats, readRDS("risk_logremoval_stats.rds")))

message("Everything all right!")

#saveRDS(risk_total_stats, "risk_total_stats.rds")
#saveRDS(risk_logremoval_stats, "risk_logremoval_stats.rds")

if (FALSE)
{
  rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
  #.rs.restartR()
  gc() #free up memrory and report the memory usage
  pryr::mem_used()
  # Sys.sleep(60)
  
  ram_usage <- readr::read_csv2("output/ram_usage.csv")
  ram_usage$number_of_treatments <- as.factor(ram_usage$number_of_treatments)
  ggplot2::ggplot(ram_usage, ggplot2::aes(x = number_of_rows/1000000, y = ram_usage_in_GB, col = number_of_treatments)) +
    ggplot2::facet_wrap(~ scenario, ncol = 1) + 
    ggplot2::xlab("Number of rows (in million)") +
    ggplot2::ylab("Maximum RAM Usage in Gigabyte (GB)") +
    ggplot2::geom_point() +
    ggplot2::geom_text(label = sprintf("%s GB", ram_usage$ram_usage_in_GB), 
                       nudge_x = 1) +
    ggplot2::theme_minimal() + 
    ggplot2::theme(legend.position = "top")
  
  pdff <- kwb.utils::preparePdf("output/ram_usage.pdf")
  ggplot2::ggplot(ram_usage, ggplot2::aes(x = number_of_rows/1000000, y = ram_usage_in_GB, col = scenario,  shape = number_of_treatments)) +
    ggplot2::xlab("Number of rows (in million)") +
    ggplot2::ylab("Maximum RAM Usage in Gigabyte (GB)") +
    ggplot2::geom_point() +
    ggplot2::geom_text(label = sprintf("%s GB", ram_usage$ram_usage_in_GB), 
                       nudge_x = 1
    ) +
    ggplot2::theme_minimal() + 
    ggplot2::theme(legend.position = "top")
  kwb.utils::finishAndShowPdf(pdff)
}
