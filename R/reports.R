#' Create report (not working for "shiny" reports)
#' @param confDirs directory containing subdirectory/ies of QMRA configurations
#' default: system.file("extdata/configs/", package = "kwb.qmra")
#' @param report_template_dir report template directory 
#' (default: system.file("extdata/report", package = "kwb.qmra")) 
#' @param report_template_name default: "workflow.Rmd"
#' @param report_output_dir  directory where report should be saved, if NULL 
#' report_template_dir is used (default: NULL)
#' @param openReport open report in browser default: TRUE
#' @return generate html report
#' @importFrom utils browseURL
#' @export
report_workflow <- function(confDirs = system.file("extdata/configs/", 
                                                   package = "kwb.qmra"),
                            report_template_dir = system.file("extdata/report", package = "kwb.qmra"),
                            report_template_name = "workflow.Rmd",
                            report_output_dir  = NULL,
                            openReport = TRUE) {
  
  
  if (is.null(report_output_dir)) report_output_dir <- report_template_dir
  
  confDirs_path <-  dir(confDirs,
                        full.names = TRUE)
  

  
  for (i in 1:length(confDirs_path)) {
    
    confDir <- confDirs_path[i]
    
    output_file <- file.path(report_output_dir,
                             sprintf("%s_%s.html", 
                                     gsub(".Rmd", "", report_template_name),
                                     basename(confDir)))
    
    print(sprintf("Writing report for configuration: %s", confDir))
    rmarkdown::render(input = file.path(report_template_dir, report_template_name),
                      output_file = output_file,
                      output_format = "html_document")
    
    if (openReport == TRUE) browseURL(url = output_file)
    
  }
  
}
