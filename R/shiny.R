#' Run shiny app
#' 
#' @param appDir directory of shiny app (default: system.file("extdata/shiny",
#'   package = "kwb.qmra"))
#' @param launch.browser If true, the system's default web browser will be
#'   launched automatically after the app is started. Defaults to true in
#'   interactive sessions only. This value of this parameter can also be a
#'   function to call with the application's URL.
#' @param ... additional parameters passed to shiny::runApp
#' @export
#' @importFrom shiny runApp
run_app <- function(
  appDir = system.file("extdata/shiny", package = "kwb.qmra"), 
  launch.browser = TRUE, 
  ...
)
{
  shiny::runApp(appDir = appDir, launch.browser = launch.browser, ...)
}
