#' Run shiny function
#'
#' @export
runExample <- function() {
  appDir <- system.file("shiny-examples", "GES_plotter", package = "analyzeGES")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `analyzeGES`.", call. = FALSE)
  }
  
  shiny::runApp(appDir, display.mode = "normal")
}