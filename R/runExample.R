#' Run shiny function
#' @examples
#' runExample()
#' @export
runExample <- function() {
  appDir <- paste0(find.package("analyzeGES"), "/shiny-examples/shiny_app")
  shiny::runApp(appDir, display.mode = "normal")
}
