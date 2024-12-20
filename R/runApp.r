#' Helper function that launches a local version of the Shiny app
#' @export

runApp <- function() {
  shiny::runApp(appDir = system.file("app", package = "fbwR"))
}