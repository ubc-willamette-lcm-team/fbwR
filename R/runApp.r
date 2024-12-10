#' Helper function that launches a local version of the Shiny app
#' @export

runApp <- function() {
  shiny::runApp("inst/shiny/app.R")
}