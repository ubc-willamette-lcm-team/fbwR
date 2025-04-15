#' Launch a local version of the FBW-R Shiny app
#' 
#' @import shiny
#' @import ggplot2
#' @import openxlsx
#' @import rhandsontable
#' @import shinycssloaders
#' @import shinyBS
#' @importFrom shinyjs click
#' @importFrom shinythemes shinytheme
#' @importFrom plotly plotlyOutput renderPlotly ggplotly
#' @importFrom DT DTOutput renderDT
#' @importFrom formattable formattableOutput renderFormattable
#' @export

runApp <- function() {
  shiny::runApp(appDir = system.file("app", package = "fbwR"))
  # shiny::runApp("inst/app/app.R")
}
