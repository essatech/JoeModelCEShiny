#' module_about_ui
#'
#' The UI portion of the about module
#' 
#' @importFrom shiny NS tagList fluidRow column actionButton tags
#' @importFrom shinydashboard box
#'
#' @param id The id for this module
#'
#' @return a tagList containing UI elements
#'
module_about_ui <- function(id) {

  ns <- NS(id)

  tagList(
    shinydashboard::box(
      width = 12,
      tags$h4("Import and Export Data"),
    )
  )
}