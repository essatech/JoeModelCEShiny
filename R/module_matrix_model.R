#' Matrix Model UI
#'
#' The UI portion of the matrix model
#' 
#' @importFrom shiny NS tagList fluidRow column actionButton tags
#' @importFrom shinydashboard box
#'
#' @param id The id for this module
#'
#' @return a tagList containing UI elements
#'
module_matrix_model_ui <- function(id) {

  ns <- NS(id)

  tagList(
    shinydashboard::box(
      width = 12,
      tags$h4("Placeholder for population model - see other code set in prep."),
    )
  )
}