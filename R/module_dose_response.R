#' dose response Module UI
#'
#' The UI portion of the dose response module
#' 
#' @importFrom shiny NS tagList fluidRow column actionButton tags
#' @importFrom shinydashboard box
#'
#' @param id The id for this module
#'
#' @return a tagList containing UI elements
#'
module_dose_response_ui <- function(id) {

  ns <- NS(id)

  tagList(
    shinydashboard::box(
      width = 12,
      tags$h4("Modify Dose Response Functions"),
    )
  )
}