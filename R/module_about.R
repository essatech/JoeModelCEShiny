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
      tags$h4("About page"),
      
      
      fluidRow(
        

        infoBox(title = NULL, color = 'blue', 
                value = 
                  actionButton("count2",
                               tags$b("Show Joe"),
                               class="chart-line",
                               width = "100%"),
                icon = icon("chart-bar"),
                subtitle = "Edit stessor magnitude value(s) for selected HUC(s)"),
        
        infoBox(title = NULL, color = 'blue', 
                value = 
                  actionButton("count2",
                               tags$b("Show Joe"),
                               class="chart-line",
                               width = "100%"),
                icon = icon("sliders-h"),
                subtitle = "Edit stessor magnitude value(s) for selected HUC(s)"),
        
        infoBox(title = NULL, color = 'blue', 
                value = 
                  actionButton("count2",
                               tags$b("Show Joe"),
                               class="chart-line",
                               width = "100%"),
                icon = icon("chart-line"),
                subtitle = "Edit stessor magnitude value(s) for selected HUC(s)"),
        
        
      ),
      
    ),



  )
}