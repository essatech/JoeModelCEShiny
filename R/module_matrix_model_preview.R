#' Matrix Model PREVIEW UI
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
module_matrix_model_preview_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    shinydashboard::box(
      width = 12,
      
      tags$b("Density-Independent Components", style = "text-align: center;"),
      
      tags$table(class = "lam_v", style = "width: 100%;",
        tags$tr(
          tags$td("Lambda"),
          tags$td("1.234")
        ),
        tags$tr(
          tags$td("Net Repo. Rate"),
          tags$td("3.424")
        ),
        tags$tr(
          tags$td("Damping Ratio"),
          tags$td("0.89")
        ),
        tags$tr(
          tags$td("Generation Time"),
          tags$td("6.68")
        )
      ),
      
      tags$br(),
      
      tags$div(
        class = "lam_bb",
        actionButton(ns("transition_matrix"), "Transition Matrix"),
        actionButton(ns("reproductive_rate"), "Reproductive Rate"),
        actionButton(ns("elasticity"), "Elasticity Matrix"),
        actionButton(ns("sensitivity"), "Sensitivity Matrix")
      )
      
    ),
    
    shinydashboard::box(
      width = 12,
      tags$b("Density-Dependent Components", style = "text-align: center;"),
      
      tags$table(class = "", style = "width: 100%;",
                 tags$tr(
                   tags$td("Adult K"),
                   tags$td("100")
                 ),
      ),
      
    ),
    
    
    
  )
    
    

}


#' Matrix Model PREVIEW SERVER
#'
#' @param none
#'
#' @return None
#'
module_matrix_model_preview_server <- function(id) {
  moduleServer(id,
               function(input, output, session) {
                 ns <- session$ns
                 
                 print("matrix model preview server")
                 
                 
               })
}