#' Display all stressor-response curves in a single modal - UI function
#'
#' The UI portion of the module_all_sr_curves_ui module
#'
#' @importFrom shiny NS tagList fluidRow column actionButton tags
#' @importFrom shinydashboard box
#'
#' @param id The id for this module
#'
#' @return a tagList containing UI elements
#'
module_all_sr_curves_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$div(
      style = "margin: auto; width: 90%; padding-bottom: 5px; padding-top: 0px;",
      actionButton(ns("open_all_sr_modal"), label = "preview all SR curves", class = "deselect-button")
    )
  )
  
}



#' Display all stressor-response curves in a single modal - Server Function
#'
#' The Server portion of the module huc results module
#'
#' @importFrom shiny reactive reactiveVal observeEvent req callModule eventReactive
#' @importFrom dplyr tbl collect mutate arrange select filter pull
#' @importFrom shinyFeedback showFeedbackDanger hideFeedback showToast
#'
#' @param none
#'
#' @return None
#'
module_all_sr_curves_server <- function(id) {
  moduleServer(id,
               function(input, output, session) {
                 ns <- session$ns
                 
                 
                 #-------------------------------------------------------
                 # Make Stressor-Response Plots for each variable
                 # of interest
                 #-------------------------------------------------------
                 # If one watershed selected return name and description
                 # If multiple watersheds selected then show number selected and provide instruction to user
                 output$text_preview <- renderUI({
                   selected_raw <- rv_clickedIds$ids
                   tl <- NULL
                 }
                 )
                 
                 
                 observeEvent(input$open_all_sr_modal, {
                   print("click.. open_all_sr_modal")
                   showModal(modalDialog(title = "Stressor-Response Curves",
                                         tagList(
                                           tags$p("The following figures show stressor response (dose:response) curves for each variable included in the input dataset. To view the cumulative system capacity, click on a variable of interest from the Stressor Variables layer panel on the left-hand side of the screen. To modify a stressor response relationship, click on the small icon to the left of the label."),
                                           uiOutput(ns("sr_all_preview")),
                                           
                                         )
                                         
                                         
                                         
                                         ))
                 })
                 
                 
               })
}
                 