#' Module HUC Results Summary Section
#'
#' The UI portion of the huc_results module
#'
#' @importFrom shiny NS tagList fluidRow column actionButton tags
#' @importFrom shinydashboard box
#'
#' @param id The id for this module
#'
#' @return a tagList containing UI elements
#'
module_huc_results_ui <- function(id) {
  ns <- NS(id)
  
  tagList(shinydashboard::box(
    width = 12,
    
    tags$div(
      style = "display: flex;",
      tags$h5(textOutput(ns(
        "n_watersheds_selected"
      )), class = "dy-accent"),
      actionButton(ns("deselect_watersheds"), "deselect all", class = "deselect-button"),
    ),
    
    fluidRow(
      infoBox(
        title = NULL,
        color = 'blue',
        value = module_huc_stressor_magnitude_ui(ns("stressor_magnitude")),
        icon = icon("sliders-h"),
        subtitle = "Modify the stressor magnitude for selected watersheds"
      ),
      
      
      infoBox(
        title = NULL,
        color = 'blue',
        value = module_joe_model_run_ui(ns("run_joe_model")),
        icon = icon("chart-bar"),
        subtitle = "Run the Cumulative Effects Joe Model"
      ),
      
      infoBox(
        title = NULL,
        color = 'blue',
        value = module_joe_vs_population_ui(ns("module_joe_vs_population")),
        icon = icon("chart-line"),
        subtitle = "Run the population model for selected watersheds"
      ),
      
      
    ),
    
    
    
    
    div(id = ns("sys_cap_buttons_all"),
        
        fluidRow(tags$h4("System Capacity Plots")),
        
        fluidRow(
          
          
          #infoBox(
          #  title = NULL,
          #  color = 'blue',
          #  value =
          #    actionButton(
          #      ns("scp_by_stressors"),
          #      tags$b("By Stressors"),
          #      class = "chart-line",
          #      width = "100%"
          #    ),
          #  icon = icon("sliders-h"),
          #  subtitle = "Plot the system capacity across stressors for selected watersheds"
          #),
          
          
          infoBox(
            title = NULL,
            color = 'blue',
            value = module_joe_model_csc_plots_selected_ui(ns("open_joe_modal_csc_plots_selected")),
            icon = icon("sliders-h"),
            subtitle = "Plot the cumulative system capacity for selected watersheds"
          ),
          
          
          infoBox(
            title = NULL,
            color = 'blue',
            value = module_joe_model_csc_plots_ui(ns("joe_model_csc_plots_all")),
            icon = icon("sliders-h"),
            subtitle = "Plot the cumulative system capacity for all watersheds"
          ),
          
        ),
        ), 

    
  ))
  
}


#' HUC Result Module Server
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
module_huc_results_server <- function(id) {
  moduleServer(id,
               function(input, output, session) {
                 ns <- session$ns
                 
                 print("Calling module_huc_results_server")
                 
                 # Call the sub-modules
                 module_huc_stressor_magnitude_server("stressor_magnitude")
                 module_joe_model_run_server("run_joe_model")
                 module_joe_model_csc_plots_server("joe_model_csc_plots_all")
                 module_joe_model_csc_plots_selected_server("open_joe_modal_csc_plots_selected")
                 module_joe_vs_population_server("module_joe_vs_population")
                 
                 # Hide deselect HUC deselect button on initial load
                 # Also assume no watersheds are selected
                 q_code <-
                   paste0(
                     "jQuery('#main_map-huc_results-deselect_watersheds').addClass('hide-this');"
                   )
                 shinyjs::runjs(code = q_code)
                 #shinyjs::disable("adjust_stressor_magnitude")
                 #shinyjs::disable("run_ce_population_model")
                 shinyjs::disable("scp_by_stressors")
                 shinyjs::disable("scp_for_selected_sheds")
                 
            
                 
                 # Show count the number of selected HUCs (e.g., 3 U)
                 output$n_watersheds_selected <- renderText({
                   hus_selected <- session$userData$rv_clickedIds$ids
                   print(hus_selected)
                   if (length(hus_selected) == 0) {
                     return("0 HUCs selected - click on a unit on the map to select")
                     # Hide the deselect button
                     q_code <-
                       paste0(
                         "jQuery('#main_map-huc_results-deselect_watersheds').addClass('hide-this');"
                       )
                     shinyjs::runjs(code = q_code)
                     # Enable other buttons dependant on selected HUCs
                     #shinyjs::disable("adjust_stressor_magnitude")
                     #shinyjs::disable("run_ce_population_model")
                     shinyjs::disable("scp_by_stressors")
                     shinyjs::disable("scp_for_selected_sheds")
                   } else {
                     nhuc <- length(hus_selected)
                     # Show the deselect button
                     q_code <-
                       paste0(
                         "jQuery('#main_map-huc_results-deselect_watersheds').removeClass('hide-this');"
                       )
                     shinyjs::runjs(code = q_code)
                     # Enable other buttons dependant on selected HUCs
                     #shinyjs::enable("adjust_stressor_magnitude")
                     #shinyjs::enable("run_ce_population_model")
                     shinyjs::enable("scp_by_stressors")
                     shinyjs::enable("scp_for_selected_sheds")
                     if (nhuc == 1) {
                       return(paste0(nhuc, " HUC selected"))
                     } else {
                       return(paste0(nhuc, " HUCs selected"))
                     }
                   }
                 })
                 
                 # Deselect any selected watersheds - clear selection
                 observeEvent(input$deselect_watersheds, {
                   session$userData$rv_clickedIds$ids <- vector() # Set to empty vector
                   q_code <-
                     paste0(
                       "jQuery('#main_map-huc_results-deselect_watersheds').addClass('hide-this');"
                     )
                   shinyjs::runjs(code = q_code)
                   #shinyjs::disable("adjust_stressor_magnitude")
                   #shinyjs::disable("run_ce_population_model")
                   shinyjs::disable("scp_by_stressors")
                   shinyjs::disable("scp_for_selected_sheds")
                   # Redraw layer and clear selection
                   session$userData$rv_redraw$redraw <- 1 + session$userData$rv_redraw$redraw
                 })
                 
                 
                 # Show or hide the csc result buttons
                 observe({
                   print("Joe model has...")
                   sim_index <- session$userData$rv_joe_model_results$sims
                   if(length(sim_index) < 1) {
                     print("...not been run (hide buttons)")
                     shinyjs::hide("sys_cap_buttons_all")
                   } else {
                     print("...has been run (show buttons)")
                     shinyjs::show("sys_cap_buttons_all")
                   }
                 })
                 

               })
}