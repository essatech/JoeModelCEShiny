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
      actionButton(
        ns("open_all_sr_modal"),
        label = "preview all SR curves",
        class = "deselect-button"
      )
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
                 # of interest with dygraph
                 #-------------------------------------------------------
                 output$distPlot <- renderUI({
                   
                   print("Plotting all SR curves...")
                   # Trigger with time
                   retrigger <- session$userData$rv_timer$time
                   
                   # Get main sheet and split stressors for adult and non adult
                   # Only show joe model stressors here
                   ms <- session$userData$rv_stressor_response$main_sheet
                   
                   ms_joe_names <- unique(ms$Stressors)

                   # Loop through Joe Model Stressors
                   dygraph_list <- list()

                   for (pp in 1:length(ms_joe_names)) {
                     this_var <- ms_joe_names[pp]
                     
                     # Plot for each stressor
                     pdat <-
                       session$userData$rv_stressor_response$sr_dat[[this_var]]
                     
                     plot_title <- this_var
                     plot_title <- gsub("_", " ", plot_title)
                     
                     # print(paste0(this_var, " this_var in renderDygraph()"))
                     # Get all SR data
                     table_vals <- pdat
                     table_vals$lwr_sd <-
                       table_vals$mean_system_capacity - table_vals$sd
                     table_vals$upr_sd <-
                       table_vals$mean_system_capacity + table_vals$sd
                     table_vals <-
                       table_vals[order(table_vals$mean_system_capacity), ]
                     table_vals <-
                       table_vals[order(table_vals$value), ]
                     
                     # Fix lower and upper sd bounds to be within range of limits
                     table_vals$lwr_sd <-
                       ifelse(table_vals$lwr_sd < table_vals$lwr,
                              table_vals$lwr,
                              table_vals$lwr_sd)
                     table_vals$upr_sd <-
                       ifelse(table_vals$upr_sd > table_vals$upr,
                              table_vals$upr,
                              table_vals$upr_sd)
                     
                     # Ensure no bad values
                     table_vals <-
                       table_vals[, c("value",
                                      "mean_system_capacity",
                                      "lwr",
                                      "upr",
                                      "lwr_sd",
                                      "upr_sd")]
                     
                     # Pretty label for plot
                     pretty_lab <-
                       gsub("_",
                            " ",
                            paste0("", this_var))
                     
                     # X-axis mouse-over formatting
                     myvFormatter <- "function formatValue(v) {
              return '';
        }"
                     
                     # Start and return the dygraph plot
                     this_dygraph <-
                       dygraph(
                         table_vals,
                         main = pretty_lab,
                         width = 260,
                         height = 220
                       ) %>%
                       dyAxis("x",
                              label = "Raw Stressor Values",
                              valueFormatter = JS(myvFormatter)) %>%
                       dyAxis("y",
                              label = "Mean System Capacity (%)",
                              valueFormatter = JS(myvFormatter)) %>%
                       dySeries(
                         c("lwr", "mean_system_capacity", "upr"),
                         label = "msc",
                         color = "grey"
                       ) %>%
                       dySeries(
                         c("lwr_sd", "mean_system_capacity", "upr_sd"),
                         label = "Mean Sys. Cap.",
                         color = "red"
                       ) %>%
                       dyCSS(
                         textConnection(
                           ".dygraph-title {
                           font-size: 14px;
                         }
                         .dygraph-legend {
                         display: none;
                         }
                         .dygraph-label {
                          font-size: 14px;
                         }
                         .dygraph-axis-label {
                           font-size: 12px;
                         }
                         .dygraph-xlabel {
                         padding-left: 5px;
                         padding-right: 5px;
                         }
                         .dygraph-xlabel {
                         padding-bottom: 10px;
                         }
                         "
                         )
                       )
                     
                     dygraph_list[[pp]] <- this_dygraph
                     
                   } # end of loop through plots
                   
                   
    
                   res <- htmltools::tagList(dygraph_list)
                   
                   return(res)
                   
                 })
                 
                 
                 
                 #-------------------------------------------------------
                 # html text for the pop-up modal
                 #-------------------------------------------------------
                 
                 observeEvent(input$open_all_sr_modal, {
                   print("click.. open_all_sr_modal")
                   
                   # By default shiny will cache the dygraphs
                   # This causes some issues with the css
                   # but we need them to rebuild every time
                   session$userData$rv_timer$time <- Sys.time()
                   
                   
                   showModal(modalDialog(
                     title = "Stressor-Response Curves",
                     size = "l",
                     easyClose = TRUE,
                     tagList(
                       tags$p(
                         "The following figures show stressor response (dose:response) curves for each variable included in the input dataset. To view the cumulative system capacity, click on a variable of interest from the Stressor Variables layer panel on the left-hand side of the screen. To modify a stressor response relationship, click on the small icon to the left of the label."
                       ),
                       #htmlOutput(ns("distPlot"), style = "display: flex; flex-wrap: wrap;"),
                       uiOutput(ns("distPlot"), style = "display: flex; flex-wrap: wrap;"),
                       
                     )
                   ))
                 })
                 
               })
}
