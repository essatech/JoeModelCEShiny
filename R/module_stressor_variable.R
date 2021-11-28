#' stressor_variable UI
#'
#' The UI portion of the main map module
#'
#' @param id The id for this module
#'
#' @return a tagList containing UI elements
#'
module_stressor_variable_ui <- function(id) {
  
  ns <- NS(id)
  
  tags$div(
    class = "stack-box", id = ns("var_id"),
    
    shinydashboard::box(
      width = 12,
      background = "light-blue",
      
      htmlOutput(ns('variable_label')),
      htmlOutput(ns('variable_val_raw')),
      
      tags$div(actionButton(ns("response_plot"), icon("cog"), class = "response-button"),
               style = "float: right; display: inline-block;"),
      
      tags$p("[0.45%]", style = "float: right;"),
      
      tags$div(numericInput(ns("hiddenload"), label = "hidden", value = 0), style = "display:none;")
    ) # end of box
  )   # end of div
  
}


#' stressor_variable Server
#'
#' The Server portion of the stressor_variable module
#'
#' @param mlabel Character. Label of the variable
#' 
#'
#' @return None
#'
module_stressor_variable_server <- function(id, stressor_index = NA) {
  moduleServer(
    id,
    function(input, output, session) {
     
      print("Inside module_stressor_variable_server")
      print(stressor_index)
      
     # Set the label
     output$variable_label <- renderUI({
       #print("Variable Label")
       label <- rv_stressor_response$pretty_names[stressor_index]
       label <- paste0(label, ":  ")
       tags$p(label, style = "float: left;")
      })
     
     # Change mouse-over raw value
     output$variable_val_raw <- renderUI({
       sname <- rv_stressor_response$stressor_names[stressor_index]
       if(is.null(rv_stressor_response$active_values_raw)) {
         tags$p(" ", style = "float: left; display:inline;")
       } else {
         raw_vals <- rv_stressor_response$active_values_raw$Mean
         names <- rv_stressor_response$active_values_raw$Stressor
         target_val <- raw_vals[which(names == sname)]
         target_val <- ifelse(is.na(target_val), " ", target_val)
         tags$p(target_val, style = "float: left; display:inline;")
       }

     })
     

     # ------------------------------------------------------
     # Set selected class as selected variable
     # Should it be styled as selected or light blue un-selected
     # ------------------------------------------------------
       observe({
         req(input$hiddenload)
         #print("Selected Variable")
         
         # Set the stressor response object as a reactive value
         if(!(is.na(stressor_index))) {
           
           active <- rv_stressor_response$active_layer
           current <- rv_stressor_response$stressor_names[stressor_index]
           
           if(!(is.na(current)) & !(is.na(active))) {
             if(active == current) {
               #print("Adding class")
               q_code <- paste0("jQuery('#main_map-", current, "-var_id').addClass('var-selected');")
               shinyjs::runjs(code = q_code)
             } else {
               q_code <- paste0("jQuery('#main_map-", current, "-var_id').removeClass('var-selected');")
               shinyjs::runjs(code = q_code)
             }
           }
         }
       })
       
       
       # ---------------------------------------------------------
       # Listen to click events to change target variable selected
       observe({
         # ensure UI is loaded - do not run if not set
         req(input$hiddenload)
         # User clicks on ID
         current <- rv_stressor_response$stressor_names[stressor_index]
         # Update reactive value of target variable selected
         updateActiveVar <- function(current) {
           #print("User Click")
           #print(current)
           rv_stressor_response$active_layer <- current
           #print(rv_stressor_response$active_layer)
         }
         # Use mouse click
         my_id <- paste0("main_map-", current, "-var_id")
         onclick(my_id,
                 updateActiveVar(current),
                 asis = TRUE)
       })
       
       
       
       
       
       
       # Open the stressor response dialog box
       observeEvent(input$response_plot, {
         showModal(modalDialog(
           title = "Stressor-Response Relationships: Temperature",
           tagList(
             tags$p("Use the table below to edit the stressor-response (dose-response) relationship for stressor: Temperature. Units are in X."),
             tags$p("Raw Value | Mean System Capacity | SD | Lower Limit | Upper Limit"),
             
             fluidRow(
               column(
                 width = 6,
                 actionButton("goButton3", "Save and Update", class = "btn-success", style = "color: white;")
                 
               )
             )
             
           ),
           easyClose = TRUE,
           size = 'l',
           footer = NULL
         ))
       })
       
       

     
    }
  )
}