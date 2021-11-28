#' Module HUC Results Summary
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
  
  tagList(
    shinydashboard::box(
      width = 12,
      #htmlOutput(ns('count_huc_select')),
      tags$h4("X watersheds selected"),
      
      
      fluidRow(
        
        infoBox(title = NULL, color = 'blue', 
                value = 
                  actionButton(ns("adjust_stressor_magnitude"),
                               tags$b("Adjust Magnitude"),
                               class="chart-line",
                               width = "100%"),
                icon = icon("sliders-h"),
                subtitle = "Modify the stressor magnitude for selected watersheds"),
        
        
        infoBox(title = NULL, color = 'blue', 
                value = 
                  actionButton(ns("run_ce_joe_model"),
                               tags$b("Joe Model"),
                               class="chart-line",
                               width = "100%"),
                icon = icon("chart-bar"),
                subtitle = "Run the Cumulative Effects Joe Model"),
        
        
        infoBox(title = NULL, color = 'blue', 
                value = 
                  actionButton(ns("run_ce_population_model"),
                               tags$b("Population Model"),
                               class="chart-line",
                               width = "100%"),
                icon = icon("chart-line"),
                subtitle = "Run the population model for selected watersheds"),
      ),
      
      fluidRow(
        tags$h4("System Capacity Plots"),
      ),
      
      fluidRow(
        
        infoBox(title = NULL, color = 'blue', 
                value = 
                  actionButton("count2",
                               tags$b("By Stressors"),
                               class="chart-line",
                               width = "100%"),
                icon = icon("sliders-h"),
                subtitle = "Plot the system capacity across stressors for selected watersheds"),
        
        
        infoBox(title = NULL, color = 'blue', 
                value = 
                  actionButton("count2",
                               tags$b("Selected Watershed"),
                               class="chart-line",
                               width = "100%"),
                icon = icon("sliders-h"),
                subtitle = "Plot the cumulative system capacity for selected watersheds"),
        
        
        infoBox(title = NULL, color = 'blue', 
                value = 
                  actionButton("count2",
                               tags$b("All Watershed"),
                               class="chart-line",
                               width = "100%"),
                icon = icon("sliders-h"),
                subtitle = "Plot the cumulative system capacity for all watersheds"),
        
      ),
      
      
      
    )
    
  )
  
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
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      print("Calling module_huc_results_server")
      
      #output$count_huc_select <- renderText({ 
        # Count the numbr of selected HUCs
        #hus_selected <- clickedIds$ids
        #print(hus_selected)
        
        #if(length(hus_selected) == 0) {
        #return(tags$h4("0 HUCs selected"))
          
        #} else {
        #  nhuc <- length(hus_selected)
        #  tags$h4(paste0(nhuc, " HUCs selected"))
        #  
        #}
      #})
      
      
      
      observeEvent(input$adjust_stressor_magnitude, {
        showModal(modalDialog(
          title = "Adjust Stressor Magnitude",
          tagList(
            tags$p("Adjust Stressor Magnitude For Selected Watersheds"),
            
            fluidRow(
              shinydashboard::box(
                numericInput("obs", "Variable A:", 10, min = 1, max = 100),
                numericInput("obs", "Variable B:", 10, min = 1, max = 100),
                numericInput("obs", "Variable C:", 10, min = 1, max = 100),
                numericInput("obs", "Variable D:", 10, min = 1, max = 100),
                numericInput("obs", "Variable E:", 10, min = 1, max = 100),
                numericInput("obs", "Variable F:", 10, min = 1, max = 100),
                numericInput("obs", "Variable G:", 10, min = 1, max = 100)
              ), 
            ),
            fluidRow(
              column(
                width = 6,
                actionButton("goButton", "Update Values", class = "btn-success", style = "color: white;")
                
              )
            )
            
          ),
          easyClose = TRUE,
          size = 'l',
          footer = NULL
        ))
      })
      
      
      
      
      
      observeEvent(input$run_ce_joe_model, {
        showModal(modalDialog(
          title = "Run the Cumulative Effect Joe Model",
          tagList(
            tags$p("Run the Cumulative Effect Joe Model For All Watersheds"),
            
            fluidRow(
              shinydashboard::box(
                radioButtons("dist", "Model Type:",
                             c("Full Model" = "norm",
                               "Partial Model" = "unif")),
                numericInput("obs", "Number of Simulations:", 10, min = 1, max = 100),
                numericInput("obs", "Name of Scenario to run:", 10, min = 1, max = 100),
                textInput("caption", "Name of Scenario to run:", ""),
              ), 
            ),
            fluidRow(
              column(
                width = 6,
                actionButton("goButton2", "Run Joe Model", class = "btn-success", style = "color: white;")
                
              )
            )
            
          ),
          easyClose = TRUE,
          size = 'm',
          footer = NULL
        ))
      })
      
      
      
      
      observeEvent(input$run_ce_population_model, {
        showModal(modalDialog(
          title = "Run the Population Model",
          tagList(
            tags$p("Run the population model with cumulative effects for selected watersheds"),
            
            fluidRow(
              column(
                width = 6,
                actionButton("goButton3", "Run Population Model", class = "btn-success", style = "color: white;")
                
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