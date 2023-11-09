#' Matrix Model Preview Stressors UI
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
module_matrix_model_preview_stress_ui <- function(id) {
  ns <- NS(id)
  
  tagList(tags$div(fluidRow(
    column(
      width = 2,
      style = "text-align: center; vertical-align: middle;",
      checkboxInput(
        ns("pm_ps_check"),
        label = NULL,
        value = FALSE,
        width = "100px"
      )
    ),
    column(
      width = 10,
      shinydashboardPlus::accordion(
        id = ns("accordion1"),
        accordionItem(
          title = htmlOutput(ns('pm_ps_label')),
          color = "danger",
          collapsed = TRUE,
          tagList(
            numericInput(ns("pm_ps_val_mean"), label = "Mean Value", value = NULL),
            numericInput(ns("pm_ps_val_sd"), label = "SD", value = NULL),
            numericInput(ns("pm_ps_val_lwr"), label = "Lower Limit", value = NULL),
            numericInput(ns("pm_ps_val_upr"), label = "Upper Limit", value = NULL)
          ),
        )
      )
    )
  )))
  
}


#' Matrix Model preview Stressors SERVER
#'
#' @param none
#'
#' @return None
#'
module_matrix_model_preview_stress_server <-
  function(id, stressor_variable = NA) {
    moduleServer(id,
                 function(input, output, session) {
                   ns <- session$ns
                   print("module_matrix_model_preview_stress_server...")
                   print(stressor_variable)
                   
                   
                   simp_num <- function(value) {
                     if(is.null(value)) {
                       return("Cannot be blank")
                     }
                     if(is.na(value)) {
                       return("Cannot be blank")
                     }
                     if(class(value) != "numeric" & class(value) != "integer") {
                       return("Must be a number")
                     }
                   }
                   iv <- InputValidator$new()
                   iv$add_rule("pm_ps_val_mean", simp_num)
                   iv$add_rule("pm_ps_val_sd", simp_num)
                   iv$add_rule("pm_ps_val_lwr", simp_num)
                   iv$add_rule("pm_ps_val_upr", simp_num)
                   
                   
                   # Turn validation on and off
                   observeEvent(input$pm_ps_check, {
                     #print(input$pm_ps_check)
                     if(input$pm_ps_check) {
                       # only show red errors if checked
                       iv$enable()
                     } else {
                       # Leave blank
                       iv$disable()
                     }
                   }, ignoreInit = TRUE)
                   
                   
                   # Set the label
                   output$pm_ps_label <- renderUI({
                     
                     #print("Variable Label")
                     param_note <-
                       paste0(stressor_variable$Life_stages,
                              ": ",
                              stressor_variable$Parameters)
                     
                     label <-
                       gsub("_", " ", stressor_variable$Stressors)
                     
                     tagList(
                       tags$div(label, style = "font-weight: bold;"),
                       tags$div(param_note, class = "pm-ht")
                     )
                     
                   })
                   
                   
                   # Listen to change in values
                   observe({
                     
                     req(input$pm_ps_val_mean)
                     req(input$pm_ps_val_sd)
                     req(input$pm_ps_val_lwr)
                     req(input$pm_ps_val_upr)
                     
                     print(input$pm_ps_check)
                     
                     
                     isolate({
                       
                       # Update values in reference object
                       this_stressor <- stressor_variable$Stressors
                       
                       session$userData$rv_sandbox_stressors$dat$Mean[which(session$userData$rv_sandbox_stressors$dat$Stressors == this_stressor)] <- input$pm_ps_val_mean
                       session$userData$rv_sandbox_stressors$dat$SD[which(session$userData$rv_sandbox_stressors$dat$Stressors == this_stressor)] <- input$pm_ps_val_sd
                       session$userData$rv_sandbox_stressors$dat$Low_Limit[which(session$userData$rv_sandbox_stressors$dat$Stressors == this_stressor)] <- input$pm_ps_val_lwr
                       session$userData$rv_sandbox_stressors$dat$Up_Limit[which(session$userData$rv_sandbox_stressors$dat$Stressors == this_stressor)] <- input$pm_ps_val_upr
                       
                       # Checkbox status
                       session$userData$rv_sandbox_stressors$dat$check_on[which(session$userData$rv_sandbox_stressors$dat$Stressors == this_stressor)] <- input$pm_ps_check
                       
                       #print(as.data.frame(session$userData$rv_sandbox_stressors$dat))
                       
                     })
                     
                     
                   })
                   
                 })
  }