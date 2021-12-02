#' Joe Model Run Form Modal UI
#'
#' Define parameters and run the Joe Model
#' 
#' @param id The id for this module
#'
#' @return a tagList containing UI elements
#'
module_joe_model_run_ui <- function(id) {
  
  ns <- NS(id)
  # Single action button to call modal
  actionButton(ns("open_joe_modal_form"),
                  tags$b("Joe Model"),
                  class="chart-line",
                  width = "100%")

}



#' Joe Model Run Form Modal Server
#'
#' Server and modal content for the Joe model server
#'
#' @param none
#'
#' @return None
#'
module_joe_model_run_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      
      print("Calling module_joe_model_run_server")
      

      #-------------------------------------------------------
      # DISABLE AND ENABLE 
      #------------------------------------------------------- 
      # this modal is never disabled ...




      #-------------------------------------------------------
      # START OF INPUT MODAL UI
      #-------------------------------------------------------      
      # Open the the Joe Model form
      observeEvent(input$open_joe_modal_form, {
        print("Joe model form click to open ...")

        # Gather a list of all the stessors to build the checkbox list


        showModal(modalDialog(
          title = "Run the Joe Model",
          tagList(

            # Dynamic UI for list of checkbox elements
            fluidRow(
              shinydashboard::box(
                width = 12,
                checkboxGroupInput(ns("check_box_group"),
                label = "(Optional) Subset Variables:",
                choices = c(),
                selected = c(),
                inline = TRUE),
              )
            ),

            fluidRow(
              shinydashboard::box(
                width = 12,
                numericInput(ns("number_of_simulations"), "Number of Simulations", MC.sims, min = 1, max = 1000),
                textInput(ns("name_of_simulation"), "Name of this Simulation (optional)", "Default"),
                uiOutput(ns("text_time_estimate"))
              )
            ),

            fluidRow(
              column(
                width = 12,
                actionButton(ns("go_button_run_joe"), "Run the Joe Model", class = "btn-success", style = "color: white;")
              )
            )

          ),
          easyClose = TRUE,
          size = 'l',
          footer = NULL
        ))
      }) # END OF INPUT MODAL UI
      #-------------------------------------------------------


      #-------------------------------------------------------
      # List of check box opportunities
      #-------------------------------------------------------
      observe({
        print("Building the check box group...")
        req(rv_stressor_response$stressor_names)
        req(input$open_joe_modal_form)
        stressors <- rv_stressor_response$stressor_names

        if(is.null(stressors)) {
            updateCheckboxGroupInput(session,
            "check_box_group",
            choices = c(),
            selected = c(),
            inline = TRUE)
        } else {
            print("Checkboxes will be ..")
            print(stressors)

            updateCheckboxGroupInput(session,
            "check_box_group",
            choices = stressors,
            selected = stressors,
            inline = TRUE)
        }

      })


      #-------------------------------------------------------
      # Update time estimate text
      #-------------------------------------------------------
      # Time estimate would presumably be a produce of the number of sims, number of hucs and number of stressors
      output$text_time_estimate <- renderUI({
          
          dat <- rv_stressor_magnitude$sm_dat

          n_hucs <- length(unique(dat$HUC_ID))
          n_stressor <- length(unique(dat$Stressor))
          n_sims <- input$number_of_simulations
        
          tl <- tagList(
                tags$p(paste0("Review: (n) HUCs: ", n_hucs, " (n) Stressors: ", n_stressor, " (n) replicates: ", n_sims)),
                tags$p("Warning: the run time estimate for the model run is: ## seconds")
          )
          
        return(tl)
      })



      #-------------------------------------------------------
      # Run the Joe Model and Store Results
      #-------------------------------------------------------
      # Run the Joe model and store the results
      observeEvent(input$go_button_run_joe, {

          # Gather the inputs
          # Stressor RESPONSE workbook data (reactive value)
          print("Gathering Joe Inputs...")
          sr_wb_dat_in <- list()
          sr_wb_dat_in$main_sheet     <- rv_stressor_response$main_sheet
          sr_wb_dat_in$stressor_names <- rv_stressor_response$stressor_names
          sr_wb_dat_in$sr_dat <- rv_stressor_response$sr_dat

          # Stressor MAGNITUDE workbook data (reactive value)
          sm_wb_dat_in <- rv_stressor_magnitude$sm_dat
          # Number of Monte Carlo sims
          n_mc_sims <- input$number_of_simulations

          # For partial model filter out non-target variables from respective datasets
          print("Filter out for partial model...")
          selected_variables <- input$check_box_group
          # Filter main sheet
          sr_wb_dat_in$main_sheet <- sr_wb_dat_in$main_sheet[which(sr_wb_dat$main_sheet$Stressors %in% selected_variables), ]
          # Stressor names
          sr_wb_dat_in$stressor_names <- sr_wb_dat_in$stressor_names[which(sr_wb_dat_in$stressor_names %in% selected_variables)]
          # Dose response relationships
          name_ind <- which(names(sr_wb_dat_in$sr_dat) %in% c(selected_variables))
          sr_wb_dat_in$sr_dat <- sr_wb_dat_in$sr_dat[name_ind]
          print(names(sr_wb_dat_in$sr_dat))
          # End of partial model filters
          
          print("Running the Joe Model...")
          # Try running the Joe model
          jm <- JoeModel_Run(
              dose = sm_wb_dat_in,
              sr_wb_dat = sr_wb_dat_in,
              MC.sims = n_mc_sims
          )

          print("Finished the Joe Model run...")
          # Store the scenario in the list object - index + 1 to prevent overwrite
          simulation_index <- length(rv_joe_model_results$sims) + 1
          
          # Store the Joe Model results in this list object
          rv_joe_model_results$sims[[simulation_index]] <- jm

          # Also store the name of the simulation (if set by user)
          sim_name <- input$name_of_simulation
          rv_joe_model_sim_names$scenario_names[[simulation_index]] <- sim_name

          # Close the modal
          removeModal()

      })



      
   
    }
  )
}