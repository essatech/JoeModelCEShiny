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
                
                actionButton(ns("selectall"),
                             label = "Select/Deselect all"),
                
                tags$p("*Note that only variables associated with adult System Capacity can be run in the Joe Model. Other stressors linked to non-adult life stages are excluded.")
                
                
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
                tags$div(
                  class = "cta",
                  actionButton(ns("go_button_run_joe"), "Run the Joe Model", class = "btn-danger fs30px", style = "color: white;")
                )
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
      # List of check box options
      #-------------------------------------------------------
      observe({
        
        print("Building the check box group...")
        req(rv_stressor_response$stressor_names)
        req(input$open_joe_modal_form)
        
        stressors <- isolate(rv_stressor_response$stressor_names)
        
        # Exclude variables that are not associated with adults
        s_options <- isolate(rv_stressor_response$main_sheet)
        s_options <- s_options[which(s_options$Life_stages == "adult"), ]
        s_acceptable <- unique(s_options$Stressors)
        
        # Filter to exlcude any variables associated with early life stages
        stressors <- stressors[which(stressors %in% s_acceptable)]

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
      # Select and deselect all boxes
      #-------------------------------------------------------
      observe({
        print("select deletect joe model variables ...")
        req(input$selectall)
        req(input$open_joe_modal_form)
        req(rv_stressor_response$stressor_names)
        
        stressors <- rv_stressor_response$stressor_names
      
        # Exclude variables that are not associated with adults
        s_options <- rv_stressor_response$main_sheet
        s_options <- s_options[which(s_options$Life_stages == "adult"), ]
        s_acceptable <- unique(s_options$Stressors)
        
        # Filter to exlcude any variables associated with early life stages
        stressors <- stressors[which(stressors %in% s_acceptable)]
        
        if(input$selectall > 0) {
          if (input$selectall %% 2 == 0) {
            updateCheckboxGroupInput(session,
                                     "check_box_group",
                                     choices = stressors,
                                     selected = stressors,
                                     inline = TRUE)
          } else {
            updateCheckboxGroupInput(session,
                                     "check_box_group",
                                     choices = stressors,
                                     selected = "",
                                     inline = TRUE)
          }
          print(input$selectall)
        } else {
          print(input$selectall)
        
        }
      })
      
      
      #-------------------------------------------------------
      # Enable disable Joe Model Run button
      #-------------------------------------------------------
      observe({
        # req(input$check_box_group)
        req(input$open_joe_modal_form)
        req(input$number_of_simulations)
        

        if(!is.null(input$check_box_group) > 0 & input$number_of_simulations > 0) {
          shinyjs::enable("go_button_run_joe")
        } else {
          shinyjs::disable("go_button_run_joe")
        }
    
      })
      
      

      
      
      
      

      #-------------------------------------------------------
      # Update time estimate text
      #-------------------------------------------------------
      # Time estimate would presumably be a produce of the number of sims, number of hucs and number of stressors
      output$text_time_estimate <- renderUI({
        
          print("joe model time estimate...")
        
          dat <- rv_stressor_magnitude$sm_dat

          n_hucs <- length(unique(dat$HUC_ID))
          n_stressor <- length(unique(input$check_box_group))
          n_stressor <- ifelse(length(n_stressor) == 0, 0, n_stressor)
          n_stressor <- ifelse(is.na(n_stressor), 0, n_stressor)
          n_sims <- input$number_of_simulations
          n_sims <- ifelse(is.na(n_sims), 0, n_sims)
          
          
          # Calculate the predicted model run time in seconds
          # Custom estimate...
          pred_time <- 8.609907e-02 + n_hucs*3.478398e-04 + -4.307692e-02*n_stressor + 2.243825e-04*n_sims +
            n_hucs*n_sims*5.485097e-04 + n_hucs*n_stressor*2.085363e-03 +  n_stressor*n_sims*-2.982730e-04 +
            n_hucs*n_stressor*n_sims*2.615058e-05
          
          # Place holder for Joe Model estimated run times
          rv_joe_model_run_time$run_time_seconds <-  pred_time
        
          tl <- tagList(
                tags$p(paste0("Review: (n) HUCs: ", n_hucs, " (n) Stressors: ", n_stressor, " (n) replicates: ", n_sims)),
                tags$p(paste0("The run time estimate for the model run is: ", pretty_print_seconds(pred_time)))
          )
          
        return(tl)
      })



      #-------------------------------------------------------
      # Run the Joe Model and Store Results
      #-------------------------------------------------------
      # Run the Joe model and store the results
      observeEvent(input$go_button_run_joe, {
        
          # Get the estimated run time 
          e_run_time <- pretty_print_seconds(rv_joe_model_run_time$run_time_seconds)

          # Show a loading spinner to the user
          show_modal_spinner(
            spin = "hollow-dots",
            color = "#0073b7",
            text = paste0("Running the Joe Model.. The estimated runtime is ", e_run_time)
          )

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
          
          # Filter the stressor magnitude too
          sm_wb_dat_in <- sm_wb_dat_in[which(sm_wb_dat_in$Stressor %in% selected_variables), ]
          
          print("Running the Joe Model...")

          # Try running the Joe model
          jm <- JoeModelCE::JoeModel_Run(
              dose = sm_wb_dat_in,
              sr_wb_dat = sr_wb_dat_in,
              MC_sims = n_mc_sims
          )
        
          
          print("Finished the Joe Model run...")
          # Store the scenario in the list object - index + 1 to prevent overwrite
          simulation_index <- length(rv_joe_model_results$sims) + 1
          
          # Store the Joe Model results in this list object
          rv_joe_model_results$sims[[simulation_index]] <- jm

          # Also store the name of the simulation (if set by user)
          sim_name <- input$name_of_simulation
          rv_joe_model_sim_names$scenario_names[[simulation_index]] <- sim_name
          
          # Update the active layer on the map to show 
          rv_stressor_response$active_layer <- "system_capacity"
          print(rv_stressor_response$active_layer)
          
          
          # Display the system capacity varible selector on the map page
          # note that the system capacity variable selector was set to 
          # display: none to avoid switching to variable before model run.
          # From ID main_map-var_id remove CLASS hide-this
          removeClass(id = "main_map-var_id", class = "hide-this", asis = TRUE)
          
          
          # Stop the loading spinner
          remove_modal_spinner()

          # Close the modal
          removeModal()

      })



      
   
    }
  )
}