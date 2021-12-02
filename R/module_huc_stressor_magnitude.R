#' HUC Stressor Magnitude Adjustment Modal Module
#'
#' Adjust the stressor magnitude values for one to many selected HUCs
#' 
#' @importFrom shiny NS tagList fluidRow column actionButton tags
#' @importFrom shinydashboard box
#'
#' @param id The id for this module
#'
#' @return a tagList containing UI elements
#'
module_huc_stressor_magnitude_ui <- function(id) {
  
  ns <- NS(id)
  # Single action button to call modal
  actionButton(ns("adjust_stressor_magnitude"),
                  tags$b("Adjust Magnitude"),
                  class="chart-line",
                  width = "100%")

}




#' HUC Stressor Magnitude Adjustment Modal Module
#'
#' Adjust the stressor magnitude values for one to many selected HUCs
#'
#' @importFrom shiny reactive reactiveVal observeEvent req callModule eventReactive
#' @importFrom dplyr tbl collect mutate arrange select filter pull
#' @importFrom shinyFeedback showFeedbackDanger hideFeedback showToast
#'
#' @param none
#'
#' @return None
#'
module_huc_stressor_magnitude_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      
      print("Calling module_huc_results_server")
      
      # Hide deselect HUC button on initial load
      #q_code <- paste0("jQuery('#main_map-huc_results-deselect_watersheds').addClass('hide-this');")
      #shinyjs::runjs(code = q_code)
      #shinyjs::disable("adjust_stressor_magnitude")


      #-------------------------------------------------------
      # DISABLE AND ENABLE 
      #------------------------------------------------------- 
      # disable button unless one HUC is selected 
      observe({
        click_hucs <- rv_clickedIds$ids
        if(length(click_hucs) > 0) {
          shinyjs::enable("adjust_stressor_magnitude")
        } else {
          shinyjs::disable("adjust_stressor_magnitude")
        }
      })



      #-------------------------------------------------------
      # START OF INPUT MODAL UI
      #-------------------------------------------------------      
      # Change the values of the underlying variables for selected HUCs
      observeEvent(input$adjust_stressor_magnitude, {
        print("Stressor magnitude modal is open ...")

        showModal(modalDialog(
          title = "Adjust Stressor Magnitude",
          tagList(
            uiOutput(ns("text_preview")),

            fluidRow(
              shinydashboard::box(
                width = 12,
                DTOutput(
                  ns("stressor_inputs")
                )
              )
            ),
            
            fluidRow(
              column(
                width = 12,
                actionButton("goButton", "Update Values", class = "btn-success", style = "color: white;")
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
      # Populate modal text based on selection
      #-------------------------------------------------------
      # If one watershed selected return name and description
      # If multiple watersheds selected then show number selected and provide instruction to user
      output$text_preview <- renderUI({
          selected_raw <- rv_clickedIds$ids
          tl <- NULL
          # Single Selection 
          if(length(selected_raw) == 1) {
             id   <- strsplit(selected_raw, "\\|")[[1]][1]
             name <- strsplit(selected_raw, "\\|")[[1]][2]
             tl <- tagList(
               tags$h3(name, style = "color: #103e85;"),
               tags$h4(paste0("HUC ID: ", id), style = "color: #0073b7;"),
               tags$p("Click on cells in the table below to update the stressor magnitude for the selected unit. Adjust the mean value for each stressor (Mean), the standard deviation (SD), the distribution type (options are: “normal”), the lower limit and upper limit (for stochastic simulations)."),
               tags$div(
                 class = "no-bullet",
                  shinydashboard::taskItem(value = 17, color = "red",
                      "Mean Cumulative System Capacity"
                  )
               )
            )
          }
          # Multi-HUC Selection 
          if(length(selected_raw) > 1) {
            # Fix format   
            getID <- function(x) {
              strsplit(x, "\\|")[[1]][1]
            }
            selected_ids <- lapply(selected_raw, getID) %>% unlist()
            selected_ids_txt <- paste(selected_ids, collapse = ", ")
            tl <- tagList(
               tags$h3("Multiple Units Selected", style = "color: #103e85;"),
               tags$h4(selected_ids_txt, style = "color: #0073b7;"),
               tags$p("Multiple HUCs selected. Use the table below to set stressor magnitude values for the selected HUCs. Note that entering a value in any of the cells will update the values for all the selected HUCs. Leave values blank to keep original values for each HUC."),
               tags$div(
                 class = "no-bullet",
                  shinydashboard::taskItem(value = 50, color = "green",
                      "Mean Cumulative System Capacity"
                  )
               )
            )
          }
        
        return(tl)
      })



      #-------------------------------------------------------
      # Populate modal input table
      #-------------------------------------------------------
      # Reformat data prior to making edit table
      # Big thing is deciding if editing only one HUC - in which case we
      # show current values as place holders - (or)
      # Multi-edit mode were any input value will overwrite all HUC
      output$stressor_inputs <- renderDT({

        # HUCs currently selected
        selected_raw <- rv_clickedIds$ids
        # Fix format   
        getID <- function(x) {
          strsplit(x, "\\|")[[1]][1]
        }
        selected_ids <- lapply(selected_raw, getID) %>% unlist()

        # Get values if single HUC or set as NA if multi
        if(length(selected_ids) == 1) {
          # Use render DT with proxy to avoid reload on edit...
          raw_data <- isolate(rv_stressor_magnitude$sm_dat)
          table_vals <- raw_data %>% filter(HUC_ID == selected_ids)
          table_vals <- table_vals[order(table_vals$Stressor), ]
          table_vals <- table_vals[, c("Stressor", "Mean", "SD", "Distribution", "Low_Limit", "Up_Limit")]
          table_vals$Mean <- round(table_vals$Mean, 2)
          table_vals$SD <- round(table_vals$SD, 2)
          table_vals$Low_Limit <- round(table_vals$Low_Limit, 2)
          table_vals$Up_Limit <- round(table_vals$Up_Limit, 2)
        }

        # If nothing selected then return an empty dataframe
        if(length(selected_ids) == 0) {
          table_vals <- data.frame(Stressor = NA, Mean = NA, SD = NA, Distribution = NA, Low_Limit = NA, Up_Limit = NA)
          table_vals <- table_vals[0, ]
        }

        # If multiple HUCs selected then return a dataframe for each variable
        # but keep values empty since only select will be edited
        if(length(selected_ids) > 1) {
          snames <- rv_stressor_response$stressor_names
          snames <- sort(snames)
          table_vals <- data.frame(Stressor = snames, Mean = NA, SD = NA, Distribution = NA, Low_Limit = NA, Up_Limit = NA)
        }

        # Build the JS DT Data Table Object
        DT::datatable(
          table_vals,
          # The Stressor column is not editable
          editable = list(target = "cell", disable = list(columns = c(1))),
          filter = "none",
          selection = "single",
          class = "cell-border stripe",
          options = list(
            pageLength = 500,
            info = FALSE,
            dom = 't',
            ordering = FALSE,
            columnDefs = list(list(
              className = 'dt-left', targets = "_all"
            ))
          )
        )

      })

      # Create a proxy for the above table
      dt_proxy <- DT::dataTableProxy('stressor_inputs')


      #------------------------------------------------------------------------
      # Update a data value cell
      #------------------------------------------------------------------------
      # When there is an edit to a cell
      # update the stessor magnitude reactive values 
      observeEvent(input$stressor_inputs_cell_edit, {
        
        # Get new value of edited cell
        info = input$stressor_inputs_cell_edit

        # Index list of stressor names
          snames <- sort(isolate(rv_stressor_response$stressor_names))

        # HUCs currently selected
          selected_raw <- rv_clickedIds$ids
          # Fix format   
          getID <- function(x) {
            strsplit(x, "\\|")[[1]][1]
          }
          selected_ids <- lapply(selected_raw, getID) %>% unlist()


        if(info$value == "") {
          print("Take no action")
        } else {
          if(info$value == "normal") {
          } else {
            info$value <- as.numeric(info$value)
            info$value <- ifelse(is.na(info$value), 0, info$value)
            
            i = as.numeric(info$row)
            j = as.numeric(info$col)
            k = as.numeric(info$value)

            var <- c("Stressor", "Mean", "SD", "Distribution", "Low_Limit", "Up_Limit")
                
            print(info)
            print(info$value)
            print(snames[i])
            print(var[j])
            print(selected_ids)
            print(" --------------------- ")

            # Update stressor magnitude value for HUC or selected HUCs
            # Update the reactive values
            rv_stressor_magnitude$sm_dat[
              which(rv_stressor_magnitude$sm_dat$HUC_ID %in% selected_ids & rv_stressor_magnitude$sm_dat$Stressor == snames[i]), var[j]] <- info$value
            
            # Update the DT data table so the user knows what they have just done
          }
        }

      })





      
      
   
    }
  )
}