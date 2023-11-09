#' module_export_ui
#'
#' The UI portion of the export model
#'
#' @importFrom shiny NS tagList fluidRow column actionButton tags
#' @importFrom shinydashboard box
#'
#' @param id The id for this module
#'
#' @return a tagList containing UI elements
#'
module_export_ui <- function(id) {
  ns <- NS(id)
  
  tagList(shinydashboard::box(width = 12,
                              tags$h3("Export/Download Data"),
                              
                              fluidRow(
                                column(
                                  width = 6,
                                  tags$div(
                                    tags$p(
                                      "Choose from the following files to export. Note that in some scenarios it may be more convenient to download the stressor-magnitude and stressor-response Excel workbooks and then edit inputs for scenarios offline in Excel or other software. Users can then reupload the revised dataset and continue running new scenarios."
                                    )
                                  ),
                                  
                                  tags$div(
                                    tags$h4("Model Parameters"),
                                    
                                    shinydashboard::box(downloadButton(
                                      ns("dl_sr_wb_dat"), "Stressor Response Workbook"
                                    )),
                                    
                                    shinydashboard::box(downloadButton(
                                      ns("dl_sm_wb_dat"), "Stressor Magnitude Workbook"
                                    )),
                                    
                                    shinydashboard::box(downloadButton(
                                      ns("dl_ws_poly"), "Watershed Polygons (Spatial)"
                                    )),
                                    
                                    shinydashboard::box(downloadButton(
                                      ns("dl_pop_mod"), "Population Model Vital Rates
"
                                    ))
                                    
                                  ),
                                  
                                  
                                  tags$div(
                                    tags$h4("Model Exports"),
                                    
                                    shinydashboard::box(downloadButton(ns("dl_jm_data"), "Joe Model Data")),
                                    
                                    shinydashboard::box(downloadButton(
                                      ns("dl_pop_mod_out"), "Population Model Ouputs"
                                    ))
                                  ),
                                  
                                )
                              )))
}



#' Export server function
#'
#' @param none
#'
#' @return None
#'
module_export_server <- function(id) {
  moduleServer(id,
               function(input, output, session) {
                 ns <- session$ns
                 
                 print("Calling module_export_server")
                 
                 
                 #-------------------------------------------------------
                 # Disable and enable pop model exports
                 observe({
                   pop_data <- session$userData$rv_pop_data_huc_ts$dat
                   if (length(pop_data) > 0) {
                     shinyjs::enable("dl_pop_mod_out")
                   } else {
                     shinyjs::disable("dl_pop_mod_out")
                   }
                   # Population model data cannot be null
                 })
                 
                 
                 #-------------------------------------------------------
                 # Disable and enable joe model exports
                 observe({
                   jm_data <- session$userData$rv_joe_model_results$sims
                   if (length(jm_data) > 0) {
                     shinyjs::enable("dl_jm_data")
                   } else {
                     shinyjs::disable("dl_jm_data")
                   }
                   # Population model data cannot be null
                 })
                 
                 
                 
                 #-----------------------------------------
                 # Stressor Response Workbook Data Download
                 output$dl_sr_wb_dat <- downloadHandler(
                   filename = function() {
                     "stressor_response.xlsx"
                   },
                   content = function(file) {
                     # Gather sheets
                     main <-
                       isolate(session$userData$rv_stressor_response$main_sheet)
                     sheet_list <- list()
                     sheet_list$Main <- main
                     
                     # Loop through other variables
                     sr_names <-
                       isolate(names(session$userData$rv_stressor_response$sr_dat))
                     
                     counter <- 2 # because main is first sheet
                     for (i in 1:length(sr_names)) {
                       this_name <- sr_names[i]
                       this_dat <-
                         isolate(session$userData$rv_stressor_response$sr_dat[this_name])
                       fix_dat <- this_dat[[1]]
                       colnames(fix_dat) <-
                         c(this_name,
                           "Mean System Capacity (%)",
                           "SD",
                           "low.limit",
                           "up.limit")
                       sheet_list[[counter]] <- fix_dat
                       names(sheet_list)[counter] <- this_name
                       counter <- counter + 1
                     }
                     
                     write_xlsx(sheet_list, path = file)
                   }
                 )
                 
                 
                 #------------------------------------------
                 # Stressor Magnitude Workbook Data Download
                 output$dl_sm_wb_dat <- downloadHandler(
                   filename = function() {
                     "stressor_magnitude.xlsx"
                   },
                   content = function(file) {
                     # Gather worksheets
                     mydata <- isolate(session$userData$rv_stressor_magnitude$sm_dat)
                     write_xlsx(mydata, path = file)
                     
                   }
                 )
                 
                 #------------------------------------------
                 # Watershed Polygons
                 output$dl_ws_poly <- downloadHandler(
                   filename = function() {
                     "huc_polygons.gpkg"
                   },
                   content = function(file) {
                     # Gather worksheets
                     poly <- isolate(session$userData$rv_HUC_layer_load$data)
                     poly_sp <- as(poly, "Spatial")
                     
                     
                     sf::st_write(poly, dsn = file)
                     
                   }
                 )
                 
                 
                 
                 #------------------------------------------
                 # Population Model Vital Rates
                 output$dl_pop_mod <- downloadHandler(
                   filename = function() {
                     "life cycles.csv"
                   },
                   content = function(file) {
                     # Gather worksheets
                     life_cycles <- isolate(session$userData$rv_life_stages$dat)
                     write.csv(life_cycles, file = file, row.names = FALSE)
                     
                   }
                 )
                 
                 
                 #------------------------------------------
                 # Population Model Results
                 output$dl_pop_mod_out <- downloadHandler(
                   filename = function() {
                     "PopulationModel.RData"
                   },
                   content = function(file) {
                     pop_data <- isolate(session$userData$rv_pop_data_huc_ts$dat)
                     save(pop_data, file = file)
                     
                   }
                 )
                 
                 
                 #------------------------------------------
                 # Save Joe Model Outputs
                 output$dl_jm_data <- downloadHandler(
                   filename = function() {
                     "JoeModelResults.xlsx"
                   },
                   content = function(file) {
                     jm_data <- isolate(session$userData$rv_joe_model_results$sims)
                     jm_data <- jm_data[[length(jm_data)]]
                     write_xlsx(jm_data, path = file)
                   }
                 )
                 
                 
               })
}