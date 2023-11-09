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
  actionButton(
    ns("adjust_stressor_magnitude"),
    tags$b("Adjust Magnitude"),
    class = "chart-line clean-button",
    width = "100%"
  )
  
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
  moduleServer(id,
               function(input, output, session) {
                 ns <- session$ns
                 
                 print("Calling module_huc_results_server")
                 
                 #-------------------------------------------------------
                 # DISABLE AND ENABLE
                 #-------------------------------------------------------
                 # disable sm adjust button unless at least one HUC is selected
                 observe({
                   click_hucs <- rv_clickedIds$ids
                   if (length(click_hucs) > 0) {
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
                   
                   showModal(
                     modalDialog(
                       title = "Adjust Stressor Magnitude",
                       tagList(
                         uiOutput(ns("text_preview")),
                         
                         fluidRow(shinydashboard::box(width = 12,
                                                      DTOutput(
                                                        ns("stressor_inputs")
                                                      ))),
                         
                         fluidRow(
                           column(
                             width = 12,
                             tags$ul(
                               tags$li("Stressor: Name of the stressor. Note that this field must match the spelling used in the stressor response workbook"), 
                               tags$li("Mean: Mean value of the stressor for specific the target HUC. Please refer to the original dose-response workbook to determine units for each stressor."), 
                               tags$li("SD: Standard deviation for the specific stressor for the target HUC. The value of each stressor will be resampled for each HUC based on the mean value and the SD. This resampling is to account for uncertainty and interannual variation in nature"),
                               tags$li("Distribution: Resampling distribution use for resampling the HUC stressor value for each simulation. Can be either 'lognormal' or 'normal'. This is field only relevant for certain variables"),
                               tags$li("Low_Limit: Lower limits for the HUC-specific stressor from resampling. Regardless of the Mean/SD values used, the stressor value sampled for this HUC will never be lower than the specified lower limit."),
                               tags$li("Up_Limit: Upper limits for the HUC-specific stressor. Regardless of the Mean/SD values used, the stressor value sampled for this HUC will never be high than the specified upper limit."),
                               tags$li("SysCap: The resulting stressor-specific system capacity for the individual HUC (or selected HUCs) based on the mean value.")
                             ),
                           )
                         ),
                         
                         fluidRow(column(
                           width = 12,
                           actionButton(ns("close_sm_modal"), "Close stressor-magnitude module", style = "margin: 15px;")
                         ))
                       ),
                       easyClose = TRUE,
                       size = 'l',
                       footer = NULL
                     )
                   )
                 }) # END OF INPUT MODAL UI
                 #-------------------------------------------------------
                 
                 
                 #-------------------------------------------------------
                 # Close stressor response modal with custom button
                 #-------------------------------------------------------
                 observeEvent(input$close_sm_modal, {
                   removeModal()
                 })
                 
                 
                 #-------------------------------------------------------
                 # Populate modal text based on selection
                 #-------------------------------------------------------
                 # If only one watershed selected return name and description
                 # If multiple watersheds selected then show number selected and provide instruction to user
                 output$text_preview <- renderUI({
                   
                   selected_raw <- rv_clickedIds$ids
                   tl <- NULL
                   
                   # Single-HUC Selection
                   if (length(selected_raw) == 1) {
                     id   <- strsplit(selected_raw, "\\|")[[1]][1]
                     name <- strsplit(selected_raw, "\\|")[[1]][2]
                     tl <- tagList(
                       tags$h4(name, style = "color: #3c8dbc;"),
                       tags$h4(paste0("HUC ID: ", id), style = "color: #3c8dbc;"),
                       tags$p(
                         "Use this table to modify the underlying stressor values for selected HUCs. Users may wish to modify values to represent some type of recovery action or habitat loss. Click on cells in the table below to update the stressor magnitude for the selected unit. Adjust the mean value for each stressor (Mean), the standard deviation (SD), the distribution type (options are: normal or lognormal), the lower limit and upper limit (for stochastic simulations). If only one HUC is selected then values will appear in the table; however, if multiple HUCs are selected then the table will appear blank. When multiple HUCs are selected all modified values will be shared across the selected HUCs. The mean system capacity is shown for selected HUCs to provide a preview of the Joe Model output."
                       ),
                       tags$div(
                         uiOutput(ns("csc_huc_indicator"))
                       )
                     )
                   }
                   
                   # Multi-HUC Selection
                   if (length(selected_raw) > 1) {
                     # Fix format
                     getID <- function(x) {
                       strsplit(x, "\\|")[[1]][1]
                     }
                     selected_ids <- lapply(selected_raw, getID) %>% unlist()
                     selected_ids_txt <- paste(selected_ids, collapse = ", ")
                     tl <- tagList(
                       tags$h3("Multiple Units Selected", style = "color: #103e85;"),
                       tags$h4(selected_ids_txt, style = "color: #0073b7;"),
                       tags$p(
                         "Multiple HUCs selected. Use the table below to set stressor magnitude values for the selected HUCs. Note that entering a value in any of the cells will update the values for all the selected HUCs. Leave values blank to keep original values for each HUC."
                       ),
                       tags$div(
                         uiOutput(ns("csc_huc_indicator"))
                       )
                     )
                   }
                   
                   return(tl)
                   
                 })
                 
                 
                 
                 #-------------------------------------------------------
                 # Populate modal input table
                 #-------------------------------------------------------
                 # Reformat data prior to making edit table
                 # Key consideration here is deciding if editing only one 
                 # HUC - in which case we show current values as place holders 
                 # (or) Multi-edit mode were any input value will overwrite all HUC
                 
                 output$stressor_inputs <- renderDT({
                   
                   print("Stressor magnitude DT...")

                   # HUCs currently selected
                   selected_raw <- rv_clickedIds$ids
                   
                   # Fix format
                   getID <- function(x) {
                     strsplit(x, "\\|")[[1]][1]
                   }
                   selected_ids <- lapply(selected_raw, getID) %>% unlist()
                   
                   
                   
                   # --------------------------------------------
                   # Populate the temporary placeholder CSC value
                   # --------------------------------------------
                   if(length(selected_ids) > 0) {
                     
                     print("Populate placeholder csc value - A...")
                     
                     # Get the stressor magnitude
                     dr <- session$userData$rv_stressor_magnitude$sm_dat
                     dr <- dr[which(dr$HUC_ID %in% selected_ids), ]
                     # Set SD to 0 for mean value
                     dr$SD <- 0
                     
                     # Make a copy of the sr_wb_dat data to 
                     # run the Joe model just once
                     sr_wb_dat_copy <- list()
                     sr_wb_dat_copy$main_sheet <- isolate(session$userData$rv_stressor_response$main_sheet)
                     sr_wb_dat_copy$stressor_names <- isolate(session$userData$rv_stressor_response$stressor_names)
                     sr_wb_dat_copy$pretty_names <- isolate(session$userData$rv_stressor_response$pretty_names)

                     if(!(is.null(session$userData$rv_stressor_response$interaction_values))) {
                      print("Running with int...")
                      sr_wb_dat_copy$MInt <- session$userData$rv_stressor_response$interaction_values
                      names(sr_wb_dat_copy$MInt)
                     }

                     # Do not isolate this - we want function to update
                     sr_wb_dat_copy$sr_dat <- session$userData$rv_stressor_response$sr_dat
                     
                     jm <- JoeModelCE::JoeModel_Run(
                       dose = dr,
                       sr_wb_dat = sr_wb_dat_copy,
                       MC_sims = 1,
                       adult_sys_cap = FALSE)
                     
                     # Temporary CSC for this HUC with these params...
                     rv_clickedIds_csc$csc <- mean(jm$ce.df$CE, na.rm = TRUE)
                     
                     print(round(rv_clickedIds_csc$csc, 3))
                     # Look at MSC for other variables...
                     var_csc <- data.frame(
                       snames = jm$sc.dose.df$Stressor,
                       sys_cap = round(jm$sc.dose.df$sys.cap * 100, 1)
                     )
                     # Group by stressor if multiple
                     var_csc <- var_csc %>% group_by(snames) %>% summarise(sys_cap = mean(sys_cap, na.rm = TRUE))
                     # Update reactive placeholder value
                     rv_clickedIds_csc$var_csc <- var_csc

                   } else {
                     # Set to NA
                     rv_clickedIds_csc$csc <- NA
                     rv_clickedIds_csc$var_csc <- NA
                     print("Reset HUC csc...")
                   }
                   
                   
                   # --------------------------------------------
                   # Build Data Table
                   # --------------------------------------------
                   
                   # Get values if single HUC or set as NA if multi
                   if (length(selected_ids) == 1) {
                     # Use render DT with proxy to avoid reload on edit...
                     raw_data <- session$userData$rv_stressor_magnitude$sm_dat
                     table_vals <- raw_data %>% filter(HUC_ID == selected_ids)
                     table_vals <- table_vals[order(table_vals$Stressor),]
                     
                     # Merge on temporary System Capacity Estimate
                     tsysm_capv <- rv_clickedIds_csc$var_csc 
                     table_vals <- base::merge(table_vals, tsysm_capv,
                                         by.x = "Stressor", by.y = "snames",
                                         all.x = TRUE, all.y = FALSE)
                    
                     table_vals <-
                       table_vals[, c("Stressor",
                                      "Mean",
                                      "SD",
                                      "Distribution",
                                      "Low_Limit",
                                      "Up_Limit",
                                      "sys_cap")]
                     table_vals$SysCap <- table_vals$sys_cap
                     table_vals$sys_cap <- NULL
                     table_vals$Mean <- round(table_vals$Mean, 2)
                     table_vals$SD <- round(table_vals$SD, 2)
                     table_vals$Low_Limit <- round(table_vals$Low_Limit, 2)
                     table_vals$Up_Limit <- round(table_vals$Up_Limit, 2)
                     # Ensure to set SysCap to nonblank if not for Joe
                     table_vals$SysCap <- ifelse(is.na(table_vals$SysCap), "Pop.", table_vals$SysCap)
                   }
                   

                   # If nothing selected then return an empty dataframe
                   if (length(selected_ids) == 0) {
                     table_vals <-
                       data.frame(
                         Stressor = NA,
                         Mean = NA,
                         SD = NA,
                         Distribution = NA,
                         Low_Limit = NA,
                         Up_Limit = NA,
                         SysCap = NA
                       )
                     table_vals <- table_vals[0, ]
                   }
                   
                   # If multiple HUCs selected then return a dataframe for each variable
                   # but keep values empty since only select will be edited
                   if (length(selected_ids) > 1) {
                     
                     raw_data <- isolate(session$userData$rv_stressor_magnitude$sm_dat)
                     table_vals <- raw_data %>% filter(HUC_ID %in% selected_ids)
                     table_vals <- table_vals[order(table_vals$Stressor), ]
                     snames <- sort(unique(table_vals$Stressor))
                     
                     snames <- sort(snames)
                     table_vals <-
                       data.frame(
                         Stressor = snames,
                         Mean = NA,
                         SD = NA,
                         Distribution = NA,
                         Low_Limit = NA,
                         Up_Limit = NA
                       )
                     # Merge on system capacity estimates per stressor
                     tsysm_capv <- rv_clickedIds_csc$var_csc 
                     table_vals <- base::merge(table_vals, tsysm_capv,
                                               by.x = "Stressor", by.y = "snames",
                                               all.x = TRUE, all.y = FALSE)
                     table_vals$SysCap <- round(table_vals$sys_cap, 1)
                     table_vals$sys_cap <- NULL
                     
                     # Ensure to set SysCap to nonblank if not for Joe
                     table_vals$SysCap <- ifelse(is.na(table_vals$SysCap), "Pop.", table_vals$SysCap)
                   }
                   


                   # Build the JS DT Data Table Object
                   DT::datatable(
                     table_vals,
                     # The Stressor column is not editable
                     editable = list(target = "cell", disable = list(columns = c(0, 6))),
                     filter = "none",
                     selection = "single",
                     rownames = FALSE,
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
                   
                   # HUCs currently selected
                   selected_raw <- rv_clickedIds$ids
                   # Fix format
                   getID <- function(x) {
                     strsplit(x, "\\|")[[1]][1]
                   }
                   selected_ids <- lapply(selected_raw, getID) %>% unlist()
                   
                   
                   # Index list of stressor names (which value was clicked)
                   raw_data <- isolate(session$userData$rv_stressor_magnitude$sm_dat)
                   table_vals <- raw_data %>% filter(HUC_ID %in% selected_ids)
                   table_vals <- table_vals[order(table_vals$Stressor), ]
                   snames <- sort(unique(table_vals$Stressor))
                   
                   
                   if (info$value == "") {
                     print("Take no action")
                   } else {
                     if (info$value == "normal") {
                       
                     } else {
                       
                       info$value <- as.numeric(info$value)
                       info$value <- ifelse(is.na(info$value), 0, info$value)
                       
                       i = as.numeric(info$row)
                       j = as.numeric(info$col)
                       k = as.numeric(info$value)
                       
                       var <-
                         c("Stressor",
                           "Mean",
                           "SD",
                           "Distribution",
                           "Low_Limit",
                           "Up_Limit")
                       
                       print(info)
                       print(info$value)
                       print(snames[i])
                       print(var[j + 1])
                       print(selected_ids)
                       print(" --------------------- ")
                       

                       # Update stressor magnitude value for HUC or selected HUCs
                       # Update the reactive values
                       if(class(session$userData$rv_stressor_magnitude$sm_dat$HUC_ID) == "numeric") {
                        selected_ids <- as.numeric(as.character(selected_ids))
                       }


                       session$userData$rv_stressor_magnitude$sm_dat[which(
                         session$userData$rv_stressor_magnitude$sm_dat$HUC_ID %in% selected_ids &
                           session$userData$rv_stressor_magnitude$sm_dat$Stressor == snames[i]
                       ), var[j + 1]] <- info$value
                       
                       # Update the DT data table so the user knows what they have just done
                     }
                   }
                   
                 })
                 
                 
                 #-------------------------------------------------------------
                 # Update the mean system capacity for the selection
                 #-------------------------------------------------------------
                 output$csc_huc_indicator <- renderUI({
                   
                   msc <- rv_clickedIds_csc$csc
                   
                   alert_col <- "#a8a8a8"
                   alert_col <- ifelse(msc < 0.2, "#d7191c50", alert_col)
                   alert_col <- ifelse(msc >= 0.2, "#fdae6150", alert_col)
                   alert_col <- ifelse(msc >= 0.5, "#a6d96a50", alert_col)
                   alert_col <- ifelse(msc >= 0.7, "#1a964150", alert_col)
                   alert_col_border <- paste0(alert_col, "30")
                   
                   
                   alert_style <- paste0("background: ",alert_col,";
                    border: #73737330;
                    border-style: solid;
                    border-width: 5px;
                    border-radius: 10px;
                    padding: 3px;
                    color: 'black;")
                   
                   print(paste0("HUC csc is: ", msc, "..."))
                   tags$h4(paste0("Mean System Capacity: ", round(msc * 100, 1), "%"), style = alert_style)
                   
                 })
                 
                 
                 
                 
                 
                 
               })
}