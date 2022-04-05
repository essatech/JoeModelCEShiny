#' Comparison between Joe Model and Population Model Predictions
#'
#' @param id The id for this module
#'
#' @return a tagList containing UI elements
#'
module_joe_vs_population_ui <- function(id) {
  ns <- NS(id)
  # Single action button to call modal
  actionButton(
    ns("module_joe_vs_population"),
    tags$b("Population model"),
    class = "chart-line",
    width = "100%"
  )
  
}



#' Comparison between Joe Model and Population Model Predictions
#'
#' @param none
#'
#' @return None
#'
module_joe_vs_population_server <- function(id) {
  moduleServer(id,
               function(input, output, session) {
                 ns <- session$ns
                 
                 print("Calling module_joe_vs_population_server")
                 
                 
                 #-------------------------------------------------------
                 # DISABLE AND ENABLE
                 #-------------------------------------------------------
                 # disable sm adjust button unless at least one HUC is selected
                 observe({
                   click_hucs <- rv_clickedIds$ids
                   if (length(click_hucs) > 0) {
                     shinyjs::enable("module_joe_vs_population")
                   } else {
                     shinyjs::disable("module_joe_vs_population")
                   }
                 })
                 
                 
                 #------------------------------------------------------
                 # Labels of selected HUCs
                 #------------------------------------------------------
                 output$renderText <- renderUI({
                   selected <- rv_clickedIds$ids
                   name_list <- list()
                   for (i in 1:length(selected)) {
                     name_list[[i]] <- tags$div(selected[i])
                   }
                   res_n <- paste(name_list, collapse = "; ")
                 })
                 
                 
                 #-------------------------------------------------------
                 # MODAL CONTENT
                 #-------------------------------------------------------
                 observeEvent(input$module_joe_vs_population, {
                   # Open the Modal
                   
                   showModal(
                     modalDialog(
                       title = "Run the Population Model",
                       tagList(
                         tags$p(
                           "Run the population model with cumulative effects for selected watersheds."
                         ),
                         
                         # Selected list of HUCs
                         tags$p(textOutput(ns(
                           "list_selected_hucs"
                         ))),
                         
                         fluidRow(
                           column(
                             width = 4,
                             actionButton(
                               ns("run_pop_model"),
                               "Run Population Model",
                               class = "btn-danger",
                               style = "color: white; width: 100%; margin-top: 24px;"
                             )
                           ),
                           column(width = 4,
                                  numericInput(
                                    ns("test_n_years"),
                                    label = "N-years",
                                    value = 50
                                  )),
                           column(
                             width = 4,
                             numericInput(
                               ns("test_n_replicates"),
                               label = "N-replicates",
                               value = 10
                             )
                           )
                         ),
                         
                         tags$h3("Time series projections"),
                         
                         tags$p(
                           "Preview time series projections for each selected watershed"
                         ),
                         
                         radioButtons(
                           ns("samp_plot_type"),
                           "Plot type:",
                           c(
                             "Adults" = "adult",
                             "Sub-Adults" = "subadult",
                             "Juveniles" = "juv",
                             "YoY" = "yoy",
                             "Lambda" = "lambda",
                             "All Life Stages" = "allstage"
                           ),
                           inline  = TRUE
                         ),
                         
                         
                         plotlyOutput(ns("samp_adults"))
                       ),
                       
                       
                       
                       tags$h3("Stressor Evaluation"),
                       
                       tags$p("Placeholder text describing each plot type ..."),
                       
                       
                       radioButtons(
                         ns("main_se_plot_type"),
                         "Plot type:",
                         c(
                           "Overview" = "overview",
                           "By Dose" = "by_dose",
                           "Joe Model Comparison" = "joe_model_comparison"
                         ),
                         inline  = TRUE
                       ),
                       
                       tags$div(style = "wdith: 80%; height: 250px; background-color: #F0F8FF"),
                       
                       
                       easyClose = TRUE,
                       size = 'l',
                       footer = NULL
                     )
                   )
                 })
                 
                 
                 
                 
                 
                 
                 #-------------------------------------------------------
                 # Run a time series projection for target watersheds ...
                 #-------------------------------------------------------
                 observeEvent(input$run_pop_model, {
                   
                   # browser()
                   
                   # Show a loading spinner to the user
                   #show_modal_spinner(
                   # spin = "hollow-dots",
                   #   color = "#0073b7",
                   #  text = paste0("Running a time series projection...")
                   #)
                   
                   # -----------------------------------
                   # Gather inputs in isolate mode
                   isolate({
                     # Gather inputs
                     test_n_years <- input$test_n_years
                     test_n_replicates <- input$test_n_replicates
                     dat <- rv_life_stages$dat
                     
                     # Gather the environmental stressors for selected HUCs
                     click_hucs <- rv_clickedIds$ids
                     splits <- lapply(click_hucs, strsplit, "\\|")
                     splits <- sapply(splits, head, 1)
                     splits <- sapply(splits, head, 1)
                     HUC_ids <- as.numeric(splits)

                     # Get the target HUCs
                     CE_df <- rv_stressor_magnitude$sm_dat
                     CE_df <-
                       CE_df[which(CE_df$HUC_ID %in% HUC_ids),]
                     
                     # Thin down stressors to target...
                     sr <- list()
                     sr$main_sheet <-
                       rv_stressor_response$main_sheet
                     sr$stressor_names <-
                       rv_stressor_response$stressor_names
                     sr$sr_dat <- rv_stressor_response$sr_dat
                     # Thin down...
                     sr$main_sheet <-
                       sr$main_sheet[which(sr$main_sheet$Stressors %in% CE_df$Stressor), ]
                     sr$stressor_names <-
                       sr$stressor_names[sr$stressor_names %in% CE_df$Stressor]
                     sr$sr_dat <-
                       sr$sr_dat[which(names(sr$sr_dat) %in%  CE_df$Stressor)]
                     
                     # Merge main sheet data
                     CE_df$Stressor_cat <- NULL
                     CE_df <-
                       merge(
                         CE_df,
                         sr$main_sheet,
                         by.x = "Stressor",
                         by.y = "Stressors",
                         all.x = TRUE
                       )
                     
                     # Stressor Magnitude...
                     # Make up dummy data for a sample watershed
                     smw_sample <-
                       data.frame(
                         HUC_ID = CE_df$HUC_ID,
                         NAME = CE_df$NAME,
                         Stressor = CE_df$Stressor,
                         Stressor_cat = CE_df$Stressor_cat,
                         Mean = CE_df$Mean,
                         SD = CE_df$SD,
                         Distribution = CE_df$Distribution,
                         Low_Limit = CE_df$Low_Limit,
                         Up_Limit = CE_df$Up_Limit
                       )
                     
                     # browser()
                     jm <- JoeModelCE::JoeModel_Run(
                       dose = smw_sample,
                       sr_wb_dat = sr,
                       MC_sims = test_n_replicates,
                       adult_sys_cap = FALSE
                     )
                     
                     nrow(jm$sc.dose.df)
                     nrow(smw_sample)
                     
                     # Gather summary at stressor level
                     dobj <- jm$sc.dose.df
                     
                     # add on missing attr columns
                     merge_cols <-
                       CE_df[, c("Stressor",
                                 "Life_stages",
                                 "Parameters",
                                 "Stressor_cat")]
                     
                     merge_cols <- merge_cols[!(duplicated(merge_cols)), ]
                     
                     m_all <-
                       merge(
                         merge_cols,
                         dobj,
                         by.x = "Stressor",
                         by.y = "Stressor",
                         all.x = TRUE,
                         all.y = TRUE
                       )
                     
                     # Fix col names
                     colnames(m_all)[colnames(m_all) == "Stressors"] <-
                       "Stressor"
                     colnames(m_all)[colnames(m_all) == "Life_stages"] <-
                       "life_stage"
                     colnames(m_all)[colnames(m_all) == "Parameters"] <-
                       "parameter"
                     colnames(m_all)[colnames(m_all) == "Stressor_cat"] <-
                       "Stressor_cat"
                     
                     # Return cleaned object
                     CE_df <- m_all
                     
                     
                   })
                   # end of isolate
                   # -------------------------------------------
                   
                   # Gather population model inputs
                   # Setup objects for population model
                   pop_mod_setup <-
                     JoeModelCE::pop_model_setup(life_cycles = dat)
                   # Build matrix elements for population model
                   pop_mod_mat <-
                     JoeModelCE::pop_model_matrix_elements(pop_mod_setup = pop_mod_setup)
                  
              
                   # Set the K.adj (K adjustment prior to pop model run)
                   life_histories <- pop_mod_mat$life_histories
                   # Mathematical expression of the transition matrix
                   life_stages_symbolic <-
                     pop_mod_mat$life_stages_symbolic
                   # Mathematical expression of the density matrix
                   density_stage_symbolic <-
                     pop_mod_mat$density_stage_symbolic
                   
                   
                   all_outputs <- list()
                   counter_huc <- 1
                   
                   # Big Loop through HUCs and reps within each HUC
                   for (hh in 1:length(HUC_ids)) {
                     this_huc <- HUC_ids[hh]
                     huc_outputs <- list()
                     counter_sim <- 1
                     
                     # Loop through simulations per HUC
                     for (ii in 1:test_n_replicates) {
                       # Environmental sample for this rep
                       if (is.null(CE_df)) {
                         CE_df_rep <- CE_df
                       } else {
                         CE_df_rep <-
                           CE_df[which(CE_df$simulation == ii & CE_df$HUC == this_huc),]
                         CE_df_rep <-
                           CE_df_rep[!(duplicated(CE_df_rep[, c("Stressor", "life_stage", "HUC")])),]
                         # Do not include regular Joe parameters
                         CE_df_rep <-
                           CE_df_rep[which(!(is.na(CE_df_rep$parameter))),]
                       }
                       
                       
                       # Run simple population projection - project forward through time
                       run_with_ce <-
                         JoeModelCE::Projection_DD(
                           M.mx = life_stages_symbolic,
                           # projection matrix expression
                           D.mx = density_stage_symbolic,
                           # density-dependence matrix
                           H.mx = NULL,
                           dat = life_histories,
                           # life history data
                           K = life_histories$Ka,
                           # initial pop size as stage-structure vector
                           Nyears = test_n_years,
                           # years to run simulation
                           p.cat = 0,
                           # Probability of catastrophe
                           CE_df = CE_df_rep
                         )
                       
                       # Gather info
                       # remove junk
                       run_with_ce$vars <- NULL
                       run_with_ce$Cat. <- NULL
                       run_with_ce$info <-
                         data.frame(huc_id = this_huc,
                                    sim = ii,
                                    type = "CE")
                       
                       huc_outputs[[counter_sim]] <- run_with_ce
                       counter_sim <- counter_sim + 1
                     }
                     
                     # Add HUCs to master list
                     all_outputs[[counter_huc]] <- huc_outputs
                     counter_huc <- 1 + counter_huc
                     
                   } # end of big loop through HUCs and reps within HUCs
                   
                  
                   
                   # Gather all data and send to reactive object
                   rv_pop_data_huc_ts$dat <-
                     all_outputs
                   
                   # Update the run counter
                   rc <- rv_pop_data_huc_ts$run_counter
                   rv_pop_data_huc_ts$run_counter <-
                     rc + 1
                   
                   # Show plot in the modal
                   rv_show_pop_main_plot$open <- TRUE
                   
                   
                 }) # end of a time series projections for target watersheds ...
                 
                 
                
                 
                 ###############################################
                 # ---------------------------------------------
                 # Render plots for preview modal time series
                 # ---------------------------------------------
                 ###############################################
                 
                 output$samp_adults <- renderPlotly({
                   
                   print("Plotting adults...")
                   
                   # Gather plot type to show:
                   plot_type <- input$samp_plot_type
                   # adult; subadult; juv; yoy; lambda; allstage
                   
                   # First time the modal is open plot screen should be blank
                   if(rv_pop_data_huc_ts$run_counter == 1) {
                     # No data to plot yet - grey empty plot
                     p <- ggplot() 
                     return(p)
                   }
                   
                   
                   # browser()
                   
                   # Generate time series plots for target watersheds
                   length(rv_pop_data_huc_ts$dat)
                   

                   # Get data for current run (from above)
                   pdat <- rv_pop_data_huc_ts$dat
                   
                   # Which data frame should be sources from the results object
                   # either counts of individuals N or lambdas
                   if (plot_type == "adult" |
                       plot_type == "subadult" |
                       plot_type == "juv" |
                       plot_type == "yoy" |
                       plot_type == "allstage") {
                     t_var <- "N"
                   }
                   if (plot_type == "lambda") {
                     t_var <- "lambdas"
                   }
                   
                   # Extract target object from bootstrap replicate runs
                   get_n_obj <- function(obj, name = "") {
                     info <- obj$info
                     obj <- obj[[name]]
                     obj <- as.data.frame(obj)
                     obj$year <- 1:nrow(obj)
                     obj <- cbind(obj, info)
                     return(obj)
                   }
                   
                   for_all_hucs <- function(huc_dat, t_var) {
                     pdata_0 <- lapply(huc_dat, get_n_obj, name = t_var)
                     pdata_0 <- do.call("rbind", pdata_0)
                     return(pdata_0)
                   }
                   
                   # Mege to a single clean data frame
                   pdata_1 <- lapply(pdat, for_all_hucs, t_var = t_var)
                   pdata_1 <- do.call("rbind", pdata_1)
                   
            
                   # ---------------------------------------------
                   # Generate plot for all life stages here (& lambda)...
                   # ---------------------------------------------
                   
                   # Min value on y-axis will be zero unless plotting lambdas which hover around 1
                   my_y_min <- 0
                   
                   # Switch to target variable
                   # Set plot titles
                   if (plot_type == "yoy") {
                     pdata_1$value <- pdata_1$V1
                     mtitle <- "YoY/Fry Abundance"
                     y_axe <- "N"
                   }
                   if (plot_type == "juv") {
                     pdata_1$value <- pdata_1$V2
                     mtitle <- "Juvenile Abundance"
                     y_axe <- "N"
                   }
                   if (plot_type == "subadult") {
                     pdata_1$value <- pdata_1$V3
                     mtitle <- "Sub-Adult Abundance"
                     y_axe <- "N"
                   }
                   if (plot_type == "adult") {
                     pdata_1$value <- pdata_1$V4
                     mtitle <- "Adult Abundance"
                     y_axe <- "N"
                   }
                   if (plot_type == "lambda") {
                     pdata_1$value <- pdata_1$obj
                     mtitle <- "Lambda Values (adults)"
                     y_axe <- "lambda (adults)"
                     my_y_min <- min(pdata_1$value)
                   }
                   
                   
                   # Summarize data by yearly averages across simulations with SD values
                   p3 <-
                     pdata_1 %>% dplyr::group_by(huc_id, year) %>% summarise(mean = mean(value, na.rm = TRUE),
                                                                          sd = sd(value, na.rm = TRUE))
                   
                   p3$lwr <- p3$mean - p3$sd
                   p3$upr <- p3$mean + p3$sd
                   
                   p3$HUC <- p3$huc_id
                   p3$HUC <- as.character(p3$HUC)
                   p3$huc_id <- NULL
                   
                
                   # Create the ggplot plotting object
                   p <-
                     ggplot(data = p3,
                            aes(
                              x = year,
                              y = mean,
                              group = HUC,
                              color = HUC
                            )) +
                     scale_y_continuous(limits = c(my_y_min, max(p3$upr))) +
                     geom_line() +
                     ggtitle(mtitle) +
                     xlab("Simulation Year") + ylab(y_axe)
                   
                   p <-
                     p + geom_ribbon(data = p3,
                                     aes(ymin = lwr, ymax = upr),
                                     alpha = 0.1) + theme_bw()
                   
                   return(p)
                   
                   
                 }) # end of ender plots for preview modal time series
                 
                 
                 
                 
                 
                 
               })
}