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
                 output$list_selected_hucs <- renderText({
                   selected <- rv_clickedIds$ids
                   res_n <- paste(selected, collapse = "; ")
                   res_n
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
                           "Run the population model with cumulative effects for selected watersheds. Model run time will be dependent on the number of years simulated (N-years) and the number of batch replicates (N-replicates)."
                         ),
                         
                         # Selected list of HUCs
                         textOutput(ns("list_selected_hucs")),
                         
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
                           "Preview time-series projections for each selected watershed. After the population model run is complete (or re-run/updated) the time series plots below will be updated to show the number of individuals simulated for each year of the simulation. Separate lines show the number of individuals in each HUC. If the plot is too messy try selecting fewer HUCs and then re-running. The default plot type shows the number of adults across each HUC through time. Change the plot type to switch between times series of adults, sub-adults, juveniles, YoY (young of year – fry, Age-0), and the population Lambda Values (calculated as Nt+1/Nt). Each time series shows mean values across batch replicates (lines). The grey bands around each line represent plus & minus one SD from the batch replicate model runs."
                         ),
                         
                         radioButtons(
                           ns("samp_plot_type"),
                           "Plot type:",
                           c(
                             "Adults" = "adult",
                             "Sub-Adults" = "subadult",
                             "Juveniles" = "juv",
                             "YoY" = "yoy",
                             "Lambda" = "lambda"
                           ),
                           inline  = TRUE
                         ),
                         
                         plotlyOutput(ns("samp_adults")),
                         
                         tags$h3("Stressor Evaluation"),
                         
                         tags$p("Evaluate stressors across does and life stage"),
                         
                         tags$p("The boxplots plots (below) show results from the population model broken down by life stage (rows) and selected HUCs (columns). Each boxplot shows the abundance of individuals (across years and batch replicates). Individual histograms are shown for selected HUCs. An additional hypothetical reference (baseline) condition is also included (on the far left). The reference ‘No Stressors’ simulation is included for convenience to help visualize cumulative effects in reference to a hypothetical baseline where the effect of all stressors is set to zero. Variability within the ‘No Stressors’ simulation is attributed entirely to demographic effects. These plots are helpful to evaluate potential impacts on a given life stage and then determine if these trends are consistent across HUCs."),
                         
                         plotlyOutput(ns("violin_plots"),
                                      height = "800px"),
                         
                         
                         tags$p("The HUC-Stressors plots (below) show results plotted across each stressor. As the population model is run across years and batch replicates, individual stressor magnitude values are continuously resampled for each of the selected HUCs (based on the mean and SD values originally provided in the stressor magnitude workbook). These plots are included to help piece together key environmental drivers that may be responsible for trends observed in the previous population time series and boxplots shown above. Each point represents the number of adults for a specific year and batch in the simulation. If the batch replicates or n-years are increased in the simulation more points will appear. If the SD is set to zero for an individual stressor, then there will be no variability across the x-axis and all points will overlap. To avoid clutter stressor plots are only included as panels if the response is less than 100%. If a stressor is not shown in the plots, then the system capacity is 100% for the selected HUCs."),
                         
                         
                         plotlyOutput(ns("sr_plots"),
                                      height = "600px"),
                         
                         
                       ),
                       
                       
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
                     # Limit user to 500 years
                     test_n_years <-
                       ifelse(test_n_years > 500, 500, test_n_years)
                     test_n_replicates <- input$test_n_replicates
                     # Limit user to 100 replicates
                     test_n_replicates <-
                       ifelse(test_n_replicates > 100, 100, test_n_replicates)
                     
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
                       CE_df[which(CE_df$HUC_ID %in% HUC_ids), ]
                     
                     # Thin down stressors to target...
                     sr <- list()
                     sr$main_sheet <-
                       rv_stressor_response$main_sheet
                     sr$stressor_names <-
                       rv_stressor_response$stressor_names
                     sr$sr_dat <- rv_stressor_response$sr_dat
                     # Thin down...
                     sr$main_sheet <-
                       sr$main_sheet[which(sr$main_sheet$Stressors %in% CE_df$Stressor),]
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
                     
                     merge_cols <-
                       merge_cols[!(duplicated(merge_cols)),]
                     
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
                   all_outputs_baseline <- list()
                   counter_huc <- 1
                   
                   # Big Loop through HUCs and reps within each HUC
                   for (hh in 1:length(HUC_ids)) {
                     this_huc <- HUC_ids[hh]
                     huc_outputs <- list()
                     huc_outputs_baseline <- list()
                     counter_sim <- 1
                     
                     # Loop through simulations per HUC
                     for (ii in 1:test_n_replicates) {
                       # Environmental sample for this rep
                       if (is.null(CE_df)) {
                         CE_df_rep <- CE_df
                       } else {
                         CE_df_rep <-
                           CE_df[which(CE_df$simulation == ii &
                                         CE_df$HUC == this_huc), ]
                         CE_df_rep <-
                           CE_df_rep[!(duplicated(CE_df_rep[, c("Stressor", "life_stage", "HUC")])), ]
                         # Do not include regular Joe parameters
                         CE_df_rep <-
                           CE_df_rep[which(!(is.na(CE_df_rep$parameter))), ]
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
                       
                       
                       # Run baseline with no CE
                       run_with_baseline <-
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
                           CE_df = NULL
                         )
                       
                       
                       # Gather info - for CE run
                       run_with_ce$vars <- NULL
                       run_with_ce$Cat. <- NULL
                       
                       run_with_ce$info <-
                         data.frame(huc_id = this_huc,
                                    sim = ii,
                                    type = "CE")
                       
                       huc_outputs[[counter_sim]] <- run_with_ce
                       
                       # Gather info - for CE run
                       run_with_baseline$vars <- NULL
                       run_with_baseline$Cat. <- NULL
                       
                       run_with_baseline$info <-
                         data.frame(huc_id = this_huc,
                                    sim = ii,
                                    type = "baseline")
                       
                       huc_outputs_baseline[[counter_sim]] <-
                         run_with_baseline
                       
                       counter_sim <- counter_sim + 1
                       
                     }
                     
                     # Add HUCs to master list
                     all_outputs[[counter_huc]] <- huc_outputs
                     all_outputs_baseline[[counter_huc]] <-
                       huc_outputs_baseline
                     
                     counter_huc <- 1 + counter_huc
                     
                   } # end of big loop through HUCs and reps within HUCs
                   
                   
                   
                   # Gather all data and send to reactive object
                   rv_pop_data_huc_ts$dat <-
                     all_outputs
                   
                   rv_pop_data_huc_ts$dat_baseline <-
                     all_outputs_baseline
                   
                   rv_pop_data_huc_ts$joe_model_comp <- CE_df
                   
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
                   if (rv_pop_data_huc_ts$run_counter == 1) {
                     # No data to plot yet - grey empty plot
                     p <- ggplot()
                     return(p)
                   }
                   
                   
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
                   pdata_1 <-
                     lapply(pdat, for_all_hucs, t_var = t_var)
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
                   
                   p3$lwr <- ifelse(is.na(p3$lwr), 0, p3$lwr)
                   p3$lwr <- ifelse(p3$lwr < 0, 0, p3$lwr)
                   p3$upr <-
                     ifelse(is.na(p3$upr), (mean(p3$mean, na.rm = TRUE) + mean(p3$sd, na.rm = TRUE)), p3$upr)
                   
                   
                   p3$HUC <- p3$huc_id
                   p3$HUC <- as.character(p3$HUC)
                   p3$huc_id <- NULL
                   
                   # Clean up mean to avoid large decimans
                   p3$mean <- round(p3$mean, 1)
                   
                   
                   
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
                 
                 
                 
                 ###############################################
                 # ---------------------------------------------
                 # Render plots for Joe Model vs Pop Model
                 # ---------------------------------------------
                 ###############################################
                 
                 # -------------------------------------------------------------
                 # Violin plot showing density by life stage
                 # relative to reference
                 output$violin_plots <- renderPlotly({
                   print("Plotting joe_vs_pop_plots...")
                   
                   # Gather plot type to show from radio button
                   # overview; by dose or Joe Model comparison
                   
                   # First time the modal is open plot screen should be blank
                   if (rv_pop_data_huc_ts$run_counter == 1) {
                     # No data to plot yet - grey empty plot
                     p <- ggplot()
                     return(p)
                   }
                   
                   # Generate time series plots for target watersheds
                   # Data object is structured as follows
                   # Reactive Value for data from model run: rv_pop_data_huc_ts
                   # dat: scenario reference; dat_baseline: no stressors
                   # Object structure: [[watershed]][[replicate]]$N
                   
                   stress <- rv_pop_data_huc_ts$dat
                   bl <- rv_pop_data_huc_ts$dat_baseline
                   
                   # Gather data from large data objects
                   gather_ts_data <- function(obj) {
                     #Gather the shed index from the parent object
                     # Gather from within replicates
                     gather_x <- function(x) {
                       info <- data.frame(x$info)
                       df <- data.frame(x$N)
                       df$rep <- info$sim
                       df$year <- 1:nrow(df)
                       df$huc_id <- info$huc_id
                       df$type <- info$type
                       return(df)
                     }
                     reps <- lapply(obj, gather_x)
                     reps <- do.call("rbind", reps)
                     return(reps)
                   }
                   
                   # Extract object for watersheds and baseline scenario
                   df_stress <- lapply(stress, gather_ts_data)
                   df_stress <- do.call("rbind", df_stress)
                   
                   df_bl <- lapply(bl, gather_ts_data)
                   df_bl <- do.call("rbind", df_bl)
                   
                   # Set the shed ID of baseline to zero
                   df_bl$huc_id <- 0
                   
                   # Prep for violin plot:
                   df_violin <- rbind(df_stress, df_bl)
                   
                   df_violin$huc_id <-
                     as.character(df_violin$huc_id)
                   df_violin$huc_id <-
                     ifelse(df_violin$huc_id == "0",
                            "No Stressors",
                            df_violin$huc_id)
                   
                   df_violin <-
                     df_violin[, c("X1", "X2", "X3", "X4", "rep", "year", "huc_id", "type")]
                   colnames(df_violin) <-
                     c("YOY",
                       "JUV",
                       "SUBADULT",
                       "ADULT",
                       "rep",
                       "year",
                       "huc_id",
                       "type")
                   
                   df_violin_l <-
                     reshape2::melt(df_violin, id.vars = c("rep", "year", "huc_id", "type"))
                   
                   p <- df_violin_l %>%
                     ggplot(aes(x = huc_id, y = value, fill = huc_id)) +
                     geom_violin(width = 1.1,
                                 alpha = 0.6) +
                     geom_boxplot(width = 0.1,
                                  color = "grey",
                                  alpha = 0.2) +
                     facet_grid(rows = vars(variable), scales = "free") +
                     ggtitle("HUC vs Reference") +
                     xlab("HUC UNIT") + ylab("Abundance (#)")
                   
                   
                   return(p)
                   
                 })
                 
                 
                 # -------------------------------------------------------------
                 # Stressor response plots for selection
                 output$sr_plots <- renderPlotly({
                   # First time the modal is open plot screen should be blank
                   if (rv_pop_data_huc_ts$run_counter == 1) {
                     # No data to plot yet - grey empty plot
                     p <- ggplot()
                     return(p)
                   }
                   
                   # Gather the Joe Model response object
                   dr_plot <- rv_pop_data_huc_ts$joe_model_comp
                   
                   # Give single meaningful name
                   dr_plot$uvar <-
                     paste0(dr_plot$Stressor, ": ", dr_plot$life_stage)
                   dr_plot$addext <-
                     ifelse(is.na(dr_plot$parameter),
                            "",
                            paste0(" (", dr_plot$parameter, ")"))
                   
                   #dr_plot$uvar <-
                   #   paste0(dr_plot$uvar, dr_plot$addext)
                   dr_plot$HUC <- as.character(dr_plot$HUC)
                   
                   # Remove any good variables with zero effect
                   too_good <- dr_plot %>% group_by(uvar) %>%
                     summarise(mmin = min(sys.cap))
                   
                   no_impact_vars <- too_good$uvar[which(too_good$mmin >= 0.99)]
                   
                   # Only show remaining
                   dr_plot_remain <-
                     dr_plot[which(!(dr_plot$uvar %in% no_impact_vars)),]
                   
                   p <- dr_plot_remain %>%
                     ggplot(aes(x = dose, y = sys.cap, color = HUC)) +
                     geom_point(alpha = 0.6) +
                     coord_cartesian(ylim = c(0, 1)) +
                     facet_wrap(vars(uvar), scales = "free_x", ncol = 3) +
                     ggtitle("HUC Stressors") +
                     xlab("Stressor Magnitude [DOSE]") + ylab("System Capacity (0 - 1) [RESPONSE]")
                   
                   return(p)
                 })
                 
               })
}