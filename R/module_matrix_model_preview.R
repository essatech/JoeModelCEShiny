#' Matrix Model preview UI
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
module_matrix_model_preview_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    shinydashboard::box(
      width = 12,
      tags$div(
        class = "lam_bb",
        tags$p(
          "Run a time series projection preview for a hypothetical sample population",
        ),
        
        fluidRow(
          column(width = 6,
                 numericInput(
                   ns("test_n_years"), label = "n years", value = 50
                 )),
          column(width = 6,
                 numericInput(
                   ns("test_n_replicates"),
                   label = "n replicates",
                   value = 10
                 ))
        ),
        
        actionButton(
          ns("demo_projection"),
          "Demo Projection Time Series",
          class = "btn btn-info",
          style = "color:white;"
        )
      ),
    ),
    
    shinydashboard::box(
      width = 12,
      tags$div(
        class = "demo_stressors",
        tags$p(
          "Set hypothetical stressor values for the sample population projection preview"
        ),
        
        # Create the stressor variable list for sandbox
        htmlOutput(ns('stressor_variable_list_pm_sandbox')),
      ),
    )
    
  )
}


#' Matrix Model preview SERVER
#'
#' @param none
#'
#' @return None
#'
module_matrix_model_preview_server <- function(id) {
  moduleServer(id,
               function(input, output, session) {
                 ns <- session$ns
                 print("matrix model preview server...")
                 
                 
                 
                 # ---------------------------------------------------------
                 # (optional) Define the stressor variables values in the 
                 # sandbox population projection simulation
                 # ---------------------------------------------------------
                 output$stressor_variable_list_pm_sandbox <- renderUI({
                   
                   print("stressor_variable_list_pm_sandbox...")
                   ms_stress <- rv_stressor_response$main_sheet
                   # Only consider stressors applicable to Population model
                   ms_stress <- ms_stress[which(ms_stress$Model %in% c("Population Model", "All")), ]
                   
                   ms_stress_list <- list()
                   
                   # All variables off by default
                   ms_stress$check_on <- FALSE
                   ms_stress$HUC_ID <- 123
                   ms_stress$NAME <- "NAME"
                   ms_stress$Mean <- NA
                   ms_stress$SD <- NA
                   ms_stress$Distribution <- "normal"
                   ms_stress$Low_Limit <- NA
                   ms_stress$Up_Limit <- NA
                   
                   isolate({
                     rv_sandbox_stressors$dat <- ms_stress
                   })

                   # Call sub modules
                   for(s in 1:nrow(ms_stress)) {
                     this_stressor <- ms_stress[s, ]
                     this_stressor_name <- this_stressor$Stressors
                     print(this_stressor$Stressors)
                     ms_stress_list[[s]] <- module_matrix_model_preview_stress_ui(ns(this_stressor_name))
                     module_matrix_model_preview_stress_server(this_stressor_name, stressor_variable = this_stressor)
                   }
                   return(ms_stress_list)
                   
                 })
                 
                 
                 
                 
                 
                 #-------------------------------------------------------
                 # Run a sample projection...
                 #-------------------------------------------------------
                 observeEvent(input$demo_projection, {
                   
                   # Show a loading spinner to the user
                   show_modal_spinner(
                     spin = "hollow-dots",
                     color = "#0073b7",
                     text = paste0("Running a time series projection...")
                   )
                   
                   
                   # -----------------------------------
                   # Gather inputs in isolate mode
                   isolate({
                     
                     # Modal is hidden or reset
                     rv_show_sample_plot$open <- FALSE
                     test_n_years <- input$test_n_years
                     test_n_replicates <- input$test_n_replicates
                     dat <- rv_life_stages$dat
                     
                     # Gather the environmental stressors (if any)
                     CE_df <- rv_sandbox_stressors$dat
                     
                     
                     if(length(CE_df) == 0) {
                       CE_df <- data.frame()
                     } else {
                       # ---------------------------------
                       # See if any enviro stressors present
                       CE_df <- CE_df[CE_df$check_on, ]
                     }

                     
                     if(nrow(CE_df) >= 1) {
                       
                       # Thin down stressors to target...
                       sr <- list()
                       sr$main_sheet <- rv_stressor_response$main_sheet
                       sr$stressor_names <- rv_stressor_response$stressor_names
                       sr$sr_dat <- rv_stressor_response$sr_dat
                       # Thin down...
                       sr$main_sheet <- sr$main_sheet[which(sr$main_sheet$Stressors %in% CE_df$Stressors), ]
                       sr$stressor_names <- sr$stressor_names[sr$stressor_names %in% CE_df$Stressors]
                       sr$sr_dat <- sr$sr_dat[which(names(sr$sr_dat) %in%  CE_df$Stressors)]
                       
                       # Stressor Magnitude...
                       # Make up dummy data for a sample watershed
                       smw_sample <- data.frame(HUC_ID = CE_df$HUC_ID, NAME = CE_df$NAME,
                                                Stressor = CE_df$Stressors, Stressor_cat = CE_df$Stressor_cat,
                                                Mean = CE_df$Mean, SD = CE_df$SD, Distribution = CE_df$Distribution,
                                                Low_Limit = CE_df$Low_Limit, Up_Limit = CE_df$Up_Limit)
                       
                       # browser()
                       jm <- JoeModelCE::JoeModel_Run(
                         dose = smw_sample,
                         sr_wb_dat = sr,
                         MC_sims = test_n_replicates,
                         adult_sys_cap = FALSE
                       )

                       # Gather summary at stressor level
                       dobj <- jm$sc.dose.df
                       
                       # add on missing attr columns
                       merge_cols <- CE_df[, c("Stressors", "Life_stages", "Parameters", "Stressor_cat")]
                       
                       m_all <- merge(merge_cols, dobj, by.x = "Stressors", by.y = "Stressor", all.x = TRUE, all.y = TRUE)
                       # Fix col names
                       colnames(m_all)[colnames(m_all) == "Stressors"] <- "Stressor"
                       colnames(m_all)[colnames(m_all) == "Life_stages"] <- "life_stage"
                       colnames(m_all)[colnames(m_all) == "Parameters"] <- "parameter"
                       colnames(m_all)[colnames(m_all) == "Stressor_cat"] <- "Stressor_cat"

                       # Return cleaned object
                       CE_df <- m_all
                       
                     } else {
                       # No enviro stressors
                       CE_df <- NULL
                     }
                     
                   })
                   # end of isolate
                   # -------------------------------------------
                   
                   print("RUN A SAMPLE POP PROJECTION...")

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
                   
                   for (ii in 1:test_n_replicates) {
                     
                     
                     # Environmental sample for this rep
                     if(is.null(CE_df)) {
                       CE_df_rep <- CE_df
                     } else {
                       CE_df_rep <- CE_df[ii, ]
                     }

                     
                     # Run simple population projection - project forward through time
                     baseline <-
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
                     # Gather
                     all_outputs[[ii]] <- baseline
                   }
                   
                   # Get the run counter
                   run_counter <-
                     rv_pop_sample_plot_data$run_counter
                   
                   # Send to reactive object
                   rv_pop_sample_plot_data$dat[[run_counter]] <-
                     all_outputs
                   
                   # Update the run counter
                   rv_pop_sample_plot_data$run_counter <-
                     run_counter + 1
                   
                   # Open the modal
                   rv_show_sample_plot$open <- TRUE
                   
                   
                 })
                 
                 
                 
                 
                 
                 
                 
                 
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
                   

                   # Get the current run
                   # (first time of is there a previous run to show)
                   isolate({
                     # In isolate no not trigger
                     crun <- rv_pop_sample_plot_data$run_counter
                     # Correction: counter is updated
                     crun <- crun - 1
                   })
                   
                   # Get data for current run (from above)
                   pdat <- rv_pop_sample_plot_data$dat[[crun]]
                   
                   # Which data frame should be sources from the results object
                   # either counts of individuals N or lambdas
                   if(plot_type == "adult" | plot_type == "subadult" | plot_type == "juv" | plot_type == "yoy" | plot_type == "allstage") {
                     t_var <- "N"
                   }
                   if(plot_type == "lambda") {
                     t_var <- "lambdas"
                   }
                   
                   # Extract target object from bootstrap replicate runs
                   get_n_obj <- function(obj, name = "") {
                     obj <- obj[[name]]
                     obj <- as.data.frame(obj)
                     obj$year <- 1:nrow(obj)
                     return(obj)
                   }
                   
                   # Mege to a single clean data frame
                   pdata_1 <- lapply(pdat, get_n_obj, name = t_var)
                   pdata_1 <- do.call("rbind", pdata_1)
                   # Our current simulation will be coded as one
                   # Out previous simulation will be coded as two (if available)
                   pdata_1$sim <- "one"
                   
                   
                   
                   # ---------------------------------------------------
                   # If we are plotting allstage generate the plot here
                   # and do not proceed further
                   if(plot_type == "allstage") {
                     pdata_1$sim <- NULL # Not showing current previous
                     # Reshape and relabel

                     p2 <- reshape2::melt(pdata_1, id = "year")
                     p2$stage <- as.character(p2$variable)
                     p2$stage <- ifelse(p2$stage == "V1", "Stage 1", p2$stage)
                     p2$stage <- ifelse(p2$stage == "V2", "Stage 2", p2$stage)
                     p2$stage <- ifelse(p2$stage == "V3", "Stage 3", p2$stage)
                     p2$stage <- ifelse(p2$stage == "V4", "Stage 4", p2$stage)
                     # Summarize averages by life stage
                     p3 <- p2 %>% dplyr::group_by(year, stage) %>% summarise(
                       mean = mean(value, na.rm = TRUE),
                       sd = sd(value, na.rm = TRUE)
                     )
                     p3$lwr <- p3$mean - p3$sd
                     p3$upr <- p3$mean + p3$sd
                     # start plot
                     p <-
                       ggplot(data = p3, aes(
                         x = year,
                         y = mean,
                         group = stage,
                         color = stage
                       )) +
                       scale_y_continuous(limits = c(0, max(p3$upr))) +
                       geom_line() +
                       ggtitle("All life stages") +
                       xlab("Simulation Year") + ylab("N")
                     p <- p + geom_ribbon(data = p3, aes(ymin = lwr, ymax = upr), alpha = 0.1) + theme_bw()
                     return(p)
                   }
                   
                   
                   # ---------------------------------------------
                   # Generate plot for all life stages here (& lambda)...
                   # ---------------------------------------------
                   
                   # Gather values for previous run
                   if(crun > 1) {
                     # Data for previous runs are available - compile here.
                     pdat_old <- rv_pop_sample_plot_data$dat[[crun - 1]]
                     pdata_1_old <- lapply(pdat_old, get_n_obj, name = t_var)
                     pdata_1_old <- do.call("rbind", pdata_1_old)
                     pdata_1_old$sim <- "two"
                     # Merge previous and current
                     pdata_1 <- rbind(pdata_1, pdata_1_old)
                   } else {
                     print("Run crun:")
                     print(crun)
                   }
                   

                   # Min value on y-axis will be zero unless plotting lambdas which hover around 1
                   my_y_min <- 0
                   
                   # Switch to target variable
                   # Set plot titles
                   if(plot_type == "yoy") {
                     pdata_1$value <- pdata_1$V1
                     mtitle <- "YoY/Fry Abundance"
                     y_axe <- "N"
                   }
                   if(plot_type == "juv") {
                     pdata_1$value <- pdata_1$V2
                     mtitle <- "Juvenile Abundance"
                     y_axe <- "N"
                   }
                   if(plot_type == "subadult") {
                     pdata_1$value <- pdata_1$V3
                     mtitle <- "Sub-Adult Abundance"
                     y_axe <- "N"
                   }
                   if(plot_type == "adult") {
                     pdata_1$value <- pdata_1$V4
                     mtitle <- "Adult Abundance"
                     y_axe <- "N"
                   }
                   if(plot_type == "lambda") {
                     pdata_1$value <- pdata_1$obj
                     mtitle <- "Lambda Values (adults)"
                     y_axe <- "lambda (adults)"
                     my_y_min <- min(pdata_1$value)
                   }
                   
                   
                   # Summarize data by yearly averages across simulations with SD values
                   pdata_2 <-
                     pdata_1 %>% dplyr::group_by(sim, year) %>% summarise(mean = mean(value, na.rm = TRUE),
                                                                     sd = sd(value, na.rm = TRUE))
                   
                   pdata_2$lwr <- pdata_2$mean - pdata_2$sd
                   pdata_2$upr <- pdata_2$mean + pdata_2$sd
                   
                   pdata_2$lwr <- ifelse(is.na(pdata_2$lwr), 0, pdata_2$lwr)
                   pdata_2$lwr <- ifelse(pdata_2$lwr < 0, 0, pdata_2$lwr)
                   pdata_2$upr <- ifelse(is.na(pdata_2$upr), (mean(pdata_2$mean, na.rm = TRUE) + mean(pdata_2$sd, na.rm = TRUE)), pdata_2$upr)

                   p3 <- pdata_2
                   p3$Simulation <- "Current"
                   p3$Simulation <- ifelse(p3$sim == "one", "Current", "Previous")
                   
                   # Create the ggplot plotting object
                   p <-
                     ggplot(data = p3, aes(
                       x = year,
                       y = mean,
                       group = Simulation,
                       color = Simulation
                     )) +
                     scale_y_continuous(limits = c(my_y_min, max(p3$upr))) +
                     geom_line() +
                     ggtitle(mtitle) +
                     xlab("Simulation Year") + ylab(y_axe)
                   
                   p <- p + geom_ribbon(data = p3, aes(ymin = lwr, ymax = upr), alpha = 0.1) + theme_bw()
                   
                   return(p)

                   
                 })

                 
                 
                 #-------------------------------------------------------
                 # Open the demo projection modal
                 #-------------------------------------------------------
                 observeEvent(rv_show_sample_plot$open, {
                   showModal(
                     modalDialog(
                       title = "Sample Time Series Projection",
                       
                       tagList(
                         
                         radioButtons(ns("samp_plot_type"), "Plot type:",
                                      c("Stage 4" = "adult",
                                        "Stage 3" = "subadult",
                                        "Stage 2" = "juv",
                                        "Stage 1" = "yoy",
                                        "Lambda" = "lambda",
                                        "All Life Stages" = "allstage"),
                                      inline  = TRUE),
                         
                         
                         plotlyOutput(ns(
                         "samp_adults"
                       ))),
                       
                       easyClose = TRUE,
                       size = "l",
                       footer = NULL
                     ),
                   )
                 }, ignoreInit = TRUE)
                 
                 
                 
                 
               })
}