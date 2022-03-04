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
        numericInput(ns("njhjhkum"), label = "Temperature_adult", value = 0),
        numericInput(ns("jlkjlk"), label = "Temperature_parr", value = 0),
        numericInput(ns("uiou"), label = "Total_Mortality", value = 0),
        numericInput(ns("ljlkjl"), label = "Habitat_loss", value = 0),
        numericInput(ns("nmmn"), label = "Spring_flow_alevin", value = 0),
        numericInput(ns("ssdfaf"), label = "Spring_flow_sub", value = 0),
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
                 
                 
                 
                 
                 
                 
                 
                 
                 #-------------------------------------------------------
                 # Run a sample projection...
                 #-------------------------------------------------------
                 observeEvent(input$demo_projection, {
                   isolate({
                     rv_show_sample_plot$open <- FALSE
                     test_n_years <- input$test_n_years
                     test_n_replicates <- input$test_n_replicates
                     dat <- rv_life_stages$dat
                     
                   })
                   
                   
                   print("RUN A SAMPLE POP PROJECTION...")
                   # Gather population model inputs
                   # Setup objects for population model
                   pop_mod_setup <-
                     pop_model_setup(life_cycles = dat)
                   # Build matrix elements for population model
                   pop_mod_mat <-
                     pop_model_matrix_elements(pop_mod_setup = pop_mod_setup)
                   
                   
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
                     # Run simple population projection - project forward through time
                     baseline <-
                       Projection_DD(
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
                 
                 
                 
                 
                 
                 # --------------------------------------
                 # Render plots for preview modal
                 # --------------------------------------
                 
                 output$samp_adults <- renderPlotly({
                   print("Plotting adults...")
                   
                   plot_type <- input$samp_plot_type
                   
                   #browser()
                   
                   # Get the current run
                   isolate({
                     crun <- rv_pop_sample_plot_data$run_counter
                   })
                   crun <- crun - 1

                  
                   # Get data for current
                   pdat <- rv_pop_sample_plot_data$dat[[crun]]
                   
                   # Switch to target variable
                   if(plot_type == "adult" | plot_type == "subadult" | plot_type == "juv" | plot_type == "yoy") {
                     t_var <- "N"
                   }
                   
                   if(plot_type == "lambda") {
                     t_var <- "lambdas"
                   }
                   
                   
                   
                   get_n_obj <- function(obj, name = "") {
                     obj <- obj[[name]]
                     obj <- as.data.frame(obj)
                     obj$year <- 1:nrow(obj)
                     obj
                   }
                   
                   pdata_1 <- lapply(pdat, get_n_obj, name = t_var)
                   pdata_1 <- do.call("rbind", pdata_1)
                   pdata_1$sim <- "one"
                   
                   
                   # Gather values for previous run
                   if(crun > 1) {
                     pdat_old <- rv_pop_sample_plot_data$dat[[crun - 1]]
                     pdata_1_old <- lapply(pdat_old, get_n_obj, name = t_var)
                     pdata_1_old <- do.call("rbind", pdata_1_old)
                     pdata_1_old$sim <- "two"
                     
                     # Merge 
                     pdata_1 <- rbind(pdata_1, pdata_1_old)
                     
                   }
                   
                   my_y_min <- 0
                   
                   # Switch to target variable
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
                   

                   

                   # Summarize by year
                   pdata_2 <-
                     pdata_1 %>% dplyr::group_by(sim, year) %>% summarise(mean = mean(value, na.rm = TRUE),
                                                                     sd = sd(value, na.rm = TRUE))
                   
                   pdata_2$lwr <- pdata_2$mean - pdata_2$sd
                   pdata_2$upr <- pdata_2$mean + pdata_2$sd

                   p3 <- pdata_2
                   p3$Simulation <- "Current"
                   p3$Simulation <- ifelse(p3$sim == "one", "Current", "Previous")
                   
                   

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
                   
                   p

                   
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
                                      c("Adults" = "adult",
                                        "Sub-Adults" = "subadult",
                                        "Juveniles" = "juv",
                                        "YoY" = "yoy",
                                        "All Life Stages" = "allstage",
                                        "Lambda" = "lambda")),
                         
                         
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