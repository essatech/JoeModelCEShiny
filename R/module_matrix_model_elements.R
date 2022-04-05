#' Matrix Model Elements UI
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
module_matrix_model_elements_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    shinydashboard::box(
      width = 12,
      
      tags$b("Density-Independent Components", style = "text-align: center;"),
      
      tags$table(class = "lam_v", style = "width: 100%;",
                 uiOutput(ns(
                   "dens_independent_comp"
                 ))),
      
      tags$br(),
      
      tags$div(class = "lam_bb",
               actionButton(ns("eigen_analysis"), "Eigen Analysis"))
      
    ),
    
    shinydashboard::box(
      width = 12,
      
      tags$b("Density-Dependent Components", style = "text-align: center;"),
      
      tags$table(
        class = "",
        style = "width: 100%;",
        tags$tr(
          tags$td("Adult K", style = "vertical-align: middle;"),
          tags$td(textOutput(ns("print_adult_k")), class = "pm-cell-values")
        ),
      ),
      
      tags$br(),
      
      tags$b("Model Components", style = "text-align: center;"),
      
      tags$div(class = "lam_bb",
               actionButton(
                 ns("model_component"), "Model Components"
               )),
      
    ),
    
    
    
  )
  
}


#' Matrix Model Elements SERVER
#'
#' @param none
#'
#' @return None
#'
module_matrix_model_elements_server <- function(id) {
  moduleServer(id,
               function(input, output, session) {
                 ns <- session$ns
                 
                 print("matrix model elements server")
                 
                 
                 
                 # Copy adult k to panel
                 output$print_adult_k <- renderText({
                   dat <- rv_life_stages$dat
                   val <- dat$Value[which(dat$Name == "k")]
                   return(val)
                 })
                 
                 
                 #-------------------------------------------------------
                 # Density-Independent Components
                 #-------------------------------------------------------
                 observe({
                   
                   print("pop mod dens-indepent matrix elements...")
                   
                   
                   # Reset assume we are clear of errors ...
                   isolate({
                     rv_ea_errors$possible_error_state <- FALSE
                     rv_ea_errors$possible_error_msg <- ""
                   })
                   
                   print("pop mod setup started...")
                   
                   # Make sure we actually have the data
                   req(rv_life_stages$dat)
                   
                   # Gather population model inputs
                   dat <- rv_life_stages$dat
                   
                   # Setup objects for population model
                   pop_mod_setup <-
                     JoeModelCE::pop_model_setup(life_cycles = dat)
                   
                   if (pop_mod_setup$possible_error_state != "All Good") {
                     
                     print("Bad error settings")
                     rv_ea_errors$possible_error_state <- TRUE
                     rv_ea_errors$possible_error_msg <-
                       pop_mod_setup$possible_error_state
                     
                   } else {
                     
                     print("Parameters ok...")
                     # Build matrix elements for population model
                     pop_mod_mat <-
                       JoeModelCE::pop_model_matrix_elements(pop_mod_setup = pop_mod_setup)
                     
                     # Preview density-independent transition projection_matrix
                     A <- pop_mod_mat$projection_matrix
                     # Assign nicknames for each stage
                     snames <-
                       c("egg_yoy", "juv", "subadult", "adult")
                     rownames(A) <- colnames(A) <- snames
                     # Simple density-independent lambda estimate
                     lambda <- popbio::lambda(A)
                     # Simple Eigen analysis
                     ea <- popbio::eigen.analysis(A)
                     
                     lambda <- round(ea$lambda1, 2)
                     damping_ratio <- round(ea$damping.ratio, 2)
                     gen_time <-
                       round(popbio::generation.time(A), 1)
                     net_repo_rate <-
                       round(popbio::net.reproductive.rate(A), 2)
                     
                     
                     # Check out symbolic matrix representations
                     # For density
                     ds <- pop_mod_setup$density_stage_symbolic
                     ds_m <-
                       matrix(as.character(ds), nrow = 4, ncol = 4)
                     ds_m <- t(ds_m)
                     colnames(ds_m) <- c("s1", "s2", "s3", "s4")
                     rownames(ds_m) <- c("s1", "s2", "s3", "s4")
                     
                     # For life stages
                     lss <- pop_mod_setup$life_stages_symbolic
                     lss_m <-
                       matrix(as.character(lss), nrow = 4, ncol = 4)
                     lss_m <- t(lss_m)
                     colnames(lss_m) <- c("s1", "s2", "s3", "s4")
                     rownames(lss_m) <- c("s1", "s2", "s3", "s4")
                     
                     
                     # Add objects to list
                     rv_eigen_analysis$dat$lambda <- lambda
                     rv_eigen_analysis$dat$damping_ratio <-
                       damping_ratio
                     rv_eigen_analysis$dat$gen_time <- gen_time
                     rv_eigen_analysis$dat$net_repo_rate <-
                       net_repo_rate
                     rv_eigen_analysis$dat$ea <- ea
                     rv_eigen_analysis$dat$pop_mod_mat <-
                       pop_mod_mat
                     rv_eigen_analysis$dat$lss_m <- lss_m
                     rv_eigen_analysis$dat$ds_m <- ds_m
                     
                   }
                   

                 })
                 
                 
                 # Calculate density-independent matrix elements...
                 output$dens_independent_comp <- renderUI({
                   print("Building DI comp...")
                   
                   if (rv_ea_errors$possible_error_state) {
                     # error state - return error message
                     tl <-
                       tags$tr(tags$td(rv_ea_errors$possible_error_msg), class = "pm-bad-inputs")
                     return(tl)
                   } else {
                     tl <- tagList(
                       tags$tr(
                         tags$td("Lambda: ", style = "vertical-align: middle;"),
                         tags$td(rv_eigen_analysis$dat$lambda, class = "pm-cell-values")
                       ),
                       tags$tr(
                         tags$td("Damping Ratio: ", style = "vertical-align: middle;"),
                         tags$td(rv_eigen_analysis$dat$damping_ratio, class = "pm-cell-values")
                       ),
                       tags$tr(
                         tags$td("Generation Time: ", style = "vertical-align: middle;"),
                         tags$td(rv_eigen_analysis$dat$gen_time, class = "pm-cell-values")
                       ),
                       tags$tr(
                         tags$td("Net Reproductive Rate: ", style = "vertical-align: middle;"),
                         tags$td(rv_eigen_analysis$dat$net_repo_rate, class = "pm-cell-values"),
                       )
                     )
                     return(tl)
                   }
                 })
                 
                 
                 
                 # Symbolic Transition matrix data table
                 output$dt_lss_m <- renderDataTable({
                   print("Building DT1...")
                   
                   # Get the transition matrix
                   A <- (rv_eigen_analysis$dat$lss_m)
                   # Add names to column
                   mnames <-
                     c("Egg / YoY", "Juveniles", "Sub-Adults", "Adults")
                   colnames(A) <- mnames
                   rownames(A) <- mnames
                   # Build the JS DT Data Table Object
                   DT::datatable(
                     A,
                     editable =  FALSE,
                     caption = "Symbolic Representation of the Transition matrix (B)",
                     filter = "none",
                     selection = "single",
                     rownames = TRUE,
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
                 
                 
                 # Symbolic Transition matrix data table
                 output$dt_ds_m <- renderDataTable({
                   print("Building DT2...")
                   
                   # Get the transition matrix
                   A <- (rv_eigen_analysis$dat$ds_m)
                   # Add names to column
                   mnames <-
                     c("Egg / YoY", "Juveniles", "Sub-Adults", "Adults")
                   colnames(A) <- mnames
                   rownames(A) <- mnames
                   # Build the JS DT Data Table Object
                   DT::datatable(
                     A,
                     editable =  FALSE,
                     caption = "Symbolic Representation of the Density-Dependence Matrix (D)",
                     filter = "none",
                     selection = "single",
                     rownames = TRUE,
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
                 
                 
                 
                 # Transition matrix data table
                 output$dt_transition_matrix <- renderDataTable({
                   print("Building DT3...")
                   
                   # Get the transition matrix
                   A <-
                     round(rv_eigen_analysis$dat$pop_mod_mat$projection_matrix,
                           3)
                   # Add names to column
                   mnames <-
                     c("Egg / YoY", "Juveniles", "Sub-Adults", "Adults")
                   colnames(A) <- mnames
                   rownames(A) <- mnames
                   # Build the JS DT Data Table Object
                   DT::datatable(
                     A,
                     editable =  FALSE,
                     caption = "Transition matrix",
                     filter = "none",
                     selection = "single",
                     rownames = TRUE,
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
                 
                 # Sensitivities matrix data table
                 output$dt_sensitivities_matrix <- renderDataTable({
                   print("Building DT4...")
                   
                   A2 <-
                     round(rv_eigen_analysis$dat$ea$sensitivities, 3)
                   mnames <-
                     c("Egg / YoY", "Juveniles", "Sub-Adults", "Adults")
                   colnames(A2) <- mnames
                   rownames(A2) <- mnames
                   DT::datatable(
                     A2,
                     editable =  FALSE,
                     caption = "Sensitivity Matrix",
                     filter = "none",
                     selection = "single",
                     rownames = TRUE,
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
                 
                 # elasticities matrix data table
                 output$dt_elasticities_matrix <- renderDataTable({
                   print("Building DT5...")
                   
                   A3 <-
                     round(rv_eigen_analysis$dat$ea$elasticities, 3)
                   mnames <-
                     c("Egg / YoY", "Juveniles", "Sub-Adults", "Adults")
                   colnames(A3) <- mnames
                   rownames(A3) <- mnames
                   DT::datatable(
                     A3,
                     editable =  FALSE,
                     caption = "Elasticities Matrix",
                     filter = "none",
                     selection = "single",
                     rownames = TRUE,
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
                 
                 # other matrix data table
                 output$dt_other_matrix <- renderDataTable({
                   print("Building DT6...")
                   
                   repro <-
                     round(rv_eigen_analysis$dat$ea$repro.value, 2)
                   ss <-
                     round(rv_eigen_analysis$dat$ea$stable.stage, 2)
                   repro <- data.frame(t(repro))
                   ss <- data.frame(t(ss))
                   repro_ss <- rbind(repro, ss)
                   colnames(repro_ss) <-
                     c("Egg / YoY", "Juveniles", "Sub-Adults", "Adults")
                   rownames(repro_ss) <-
                     c("Repro. Value", "Stable Stage")
                   DT::datatable(
                     repro_ss,
                     editable =  FALSE,
                     caption = "Reproductive Values & Stable Stage",
                     filter = "none",
                     selection = "single",
                     rownames = TRUE,
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
                 
                 output$lambda_txt <-
                   renderText({
                     paste0("Lambda: ", rv_eigen_analysis$dat$lambda)
                   })
                 
                 
                 
                 #-------------------------------------------------------
                 # Density-Independent Modal
                 #-------------------------------------------------------
                 observeEvent(input$eigen_analysis, {
                   print("Eigen Analysis...")
                   
                   showModal(
                     modalDialog(
                       title = "Full Matrix Eigen Analysis",
                       tagList(
                         tags$p(
                           "The following tables represent outputs from an eigen analysis of the stage-structured matrix model. Note that these values are only relevant for density-independent growth conditions and will be misleading if not interpreted alongside density-dependent constraints."
                         ),
                         
                         tags$p(textOutput(ns("lambda_txt"))),
                         tags$p(
                           "The Lambda value represents the intrinsic population growth rate (at stable stage & equilibrium conditions). Lambda values greater than 1.0 indicate the population will increase and lambda values less than one indicate the population will decrease."
                         ),
                         
                         
                         tags$p(
                           "The following table shows the transition matrix for density-independent growth. The values represented here are adjusted for survival, growth, reproduction & the sex ratio, but they do not consider density-dependent constraints on population growth."
                         ),
                         DT::dataTableOutput(ns("dt_transition_matrix")),
                         tags$br(),
                         
                         tags$p(
                           "The next table shows the sensitivities matrix. What effect does an absolute change in a vital rate have on lambda? For example, if we change first-year survival by 0.001, how much will that affect the population growth rate?"
                         ),
                         DT::dataTableOutput(ns("dt_sensitivities_matrix")),
                         tags$br(),
                         
                         tags$p(
                           "The next table shows the elasticities matrix. What effect does a proportional change in vital rate have on population growth. For example, if we change first-year survival by 1%, how much will that affect population growth?"
                         ),
                         DT::dataTableOutput(ns("dt_elasticities_matrix")),
                         tags$br(),
                         
                         tags$p(
                           "The next table shows the reproductive value and stable stage distribution for each life stage. The reproductive value shows the value of a given stage as a seed for population growth (the first age class has a reproductive value of 1.0 by definition). The stable stage distribution represents the proportion of the population in each stage under hypothetical non-stochastic density-independent growth conditions (e.g., what proportion of the total population are juvenile, adults etc.?)."
                         ),
                         DT::dataTableOutput(ns("dt_other_matrix")),
                         tags$br(),
                         
                       ),
                       easyClose = TRUE,
                       size = "l",
                       footer = NULL
                     )
                   )
                 })
                 
                 
                 
                 
                 #-------------------------------------------------------
                 # Density-Dependent Modal
                 #-------------------------------------------------------
                 observeEvent(input$model_component, {
                   print("Model Component...")
                   
                   showModal(
                     modalDialog(
                       title = "Model Component",
                       tagList(
                         tags$p(
                           "Under density-dependent growth conditions the projection matrix (A) is the product of the transition matrix (B), consisting of the life history charactersitcis, and the density-dependce matrix (D). A = B*D."
                         ),
                         DT::dataTableOutput(ns("dt_lss_m")),
                         DT::dataTableOutput(ns("dt_ds_m")),
                       ),
                       easyClose = TRUE,
                       size = "l",
                       footer = NULL
                     ),
                   )
                 })
                 
                 
                 
               })
}