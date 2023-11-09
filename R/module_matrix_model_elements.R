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
                 ns("model_component"), "Symbolic Representations"
               )),
      
      
      tags$b("Compensation Ratios", style = "text-align: center;"),
      
      tags$div(class = "lam_bb",
               actionButton(
                 ns("compensation_ratios"),
                 "Compensation Ratios (explainer)"
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
                   print("print_adult_k...")
                   dat <- session$userData$rv_life_stages$dat
                   val <- dat$Value[which(dat$Name == "k")]
                   return(val)
                 })
                 
                 
                 
                 # Beverton-Holt Sample Plot
                 output$bh_plot <- renderPlot({
                   bh <- function(K = NA,
                                  surv = NA,
                                  Nt = NA) {
                     Nt2 <- (surv * Nt) / (1 + Nt * (surv/K))
                     return(Nt2)
                   }
                   
                   surv <- 0.8
                   K <- 100
                   Nt <- seq(0, 1500, by = 5)
                   res <- lapply(Nt, bh, K = K, surv = surv)
                   Nt2 <- unlist(res)
                   plot(
                     Nt,
                     Nt2,
                     type = 'l',
                     xlim = c(0, 1000),
                     ylim = c(0, 100),
                     xlab = expression('N'[' i, t']),
                     ylab = expression('N'[' i, t+1']),
                     sub = "BH function with K = 100 and productivity = 0.8"
                   )
                   abline(0,
                          surv,
                          lty = 3,
                          col = "red",
                          lwd = 1.5)
                   abline(
                     h = K,
                     lty = 2,
                     col = "blue",
                     lwd = 1.5
                   )
                   # abline(
                   #   h = K * surv,
                   #   lty = 4,
                   #   col = "green",
                   #   lwd = 1.5
                   # )
                 })
                 
                 
                 
 
                 
                 # Compensation Ratio Sample Plot
                 output$cr_samp_plot <- renderPlot({
                   # return(expression(cr / (1 + (cr - 1) * (x / k))))
                   crf <-
                     function(CR = NA,
                              Nt = NA,
                              K = NA,
                              surv = NA) {
                       val <- (CR * surv) / (1 + (CR - 1) * (Nt / K))
                       val <- ifelse(val > 1, 1, val)
                     }
                   
                   surv <- 0.8
                   K <- 100
                   Nt <- seq(0, 500, by = 5)
                   CR <- 1
                   res <-
                     lapply(Nt,
                            crf,
                            CR = CR,
                            K = K,
                            surv = surv)
                   CR_adj_1 <- unlist(res)
                   
                   CR <- 1.1
                   res <-
                     lapply(Nt,
                            crf,
                            CR = CR,
                            K = K,
                            surv = surv)
                   CR_adj_1.1 <- unlist(res)
                   
                   CR <- 2
                   res <-
                     lapply(Nt,
                            crf,
                            CR = CR,
                            K = K,
                            surv = surv)
                   CR_adj_2 <- unlist(res)
                   
                   CR <- 5
                   res <-
                     lapply(Nt,
                            crf,
                            CR = CR,
                            K = K,
                            surv = surv)
                   CR_adj_5 <- unlist(res)
                   
                   CR <- 1.3
                   res <-
                     lapply(Nt,
                            crf,
                            CR = CR,
                            K = K,
                            surv = surv)
                   CR_adj_1.3 <- unlist(res)
                   
                   CR <- 0.97
                   res <-
                     lapply(Nt,
                            crf,
                            CR = CR,
                            K = K,
                            surv = surv)
                   CR_adj_0.9 <- unlist(res)
                   
                   plot(
                     Nt,
                     CR_adj_1,
                     type = 'l',
                     xlim = c(5, 600),
                     ylim = c(0, 1),
                     xlab = expression('N'[' i, t']),
                     ylab = expression('Modified survival'[' i, t']),
                     sub = "CR function with K = 100 and productivity = 0.8"
                   )
                   points(Nt, CR_adj_1.1, type = 'l')
                   points(Nt, CR_adj_2, type = 'l')
                   points(Nt, CR_adj_5, type = 'l')
                   #points(Nt, CR_adj_1.3, type = 'l')
                   points(Nt, CR_adj_0.9, type = 'l', col = 'grey')
                   
                   text(550,
                        tail(CR_adj_1.1)[1],
                        labels = c("CR: 1.1"),
                        cex = 0.8)
                   text(550,
                        tail(CR_adj_2)[1],
                        labels = c("CR: 2"),
                        cex = 0.8)
                   text(550,
                        tail(CR_adj_5)[1],
                        labels = c("CR: 5"),
                        cex = 0.8)
                   text(550,
                        tail(CR_adj_1)[1],
                        labels = c("CR: 1.0"),
                        cex = 0.8)
                   #text(550, tail(CR_adj_1.3)[1], labels = c("CR: 1.3"), cex = 0.8)
                   text(550,
                        tail(CR_adj_0.9)[1],
                        labels = c("CR: 0.97"),
                        cex = 0.8)
                   
                   
                   abline(
                     h = 0.8,
                     lty = 3,
                     col = "red",
                     lwd = 1.5
                   )
                   abline(
                     v = K,
                     lty = 2,
                     col = "blue",
                     lwd = 1.5
                   )
                   abline(
                     h = K * surv,
                     lty = 4,
                     col = "green",
                     lwd = 1.5
                   )
                 })
                 
                 
                 
                 
                 
                 
                 #-------------------------------------------------------
                 # Density-Independent Components
                 #-------------------------------------------------------
                 observe({
                   print("pop mod dens-indepent matrix elements...")
                   
                   
                   # Reset assume we are clear of errors ...
                   isolate({
                     session$userData$rv_ea_errors$possible_error_state <- FALSE
                     session$userData$rv_ea_errors$possible_error_msg <- ""
                   })
                   
                   print("pop mod setup started...")
                   
                   # Make sure we actually have the data
                   req(session$userData$rv_life_stages$dat)
                   
                   # Gather population model inputs
                   dat <- session$userData$rv_life_stages$dat
                   
                   # Setup objects for population model
                   pop_mod_setup <-
                     JoeModelCE::pop_model_setup(life_cycles = dat)
                   
                   if (pop_mod_setup$possible_error_state != "All Good") {
                     print("Bad error settings")
                     session$userData$rv_ea_errors$possible_error_state <- TRUE
                     session$userData$rv_ea_errors$possible_error_msg <-
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
                     session$userData$rv_eigen_analysis$dat$lambda <- lambda
                     session$userData$rv_eigen_analysis$dat$damping_ratio <-
                       damping_ratio
                     session$userData$rv_eigen_analysis$dat$gen_time <- gen_time
                     session$userData$rv_eigen_analysis$dat$net_repo_rate <-
                       net_repo_rate
                     session$userData$rv_eigen_analysis$dat$ea <- ea
                     session$userData$rv_eigen_analysis$dat$pop_mod_mat <-
                       pop_mod_mat
                     session$userData$rv_eigen_analysis$dat$lss_m <- lss_m
                     session$userData$rv_eigen_analysis$dat$ds_m <- ds_m
                     
                     
                     
                     
                     
                     
                   }
                   
                   
                 })
                 
                 
                 # Calculate density-independent matrix elements...
                 output$dens_independent_comp <- renderUI({
                   print("Building DI comp...")
                   
                   if (session$userData$rv_ea_errors$possible_error_state) {
                     # error state - return error message
                     tl <-
                       tags$tr(tags$td(session$userData$rv_ea_errors$possible_error_msg), class = "pm-bad-inputs")
                     return(tl)
                   } else {
                     tl <- tagList(
                       tags$tr(
                         tags$td("Lambda: ", style = "vertical-align: middle;"),
                         tags$td(session$userData$rv_eigen_analysis$dat$lambda, class = "pm-cell-values")
                       ),
                       tags$tr(
                         tags$td("Damping Ratio: ", style = "vertical-align: middle;"),
                         tags$td(session$userData$rv_eigen_analysis$dat$damping_ratio, class = "pm-cell-values")
                       ),
                       tags$tr(
                         tags$td("Generation Time: ", style = "vertical-align: middle;"),
                         tags$td(session$userData$rv_eigen_analysis$dat$gen_time, class = "pm-cell-values")
                       ),
                       tags$tr(
                         tags$td("Net Reproductive Rate: ", style = "vertical-align: middle;"),
                         tags$td(session$userData$rv_eigen_analysis$dat$net_repo_rate, class = "pm-cell-values"),
                       )
                     )
                     return(tl)
                   }
                 })
                 
                 
                 
                 
                 
                 # Symbolic Transition matrix data table
                 output$dt_lss_m <- renderDataTable({
                   print("Building DT1...")
                   
                   # Get the transition matrix
                   A <- (session$userData$rv_eigen_analysis$dat$lss_m)
                   # Add names to column
                   mnames <-
                     c("Stage 1", "Stage 2", "Stage 3", "Stage 4")
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
                   A <- (session$userData$rv_eigen_analysis$dat$ds_m)
                   # Add names to column
                   mnames <-
                     c("Stage 1", "Stage 2", "Stage 3", "Stage 4")
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
                     round(session$userData$rv_eigen_analysis$dat$pop_mod_mat$projection_matrix,
                           3)
                   # Add names to column
                   mnames <-
                     c("Stage 1", "Stage 2", "Stage 3", "Stage 4")
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
                     round(session$userData$rv_eigen_analysis$dat$ea$sensitivities, 3)
                   mnames <-
                     c("Stage 1", "Stage 2", "Stage 3", "Stage 4")
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
                     round(session$userData$rv_eigen_analysis$dat$ea$elasticities, 3)
                   mnames <-
                     c("Stage 1", "Stage 2", "Stage 3", "Stage 4")
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
                 output$dt_stablestage_matrix <- renderDataTable({
                   print("Building DT6...")
                   
                   repro <-
                     round(session$userData$rv_eigen_analysis$dat$ea$repro.value, 2)
                   ss <-
                     round(session$userData$rv_eigen_analysis$dat$ea$stable.stage, 2)
                   repro <- data.frame(t(repro))
                   ss <- data.frame(t(ss))
                   repro_ss <- rbind(repro, ss)
                   colnames(repro_ss) <-
                     c("Stage 1", "Stage 2", "Stage 3", "Stage 4")
                   rownames(repro_ss) <-
                     c("Repro. Value", "Stable Stage")
                   DT::datatable(
                     repro_ss,
                     editable =  FALSE,
                     caption = "Reproductive Values & Stable Stage Distribution (0 - 1)",
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
                 output$dt_stablestage_k <- renderDataTable({
                   print("Building dt_stablestage_k...")
                   
                   ss <- session$userData$rv_eigen_analysis$dat$ea$stable.stage
                   nstage <-
                     session$userData$rv_eigen_analysis$dat$pop_mod_mat$life_histories$Nstage
                   Ka <-
                     session$userData$rv_eigen_analysis$dat$pop_mod_mat$life_histories$Ka
                   
                   ss <- as.numeric(ss)
                   # evaluate whether we are quantifying the initial carrying capacities correctly
                   k_stage <- ss * Ka / ss[nstage]
                   ss <-
                     round(ss, 3)
                   ss <- data.frame(t(ss))
                   k_stage <-
                     round(k_stage, 0)
                   # Get K
                   # evaluate whether we are quantifying the initial carrying capacities correctly
                   
                   repro_ss <- rbind(ss, k_stage)
                   colnames(repro_ss) <-
                     c("Stage 1", "Stage 2", "Stage 3", "Stage 4")
                   rownames(repro_ss) <-
                     c("Stable Stage", "Stage Capacities K")
                   DT::datatable(
                     repro_ss,
                     editable =  FALSE,
                     caption = "Stable Stage Distributions (0 - 1) & Stage Capacities (K)",
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
                     paste0("Lambda: ", session$userData$rv_eigen_analysis$dat$lambda)
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
                         DT::dataTableOutput(ns("dt_stablestage_matrix")),
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
                 
                 
                 
                 
                 
                 #-------------------------------------------------------
                 # Compensation Ratios Modal
                 #-------------------------------------------------------
                 observeEvent(input$compensation_ratios, {
                   print("compensation_ratios modal ...")
                   
                   showModal(
                     modalDialog(
                       title = "Compensation Ratios",
                       tagList(
                         tags$p(
                           "It is rare for natural populations to grow in perpetuity without any constraints on grow, survival, and reproduction. Therefore, population models will typically include mechanism(s) to constrain population growth or sustained densities. Compensation ratios (CR values) are used in this tool to parameterize and govern density-dependent growth conditions. Compensation ratios (described below) are a re-parameterization of the classical Beverton-Holt function for density-dependent growth."
                         ),
                         tags$p(
                           "The Beverton-Holt function gives the expected number of individuals, in the subsequent time step (N at time + 1, or density) as a function of the number of individuals in the current time step (N at time). In the case of matrix population models, this relationship is expressed as the number of individuals transitioning between two stages (e.g., from stage 2 to stage 3) where density dependent constraints are present. In the Beverton-Holt function, governing parameters include an estimate of carrying capacity (K), a baseline estimate of survival (S) (productivity) for the transition probability (in the absence of any density-dependent effects) and the number of individuals in the current stage class (Nt)."
                         ),
                         withMathJax(),
                         helpText(
                           'Beverton-Holt function for density-dependent growth:
                                   $$N_{t+1}=\\frac{S \\cdot N_t}{1 + (\\frac{S}{K}) \\cdot N_t}$$'
                         ),
                         
                         fluidRow(
                           column(1),
                           column(
                             10,
                             align = "center",
                             plotOutput(ns("bh_plot")),
                             tags$p(
                               "Overview of the Beverton-Holt function showing the number of individuals at time (t) on the x-axis and the number of individuals at time + 1 one on the y-axis. The curved black line shows the effects of density dependant growth. The steep red line is the productivity of 0.8 under density-independent growth conditions. The blue line is the hypothetical caring capacity of 100, and the green line is the max achievable capacity calculated K (100) multiply by 0.8 = 80."
                             )
                           ),
                           column(1),
                         ),
                         tags$p(
                           "Compensation Ratios (CR) are used in the population modelling component of this tool as a modified version of the Beverton-Holt equation to adjust the survivorship of each life stage based on the observed densities (abundance, N i,t) and stage-specific carrying capacities (Ki):"
                         ),
                         
                         withMathJax(),
                         helpText(
                           'Compensation Ratio CR for life stage i:
                                   $$S_{i,t}=\\frac{S_{i,0} \\cdot w_i}{1 + \\frac{w_i - 1 \\cdot N_{i,t}}{K_i}}$$'
                         ),
                         
                         
                         tags$p(
                           "In the CR equation Si,0 is the baseline survivorship under density-independent growth conditions; wi is the compensation ratio (CR value) of life stage i; Ni,t is the current number of individuals in life stage i in a given time step (t); and Ki is the carrying capacity of life stage i. The compensation ratios, in essence, modify the survivorship of each life stage based on how far the stage-specific abundance (Ni,t) has departed from its assumed carrying capacity (Ki)."
                         ),
                         
                         tags$p(
                           "A plot of compensation ratios is provided below to illustrate their effects of stage-specific survivorship transitions. In this example abundance values of a hypothetical stage class (i) are plotted along the x-axis with a carrying capacity (Ki) set to 100 individuals (blue vertical line). The hypothetical stage class (i) has a baseline survivorship (productivity) value of 0.8 in the absence of density-dependent growth conditions (horizontal red line). The y-axis on the plot shows how the default survivorship value of 0.8 is modified based on the stage-specific compensation ratio for stage class (CR i). The survivorship value for the stage class is suppressed as the abundance values exceed the carrying capacity K. The effects are amplified as compensation ratios are increased. Compensation ratios of 1.0 leave the vital rate unmodified. Compensation ratios less than 1.0 increase survivorship values (allowing for a potential positive effect of density). When the abundance of the age class is less than the carrying capacity baseline survivorship values can actually be increase. However, within the model code adjusted survivorship values are fixed so that they never exceed 1.0 for any stage transition."
                         ),
                         
                         fluidRow(
                           column(1),
                           column(10, align = "center",
                                  plotOutput(ns(
                                    "cr_samp_plot"
                                  ))),
                           column(1)
                         ),
                         
                         tags$b("Carrying Capacity Estimates:"),
                         tags$p(
                           "It is also useful to understand how stage-specific carrying capacity values are estimated in the tool. Current, it is only possible to modify the carrying capacity for the adult age class (stage 4) via the K parameter in the inputs. The K values for other age classes are calculated from the stable stage distribution of the underlying transition matrix (B):"
                         ),
                         tags$ul(
                           tags$li(
                             "K (Stage-0, eggs): Calculated as the project of individuals in the mature age classes, multiplied by the maturation probability for each class, the number of spawning events, eggs per female, sex ration and spawning interval."
                           ),
                           tags$li(
                             "K (Stage-0, fry): K values for young-of-the-year (fry/Age-0) individuals is calculated as the produce of K eggs (above) * the egg survival (SE)."
                           ),
                           tags$li(
                             "K (Stage-1): Calculated from the stable-stage distribution of the transition matrix (B) after setting the adult stage (Stage-4) to K (e.g., 100)."
                           ),
                           tags$li(
                             "K (Stage-2): Calculated from the stable-stage distribution of the transition matrix (B) after setting the adult stage (Stage-4) to K (e.g., 100)."
                           ),
                           tags$li(
                             "K (Stage-3): Calculated from the stable-stage distribution of the transition matrix (B) after setting the adult stage (Stage-4) to K (e.g., 100)."
                           ),
                           tags$li(
                             "K (Stage-4): Manually input by user for the population of interest."
                           )
                         ),
                         tags$b(
                           "Stage-stage distribution (portions) under current model settings:"
                         ),
                         
                         DT::dataTableOutput(ns("dt_stablestage_k")),
                         br(),
                         p(
                           paste0(
                             "Current K values per stage class. Calculated from stable-stage distributions after fixing the adult (stage-4) K to: ",
                             textOutput(ns("print_adult_k"))
                           )
                         ),
                         br(),
                         
                         tags$b("Density-Dependence Matrix (D):"),
                         tags$p(
                           "Based on the derived stage-specific carry capacities (K values), baseline survivorship values (SE, S0, surv_1, …) and the corresponding compensation ratios (cr_E, cr_0, cr_1, …) a density-dependence matrix (D) for a hypothetical population vector of egg: 10,000,000, fry: 1,000,000; stage 1: 100,000, stage 2: 10,000, stage 3: 1,000 & stage 4: 100 we appear as follows:"
                         ),
                         
                         tags$p(
                           "The density-dependence matrix (D) represents modifiers for the survivorship estimate for each stage transition. The density dependence matrix (D) is then multiplied with the corresponding transition matrix (B) of density-independent transition probabilities:"
                         ),
                         
                         DT::dataTableOutput(ns("dt_transition_matrix")),
                         
                         
                         tags$p(
                           "Then, to get the finalized projection matrix (A) the two previous matrices are multipled together [A is a product of B*D = A]. The projection matrix (A) is recalculated for each time step. Note that vital rate modifiers from environmental parameters linked to stressor-response functions will result in changes to either the survivorship, capacity or fecundity values (omitted here)."
                         ),
                         
                         
                         tags$p(
                           "Compensation ratios are (generally) widely used as parameters in stock-recruitment functions although they are admittedly less popular in classical matrix population modeling. Steepness (the proportion of recruitment produced when stock size is reduced to 20% of initial biomass) is sometimes used in place of compensation ratios. Numerous other methods exist to introduce density dependence into stage-structured population models. The compensation ratios were used in this tool to represent a versatile mechanism for applications to a large number of hypothetical species profiles. For additional background please review the following references to learn more about compensation ratios and population modelling with density-dependent growth. "
                         ),
                         tags$b("Useful References:"),
                         tags$p(
                           "Goodyear, C. P. (1980). Compensation in fish populations. Biological monitoring of fish, 253-280."
                         ),
                         tags$p(
                           "Myers, R. A. (2001). Stock and recruitment: generalizations about maximum reproductive rate, density dependence, and variability using meta-analytic approaches. ICES Journal of Marine Science, 58(5), 937-951."
                         ),
                         tags$p(
                           "Rose, K. A., Cowan Jr, J. H., Winemiller, K. O., Myers, R. A., & Hilborn, R. (2001). Compensatory density dependence in fish populations: importance, controversy, understanding and prognosis. Fish and Fisheries, 2(4), 293-327."
                         ),
                         tags$p(
                           "Myers, R. A., Bowen, K. G., & Barrowman, N. J. (1999). Maximum reproductive rate of fish at low population sizes. Canadian Journal of Fisheries and Aquatic Sciences, 56(12), 2404-2419."
                         ),
                         tags$p(
                           "Walters, C. J., & Martell, S. J. (2004). Fisheries ecology and management. Princeton University Press."
                         ),
                         tags$p(
                           "Forrest, R. E., McAllister, M. K., Dorn, M. W., Martell, S. J., & Stanley, R. D. (2010). Hierarchical Bayesian estimation of recruitment parameters and reference points for Pacific rockfishes (Sebastes spp.) under alternative assumptions about the stock–recruit function. Canadian Journal of Fisheries and Aquatic Sciences, 67(10), 1611-1634."
                         )
                       ),
                       easyClose = TRUE,
                       size = "l",
                       footer = NULL
                     ),
                   )
                 })
                 
                 
                 
                 
                 
                 
                 
                 
               })
}