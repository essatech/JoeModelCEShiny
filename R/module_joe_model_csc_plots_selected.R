#' Joe Model Cumulative System Capacity Plots UI - For Selected HUCs
#'
#' Define parameters and run the Joe Model
#' 
#' @param id The id for this module
#'
#' @return a tagList containing UI elements
#'
module_joe_model_csc_plots_selected_ui <- function(id) {
  
  ns <- NS(id)
  # Single action button to call modal
  actionButton(ns("open_joe_modal_csc_plots_selected"),
               tags$b("Selected watersheds"),
               class="chart-line clean-button",
               width = "100%")
  
}



#' Joe Model Cumulative System Capacity Plots Server - For Selected HUCs
#'
#' Server and modal content for the Joe model server
#'
#' @param none
#'
#' @return None
#'
module_joe_model_csc_plots_selected_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      
      print("Calling module_joe_model_csc_plots_selected_server")
      
      #-------------------------------------------------------
      # DISABLE AND ENABLE 
      #------------------------------------------------------- 
      # this modal is disabled if the Joe Model results are empty
      # this modal is also disabled if there is no selection
      observe({
        sims <- session$userData$rv_joe_model_results$sims
        if(length(sims) > 0) {
          selected <- session$userData$rv_clickedIds$ids
          if(length(selected) == 0) {
            shinyjs::disable("open_joe_modal_csc_plots_selected")
          } else {
            shinyjs::enable("open_joe_modal_csc_plots_selected")
          }
        } else {
          shinyjs::disable("open_joe_modal_csc_plots_selected")
          
        }
      })
      
      
      
      
      #-------------------------------------------------------
      # START OF INPUT MODAL UI
      #-------------------------------------------------------      
      # Display the CSC plots for SELECTED watersheds
      observeEvent(input$open_joe_modal_csc_plots_selected, {
        
        print("Joe model results for selected HUCs modal...")
        # Gather a list of all the stessors to build the checkbox list
        showModal(modalDialog(
          title = "Cumulative System Capacity Plots - Selected HUCs",
          tagList(
            shinydashboard::box(
              width = 12,
              fluidRow(
                column(width = 12,
                       tags$p("This section provides the Joe Model results for the selected HUCs. The following table contains summary statistics for the system capacity (SC) across each simulation (batch replicate). The first column describes variability in the global mean (how each batch replicate varies) and the second column describes variability across the selected HUCs. A histogram is included (below) to visualize system capacity across selected HUCs and batch replicates."),
                )
              ),
              fluidRow(
                column(width = 10,
                       tags$p("Selected HUCs"),
                       uiOutput(ns("list_selected_hucs"))
                       )
              ),
              fluidRow(
                column(DT::dataTableOutput(ns("csc_tables")), width = 10)
              ),
              fluidRow(
                column(plotOutput(ns("csc_hist")), width = 10)
              )
            ),
            fluidRow(
              shinydashboard::box(
                width = 12,
                fluidRow(
                  column(width = 12,
                         tags$p("Histograms of cumulative system capacity can also be generated individually for each HUC, however the rendering process is slow. Click the button below to generate cumulative system capacity plots across all selected HUCs individually."),
                  )
                ),
                actionButton(ns("csc_show_all_plots"), "display individual plots for selected HUCs (slow rendering)"),
                plotOutput(ns("csc_plot_panel")),
              )
            ),
          ),
          easyClose = TRUE,
          size = 'l',
          footer = NULL
        ))
      }) # END OF INPUT MODAL UI
      #-------------------------------------------------------
      
      
      
      #------------------------------------------------------
      # Labels of selected HUCs
      #------------------------------------------------------
      output$list_selected_hucs <- renderUI({
        selected <- session$userData$rv_clickedIds$ids
        name_list <- list()
        for(i in 1:length(selected)) {
          name_list[[i]] <- tags$div(selected[i])
        }
        res_n <- htmltools::tagList(name_list)
      })
      
      #-------------------------------------------------------
      # Generate CSC Joe Model Summary Tables
      #-------------------------------------------------------
      output$csc_tables <- renderDataTable({
        
        selected <- session$userData$rv_clickedIds$ids
        selected_ids <- strsplit(selected, "\\|")
        selected_ids <- sapply(selected_ids, `[`, 1)
        selected_ids <- as.numeric(as.character(selected_ids))
        
        # Build summary table of Joe Model results
        # Get the most recent result set
        simulation_index <- length(session$userData$rv_joe_model_results$sims)
        # Get the Joe model results object 
        jmr <- session$userData$rv_joe_model_results$sims[[simulation_index]]
        
        # Subset for selection
        jmr$ce.df <- jmr$ce.df[which(jmr$ce.df$HUC %in% selected_ids), ]
        

        # Summary across simulations
        # Look at system wide CE scores
        sim_scores <- jmr$ce.df %>% group_by(simulation) %>%
          summarise(
            CE_mean = mean(CE, na.rm = TRUE)
          )
        
        s_obj_sim <- summary(sim_scores$CE_mean * 100, na.rm = TRUE)
        
        # Summary across HUCs
        # Look at system wide CE scores
        h_scores <- jmr$ce.df %>% group_by(HUC) %>%
          summarise(
            CE_mean = mean(CE, na.rm = TRUE)
          )
        
        s_obj_huc <- summary(h_scores$CE_mean * 100, na.rm = TRUE)
        
        df_csc_res <- data.frame(sims = as.matrix(s_obj_sim)[,1], hucs = as.matrix(s_obj_huc)[,1])
        df_csc_res <- round(df_csc_res, 1)
        
        
        # Build the JS DT Data Table Object
        my_dt <- DT::datatable(
          df_csc_res,
          editable =  FALSE,
          caption = "Mean system capacity summary across all simulations for selected HUCs (subset)",  
          colnames = c('Global Mean SC (per simulation, %)' = 'sims', 'Mean SC Across Selected HUCs (%)' = 'hucs'),
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
      
      
      
      #-------------------------------------------------------
      # Historgram for CSC per HUC
      #-------------------------------------------------------
      output$csc_hist <- renderPlot({
        
        selected <- session$userData$rv_clickedIds$ids
        selected_ids <- strsplit(selected, "\\|")
        selected_ids <- sapply(selected_ids, `[`, 1)
        selected_ids <- as.numeric(as.character(selected_ids))
        
        # Build summary table of Joe Model results
        # Get the most recent result set
        simulation_index <- length(session$userData$rv_joe_model_results$sims)
        # Get the Joe model results object 
        jmr <- session$userData$rv_joe_model_results$sims[[simulation_index]]
        
        # Subset for selection
        jmr$ce.df <- jmr$ce.df[which(jmr$ce.df$HUC %in% selected_ids), ]
        
        
        # Summary across HUCs
        # Look at system wide CE scores
        h_scores <- jmr$ce.df %>% group_by(HUC) %>%
          summarise(
            CE_mean = mean(CE, na.rm = TRUE)
          )
        
        hist(h_scores$CE_mean * 100, xlab = "mean sys. capacity per HUC (%)", main = "Selected HUCs")
        
      })
      
      
      #-------------------------------------------------------
      # Generate CSC Joe Model Plot Panel
      #-------------------------------------------------------
      
      # Set trigger to load all plots..
      pp <- eventReactive(input$csc_show_all_plots, {
        
        selected <- session$userData$rv_clickedIds$ids
        selected_ids <- strsplit(selected, "\\|")
        selected_ids <- sapply(selected_ids, `[`, 1)
        selected_ids <- as.numeric(as.character(selected_ids))
        
        # Build summary table of Joe Model results
        # Get the most recent result set
        simulation_index <- length(session$userData$rv_joe_model_results$sims)
        # Get the Joe model results object 
        jmr <- session$userData$rv_joe_model_results$sims[[simulation_index]]
        
        # Subset for selection
        jmr$ce.df <- jmr$ce.df[which(jmr$ce.df$HUC %in% selected_ids), ]
        
        
        # Get the Joe Model result object
        plot_df <- jmr$ce.df
        plot_df$HUC <- as.character(plot_df$HUC)
        
        # Median by plot
        plot_df_median <- plot_df %>% group_by(HUC) %>% summarise(median = median(CE, na.rm = TRUE))
        plot_df_mean <- plot_df %>% group_by(HUC) %>% summarise(mean = mean(CE))
        
        # Set Andy theme
        andy_theme <- theme(
          axis.text.y   = element_text(size = 12),
          axis.text.x   = element_text(size = 12),
          axis.title.y  = element_text(size = 14),
          axis.title.x  = element_text(size = 14),
          panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          panel.border = element_rect(
            colour = "black",
            fill = NA,
            size = 0.5
          )
        )
        
        
        # Generate big panel plot
        big_plot <- ggplot(data = plot_df) +
          geom_freqpoly(aes(x = CE, y = ..ndensity..),
                        size = 1,
                        binwidth = 0.01) +
          geom_vline(
            data = plot_df_median,
            mapping = aes(xintercept = median, color = "median"),
            linetype = "dashed"
          ) +
          geom_vline(
            data = plot_df_mean,
            mapping = aes(xintercept = mean, color = "mean"),
            linetype = "dashed"
          ) +
          scale_color_manual(name = "statistics",
                             values = c(median = "blue", mean = "red")) +
          scale_x_continuous(limits = c(0, 1)) +
          facet_wrap( ~ HUC, ncol = 5) +
          geom_rect(
            data = data.frame(
              xmin = 0,
              xmax = 0.2,
              ymin = 0,
              ymax = Inf
            ),
            aes(
              xmin = xmin,
              xmax = xmax,
              ymin = ymin,
              ymax = ymax
            ),
            fill = "red",
            alpha = 0.2
          ) +
          geom_rect(
            data = data.frame(
              xmin = 0.2,
              xmax = 0.5,
              ymin = 0,
              ymax = Inf
            ),
            aes(
              xmin = xmin,
              xmax = xmax,
              ymin = ymin,
              ymax = ymax
            ),
            fill = "orange",
            alpha = 0.2
          ) +
          geom_rect(
            data = data.frame(
              xmin = 0.5,
              xmax = 0.7,
              ymin = 0,
              ymax = Inf
            ),
            aes(
              xmin = xmin,
              xmax = xmax,
              ymin = ymin,
              ymax = ymax
            ),
            fill = "yellow",
            alpha = 0.2
          ) +
          geom_rect(
            data = data.frame(
              xmin = 0.7,
              xmax = 1,
              ymin = 0,
              ymax = Inf
            ),
            aes(
              xmin = xmin,
              xmax = xmax,
              ymin = ymin,
              ymax = ymax
            ),
            fill = "light green",
            alpha = 0.2
          ) +
          xlab("Cumulative system capacity") +
          ylab("Scaled probability") +
          andy_theme
        
        
        # Hide trigger button after pressed
        shinyjs::hide("csc_show_all_plots")
        
        return(big_plot)
        
      })
      
      
      # Make plot height dynamic
      heightSize <- reactive({
        print("Adjust csc plot height size...")
        # Number of selected HUC
        selected <- session$userData$rv_clickedIds$ids
        selected_ids <- strsplit(selected, "\\|")
        selected_ids <- sapply(selected_ids, `[`, 1)
        selected_ids <- as.numeric(as.character(selected_ids))
        
        my_df_rows <- length(selected_ids) / 5 # 5 columns
        plot_height <- 60 + as.integer(120 * my_df_rows)
        print(plot_height)

        return(plot_height)
        
      })
      
      # Generate plots for the latest 
      output$csc_plot_panel <- renderPlot({
        
        print("renderPlot...")
        if(input$csc_show_all_plots == 0) {
          return(NULL)
        } else {
          # Wait for button click to render plot
          pp()
        }
        
      }, height = heightSize)
      
      
      
      
      
    }
  )
}