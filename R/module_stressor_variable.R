#' stressor_variable UI
#'
#' The UI portion of the main map module
#'
#' @param id The id for this module
#'
#' @return a tagList containing UI elements
#'
module_stressor_variable_ui <- function(id) {
  ns <- NS(id)

  tags$div(
    class = "stack-box map-variable", id = ns("var_id"),
    shinydashboard::box(
      width = 12,
      background = "light-blue",
      class = "stack-box-border",
      htmlOutput(ns("variable_label")),
      htmlOutput(ns("variable_val_raw")),
      tags$div(actionButton(ns("response_plot"), icon("chart-line"), class = "response-button"),
        style = "float: right; display: inline-block;"
      ),
      tags$p("", style = "float: right;"),
      tags$div(numericInput(ns("hiddenload"), label = "hidden", value = 0), style = "display:none;")
    ) # end of box
  ) # end of div
}


#' stressor_variable Server
#'
#' The Server portion of the stressor_variable module
#'
#' @param mlabel Character. Label of the variable
#'
#'
#' @return None
#'
module_stressor_variable_server <- function(id, stressor_index = NA) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      # Set the label
      output$variable_label <- renderUI({
        # print("Variable Label")
        label <- session$userData$rv_stressor_response$pretty_names[stressor_index]
        # Adjust if matrix interaction term
        if (stressor_index > length(session$userData$rv_stressor_response$pretty_names)) {
          label <- session$userData$rv_stressor_response$interaction_names[stressor_index - length(session$userData$rv_stressor_response$pretty_names)]
        }
        label <- paste0(label, "  ")
        tags$p(label, style = "float: left;")
      })

      # Change mouse-over raw value
      output$variable_val_raw <- renderUI({
        sname <- session$userData$rv_stressor_response$stressor_names[stressor_index]

        # Adjust if matrix interaction term
        if (stressor_index > length(session$userData$rv_stressor_response$stressor_names)) {
          sname <- session$userData$rv_stressor_response$interaction_names[stressor_index - length(session$userData$rv_stressor_response$pretty_names)]
        }

        if (is.null(session$userData$rv_stressor_response$active_values_raw)) {
          tags$p(" ", style = "float: left; display:inline;")
        } else {
          raw_vals <- session$userData$rv_stressor_response$active_values_raw$Mean
          names <- session$userData$rv_stressor_response$active_values_raw$Stressor
          target_val <- raw_vals[which(names == sname)]
          target_val <- ifelse(is.na(target_val), " ", paste0(" ", target_val))
          tags$p(target_val, style = "float: left; display:inline; padding-left: 8px; font-size: 90%;")
        }
      })


      # ------------------------------------------------------
      # Set selected class as selected variable
      # Should it be styled as selected or light blue un-selected
      # ------------------------------------------------------
      observe({
        req(input$hiddenload)
        # print("Selected Variable")
        
        # MJB added here June 6 2023
        
        
        # Set the stressor response object as a reactive value
        if (!(is.na(stressor_index))) {
          
          active <- session$userData$rv_stressor_response$active_layer
          current <- session$userData$rv_stressor_response$stressor_names[stressor_index]


          # Fix name if selecting an interaction matrix
          if (is.na(current)) {
            # Assume interaction matrix
            current <- session$userData$rv_stressor_response$interaction_names[stressor_index - length(session$userData$rv_stressor_response$pretty_names)]
          }
          
         
         if(is.null(current)) {
           current <- NA # MJB added - fix exception for null
         }
          
          if (!(is.na(current)) & !(is.na(active))) {
            if (active == current) {
              # print("Adding class")
              q_code <- paste0("jQuery('#main_map-", current, "-var_id').addClass('var-selected');")
              shinyjs::runjs(code = q_code)
            } else {
              q_code <- paste0("jQuery('#main_map-", current, "-var_id').removeClass('var-selected');")
              shinyjs::runjs(code = q_code)
            }
          }
        }
      })


      # ---------------------------------------------------------
      # Listen to click events to change target variable selected
      observe({
        # ensure UI is loaded - do not run if not set
        req(input$hiddenload)
        # User clicks on ID
        current <- session$userData$rv_stressor_response$stressor_names[stressor_index]

        # If interaction matrix
        if (is.na(current)) {
          # Assume interaction matrix
          current <- session$userData$rv_stressor_response$interaction_names[stressor_index - length(session$userData$rv_stressor_response$pretty_names)]
        }

        # Update reactive value of target variable selected
        updateActiveVar <- function(current) {
          print(paste0("User click.. update to layer... ", current))
          session$userData$rv_stressor_response$active_layer <- current
        }
        # Use mouse click
        my_id <- paste0("main_map-", current, "-var_id")
        onclick(my_id,
          updateActiveVar(current),
          asis = TRUE
        )
      })






      #-------------------------------------------------------
      # Stressor response modal dialog
      #-------------------------------------------------------
      # Open the stressor response dialog box
      observeEvent(input$response_plot, {

        # Dont load until button clicked
        req(input$hiddenload)

        this_var <- session$userData$rv_stressor_response$pretty_names[stressor_index]
        # print(paste0("Stressor response modal is open for ... ", this_var))

        # -------------------------------------------------------
        # If interaction matrix - show but not editable
        if (is.na(this_var)) {
          # Assume interaction matrix
          this_var <- session$userData$rv_stressor_response$interaction_names[stressor_index - length(session$userData$rv_stressor_response$pretty_names)]

          showModal(modalDialog(
            title = paste0("Stressor-Response Relationship: ", this_var),
            tagList(
              tags$p("Custom 2-way matrix interaction surface: The 2-factor interaction matrices are defined in the Stressor-Response Excel workbook. Customizable 2-factor interaction matrices may be (optionally) included by users to specify non-additive interactions between stressor variables (e.g., antagonistic, synergistic, etc.). If included, these matrices define the mean system capacity at different combination levels between two stressors. This can be especially important to capture conditional effects, attenuating or exacerbating factors, and/or compound variance and uncertainties. The 2-factor interaction matrices can also be a convenient mechanism to explore hypothetical and experimental scenarios."),
              tags$b("Mean System Capacity:"),
              fluidRow(
                shinydashboard::box(
                  width = 12,
                  tags$p(textOutput(ns("text_interaction_matrix"))),
                  DT::dataTableOutput(ns('interaction_matrix_main'))
                )
              ),
              tags$b("SD:"),
              fluidRow(
                shinydashboard::box(
                  width = 12,
                  tags$p(textOutput(ns("text_interaction_matrix"))),
                  DT::dataTableOutput(ns('interaction_matrix_sd'))
                )
              ),
              tags$b("Lower Limit:"),
              fluidRow(
                shinydashboard::box(
                  width = 12,
                  tags$p(textOutput(ns("text_interaction_matrix"))),
                  DT::dataTableOutput(ns('interaction_matrix_ll'))
                )
              ),
              tags$b("Upper Limit:"),
              fluidRow(
                shinydashboard::box(
                  width = 12,
                  tags$p(textOutput(ns("text_interaction_matrix"))),
                  DT::dataTableOutput(ns('interaction_matrix_ul'))
                )
              )
            ),
            easyClose = TRUE,
            size = "l",
            footer = NULL
          ))

          # end of matrix interaction surface
        } else {

          # -------------------------------------------------------
          # Normal variable - not a matrix interaction surface
          # Currently selected variable name
          # note can not use this_var <- session$userData$rv_stressor_response$active_layer

          this_var_pretty <- gsub("_", " ", this_var)

          # Main sheet attributes
          this_main <- session$userData$rv_stressor_response$main_sheet

          showModal(modalDialog(
            title = paste0("Stressor-Response Relationship: ", this_var_pretty),
            tagList(
              tags$p("Use the table below to edit and adjust the stressor-response (dose-response) relationship for August Flow rate. Click on cells in the table to adjust values. The graph shows the dose:response relationship between the raw stressor values (x-axis) and the mean system capacity (y-axis). The red line shows the mean value, and the shading represents uncertainty in the relationship. The red shading represents one standard deviation, and the grey shading represents the upper and lower bounds of min and max values. Click and drag within the graph window to zoom in on particular trends; double-click the graph to zoom out to full view."),
              tags$b(textOutput(ns("text_preview"))),
              fluidRow(
                shinydashboard::box(
                  width = 12,
                  dygraphOutput(ns("dose_response_plot"))
                )
              ),
              fluidRow(
                shinydashboard::box(
                  width = 12,
                  tags$p("Double-click a cell to edit its value."),
                  DTOutput(
                    ns("stressor_response_dt")
                  ),
                  actionButton(ns("close_sr_modal"), "Close stressor-response module", style = "margin: 15px;")
                )
              )
            ),
            easyClose = TRUE,
            size = "l",
            footer = NULL
          ))
        }
      })


      #-------------------------------------------------------
      # Close stressor response modal with custom button
      #-------------------------------------------------------
      observeEvent(input$close_sr_modal, {
        removeModal()
      })



      #-------------------------------------------------------
      # Stressor response modal dialog
      #-------------------------------------------------------
      # Add table data as data table
      # This is the doese response relationship for each stressor
      output$stressor_response_dt <- renderDT({

        # Do not run on app load
        req(session$userData$rv_stressor_response$active_layer)

        # Get all SR data
        sr_data <- session$userData$rv_stressor_response$sr_dat
        # Filter for target layer
        this_var <- session$userData$rv_stressor_response$active_layer # e.g., temperature
        table_vals <- sr_data[[this_var]] # e.g., temperature

        # Build the JS DT Data Table Object
        DT::datatable(
          table_vals,
          # The Stressor column is not editable
          editable = TRUE, # list(target = "cell", disable = list(columns = c(1))),
          colnames = c(
            "Raw Value" = "value", "Mean System Capacity (0-100)" = "mean_system_capacity",
            "SD (0-100)" = "sd", "Lower Limit (0)" = "lwr", "Upper Limit (100)" = "upr"
          ),
          filter = "none",
          selection = "single",
          rownames = FALSE,
          class = "cell-border stripe",
          options = list(
            pageLength = 500,
            info = FALSE,
            dom = "t",
            ordering = FALSE,
            columnDefs = list(list(
              className = "dt-left", targets = "_all"
            ))
          )
        )
      })

      # Create a proxy for the above table
      dt_proxy <- DT::dataTableProxy("stressor_response_dt")


      #-------------------------------------------------------
      # Populate summary statistics for stressor in modal
      #-------------------------------------------------------
      # Display mean min and max
      output$text_preview <- renderText({
        this_var <- isolate(session$userData$rv_stressor_response$active_layer) # e.g., temperature
        # Stressor magnitude data
        sm_df <- session$userData$rv_stressor_magnitude$sm_dat
        # Subset to targer variable
        sm_sub <- sm_df[which(sm_df$Stressor == this_var), ]
        my_mean <- round(mean(sm_sub$Mean, na.rm = TRUE), 2)
        my_min <- min(sm_sub$Mean, na.rm = TRUE)
        my_max <- max(sm_sub$Mean, na.rm = TRUE)

        data_rng_txt <- paste0("HUC Values, Mean: ", my_mean, " (Min: ", my_min, ", Max: ", my_max, ")")
        return(data_rng_txt)
      })



      #------------------------------------------------------------------------
      # Update a data value cell
      #------------------------------------------------------------------------
      # When there is an edit to a cell
      # update the stessor response reactive values
      observeEvent(input$stressor_response_dt_cell_edit, {

        # Get new value of edited cell
        info <- input$stressor_response_dt_cell_edit

        # Index list of stressor names
        this_var <- isolate(session$userData$rv_stressor_response$active_layer) # e.g., temperature

        # print(paste0("Edit value for ... ", this_var))

        # HUCs currently selected
        selected_raw <- session$userData$rv_clickedIds$ids
        # Fix format
        getID <- function(x) {
          strsplit(x, "\\|")[[1]][1]
        }
        selected_ids <- lapply(selected_raw, getID) %>% unlist()

        info$value <- as.numeric(info$value)
        info$value <- ifelse(is.na(info$value), 0, info$value)

        i <- as.numeric(info$row)
        j <- as.numeric(info$col)
        j <- j + 1 # Weird issue with row names missing
        k <- as.numeric(info$value)

        # Ensure value is ok
        if (j > 1) {
          # Keep system capacity in bounds
          k <- ifelse(k < 0, 0, k)
          k <- ifelse(k > 100, 100, k)
        }

        var <- c("value", "mean_system_capacity", "sd", "lwr", "upr")


        # Update stressor response value
        session$userData$rv_stressor_response$sr_dat[[this_var]][i, j] <- k
        # Update the DT data table so the user knows what they have just done

        # Also invalidate the cumulative system capacity score in the stressor magnitude table
        print("click csc invalidated...")
        session$userData$rv_clickedIds_csc$csc <- NA

        
      })



      #------------------------------------------------------------------------
      # renderDygraph charts data visualization
      #------------------------------------------------------------------------
      output$dose_response_plot <- renderDygraph({

        # Filter for target layer
        this_var <- session$userData$rv_stressor_response$active_layer # e.g., temperature

        # print(paste0(this_var, " this_var in renderDygraph()"))
        # Get all SR data
        table_vals <- session$userData$rv_stressor_response$sr_dat[[this_var]]
        table_vals$lwr_sd <- table_vals$mean_system_capacity - table_vals$sd
        table_vals$upr_sd <- table_vals$mean_system_capacity + table_vals$sd
        table_vals <- table_vals[order(table_vals$mean_system_capacity), ]
        table_vals <- table_vals[order(table_vals$value), ]

        # Fix lower and upper sd bounds to be within range of limits
        table_vals$lwr_sd <- ifelse(table_vals$lwr_sd < table_vals$lwr, table_vals$lwr, table_vals$lwr_sd)
        table_vals$upr_sd <- ifelse(table_vals$upr_sd > table_vals$upr, table_vals$upr, table_vals$upr_sd)

        # Ensure no bad values
        table_vals <- table_vals[, c("value", "mean_system_capacity", "lwr", "upr", "lwr_sd", "upr_sd")]

        # Pretty label for plot
        pretty_lab <- gsub("_", " ", paste0("Stressor-Response curve for ", this_var))

        # X-axis mouse-over formatting
        myvFormatter <- "function formatValue(v) {
              var prefix = 'Raw Stressor: ';
              return prefix + String(v);
        }"

        # Start and return the dygraph plot
        dygraph(table_vals, main = pretty_lab) %>%
          dyAxis("x", label = "Raw Stressor Values", valueFormatter = JS(myvFormatter)) %>%
          dyAxis("y", label = "Mean System Capacity (%)") %>%
          dySeries(c("lwr", "mean_system_capacity", "upr"), label = "msc", color = "grey") %>%
          dySeries(c("lwr_sd", "mean_system_capacity", "upr_sd"), label = "Mean Sys. Cap.", color = "red")
      })



      #------------------------------------------------------------------------
      # render interaction matrix table
      #------------------------------------------------------------------------
      output$interaction_matrix_main <- DT::renderDataTable({
          print("Rendering matrix intraction table...")
          this_var <- session$userData$rv_stressor_response$interaction_names[stressor_index - length(session$userData$rv_stressor_response$stressor_names)]
          mat_data <- session$userData$rv_stressor_response$interaction_values[[this_var]]
          mtable <- mat_data$mat_msc[, 2:ncol(mat_data$mat_msc)]
          rnms <- unlist(mat_data$mat_msc[, 1])
          rnms <- as.numeric(rnms)
          mtable <- data.frame(mtable)
          rownames(mtable) <- rnms
          colnames_cust <- gsub("X", "", colnames(mtable))
          datatable(mtable,
          colnames = colnames_cust,
          )
      })

      output$interaction_matrix_sd <- DT::renderDataTable({
          print("Rendering matrix intraction table...")
          this_var <- session$userData$rv_stressor_response$interaction_names[stressor_index - length(session$userData$rv_stressor_response$stressor_names)]
          mat_data <- session$userData$rv_stressor_response$interaction_values[[this_var]]
          mtable <- mat_data$mat_sd[, 2:ncol(mat_data$mat_sd)]
          rnms <- unlist(mat_data$mat_sd[, 1])
          rnms <- as.numeric(rnms)
          mtable <- data.frame(mtable)
          rownames(mtable) <- rnms
          colnames_cust <- gsub("X", "", colnames(mtable))
          datatable(mtable,
          colnames = colnames_cust,
          )
      })

      output$interaction_matrix_ll <- DT::renderDataTable({
          print("Rendering matrix intraction table...")
          this_var <- session$userData$rv_stressor_response$interaction_names[stressor_index - length(session$userData$rv_stressor_response$stressor_names)]
          mat_data <- session$userData$rv_stressor_response$interaction_values[[this_var]]
          mtable <- mat_data$mat_ll[, 2:ncol(mat_data$mat_ll)]
          rnms <- unlist(mat_data$mat_ll[, 1])
          rnms <- as.numeric(rnms)
          mtable <- data.frame(mtable)
          rownames(mtable) <- rnms
          colnames_cust <- gsub("X", "", colnames(mtable))
          datatable(mtable,
          colnames = colnames_cust,
          )
      })

      output$interaction_matrix_ul <- DT::renderDataTable({
          print("Rendering matrix intraction table...")
          this_var <- session$userData$rv_stressor_response$interaction_names[stressor_index - length(session$userData$rv_stressor_response$stressor_names)]
          mat_data <- session$userData$rv_stressor_response$interaction_values[[this_var]]
          mtable <- mat_data$mat_ul[, 2:ncol(mat_data$mat_ul)]
          rnms <- unlist(mat_data$mat_ul[, 1])
          rnms <- as.numeric(rnms)
          mtable <- data.frame(mtable)
          rownames(mtable) <- rnms
          colnames_cust <- gsub("X", "", colnames(mtable))
          datatable(mtable,
          colnames = colnames_cust,
          )
      })

      # Interaction matrix text
      output$text_interaction_matrix <- renderText({
          this_var <- session$userData$rv_stressor_response$interaction_names[stressor_index - length(session$userData$rv_stressor_response$stressor_names)]
          mat_data <- session$userData$rv_stressor_response$interaction_values[[this_var]]
          mcol <- mat_data$Columns
          mrow <- mat_data$Rows
          paste0("Interpolation matrix columns are stressor ", mcol, " and rows are stressor ", mrow, ".")
      })



      # Finally return the stressor name
      return(stressor_index)
    }
  )
}
