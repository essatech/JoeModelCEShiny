#' module_import_ui
#'
#' The UI portion of the import export model
#'
#' @importFrom shiny NS tagList fluidRow column actionButton tags
#' @importFrom shinydashboard box
#'
#' @param id The id for this module
#'
#' @return a tagList containing UI elements
#'
module_import_ui <- function(id) {
  ns <- NS(id)

  tagList(shinydashboard::box(
    width = 10,
    tags$h3("Data Upload"),
    fluidRow(column(
      width = 12,
      tags$p(
        "Click the ‘browse…’ button to upload files. Note that uploaded a new data source will overwrite all local changes and you will need to re-run your results. Stressor response files, stressor magnitude files and watershed polygons must all be validated externally, or you will experience errors. Please see notes below to ensure proper data format. See sample data at the link below to help match "
      ),
      tags$a(href = "https://github.com/essatech/JoeModelCEShiny/tree/main/data/demo", "https://github.com/essatech/JoeModelCEShiny/tree/main/data/demo"),
      tags$p(""),
    )),
    fluidRow(
      column(
        width = 5,
        shinydashboard::box(
          width = 12,
          accordion(
            id = "accordion1",
            accordionItem(
              title = "Stressor Response Workbook",
              collapsed = TRUE,
              tagList(tags$p("...."), )
            )
          ),
          fileInput(
            ns("up_sr_wb_dat"),
            label = "Stressor Response Workbook (xlsx)",
            multiple = FALSE,
            accept = c(".xlsx")
          ),
          div(
            style = "color: #ffffff; background: #ff000059; border-radius: 5px; margin: 5px;",
            textOutput(ns(
              "upload_error_msg_sr"
            ))
          )
        )
      ),
      column(
        width = 5,
        shinydashboard::box(
          width = 12,
          accordion(
            id = "accordion2",
            accordionItem(
              title = "Stressor Magnitude Workbook",
              collapsed = TRUE,
              tagList(tags$p("...."), )
            )
          ),
          fileInput(
            ns("up_sm_wb_dat"),
            label = "Stressor Magnitude Workbook (xlsx)",
            multiple = FALSE,
            accept = c(".xlsx")
          ),
          div(
            style = "color: #ffffff; background: #ff000059; border-radius: 5px; margin: 5px;",
            textOutput(ns(
              "upload_error_msg_sm"
            ))
          )
        )
      )
    ),
    fluidRow(
      column(
        width = 5,
        shinydashboard::box(
          width = 12,
          accordion(
            id = "accordion3",
            accordionItem(
              title = "Watershed Polygons (Spatial) [GeoPackage .gpkg or .shp]",
              collapsed = TRUE,
              tagList(tags$p("...."), )
            )
          ),
          fileInput(
            ns("up_sheds"),
            label = "polygons as .gpkg file",
            multiple = TRUE,
            accept = c(".gpkg", ".shp", ".cpg", ".dbf", ".prj", ".shx")
          ),
          div(
            style = "color: #ffffff; background: #ff000059; border-radius: 5px; margin: 5px;",
            textOutput(ns(
              "upload_error_msg_sheds"
            ))
          )
        )
      ),
      column(
        width = 5,
        shinydashboard::box(
          width = 12,
          accordion(
            id = "accordion4",
            accordionItem(
              title = "Population Model Vital Rates",
              collapsed = TRUE,
              tagList(tags$p("...."), )
            )
          ),
          fileInput(
            ns("up_vital"),
            label = "life cycles.csv",
            multiple = FALSE,
            accept = c(".csv")
          ),
          div(
            style = "color: #ffffff; background: #ff000059; border-radius: 5px; margin: 5px;",
            textOutput(ns(
              "upload_error_msg_vital"
            ))
          )
        )
      )
    ),
  ))
}





#' Import server function
#'
#' @param none
#'
#' @return None
#'
module_import_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      print("Calling module_import_server")

      #--------------------------------------
      # Stressor Resposne Workbook Data Download
      observe({
        # Require the file
        req(input$up_sr_wb_dat)

        upload_ok <- FALSE

        # Run import function in a try catch
        # to avoid app crashing on upload errors
        tryCatch(
          {
            in_file <- input$up_sr_wb_dat

            if (is.null(in_file)) {
              return(NULL)
            }


            # Ensure there are no spaces in file name
            # file.rename(in_file$datapath, paste(in_file$datapath, ".xlsx", sep=""))
            # Extract the stressor response relationships
            sr_wb_dat <-
              JoeModelCE::StressorResponseWorkbook(filename = input$up_sr_wb_dat$datapath)

            # Update the session$userData$rv_stressor_response reactive object
            start_time <- Sys.time()

            # Designate the stressor response object as a reactive value
            session$userData$rv_stressor_response$main_sheet <-
              sr_wb_dat$main_sheet
            session$userData$rv_stressor_response$stressor_names <-
              sr_wb_dat$stressor_names
            session$userData$rv_stressor_response$pretty_names <-
              sr_wb_dat$pretty_names
            session$userData$rv_stressor_response$sr_dat <-
              sr_wb_dat$sr_dat
            session$userData$rv_stressor_response$active_layer <-
              sr_wb_dat$stressor_names[1]
            session$userData$rv_stressor_response$active_values_raw <-
              NULL
            session$userData$rv_stressor_response$active_values_response <-
              NULL
            session$userData$rv_stressor_response$active_refresh <-
              start_time
            session$userData$rv_stressor_response$hover_values <-
              FALSE
            session$userData$rv_stressor_response$interaction_names <-
              names(sr_wb_dat$MInt)
            session$userData$rv_stressor_response$interaction_values <-
              sr_wb_dat$MInt


            # Trigger redraw to clear selection
            session$userData$rv_redraw$redraw <- 0
            # Selected HUCs - Create an empty vector to hold all HUC click ids
            session$userData$rv_clickedIds$ids <- vector()
            # Selected HUCs Cumulative System Capacity - store temporary CSC for selected HUCs
            session$userData$rv_clickedIds_csc$csc <- NA
            session$userData$rv_clickedIds_csc$var_csc <- NA

            # Joe model results holder - assume multiple runs
            session$userData$rv_joe_model_results$sims <- list()

            # Joe model scenario name holder - assume multiple runs
            session$userData$rv_joe_model_sim_names$scenario_names <-
              list()

            # Place holder for Joe Model estimated run times
            session$userData$rv_joe_model_run_time$run_time_seconds <-
              list()

            # Clear out any population model runs
            session$userData$rv_pop_data_huc_ts$dat <- list()
            session$userData$rv_pop_data_huc_ts$run_counter <- 1
            session$userData$rv_pop_data_huc_ts$update_ts_plots <- FALSE
            session$userData$rv_show_pop_main_plot$open <- FALSE
            session$userData$rv_sandbox_stressors$dat <- list()

            # Clear out sample plot data
            # session$userData$rv_show_sample_plot$open <- FALSE
            session$userData$rv_pop_sample_plot_data$dat <- list()
            session$userData$rv_pop_sample_plot_data$run_counter <- 1
            
            # Hide system capacity button
            addClass(id = "main_map-var_id", class = "hide-this", asis = TRUE)
            

            upload_ok <- TRUE

            output$upload_error_msg_sr <- renderText({
              ""
            })
          },
          error = function(e) {
            # return a safeError if a parsing error occurs
            print("Upload error...")

            output$upload_error_msg_sr <- renderText({
              "Upload Error: Stressor Response Workbook (xlsx) did not import correctly. Check data format, worksheet names and column names."
            })
          }
        )
      }) # end of Stressor Response Workbook Data Download



      #--------------------------------------
      # Stressor Magnitude Workbook Data
      observe({
        # Require the file
        req(input$up_sm_wb_dat)

        upload_ok <- FALSE

        # Run import function in a try catch
        # to avoid app crashing on upload errors
        tryCatch(
          {
            in_file <- input$up_sm_wb_dat

            if (is.null(in_file)) {
              return(NULL)
            }

            # Extract the stressor response relationships
            sm_wb_dat <-
              JoeModelCE::StressorMagnitudeWorkbook(
                filename = input$up_sm_wb_dat$datapath,
                scenario_worksheet = 1
              ) # natural_unc

            # First reset the stressor response workbook
            start_time <- Sys.time()

            session$userData$rv_stressor_response$active_layer <-
              session$userData$rv_stressor_response$stressor_names[1]
            session$userData$rv_stressor_response$active_values_raw <-
              NULL
            session$userData$rv_stressor_response$active_values_response <-
              NULL
            session$userData$rv_stressor_response$active_refresh <-
              start_time
            session$userData$rv_stressor_response$hover_values <- FALSE

            # Update the stressor magnitude data
            session$userData$rv_stressor_magnitude$sm_dat <- sm_wb_dat

            # Trigger redraw to clear selection
            session$userData$rv_redraw$redraw <- 0

            # Selected HUCs - Create an empty vector to hold all HUC click ids
            session$userData$rv_clickedIds$ids <- vector()

            # Selected HUCs Cumulative System Capacity - store temporary CSC for selected HUCs
            session$userData$rv_clickedIds_csc$csc <- NA
            session$userData$rv_clickedIds_csc$var_csc <- NA

            # Joe model results holder - assume multiple runs
            session$userData$rv_joe_model_results$sims <- list()

            # Joe model scenario name holder - assume multiple runs
            session$userData$rv_joe_model_sim_names$scenario_names <-
              list()

            # Place holder for Joe Model estimated run times
            session$userData$rv_joe_model_run_time$run_time_seconds <-
              list()

            # Clear out any population model runs
            session$userData$rv_pop_data_huc_ts$dat <- list()
            session$userData$rv_pop_data_huc_ts$run_counter <- 1
            session$userData$rv_pop_data_huc_ts$update_ts_plots <- FALSE
            session$userData$rv_show_pop_main_plot$open <- FALSE
            session$userData$rv_sandbox_stressors$dat <- list()

            # Clear out sample plot data
            # session$userData$rv_show_sample_plot$open <- FALSE
            session$userData$rv_pop_sample_plot_data$dat <- list()
            session$userData$rv_pop_sample_plot_data$run_counter <- 1
            
            # Hide system capacity button
            addClass(id = "main_map-var_id", class = "hide-this", asis = TRUE)

            upload_ok <- TRUE

            output$upload_error_msg_sr <- renderText({
              ""
            })
          },
          error = function(e) {
            # return a safeError if a parsing error occurs
            print("Upload error...")

            output$upload_error_msg_sm <- renderText({
              "Upload Error: Stressor Magnitude Workbook (xlsx) did not import correctly. Check data format and column names."
            })
          }
        )
      }) # end of Stressor Magnitude Workbook Data Download



      #--------------------------------------
      # Watershed Polygon Upload
      observe({
        # Require the file
        req(input$up_sheds)

        upload_ok <- FALSE

        # Run import function in a try catch
        # to avoid app crashing on upload errors

        tryCatch(
          {
            in_file <- input$up_sheds

            if (is.null(in_file)) {
              return(NULL)
            }

            if (nrow(in_file) > 1) {
              # Shapefile load
              infiles <- in_file$datapath # get the location of files
              dir <- unique(dirname(infiles)) # get the directory
              outfiles <- file.path(dir, in_file$name) # create new path name
              name <- strsplit(in_file$name[1], "\\.")[[1]][1] # strip name
              purrr::walk2(infiles, outfiles, ~ file.rename(.x, .y)) # rename files
              hmdl <- sf::read_sf(file.path(dir, paste0(name, ".shp"))) # read-in shapefile
            } else {
              # Geopackage load
              # Load in the default watersheds geojson layer
              hmdl <- sf::st_read(input$up_sheds$datapath[1])
            }
            
            print("File loading....")
            
            # Fix col names - if needed
            cnames <- colnames(hmdl)
            if(!("HUC_ID" %in% cnames)) {
              print("HUC_ID not in col names...")
              use_id <- which(grepl("id", tolower(cnames)))[1]
              hmdl$HUC_ID <- hmdl[[use_id]]
            }
            if(!("NAME" %in% cnames)) {
              print("NAME not in col names...")
              use_name <- which(grepl("name", tolower(cnames)))[1]
              hmdl$NAME <- hmdl[[use_name]]
            }

            hmdl$HUC_ID <- as.numeric(hmdl$HUC_ID)
            hmdl$uid <-
              paste0(hmdl$HUC_ID, "|", hmdl$NAME)

            # Ensure the projection is 4326
            if (st_crs(hmdl)$epsg != 4326) {
              hmdl <- st_transform(hmdl, 4326)
            }

            # Save default HUC to reactive values
            session$userData$rv_HUC_geom$huc_geom <- hmdl
            session$userData$rv_HUC_geom$leg_col <- NA
            session$userData$rv_HUC_geom$leg_lab <- NA
            session$userData$rv_HUC_geom$color_df <- NA

            # Initial load
            bbox <- st_bbox(hmdl)
            session$userData$rv_HUC_layer_load$data <- hmdl
            session$userData$rv_HUC_layer_load$xmin <- bbox$xmin
            session$userData$rv_HUC_layer_load$ymin <- bbox$ymin
            session$userData$rv_HUC_layer_load$xmax <- bbox$xmax
            session$userData$rv_HUC_layer_load$ymax <- bbox$ymax
            session$userData$rv_HUC_layer_load$reload_map <- TRUE
            session$userData$rv_HUC_layer_load$add_polygons <- FALSE

            # First reset the stressor response workbook
            start_time <- Sys.time()

            session$userData$rv_stressor_response$active_layer <-
              isolate(session$userData$rv_stressor_response$stressor_names[1])
            session$userData$rv_stressor_response$active_values_raw <-
              NULL
            session$userData$rv_stressor_response$active_values_response <-
              NULL
            session$userData$rv_stressor_response$active_refresh <-
              start_time
            session$userData$rv_stressor_response$hover_values <- FALSE


            # Update the stressor magnitude data
            session$userData$rv_stressor_magnitude$sm_dat <-
              isolate(session$userData$rv_stressor_magnitude$sm_dat)

            # Trigger redraw to clear selection
            session$userData$rv_redraw$redraw <- 0

            # Selected HUCs - Create an empty vector to hold all HUC click ids
            session$userData$rv_clickedIds$ids <- vector()

            # Selected HUCs Cumulative System Capacity - store temporary CSC for selected HUCs
            session$userData$rv_clickedIds_csc$csc <- NA
            session$userData$rv_clickedIds_csc$var_csc <- NA

            # Joe model results holder - assume multiple runs
            session$userData$rv_joe_model_results$sims <- list()

            # Joe model scenario name holder - assume multiple runs
            session$userData$rv_joe_model_sim_names$scenario_names <-
              list()

            # Place holder for Joe Model estimated run times
            session$userData$rv_joe_model_run_time$run_time_seconds <-
              list()

            # Clear out any population model runs
            session$userData$rv_pop_data_huc_ts$dat <- list()
            session$userData$rv_pop_data_huc_ts$run_counter <- 1
            session$userData$rv_pop_data_huc_ts$update_ts_plots <- FALSE
            session$userData$rv_show_pop_main_plot$open <- FALSE
            session$userData$rv_sandbox_stressors$dat <- list()

            # Clear out sample plot data
            # session$userData$rv_show_sample_plot$open <- FALSE
            session$userData$rv_pop_sample_plot_data$dat <- list()
            session$userData$rv_pop_sample_plot_data$run_counter <- 1
            
            # Hide system capacity button
            addClass(id = "main_map-var_id", class = "hide-this", asis = TRUE)

            upload_ok <- TRUE

            output$upload_error_msg_sr <- renderText({
              ""
            })
          },
          error = function(e) {
            # return a safeError if a parsing error occurs
            print("Upload error...")

            output$upload_error_msg_sheds <-
              renderText({
                "Upload Error: HUC watersheds did not import correctly. Ensure data is uploaded as a gpkg file with latitude/longitude (EPSG:4326) projects and field attribute (column) names HUC_ID and NAME with the HUC_ID and values corresponding to those in the stressor magnitude file."
              })
          }
        )
      }) # end of Stressor Magnitude Workbook Data Download




      #--------------------------------------
      # Upload vital rates
      observe({
        # Require the file
        req(input$up_vital)

        upload_ok <- FALSE

        # Run import function in a try catch
        # to avoid app crashing on upload errors

        tryCatch(
          {
            in_file <- input$up_vital

            if (is.null(in_file)) {
              return(NULL)
            }

            # Load in the default watersheds geojson layer
            life_stages <-
              read.csv(input$up_vital$datapath)
            
            print("running JavaScript... updateAllInputs")
            # Update all numeric inputs through javascript
            js$updateAllInputs(rjson::toJSON(life_stages))

            session$userData$rv_life_stages$dat <- life_stages

            session$userData$rv_eigen_analysis$dat <- list()

            session$userData$rv_ea_errors$possible_error_state <- FALSE
            session$userData$rv_ea_errors$possible_error_msg <- ""

            # session$userData$rv_show_sample_plot$open <- FALSE

            session$userData$rv_pop_sample_plot_data$dat <- list()
            session$userData$rv_pop_sample_plot_data$run_counter <- 1

            # Sand box stressor values
            session$userData$rv_sandbox_stressors$dat <- list()

            session$userData$rv_pop_data_huc_ts$dat <- list()
            session$userData$rv_pop_data_huc_ts$run_counter <- 1
            session$userData$rv_pop_data_huc_ts$update_ts_plots <- FALSE

            session$userData$rv_show_pop_main_plot$open <- FALSE

            output$upload_error_msg_sheds <-
              renderText({
                ""
              })
          },
          error = function(e) {
            print("Upload error...")

            output$upload_error_msg_sheds <-
              renderText({
                "Upload Error: Vital rate parameters did not import correctly. Please check file against the reference and reupload."
              })
          }
        )
      }) # end vital rate import
    }
  )
}
