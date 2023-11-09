module_setup_reactives <- function(session = NA) {
  
  print("Setting up reactive values for user session...")

  # Designate stressor magnitude as reactive values
  rv_stressor_magnitude <- reactiveValues(
    sm_dat = sm_wb_dat
  )
  # Add to userData object for session
  session$userData$rv_stressor_magnitude <- rv_stressor_magnitude
  
  # Designate the stressor response object as a reactive value
  rv_stressor_response <- reactiveValues(
    main_sheet             = sr_wb_dat$main_sheet,
    stressor_names         = sr_wb_dat$stressor_names,
    pretty_names           = sr_wb_dat$pretty_names,
    sr_dat                 = sr_wb_dat$sr_dat,
    active_layer           = sr_wb_dat$stressor_names[1],
    active_values_raw      = NULL,
    active_values_response = NULL,
    active_refresh         = start_time,
    hover_values           = FALSE,
    interaction_names = names(sr_wb_dat$MInt),
    interaction_values = sr_wb_dat$MInt
  )
  session$userData$rv_stressor_response <- rv_stressor_response
  
  # Life cycle parameters object for population model
  rv_life_stages <- reactiveValues(
    dat = life_stages
  )
  session$userData$rv_life_stages <- rv_life_stages

  # Interactive realtime eigen analysis for the population model
  rv_eigen_analysis <- reactiveValues(
    dat = list()
  )
  session$userData$rv_eigen_analysis <- rv_eigen_analysis
  
  # Not used - placeholder
  rv_demo_matricies <- reactiveValues(
    set = 1,
    dat = list()
  )
  session$userData$rv_demo_matricies <- rv_demo_matricies

  # Reactive object for error states and error messages
  rv_ea_errors <- reactiveValues(
    possible_error_state = FALSE,
    possible_error_msg = ""
  )
  session$userData$rv_ea_errors <- rv_ea_errors
  
  # Matrix population model sandbox plots
  rv_show_sample_plot <- reactiveValues(
    open = FALSE
  )
  session$userData$rv_show_sample_plot <- rv_show_sample_plot
  
  # Matrix population model sandbox plot data
  rv_pop_sample_plot_data <- reactiveValues(
    dat = list(),
    run_counter = 1
  )
  session$userData$rv_pop_sample_plot_data <- rv_pop_sample_plot_data

  # Sand box stressor values
  rv_sandbox_stressors <- reactiveValues(
    dat = list()
  )
  session$userData$rv_sandbox_stressors <- rv_sandbox_stressors
  
  # Population model time series projection data
  rv_pop_data_huc_ts <- reactiveValues(
    dat = list(),
    dat_baseline = list(),
    run_counter = 1,
    update_ts_plots = FALSE,
    joe_model_comp = NULL
  )
  session$userData$rv_pop_data_huc_ts <- rv_pop_data_huc_ts

  # Population model results diplay
  rv_show_pop_main_plot <- reactiveValues(
    open = FALSE
  )
  session$userData$rv_show_pop_main_plot <- rv_show_pop_main_plot

  
  # Save default HUC to reactive values
  rv_HUC_geom <- reactiveValues(
    huc_geom = hmdl,              # Polygon geometry global reactive object
    leg_col = NA,                 # Legend color to display
    leg_lab = NA,                 # Legend label to display
    color_df = NA                 # Color reference data frame
  )
  session$userData$rv_HUC_geom <- rv_HUC_geom


  # HUC layer geometry extent and map redraw trigger
  rv_HUC_layer_load <- reactiveValues(
    data = hmdl,
    xmin = bbox_global$xmin,
    ymin = bbox_global$ymin,
    xmax = bbox_global$xmax,
    ymax = bbox_global$ymax,
    reload_map = TRUE,
    add_polygons = FALSE
  )
  session$userData$rv_HUC_layer_load <- rv_HUC_layer_load


  # Trigger redraw to clear selection
  rv_redraw <- reactiveValues(
    redraw = 0
  )
  session$userData$rv_redraw <- rv_redraw

    
  # Selected HUCs - Create an empty vector to hold all HUC click ids
  rv_clickedIds <- reactiveValues(ids = vector())
  session$userData$rv_clickedIds <- rv_clickedIds
  
  # Selected HUCs Cumulative System Capacity - store temporary CSC for selected HUCs
  rv_clickedIds_csc <- reactiveValues(csc = NA, var_csc = NA)
  session$userData$rv_clickedIds_csc <- rv_clickedIds_csc

  # Timer to prevent caching
  # (necessary in some modules for css issues with dygraphs)
  rv_timer <- reactiveValues(time = Sys.time())
  session$userData$rv_timer <- rv_timer

  # Show all csc plots (time consuming)
  rv_show_all_plots <- reactiveValues(show_all = NULL)
  session$userData$rv_show_all_plots <- rv_show_all_plots

  
# Joe model results holder - assume multiple runs
  rv_joe_model_results <- reactiveValues(
    sims = list()
  )
  session$userData$rv_joe_model_results <- rv_joe_model_results


# Joe model scenario name holder - assume multiple runs
  rv_joe_model_sim_names <- reactiveValues(
    scenario_names = list()
  )
  session$userData$rv_joe_model_sim_names <- rv_joe_model_sim_names


# Place holder for Joe Model estimated run times
  rv_joe_model_run_time <- reactiveValues(
    run_time_seconds = list()
  )
  session$userData$rv_joe_model_run_time <- rv_joe_model_run_time

  
#------------------------------------------
# Define other Reactive Values
#------------------------------------------

# See good tutorial for js/leaflet/shiny mouse events here: 
# https://medium.com/ibm-data-ai/capture-and-leverage-mouse-locations-and-clicks-on-leaflet-map-6d8601e466a5

# Flag to test if user cursor is over map
rv_map_shape <- reactiveVal(FALSE) # Single value
session$userData$rv_map_shape <- rv_map_shape

# Placeholders for the click locations of HUC IDs
rv_map_location <- reactiveValues(huc_id = NULL, huc_name = NULL, lat = NULL, lng = NULL) # List of values
session$userData$rv_map_location <- rv_map_location


  
  
  
  
  
  
  
  
 
}
