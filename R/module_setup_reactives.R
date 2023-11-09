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
  
  rv_eigen_analysis <- reactiveValues(
    dat = list()
  )
  
  rv_demo_matricies <- reactiveValues(
    set = 1,
    dat = list()
  )
  
  rv_ea_errors <- reactiveValues(
    possible_error_state = FALSE,
    possible_error_msg = ""
  )
  
  rv_show_sample_plot <- reactiveValues(
    open = FALSE
  )
  
  rv_pop_sample_plot_data <- reactiveValues(
    dat = list(),
    run_counter = 1
  )
  
  # Sand box stressor values
  rv_sandbox_stressors <- reactiveValues(
    dat = list()
  )
  
  rv_pop_data_huc_ts <- reactiveValues(
    dat = list(),
    dat_baseline = list(),
    run_counter = 1,
    update_ts_plots = FALSE,
    joe_model_comp = NULL
  )
  
  rv_show_pop_main_plot <- reactiveValues(
    open = FALSE
  )
  
  
  
  
  
  
  
  
  
  
  
  
  
  # Initial load flag
  session$userData$rv_initial_load <- reactiveVal(NULL)
  
  # Excel data object
  rv_excel_data <- reactiveValues(
    rv_excel_data = NULL
  )
  
  # Add to userData object for session
  session$userData$rv_excel_data <- rv_excel_data
  
  # Bad crop names
  rv_crop_names <- reactiveValues(
    rv_only_in_price_fraction = NULL,
    rv_only_in_avg_yeild = NULL,
    rv_total_missing_area = NULL
  )
  session$userData$rv_crop_names <- rv_crop_names
  
  # Crop selection
  rv_crop_selection <- reactiveValues(
    rv_crop_selection = NULL
  )
  session$userData$rv_crop_selection <- rv_crop_selection
  
  # Crop selection inputs
  rv_crop_selection_inputs <- reactiveValues(
    pct_coverage = 1,
    yrs_dat = 3,
    start_year = NULL,
    stop_year = NULL
  )
  session$userData$rv_crop_selection_inputs <- rv_crop_selection_inputs
  
  
  # LTAY tables
  rv_ltay <- reactiveValues(
    crop = NULL,
    zone = NULL
  )
  session$userData$rv_ltay <- rv_ltay
  
  
  # Parcel report
  rv_parcel_data <- reactiveValues(
    dat = NULL
  )
  session$userData$rv_parcel_data <- rv_parcel_data
}
