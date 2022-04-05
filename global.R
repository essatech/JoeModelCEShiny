#---------------------------------------------------------------------
# Shiny Global Imports
# This is the Shiny App global script.
# This file is sourced and run once when the app first loads
# See tutorial here: https://shiny.rstudio.com/articles/scoping.html
#--------------------------------------------------------------------

rm(list = ls())


# Load local package
# library(devtools) 
# remove.packages("JoeModelCE")
# devtools::install(pkg = "../package/JoeModelCE/", upgrade = "always")
# devtools::install_github("essatech/JoeModelCE")
# library(JoeModelCE) 
# file.sources  <- list.files(path = "../JoeModelCE/R/", pattern = "*.R")
# sapply(paste0("../JoeModelCE/R/", file.sources), source, .GlobalEnv)
library(JoeModelCE) 

# Load necessary libraries
library(dplyr)
library(readxl)
library(shiny)
library(DT)
library(shinyjs)
library(shinycssloaders) # DROP
library(lubridate) # DROP
library(shinyFeedback)
# library(dbplyr)
# library(config) # DROP
library(shinydashboard)
library(shinydashboardPlus)
library(shinybusy)
library(waiter)
library(shinyWidgets) # DROP
library(htmlwidgets)
#library(highcharter) # DROP
library(dygraphs) 
library(sf)
#library(rgdal) # DROP
library(DT)
library(readxl)
library(leaflet)
library(tidyr) # DROP
#library(tidyselect) # DROP
library(reshape2)
#library(rmapshaper) # DROP
library(popbio)
library(testthat)
library(ggplot2)
library(shinyvalidate)
library(ggthemes)
library(plotly)



#load libraries for the functions
# library(pracma) #needed for fsolve

# Optionally enable react log - useful for debugging
 library(reactlog)
 reactlog::reactlog_enable()
 # shiny::reactlogShow() # Run this once app closes


# TODO: remove this for deploy - puase on error
# options(shiny.error = browser)

# Shiny Pre-loader Spinner
 options(spinner.color = "#ffffff", spinner.color.background = "#0073b7", spinner.size = 3)


# Useful leaflet demos - TODO delete
# https://github.com/IBM-DSE/Shiny-Examples-with-Blog


#-------------------------------------------------
# Load in default stressor response relationships
#-------------------------------------------------
  # Load Stressor Response Files 
  file_name_stressor_response <- paste0("./data/stressor_response_demo.xlsx")
  #file_name_stressor_response <- paste0("./data/stressor-response_fixed_sqam.xlsx")

  
  # Extract the stressor response relationships
  sr_wb_dat <- JoeModelCE::StressorResponseWorkbook(filename = file_name_stressor_response)
  names(sr_wb_dat)
  
  start_time <- Sys.time()
  
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
    hover_values           = FALSE
  )

  

#-------------------------------------------------
# Load in default Stressor magnitude
#-------------------------------------------------
  # Extract the stressor magnitude values associated with each HUC
  # Fixed: stressor_magnitude_fixed_rn_ARTR
  # UNC: stressor_magnitude_unc_ARTR
  #file_name_stressor_magnitude <- paste0("./data/stressor_magnitude_unc_ARTR.xlsx")
  file_name_stressor_magnitude <- paste0("./data/stressor_magnitude_demo.xlsx")
  #file_name_stressor_magnitude <- paste0("./data/stressor_magnitude_fixed_rn_sqam.xlsx")
  
  
  sm_wb_dat <-  JoeModelCE::StressorMagnitudeWorkbook(
                    filename = file_name_stressor_magnitude,
                    scenario_worksheet = 1) # natural_unc
  
  # Designate stressor magnitude as reactive values
  rv_stressor_magnitude <- reactiveValues(
    sm_dat = sm_wb_dat
  )
  
  
#------------------------------------------------------
# Load in the life stages file for the population model
#------------------------------------------------------
  # When app launches defaults will be loaded from this file
  life_stages <- read.csv("./data/life cycles.csv")
  
  # Set as a reactive object
  rv_life_stages <- reactiveValues(
    dat = life_stages
  )
  
  rv_eigen_analysis <- reactiveValues(
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
    run_counter = 1,
    update_ts_plots = FALSE
  )
  
  rv_show_pop_main_plot <- reactiveValues(
    open = FALSE
  )
  
  
  
#-------------------------------------------------
# Map geometry and map object reactive values
#-------------------------------------------------
  # Load in the default watersheds geojson layer - Athabasca
  hmdl <- sf::st_read("./data/watersheds.gpkg")
  #hmdl <- sf::st_read("./data/sqam.gpkg")
  
  hmdl$HUC_ID <- as.numeric(hmdl$HUC_ID)
  hmdl$uid <- paste0(hmdl$HUC_ID, "|", hmdl$NAME)

  # Which variable should be displayed first - alphabetical
  first_var <- sort(sr_wb_dat$stressor_names)[1]
  
  # Save default HUC to reactive values
  rv_HUC_geom <- reactiveValues(
    huc_geom = hmdl,              # Polygon geometry global reactive object
    leg_col = NA,                 # Legend color to display
    leg_lab = NA,                 # Legend label to display
    color_df = NA                 # Color reference data frame
  )
  
  # Layer bounds for initial load
  # If a new polygon file is imported the layer is updated
  # and map zoom and pan should change
  bbox <- st_bbox(hmdl)
  rv_HUC_layer_load <- reactiveValues(
    data = hmdl,
    xmin = bbox$xmin,
    ymin = bbox$ymin,
    xmax = bbox$xmax,
    ymax = bbox$ymax,
    reload_map = TRUE,
    add_polygons = FALSE
  )
  
  # Trigger redraw to clear selection
  rv_redraw <- reactiveValues(
    redraw = 0
  )
    
  # Selected HUCs - Create an empty vector to hold all HUC click ids
  rv_clickedIds <- reactiveValues(ids = vector())
  
  # Selected HUCs Cumulative System Capacity - store temporary CSC for selected HUCs
  rv_clickedIds_csc <- reactiveValues(csc = NA, var_csc = NA)
  
  # Timer to prevent caching
  # (necessary in some modules for css issues with dygraphs)
  rv_timer <- reactiveValues(time = Sys.time())
  
  # Show all csc plots (time consuming)
  rv_show_all_plots <- reactiveValues(show_all = NULL)
  
  
  
  # System Capacity Choropleth Map 
  # Color ramp is 0 - 100 (global) across all variables
  color_func <- colorQuantile(c("#f22300", "#e0af00", "#ebcc2a",
                                "#79b7c5", "#3b9ab2"),
                              domain = c(0, 100),
                              na.color = "lightgrey",
                              n = 8)

  # Generate legend - will always be 0 - 100 for system capacity
  leg_col <- lapply(c(0, 20, 40, 60, 80, 100), color_func) %>% unlist()
  leg_lab <- c(0, 20, 40, 60, 80, 100)



#------------------------------------------------------
# Joe Model initial default settings and result holder
#------------------------------------------------------

# Set number of Monte Carlo simulations for the Joe model
  MC.sims <- 100

# Scenarios to run
  scn.run <- "natural_unc"

# Boolean to trigger doses read from file
  read.dose <- TRUE

# Joe model results holder - assume multiple runs
  rv_joe_model_results <- reactiveValues(
    sims = list()
  )
# Joe model scenario name holder - assume multiple runs
  rv_joe_model_sim_names <- reactiveValues(
    scenario_names = list()
  )
# Place holder for Joe Model estimated run times
  rv_joe_model_run_time <- reactiveValues(
    run_time_seconds = list()
  )


#------------------------------------------
# Define other Reactive Values
#------------------------------------------

# See good tutorial for js/leaflet/shiny mouse events here: 
# https://medium.com/ibm-data-ai/capture-and-leverage-mouse-locations-and-clicks-on-leaflet-map-6d8601e466a5

# Flag to test if user cursor is over map
rv_map_shape <- reactiveVal(FALSE) # Single value

# Placeholders for the click locations of HUC IDs
# TODO MOVE BACK
rv_map_location <- reactiveValues(huc_id = NULL, huc_name = NULL, lat = NULL, lng = NULL) # List of values




#------------------------------------------
# Deployment reminder checklist
#------------------------------------------
# turn off reactlog::reactlog_enable()
# turn on preloader in ui.R (preloader)
# load package functions into app


