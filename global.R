#---------------------------------------------------------------------
# Shiny Global Imports
# This is the Shiny App global script.
# This file is sourced and run once when the app first loads
# See tutorial here: https://shiny.rstudio.com/articles/scoping.html
#--------------------------------------------------------------------

# rm(list = ls())

# Load local package
library(devtools) 
# remove.packages("JoeModelCE")
# devtools::install(pkg = "../package/JoeModelCE/", upgrade = "always")
# library(JoeModelCE) 
file.sources  <- list.files(path = "../JoeModelCE/R/", pattern = "*.R")
sapply(paste0("../JoeModelCE/R/", file.sources), source, .GlobalEnv)


# Load necessary libraries
#library(utils) # DROP
library(dplyr)
library(readxl)
library(shiny)
#library(pkgload) # DROP
library(DT)
#library(DBI) # DROP
#library(RSQLite) # DROP
library(shinyjs)
library(shinycssloaders) # DROP
library(lubridate) # DROP
library(shinyFeedback)
library(dbplyr)
library(config) # DROP
#library(RPostgreSQL) # DROP
library(shinydashboard)
library(shinydashboardPlus)
library(shinybusy)
library(waiter) # DROP
#library(dygraphs) # DROP
library(shinyWidgets) # DROP
library(htmlwidgets)
library(highcharter) # KEEP .. maybe drop
library(dygraphs) # use this instead of high chaets...
library(sf)
library(rgdal) # DROP
library(DT)
library(readxl)
library(leaflet)
library(tidyr) # DROP
library(tidyselect) # DROP
# library(TruncatedDistributions) found on R-Forge and not CRAN: code to install and load is in "R/module_joe_model_csc_plots.R"
library(reshape2)
library(rmapshaper) # DROP
library(popbio)
library(testthat)



#load libraries for the functions
#library(pracma) #needed for fsolve


# Optionally enable react log - useful for debugging
library(reactlog)
reactlog::reactlog_enable()

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
  file_name_stressor_response <- paste0("./data/stressor-response_fixed_ARTR.xlsx")
  #file_name_stressor_response <- paste0("./data/stressor-response_fixed_sqam.xlsx")

  
  # Extract the stressor response relationships
  sr_wb_dat <- StressorResponseWorkbook(filename = file_name_stressor_response)
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
  file_name_stressor_magnitude <- paste0("./data/stressor_magnitude_fixed_rn_ARTR.xlsx")
  #file_name_stressor_magnitude <- paste0("./data/stressor_magnitude_unc_ARTR.xlsx")
  #file_name_stressor_magnitude <- paste0("./data/stressor_magnitude_fixed_rn_sqam.xlsx")
  
  
  sm_wb_dat <-  StressorMagnitudeWorkbook(
                    filename = file_name_stressor_magnitude,
                    scenario_worksheet = 1) # natural_unc
  
  # Designate stressor magnitude as reactive values
  rv_stressor_magnitude <- reactiveValues(
    sm_dat = sm_wb_dat
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
  
  # System Capacity Choropleth Map 
  # Color ramp is 0 - 100 (global) across all variables
  color_func <- colorQuantile(c("#d7191c", "#fdae61", "#ffffbf",
                                "#a6d96a", "#1a9641"),
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
rv_map_location <- reactiveValues(huc_id = NULL, huc_name = NULL, lat = NULL, lng = NULL) # List of values





#------------------------------------------
# Deployment reminder checklist
#------------------------------------------
# turn off reactlog::reactlog_enable()
# turn on preloader
# load package functions into app


