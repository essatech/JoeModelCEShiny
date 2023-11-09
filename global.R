#---------------------------------------------------------------------
# Shiny Global Imports
# This is the Shiny App global script
# This file is sourced and run once when the app first loads
# See tutorial here: https://shiny.rstudio.com/articles/scoping.html
#--------------------------------------------------------------------

rm(list=ls()) # Remove all object from memory

# Download the Joe Model
# See download instructions here: https://github.com/essatech/JoeModelCE/
# library(devtools)
# devtools::install_github("essatech/JoeModelCE")
library(JoeModelCE) 

# Load necessary libraries
library(dplyr)
library(readxl)
library(writexl)
library(shiny)
library(DT)
library(shinyjs)
library(shinyFeedback)
library(shinydashboard)
library(shinydashboardPlus)
library(shinybusy)
library(waiter)
library(shinyWidgets)
library(htmlwidgets)
library(dygraphs) 
library(sf)
library(DT)
library(readxl)
library(leaflet)
library(tidyr)
library(reshape2)
library(popbio)
library(testthat)
library(ggplot2)
library(shinyvalidate)
library(ggthemes)
library(plotly)

# TODO: remove this for deploy - puase on error
# options(shiny.error = browser)

# Shiny Pre-loader Spinner
 options(spinner.color = "#ffffff", spinner.color.background = "#0073b7", spinner.size = 3)
 
# Upload file size limit to 32MB
 options(shiny.maxRequestSize = 32*1024^2)
 
# Useful leaflet demos - TODO delete
# https://github.com/IBM-DSE/Shiny-Examples-with-Blog

#-------------------------------------------------
# Load in default stressor response relationships
#-------------------------------------------------
  # Load stressor-response Files 
  file_name_stressor_response <- paste0("./data/stressor_response_demo.xlsx")

  # Extract the stressor-response relationships
  sr_wb_dat <- JoeModelCE::StressorResponseWorkbook(filename = file_name_stressor_response)
  names(sr_wb_dat)
  
  start_time <- Sys.time()
  


#-------------------------------------------------
# Load in default Stressor magnitude
#-------------------------------------------------
  # Extract the stressor magnitude values associated with each HUC
  file_name_stressor_magnitude <- paste0("./data/stressor_magnitude_demo.xlsx")

  # Stressor magnitude file
  sm_wb_dat <-  JoeModelCE::StressorMagnitudeWorkbook(
                    filename = file_name_stressor_magnitude,
                    scenario_worksheet = 1) # natural_unc

  
#------------------------------------------------------
# Load in the life stages file for the population model
#------------------------------------------------------
  # When app launches defaults will be loaded from this file
  life_stages <- read.csv("./data/life cycles.csv")
  
 
  
  
  
#-------------------------------------------------
# Map geometry and map object reactive values
#-------------------------------------------------
  
  # Load in the default watersheds geojson layer - Athabasca
  hmdl <- sf::st_read("./data/watersheds.gpkg")
  
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
rv_map_location <- reactiveValues(huc_id = NULL, huc_name = NULL, lat = NULL, lng = NULL) # List of values



#------------------------------------------
# Deployment reminder checklist
#------------------------------------------
# turn off reactlog::reactlog_enable()
# turn on preloader in ui.R (preloader)
# load package functions into app
# Optionally enable react log - useful for debugging
# library(reactlog)
# reactlog::reactlog_enable()
# shiny::reactlogShow() # Run this once app closes

#------------------------------------------
# Deployment reminder checklist
#------------------------------------------
# library(shinytest2)
# shinytest2::record_test()
# shinytest2::test_app()

# Run shiny app in browser
# shiny::runApp(launch.browser = TRUE)


