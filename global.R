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
file.sources  <- list.files(path = "../JoeModelCE/R/", pattern="*.R")
sapply(paste0("../JoeModelCE/R/", file.sources), source, .GlobalEnv)


# Load necessary libraries
library(utils)
library(dplyr)
library(readxl)
library(shiny)
library(pkgload)
library(DT)
library(DBI)
library(RSQLite)
library(shinyjs)
library(shinycssloaders)
library(lubridate)
library(shinyFeedback)
library(dplyr)
library(dbplyr)
library(config)
library(RPostgreSQL) # DROP?
library(shinydashboard)
library(shinydashboardPlus) # DROP?
library(waiter) # DROP?
library(dygraphs) # DROP?
library(shinyWidgets)
library(dygraphs)
library(htmlwidgets)

library(sf)

# load libraries
library(rgdal)
library(plyr)
library(shiny)
library(DT)
library(readxl)
library(leaflet); library(tidyr)
library(tidyselect)
library(TruncatedDistributions)
library(reshape2)
library(rmapshaper)

#load libraries for the functions
#library(pracma) #needed for fsolve


# Optionally enable react log - useful for debugging
library(reactlog)
reactlog::reactlog_enable()


# https://medium.com/ibm-data-ai/capture-and-leverage-mouse-locations-and-clicks-on-leaflet-map-6d8601e466a5
# https://github.com/IBM-DSE/Shiny-Examples-with-Blog




#-------------------------------------------------
# Load in default stressor response relationships
#-------------------------------------------------
  # Load Stressor Response Files
  file_name_stressor_response <- paste0("./data/stressor-response_fixed_ARTR.xlsx")
  
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
  file_name_stressor_magnitude <- paste0("./data/stressor_magnitude_unc_ARTR.xlsx")
  
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
  hmdl$HUC_ID <- as.numeric(hmdl$HUC_ID)
  hmdl$uid <- paste0(hmdl$HUC_ID, "|", hmdl$NAME)

  # Which variable should be displayed first - alphabetical
  first_var <- sort(sr_wb_dat$stressor_names)[1]
  
  # Save default HUC to reactive values
  rv_HUC_geom <- reactiveValues(
    huc_geom = hmdl,              # Polygon geometry global reactive object
    leg_col = NA,                 # Legend color to display
    leg_lab = NA                  # Legend label to display
  )

  # Selected HUCs - Create an empty vector to hold all HUC click ids
  rv_clickedIds <- reactiveValues(ids = vector())
  
  # System Capacity choropleth map 0 - 1 color ramp is global
  color_func <- colorQuantile(c("#d7191c", "#fdae61", "#ffffbf",
                                "#a6d96a", "#1a9641"),
                              domain = c(0, 100),
                              na.color = "lightgrey",
                              n = 8)

  # Generate legend - will likely always be 0 - 1 for system capacity
  leg_col <- lapply(c(0, 20, 40, 60, 80, 100), color_func) %>% unlist()
  leg_lab <- c(0, 20, 40, 60, 80, 100)










#------------------------------------------
# Define Initial Static Values
#------------------------------------------

# Set number of Monte Carlo simulations for the Joe model
MC.sims <- 100

# Scenarios to run
scn.run <- "natural_unc"

# Boolean to trigger doses read from file
read.dose <- TRUE



#------------------------------------------
# Define Reactive Values
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


