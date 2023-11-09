#---------------------------------------------------------------------
# Shiny Global Imports
# This is the Shiny App global script
# This file is sourced and run once when the app first loads
# See tutorial here: https://shiny.rstudio.com/articles/scoping.html
#--------------------------------------------------------------------

rm(list = ls()) # Remove all object from memory

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
library(rjson)

# TODO: remove this for deploy - puase on error
# options(shiny.error = browser)

# Shiny Pre-loader Spinner
options(
  spinner.color = "#ffffff",
  spinner.color.background = "#0073b7",
  spinner.size = 3
)

# Upload file size limit to 32MB
options(shiny.maxRequestSize = 32 * 1024 ^ 2)

# Useful leaflet demos - TODO delete
# https://github.com/IBM-DSE/Shiny-Examples-with-Blog

#-------------------------------------------------
# Load in default stressor response relationships
#-------------------------------------------------
# Load stressor-response Files
file_name_stressor_response <-
  paste0("./data/stressor_response_demo.xlsx")

# Extract the stressor-response relationships
sr_wb_dat <-
  JoeModelCE::StressorResponseWorkbook(filename = file_name_stressor_response)
names(sr_wb_dat)

start_time <- Sys.time()



#-------------------------------------------------
# Load in default Stressor magnitude
#-------------------------------------------------
# Extract the stressor magnitude values associated with each HUC
file_name_stressor_magnitude <-
  paste0("./data/stressor_magnitude_demo.xlsx")

# Stressor magnitude file
sm_wb_dat <-  JoeModelCE::StressorMagnitudeWorkbook(filename = file_name_stressor_magnitude,
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

# Layer bounds for initial load
# If a new polygon file is imported the layer is updated
# and map zoom and pan should change
bbox <- st_bbox(hmdl)
bbox_global <- bbox

# System Capacity Choropleth Map
# Color ramp is 0 - 100 (global) across all variables
color_func <- colorQuantile(
  c("#f22300", "#e0af00", "#ebcc2a",
    "#79b7c5", "#3b9ab2"),
  domain = c(0, 100),
  na.color = "lightgrey",
  n = 8
)

# Generate legend - will always be 0 - 100 for system capacity
leg_col <-
  lapply(c(0, 20, 40, 60, 80, 100), color_func) %>% unlist()
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
