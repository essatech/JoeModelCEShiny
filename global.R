#---------------------------------------------------------------------
# Shiny Global Imports
# This is the Shiny App global script.
# This file is sourced and run once
# when the app first loads
# See tutorial here: https://shiny.rstudio.com/articles/scoping.html
#--------------------------------------------------------------------

# rm(list = ls())

# Load local package
library(devtools)
# remove.packages("JoeModelCE")
# devtools::install(pkg = "../package/JoeModelCE/", upgrade = "always")
# library(JoeModelCE)

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
library(reactlog)
library(shinydashboard)
library(shinydashboardPlus) # DROP?
library(waiter) # DROP?
library(dygraphs) # DROP?
library(shinyWidgets)
library(dygraphs)

library(sf)

# load libraries
library(rgdal); library(plyr)
library(shiny); library(DT); library(readxl)
library(leaflet); library(tidyr); library(tidyselect)
library(TruncatedDistributions); library(reshape2)
library(rmapshaper)

#load libraries for the functions
#library(pracma) #needed for fsolve


# Optionally enable react log - useful for debugging
reactlog::reactlog_enable()






#------------------------------------------
# Load in external data files
#------------------------------------------

# Load Stressor Magnitude and Response Files
file_name_stressor_response <- paste0("./data/stressor-response_fixed_ARTR.xlsx")
file_name_stressor_magnitude <- paste0("./data/stressor_magnitude_unc_ARTR.xlsx")

# File to read and view stressor-response relations
main.sheet <- read_xlsx(file_name_stressor_response, sheet="Main")

# Load in spatial data layer for leaflet map
HUC.Map <- st_read("./data/watersheds.gpkg")
print("Fix projection")
HUC.Map <- st_transform(HUC.Map, 4326)


HUC.Map$HUC_10 <- as.numeric(HUC.Map$HUC_10)
min(HUC.Map$HUC_10)
max(HUC.Map$HUC_10)
HUC.Map$test_val <- rnorm(n = nrow(HUC.Map))

color_func <- colorQuantile("YlOrRd",
               domain = HUC.Map$test_val,
               na.color = "lightgrey",
               n = 8)
color_vec <- color_func(HUC.Map$test_val)

leg_col <- lapply(c(-1, -0.5, 0, 0.5, 1), color_func) %>% unlist()
leg_lab <- c(-1, -0.5, 0, 0.5, 1)



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



























#------------------------------------------
# Deployment reminder checklist
#------------------------------------------
# turn off reactlog::reactlog_enable()
# turn on preloader
# load package functions into app


