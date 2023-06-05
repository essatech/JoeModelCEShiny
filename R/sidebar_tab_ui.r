#' Sidear navigation tab
#'
#' The UI portion sidebar menu
#'
#' @importFrom shiny NS tagList fluidRow column actionButton tags
#' @importFrom shinydashboard sidebarMenu dashboardSidebar menuItem menuSubItem
#'
#' @param id The id for this module
#'
#' @return a dashboardSidebar UI element
#'
sidebar_tab_ui <- function(id) {

  # `NS(id)` returns a namespace function, which was save as `ns` and will invoke later.
  ns <- NS(id)

  sidebar <- dashboardSidebar(

    #tags$img(src = "./img/banner.png", width = "100%"),

    # Setting id makes input$tabs give the tabName of currently-selected tab
    shinydashboard::sidebarMenu(
      id = "tabs",
      menuItem("About", tabName = "tab_about", icon = icon("info")),
      menuItem("Map Overview", tabName = "tab_main_map", icon = icon("map")),
      menuItem("Population Model", tabName = "tab_matrix_model", icon = icon("sliders-h")),
      menuItem("Stressor Sandbox", tabName = "tab_stressor_sandbox", icon = icon("asterisk")),
      menuItem("Upload Data", tabName = "tab_import", icon = icon("upload")),
      menuItem("Download Data", tabName = "tab_export", icon = icon("download"))
      
    )
  )

  # Return the sidebar navigation tab
  return(sidebar)

}


