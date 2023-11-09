#---------------------------------------------------------------------
# Shiny UI Function
# This is the Shiny App server function
# Note this app is build using the 
# global.R, server.R and ui.R paradigm
# with shiny modules
# See tutorial here: https://shiny.rstudio.com/articles/modules.html
#--------------------------------------------------------------------
# library(shiny); runApp()
ui <- dashboardPage(
  skin = "blue-light",
  
  # Pre-loader spinner - nice touch for final version?
  # preloader = list(html = tagList(spin_three_bounce(), "Loading Application..."), color = "#3c8dbc"),
  
  # --------------------- ----------------------------------------
  # HEADER - page title.
  header = dashboardHeader(
    title = "CEMPRA - Cumulative Effects Model for Prioritizing Recovery Actions",
    titleWidth = "100%"
  ),
  
  
  # -------------------------------------------------------------
  # SIDEBAR - sidebar navigation tab.
  sidebar = sidebar_tab_ui("sidebar_navigation"),
  
  
  # -------------------------------------------------------------
  # BODY - main body of application - large right-side tab.
  body = dashboardBody(
    
    # Import custom css files to html head tag
    tags$head(
      #tags$link(rel = "stylesheet", type = "text/css", href = "./css/bcgov.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "./css/ce_tool_style.css"),
      tags$script(src = "./js/leaf-hover.js"),
      tags$script(src = "./js/app_utility.js")
      #tags$script(src = "./js/updateAllInputs.js")
      
    ),
    
    # Import custom js files - note these are used with DT
    shinyFeedback::useShinyFeedback(),
    shinyjs::useShinyjs(),
    # Initialize JS script which updates the inputs after new scenario is loaded
    extendShinyjs(script = "./js/updateAllInputs.js", functions = c("updateAllInputs")),
    
    # Individual pages - right side tab content
    tabItems(
      tabItem("tab_about",
              module_about_ui("about")
      ),
      tabItem("tab_main_map",
              module_main_map_ui("main_map")
      ),
      tabItem("tab_ce_joe_model",
              module_ce_model_ui("ce_model")
      ),
      tabItem("tab_dose_response",
              module_dose_response_ui("dose_response")
      ),
      tabItem("tab_matrix_model",
              module_matrix_model_ui("matrix_model")
      ),
      tabItem("tab_stressor_sandbox",
        tags$p("tab_stressor_sandbox")
              #module_matrix_model_ui("matrix_model")
              # library(shiny); runApp()
      ),
      tabItem("tab_import",
              module_import_ui("import")
      ),
      tabItem("tab_export",
              module_export_ui("export")
      )
    )
  ),
  
  
  
  
  
)
