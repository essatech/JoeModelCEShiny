#---------------------------------------------------------------------
# Shiny Server Function
# This is the Shiny App server function
# Note this app is build using the 
# global.R, server.R and ui.R paradigm
# with shiny modules
# See tutorial here: https://shiny.rstudio.com/articles/modules.html
#--------------------------------------------------------------------
server <- function(input, output, session) {
  
  # Setup reactive objects for the current session
  # These are used to store data that is used across modules
  module_setup_reactives(session)
  
  
  # Call main module server functions
  module_main_map_server("main_map")
  module_stressor_variable_server("stressor_variable")
  module_matrix_model_server("matrix_model")
  module_export_server("export")
  module_import_server("import")
  
  
  # Call the server function portion of the `scenario_table_module.R` module file
  print("Called scenario_table - ok")
  
  
}