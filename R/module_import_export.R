#' module_import_export_ui
#'
#' The UI portion of the import export model
#' 
#' @importFrom shiny NS tagList fluidRow column actionButton tags
#' @importFrom shinydashboard box
#'
#' @param id The id for this module
#'
#' @return a tagList containing UI elements
#'
module_import_export_ui <- function(id) {

  ns <- NS(id)

  tagList(
    shinydashboard::box(
      width = 12,
      tags$h3("Data Upload and Results Export"),
      
      fluidRow(
        column(
          width = 6,
          tags$p("Click the ‘browse…’ button to upload files. Note that uploading a new data source will overwrite all local changes and you will need to re-run your results. Stressor response files, stressor magnitude files and watershed polygons must all be validated externally, or you will experience errors. Please see notes below to ensure proper data format."),
        )
      ),
      
      fluidRow(
        shinydashboard::box(
          accordion(
            id = "accordion1",
            accordionItem(
              title = "Stressor Response Workbook",
              collapsed = TRUE,
              tagList(
                tags$p("Quisque nec metus auctor, efficitur nisi eu, porta quam. Integer tincidunt lobortis nisl. Interdum et malesuada fames ac ante ipsum primis in faucibus. Integer malesuada diam sit amet mattis fermentum. Proin vestibulum, odio eu tristique vestibulum, dui ex dignissim purus, ut placerat nunc orci quis leo. Ut ligula diam, gravida id elementum a, sagittis vel sapien. Donec id est sed urna condimentum rutrum. Curabitur at eros et elit molestie viverra vel vel magna. Phasellus congue dignissim quam, et eleifend felis pharetra eget. Fusce volutpat elit in ligula tincidunt, eu tincidunt libero ullamcorper. Suspendisse imperdiet lobortis tortor non fermentum. Pellentesque pharetra erat nec neque aliquet, ut congue turpis iaculis. Nam porta tincidunt elementum. Sed non erat maximus dolor ullamcorper congue sed et turpis. Aliquam in lacus elit."),
              )
            )
          ),
          fileInput("file1", label = NULL,
                    multiple = FALSE,
                    accept = c("text/csv",
                               "text/comma-separated-values,text/plain",
                               ".csv"))
        ),
      ),
    
      fluidRow(
        shinydashboard::box(
          accordion(
            id = "accordion2",
            accordionItem(
              title = "Stressor Magnitude Workbook",
              collapsed = TRUE,
              tagList(
                tags$p("Quisque nec metus auctor, efficitur nisi eu, porta quam. Integer tincidunt lobortis nisl. Interdum et malesuada fames ac ante ipsum primis in faucibus. Integer malesuada diam sit amet mattis fermentum. Proin vestibulum, odio eu tristique vestibulum, dui ex dignissim purus, ut placerat nunc orci quis leo. Ut ligula diam, gravida id elementum a, sagittis vel sapien. Donec id est sed urna condimentum rutrum. Curabitur at eros et elit molestie viverra vel vel magna. Phasellus congue dignissim quam, et eleifend felis pharetra eget. Fusce volutpat elit in ligula tincidunt, eu tincidunt libero ullamcorper. Suspendisse imperdiet lobortis tortor non fermentum. Pellentesque pharetra erat nec neque aliquet, ut congue turpis iaculis. Nam porta tincidunt elementum. Sed non erat maximus dolor ullamcorper congue sed et turpis. Aliquam in lacus elit."),
              )
            )
          ),
          fileInput("file2", label = NULL,
                    multiple = FALSE,
                    accept = c("text/csv",
                               "text/comma-separated-values,text/plain",
                               ".csv"))
        ),
      ),
      
      fluidRow(
        shinydashboard::box(
          accordion(
            id = "accordion3",
            accordionItem(
              title = " Watershed Polygons (Spatial)",
              collapsed = TRUE,
              tagList(
                tags$p("Quisque nec metus auctor, efficitur nisi eu, porta quam. Integer tincidunt lobortis nisl. Interdum et malesuada fames ac ante ipsum primis in faucibus. Integer malesuada diam sit amet mattis fermentum. Proin vestibulum, odio eu tristique vestibulum, dui ex dignissim purus, ut placerat nunc orci quis leo. Ut ligula diam, gravida id elementum a, sagittis vel sapien. Donec id est sed urna condimentum rutrum. Curabitur at eros et elit molestie viverra vel vel magna. Phasellus congue dignissim quam, et eleifend felis pharetra eget. Fusce volutpat elit in ligula tincidunt, eu tincidunt libero ullamcorper. Suspendisse imperdiet lobortis tortor non fermentum. Pellentesque pharetra erat nec neque aliquet, ut congue turpis iaculis. Nam porta tincidunt elementum. Sed non erat maximus dolor ullamcorper congue sed et turpis. Aliquam in lacus elit."),
              )
            )
          ),
          fileInput("file2", label = NULL,
                    multiple = FALSE,
                    accept = c("text/csv",
                               "text/comma-separated-values,text/plain",
                               ".csv"))
        ),
      ),
      
      
      fluidRow(
        shinydashboard::box(
          accordion(
            id = "accordion4",
            accordionItem(
              title = "Population Model Vital Rates",
              collapsed = TRUE,
              tagList(
                tags$p("Quisque nec metus auctor, efficitur nisi eu, porta quam. Integer tincidunt lobortis nisl. Interdum et malesuada fames ac ante ipsum primis in faucibus. Integer malesuada diam sit amet mattis fermentum. Proin vestibulum, odio eu tristique vestibulum, dui ex dignissim purus, ut placerat nunc orci quis leo. Ut ligula diam, gravida id elementum a, sagittis vel sapien. Donec id est sed urna condimentum rutrum. Curabitur at eros et elit molestie viverra vel vel magna. Phasellus congue dignissim quam, et eleifend felis pharetra eget. Fusce volutpat elit in ligula tincidunt, eu tincidunt libero ullamcorper. Suspendisse imperdiet lobortis tortor non fermentum. Pellentesque pharetra erat nec neque aliquet, ut congue turpis iaculis. Nam porta tincidunt elementum. Sed non erat maximus dolor ullamcorper congue sed et turpis. Aliquam in lacus elit."),
              )
            )
          ),
          fileInput("file4", label = NULL,
                    multiple = FALSE,
                    accept = c("text/csv",
                               "text/comma-separated-values,text/plain",
                               ".csv"))
        ),
      ),
      

      

      
      

      
      
      
    )
  )
}