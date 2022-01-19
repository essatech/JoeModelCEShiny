#' module_about_ui
#'
#' The UI portion of the about module
#' 
#' @importFrom shiny NS tagList fluidRow column actionButton tags
#' @importFrom shinydashboard box
#'
#' @param id The id for this module
#'
#' @return a tagList containing UI elements
#'
#'
#'
module_about_ui <- function(id) {

  ns <- NS(id)

  tagList(
    fluidRow(
      shinydashboard::box(
        width = 12,
        
        tags$div(
          
          tags$h1("Joe/Population Model CE"),
          tags$img(src = './img/cover_image.png', height = '300px'),
          
          tags$p("The Joe Model Cumulative Effects package (JoeModelCE) is a collection of functions to support the application of the ‘Alberta Environmental Parks Cumulative Effects Assessment Joe Model’ (add citation) coupled with a flexible population modelling framework. This package is accompanied by an interactive web-based R Shiny dashboard (add link). The intent of the package."),
          
          style = "text-align: center;",
          
        ),
        

        

        
        
        
        tags$h4("Contributors"),
        tags$p("The JoeModelCE package is being developed as part of a larger initiative between groups [A], [B] & [C]. 
Contributors include:"),
        
        tags$div(tags$ul(
          tags$li("Jordan Rosenfled (TODO: add description) ..."),
          tags$li("Andrew Paul ..."),
          tags$li("Eva Enders ..."),
          tags$li("Kyle Wilson  ..."),
          tags$li("Isuru Dharmasena  ..."),
          tags$li("others ..."),
          tags$li("ESSA Technologies Ltd ...")
          ),  style = "font-size: 15px"),
        
        tags$h4("Features"),
        
        tags$div(tags$ul(
          tags$li("Run custom implementations of the Joe Model on non-standard data formats."),
          tags$li("plaeholder ..."),
          tags$li("plaeholder ..."),
          tags$li("plaeholder ...")
        ),  style = "font-size: 15px"),
        
      
        

        
      ),
    )




  )
}