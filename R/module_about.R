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
          
          tags$p("Welcome to the Joe Model Cumulative Effects and Population Model Shiny app!"),
          
          tags$p("This interactive webtool is powered by the",
                 tags$a(href = "https://github.com/essatech/JoeModelCE","JoeModelCE R package,"), 
                 "which is a collection of functions to support the application of the Alberta Environmental Parks Cumulative Effects Assessment Joe Model",
                 tags$a(href = "https://waves-vagues.dfo-mpo.gc.ca/Library/40871344.pdf", "(AEP CEM; DFO 2019)"),
                 ", coupled with a flexible population modelling framework."),
          
          tags$p("This webtool is part of a larger initiative to develop a framework for modelling cumulative impacts to Species at Risk (SAR) to guide recovery planning and adaptive management based on stressor-response functions related to taxa-specific threats. This framework allows users to generate models across a range of complexity and data quality, treating stressor-response functions as modular entities."),
          
          style = "text-align: center;",
          
        ),
        
        
        tags$h4("Contributors"),
        
        tags$p("This project is a broad collaboration between Fisheries and Oceans Canada (DFO), B.C. Ministry of Environment and Climate Change Strategy (ECCS), Alberta Environment and Parks (AEP), and Simon Fraser University (SFU)."),
        
        tags$div(tags$ul(
          
          tags$li(
            tags$a(href = "https://profils-profiles.science.gc.ca/en/profile/eva-enders", "Dr. Eva Enders:"),
            "Project Lead; DFO Research Scientist"),
          
          tags$li(
            tags$a(href = "http://www.aferu.ca/rosenfeld-lab", "Dr. Jordan Rosenfeld:"),
            "Project design and coordination; ECCS Aquatic Ecologist"),
          
          tags$li(
            tags$a(href = "https://github.com/andrewpaul68", "Dr. Andrew Paul:"),
            "Collaborator; AEP Research Scientist"),
          
          tags$li(
            tags$a(href = "https://github.com/klwilson23", "Dr. Kyle Wilson:"),
            "Population model development"),
          
          tags$li(
            tags$a(href = "https://www.linkedin.com/in/isuru-dharmasena-90269895/?originalSubdomain=ca", "Isuru Dharmasena:"),
            "Core Shiny app development"),
          
          tags$li(
            tags$a(href = "https://github.com/mattjbayly", "Matthew Bayly,"), 
            "Marc Porter, Alejandra Urcelay, and",
            tags$a(href = "https://github.com/julianheavyside", "Julian Heavyside"),
            "from",
            tags$a(href = "https://essa.com", "ESSA Technologies Ltd:"),
            "R package and Shiny app development support"),
        ), 
        
        style = "font-size: 15px"),
        
        
        tags$h4("Features"),
        
        tags$div(tags$ul(
          tags$li("Run custom implementations of the Joe Model on non-standard data formats."),
          tags$li("Batch-run the integrated Joe Model/Population model across large datasets."),
          tags$li("Run sensitivity tests."),
          tags$li("Explore model extensions.")
        ),  
        
        style = "font-size: 15px"
        
        ),
        
      ),
    )
  )
}