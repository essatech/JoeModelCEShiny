#' module_about_ui
#'
#' The UI portion of the about module
#'
#' @importFrom shiny NS tagList fluidRow column actionButton tags
#' @importFrom shinydashboard box
#' @importFrom li shiny
#'
#' @param id The id for this module
#'
#' @return a tagList containing UI elements
#'
#'
#'
module_about_ui <- function(id) {
  ns <- NS(id)
  
  tagList(fluidRow(
    class = "cover-background",
    
    tags$div(
      class = "neat-text",
      
      style = "justify-content: center;
    text-align: center;",
      
      tags$h1("Joe Model Cumulative Effect Tool", class = "main-title"),


        shiny::HTML(
          '<iframe 
          style = "border-style: solid;
      border-width: 5px;
      border-radius: 4px;
      border-color: #e0af00;"
          width="655" height="355" src="https://www.youtube.com/embed/Cp3UdWlkaKU" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'
        ),
      
      
      shiny::br(),
      
      
      fluidRow(
        column(width = 2),
        column(width = 8,
        
      shinydashboard::box(
        width = NULL,
      
        tags$h4("Welcome to the Joe Model Cumulative Effects and Population Model Shiny app!", class = "ce-header-font"),

        tags$p(
          "This interactive webtool is powered by the",
          tags$a(href = "https:essatech.github.io/JoeModelCE", "JoeModelCE R package,"),
          "which is a collection of functions to support the application of the Alberta Environmental Parks Cumulative Effects Assessment Joe Model",
          tags$a(href = "https://waves-vagues.dfo-mpo.gc.ca/Library/40871344.pdf", "(AEP CEM; DFO 2019),"),
          "coupled with a flexible population modelling framework. The AEP Joe Model is designed to assess threats to aquatic species within Alberta and prioritize recovery, research, and regulation efforts to watersheds of concern."
        ),
        
        tags$p(
          "This webtool is part of a larger initiative to develop a framework for modelling cumulative impacts to Species at Risk (SAR). It is designed to examine stressor-response functions related to taxa-specific threats and guide recovery planning and adaptive management. The framework allows users to generate models across a range of complexity and data quality, treating stressor-response functions as modular entities. The Joe Model Cumulative Effect Tool is especially well suited for modelling cumulative effect and summarizing landscape-scale stressors for non-target fish species."
        ),
      
      tags$h3("Contributors", class = "ce-header-font"),
      
      tags$p(
        "This project is a broad collaboration between Fisheries and Oceans Canada (DFO), B.C. Ministry of Environment and Climate Change Strategy (ECCS), Alberta Environment and Parks (AEP), and Simon Fraser University (SFU)."
      ),
      
      tags$div(
        tags$ul(style = "list-style-type:none",
          
          tags$li(
                  tags$a(href = "http://www.aferu.ca/rosenfeld-lab", "Dr. Jordan Rosenfeld:"),
                  "Project design and coordination; ECCS Aquatic Ecologist"
          ),
                
          tags$li(
            tags$a(href = "https://profils-profiles.science.gc.ca/en/profile/eva-enders", "Dr. Eva Enders:"),
            "Project Lead; DFO Research Scientist"
          ),
          
          
          tags$li(
            tags$a(href = "https://github.com/andrewpaul68", "Dr. Andrew Paul:"),
            "Collaborator; AEP Research Scientist"
          ),
          
          tags$li(
            tags$a(href = "https://github.com/klwilson23", "Dr. Kyle Wilson:"),
            "Population model development"
          ),
          
          tags$li(
            tags$a(href = "https://www.linkedin.com/in/isuru-dharmasena-90269895/?originalSubdomain=ca", "Isuru Dharmasena:"),
            "Core Shiny app development"
          ),
          
          tags$li(
            tags$a(href = "https://github.com/mattjbayly", "Matthew Bayly,"),
            "Marc Porter, Alejandra Urcelay, and",
            tags$a(href = "https://github.com/julianheavyside", "Julian Heavyside"),
            "from",
            tags$a(href = "https://essa.com", "ESSA Technologies Ltd:"),
            "R package and Shiny app development support"
          ),
        ),
        
      ),
      
      
      tags$h3("Features", class = "ce-header-font"),
      
      tags$div(tags$ul(style = "list-style-type:none",
        tags$li(
          "Run custom implementations of the Joe Model on non-standard data formats."
        ),
        tags$li(
          "Batch-run the integrated Joe Model/Population model across large datasets."
        ),
        tags$li("Run sensitivity tests."),
        tags$li("Explore model extensions.")
      )
      ),
      
    ), # end of box
    
        ), # end of column
    column(width = 2),
      ), # end of fluid row
      
    ), # end of main page div
    
  )) # tag list and fluid row
}