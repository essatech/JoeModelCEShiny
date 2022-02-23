#' Matrix Model INPUTS UI
#'
#' The UI portion of the matrix model
#'
#' @importFrom shiny NS tagList fluidRow column actionButton tags
#' @importFrom shinydashboard box
#'
#' @param id The id for this module
#'
#' @return a tagList containing UI elements
#'
module_matrix_model_inputs_ui <- function(id) {
  ns <- NS(id)
  
  tagList(class = "popmode_input",
    tags$h4("Survival Parameters"),
    
    
    fluidRow(
      column(
        width = 4,
        numericInput(ns("SE"), label = "Egg Survival", value = life_stages$Value[which(life_stages$Name == "SE")]),
      ),
      column(
        width = 4,
        numericInput(ns("S0"), label = "YOY Survival", value = life_stages$Value[which(life_stages$Name == "S0")]),
      ),
      column(
        width = 4,
        numericInput(ns("surv_1"), label = "Hatchling Survival", value = life_stages$Value[which(life_stages$Name == "surv_1")]),
      ),
    ),
    
    fluidRow(
      column(
        width = 4,
        numericInput(ns("surv_2"), label = "Juvenile Survival", value = life_stages$Value[which(life_stages$Name == "surv_2")]),
        
      ),
      column(
        width = 4,
        numericInput(ns("surv_3"), label = "Sub-Adult Survival", value = life_stages$Value[which(life_stages$Name == "surv_3")]),
        
      ),
      column(
        width = 4,
        numericInput(ns("surv_4"), label = "Adult Survival", value = life_stages$Value[which(life_stages$Name == "surv_4")]),
        
      ),
    ),
    
    
    fluidRow(
      column(
        width = 4,
        numericInput(ns("egg_rho"), label = "Correlation in egg fecundity through time", value = life_stages$Value[which(life_stages$Name == "egg_rho")]),
      ),
      column(
        width = 4,
        numericInput(ns("M.cv"), label = "Coefficient of variation in stage-specific mortality", value = life_stages$Value[which(life_stages$Name == "M.cv")]),
      ),
      column(
        width = 4,
        numericInput(ns("M.rho"), label = "Correlation in mortality through time", value = life_stages$Value[which(life_stages$Name == "M.rho")]),
        
      )
      
    ),
    
    tags$hr(),
    
    tags$h4("Growth Parameters"),
    
    fluidRow(
      column(
        width = 3,
        numericInput(ns("year_1"), label = "Years as Hatchling", value = life_stages$Value[which(life_stages$Name == "year_1")]),
      ),
      column(
        width = 3,
        numericInput(ns("year_2"), label = "Years as Juvenile", value = life_stages$Value[which(life_stages$Name == "year_2")]),
      ),
      column(
        width = 3,
        numericInput(ns("year_3"), label = "Years as Subadult", value = life_stages$Value[which(life_stages$Name == "year_3")]),
      ),
      column(
        width = 3,
        numericInput(ns("year_4"), label = "Years as Adult", value = life_stages$Value[which(life_stages$Name == "year_4")]),
      ),
    ),
    
    tags$hr(),
    
    
    tags$h4("Reproduction Parameters"),
    
    fluidRow(
      column(
        width = 4,
        numericInput(ns("events"), label = "Spawn Events per Female", value = life_stages$Value[which(life_stages$Name == "events")]),
      ),
      column(
        width = 4,
        numericInput(ns("eps"), label = "Eggs per Female Spawner", value = life_stages$Value[which(life_stages$Name == "eps")]),
      ),
      column(
        width = 4,
        numericInput(ns("eps_sd"), label = "Variance in Eggs per Female", value = life_stages$Value[which(life_stages$Name == "eps_sd")]),
      ),
      
    ),
    
    
    
    fluidRow(
      column(
        width = 6,
        numericInput(ns("egg_rho"), label = "Correlation in egg fecundity through time", value = life_stages$Value[which(life_stages$Name == "egg_rho")]),
      ),
      column(
        width = 6,
        numericInput(ns("int"), label = "Spawning Interval", value = life_stages$Value[which(life_stages$Name == "int")])
      )
    ),
    
    
    fluidRow(
      column(
        width = 3,
        numericInput(ns("mat_1"), label = "Maturity as Hatchling", value = life_stages$Value[which(life_stages$Name == "mat_1")]),
      ),
      column(
        width = 3,
        numericInput(ns("mat_2"), label = "Maturity as Juvenile", value = life_stages$Value[which(life_stages$Name == "mat_2")]),
      ),
      column(
        width = 3,
        numericInput(ns("mat_3"), label = "Maturity as Subadult", value = life_stages$Value[which(life_stages$Name == "mat_3")]),
      ),
      column(
        width = 3,
        numericInput(ns("mat_4"), label = "Maturity as Adult", value = life_stages$Value[which(life_stages$Name == "mat_4")]),
      )
    ),
    
    tags$hr(),
    
    
    tags$h4("Density Dependence"),
    
    fluidRow(column(
      width = 12,
      numericInput(ns("k"), label = "Adult Carrying Capacity (K)", value = life_stages$Value[which(life_stages$Name == "k")]),
      
    )),
    
    
    tags$h4("Compensation Ratios"),
    
    
    fluidRow(
      column(
        width = 4,
        numericInput(ns("cr_E"), label = "Egg Survival CR", value = life_stages$Value[which(life_stages$Name == "cr_E")]),
      ),
      column(
        width = 4,
        numericInput(ns("cr_0"), label = "YOY Survival CR", value = life_stages$Value[which(life_stages$Name == "cr_0")]),
      ),
      column(
        width = 4,
        numericInput(ns("cr_1"), label = "Hatchling Survival CR", value = life_stages$Value[which(life_stages$Name == "cr_1")]),
      ),
      
    ),
    
    
    
    fluidRow(
      column(
        width = 4,
        numericInput(ns("cr_2"), label = "Juvenile Survival CR", value = life_stages$Value[which(life_stages$Name == "cr_2")]),
      ),
      column(
        width = 4,
        numericInput(ns("cr_3"), label = "Subadult Survival CR", value = life_stages$Value[which(life_stages$Name == "cr_3")]),
      ),
      column(
        width = 4,
        numericInput(ns("cr_4"), label = "Adult Survival CR", value = life_stages$Value[which(life_stages$Name == "cr_4")]),
      ),
      
    ),
    
    
    tags$hr(),
    
    
    tags$h4("Other Parameters"),
    
    fluidRow(
      column(
        width = 6,
        numericInput(ns("SR"), label = "sex ratio", value = life_stages$Value[which(life_stages$Name == "SR")]),
      )
    )
    
  )
}


#' Matrix Model INPUTS SERVE
#'
#' @param none
#'
#' @return None
#'
module_matrix_model_inputs_server <- function(id) {
  moduleServer(id,
               function(input, output, session) {
                 ns <- session$ns
                 
                 print("matrix model inputs server")
                 
                 
               })
}