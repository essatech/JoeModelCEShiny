#' Matrix Model Inputs UI
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
  
  tagList(
    class = "popmode_input",
    tags$h4("Survival Parameters"),
    
    tags$p("Define the individual survivorship probabilities for each stage class evaluated on annual time steps in the simulation.", class = "pm-ht"),
    
    fluidRow(
      column(
        width = 4,
        numericInput(ns("SE"), label = "SE (Egg Survival)", value = life_stages$Value[which(life_stages$Name == "SE")]),
      ),
      column(
        width = 4,
        numericInput(ns("S0"), label = "S0 (YOY Survival)", value = life_stages$Value[which(life_stages$Name == "S0")]),
      ),
      column(
        width = 4,
        numericInput(ns("surv_1"), label = "surv_1 (Stage 1 Survival)", value = life_stages$Value[which(life_stages$Name == "surv_1")]),
      ),
    ),
    
    fluidRow(
      column(
        width = 4,
        numericInput(ns("surv_2"), label = "surv_2 (Stage 2 Survival)", value = life_stages$Value[which(life_stages$Name == "surv_2")]),
        
      ),
      column(
        width = 4,
        numericInput(ns("surv_3"), label = "surv_3 (Stage 3 Survival)", value = life_stages$Value[which(life_stages$Name == "surv_3")]),
        
      ),
      column(
        width = 4,
        numericInput(ns("surv_4"), label = "surv_4 (Stage 4 Survival)", value = life_stages$Value[which(life_stages$Name == "surv_4")]),
        
      ),
    ),
    
    tags$p("Interannual stochasticity is introduced into the model. Mortality also correlates intra-annually among size classes and over time (correlation diminishes as distance between stages increases).", class = "pm-ht"),
    
    fluidRow(
      column(
        width = 6,
        numericInput(ns("M.cv"), label = "M.cv (Coefficient of variation in stage-specific mortality)", value = life_stages$Value[which(life_stages$Name == "M.cv")]),
      ),
      column(
        width = 6,
        numericInput(ns("M.rho"), label = "M.rho (Correlation in mortality through time)", value = life_stages$Value[which(life_stages$Name == "M.rho")]),
        
      )
      
    ),
    
    tags$hr(),
    
    tags$h4("Growth Parameters"),
    
    tags$p("Growth is represented as time (years) spent in each stage. This circumvents defining size attributes for each stage and allows for more flexibility to parameterize different species in the model.", class = "pm-ht"),
    
    fluidRow(
      column(
        width = 3,
        numericInput(ns("year_1"), label = "year_1 (Years as Stage 1)", value = life_stages$Value[which(life_stages$Name == "year_1")]),
      ),
      column(
        width = 3,
        numericInput(ns("year_2"), label = "year_2 (Years as Stage 2)", value = life_stages$Value[which(life_stages$Name == "year_2")]),
      ),
      column(
        width = 3,
        numericInput(ns("year_3"), label = "year_3 (Years as Stage 3)", value = life_stages$Value[which(life_stages$Name == "year_3")]),
      ),
      column(
        width = 3,
        numericInput(ns("year_4"), label = "year_4 (Years as Stage 4)", value = life_stages$Value[which(life_stages$Name == "year_4")]),
      ),
    ),
    
    tags$hr(),
    
    
    tags$h4("Reproduction Parameters"),
    
    tags$p("The primary reproduction parameters include the spawning events per female (within a year), eggs per female spawner and variance in eggs per female.", class = "pm-ht"),
    
    fluidRow(
      column(
        width = 4,
        numericInput(ns("events"), label = "events (Spawn Events per Female)", value = life_stages$Value[which(life_stages$Name == "events")]),
      ),
      column(
        width = 4,
        numericInput(ns("eps"), label = "eps (Eggs per Female Spawner)", value = life_stages$Value[which(life_stages$Name == "eps")]),
      ),
      column(
        width = 4,
        numericInput(ns("eps_sd"), label = "eps_sd (Variance in Eggs per Female)", value = life_stages$Value[which(life_stages$Name == "eps_sd")]),
      ),
      
    ),
    
    tags$p("The correlation in egg fecundity through time determines the similarity in residual egg production across age classes through years. (TODO add description for spawning interval).", class = "pm-ht"),
    
    fluidRow(
      column(
        width = 6,
        numericInput(ns("egg_rho"), label = "egg_rho (Correlation in egg fecundity through time)", value = life_stages$Value[which(life_stages$Name == "egg_rho")]),
      ),
      column(
        width = 6,
        numericInput(ns("int"), label = "int (Spawning Interval - years)", value = life_stages$Value[which(life_stages$Name == "int")])
      )
    ),
    
    tags$p("The probability (portion) that an individual is sexually mature in each stage class.", class = "pm-ht"),
    
    fluidRow(
      column(
        width = 3,
        numericInput(ns("mat_1"), label = "mat_1 (Maturity as Stage 1)", value = life_stages$Value[which(life_stages$Name == "mat_1")]),
      ),
      column(
        width = 3,
        numericInput(ns("mat_2"), label = "mat_2 (Maturity as Stage 2)", value = life_stages$Value[which(life_stages$Name == "mat_2")]),
      ),
      column(
        width = 3,
        numericInput(ns("mat_3"), label = "mat_3 (Maturity as Stage 3)", value = life_stages$Value[which(life_stages$Name == "mat_3")]),
      ),
      column(
        width = 3,
        numericInput(ns("mat_4"), label = "mat_4 (Maturity as Stage 4)", value = life_stages$Value[which(life_stages$Name == "mat_4")]),
      )
    ),
    
    tags$hr(),
    
    
    tags$h4("Density Dependence"),
    
    tags$p("The adult carrying capacity will determine the mean number of adults during the simulation. The abudnacne of adults in the population will generally hover around this value with stochasticity after the population stabilizes. The abundance estimates for other life stages are essentially back-calculated in each year of the simulation such that the adult carrying capacity is achieved with a global lambda value equal to 1.", class = "pm-ht"),
    
    
    fluidRow(column(
      width = 12,
      numericInput(ns("k"), label = "(K) Stage 4 - Adult Carrying Capacity", value = life_stages$Value[which(life_stages$Name == "k")]),
      
    )),
    
    
    tags$h4("Compensation Ratios"),
    
    tags$p("Compensation ratios for density dependent growth. Density dependence is introduced into the model as compensatory density dependence with a fixed adult carrying capacity and the assumption of population stability over a longterm horizon.", class = "pm-ht"),
    
    
    fluidRow(
      column(
        width = 4,
        numericInput(ns("cr_E"), label = "cr_E (Egg Survival CR)", value = life_stages$Value[which(life_stages$Name == "cr_E")]),
      ),
      column(
        width = 4,
        numericInput(ns("cr_0"), label = "cr_0 (Stage 0 Survival CR)", value = life_stages$Value[which(life_stages$Name == "cr_0")]),
      ),
      column(
        width = 4,
        numericInput(ns("cr_1"), label = "cr_1 (Stage 1 Survival CR)", value = life_stages$Value[which(life_stages$Name == "cr_1")]),
      ),
      
    ),
    
    
    
    fluidRow(
      column(
        width = 4,
        numericInput(ns("cr_2"), label = "cr_2 (Stage 2 Survival CR)", value = life_stages$Value[which(life_stages$Name == "cr_2")]),
      ),
      column(
        width = 4,
        numericInput(ns("cr_3"), label = "cr_3 (Stage 3 Survival CR)", value = life_stages$Value[which(life_stages$Name == "cr_3")]),
      ),
      column(
        width = 4,
        numericInput(ns("cr_4"), label = "cr_4 (Stage 4 Survival CR)", value = life_stages$Value[which(life_stages$Name == "cr_4")]),
      ),
      
    ),
    
    
    tags$hr(),
    
    
    tags$h4("Other Parameters"),
    
    fluidRow(column(
      width = 6,
      numericInput(ns("SR"), label = " SR - Sex ratio (portion female at birth)", value = life_stages$Value[which(life_stages$Name == "SR")]),
    ))
    
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
                 
                 
                 # Add form validators (front-end only)
                 btwn_01 <- function(value) {
                   if(is.null(value)) {
                     return("Cannot be blank")
                   }
                   if(is.na(value)) {
                     return("Cannot be blank")
                   }
                   if(class(value) != "numeric" & class(value) != "integer") {
                     return("Must be a number")
                   }
                   if (value > 1) {
                     return("Value must be between 0 and 1")
                   }
                   if (value < 0) {
                     return("Value must be between 0 and 1")
                   }
                 }
                 simp_num <- function(value) {
                   if(is.null(value)) {
                     return("Cannot be blank")
                   }
                   if(is.na(value)) {
                     return("Cannot be blank")
                   }
                   if(class(value) != "numeric" & class(value) != "integer") {
                     return("Must be a number")
                   }
                 }
                 gtr_0 <- function(value) {
                   if(is.null(value)) {
                     return("Cannot be blank")
                   }
                   if(is.na(value)) {
                     return("Cannot be blank")
                   }
                   if(class(value) != "numeric" & class(value) != "integer") {
                     return("Must be a number")
                   }
                   if (value <= 0) {
                     return("Value must be between greater than 0")
                   }
                 }
                 
                 
                 
                 iv <- InputValidator$new()
                 iv$add_rule("k", simp_num)
                 iv$add_rule("events", simp_num)
                 iv$add_rule("eps", simp_num)
                 iv$add_rule("int", simp_num)
                 iv$add_rule("SE", btwn_01)
                 iv$add_rule("S0", btwn_01)
                 iv$add_rule("SR", btwn_01)
                 iv$add_rule("surv_1", btwn_01)
                 iv$add_rule("surv_2", btwn_01)
                 iv$add_rule("surv_3", btwn_01)
                 iv$add_rule("surv_4", btwn_01)
                 iv$add_rule("year_1", gtr_0)
                 iv$add_rule("year_2", gtr_0)
                 iv$add_rule("year_3", gtr_0)
                 iv$add_rule("year_4", gtr_0)
                 iv$add_rule("cr_E", simp_num)
                 iv$add_rule("cr_0", simp_num)
                 iv$add_rule("cr_1", simp_num)
                 iv$add_rule("cr_2", simp_num)
                 iv$add_rule("cr_3", simp_num)
                 iv$add_rule("cr_4", simp_num)
                 iv$add_rule("mat_1", btwn_01)
                 iv$add_rule("mat_2", btwn_01)
                 iv$add_rule("mat_3", btwn_01)
                 iv$add_rule("mat_4", btwn_01)
                 iv$add_rule("eps_sd", simp_num)
                 iv$add_rule("egg_rho", simp_num)
                 iv$add_rule("M.cv", simp_num)
                 iv$add_rule("M.rho", simp_num)
                 iv$enable()
                 

                 
                 # Listen for any changes to matrix model input parameters
                 #  and on change update the reactive values object
                 # session$userData$rv_life_stages$dat
                 observe({
                   # Do not run if any input is null (update while typing...)
                   req(input$k >= 0)
                   req(input$events >= 0)
                   req(input$eps >= 0)
                   req(input$int > 0)
                   req(input$SE >= 0 & input$SE <= 1)
                   req(input$S0 >= 0 & input$S0 <= 1)
                   req(input$SR >= 0 & input$SR <= 1)
                   req(input$surv_1 >= 0 & input$surv_1 <= 1)
                   req(input$surv_2 >= 0 & input$surv_2 <= 1)
                   req(input$surv_3 >= 0 & input$surv_3 <= 1)
                   req(input$surv_4 >= 0 & input$surv_4 <= 1)
                   req(input$year_1 > 0)
                   req(input$year_2 > 0)
                   req(input$year_3 > 0)
                   req(input$year_4 > 0)
                   req(input$cr_E >= 0)
                   req(input$cr_0 >= 0)
                   req(input$cr_1 >= 0)
                   req(input$cr_2 >= 0)
                   req(input$cr_3 >= 0)
                   req(input$cr_4 >= 0)
                   req(input$mat_1 >= 0 & input$mat_1 <= 1)
                   req(input$mat_2 >= 0 & input$mat_2 <= 1)
                   req(input$mat_3 >= 0 & input$mat_3 <= 1)
                   req(input$mat_4 >= 0 & input$mat_4 <= 1)
                   req(input$eps_sd >= 0)
                   req(input$egg_rho >= 0)
                   req(input$M.cv >= 0)
                   req(input$M.rho >= 0)
                   req(session$userData$rv_life_stages$dat)
                   
                   print("updating pop. model inputs...")
                   
                   isolate({
                     session$userData$rv_life_stages$dat$Value[which(session$userData$rv_life_stages$dat$Name == "k")] <-
                       input$k
                     session$userData$rv_life_stages$dat$Value[which(session$userData$rv_life_stages$dat$Name == "events")] <-
                       input$events
                     session$userData$rv_life_stages$dat$Value[which(session$userData$rv_life_stages$dat$Name == "eps")] <-
                       input$eps
                     session$userData$rv_life_stages$dat$Value[which(session$userData$rv_life_stages$dat$Name == "int")] <-
                       input$int
                     session$userData$rv_life_stages$dat$Value[which(session$userData$rv_life_stages$dat$Name == "SE")] <-
                       input$SE
                     session$userData$rv_life_stages$dat$Value[which(session$userData$rv_life_stages$dat$Name == "S0")] <-
                       input$S0
                     session$userData$rv_life_stages$dat$Value[which(session$userData$rv_life_stages$dat$Name == "SR")] <-
                       input$SR
                     session$userData$rv_life_stages$dat$Value[which(session$userData$rv_life_stages$dat$Name == "surv_1")] <-
                       input$surv_1
                     session$userData$rv_life_stages$dat$Value[which(session$userData$rv_life_stages$dat$Name == "surv_2")] <-
                       input$surv_2
                     session$userData$rv_life_stages$dat$Value[which(session$userData$rv_life_stages$dat$Name == "surv_3")] <-
                       input$surv_3
                     session$userData$rv_life_stages$dat$Value[which(session$userData$rv_life_stages$dat$Name == "surv_4")] <-
                       input$surv_4
                     session$userData$rv_life_stages$dat$Value[which(session$userData$rv_life_stages$dat$Name == "year_1")] <-
                       input$year_1
                     session$userData$rv_life_stages$dat$Value[which(session$userData$rv_life_stages$dat$Name == "year_2")] <-
                       input$year_2
                     session$userData$rv_life_stages$dat$Value[which(session$userData$rv_life_stages$dat$Name == "year_3")] <-
                       input$year_3
                     session$userData$rv_life_stages$dat$Value[which(session$userData$rv_life_stages$dat$Name == "year_4")] <-
                       input$year_4
                     session$userData$rv_life_stages$dat$Value[which(session$userData$rv_life_stages$dat$Name == "cr_E")] <-
                       input$cr_E
                     session$userData$rv_life_stages$dat$Value[which(session$userData$rv_life_stages$dat$Name == "cr_0")] <-
                       input$cr_0
                     session$userData$rv_life_stages$dat$Value[which(session$userData$rv_life_stages$dat$Name == "cr_1")] <-
                       input$cr_1
                     session$userData$rv_life_stages$dat$Value[which(session$userData$rv_life_stages$dat$Name == "cr_2")] <-
                       input$cr_2
                     session$userData$rv_life_stages$dat$Value[which(session$userData$rv_life_stages$dat$Name == "cr_3")] <-
                       input$cr_3
                     session$userData$rv_life_stages$dat$Value[which(session$userData$rv_life_stages$dat$Name == "cr_4")] <-
                       input$cr_4
                     session$userData$rv_life_stages$dat$Value[which(session$userData$rv_life_stages$dat$Name == "mat_1")] <-
                       input$mat_1
                     session$userData$rv_life_stages$dat$Value[which(session$userData$rv_life_stages$dat$Name == "mat_2")] <-
                       input$mat_2
                     session$userData$rv_life_stages$dat$Value[which(session$userData$rv_life_stages$dat$Name == "mat_3")] <-
                       input$mat_3
                     session$userData$rv_life_stages$dat$Value[which(session$userData$rv_life_stages$dat$Name == "mat_4")] <-
                       input$mat_4
                     session$userData$rv_life_stages$dat$Value[which(session$userData$rv_life_stages$dat$Name == "eps_sd")] <-
                       input$eps_sd
                     session$userData$rv_life_stages$dat$Value[which(session$userData$rv_life_stages$dat$Name == "egg_rho")] <-
                       input$egg_rho
                     session$userData$rv_life_stages$dat$Value[which(session$userData$rv_life_stages$dat$Name == "M.cv")] <-
                       input$M.cv
                     session$userData$rv_life_stages$dat$Value[which(session$userData$rv_life_stages$dat$Name == "M.rho")] <-
                       input$M.rho
                   })
                   
                 })
                 
                 
                 

                 
                 
               })
}