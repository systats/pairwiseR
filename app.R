pacman::p_load(devtools, shiny, shiny.semantic, semantic.dashboard, tidyverse, DT,
               RSQLite, dbplyr, R6, shinyjs, shinytoastr, shinyuser, pairwiseR)
# devtools::document()
# devtools::install()
# devtools::install_github("systats/shinyuser")

# new <- shinyuser::user$new("data/users")
# new$username <- "test_1"
# new$password <- "2020"
# new$load("root1")
# new$update()

library(shiny)
library(shiny.semantic)
library(shinyjs)
library(semantic.dashboard)
library(dplyr)
library(stringr)
library(purrr)
library(jsonlite)
library(R6)
library(RSQLite)
# install.packages("V8")
library(V8)

source("R/vignette_mod.R")

ui <- shiny.semantic::semanticPage(
        shiny::tags$head(
            shiny::tags$link(rel="stylesheet", href="styles/main.css")
        ),
        dashboardHeader(
            inverted = T, 
            manager_ui("manager")
        ),
        shinyjs::useShinyjs(),
        div(class = "ui text container",
            vignette_ui("action")
        )
    )

server <- function(input, output, session){
    
    ### User authentification
    user <- callModule(login_server, "user")
    ### User managment
    callModule(manager_server, "manager", user)
    ### Authorized content
    output$authorized <- renderUI({ 
        print(user())
        if(user()$status == 1){
            ui 
        } else { 
            login_ui("user", "", signin = T, recover = F, label_login = "User", label_pw = "Passwort")
        } 
    })
    
    log <- reactiveValues(state = 0)
    logstate <- reactive({ log$state })
    
    pair <- reactive({

        req(user())
        if(user()$status == 0) return(NULL)
        req(input$party)

        logstate()

        # print(glimpse(user()$user))

        con <- pairwiseR::init_db(user = user()$username, path = "data/mp.db")

        pair_mp <- get_pair_matrix(party = input$party)

        d <- pairwiseR::get_new_pair(user = user()$username,
                                     con = con,
                                     pair_mp = pair_mp, party = input$party)
        return(d)
    })

    action <- callModule(vignette_server, "action", pair)

    observe({
        req(action())
        req(user())
        # print(glimpse(user()))
        message(user()$user, " > ", action(), " > ", pageid = pair()$pageid_1, " ", pageid = pair()$pageid_2)
        message("\n\n")
    })

    observeEvent(action(), {

        con <- pairwiseR::init_db(user = user()$username, path = "data/mp.db") #, force = T

        if(str_detect(action(), "ignore")){
            if(str_detect(action(), "a")){
                add_dont_know(user = user()$username, pageid = pair()$pageid_1, name = pair()$name_1, party = pair()$party_1, con = con)
            } else if(str_detect(action(), "b")){
                add_dont_know(user = user()$username, pageid = pair()$pageid_2, name = pair()$name_2, party = pair()$party_2, con = con)
            } else {
                add_dont_know(user = user()$username, pageid = pair()$pageid_1, name = pair()$name_1, party = pair()$party_1, con = con)
                add_dont_know(user = user()$username, pageid = pair()$pageid_2, name = pair()$name_2, party = pair()$party_2, con = con)
            }
        }

        if(str_detect(action(), "^(a|b)b?$")){
            
            if(action() == "a") outcome <- 1
            if(action() == "b") outcome <- -1
            if(action() == "ab") outcome <- 0

            add_comparison(user = user()$username,
                           pageid_1 = pair()$pageid_1,
                           pageid_2 = pair()$pageid_2,
                           name_1 = pair()$name_1,
                           pair()$name_2,
                           more_left = outcome,
                           time = lubridate::now(),
                           party = input$party,
                           con = con
                         )
        }

        log$state <- 1
        log$state <- 0
    })
}

shinyApp(meta_ui(), server)


