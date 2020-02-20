pacman::p_load(devtools, shiny, shiny.semantic, semantic.dashboard, tidyverse, DT,
               RSQLite, dbplyr, R6, shinyjs, shinytoastr, shinyuser, pairwiseR)
# devtools::document()
# devtools::install()
# devtools::install_github("systats/shinyuser")
# unlink("/Library/Frameworks/R.framework/Versions/3.6/Resources/library/00LOCK-pairwiseR", recursive = TRUE)

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
# 4x3 footer decisions with popups
# A | B
# ignore A| ignore B
# igno re | back

# library(shinyuser)
# options(shiny.maxRequestSize=200*1024^2) 

# pairwiseR::init_db("root", "data/mp.db")

### Needed for user db initialization
ui <- shiny.semantic::semanticPage(
  shiny::tags$head(
    shiny::tags$link(rel="stylesheet", href="styles/main.css")
  ),
  dashboardHeader(
    inverted = T, 
    manager_ui("manager"),
    shinyjs::useShinyjs()
  ),
  shinyjs::useShinyjs(),
  div(class = "ui text container",
    br(),
    vignette_ui("action"),
    br(),
    div(class="ui green progress", id = "global",
        div(class="bar",
            div(class="progress")
        )
    )
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
    
    ### Progress bar by user and party
    observe({
      req(user())
      action()
      
      total <- 300
        
      res <- dplyr::src_sqlite("data/mp.db") %>%
        dplyr::tbl("com") %>%
        as_tibble() %>%
        dplyr::filter(party == input$party) %>%
        dplyr::filter(user == user()$username) %>%
        nrow()
      
      times <- res %/% total
      res <- res - times*total
      
      shinyjs::runjs(glue::glue("$('#global').progress({ value: <res>, total: <total>});", .open = "<", .close = ">"))
    })
    
    ### Vignette
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
                                     pair_mp = pair_mp, party = input$party) %>% glimpse

        return(d)
    })

    action <- callModule(vignette_server, "action", pair)

    observe({
        req(action())
        req(user())
        # print(glimpse(user()))
        message(user()$user, " > ", action(), " > ", pageid = pair()$pageid_1, " ", pageid = pair()$pageid_2)
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


