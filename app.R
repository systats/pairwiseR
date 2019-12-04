pacman::p_load(devtools, shiny, shiny.semantic, semantic.dashboard, tidyverse, DT,
               RSQLite, dbplyr, R6, shinyjs, shinytoastr, shinyuser, pairwiseR)
# devtools::document()
# devtools::install()
# devtools::install_github("systats/shinyuser")
# devtools::install_github("benjaminguinaudeau/pairwiseR")
# unlink("/Library/Frameworks/R.framework/Versions/3.6/Resources/library/00LOCK-pairwiseR", recursive = TRUE)

source("R/vignette_mod.R")
# 4x3 footer decisions with popups
# A | B
# ignore A| ignore B
# igno re | back

# library(shinyuser)
# options(shiny.maxRequestSize=200*1024^2) 

### Needed for user db initialization
check_user_db()
pairwiseR::add_user_db(user = "ben", password = "1234", signed_in = NA, role = "admin")
pairwiseR::add_user_db(user = "simon", password = "1234", signed_in = NA, role = "admin")
# User <user()$user> with password <2019> was created

ui <- function(){
    
    shiny.semantic::semanticPage(
        shiny::tags$head(
            shiny::tags$link(rel="stylesheet", href="styles/main.css")
        ),
        dashboardHeader(
            inverted = T, 
            tagList(login_ui("login")) 
        ),
        shinyjs::useShinyjs(),
        div(class = "ui text container",
            br(),
            div(class = "ui grid",
                div(class = "five wide column",
                    div(class = "ui header", "Wähle eine Partei")
                ),
                div(class = "five wide column",
                    dropdown("party", choices = c("SPD", "GRUENE", "PDS/LINKE", "CDU/CSU"), value = "SPD")
                ),
                div(class = "six wide column",
                   ""
                )
            ),
            br(),
            vignette_ui("action")
        )
    )
}

server <- function(input, output, session){
    
    log <- reactiveValues(state = 0)
    
    logstate <- reactive({log$state})
    
    # which user? connect to db
    user <- callModule(login_server, "login") 
    
    pair <- reactive({
        
        req(user())
        req(input$party)
        logstate()
        # print(glimpse("user()$user"))
        
        con <- pairwiseR::init_db(user = "user()$user", path = "data/mp.db")

        pair_mp <- get_pair_matrix(party = input$party)
        
        d <- pairwiseR::get_new_pair(user = "user()$user", 
                                     con = con, 
                                     pair_mp = pair_mp, party = input$party) %>% glimpse

        return(d)
    })
    
    action <- callModule(vignette_server, "action", pair)
    
    observe({
        req(action())
        req(user())
        # print(glimpse(user()))
        message("user()$user", " > ", action(), " > ", pageid = pair()$pageid_1, " ", pageid = pair()$pageid_2)
    })
    
    observeEvent(action(), {
        
        con <- pairwiseR::init_db(user = "user()$user", path = "data/mp.db") #, force = T
        
        if(str_detect(action(), "ignore")){
            if(str_detect(action(), "a")){
                add_dont_know(user = "user()$user", pageid = pair()$pageid_1, name = pair()$name_1, party = pair()$party_1, con = con)
            } else if(str_detect(action(), "b")){
                add_dont_know(user = "user()$user", pageid = pair()$pageid_2, name = pair()$name_2, party = pair()$party_2, con = con)
            } else {
                add_dont_know(user = "user()$user", pageid = pair()$pageid_1, name = pair()$name_1, party = pair()$party_1, con = con)
                add_dont_know(user = "user()$user", pageid = pair()$pageid_2, name = pair()$name_2, party = pair()$party_2, con = con)
            }
        }
        
        if(str_detect(action(), "^(a|b)b?$")){
            if(action() == "a") outcome <- 1
            if(action() == "b") outcome <- -1
            if(action() == "ab") outcome <- 0
            
            add_comparison(user = "user()$user", 
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



shinyApp(ui, server)