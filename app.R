pacman::p_load(devtools, shiny, shiny.semantic, semantic.dashboard, tidyverse, DT,
               RSQLite, dbplyr, R6, shinyjs, shinytoastr, shinyuser, pairwiseR)
# devtools::document()
# devtools::install()
# devtools::install_github("systats/shinyuser")
# devtools::install_github("benjaminguinaudeau/pairwiseR")


# 4x3 footer decisions with popups
# A | B
# ignore A| ignore B
# igno re | back

# library(shinyuser)
# options(shiny.maxRequestSize=200*1024^2) 

### Needed for user db initialization
check_user_db()
# User <root> with password <2019> was created

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
            dropdown("party", choices = c("SPD", "GRUENE", "PDS/LINKE", "CDU/CSU"), value = "SPD"),
            br(),
            div(class = "ui large header", "Welche der beiden Abgeordneten ist linker?"),
            p(uiOutput("content")),
            br(),
            div(id="footer_container",
                div(class="footer", 
                    user_input_ui("action")
                )
            )
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
        
        con <- pairwiseR::init_db(user = "root", path = "data/mp.db")
        #dks <- con %>% tbl("dk") %>% glimpse
        #comps <- con %>% tbl("com") %>% glimpse
        
        print(input$party)
        print(user()$user)
        pair_mp <- get_pair_matrix(party = input$party)
        
        d <- pairwiseR::get_new_pair(user = "root", con = con, pair_mp = pair_mp) %>% glimpse
        
        #con <- pairwiseR::init_db(user = "root", path = "data/mp.db")
        #pair_mp <- get_pair_matrix(party = "SPD")
        return(d)
    })
    
    # present from db
    output$content <- renderUI({
        req(pair())
        div(class = "ui grid",
            div(class = "ui eight wide column",
                pair()$name_1
            ),
            div(class = "ui eight wide column",
                pair()$name_2
            )
        )
        
    })
    
    action <- callModule(user_input_server, "action")
    
    observe({
        req(action())
        req(user())
        # print(glimpse(user()))
        message(user()$user, " > ", action(), " > ", pageid = pair()$pageid_1, " ", pageid = pair()$pageid_2)
    })
    
    observeEvent(action(), {
        con <- pairwiseR::init_db(user = "root", path = "data/mp.db") #, force = T
        
        if(str_detect(action(), "ignore")){
            if(str_detect(action(), "a")){
                add_dont_know(user = user()$user, pageid = pair()$pageid_1, name = pair()$name_1, party = pair()$party_1, con = con)
            } else if(str_detect(action(), "b")){
                add_dont_know(user = user()$user, pageid = pair()$pageid_2, name = pair()$name_2, party = pair()$party_2, con = con)
            } else {
                add_dont_know(user = user()$user, pageid = pair()$pageid_1, name = pair()$name_1, party = pair()$party_1, con = con)
                add_dont_know(user = user()$user, pageid = pair()$pageid_2, name = pair()$name_2, party = pair()$party_2, con = con)
            }
        }
        
        if(str_detect(action(), "^(a|b)b?$")){
            if(action() == "a") outcome <- 1
            if(action() == "b") outcome <- -1
            if(action() == "ab") outcome <- 0
            
            add_comparison(user = user()$user, 
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