#' user_input_ui
#' @export
user_input_ui <- function(id){
  
  ns <- NS(id)
  
  tagList(
    div(class = "ui bottom attached buttons", id = "button_group",
        actionButton(ns("ignore_a"), label = "", icon = icon("green exclamation circle"), class = "big basic ui button"),
        actionButton(ns("a"), label = "A", class = "big green ui button"),
        actionButton(ns("ab"), label = "AB", class = "big ui button"),
        actionButton(ns("b"), label = "B", class = "big red ui button"),
        actionButton(ns("ignore_b"), label = "", icon = icon("red exclamation circle"), class = "big ui button"),
        actionButton(ns("ignore"), label = "", icon = icon("exclamation circle"), class = "big basic ui button"),
        actionButton(ns("undo"), label = "", icon = icon("undo"), class = "big ui button")
    )
  )
  
}

#' user_input_server
#' @export
user_input_server <- function(input, output, session){
  
  log <- reactiveValues(state = "")
  

  observeEvent(input$ignore_a, {
    log$state <- "ignore_a"
  })
  
  observeEvent(input$a, {
    log$state <- "a"
  })
  
  observeEvent(input$ab, {
    log$state <- "ab"
  })
  
  observeEvent(input$b, {
    log$state <- "b"
  })
  
  observeEvent(input$ignore_b, {
    log$state <- "ignore_b"
  })

  observeEvent(input$ignore, {
    log$state <- "ignore"
  })

  observeEvent(input$undo, {
    log$state <- "undo"
  })
  
  observeEvent(log$state, {
    req(log$state)
    
    c("a", "b", "ab", "ignore_a", "ignore_b", "ignore", "undo") %>%
      walk(~{
        shinyjs::disable(.x)
        shinyjs::delay(2000, shinyjs::enable(.x))
      })

    shinyjs::delay(2000, log$state <- "")
  })

  out <- eventReactive(log$state, {
    req(log$state)
    log$state
  })
  
  return(out)
}