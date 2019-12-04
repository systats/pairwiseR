#' vignette_ui
#' @export
vignette_ui <- function(id){
  ns <- NS(id)  
  
  tagList(
    uiOutput(ns("content")),
    br(),
    br(),
    br(),
    div(class = "ui buttons",style = "display: flex; justify-content: center; ",
  
       actionButton(ns("ignore_a"), label = "", icon = icon("green exclamation circle"), class = "big basic ui button"),
       actionButton(ns("ignore"), label = "", icon = icon("exclamation circle"), class = "big ui button"),    
       actionButton(ns("ab"), label = "AB", class = "big ui button"),
       actionButton(ns("ignore_b"), label = "", icon = icon("red exclamation circle"), class = "big basic ui button")

    )
  )

}

#' vignette_server
#' @export
vignette_server <- function(input, output, session, pair){
  
  output$content <- renderUI({
    req(pair())
    tagList(
      div(class = "ui large header", "Welche der beiden Abgeordneten ist linker?"),
      br(),
      div(class = "ui grid",
          div(class = "ui eight wide column",
              a(class="ui green card", id = session$ns("a"), href = "#", class = "action-button",
                div(class="content",
                    div(class = "ui header", pair()$name_1),
                    div(class = "meta", pair()$party_1)
                )
              )
          ),
          div(class = "ui eight wide column",
              a(class="ui red card", id = session$ns("b"), href = "#", class = "action-button",
                div(class="content",
                    div(class = "ui header", pair()$name_2),
                    div(class = "meta", pair()$party_2)
                )
              )
          )
      )
    )
  })
  
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
    
    c("a", "b", "ab", "ignore_a", "ignore_b", "ignore") %>%
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