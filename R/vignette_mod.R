#' vignette_ui
#' @export
vignette_ui <- function(id){
  ns <- NS(id)  
  
  tagList(
    br(),
    span("Welches dieser zwei Mitglieder des Bundestags vertritt die linkere Position?", style = "font-size:20px;"),
    div(class = "sixteen wide column",
        a(class="ui fluid button action-button", id = ns("ab"), href = "#", 
          div(class="content",
              HTML('<center>'),
              div(class = "meta", "Gleiche Position"),
              HTML('</center>')
          )
        )
    ),
    br(),
    br(),
    div(class = "ui two column middle aligned centerd grid",
        div(class = "column",
            a(class="ui grey card action-button", id = ns("a"), href = "#",
              div(class="content",
                  uiOutput(ns("left"))
              ),
              uiOutput(ns("left_image"))

            )
        ),
        div(class = "column",
            a(class="ui grey card action-button", id = ns("b"), href = "#",
              div(class="content",
                  uiOutput(ns("right"))
              ),
              uiOutput(ns("right_image")),
            )
        )
    ),
    br(),
    br(),
    br(),
    div(class = "ui buttons",style = "display: flex; justify-content: center; ",
  
       actionButton(ns("ignore_a"), label = "Unbekannt", class = "big basic grey ui button"),
       #actionButton(ns("ignore"), label = "Beide", class = "big ui button"),    
       actionButton(ns("ignore_b"), label = "Unbekannt", class = "big basic grey ui button")

    ), 
    div(class = "ui buttons",style = "display: flex; justify-content: center; ",
  
       actionButton(ns("remove_last"), label = "Undo", class = "big basic grey ui button"),
    )
    
  )

}






#' vignette_server
#' @export
vignette_server <- function(input, output, session, pair, user){
  
  
  output$left <- renderUI({
    tagList(
      div(class = "ui header", pair()$name_1),
      div(class = "meta", pair()$party_1)
    )
  })
  
  output$left_image <- renderUI({
    img(class = "ui centered image", src = pairwiseR::mp$image_url[pairwiseR::mp$pageid == as.numeric(pair()$pageid_1)])
  })
  
  
  output$right <- renderUI({
    tagList(
      div(class = "ui header", pair()$name_2),
      div(class = "meta", pair()$party_2)
    )
  })
  
  output$right_image <- renderUI({
    img(class = "ui centered image", src = pairwiseR::mp$image_url[pairwiseR::mp$pageid == as.numeric(pair()$pageid_2)])
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
  
  observeEvent(input$remove_last, {
      removed <- con %>%
            remove_last_action(user = user)
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
