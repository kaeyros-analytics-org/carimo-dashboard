
################ function to manage pivot item by id 
setClickedId <- function(inputId) {
  shiny.fluent::JS(glue::glue("item => Shiny.setInputValue('{inputId}', item.props.id)"))
}

mainContentRouter_ui <- function(id) {
  
  ns <- NS(id)
  fluentPage(
    withSpinner(uiOutput(ns("mainContent")),
                         type = 8,
                         color = 'grey', size = 0.7))
}

mainContentRouter_server <- function(input, output, session, filterStates) {
  
  observeEvent(filterStates$dataNavi$dataset, 
               { print(paste("mon dataset: ", filterStates$dataNavi$dataset))
                 # generate Ressources content ####
                 if(filterStates$dataNavi$dataset == "Map") {
                   output$mainContent <- renderUI({
                     div( id = "navtabs",
                          ui_map
                     )
                   })
                   # generate Réemploies content ####
                 } else if(filterStates$dataNavi$dataset == "Dashboard") {
                   output$mainContent <- renderUI({
                     ui_dashboard
                   })
                   # generate Recouvrement content ####
                 } else { # Home
                   output$mainContent <- renderUI({
                     tagList(
                       includeMarkdown("./www/htmlComponents/home.html"),
                     )
                   })
                 }
                 
               })
  ############# This UI is for map Layout Page
  ui_map = map_ui(session$ns("map"))
  ############# This UI is for dashboard Layout Page
  ui_dashboard = dashboard_ui(session$ns("dashboard"))

  observeEvent(input$ressources_tabs, {
    print("ok")
    cat(" dans ressources Vous avez cliqué sur le tabPanel avec l'ID :", input$ressources_tabs, "\n")
  })
  
  callModule(map_server, id = "map", filterStates)
  callModule(dashboard_server, id = "dashboard", filterStates)
}










