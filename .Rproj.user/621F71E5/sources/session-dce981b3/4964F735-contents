############# CARD OVERVIEW
catalog_overview_card <- function(title, text, content) {
  div(class = "overview_card",
      tagList(
        div(class = "overview_card_header",
            Text(class = "overview_card_title", title),
            h2(text)
        ),
        h1(class = "overview_card_content", content)
      )
  )
}

######## UI for dashboard
dashboard_ui <- function(id){
  ns <- NS(id)
  fluentPage(
    tags$style("
               .fieldGroup-82{border: none;}
               "),
    
    ################### Header CArd
    div( class="container-fluid",
      h3("Analytics Overview"),
      div( class = "cards_overview_list",
           catalog_overview_card("Customers Stats", "Total Customers", "35k"),
           catalog_overview_card("Sales Stats", "Turnover", "12k"),
           catalog_overview_card("Tombola Stats", "Total Winners", "58k")
      )
    ),
    
    br(), ######### Make Space
    
    ################ Dashboard Layout 1
    div(class="container-fluid",
        div(class="row p-0 m-0", 
            div(class="col-lg-6 pr-1 pl-0", id = "customer_age",
                uiOutput(ns("customers_age"))),
            div(class="col-lg-6 pl-1 pr-0", id = "linechart",
                uiOutput(ns("customers_quarter"))))),
    
    ################ Dashboard Layout 2
    div(class="container-fluid",
        uiOutput(ns("sales_report"))
        ),
    
    ################ Dashboard Layout 3
    div(class="container-fluid",
        div(class="row p-0 m-0", 
            div(class="col-lg-6 pr-1 pl-0", id = "",
                uiOutput(ns("most_popular_product"))),
            div(class="col-lg-6 pl-1 pr-0", id = "",
                ""
                )
            )
        )
  )
}

########### Server for CPIO
dashboard_server <- function(input, output, session, filterStates){
  
  output$customers_age <- renderUI({
    out <- accordionCard(accordionId = "accordionPlotcustomers_age",
                         headerId = 'plotchart1',
                         targetId = 'collapseCcplotchartCustomers_age',
                         headerContent = paste0("Customers Age Repartitions", sep = ""),
                         bodyContent = plotlyOutput(session$ns("plot1")),
                         iconId = paste0("_plotchart"),
                         dataset = "dataset")
  })
  
  
  output$customers_quarter <- renderUI({
    out <- accordionCard(accordionId = "accordionPlotcustomers_quarter",
                         headerId = 'plotchart2',
                         targetId = 'collapseCcplotchartCustomers_quarter',
                         headerContent = paste0("Customers Quarter Repartitions", sep = ""),
                         bodyContent = plotlyOutput(session$ns("plot2")),
                         iconId = paste0("_plotchart"),
                         dataset = "dataset")
  })
  output$sales_report <- renderUI({
    out <- accordionCard(accordionId = "accordionPlotsales_report",
                         headerId = 'plotchart3',
                         targetId = 'collapseCcplotchartSales_report',
                         headerContent = paste0("Sales reports", sep = ""),
                         bodyContent = plotlyOutput(session$ns("sales_report_line_chart"), width = 600),
                         iconId = paste0("_plotchart"),
                         dataset = "dataset")
  })
  output$most_popular_product <- renderUI({
    out <- accordionCard(accordionId = "accordionPlotmost_popular_product",
                         headerId = 'plotchart4',
                         targetId = 'collapseCcplotchartMost_popular_product',
                         headerContent = paste0("Most Popular Products", sep = ""),
                         bodyContent = plotlyOutput(session$ns("most_popular_product_bar")),
                         iconId = paste0("_plotchart"),
                         dataset = "dataset")
  })
  
  ######################### Output Render
  
  output$plot1 <- renderPlotly({
    
    Animals <- c("giraffes", "orangutans", "monkeys")
    SF_Zoo <- c(20, 14, 23)
    LA_Zoo <- c(12, 18, 29)
    data <- data.frame(Animals, SF_Zoo, LA_Zoo)
    
    fig <- plot_ly(data, x = ~Animals, y = ~SF_Zoo, type = 'bar', name = 'SF Zoo')
    fig <- fig %>% add_trace(y = ~LA_Zoo, name = 'LA Zoo')
    fig <- fig %>% layout(yaxis = list(title = 'Count'), barmode = 'group')
    
    fig
    
  })
  
  output$plot2 <- renderPlotly({
    
    USPersonalExpenditure <- data.frame("Categorie"=rownames(USPersonalExpenditure), USPersonalExpenditure)
    data <- USPersonalExpenditure[,c('Categorie', 'X1960')]
    
    fig <- plot_ly(data, labels = ~Categorie, values = ~X1960, type = 'pie')
    fig <- fig %>% layout(title = 'United States Personal Expenditures by Categories in 1960',
                          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
    fig
  })
  
  output$sales_report_line_chart <- renderPlotly({
    
    x <- c('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December')
    y1 <- c(20, 14, 25, 16, 18, 22, 19, 15, 12, 16, 14, 17)
    y2 <- c(19, 14, 22, 14, 16, 19, 15, 14, 10, 12, 12, 16)
    data <- data.frame(x, y1, y2)
    
    #The default order will be alphabetized unless specified as below:
    data$x <- factor(data$x, levels = data[["x"]])
    
    fig <- plot_ly(data, x = ~x, y = ~y1, type = 'bar', name = 'Primary Product', marker = list(color = 'rgb(49,130,189)'))
    fig <- fig %>% add_trace(y = ~y2, name = 'Secondary Product', marker = list(color = 'rgb(204,204,204)'))
    fig <- fig %>% layout(xaxis = list(title = "", tickangle = -45),
                          yaxis = list(title = ""),
                          margin = list(b = 100),
                          barmode = 'group')
    
    fig
  })
  
  output$most_popular_product_bar <- renderPlotly({
    
    x <- c('Feature A', 'Feature B', 'Feature C', 'Feature D', 'Feature E')
    y <- c(20, 14, 23, 25, 22)
    data <- data.frame(x, y)
    
    fig <- plot_ly(data, x = ~x, y = ~y, type = 'bar', color = I("green"))
    fig <- fig %>% layout(title = "Features",
                          xaxis = list(title = ""),
                          yaxis = list(title = ""))
    
    fig
  })
}