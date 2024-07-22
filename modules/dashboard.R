nb_customers <- shiny::reactiveFileReader(1000, NULL, "data/nb_customers.rds", readRDS)
amount_sales <- shiny::reactiveFileReader(1000, NULL, "data/amount_sales.rds", readRDS)
winners <- shiny::reactiveFileReader(1000, NULL, "data/winners.rds", readRDS)
age_sexe <- shiny::reactiveFileReader(1000, NULL, "data/age_sexe.rds", readRDS)
quartiers <- shiny::reactiveFileReader(1000, NULL, "data/quartiers.rds", readRDS)
data_time_series <- shiny::reactiveFileReader(1000, NULL, "data/data_time_series.rds", readRDS)
popular_products <- shiny::reactiveFileReader(1000, NULL, "data/popular_products.rds", readRDS)
sales_pro <- shiny::reactiveFileReader(1000, NULL, "data/sales_pro.rds", readRDS)
quarters_CA <- shiny::reactiveFileReader(1000, NULL, "data/quarters_CA.rds", readRDS)


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
    div( class="container-fluid",br(),
      # h3(style="margin-left:60px",
      #    "Vue d'ensemble des analyses"),
      div( class="row p-0 m-0",
        div( class = "cards_overview_list",
             catalog_overview_card("Statistiques des clients", "Nombre de clients", textOutput(ns("nb_customers"))),#"35k"
             catalog_overview_card("Statistique des ventes", "Chiffre d'affaire (Fcfa)", textOutput(ns("sales"))),
             catalog_overview_card("Statistique de la tombola", "Nombre de gagnants", textOutput(ns("win")))
        )
      )
    ),
    
    br(), ######### Make Space
    
    ################ Dashboard Layout 1
    div(class="container-fluid",
        div(class="row p-0 m-0", style= "justify-content: center; gap: 80px;", 
            div(class="col-lg-5 pr-1 pl-0", id = "customer_age",
                uiOutput(ns("quarter_sales"))),
            div(class="col-lg-5 pl-1 pr-0", id = "linechart",
                uiOutput(ns("customers_quarter"))))),
    br(),
    
    ################ Dashboard Layout 2
    div(class="container-fluid",
        div(class="row p-0 m-0", style= "justify-content: center; gap: 80px;", 
            div(class="col-lg-5 pr-1 pl-0", id = "customer_age",
                uiOutput(ns("sales_report"))),
            div(class="col-lg-5 pl-1 pr-0", id = "linechart",
                uiOutput(ns("customers_age"))))),
    # div(class="container-fluid",style="width:100%;justify-content: center;padding-left:60px; padding-right:60px",
    #     uiOutput(ns("sales_report"))
    #     ),
    br(),
    ################ Dashboard Layout 3
    div(class="container-fluid",
        div(class="row p-0 m-0", style= "justify-content: center; gap: 80px;", 
            div(class="col-lg-5 pr-1 pl-0", id = "",
                uiOutput(ns("most_popular_product"))),
            div(class="col-lg-5 pl-1 pr-0", id = "",
                uiOutput(ns("table"))
                #""
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
                         headerContent = paste0("Répartitions par âge et sexe des clients", sep = ""),
                         bodyContent = plotlyOutput(session$ns("plot1")),
                         iconId = paste0("_plotchart"),
                         dataset = "dataset")
  })
  
  
  output$customers_quarter <- renderUI({ 
    out <- accordionCard(accordionId = "accordionPlotcustomers_quarter",
                         headerId = 'plotchart2',
                         targetId = 'collapseCcplotchartCustomers_quarter',
                         headerContent = paste0("Répartition des clients par quartier", sep = ""),
                         bodyContent = plotlyOutput(session$ns("plot2")),
                         iconId = paste0("_plotchart"),
                         dataset = "dataset")
  })
  output$quarter_sales <- renderUI({ 
    out <- accordionCard(accordionId = "accordionPlotcustomers_quarter",
                         headerId = 'plotchart2',
                         targetId = 'collapseCcplotchartCustomers_quarter',
                         headerContent = paste0("Chiffre d'affaire par quartier (en CFA)", sep = ""),
                         bodyContent = plotlyOutput(session$ns("quartes_sales_revenue")),
                         iconId = paste0("_plotchart"),
                         dataset = "dataset")
  })
  
  output$sales_report <- renderUI({
    out <- accordionCard(accordionId = "accordionPlotsales_report",
                         headerId = 'plotchart3',
                         targetId = 'collapseCcplotchartSales_report',
                         headerContent = paste0("Rapport des ventes", sep = ""),
                         bodyContent = plotlyOutput(session$ns("sales_report_line_chart")),
                         iconId = paste0("_plotchart"),
                         dataset = "dataset")
  })
  output$most_popular_product <- renderUI({
    out <- accordionCard(accordionId = "accordionPlotmost_popular_product",
                         headerId = 'plotchart4',
                         targetId = 'collapseCcplotchartMost_popular_product',
                         headerContent = paste0("Produits les plus populaires", sep = ""),
                         bodyContent = plotlyOutput(session$ns("most_popular_product_bar")),
                         iconId = paste0("_plotchart"),
                         dataset = "dataset")
  })
  
  output$table <- renderUI({
    out <- accordionCard(accordionId = "accordionPlotmost_popular_product",
                         headerId = 'plotchart4',
                         targetId = 'collapseCcplotchartproducts',
                         headerContent = paste0("Chiffre d'affaire par produit (en CFA)", sep = ""),
                         bodyContent = reactableOutput(session$ns("products"),height = "400px"),
                         iconId = paste0("_plotchart"),
                         dataset = "dataset")
  })
  
  ######################### Output Render
  output$nb_customers <- renderText({
    nb_customers()
  })
  output$sales <- renderText({
    #paste(format(round(as.numeric(amount_sales()), 1), big.mark=","),"FCFA")
    paste0(formatC(amount_sales()/1000, format = "d", big.mark = "."),"K")
    #amount_sales()
  })
  output$win <- renderText({
    winners()
  })
  
  output$plot1 <- renderPlotly({
    yiord_palette <- c("#2D5BFF", "#F1887B")
    plotly::plot_ly(age_sexe(), x = ~Age, y = ~percentage, color = ~gender, type = "bar", colors = yiord_palette #,
                    #text = ~paste(paste("Tranche d'age",":"), Age, "<br>Nombre de personnes: ", count, "<br>",paste("Sexe",":"), gender)
                    ) %>%
      layout(
        xaxis = list(title = "Age"),
        yaxis = list(title = "Nombre de personnes",ticksuffix = "%"),
        barmode = "group")%>%
      #style(hoverinfo = "text") %>%
      config(displayModeBar = T,displaylogo = FALSE, modeBarButtonsToRemove = list(
        'sendDataToCloud',
        'toImage',
        'zoomIn2d',
        "zoomOut2d",
        'toggleSpikelines',
        'resetScale2d',
        'lasso2d',
        'zoom2d',
        'pan2d',
        'select2d',#,
        'hoverClosestCartesian',#,
        'hoverCompareCartesian'),
        zoom2d = F,
        scrollZoom = F)

  })
  
  output$plot2 <- renderPlotly({
    plotly::plot_ly(quartiers(), labels= ~Var1,
                    values= ~Freq, type="pie",
                    hoverinfo = 'text',
                    textinfo = 'label+percent',
                    insidetextfont = list(color = '#FFFFFF'),
                    text = ~paste(paste("Quartier",":",Var1),
                                  "<br>Number of persons :", Freq,
                                  "<br>Percentage :", pct1),
                    marker = list(colors = c("#122352", "#162c66","#1b347a","#1f3d8f", "#2446a3","#284fb8",
                                            "#2d57cc", "#3160e0","#3567f0","#3669f5"),
                                  line = list(color = '#FFFFFF', width = 1),showlegend = FALSE)) %>%
      layout(title="",
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) %>%
      layout(showlegend = FALSE) %>%
      config(displayModeBar = T,displaylogo = FALSE, modeBarButtonsToRemove = list(
        'sendDataToCloud',
        'toImage',
        'autoScale2d',
        'zoomIn2d',
        "zoomOut2d",
        'toggleSpikelines',
        'resetScale2d',
        'lasso2d',
        'zoom2d',
        'pan2d',
        'select2d',#,
        'hoverClosestCartesian',#,
        'hoverCompareCartesian'),
        scrollZoom = F)
  })
  
  output$sales_report_line_chart <- renderPlotly({
    plotly::plot_ly(data = data_time_series(), type = "scatter", mode = "lines") %>%
      add_trace(x = ~date, y = ~montant, name = "Volume", mode = 'lines+markers',
                line = list(color = '#4A3AFF', width = 3.5), marker=list(color = '#4A3AFF', width = 9),
                hovertext = paste("Date :", data_time_series()$date,
                                  "<br>Trans Volume :",paste0(formatC(data_time_series()$montant/1000, format = "d", big.mark = "."),"k CFA")
                ),
                hoverinfo = 'text') %>%
      layout(title = "",
             uniformtext=list(minsize=15, mode='show'),
             xaxis = list(title = "<b> Date </b>",type="date", tickformat="%Y-%m-%d", tickangle= -45,
                          tickvals = data_time_series()$date,
                          tickfont = list(size = 14),
                          titlefont = list(size = 16)),
             yaxis = list(title = "<b> Volume de transaction en CFA </b>", titlefont = list(color = "#4A3AFF", size = 17),
                          
                          tickfont = list(size = 14)),
                          showlegend = FALSE) %>% 
      config(displayModeBar = T,displaylogo = FALSE, modeBarButtonsToRemove = list(
        'sendDataToCloud','toImage','zoomIn2d',"zoomOut2d",'toggleSpikelines',
        'resetScale2d','lasso2d','zoom2d','pan2d','select2d','hoverCompareCartesian'),
        scrollZoom = F)  })
  
  output$most_popular_product_bar <- renderPlotly({

    plotly::plot_ly(popular_products(), x = ~Var1,
                    type = "bar",
                    y = ~Freq,
                    marker = list(color = c("#27BA67", "#27BA67","#27BA67", "#27BA67",
                                            "#27BA67")),
                    text = paste(popular_products()$Freq, sep = ""), textposition = 'outside',
                    textfont = list(size = 10), # size is defined here
                    hovertext = paste(paste("Produit",":",popular_products()$Var1),
                                      "<br>Nombre d'achats :", popular_products()$Freq
                    ), #) %>%
                    hoverinfo = 'text') %>%
      layout(title = "",
             uniformtext=list(minsize=10, mode='show'),
             xaxis = list(title = "<b> </b>", 
                          tickfont = list(size = 11),
                          titlefont = list(size = 16), 
                          tickangle= -45, showgrid = FALSE),
             yaxis = list(title = "<b> Frequence </b>",
                          titlefont = list(size = 12),
                          tickfont = list(size = 12)
             )
      ) %>%
      config(displayModeBar = T,displaylogo = FALSE, modeBarButtonsToRemove = list(
        'sendDataToCloud',
        'toImage',
        'zoomIn2d',
        "zoomOut2d",
        'toggleSpikelines',
        'resetScale2d',
        'lasso2d',
        'zoom2d',
        'pan2d',
        'select2d',#,
        'hoverClosestCartesian',#,
        'hoverCompareCartesian'),
        scrollZoom = F)
  })
  
  output$quartes_sales_revenue <- renderPlotly({
    quarters_CA() %>% plot_ly(
      x = ~montant,
      y = ~quartier,
      type = "bar",
      orientation = "h",
      hoverinfo = 'text',
      hovertext = ~paste(paste("Quartier",":",quartier),
                         "<br>Chiffre d'affaire généré :", paste0(formatC(quarters_CA()$montant, format = "d", big.mark = ".")," CFA")
      )
    ) %>%
      layout(
        title = "",
        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE,title = "<b> Quartiers </b>")
      ) %>%
      config(
        displayModeBar = T,
        displaylogo = FALSE,
        modeBarButtonsToRemove = list(
          'sendDataToCloud','zoomIn2d',"zoomOut2d",'toggleSpikelines','resetScale2d','lasso2d','toImage',
          'zoom2d','pan2d','select2d','hoverClosestCartesian','hoverCompareCartesian'
        ),
        scrollZoom = F
      )
  })
  
  output$products <- renderReactable({
    reactable(sales_pro())
  })
}