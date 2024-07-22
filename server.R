
# Define server logic ----
server <- function(input, output, session) {
  
  ################### This code change the Tab Head Layout
  Sys.sleep(1.5)
  observeEvent(input$datasetNav,
               {
                 filterStates$allDataset <- input$allNav
                 filterStates$dataNavi$dataset <- input$datasetNav
               })
  
  ######### Set the first active page Layout
  callModule(mainContentRouter_server, id = "mainContentRouter", filterStates = filterStates)
  
  ########## Call filter server module
  callModule(filterStatesRouter_server, id = "filterStates", filterStates = filterStates)
  
  ####### Call server module for start tour.
  callModule(headerWalkthrough_server, id = "walkthrough", filterStates = filterStates)
  
  ############## Icon selection for display modal form
  observeEvent(input$iconSelection, {
    callModule(headerFormModal_server, id = "formModal", iconSelection = input$iconSelection)
    callModule(headerFeedbackModal_server, id = "feedbackModal", iconSelection = input$iconSelection)
  })

  ################ Apply filter woth sidebar DATA
  observeEvent(input$filter_data, {
    print("Apply the filter")
    filterStates$citySelected <- input$cityInput
    filterStates$statusSelected <- input$statusInput
    filterStates$date_start <- input$dateRangeInput[1]
    filterStates$date_end <- input$dateRangeInput[2]
    filterStates$filterButton <- TRUE
  })
  
  ################ Reset filter on sidebar DATA
  observeEvent(input$reset_filter, {
    print("Reset the filter")
    #filterStates$countrySelected <- NULL
    filterStates$citySelected <- "TOUT"
    filterStates$statusSelected <- "TOUT"
    filterStates$date_start <- "2024-01-01"
    c <- Sys.Date()
    filterStates$filterButton <- FALSE
  })
  
  # observeEvent(filterStates$citySelected,{
  #   if(filterStates$citySelected == "TOUT"){
  #     choice = c("tout")
  #   } else if(filterStates$citySelected == "EDEA"){
  #     choice = unique(data_unique$carimo_product[data_unique$ville == filterStates$citySelected])
  #   } else{
  #     choice = c("tout",as.character(unique(data_unique$carimo_product)))
  #   }
  #   updateSelectInput(session, "statusInput", choices = choice)
  # })
  
  observeEvent(input$cityInput,{
    if(input$cityInput == "TOUT"){
      choice = c("TOUT",as.character(unique(data_unique$carimo_product)))
    } else if(input$cityInput == "EDEA"){
      choice = unique(data_unique$carimo_product[data_unique$ville == input$cityInput])
    } else if (input$cityInput == "BAFOUSSAM"){
      choice = unique(data_unique$carimo_product[data_unique$ville == input$cityInput])
    }else{
      choice = c("TOUT",as.character(unique(data_unique$carimo_product)))
    }
    updateSelectInput(session, "statusInput", choices = choice)
  })
  
  filtered_data <- shiny::eventReactive(input$filter_data,{
    req(filterStates$date_start,filterStates$date_end, filterStates$citySelected,filterStates$statusSelected)
    if (filterStates$citySelected=="TOUT" & filterStates$statusSelected=="TOUT"){
      data <- data_unique %>%
        filter(formatted_date>=filterStates$date_start & formatted_date<=filterStates$date_end)
    } else if (filterStates$citySelected=="TOUT" & filterStates$statusSelected!="TOUT"){
      data <- data_unique %>%
        filter(formatted_date>=filterStates$date_start & formatted_date<=filterStates$date_end) %>%
        filter(carimo_product==filterStates$statusSelected)
    } else if (filterStates$citySelected!="TOUT" & filterStates$statusSelected=="TOUT"){
      data <- data_unique %>%
        filter(formatted_date>=filterStates$date_start & formatted_date<=filterStates$date_end) %>%
        filter(ville==filterStates$citySelected)
    } else {
      data <- data_unique %>%
        filter(formatted_date>=filterStates$date_start & formatted_date<=filterStates$date_end) %>%
        filter(ville==filterStates$citySelected) %>%
        filter(carimo_product==filterStates$statusSelected)
    }
    return(data)
  }, ignoreNULL = FALSE)
  
  filtered_data_unique <- shiny::eventReactive(input$filter_data,{
    req(filterStates$date_start,filterStates$date_end, filterStates$citySelected,filterStates$statusSelected)
    if (filterStates$citySelected=="TOUT" & filterStates$statusSelected=="TOUT"){
      data <- data %>%
        filter(formatted_date>=filterStates$date_start & formatted_date<=filterStates$date_end)
    } else if (filterStates$citySelected=="TOUT" & filterStates$statusSelected!="TOUT"){
      data <- data %>%
        filter(formatted_date>=filterStates$date_start & formatted_date<=filterStates$date_end) %>%
        filter(carimo_product==filterStates$statusSelected)
    } else if (filterStates$citySelected!="TOUT" & filterStates$statusSelected=="TOUT"){
      data <- data %>%
        filter(formatted_date>=filterStates$date_start & formatted_date<=filterStates$date_end) %>%
        filter(ville==filterStates$citySelected)
    } else {
      data <- data %>%
        filter(formatted_date>=filterStates$date_start & formatted_date<=filterStates$date_end) %>%
        filter(ville==filterStates$citySelected) %>%
        filter(carimo_product==filterStates$statusSelected)
    }
    return(data)
  }, ignoreNULL = FALSE)
  
  data_geo <- shiny::eventReactive(input$filter_data,{
    req(filterStates$date_start,filterStates$date_end, filterStates$citySelected,filterStates$statusSelected)
    if (filterStates$citySelected=="TOUT" & filterStates$statusSelected=="TOUT"){
      geo <- geo %>%
        filter(formatted_date>=filterStates$date_start & formatted_date<=filterStates$date_end)
    } else if (filterStates$citySelected=="TOUT" & filterStates$statusSelected!="TOUT"){
      geo <- geo %>%
        filter(formatted_date>=filterStates$date_start & formatted_date<=filterStates$date_end) %>%
        filter(carimo_product==filterStates$statusSelected)
    } else if (filterStates$citySelected!="TOUT" & filterStates$statusSelected=="TOUT"){
      geo <- geo %>%
        filter(formatted_date>=filterStates$date_start & formatted_date<=filterStates$date_end) %>%
        filter(ville==filterStates$citySelected)
    } else {
      geo <- geo %>%
        filter(formatted_date>=filterStates$date_start & formatted_date<=filterStates$date_end) %>%
        filter(ville==filterStates$citySelected) %>%
        filter(carimo_product==filterStates$statusSelected)
    }
    return(geo)
  }, ignoreNULL = FALSE)
  

  localisation <- reactiveVal(NULL)
  observe({
    df <- data_geo() %>% group_by(quartier,longitude,latitude) %>%
      summarise(count=n())
    localisation(df)
    saveRDS(localisation(), paste("data/","localisation",".rds", sep = ""))
    
  })
  
  nb_customers <-reactiveVal(NULL)
  observe({
    nb_customers(nrow(as.data.frame(unique(filtered_data()$phone_number))))
    saveRDS(nb_customers(), paste("data/","nb_customers",".rds", sep = ""))
  })
  amount_sales <- reactiveVal(NULL)
  observe({
    amount_sales(sum(filtered_data()$montant, na.rm = TRUE))
    saveRDS(amount_sales(), paste("data/","amount_sales",".rds", sep = ""))
  })
  winners <- reactiveVal(NULL)
  observe({
    data <- filtered_data() %>% filter(isWinner!="lose")
    nrow(as.data.frame(unique(data$`_id`)))
    winners(nrow(as.data.frame(unique(data$`_id`))))
    saveRDS(winners(), paste("data/","winners",".rds", sep = ""))
  })
  
  age_sexe <- reactiveVal(NULL)
  observe({
    df <- as.data.frame(filtered_data_unique() %>% 
                                filter(!is.na(Age)) %>%
                                group_by(Age, gender) %>%
                                summarise(count=n())) %>%
      dplyr::mutate(percentage = round(100*(count/sum(count)),2),
                    pct1 = paste0(percentage, "%"))
    age_sexe(df)
    saveRDS(age_sexe(), paste("data/","age_sexe",".rds", sep = ""))
    
  })
  
  quartiers <- reactiveVal(NULL)
  observe({
    df <- as.data.frame(table(filtered_data_unique()$quartier))
    df <- df %>% arrange(desc(Freq)) %>%
      dplyr::mutate(percentage = round(100*(Freq/sum(Freq)),2),
                    pct1 = paste0(percentage, "%")) %>%
      filter(Var1!="") %>%
      head(10)
    quartiers(df)
    saveRDS(quartiers(), paste("data/","quartiers",".rds", sep = ""))
  })

  data_time_series <- reactiveVal(NULL)
  observe({
    df <- filtered_data() %>% group_by(formatted_date) %>%
      summarise(montant=sum(montant,na.rm = TRUE)) %>%
      rename(date=formatted_date)
    data_time_series(df)
    saveRDS(data_time_series(), paste("data/","data_time_series",".rds", sep = ""))
  })
  
  popular_products <- reactiveVal(NULL)
  observe({
    df <- as.data.frame(table(filtered_data()$produits))
    df <- df %>%
      group_by(Var1) %>%
      summarize(Freq = sum(Freq)) %>%
      arrange(desc(Freq)) %>%
      head(5)
    df$Var1 <- as.character(df$Var1)
    popular_products(df)
    saveRDS(popular_products(), paste("data/","popular_products",".rds", sep = ""))
  })
  
  sales_pro <- reactiveVal(NULL)
  observe({
    df <- filtered_data() %>%
      group_by(produits,montant) %>%
      summarise(montant=sum(montant,na.rm = TRUE)) %>%
      arrange(desc(montant)) %>%
      filter(montant!=0)
    sales_pro(df)
    saveRDS(sales_pro(), paste("data/","sales_pro",".rds", sep = ""))
  })
  
  quarters_CA <- reactiveVal(NULL)
  observe({
    df1 <- filtered_data() %>%
      group_by(quartier) %>% #,montant
      summarise(montant=sum(montant,na.rm = TRUE)) %>%
      arrange(desc(montant)) %>%
      filter(montant!=0) %>%
      head(10) %>%
      arrange(desc(montant))
    df1$quartier <- factor(df1$quartier, levels = unique(df1$quartier)[order(df1$montant, decreasing = FALSE)])
    quarters_CA(df1)
    saveRDS(quarters_CA(), paste("data/","quarters_CA",".rds", sep = ""))
  })
  
  
  

}
##
