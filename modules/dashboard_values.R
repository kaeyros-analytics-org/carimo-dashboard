#' eval(parse('./modules/preprocessing.R', encoding="UTF-8"))
#' 
#' 
#' #number of customers
#' #nb_customers <- nrow(as.data.frame(unique(data_unique$`_id`)))
#' 
#' #sales stats
#' # amount_sales <- sum(data_unique$montant, na.rm = TRUE)
#' 
#' #total winners
#' win <- nrow(data %>% filter(isWinner!="lose"))
#' 
#' #customer age repartition
#' # Fonction pour convertir les valeurs numériques en dates et les catégoriser
#' # convert_and_categorize <- function(x) {
#' #   if (grepl("^\\d+$", x)) {
#' #     date <- as.Date("1900-01-01") + as.numeric(x) - 2
#' #     age <- as.numeric(format(Sys.Date(), "%Y")) - as.numeric(format(date, "%Y"))
#' #     if (age < 18){
#' #       return("18-25")
#' #     }else if (age >= 18 && age <= 25) {
#' #       return("18-25")
#' #     } else if (age >= 26 && age <= 35) {
#' #       return("26-35")
#' #     } else if (age >= 36 && age <= 55) {
#' #       return("36-55")
#' #     } else {
#' #       return("56+")
#' #     }
#' #   } else {
#' #     return(x)
#' #   }
#' # }
#' 
#' # # Appliquer la fonction à la colonne 'value'
#' # data_modif <- data %>%
#' #   mutate(Age = sapply(age, convert_and_categorize))
#' # age_sexe <- as.data.frame(data_modif %>% 
#' #                             filter(!is.na(Age)) %>%
#' #                             group_by(Age, gender) %>%
#' #                             summarise(count=n()))
#' # 
#' # yiord_palette <- c("#ECC440", "#EE82EE")
#' #' plotly::plot_ly(age_sexe, x = ~Age, y = ~count, color = ~gender, type = "bar", colors = yiord_palette,
#' #'                 text = ~paste(paste("Tranche d'age",":"), Age, "<br>Nombre de personnes: ", count, "<br>",paste("Sexe",":"), gender)) %>%
#' #'   layout(#title = "Frequency of Different Forms of Discrimination by Age Group",
#' #'     xaxis = list(title = "Age"),
#' #'     yaxis = list(title = "Nombre de personnes",range = c(0,max(age_sexe$count)+2)),
#' #'     barmode = "group")%>%
#' #'   style(hoverinfo = "text") %>%
#' #'   config(displayModeBar = T,displaylogo = FALSE, modeBarButtonsToRemove = list(
#' #'     'sendDataToCloud',
#' #'     #'toImage',
#' #'     'autoScale2d',
#' #'     'zoomIn2d',
#' #'     "zoomOut2d",
#' #'     'toggleSpikelines',
#' #'     'resetScale2d',
#' #'     'lasso2d',
#' #'     'zoom2d',
#' #'     'pan2d',
#' #'     'select2d',#,
#' #'     'hoverClosestCartesian',#,
#' #'     'hoverCompareCartesian'),
#' #'     scrollZoom = T)
#' 
#' 
#' # customers quarters repartition
#' # Liste des villes connues
#' #villes_connues <- c("yaoundé", "Yaoundé", "edea", "Edea")
#' # villes_connues <- c("YAOUNDÉ", "EDEA")
#' # 
#' # 
#' # # Fonction pour extraire la ville
#' # extract_ville <- function(location) {
#' #   match <- str_extract(location, paste(villes_connues, collapse = "|"))
#' #   return(ifelse(is.na(match), "", match))
#' # }
#' # 
#' # # Fonction pour extraire le quartier
#' # extract_quartier <- function(location, ville) {
#' #   if (ville != "") {
#' #     return(str_trim(gsub(ville, "", location)))
#' #   } else {
#' #     return(location)
#' #   }
#' # }
#' # 
#' # # Utiliser mutate pour séparer les valeurs
#' # data$location_patenaire <- stringr::str_to_upper(data$location_patenaire)
#' # data_quarters <- data %>%
#' #   mutate(
#' #     ville = sapply(location_patenaire, extract_ville),
#' #     quartier = mapply(extract_quartier, location_patenaire, sapply(location_patenaire, extract_ville))
#' #   )
#' # quartiers <- as.data.frame(table(data_quarters$quartier))
#' # quartiers <- quartiers %>% arrange(desc(Freq)) %>%
#' #   dplyr::mutate(percentage = round(100*(Freq/sum(Freq)),2),
#' #                 pct1 = paste0(percentage, "%")) %>%
#' #   head(10)
#' 
#' #plot pie chart for quarters
#' #' plotly::plot_ly(quartiers, labels= ~Var1,
#' #'                 values= ~Freq, type="pie",
#' #'                 hoverinfo = 'text',
#' #'                 textinfo = 'label+percent',
#' #'                 insidetextfont = list(color = '#FFFFFF'),
#' #'                 text = ~paste(paste("Quartier",":",Var1),
#' #'                               "<br>Number of persons :", Freq,
#' #'                               "<br>Percentage :", pct1),
#' #'                 marker = list(color = c("#ff0000", "#ffa500","#ffff00", "#00ff00",
#' #'                                         "#0000ff",  "#4b0082", "#8f00ff","#FE2E9A","#81BEF7","#B43104","#FA5858","#D0FA58"),
#' #'                               line = list(color = '#FFFFFF', width = 1),showlegend = FALSE)) %>%
#' #'   # marker = list(colors = c("#85C2FF", "#85C2FF","#85C2FF","#85C2FF","#85C2FF"),
#' #'   #               line = list(color = '#FFFFFF', width = 1),showlegend = FALSE)) %>%
#' #'   layout(title="",
#' #'          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
#' #'          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) %>%
#' #'   layout(showlegend = FALSE) %>%
#' #'   config(displayModeBar = T,displaylogo = FALSE, modeBarButtonsToRemove = list(
#' #'     'sendDataToCloud',
#' #'     #'toImage',
#' #'     'autoScale2d',
#' #'     'zoomIn2d',
#' #'     "zoomOut2d",
#' #'     'toggleSpikelines',
#' #'     'resetScale2d',
#' #'     'lasso2d',
#' #'     'zoom2d',
#' #'     'pan2d',
#' #'     'select2d',#,
#' #'     'hoverClosestCartesian',#,
#' #'     'hoverCompareCartesian'),
#' #'     scrollZoom = T)
#' #' 
#' #' #sales report sur le temps
#' #' library(ggplot2)
#' f <- data_unique %>% group_by(formatted_date) %>%
#'   summarise(montant=sum(montant,na.rm = TRUE)) %>%
#'   rename(date=formatted_date)
#' #' f <- as.data.frame(f)
#' #' all_dates <- data.frame(date = seq.Date(from = f[[1]][1], to = f[[1]][nrow(f)], by = "day"))
#' #' 
#' #' # Fusionner avec le DataFrame original en utilisant une jointure externe
#' #' df_full <- all_dates %>%
#' #'   left_join(f, by = "date") %>% 
#' #'   replace_na(list(dbl = 0))
#' #' 
#' #' fig <- plot_ly(df_full, type = 'scatter', mode = 'lines')%>%
#' #'   add_trace(x = ~date, y = ~dbl)%>%
#' #'   layout(showlegend = F)
#' #' fig <- fig %>%
#' #'   layout(
#' #'     xaxis = list(zerolinecolor = '#ffff',
#' #'                  zerolinewidth = 2,
#' #'                  gridcolor = 'ffff'),
#' #'     yaxis = list(zerolinecolor = '#ffff',
#' #'                  zerolinewidth = 2,
#' #'                  range=c(0,max(df_full$dbl)),
#' #'                  gridcolor = 'ffff'),
#' #'     plot_bgcolor='#e5ecf6', width = 900)
#' 
#' products <- as.data.frame(table(data_unique$produits))
#' products %>% arrange(Freq)
#' 
#' # soins du corps,soins du visage,soins des cheveux,hygiène personnelle,produits pour enfants,compléments alimentaires
#' # soin des lèvres,accessoires de bain,autres produits
#' fig <- ggplot(f, aes(x = date, y = montant)) +
#'   geom_line(color = "#0099f9", size = 1) +
#'   geom_point(color = "#0099f9", size = 3) +
#'   scale_y_continuous(labels = label_number(big.mark = ",", decimal.mark = ".", scale_cut = cut_short_scale())) +
#'   theme(panel.background = element_rect(fill = "white"),
#'           plot.background = element_rect(fill = "white", color = "NA"),
#'         axis.line = element_line(color = "black"))
#' ggplotly(fig)
#'   
#' d <- data_unique %>% filter(ville=="YAOUNDÉ") %>% filter(carimo_product=="ancien") 
#' d <- d %>% group_by(formatted_date) %>%
#'   summarise(montant=sum(montant,na.rm = TRUE))
#' 
#' # d <- d %>% group_by(formatted_date) %>%
#' #   summarise(montant=sum(montant,na.rm = TRUE)) #%>%
#'   #rename(date=formatted_date)
#' ggplot(d, aes(x = formatted_date, y = montant)) +
#'   geom_line(color = "#0099f9", size = 1) +
#'   geom_point(color = "#0099f9", size = 3) +
#'   #scale_y_continuous(labels = label_number(big.mark = ",", decimal.mark = ".", scale_cut = cut_short_scale())) +
#'   theme(panel.background = element_rect(fill = "white"),
#'         plot.background = element_rect(fill = "white", color = "NA"),
#'         axis.line = element_line(color = "black"))
#' 
#' #most products
#' products <- as.data.frame(table(data_unique$produits))
#' products <- products %>%
#'   group_by(Var1) %>%
#'   summarize(Freq = sum(Freq)) %>%
#'   arrange(desc(Freq)) %>%
#'   head(5)
#' products$Var1 <- as.character(products$Var1)
#' 
#' 
#' 
#' # df <- as.data.frame(table(filtered_data_unique()$produits))
#' # df <- df %>%
#' #   group_by(Var1) %>%
#' #   summarize(Freq = sum(Freq)) %>%
#' #   arrange(desc(Freq)) %>%
#' #   head(5)
#' 
#' plotly::plot_ly(products, x = ~Var1,
#'                 type = "bar",
#'                 y = ~Freq,
#'                 marker = list(color = c("#1FAD87", "#1FAD87","#1FAD87", "#1FAD87",
#'                                         "#1FAD87")),
#'                 text = paste(products$Freq, sep = ""), textposition = 'outside',
#'                 textfont = list(size = 10), # size is defined here
#'                 hovertext = paste(paste("Produit",":",products$Var1),
#'                                   "<br>Nombre d'achats :", products$Freq
#'                 ), #) %>%
#'                 hoverinfo = 'text') %>%
#'   layout(title = "",
#'          uniformtext=list(minsize=10, mode='show'),
#'          xaxis = list(title = "<b> </b>", 
#'                       tickfont = list(size = 11),
#'                       titlefont = list(size = 16), 
#'                       tickangle= -45, showgrid = FALSE),
#'          yaxis = list(title = "<b> Percentage </b>",
#'                       titlefont = list(size = 12),
#'                       tickfont = list(size = 12)
#'                      )
#'   ) %>%
#'   config(displayModeBar = T,displaylogo = FALSE, modeBarButtonsToRemove = list(
#'     'sendDataToCloud',
#'     #'toImage',
#'     'autoScale2d',
#'     'zoomIn2d',
#'     "zoomOut2d",
#'     'toggleSpikelines',
#'     'resetScale2d',
#'     'lasso2d',
#'     'zoom2d',
#'     'pan2d',
#'     'select2d',#,
#'     'hoverClosestCartesian',#,
#'     'hoverCompareCartesian'),
#'     scrollZoom = T)
