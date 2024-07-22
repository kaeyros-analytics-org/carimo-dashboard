library(readxl)
library(dplyr)
library(tidyr)
library(stringdist)
library(fuzzyjoin)
library(stringr)
library(plotly)

# Fonction pour convertir les valeurs numériques en dates et les catégoriser
convert_and_categorize <- function(x) {
  if (grepl("^\\d+$", x)) {
    date <- as.Date("1900-01-01") + as.numeric(x) - 2
    age <- as.numeric(format(Sys.Date(), "%Y")) - as.numeric(format(date, "%Y"))
    if (age < 18){
      return("18-25")
    }else if (age >= 18 && age <= 25) {
      return("18-25")
    } else if (age >= 26 && age <= 35) {
      return("26-35")
    } else if (age >= 36 && age <= 55) {
      return("36-55")
    } else {
      return("56+")
    }
  } else {
    return(x)
  }
}

villes_connues <- c("YAOUNDÉ", "EDEA")


# Fonction pour extraire la ville
extract_ville <- function(location) {
  match <- str_extract(location, paste(villes_connues, collapse = "|"))
  return(ifelse(is.na(match), "", match))
}

# Fonction pour extraire le quartier
extract_quartier <- function(location, ville) {
  if (ville != "") {
    return(str_trim(gsub(ville, "", location)))
  } else {
    return(location)
  }
}

# Spécifiez le chemin du dossier contenant les fichiers Excel
path <- getwd()
folder_path <- paste(path,"/data",sep = "")
#data <- read.csv(paste(folder_path,"/users.csv",sep = ""))
data <- read_excel((paste(folder_path,"/users.xls",sep = "")))
data$achat_category <- str_to_lower(data$achat_category)

path_product <- paste(folder_path,"/carimo_products_1.xlsx", sep = "")
products <- read_excel(path_product,sheet = "products")

data <- data %>%
  filter(!str_detect(name, "Marc Arthur FOUDA") & !str_detect(name, "DOM") & !str_detect(name, "dom2")
         & !str_detect(name, "Kaeyros") & !str_detect(achat_category, "Test") & !str_detect(achat_category, "test")) %>%
  filter(!(name == "Eyenga pancrace" & is.na(isWinner)))
data <- data %>%
  mutate(across(everything(), ~ gsub("ã©", "é", .))) %>%
  mutate(across(everything(), ~ gsub("ã¨", "è", .))) %>% 
  mutate(across(everything(), ~ gsub("Ã©", "é", .))) %>%
  mutate(across(everything(), ~ gsub("Yaounde", "Yaoundé", .))) 

#function to extract date
extract_and_format_date <- function(datetime_str) {
  parts <- strsplit(datetime_str, " ")[[1]]
  day <- parts[3]
  month <- parts[2]
  year <- parts[4]
  
  months <- c("Jan" = "01", "Feb" = "02", "Mar" = "03", "Apr" = "04", "May" = "05", "Jun" = "06",
              "Jul" = "07", "Aug" = "08", "Sep" = "09", "Oct" = "10", "Nov" = "11", "Dec" = "12")
  
  formatted_date <- paste(year, months[month], day, sep = "-")
  return(formatted_date)
}

# Appliquer la transformation à la colonne du dataframe
data <- data %>%
  mutate(formatted_date = sapply(createdAt, extract_and_format_date))
data$formatted_date <- as.character(data$formatted_date)
data$formatted_date <- as.Date(data$formatted_date)
data <- data %>%
  mutate(Age = sapply(age, convert_and_categorize))

data$location_patenaire <- stringr::str_to_upper(data$location_patenaire)

data <- data %>%
  mutate(
    ville = sapply(location_patenaire, extract_ville),
    quartier = mapply(extract_quartier, location_patenaire, sapply(location_patenaire, extract_ville))
  )
data$ville <-ifelse(data$ville=="", "YAOUNDÉ",data$ville)
data$carimo_product <- ifelse(data$carimo_product=="true","ancien","nouveau")
data$quartier <- gsub(",BILEL AUSSI","ODZA", data$quartier)

  

data_final <- separate_rows(data, achat_category, sep = ",")
  

words_to_replace <- c("huile velours "," huile velours","savon or ","savon or","5 savons métis",
                  "amino","the soir","gélule minceur",
                  "gélules slimax","fabulons","thé jour","gélules minceurs"," crème metiss", 
                  " gant gommant", " gélules minceurs","crème metiss","fabulous"," gommage fabulons") 

replacements <- c("huile velours","huile velours","savon 24k","savon 24k","savon métiss",
  "savon Amino Carimo","thé yamad soir",
  "gelules amincissantes","gelules amincissantes","fabulous","thé matin","gelules amincissantes",
  "crème metiss","eponge gommante","gelules amincissantes","crème visage metiss","gommage fabulous","gommage fabulous")

data_final <- data_final %>%
  mutate(new = str_replace_all(achat_category, setNames(replacements, words_to_replace)))

df_correspondances <- fuzzyjoin::stringdist_left_join(data_final, products, 
                                                      by = c("new" = "PRODUITS"), 
                                                      method = "jw", 
                                                      max_dist = 0.2, 
                                                      distance_col = "distance")
df_correspondances <- df_correspondances %>%
  mutate(PRODUITS = if_else(is.na(PRODUITS), achat_category, PRODUITS))

df_correspondances <- df_correspondances %>%
  group_by(achat_category) %>%
  slice_min(order_by = distance, n = 1) %>%
  ungroup()

data_unique <- df_correspondances %>%
  group_by(`_id`,name,phone_number,age, gender,achat_category,type_client,partenaire_phone,
           partenaire_type,location_patenaire,carimo_product,isWinner,createdAt,formatted_date) %>%
  summarise(
    produits = first(PRODUITS),
    gamme = first(GAMME),
    montant = first(MONTANT),
    distance = first(distance)
  ) %>%
  ungroup()



# Utiliser mutate pour séparer les valeurs
data_unique$location_patenaire <- stringr::str_to_upper(data_unique$location_patenaire)

data_unique <- data_unique %>%
  mutate(
    ville = sapply(location_patenaire, extract_ville),
    quartier = mapply(extract_quartier, location_patenaire, sapply(location_patenaire, extract_ville))
  )
data_unique$ville <-ifelse(data_unique$ville=="", "YAOUNDÉ",data_unique$ville)

data_unique <- data_unique %>%
  mutate(Age = sapply(age, convert_and_categorize))

data_unique$quartier <- gsub(",BILEL AUSSI","ODZA", data_unique$quartier)
data_unique$produits <- gsub("gommage fabulons","gommage fabulous PM", data_unique$produits)
data_unique$produits <- gsub(" gommage fabulous","gommage fabulous", data_unique$produits)

