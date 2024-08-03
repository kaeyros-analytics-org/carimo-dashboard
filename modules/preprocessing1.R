library(readxl)
library(dplyr)
library(tidyr)
library(stringdist)
library(fuzzyjoin)
library(stringr)
library(plotly)

villes_connues <- c("YAOUNDÉ", "EDEA","BAFOUSSAM","MAROUA","GAROUA","DOUALA")


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

# Spécifiez le chemin du dossier contenant les fichiers Excel
path <- getwd()
folder_path <- paste(path,"/data",sep = "")
data <- read_excel((paste(folder_path,"/carimo_omnisport.xls",sep = "")))
data$achat_category <- str_to_lower(data$achat_category)

path_product <- paste(folder_path,"/carimo_products_1.xlsx", sep = "")
products <- read_excel(path_product,sheet = "products")

data <- data %>%
  mutate(across(everything(), ~ gsub("ã©", "é", .))) %>%
  mutate(across(everything(), ~ gsub("ã¨", "è", .))) %>% 
  mutate(across(everything(), ~ gsub("Ã©", "é", .))) %>% 
  mutate(across(everything(), ~ gsub("Yaounde", "Yaoundé", .))) %>%
  mutate(across(everything(), ~ gsub("Yde", "Yaoundé", .))) %>%
  mutate(across(everything(), ~ gsub("Ã¨", "E", .)))
data <- data %>% 
  mutate(isWinner = ifelse(is.na(isWinner), "lose", isWinner))
data$gender <- gsub("Female","Femme",data$gender)
data$gender <- gsub("Male","Homme",data$gender)

# Appliquer la transformation à la colonne du dataframe
data <- data %>%
  mutate(formatted_date = sapply(createdAt, extract_and_format_date))
data$formatted_date <- as.character(data$formatted_date)
data$formatted_date <- as.Date(data$formatted_date)
data$Age <- data$age

data$location_patenaire <- stringr::str_to_upper(data$location_patenaire)

data <- data %>%
  mutate(
    ville = sapply(location_patenaire, extract_ville),
    quartier = mapply(extract_quartier, location_patenaire, sapply(location_patenaire, extract_ville))
  )
data$ville <-ifelse(data$ville=="", "YAOUNDÉ",data$ville)
data$quartier <-ifelse(data$quartier=="", data$ville,data$quartier)
data$carimo_product <- ifelse(data$carimo_product=="true","ANCIEN","NOUVEAU")
data$quartier <- gsub("NVOG BIG","MVOG MBI", data$quartier)
data$quartier <- gsub("MVOG BI","MVOG MBI", data$quartier)
data$quartier <- gsub("Ã‰COLE DES POSTS","ECOLE DES POSTES", data$quartier) 
data$quartier <- gsub("SIMBOG","SIMBOCK", data$quartier)
data$quartier <- gsub("Ã‰MANA \\( \\)","EMANA", data$quartier)
data$quartier <- gsub("\\(ÉMANA\\)","EMANA", data$quartier)
data$quartier <- gsub("BITING","BITENG", data$quartier)
data$quartier <- gsub("BATA","NLONGKAK", data$quartier)
data$quartier <- gsub("AWAILLE","AWAE",data$quartier) 
data$quartier <- gsub("TERMINUS","MIMBOMAN TERMINUS",data$quartier) 
data$quartier <- gsub("NKLOBISONNE","NKOLBISSON",data$quartier) 
data$quartier <- gsub("EMANA FOKOU","EMANA",data$quartier) 
data$quartier <- gsub("MESSA","CAMP SIC MESSA",data$quartier)
data$quartier <- gsub("NSIMEYONG","SHELL NSIMEYONG",data$quartier)






data_final <- separate_rows(data, achat_category, sep = ",")


words_to_replace <- c("huile velours "," huile velours","savon or ","savon or","5 savons métis",
                      "amino","the soir","gélule minceur",
                      "gélules slimax","fabulons","thé jour","gélules minceurs"," crème metiss", 
                      " gant gommant", " gélules minceurs","crème metiss","fabulons","soins corporels",
                      " gommage")

replacements <- c("huile velours","huile velours","savon 24k","savon 24k","savon métiss",
                  "savon Amino Carimo","thé yamad soir",
                  "gelules amincissantes","gelules amincissantes","fabulous","thé matin","gelules amincissantes",
                  "crème metiss","eponge gommante","gelules amincissantes","crème visage metiss","fabulous","hammam",
                  "gommage fabulous")

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
  group_by(`_id`,name,phone_number,Age, gender,achat_category,type_client,partenaire_phone,
           partenaire_type,location_patenaire,carimo_product,isWinner,createdAt,formatted_date,ville,
           quartier,new) %>%
  summarise(
    produits = first(PRODUITS),
    gamme = first(GAMME),
    montant = first(MONTANT),
    distance = first(distance)
  ) %>%
  ungroup()


file_path <- "./data/cameroon-latest.osm.pbf"

# Lire le fichier .pbf
data_geo <- readRDS("data/qquaters.rds")
data_geo$name <- str_to_upper(data_geo$name)
list_to_exclude <- c("BASTOS","MADAGASCAR","MELEN","MENDONG","NSAM","MESSA","MANGUIER","EMANA","NGOUSSO")
data_geo <- data_geo %>%
  filter(!name %in% list_to_exclude)
data_geo$name <- gsub("MARCHÉ DE MVOG MBI","MVOG MBI",data_geo$name)
data_geo$name <- gsub("CARREFOUR BIYEM-ASSI","BIYEM ASSI",data_geo$name) 
data_geo$name <- gsub("CARREFOUR MESSASSI","MESSASSI",data_geo$name) 
data_geo$name <- gsub("EMOMBO CHAPELLE","EMOMBO",data_geo$name) 
data_geo$name <- gsub("CARREFOUR JOUVENCE","JOUVENCE",data_geo$name) 
data_geo$name <- gsub("SANTA LUCIA AHALA","AHALA",data_geo$name) 
data_geo$name <- gsub("TOTAL ETOUDI","ÉTOUDI",data_geo$name) 
data_geo$name <- gsub("CARREFOUR TSINGA VILLAGE","TSINGA VILLAGE",data_geo$name) 
data_geo$name <- gsub("CARREFOUR RÉGIE","REGIS",data_geo$name) 
data_geo$name <- gsub("CARREFOUR AWAÉ","AWAE",data_geo$name) 
data_geo$name <- gsub("PHARMACIE NKOZOA","NKOZOA",data_geo$name) 
data_geo$name <- gsub("PHARMACIE NKOZOA","NKOZOA",data_geo$name) 
data_geo$name <- gsub("NGOA EKELE CHÂTEAU","NGOA EKELE",data_geo$name) 	
data_geo$name <- gsub("ROND-POINT NLONGKAK","NLONKAK",data_geo$name) 
data_geo$name <- gsub("MARCHE CENTRAL DE LEBOUDI","LEBOUDI",data_geo$name) 
data_geo$name <- gsub("ROND-POINT BASTOS","BASTOS",data_geo$name) 
data_geo$name <- gsub("CITÉ SIC MADAGASCAR","MADAGASCAR",data_geo$name) 
data_geo$name <- gsub("TOTAL MELEN","MELEN",data_geo$name) 
data_geo$name <- gsub("DOVV MENDONG","MENDONG",data_geo$name) 
data_geo$name <- gsub("CARREFOUR NSAM","NSAM",data_geo$name) 
data_geo$name <- gsub("CAMP SIC MESSA","MESSA",data_geo$name) 
data_geo$name <- gsub("RUE MANGUIERS","MANGUIER",data_geo$name) 
data_geo$name <- gsub("EMANA PONT","EMANA",data_geo$name) 
data_geo$name <- gsub("DOVV NGOUSSO","NGOUSSO",data_geo$name) 
data_geo$name <- gsub("EDEA PARK","EDEA",data_geo$name) 
data_geo$name <- gsub("ARCHITEC LES CONCEPTEURS RÉUNIS, TITI GARAGE, YAOUNDÉ","TITI GARAGE",data_geo$name) 

# data_geo <- data_geo %>%
#   mutate(geometry = ifelse(name == "NSIMEYONG", "POINT (11.49596 3.8399)", geometry))




df_geo <- fuzzyjoin::stringdist_left_join(data, data_geo, 	
                                          by = c("quartier" = "name"), 
                                          method = "jw", 
                                          max_dist = 0.01, 
                                          distance_col = "distance")

geo <- df_geo %>%
  select(phone_number,ville,carimo_product,formatted_date,quartier,geometry) %>%
  group_by(phone_number,quartier) %>% #,quartier,geometry
  summarise(
    quartier = first(quartier),
    ville = first(ville),
    carimo_product = first(carimo_product),
    formatted_date = first(formatted_date),
    phone_number = first(phone_number),
    geometry = first(geometry)
  ) %>%
  ungroup()
geo$geometry <- gsub("POINT \\(", "", geo$geometry) 
geo$geometry <- gsub("\\)", "", geo$geometry)
geo$geometry <- gsub("c\\(", "", geo$geometry)

geo <- geo %>%
  separate(geometry, into = c("longitude", "latitude"), sep = ", ") %>%
  mutate(across(c(longitude, latitude), as.numeric))
