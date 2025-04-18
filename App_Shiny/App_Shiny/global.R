library(shiny)
library(shinythemes)
library(bslib)
library(markdown)
library(rAmCharts)
library(colourpicker)
library(tidyverse)
library(shinydashboard)
library(leaflet)
library(dplyr)
library(shinyjs)
library(plotly)
library(DT)
library(openxlsx)



#Importation des données
data_covid_19 <- 
  world_covid_19 <- 
  read_delim("data/covid_19_clean_complete.csv",delim = ",", na = c(""))

data_covid_19 <-
  data_covid_19 |>
  relocate(Date, .before = 'Province/State')

#Visualiser les données
#view(data_covid_19)

world_data <- 
  data_covid_19 |>
  summarise(
    Confirmed= sum(Confirmed, na.rm = TRUE),
    Deaths= sum(Deaths, na.rm = TRUE),
    Recovered= sum(Recovered ,na.rm = TRUE),
    Active= sum(Active, na.rm = TRUE),
  )

# Création de la variable data_01 
# Les cas confirmés en fonction des pays
data_01 <- data_covid_19 |>
  group_by(`Country/Region`) |>
  summarise(
    latitude = mean(Lat, na.rm = TRUE),
    longitude = mean(Long, na.rm = TRUE),
    confirmed_total = sum(Confirmed, na.rm = TRUE),
    Deaths_total = sum(Deaths,na.rm = TRUE),
    Recovered_total = sum(Recovered,na.rm = TRUE),
    Active_total = sum(Active,na.rm = TRUE)
    )

# Extraction des continents
data_region <- 
  world_covid_19 |>
  group_by(`WHO Region`) |>
  summarise(
    latitude = mean(Lat, na.rm = TRUE),
    longitude = mean(Long, na.rm = TRUE),
    Confirmed_total = sum(Confirmed, na.rm = TRUE),
    Deaths_total = sum(Deaths, na.rm = TRUE),
    Recovered_total = sum(Recovered, na.rm = TRUE),
    Active_total = sum(Active, na.rm = TRUE),)

# Extraction des continents 01
data_region_01 <- 
  world_covid_19 |>
  group_by(`WHO Region`) |>
  summarise(
    Lat = mean(Lat, na.rm = TRUE),
    Long = mean(Long, na.rm = TRUE),
    Confirmed = sum(Confirmed, na.rm = TRUE),
    Deaths = sum(Deaths, na.rm = TRUE),
    Recovered = sum(Recovered, na.rm = TRUE),
    Active = sum(Active, na.rm = TRUE),)

#Création de la carte leaflet()
ma_carte_00 <- 
  leaflet() |>
  addTiles() |>
  setView(lng = 0, lat = 30, zoom = 3)

#On extrait les éléments de providers qui comment par Esri
esri <- grep("^Esri", providers, value = TRUE)

#Ajout des tuiles de sélections
for (provider in esri) {
  if (provider=="Esri") {
    ma_carte_00 <- ma_carte_00 |> addProviderTiles(provider, group = provider)
  }
  else if (provider=="Esri.WorldTopoMap"){
    ma_carte_00 <- ma_carte_00 |> addProviderTiles(provider, group = provider)
  }
  else if (provider=="Esri.NatGeoWorldMap"){
    ma_carte_00 <- ma_carte_00 |> addProviderTiles(provider, group = provider)
  }
  else if(provider=="Esri.WorldPhysical"){
    ma_carte_00 <- ma_carte_00 |> addProviderTiles(provider, group = provider)
  }
  else if (provider=="Esri.WorldImagery"){
    ma_carte_00 <- ma_carte_00 |> addProviderTiles(provider, group = provider)
  }
  #print(provider)
}

#Afficher la carte avec les tuiles
ma_carte_00 <-
ma_carte_00 |>
  addLayersControl(
    # Les différentes font de la carte
    baseGroups = c(names(esri[1]),names(esri[4]), names(esri[5]),names(esri[8]),names(esri[10])),
    options = layersControlOptions(collapsed = FALSE)) |>
  addMiniMap( #Minimap
    tiles = esri[[1]], 
    toggleDisplay = TRUE,
    position = "bottomleft") %>%
  htmlwidgets::onRender("
    function(el, x) {
      var myMap = this;
      myMap.on('baselayerchange',
        function (e) {
          myMap.minimap.changeLayer(L.tileLayer.provider(e.name));
        })
    }")

#Affichage de la carte du monde
ma_carte_01 <- 
  ma_carte_00 |>
  #addProviderTiles("CartoDB.DarkMatter") |>
  addCircleMarkers(
    data = data_01,
    lng = ~longitude,
    lat = ~latitude,
    color = "red",
    popup = ~paste(
      `Country/Region`, "<br>",
      "Cas confirmés:", confirmed_total,"<br>",
      "Décès:",Deaths_total,"<br>",
      "Guéris:",Recovered_total),
    labelOptions = labelOptions(noHide = TRUE, direction = "auto"),
    clusterOptions = markerClusterOptions(),
    radius = 3
  ) |>
  setView(0, 0, zoom = 2)

#Extraction du Mois de la BD
data_covid_19$Month <-
  format(world_covid_19$Date, "%m/%Y")
# Variable mois
months <- 
  data_covid_19 |>
  group_by(Month,`WHO Region`) |>
  summarise( 
    Confirmed = sum(Confirmed),
    Deaths= sum(Deaths),
    Recovered= sum(Recovered),
    Active = sum(Active)
  )
######## LE MOIS EXTRAIT EST EN CHAINE DE CARACTERE #########
# On peut utiliser fct_recode(as.character(month$Month)
# CELA AURAIT DONNE
#month$Month <- 
  #fct_recode(
    #as.character(total_month$month),
    #"janvier"="01/2020","février"="02/2020","mars"="03/2020","avril"="04/2020",
    #"mai"="05/2020", "juin"="06/2020","juillet"="07/2020")
# Ensuite transformer en date
month <- 
  data_covid_19 |>
  group_by(Month) |>
  summarise( 
    Confirmed = sum(Confirmed),
    Deaths= sum(Deaths),
    Recovered= sum(Recovered),
    Active = sum(Active)
  )
### OU ENCORE
month$Month <- paste0("01/",month$Month)
month$Month <- as.Date(month$Month, format = "%d/%m/%Y")
# ICI on ajoute le 01 et on convertit en date par la suite
# Maintenant les dates en nom des mois
#month$Month <- format(month$Month, "%B")
