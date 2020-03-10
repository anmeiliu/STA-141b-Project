library(tidyverse)
library(jsonlite)
library(dplyr)
library(sqldf)
library(plotly)


#Sample Data 1000 rows
data <- fromJSON("https://data.cms.gov/resource/97k6-zzx3.json")

#Coordinates for all zipcodes in US
zipcodes <- read.csv("geocoding/uszips.csv")

#Data processing
data <- data %>% rename(zip = provider_zip_code) 

#Joining Data, finding coordinates based on zip codes
hospital_data <- sqldf("SELECT *
              FROM data
              JOIN zipcodes USING(zip)")

#Finding total number of discharges per hospital (regardless of DRG_Definition)
hospital_info <- sqldf("SELECT provider_name, AVG(provider_id) AS provider_id, SUM(total_discharges) AS total_discharge_per_hospital FROM hospital_data GROUP BY provider_id")

#Joining total number of discharges per hospital to original dataset 
hospital_data2 <-  sqldf("SELECT *
              FROM hospital_data
              LEFT JOIN hospital_info USING(provider_id)")


#plotly map Geomap
g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showland = TRUE,
  landcolor = toRGB("gray95"),
  subunitcolor = toRGB("gray85"),
  countrycolor = toRGB("gray85"),
  countrywidth = 0.5,
  subunitwidth = 0.5
)

#map
fig <- plot_geo(hospital_data2, lat = ~lat, lon = ~lng)
fig <- fig %>% add_markers(
  text = ~paste(provider_name, paste(city, state_name, sep = " "),  sep = "<br />"),
  color = ~total_discharges_per_hospital, symbol = I("square"), size = I(8), hoverinfo = "text"
)
fig <- fig %>% layout(title = 'Hospital Data', geo = g)
fig

