library(tidyverse)
library(jsonlite)
library(dplyr)


data <- fromJSON("https://data.cms.gov/resource/97k6-zzx3.json")

data <- data %>% 
  mutate(whole_address = paste(provider_street_address, provider_city, provider_state, sep = ', '))



addresses <- c("Baker Street 221b, London", "gasodifasf", "Brandenburger Tor, Berlin", 
               "Platz der Deutschen Einheit 1, Hamburg", "Arc de Triomphe de l’Etoile, Paris",
               "Дворцовая пл., Санкт-Петербург, Россия")

coords <- c()

coords <- suppressWarnings(lapply(addresses, function(address) {

    api_output <- nominatim_osm(address)
    
    if (is.data.frame(api_output) && nrow(api_output) == 0) {
      api_output <- c(NA)
    }
    
    return(data.frame(address = address, api_output))}
    
    ) %>%

    bind_rows() %>% data.frame())


coords

