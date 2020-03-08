##############################################
# R SCRIPT FOR DATA PLOTLY DATA VISUALIZATIONS
##############################################

library(tidyverse)
library(jsonlite)
library(dplyr)

data <- fromJSON("https://data.cms.gov/resource/97k6-zzx3.json")

data <- data %>% 
  mutate(whole_address = paste(provider_street_address, provider_city, provider_state, sep = ', '))
