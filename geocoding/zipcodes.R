library(tidyverse)
library(jsonlite)
library(dplyr)
library(sqldf)


data <- fromJSON("https://data.cms.gov/resource/97k6-zzx3.json")
zipcodes <- read.csv("uszips.csv")


data <- data %>% rename(zip = provider_zip_code) 

hospital_data <- sqldf("SELECT *
              FROM data
              JOIN zipcodes USING(zip)")

View(hospital_data)
