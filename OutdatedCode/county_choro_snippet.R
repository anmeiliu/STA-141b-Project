library(RSocrata)
library(choroplethr)
library(dplyr)

source("HospitalViz/api_helper")
zip_agg <- get_api_call(list("$select" = c("provider_zip_code","sum(total_discharges)"), "$group" = "provider_zip_code"))

discharge_per_cty <- zip_agg %>% 
  mutate(zip = as.integer(provider_zip_code)) %>% 
  left_join(zipcodes, by = "zip") %>% 
  select(sum_total_discharges, 
         county_fips) %>% 
  group_by(county_fips) %>% 
  summarise(value = sum(as.integer(sum_total_discharges))) %>% 
  rename(region = county_fips)
