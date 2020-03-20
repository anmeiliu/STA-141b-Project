library(tidyverse)
library(jsonlite)
library(plotly)
library(RSocrata)

source("HospitalViz/api_helper.R")

zip_agg <- get_api_call(list("$select" = c("provider_zip_code","sum(total_discharges)"), "$group" = "provider_zip_code", "$limit" = "50000"))
zipcode_ref <- read_csv("geocoding/uszips.csv", col_types = cols(zip = col_integer(), county_fips = col_character()))

discharge_per_cty <- zip_agg %>% 
  mutate(zip = as.integer(provider_zip_code)) %>% 
  left_join(zipcode_ref, by = "zip") %>% 
  #  filter(state_id == "ME") %>%
  select(sum_total_discharges, 
         county_fips) %>% 
  group_by(county_fips) %>% 
  summarise(value = sum(as.integer(sum_total_discharges))) %>% 
  rename(region = county_fips)

county_agg <- discharge_per_cty

fig <- plot_ly() 
fig <- fig %>% add_trace(
  type="choroplethmapbox",
  geojson="https://raw.githubusercontent.com/plotly/datasets/master/geojson-counties-fips.json",
  locations=county_agg$region,
  z=county_agg$value,
  colorscale="Reds",
  zmin=0,
  zmax=50000,
  marker=list(line=list(
    width=0),
    opacity=0.5
  )
  
)
fig <- fig %>% layout(
  mapbox=list(
    style="carto-positron",
    zoom =2,
    center=list(lon= -95.71, lat=37.09))
)
fig