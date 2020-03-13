library(shiny)
library(tidyverse)
library(jsonlite)
library(plotly)
library(RSocrata)

zipcode_ref <- read_csv("uszips.csv", col_types = cols(zip = col_integer(), county_fips = col_character()))


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  sidebarLayout(
    sidebarPanel(
      checkboxInput("agg_by_county", "Aggregate values by county instead?")
    ),
    mainPanel(
      plotlyOutput("choropleth_plot")
    )
  )
)

server <- function(input, output) {
  
  output$choropleth_plot <- renderPlotly({
    if (input$agg_by_county) {
      agg_geom <- "https://raw.githubusercontent.com/plotly/datasets/master/geojson-counties-fips.json"
      agg_key <- "id"
      zip_agg <- get_api_call(list("$select" = c("provider_zip_code","sum(total_discharges)"), 
                                   "$group" = "provider_zip_code", "$limit" = "50000"))
      
      agg <- zip_agg %>% 
        mutate(zip = as.integer(provider_zip_code)) %>% 
        left_join(zipcode_ref, by = "zip") %>% 
        select(sum_total_discharges, 
               county_fips) %>% 
        group_by(county_fips) %>% 
        summarise(value = sum(as.integer(sum_total_discharges))) %>% 
        rename(region = county_fips)
      colorscale <- "Reds"
      zmax <- 50000
      
    } else {
      agg_geom <- "https://raw.githubusercontent.com/PublicaMundi/MappingAPI/master/data/geojson/us-states.json"
      agg_key <- "properties.name"
      state_agg <- get_api_call(list("$select" = c("provider_state","sum(total_discharges)"), 
                                     "$group" = "provider_state"))
      agg <- state_agg %>% 
        rename(state_id = provider_state) %>% 
        left_join(zipcode_ref, by = "state_id") %>% 
        select(sum_total_discharges, 
               state_name) %>% 
        group_by(state_name) %>% 
        summarise(value = sum(as.integer(sum_total_discharges))) %>% 
        rename(region = state_name)
      colorscale <- "Reds"
      zmax <- 100000000
    }
    
    fig <- plot_ly() 
    fig <- fig %>% add_trace(
      type="choroplethmapbox",
      geojson=agg_geom,
      featureidkey=agg_key,
      locations=agg$region,
      z=agg$value,
      colorscale=colorscale,
      zmin=0,
      zmax=zmax,
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
  })
}

shinyApp(ui = ui, server = server)

