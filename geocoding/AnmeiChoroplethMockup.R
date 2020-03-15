library(shiny)
library(tidyverse)
library(jsonlite)
library(plotly)
library(RSocrata)

source("plot_helper.R")


# define constants/settings
log_scale_base <- 2
default_tick_mode <- "auto"

# preloads
zipcode_ref <- read_csv("uszips.csv", col_types = cols(zip = col_integer(), county_fips = col_character()))


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  sidebarLayout(
    sidebarPanel(
      radioButtons("aggregation_level", 
                   "Aggregation level:", 
                   c("State" = "st", "County" = "cty"),
                   selected = "st"
                   ),
      checkboxInput("log_scale",
                    "Log scale the data?")
    ),
    mainPanel(
      plotlyOutput("choropleth_plot")
    )
  )
)

server <- function(input, output) {
  
  output$choropleth_plot <- renderPlotly({
    agg_by_county <- input$aggregation_level == "cty"
    if (agg_by_county) {
      agg_geom <- "https://raw.githubusercontent.com/plotly/datasets/master/geojson-counties-fips.json"
      agg_key <- "id"
      zip_agg <- get_api_call(list("$select" = c("provider_zip_code","sum(total_discharges)"), 
                                   "$group" = "provider_zip_code", "$limit" = "50000"))
      
      agg <- zip_agg %>% 
        mutate(zip = as.integer(provider_zip_code)) %>% 
        left_join(zipcode_ref, by = "zip") %>% 
        select(sum_total_discharges, 
               population,
               county_fips,
               county_name) %>% 
        group_by(county_fips, county_name) %>% 
        summarise(value = sum(as.integer(sum_total_discharges)), pop = sum(population)) %>%
        drop_na() %>%
        mutate(region_name = county_name) %>%
        rename(region = county_fips)
      colorscale <- "Reds"
      
    } else {
      agg_geom <- "https://raw.githubusercontent.com/PublicaMundi/MappingAPI/master/data/geojson/us-states.json"
      agg_key <- "properties.name"
      state_agg <- get_api_call(list("$select" = c("provider_state","sum(total_discharges)"), 
                                     "$group" = "provider_state"))
      agg <- state_agg %>% 
        rename(state_id = provider_state) %>% 
        left_join(zipcode_ref %>% 
                    group_by(state_id, state_name) %>% 
                    summarize(population = sum(population)), 
                  by = "state_id") %>% 
        select(sum_total_discharges, 
               population,
               state_name) %>% 
        group_by(state_name) %>% 
        summarise(value = sum(as.integer(sum_total_discharges)), pop = sum(population)) %>%
        mutate(region_name = state_name) %>%
        rename(region = state_name)
      colorscale <- "Reds"
    }
    
    zmin <- min(agg$value)
    zmax <- max(agg$value)
    
    agg <- agg %>% mutate(hover = paste(
      region_name,
      "<br />Population:",
      prettyNum(pop, big.mark = ","),
      "<br />Discharges:",
      prettyNum(value, big.mark = ",")
    ))
    
    if (input$log_scale) {
      agg <- agg %>% mutate(value = log(value, log_scale_base))
      zmin <- log(zmin, log_scale_base)
      zmax <- log(zmax, log_scale_base)
      tickvals <- zmin + ((zmax - zmin) * c(0, 0.2, 0.4, 0.6, 0.8, 1))
      ticktext <- sapply(log_scale_base^tickvals, pretty_print_large_number)
      tickmode <- "array"
    } else {
      zmin <- 0
      tickvals <- zmin + ((zmax - zmin) * c(0, 0.2, 0.4, 0.6, 0.8, 1))
      ticktext <- sapply(tickvals, pretty_print_large_number)
      tickmode <- default_tick_mode
    }
    
    
    fig <- plot_ly() 
    fig <- fig %>% add_trace(
      type="choroplethmapbox",
      geojson=agg_geom,
      featureidkey=agg_key,
      locations=agg$region,
      z=agg$value,
      colorscale=colorscale,
      zmin=zmin,
      zmax=zmax,
      marker=list(line=list(
        width=0),
        opacity=0.5
      ),
      text=agg$hover,
      hoverinfo = "text",
      colorbar=list(
        tickmode=tickmode,
        tickvals=tickvals,
        ticktext=ticktext
      )
    )
    
    fig <- fig %>% layout(
      mapbox=list(
        style="carto-positron",
        zoom =2,
        center=list(lon= -95.71, lat=37.09)
        )
      )
    
    fig
  })
}

shinyApp(ui = ui, server = server)

