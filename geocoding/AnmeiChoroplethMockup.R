library(shiny)
library(tidyverse)
library(jsonlite)
library(httr)
library(plotly)
library(RSocrata)

source("plot_helper.R")


# define constants/settings
log_scale_base <- 2
tick_intervals <- c(0, 0.2, 0.4, 0.6, 0.8, 1)
default_tick_mode <- "auto"

# preloads
zipcode_ref <- read_csv("uszips.csv", col_types = cols(zip = col_integer(), 
                                                       county_fips = col_character()))
county_pop_call <- GET("https://api.census.gov/data/2018/pep/population",
                query = list(
                  get = "POP",
                  `for` = "county:*",
                  `in` = "state:*"
                ))
county_mat <- data.frame(matrix(unlist(content(county_pop_call)), ncol = 3, byrow = TRUE)[-1,])
county_ref <- county_mat %>% 
  unite(county_fips, c(X2, X3), sep = "") %>%
  mutate(pop =  strtoi(X1)) %>% 
  select(-X1)

drg_groups <- get_api_call(list("$select" = "distinct drg_definition"))


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  sidebarLayout(
    sidebarPanel(
      radioButtons("aggregation_level", 
                   "Aggregation level:", 
                   c("State" = "st", "County (slower)" = "cty"),
                   selected = "st"
                   ),
      checkboxInput("log_scale",
                    "Log scale the data?"),
      checkboxInput("correct_by_pop",
                    "Correct for population?"),
      selectInput("drg_group", 
                  "Filter by DRG group",
                  c("All groups", drg_groups$drg_definition))
    ),
    mainPanel(
      plotlyOutput("choropleth_plot")
    )
  )
)

server <- function(input, output) {
  
  agg_by_county <- reactive({    
    input$aggregation_level == "cty"
    })
  
  api_call_param <- reactive({
    if (agg_by_county()) {
      api_call_param <- list("$select" = c("provider_zip_code","sum(total_discharges)"), 
                             "$group" = "provider_zip_code", "$limit" = "50000")
    } else {
      api_call_param <- list("$select" = c("provider_state","sum(total_discharges)"), 
                             "$group" = "provider_state")
    }
    
    if (input$drg_group != "All groups") {
      api_call_param[["$where"]] <- paste("drg_definition='", 
                                          URLencode(input$drg_group, reserved = TRUE), 
                                          "'", sep = "")
    }
    api_call_param
  })
  
  construct_aggregate <- reactive({
    if (agg_by_county()) {
      zip_agg <- get_api_call(api_call_param())
      
      agg <- zip_agg %>% 
        mutate(zip = as.integer(provider_zip_code)) %>% 
        left_join(zipcode_ref, by = "zip") %>% 
        select(sum_total_discharges, 
               county_fips,
               county_name) %>% 
        group_by(county_fips, county_name) %>% 
        summarise(value = sum(as.integer(sum_total_discharges))) %>%
        left_join(county_ref, by = "county_fips") %>%
        drop_na() %>%
        mutate(region_name = county_name) %>%
        rename(region = county_fips)
      
      agg
      
    } else {
      state_agg <- get_api_call(api_call_param())
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
      
      agg
    }
  })
  
  output$choropleth_plot <- renderPlotly({
    
    if (agg_by_county()) {
      agg_geom <- "https://raw.githubusercontent.com/plotly/datasets/master/geojson-counties-fips.json"
      agg_key <- "id"
      colorscale <- "Reds"
      
    } else {
      agg_geom <- "https://raw.githubusercontent.com/PublicaMundi/MappingAPI/master/data/geojson/us-states.json"
      agg_key <- "properties.name"
      colorscale <- "Reds"
    }
    
    agg <- construct_aggregate() %>% mutate(hover = paste(
      region_name,
      "<br />Population:",
      pretty_print_large_number(pop),
      "<br />Discharges:",
      pretty_print_large_number(value)
    ))
    
    if (input$correct_by_pop) {
      agg <- agg %>% mutate(value = value/pop)
    }
    
    zmin <- min(agg$value)
    zmax <- max(agg$value)
    
    if (input$log_scale) {
      agg <- agg %>% mutate(value = log(value, log_scale_base))
      zmin <- log(zmin, log_scale_base)
      zmax <- log(zmax, log_scale_base)
      tickvals <- zmin + ((zmax - zmin) * tick_intervals)
      ticktext <- pretty_print_large_number(log_scale_base^tickvals)
      tickmode <- "array"
    } else {
      zmin <- 0
      tickvals <- zmin + ((zmax - zmin) * tick_intervals)
      ticktext <- pretty_print_large_number(tickvals)
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

