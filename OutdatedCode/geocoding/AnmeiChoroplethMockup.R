library(shiny)
library(tidyverse)
library(scales)
library(jsonlite)
library(httr)
library(plotly)
library(RSocrata)

source("plot_helper.R")

# define constants/settings ====
log_scale_base <- 2 # what log base to log scale data by
tick_intervals <-
  c(0, 0.2, 0.4, 0.6, 0.8, 1) # where to place ticks on the scale
default_tick_mode <-
  "auto" # default tick mode when it doesn't need to be overridden

states <-
  c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", 
    "GA", "HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD", 
    "ME", "MI", "MN", "MO", "MS", "MT", "NC", "ND", "NE", "NH", "NJ", 
    "NM", "NV", "NY", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", 
    "TX", "UT", "VA", "VT", "WA", "WI", "WV", "WY")

# for whatever reason, one of the vars appears to be mislabeled in the api
allowable_vars <- c("total_discharges", "average_covered_charges", "average_medicare_payments")
names(allowable_vars) <- c("Total Discharges", "Average Covered Charges", "Average Total Payments")


# preloads ====
# not sure what the source for this
zipcode_ref <-
  read_csv("uszips.csv",
           col_types = cols(zip = col_integer(),
                            county_fips = col_character()))

# 2018 population data from census (2019 API not available?)
county_pop_call <-
  GET(
    "https://api.census.gov/data/2018/pep/population",
    query = list(get = "POP",
                 `for` = "county:*",
                 `in` = "state:*")
  )
county_mat <-
  data.frame(matrix(unlist(content(county_pop_call)), ncol = 3, byrow = TRUE)[-1,])
county_ref <- county_mat %>%
  unite(county_fips, c(X2, X3), sep = "") %>%
  mutate(pop =  strtoi(X1)) %>%
  select(-X1)

# diagnosis-related group list for selector
drg_groups <-
  get_api_call(list("$select" = "distinct drg_definition"))

# user interface ====
ui <-
  fluidPage(tabsetPanel(
    type = "tabs",
    # nationwide tab ====
    tabPanel(
      "View national dataset",
      sidebarLayout(
        sidebarPanel(
          selectInput(
            "drg_group",
            "Filter by DRG group",
            c("All groups", drg_groups$drg_definition)
          ),
          radioButtons(
            "aggregation_level",
            "Aggregation level:",
            c("State" = "st", "County (slower)" = "cty"),
            selected = "st"
          ),
          hr(),
          checkboxInput("log_scale",
                        "Log scale the data?"),
          checkboxInput("correct_by_pop",
                        "Correct for population?"),
          checkboxInput("count_hospitals",
                        "Display number of hospitals per region?")
        ),
        mainPanel(tabsetPanel(
          type = "tabs",
          tabPanel("Map", plotlyOutput("choropleth_plot", height = "600px")),
          tabPanel(
            "Histogram",
            plotOutput("histogram_plot_full", height = "600px")
          )
        ))
      )
    ),
    # filtered tab ====
    tabPanel("Explore the data", 
             sidebarLayout(
               # TODO: filters by state -> county -> specific hospital
               # things I should be able to do:
               # which drg is most costly at this hospital?
               # how much does treating a specific drg cost at various hospitals?
               # histograms of discharge # by state, county
               # histograms of discharge # by drg?
               sidebarPanel(
                 selectInput("expl_drg", "Filter by DRG group", c("All groups", drg_groups$drg_definition)),
                 selectInput("expl_state", "Filter by state", c("All states", states)),
                 uiOutput("expl_hospital"),
                 selectInput("expl_var", "Variable shown", allowable_vars),
                 actionButton("submit_button", "Show selected data")), 
               mainPanel(
                 conditionalPanel("output.setempty", 
                                  "Sorry, there is no data to display"
                                  ),
                 plotOutput("subset_plot"))
               )
             )
  ))

# server function ====
server <- function(input, output) {
  # nationwide data stuff ====
  # reactive wrapper for aggregation level (which is used in several places)
  agg_by_county <- reactive({
    input$aggregation_level == "cty"
  })
  
  log_scale <- reactive({
    input$log_scale
  })
  
  summary_vars <- reactive({
    if (input$count_hospitals) {
      c("sum_total_discharges", "count_distinct_provider_id")
    } else {
      "sum_total_discharges"
    }
  })
  
  # reactive generation for API call parameters
  api_call_param <- reactive({
    if (agg_by_county()) {
      # limit forces all data to be displayed
      api_call_param <-
        list(
          "$select" = c("provider_zip_code", "sum(total_discharges)"),
          "$group" = "provider_zip_code",
          "$limit" = "50000"
        )
    } else {
      api_call_param <-
        list(
          "$select" = c("provider_state", "sum(total_discharges)"),
          "$group" = "provider_state"
        )
    }
    
    # filter by DRG if option is selected
    # all DRGs contain spaces so must be passed as string literals
    # some DRGs contain ampersands so must be URL encoded
    if (input$drg_group != "All groups") {
      api_call_param[["$where"]] <- paste("drg_definition='",
                                          URLencode(input$drg_group, reserved = TRUE),
                                          "'",
                                          sep = "")
    }
    
    if (input$count_hospitals) {
      api_call_param[["$select"]] <-
        c(api_call_param[["$select"]], "count(distinct provider_id)")
    }
    
    api_call_param
  })
  
  # begins creating the inital aggregate dataframe
  begin_construction <- reactive({
    if (agg_by_county()) {
      zip_agg <- get_api_call(api_call_param())
      
      agg <- zip_agg %>%
        mutate(zip = as.integer(provider_zip_code)) %>%
        left_join(zipcode_ref, by = "zip") %>%
        select(c(summary_vars(),
                 county_fips,
                 county_name)) %>%
        group_by(county_fips, county_name)
      
    } else {
      state_agg <- get_api_call(api_call_param())
      agg <- state_agg %>%
        rename(state_id = provider_state) %>%
        left_join(
          zipcode_ref %>%
            group_by(state_id, state_name) %>%
            summarize(population = sum(population)),
          by = "state_id"
        ) %>%
        select(c(summary_vars(),
                 population,
                 state_name)) %>%
        group_by(state_name)
    }
    agg
  })
  
  # summarize step must be done reactively to summarize correct vars
  continue_construction <- reactive({
    agg <- begin_construction()
    if (input$count_hospitals) {
      if (agg_by_county()) {
        agg <- agg %>%
          summarize(value = sum(as.integer(sum_total_discharges)),
                    hospitals = sum(as.integer(count_distinct_provider_id)))
      } else {
        agg <- agg %>%
          summarize(
            value = sum(as.integer(sum_total_discharges)),
            hospitals = sum(as.integer(count_distinct_provider_id)),
            pop = sum(population)
          )
      }
    } else {
      if (agg_by_county()) {
        agg <- agg %>%
          summarise(value = sum(as.integer(sum_total_discharges)))
      } else {
        agg <- agg %>%
          summarise(value = sum(as.integer(sum_total_discharges)),
                    pop = sum(population))
      }
    }
    
    agg
  })
  
  # this step wraps the aggregation steps and completes construction
  construct_aggregate <- reactive({
    agg <- continue_construction()
    if (agg_by_county()) {
      agg <- agg %>%
        left_join(county_ref, by = "county_fips") %>%
        drop_na() %>%
        mutate(region_name = county_name) %>%
        rename(region = county_fips)
    } else {
      agg <- agg %>%
        mutate(region_name = state_name) %>%
        rename(region = state_name)
    }
    agg
  })
  
  # this step adds tooltips (which must be done reactively)
  construct_and_add_tooltip <- reactive({
    if (input$count_hospitals) {
      agg <- construct_aggregate() %>% mutate(
        hover = paste(
          region_name,
          "<br />Population:",
          pretty_print_large_number(pop),
          "<br />Discharges:",
          pretty_print_large_number(value),
          "<br />Unique Hospitals:",
          pretty_print_large_number(hospitals)
        )
      )
    } else {
      agg <- construct_aggregate() %>% mutate(
        hover = paste(
          region_name,
          "<br />Population:",
          pretty_print_large_number(pop),
          "<br />Discharges:",
          pretty_print_large_number(value)
        )
      )
    }
  })
  
  # plot the choropleth
  output$choropleth_plot <- renderPlotly({
    # set aggregation-level specific display options
    # plotly's mapbox choropleth prefers jsons as URLs
    if (agg_by_county()) {
      agg_geom <-
        "https://raw.githubusercontent.com/plotly/datasets/master/geojson-counties-fips.json"
      agg_key <- "id"
      colorscale <- "Reds"
    } else {
      agg_geom <-
        "https://raw.githubusercontent.com/PublicaMundi/MappingAPI/master/data/geojson/us-states.json"
      agg_key <- "properties.name"
      colorscale <- "Reds"
    }
    
    agg <- construct_and_add_tooltip()
    
    # correct values by population if selected
    if (input$correct_by_pop) {
      agg <- agg %>% mutate(value = value / pop)
    }
    
    # find current minimum and maximum of values
    zmin <- min(agg$value)
    zmax <- max(agg$value)
    
    # rescale values by log if necessary
    if (log_scale()) {
      agg <- agg %>% mutate(value = log(value, log_scale_base))
      zmin <- log(zmin, log_scale_base)
      zmax <- log(zmax, log_scale_base)
      tickvals <- zmin + ((zmax - zmin) * tick_intervals)
      ticktext <-
        pretty_print_large_number(log_scale_base ^ tickvals)
      print(tickvals)
      print(pretty_print_large_number(log_scale_base ^ tickvals[1:6]))
      print(ticktext)
      tickmode <-
        "array" # forces choropleth to use passed in tickvals/ticktext
    } else {
      zmin <- 0
      tickvals <- zmin + ((zmax - zmin) * tick_intervals)
      ticktext <- pretty_print_large_number(tickvals)
      tickmode <-
        default_tick_mode # if default is auto, tickvals/ticktext will be ignored
    }
    
    # actual plot generation
    fig <- plot_ly()
    fig <- fig %>% add_trace(
      type = "choroplethmapbox",
      geojson = agg_geom,
      featureidkey = agg_key,
      locations = agg$region,
      z = agg$value,
      colorscale = colorscale,
      zmin = zmin,
      zmax = zmax,
      marker = list(line = list(width = 0),
                    opacity = 0.5),
      text = agg$hover,
      hoverinfo = "text",
      colorbar = list(
        tickmode = tickmode,
        tickvals = tickvals,
        ticktext = ticktext
      )
    )
    
    fig <- fig %>% layout(mapbox = list(
      style = "carto-positron",
      zoom = 3,
      center = list(lon = -95.71, lat = 37.09)
    ))
    
    fig
  })
  
  output$histogram_plot_full <- renderPlot({
    plot <- ggplot(construct_aggregate(), aes(x = value)) +
      geom_histogram(bins = 10,
                     color = "black",
                     fill = "white") +
      ggtitle("How many regions discharged how many patients?") +
      xlab("Number of discharges") +
      ylab("Number of regions")
    if (log_scale()) {
      plot <- plot + scale_x_log10(labels = comma)
    } else {
      plot <- plot + scale_x_continuous(labels = comma)
    }
    plot
  })
  
  # filtered subset stuff ====
  
  is_drg_filtered <- reactive({
    input$expl_drg != "All groups"
  })
  
  is_state_filtered <- reactive({
    input$expl_state != "All states"
  })
  
  is_hospital_filtered <- reactive({
    input$expl_hospital != "None"
  })
  
  fetch_eligible_hospitals <- reactive({
    eligible <- get_api_call(list("$select" = c("distinct provider_id", "provider_name"),
                                  "$where" = paste("provider_state='", input$expl_state, "'", sep = ""),
                                  "$order" = "provider_name"))
    eligible <- eligible %>% mutate(display = paste(provider_name, " (ID: ", provider_id, ")", sep = ""))
    eligible
  })
  
  eligible_hospitals <- reactive({
    if (!is_state_filtered()) {
      eligible <- c("None")
      names(eligible) <- c("All hospitals (select a state for more options)")
    } else {
      eligible <- c("None", fetch_eligible_hospitals()$provider_id)
      names(eligible) <- c("All hospitals", fetch_eligible_hospitals()$display)
    }
    eligible
  })
  
  select_string <- reactive({
    select_string <- c(input$expl_var, "provider_id")
    if (is_drg_filtered()) {
      select_string <- c(select_string, "drg_definition")
    }
    if (is_state_filtered()) {
      select_string <- c(select_string, "provider_state")
    }
    select_string
  })
  
  loc_string <- reactive({
    if (is_hospital_filtered()) {
      loc_string <- paste("provider_id=", input$expl_hospital, sep = "")
    } else if (is_state_filtered()) {
      loc_string <- paste("provider_state=", input$expl_state, sep = "")
    }
    loc_string
  })
  
  drg_string <- reactive({
    drg_string <- paste("drg_definition='", URLencode(input$expl_drg, reserved = TRUE), "'", sep = "")
  })
  
  where_string <- reactive({
    where_string <- c()
    if (is_state_filtered() | is_hospital_filtered()) {
      where_string <- c(where_string, loc_string())
    }
    if (is_drg_filtered()){
      where_string <- c(where_string, drg_string())
    }
    where_string <- paste(where_string, collapse = " AND ")
    where_string
  })
  
  fetch_filtered_data <- reactive({
    api_params <- list("$select" = select_string())
    if (is_state_filtered() | is_hospital_filtered() | is_drg_filtered()) {
      api_params[["$where"]] = where_string()
    }
    get_api_call(api_params)
  })
  
  filter_set_num_hospital <- reactive({
    n_distinct(fetch_filtered_data()$provider_id)
  })
  
  create_subset_plot <- reactive({
    data <- fetch_filtered_data()
  })
  
  load_subset_plot <- eventReactive(input$submit_button, {
    plot <- create_subset_plot()
    plot
  })
  
  
  output$expl_hospital <- renderUI({selectInput("expl_hospital", 
                                                "Filter by hospital", 
                                                eligible_hospitals())})

  
  output$setempty <- reactive({
#    filter_set_num_hospital() == 0
    })
  
  outputOptions(output, "setempty", suspendWhenHidden = FALSE)
  
  output$subset_plot <- renderPlot({load_subset_plot()})
}

shinyApp(ui = ui, server = server)
