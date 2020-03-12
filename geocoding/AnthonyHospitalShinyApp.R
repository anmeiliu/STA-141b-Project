#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(jsonlite)
library(tidyverse)
library(sqldf)

data <- fromJSON("https://data.cms.gov/resource/97k6-zzx3.json")

data <- data %>% rename(zip = provider_zip_code) 
zipcodes <- read.csv("uszips.csv")

#Joining Data, finding coordinates based on zip codes
hospital_data <- sqldf("SELECT *
              FROM data
              JOIN zipcodes USING(zip)")

#Finding total number of discharges per hospital (regardless of DRG_Definition)
hospital_info <- sqldf("SELECT provider_name, MAX(lat) AS lat, MAX(lng) AS lng, 
                       AVG(provider_id) AS provider_id,
                       SUM(total_discharges) AS total_discharges_per_hospital,
                       AVG(average_covered_charges) AS Average_Coverage
                       FROM hospital_data GROUP BY provider_id")

hospital_info2 <- sqldf("SELECT info.provider_name, info.lat, info.lng, info.Average_Coverage,
                          info.provider_id, info.total_discharges_per_hospital,
                          data.provider_state, data.city, data.state_name,
                          data.density AS Population_Density, data.population AS Population
                        FROM hospital_info AS info
                        INNER JOIN hospital_data AS data ON
                          data.provider_id = info.provider_id")




library(shiny)
library(tidyverse)
library(jsonlite)
library(dplyr)
library(sqldf)
library(plotly)




# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Hospital Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("state_choice", "Choose a State", choices = c("Select All", unique(hospital_info2$provider_state)))

        ),

        mainPanel(
           plotlyOutput("hospital_plot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(session, input, output) {

    
    hospital_info3 <- reactive({
        if(input$state_choice == "Select All") {
            hospital_info2}
        else {
            filter(hospital_info2, provider_state == input$state_choice)}
        })
    
    #hospital_info3 <- reactive({filter(hospital_info2, provider_state == input$state_choice)})
    output$dtab <- renderDataTable(hospital_info3())
    

    
    output$hospital_plot <- renderPlotly({
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
        
        plot_geo(hospital_info3(), lat = ~lat, lon = ~lng) %>% 
        add_markers(
            text = ~paste(provider_name, paste(city, state_name, sep = ", "), 
                paste("Total Discharges:", total_discharges_per_hospital, " "),  
                paste("Average Coverage: $", Average_Coverage, sep = ""), sep = "<br />"),
                color = ~Population, symbol = I("square"), size = I(8), hoverinfo = "text") %>% 
        layout(title = 'Hospital Data', geo = g)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
