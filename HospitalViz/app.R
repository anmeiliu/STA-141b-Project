#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(RSocrata)
source("api_helper.R")

app_key <- Sys.getenv("APP_KEY")
api_endpoint <- "https://data.cms.gov/resource/97k6-zzx3.json"

drg_groups <- get_api_call(list("$select" = "distinct drg_definition"))
hospitals <- get_api_call(list("$select" = c("distinct provider_name", "provider_id"), "$order" = "provider_name"))

ui <- fluidPage(
   
   titlePanel("Hospital Data"),
   
   sidebarLayout(
      sidebarPanel(
         selectInput("drg_group",
                     "DRG group",
                     drg_groups$drg_definition
                     ),
         selectInput("hospital", 
                     "Hospital",
                     hospitals$provider_name)
      ),
       mainPanel("hi")
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
}

# Run the application 
shinyApp(ui = ui, server = server)

