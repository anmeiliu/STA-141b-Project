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

app_key <- Sys.getenv("APP_KEY")
api_endpoint <- "https://data.cms.gov/resource/97k6-zzx3.json"

drg_groups <- read.socrata("https://data.cms.gov/resource/97k6-zzx3.json?$select=distinct drg_definition&$order=drg_definition", app_key)
drg_groups <- drg_groups$drg_definition

ui <- fluidPage(
   
   titlePanel("Hospital Data"),
   
   sidebarLayout(
      sidebarPanel(
         selectInput("drg_group",
                     "DRG group",
                     drg_groups      
                     )
      ),
       mainPanel("hi")
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
}

# Run the application 
shinyApp(ui = ui, server = server)

