library(pipeR)
library(DT)
library(FredR)
library(plotly)
library(shiny)

source(".api_key")
api_key = key

fred <- FredR(api_key)
fed_rev <- fred$series.observations(series_id = 'FYFRGDA188S') #as a fraction of GDP
fed_rev$date <- as.Date(fed_rev$date)
annual_rev = as.numeric(fed_rev$value)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Federal Revenue Statistics"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      numericInput("yrs", 
                   "Last N Years:", 
                   value = 75, 
                   min = 1, 
                   length(annual_rev))),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot", width = 600, height = 400)
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$distPlot <- renderPlot({
    hist(tail(annual_rev, input$yrs),
         main = "Revenue as a Percentage of GDP",
         xlab = "Percentage of GDP",
         col = "goldenrod")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

