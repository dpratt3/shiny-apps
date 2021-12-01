library(pipeR)
library(DT)
library(FredR)
library(plotly)
library(shiny)
library(dplyr)

source(".api_key")
api_key = key

fred <- FredR(api_key)
fed_rev <- fred$series.observations(series_id = 'FYFRGDA188S') # as a fraction of GDP
top_marg <- fred$series.observations(series_id = 'IITTRHB') # Top marginal rate
fed_rev$date <- as.Date(fed_rev$date)
top_marg$date <- as.Date(top_marg$date)
rev_marg <- inner_join(fed_rev, top_marg, by = "date", keep = F)
rev_marg <- as.data.frame(rev_marg)
rev_marg <- cbind.data.frame(rev_marg$date, rev_marg$value.x, rev_marg$value.y)
colnames(rev_marg) = c("date", "gdp_rev", "top_marg")

# consider only the annual revenue for the histogram
annual_rev = as.numeric(matrix(rev_marg$gdp_rev))

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
      plotOutput("distPlot1", width = 600, height = 400),
      plotOutput("distPlot2", width = 600, height = 400)
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$distPlot1 <- renderPlot({
    hist(tail(annual_rev, input$yrs),
         main = "Revenue as a Percentage of GDP",
         xlab = "Percentage of GDP",
         col = "goldenrod")
  })
  output$distPlot2 <- renderPlot({
    data = tail(rev_marg, input$yrs) # subset data
    plot(as.numeric(data[ ,2]), 
         as.numeric(data[ ,3]),
         type = "p",
         main = "Revenue Percent vs. Top Marginal Rate",
         xlab = "Top Marginal Rate",
         ylab = "Revenue Percentage of GDP",
         col = "forestgreen") + grid()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

