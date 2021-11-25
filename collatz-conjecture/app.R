library(plotly) 
library(shiny)
library(gmp)

# Define UI for application that plots a time series
ui <- fluidPage(
  
  # Application title
  titlePanel("Collatz Conjecture"),
  
  # Sidebar with a numeric input for number of bins 
  sidebarLayout(
    sidebarPanel(
      numericInput("bins", 
                   "Initial value:", 
                   989345275647, 
                   min = 4, 
                   max = 2^64), # Maximum accurate modulus value
      verbatimTextOutput("value"),
      downloadButton('download',
                     "Download the data")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotlyOutput("distPlot")
    )
  )
)

# Define server logic
server <- function(input, output) {
  collatz = function(n){
    path = vector() 
    i = 1 
    while(n > 1){
      if(mod.bigz(n, 2) == 1){
        n = 3 * n + 1
        path[[i]] = n
      } else {
        n = n/2 
        path[[i]] = n
      }
      i = i + 1
    }
    return(path)
  }
  
  collatz_data <- reactive(cbind.data.frame(1:length(collatz(input$bins)), 
                                            collatz(input$bins)))
  
  output$distPlot <- renderPlotly({
    
    collatz_data = collatz_data()
    colnames(collatz_data) = c("step", "value")
    
    fig <- plot_ly(collatz_data, 
                   type = 'scatter', 
                   mode = 'lines') %>%
      add_trace(x = ~step, 
                y = ~value, 
                fill = 'tozeroy') %>%
      layout(showlegend = F, title = list(xanchor = "left", 
                                          x = 0.1, 
                                          text = paste(format(input$bins, big.mark = ","))))
    
    fig <- fig %>%
      layout(
        xaxis = list(zerolinecolor = '#ffff',
                     zerolinewidth = 2,
                     gridcolor = 'ffff'),
        yaxis = list(zerolinecolor = '#ffff',
                     zerolinewidth = 2,
                     gridcolor = 'ffff'),
        plot_bgcolor='#e5ecf6') %>%
      config(displaylogo = FALSE)  %>%
      config(modeBarButtonsToRemove = c("zoomIn2d", 
                                        "zoomOut2d",
                                        "zoom2d",
                                        "autoScale2d", 
                                        "pan2d", 
                                        "hoverClosest3d",
                                        "resetScale2d",
                                        "hoverClosestCartesian")) # hoverCompareCartesian
    
    fig
    
  })
  
  output$download <- downloadHandler(
    filename = function(){paste0("collatz_conj_", input$bins,".csv")}, 
    content = function(fname){
      data = collatz_data()
      colnames(data) = c("step", "value")
      write.csv(data, fname, row.names=FALSE)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
