## app.R ##
library(shinydashboard)
library(shiny)
library(purrr)
library(ggplot2)
library(fredr)
library(RPostgres)

source(".pg_user")
source(".pg_pwd")
source(".db_name")
source(".pg_host")
source(".pg_db")

con <- dbConnect(RPostgres::Postgres(), 
                 user = pg_user, 
                 password = pg_pwd,
                 host = pg_host,
                 dbname = pg_db)

series_list = dbGetQuery(con, "SELECT series FROM table_descriptions")

ui <- dashboardPage(
  dashboardHeader(title = "FRED Data Browser"),
  dashboardSidebar(
    selectInput("select1", "Select a series", 
                choices = series_list, selected = tolower("FYFRGDA188S")),
    selectInput("select2", "Select a series", 
                choices = series_list, selected = tolower("IITTRHB"))
  ),
  dashboardBody(
    fluidRow(
      box(plotOutput("plot1", height = 250)
      )
    )
  )
)

server <- function(input, output) {
  output$plot1 <- renderPlot({
    series1 = dbGetQuery(con, paste("SELECT date, value, series_id FROM ", input$select1, ";")) # gdpca 
    series2 = dbGetQuery(con, paste("SELECT date, value, Series_id FROM ", input$select2, ";")) # gdp
    rbind.data.frame(series1, series2) %>%
      ggplot(data = ., mapping = aes(x = date, y = value, color = series_id)) +
      geom_line() +
      labs(x = "Observation Date", y = "Rate", color = "Series")
  })
}

shinyApp(ui, server)

