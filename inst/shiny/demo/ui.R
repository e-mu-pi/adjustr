library(shiny)
library(lubridate)

shinyUI( fluidPage(
  titlePanel("Return Type Comparison"),
  
  sidebarLayout(
    sidebarPanel(
      dateRangeInput("date_range",
                     label = "Date range",
                     start = as.Date(Sys.Date()- years(10) ),
                     end = as.Date(Sys.Date()) ),
      selectizeInput("symbol",
                     label = "Stock symbol",
                     choices = list("SPY"),
                     options = list(create = TRUE) ),
      submitButton("Get Data")
      ),
    mainPanel(
      plotOutput("return_plot"),
      plotOutput("return_diff_plot"),
      dataTableOutput("return_values")
      )
    )
  )
)