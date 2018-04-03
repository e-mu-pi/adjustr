library(shiny)
library(lubridate)

shinyUI( fluidPage(
  titlePanel("Return Type Comparison"),

  sidebarLayout(
    sidebarPanel(
      dateRangeInput("date_range",
                     label = "Date range",
                     start = as.Date(Sys.Date()- years(2) ),
                     end = as.Date(Sys.Date()) ),
      selectizeInput("symbol",
                     label = "Stock symbol",
                     choices = list("SPY", "GLD", "UUP", "QQQ",
                                    "IWM", "EEM", "EFA", "IYR",
                                    "USO", "TLT"),
                     options = list(create = TRUE) ),
      actionButton("get_data", "Get Data")
      ),
    mainPanel(
      plotOutput("raw_signal_plot"),
      plotOutput("adjusted_signal_plot"),
      dataTableOutput("return_values")
      )
    )
  )
)
