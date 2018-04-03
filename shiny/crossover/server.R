library(shiny)

library(quantmod)
library(data.table)
library(adjustr)

shinyServer( function(input, output) {

  all_data <- reactive({
    input$get_data
#     x <- getDTSymbols(input$symbol,
#                  from = input$date_range[1],
#                  to = input$date_range[2])
    symbol <- isolate(input$symbol)
    y <- getDTSymbols(symbol)
#     splits <- getSplits(symbol)
#     dividends <- getDividends(symbol)
#     y <- make_raw_value(x, dividends, splits = splits)
    y[, raw_slow_ema := EMA(as.matrix(rawvalue), 100)]
    y[, raw_fast_ema := EMA(as.matrix(rawvalue), 20)]
    y[, adjusted_slow_ema := EMA(as.matrix(adjusted), 100)]
    y[, adjusted_fast_ema := EMA(as.matrix(adjusted), 20)]
#     y[, close_return := ( close - first(close) ) / first(close) ]
#     y[, raw_return := make_raw_return(close, rawshares, rawdividend) ]
#     y[, reinvested_return := make_reinvested_return(close, rawshares, rawdividend)]
    setkey(y, symbol, index)
    y
  })

  range_data <- reactive({
    all_data()[index >= input$date_range[1] &
                 index <= input$date_range[2],]
  })

  output$return_values <- renderDataTable( range_data() )

  output$raw_signal_plot <- renderPlot({
    plot_data <- copy(range_data())
    if( ! is.null( plot_data ) && nrow( plot_data ) > 0 ) {
      zoo_data <- as.zoo( as.xts(plot_data[,list(index,
                                                  rawvalue,
                                                  raw_slow_ema,
                                                  raw_fast_ema)]) )
      ncols <- 3
      all_values <- as.numeric(zoo_data)
      tsRainbow <- rainbow( ncols )
      plot( x = zoo_data,
            ylab = "Value",
            main = "Raw Value with EMA",
            col = tsRainbow,
            screens = 1)
      legend( x = "topleft", legend = colnames(zoo_data),
              lty = 1, col = tsRainbow)
    }
  })

output$adjusted_signal_plot <- renderPlot({
  plot_data <- copy(range_data())
  if( ! is.null( plot_data ) && nrow( plot_data ) > 0 ) {
    zoo_data <- as.zoo( as.xts(plot_data[,list(index,
                                                  adjusted,
                                                  adjusted_slow_ema,
                                                  adjusted_fast_ema)]) )
    ncols <- 3
    all_values <- as.numeric(zoo_data)
    tsRainbow <- rainbow( ncols )
    plot( x = zoo_data,
          ylab = "Value",
          main = "Adjusted Value with EMA",
          col = tsRainbow,
          screens = 1)
    legend( x = "topleft", legend = colnames(zoo_data),
            lty = 1, col = tsRainbow)
  }
})

})
