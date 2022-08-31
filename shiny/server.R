library(shiny)

library(quantmod)
library(data.table)
library(adjustr)

shinyServer( function(input, output) {
  
  price_data <- reactive({
    x <- getDTSymbols(input$symbol, 
                 from = input$date_range[1], 
                 to = input$date_range[2])
    splits <- getSplits(input$symbol)
    dividends <- getDividends(input$symbol)
    y <- make_raw_value(x, dividends, splits = splits)
    y[, close_return := ( close - first(close) ) / first(close) ]
    y[, adjusted_return := ( adjusted - first(adjusted) ) / first(adjusted)]
    y[, raw_return := make_raw_return(close, rawshares, rawdividend) ]
    y[, reinvested_return := make_reinvested_return(close, rawshares, rawdividend)]
    y[, raw_minus_adjusted := raw_return - adjusted_return]
    y[, reinvested_minus_adjusted := reinvested_return - adjusted_return]
    setkey(y, symbol, index)
    y
  })
  
  output$return_values <- renderDataTable( price_data() )
  
  output$return_plot <- renderPlot({
    if( ! is.null(price_data() ) && nrow(price_data() ) > 0 ) {
      zoo_data <- as.zoo( as.xts(price_data()[,list(index, 
                                                  adjusted_return,
                                                  raw_return,
                                                  reinvested_return)]) )
      ncols <- 3
      all_values <- as.numeric(zoo_data)
      tsRainbow <- rainbow( ncols )
      plot( x = zoo_data, 
            ylab = "Cumulative Return", 
            main = "Cumulative Return Comparison",
            col = tsRainbow,
            screens = 1,
            yaxt = "n")
      axis(2, at = pretty(all_values), lab = paste0(pretty(all_values)*100, "%") )
      legend( x = "topleft", legend = colnames(zoo_data),
              lty = 1, col = tsRainbow)
    }
  })
  
output$return_diff_plot <- renderPlot({
  if( ! is.null(price_data() ) && nrow(price_data() ) > 0 ) {
    zoo_data <- as.zoo( as.xts(price_data()[,list(index, 
                                                  raw_minus_adjusted,
                                                  reinvested_minus_adjusted)]) )
    ncols <- 2
    all_values <- as.numeric(zoo_data)
    tsRainbow <- rainbow( ncols )
    plot( x = zoo_data, 
          ylab = "Cumulative Return Difference", 
          main = "Cumulative Return Difference Comparison",
          col = tsRainbow,
          screens = 1,
          yaxt = "n")
    axis(2, at = pretty(all_values), lab = paste(pretty(all_values)*100, "%") )
    legend( x = "topleft", legend = colnames(zoo_data),
            lty = 1, col = tsRainbow)
  }
})

})