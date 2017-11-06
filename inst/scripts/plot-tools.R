#' @import data.table
NULL

#' Plot symbol value with moving averages.
#'
#' @examples
#' \dontrun{
#' plot_with_ma("AAPL", "2007-01-01")
#' }
#' @export
plot_with_ma <- function(symbol, data_start, plot_start = Sys.Date()-lubridate::years(2), plot_end = Sys.Date(), fast_ma = 20, slow_ma = 100) {
  y <- adjustr::getDTSymbols(symbol, from = data_start)
#   splits <- quantmod::getSplits(symbol, from = data_start)
#   dividends <- quantmod::getDividends(symbol, from = data_start)
#   y <- make_raw_value(x, dividends, splits = splits)
  y[, raw_slow_ema := TTR::EMA(as.matrix(rawvalue), slow_ma)]
  y[, raw_fast_ema := TTR::EMA(as.matrix(rawvalue), fast_ma)]
  y[, adjusted_slow_ema := TTR::EMA(as.matrix(Adjusted), slow_ma)]
  y[, adjusted_fast_ema := TTR::EMA(as.matrix(Adjusted), fast_ma)]
  #     y[, close_return := ( close - first(close) ) / first(close) ]
  #     y[, raw_return := make_raw_return(close, rawshares, rawdividend) ]
  #     y[, reinvested_return := make_reinvested_return(close, rawshares, rawdividend)]
  setkey(y, symbol, index)

  z <- y[ index >= as.Date(plot_start) & index <= as.Date(plot_end),]

  value_type <- "adjusted"
  ylab <- switch(value_type,
                 adjusted = "Adjusted Value",
                 raw = "Raw Value")
  zoo_data <- switch(value_type,
                     adjusted = as.zoo( as.xts(z[,list(index,
                                                  Adjusted,
                                                  adjusted_slow_ema,
                                                  adjusted_fast_ema)]) ),
                     raw = as.zoo( as.xts(z[,list(index,
                                                   rawvalue,
                                                   raw_slow_ema,
                                                   raw_fast_ema)]) ) )

#   zoo_data <- as.zoo( as.xts(z[,list(index,
#                                              rawvalue,
#                                              raw_slow_ema,
#                                              raw_fast_ema)]) )

#   zoo_data <- as.zoo( as.xts(z[,list(index,
#                                              adjusted,
#                                              adjusted_slow_ema,
#                                              adjusted_fast_ema)]) )
  ncols <- 3
  all_values <- as.numeric(zoo_data)
  tsRainbow <- rainbow( ncols )
  plot( x = zoo_data,
        ylab = ylab,
        xlab = "Date",
        main = symbol,
        col = tsRainbow,
        screens = 1)
#   text( x = last(index(zoo_data)),
#         y = last(zoo_data),
#         labels = paste(names(zoo_data), last(zoo_data)))
  legend( x = "topleft", legend = colnames(zoo_data),
          lty = 1, col = tsRainbow)

# molten_data <- melt(z, c("index"), c("rawvalue", "raw_slow_ema", "raw_fast_ema"))
# qplot(index, value, data = molten_data, colour = molten_data[,variable], geom = "step")


  invisible(z)
}

#' Load symbol value with moving averages.
#'
#' @examples
#' \dontrun{
#' load_with_ma("AAPL", "2007-01-01")
#' }
#' @export
load_with_ma <- function(symbol, data_start, plot_start = Sys.Date()-lubridate::years(2),
												 plot_end = Sys.Date(), fast_ma = 20, slow_ma = 100) {
	y <- adjustr::getDTSymbols(symbol, from = data_start)
	raw_slow_ema <- data.table( symbol=symbol,
															Date = y[,index],
		                          value = TTR::EMA(as.matrix(y[,rawvalue]), slow_ma),
															price_type = 'raw',
															window = slow_ma)
	raw_fast_ema <- data.table( symbol=symbol,
															Date = y[,index],
															value = TTR::EMA(as.matrix(y[,rawvalue]), fast_ma),
															price_type = 'raw',
															window = fast_ma)
	adjusted_slow_ema <- data.table( symbol=symbol,
																	 Date = y[,index],
																	 value = TTR::EMA(as.matrix(y[,Adjusted]), slow_ma),
																	 price_type = 'adjusted',
																	 window = slow_ma)
	adjusted_fast_ema <- data.table( symbol=symbol,
																	 Date = y[,index],
																	 value = TTR::EMA(as.matrix(y[,Adjusted]), fast_ma),
																	 price_type = 'adjusted',
																	 window = fast_ma)
	raw <- data.table( symbol=symbol,
										 Date = y[,index],
										 value = y[,rawvalue], price_type = 'raw', window=NA)
	adjusted <- data.table( symbol=symbol,
													Date = y[,index],
													value=y[,Adjusted], price_type='adjusted', window=NA)
	tall_data <- rbind( raw, adjusted, raw_slow_ema, raw_fast_ema, adjusted_fast_ema, adjusted_slow_ema)
	setkey(tall_data, symbol, Date)

	tall_data[ Date >= as.Date(plot_start) & Date <= as.Date(plot_end),]
}
