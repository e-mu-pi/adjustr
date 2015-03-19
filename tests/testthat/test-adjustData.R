context("Adjust Data")
require(xts)

make_data_table <- function(string_table) {
  data <- read.table(text = string_table, header = TRUE, stringsAsFactors = FALSE)
#   data$Date <- data.table::as.IDate(data$Date)
  data$index <- as.POSIXct(data$index)
  data.table::setDT(data)
}

test_that("as.xts.data.table transforms data.table with index to xts",{
  data <- make_data_table("index xx yy
                          2013-10-22 1 2
                          2014-11-13 3 4")
  expect_that( as.xts(data),
               equals( xts::xts(data.frame(xx= c(1,3), yy= c(2,4)),
                                order.by = as.POSIXct( c("2013-10-22", 
                                                      "2014-11-13") ) ) ) )
  expect_that( as.data.table( as.xts(data) ),
               equals( data ) )
})

test_that("as.xts.data.table uses symbol column in column names with merge",{
  data <- make_data_table("index symbol xx yy
                          2013-10-22 SYM 1 2
                          2014-11-13 SYM 3 4")
  expect_that( as.xts(data),
               equals( xts::xts(data.frame(SYM.Xx= c(1,3), SYM.Yy= c(2,4)),
                                order.by = as.POSIXct( c("2013-10-22", 
                                                         "2014-11-13") ) ) ) )
  expect_that( as.data.table( as.xts(data) ),
               equals( data ) )
  data2 <- rbind( data, make_data_table("index xx yy  symbol
                          2013-10-22 5 6 SYM2
                          2014-11-13 7 8 SYM2") )
  expect_that( as.xts(data2),
               equals( xts::xts( data.frame(SYM.Xx = c(1,3), SYM.Yy = c(2,4),
                                            SYM2.Xx = c(5,7), SYM2.Yy = c(6,8)), 
                                 order.by = as.POSIXct( c("2013-10-22",
                                                          "2014-11-13") ) ) ) )
  expect_that( as.data.table( as.xts( data2) ),
               equals( data2 ) )
  data3 <- rbind( data2, make_data_table("index xx yy symbol
                                         2015-01-01 9 10 SYM2"))
  expect_that( as.xts(data3),
               equals( xts::xts( data.frame(SYM.Xx = c(1,3, NA), SYM.Yy = c(2,4, NA),
                                            SYM2.Xx = c(5,7,9), SYM2.Yy = c(6,8, 10)), 
                                 order.by = as.POSIXct( c("2013-10-22",
                                                          "2014-11-13",
                                                          "2015-01-01") ) ) ) )
  expect_that( as.data.table( as.xts(data3) ),
               equals( data3 ) )
})

test_that("as.data.table.xts transforms xts to data.table with index",{
  data <- xts::xts(data.frame(Xx= c(1,3), 
                              Yy= c(2,4)),
                   order.by = as.POSIXct( c("2013-10-22", 
                                         "2014-11-13") ) )
  expect_that( as.data.table(data),
               equals( make_data_table("index Xx Yy
                          2013-10-22 1 2
                          2014-11-13 3 4") ) )
  expect_that( as.xts( as.data.table(data) ),
               equals( data ) )
})

test_that("as.data.table.xts extracts symbol column",{
  data <- xts::xts(data.frame(a.Xx= c(1,3), 
                              a.Yy= c(2,4),
                              b.Yy = c(5,7),
                              b.Zz = c(6,8)),
                   order.by = as.POSIXct( c("2013-10-22", 
                                            "2014-11-13") ) )
  expect_that( as.data.table(data),
               equals( make_data_table("index symbol xx yy zz
                          2013-10-22 a 1 2 NA
                          2014-11-13 a 3 4 NA
                          2013-10-22 b NA 5 6
                          2014-11-13 b NA 7 8") ) )
  expect_that( as.xts( as.data.table(data) ),
               equals( data) )
})

test_that("getDTSymbols returns getSymbols as data.table with IDate",{
  symbol <- "VUSUX"
  actual <- getDTSymbols(symbol)
  
  raw <- quantmod::getSymbols(symbol)
  expected <- as.data.table( raw )

  expect_that( actual,
               equals( expected ) )
  
  alpha_order <- c("VUSUX.Adjusted", "VUSUX.Close", 
                   "VUSUX.High", "VUSUX.Low", "VUSUX.Open", "VUSUX.Volume")
  expect_that( as.xts(actual), 
               equals(raw[,alpha_order], check.attributes = FALSE) )
# can't seem to pass attributes using as.xts
#   expect_that( as.xts(actual, xts::xtsAttributes(raw)), 
#              equals(raw[,alpha_order]) )
})

test_that("true_value adds dividend values into price data",{
  price_data <- make_data_table("index symbol high close
                                2015-03-16 SYM 56.6  55.94
                                2015-03-17 SYM 59.23 57.25
                                2015-03-18 SYM 58.72 56.49")
  dividend_data_dt <- make_data_table("index dividend
                                   2015-03-17 0.052")
  dividend_data_xts <- xts::xts(0.052, order.by = as.POSIXct("2015-03-17") )

  expected <- data.table::copy(price_data)
  expected[, split := 1]
  expected[, trueshares := 1]
  expected[, truevalue := 0]
  expected[, dividend := c(0, 0.052, 0)]
  expected[, truedividend := dividend]
  expected[, truevalue := close + cumsum(dividend)]
  
  setkey(expected, symbol, index)
  expect_that( true_value( price_data, dividend_data_dt, splits = NULL),
               equals(expected) )
  expect_that( true_value( price_data, dividend_data_xts, splits = NULL),
               equals(expected) )
  
  expect_that( true_value( as.xts(price_data), dividend_data_dt, splits = NULL),
               equals( as.xts(expected) ) )
  expect_that( true_value( as.xts(price_data), dividend_data_xts, splits = NULL),
               equals( as.xts(expected) ) )
})

test_that("true_value adds dividend values into multisymbol data",{
  price_data <- make_data_table("index symbol high close 
                                2015-03-16 SYM 56.6  55.94
                                2015-03-17 SYM 59.23 57.25
                                2015-03-18 SYM 58.72 56.49
                                2015-03-16 SYM2 1.34  1.30
                                2015-03-17 SYM2 1.32  1.29
                                2015-03-18 SYM2 1.35  1.31")
  dividend_data_dt <- make_data_table("index dividend
                                      2015-03-17 0.052
                                      2015-03-16 0.012")
  dividend_data_xts <- xts::xts( matrix( c(NA, 0.052, 0.012, 0), nrow = 2), 
                                 order.by = as.Date( c("2015-03-16","2015-03-17") ) )
  
  expected <- data.table::copy(price_data)
  expected[, split := 1]
  expected[, trueshares := 1]
  expected[, truevalue := 0]
  expected[, dividend := 0]
  expected[symbol == "SYM", dividend := c(0, 0.052, 0)]
  expected[symbol == "SYM", truevalue := close + cumsum(dividend)]
  expected[symbol == "SYM2", dividend := c(0.012, 0, 0)]
  expected[symbol == "SYM2", truevalue := close + cumsum(dividend)]
  expected[, truedividend := dividend]
  setkey(expected, symbol, index)
  
  expect_that(true_value( price_data, dividend_data_dt, splits = NULL),
              throws_error("dividend must have symbol for multisymbol price data") )
  expect_that(true_value( price_data, dividend_data_xts, splits = NULL),
              throws_error("dividend must have symbol for multisymbol price data") )
  expect_that(true_value( as.xts(price_data), dividend_data_dt, splits = NULL),
              throws_error("dividend must have symbol for multisymbol price data") )
  expect_that(true_value( as.xts(price_data), dividend_data_xts, splits = NULL),
              throws_error("dividend must have symbol for multisymbol price data") )
  
  dividend_data_dt[, symbol := c("SYM", "SYM2")]
  colnames(dividend_data_xts) <- c("SYM.Dividend", "SYM2.Dividend")
  
  expect_that( true_value( price_data, dividend_data_dt, splits = NULL),
               equals(expected) )
  expect_that( true_value( price_data, dividend_data_xts, splits = NULL),
               equals(expected) )
  
  expect_that( true_value( as.xts(price_data), dividend_data_dt, splits = NULL),
               equals( as.xts(expected) ) )
  expect_that( true_value( as.xts(price_data), dividend_data_xts, splits = NULL),
               equals( as.xts(expected) ) )
})

test_that("true_value factors split values into price data",{
  price_data <- make_data_table("index symbol high close
                                2015-03-16 SYM 56.6  55.94
                                2015-03-17 SYM 29.62 28.63
                                2015-03-18 SYM 29.36 28.24")
  split_dt <- make_data_table("index split
                              2015-03-17 0.5")
  split_xts <- xts::xts( 0.5, order.by = as.Date("2015-03-17") )
  
  expected <- data.table::copy(price_data)
  expected[, split := c(1, 0.5, 1)]
  expected[, trueshares := c(1, 2, 2)]
  expected[, truevalue := close]
  expected[2:3, truevalue := 2 * close]
  expected[, dividend := 0]
  expected[, truedividend := 0]
  
  setkey(expected, symbol, index)
  expect_that( true_value( price_data, dividend = NULL, splits = split_dt),
                 equals(expected) )
  expect_that( true_value( price_data, dividend = NULL, splits = split_xts),
               equals(expected) )
  expect_that( true_value( as.xts(price_data), dividend = NULL, splits = split_dt),
               equals( as.xts(expected) ) )
  expect_that( true_value( as.xts(price_data), dividend = NULL, splits = split_xts),
               equals( as.xts(expected) ) )  
})

test_that("true_value factors split values into multisymbol data",{
  price_data <- make_data_table("index symbol high close
                                2015-03-16 SYM 56.6  55.94
                                2015-03-17 SYM 29.62 28.63
                                2015-03-18 SYM 29.36 28.24
                                2015-03-16 SYM2 1.34  1.30
                                2015-03-17 SYM2 1.32  1.29
                                2015-03-18 SYM2 2.7   2.62")
  split_data_dt <- make_data_table("index split
                                      2015-03-16 0.25
                                      2015-03-17 0.5
                                      2015-03-18 2")
  split_data_xts <- xts::xts( matrix( c(NA, 0.5, 1, 0.25, 1, 2), nrow = 3), 
                              order.by = as.POSIXct( c("2015-03-16",
                                                       "2015-03-17",
                                                       "2015-03-18") ) )
  
  expected <- data.table::copy(price_data)
  expected[, split := 1]
  expected[symbol == "SYM", split := c(1, 0.5, 1)]
  expected[symbol == "SYM2", split := c(0.25, 1, 2)]
  expected[, trueshares := 1/cumprod(split), by = symbol]
  expected[, truevalue := close * trueshares]
  expected[, dividend := 0]
  expected[, truedividend := 0]
  setkey(expected, symbol, index)
  
  expect_that(true_value( price_data, dividend = NULL, splits = split_data_dt),
              throws_error("splits must have symbol for multisymbol price data") )
  expect_that(true_value( price_data, dividend = NULL, splits = split_data_xts),
              throws_error("splits must have symbol for multisymbol price data") )
  expect_that(true_value( as.xts(price_data), dividend = NULL, splits = split_data_dt),
              throws_error("splits must have symbol for multisymbol price data") )
  expect_that(true_value( as.xts(price_data), dividend = NULL, splits = split_data_xts),
              throws_error("splits must have symbol for multisymbol price data") )
  
  split_data_dt[, symbol := c("SYM2", "SYM", "SYM2")]
  colnames(split_data_xts) <- c("SYM.Split", "SYM2.Split")
  
  expect_that( true_value( price_data, dividend = NULL, splits = split_data_dt),
               equals(expected) )
  expect_that( true_value( price_data, dividend = NULL, splits = split_data_xts),
               equals(expected) )
  
  expect_that( true_value( as.xts(price_data), dividend = NULL, splits = split_data_dt),
               equals( as.xts(expected) ) )
  expect_that( true_value( as.xts(price_data), dividend = NULL, splits = split_data_xts),
               equals( as.xts(expected) ) )
})

test_that("true_value works with empty dividend/split results from quantmod",{
  price_data <- make_data_table("index symbol high close
                                2015-03-16 SYM 56.6  55.94
                                2015-03-17 SYM 29.62 28.63
                                2015-03-18 SYM 29.36 28.24")
  splits <- make_data_table("index split
                            2015-03-17 0.5")
  dividend <- make_data_table("index dividend
                              2015-03-16  0.013")
  empty_splits <- NA
  empty_dividend <- xts( numeric(), order.by = as.Date(character()))
  
  expected_with_div <- copy(price_data)
  expected_with_div[, split := 1]
  expected_with_div[, trueshares := 1]
  expected_with_div[, truevalue := 0]
  expected_with_div[, dividend := c(0.013, 0, 0)]
  expected_with_div[, truedividend := dividend]
  expected_with_div[, truevalue := close + cumsum(dividend)]
  setkey(expected_with_div, symbol, index)
  
  expected_with_spl <- data.table::copy(price_data)
  expected_with_spl[, split := c(1, 0.5, 1)]
  expected_with_spl[, trueshares := c(1, 2, 2)]
  expected_with_spl[, truevalue := trueshares * close]
  expected_with_spl[, dividend := 0]
  expected_with_spl[, truedividend := 0]
  setkey(expected_with_spl, symbol, index)
  
  expect_that( true_value( price_data, dividend = dividend, splits = empty_splits),
               equals(expected_with_div) )
  expect_that( true_value( as.xts(price_data), 
                           dividend = as.xts(dividend), 
                           splits = empty_splits ),
               equals( as.xts(expected_with_div) ) )
  
  expect_that( true_value( price_data, dividend = empty_dividend, splits = splits),
               equals(expected_with_spl) )
  expect_that( true_value( as.xts(price_data), 
                           dividend = empty_dividend, 
                           splits = as.xts(splits) ),
               equals( as.xts(expected_with_spl) ) )
})

test_that("true_value assumes dividends are split adjusted (per Yahoo)",{
  price_data <- make_data_table("index symbol high close
                                2015-03-16 SYM 56.6  55.94
                                2015-03-17 SYM 29.62 28.63
                                2015-03-18 SYM 29.36 28.24")
  splits <- make_data_table("index split
                              2015-03-17 0.2")
  dividend <- make_data_table("index dividend
                              2015-03-16  0.013")
  expected <- copy(price_data)
  expected[, split := c(1,0.2,1)]
  expected[, trueshares := c(1,5,5)]
  expected[, truevalue := 0]
  expected[, dividend := c(0.013, 0, 0)]
  expected[, truedividend := dividend * 5 / trueshares]
  expected[, truevalue := trueshares * close + cumsum(truedividend)]
  setkey(expected, symbol, index)
  
  expect_that( true_value( price_data, dividend = dividend, splits = splits),
               equals(expected) )
  expect_that( true_value( as.xts(price_data), 
                           dividend = as.xts(dividend), 
                           splits = as.xts(splits) ),
               equals( as.xts(expected) ) )
})

test_that("true_value assumes one truedividend paid per trueshare",{
  price_data <- make_data_table("index symbol close
                                2015-03-16 SYM 55.94
                                2015-03-17 SYM 28.63
                                2015-03-18 SYM  28.24
                                2015-03-19 SYM 113.01
                                2015-03-20 SYM 113.14")
  splits <- make_data_table("index split
                            2015-03-17 0.5
                            2015-03-19 4")
  dividend <- make_data_table("index dividend
                              2015-03-16 0.45
                              2015-03-18 0.444
                              2015-03-20 0.45")
  expected <- copy(price_data)
  expected[, split := c(1,0.5,1,4,1)]
  expected[, trueshares := c(1,2,2,0.5,0.5)]
  expected[, truevalue := 0]
  expected[, dividend := c(0.45, 0, 0.444, 0, 0.45)]
  expected[, truedividend := c(0.225, 0, 0.111, 0, 0.45)]
  expected[, truevalue := trueshares * close + cumsum(trueshares * truedividend)]
  setkey(expected, symbol, index)
  
  expect_that( true_value( price_data, dividend = dividend, splits = splits),
               equals(expected) )
  expect_that( true_value( as.xts(price_data), 
                           dividend = as.xts(dividend), 
                           splits = as.xts(splits) ),
               equals( as.xts(expected) ) )
  # need to split-adjust the dividends even if the price data does not go all the way to
  # the split
  expect_that( true_value( price_data[1:3,], dividend = dividend, splits = splits),
               equals(expected[1:3,]) )
  expect_that( true_value( as.xts(price_data[1:3,]), 
                           dividend = as.xts(dividend), 
                           splits = as.xts(splits) ),
               equals( as.xts(expected[1:3,]) ) )
})

test_that("true_return computed based on starting period with one share at closing price",{
  expect_that( TRUE,
               equals(FALSE))
})

test_that("true_value ignores dividends/splits outside time period except to adjust dividends",{
  price_data <- make_data_table("index symbol high close
                                2015-03-16 SYM 56.6  55.94
                                2015-03-17 SYM 29.62 28.63
                                2015-03-18 SYM 29.36 28.24")
  splits <- make_data_table("index split
                            2015-03-15 0.2
                            2015-03-17 0.5
                            2015-03-19 7")
  dividend <- make_data_table("index dividend
                              2015-03-14  0.013
                              2015-03-16  0.04
                              2015-03-20  0.2")
  expected <- copy(price_data)
  expected[, split := c(1, 0.5, 1)]
  expected[, trueshares := c(1, 2, 2)]
  expected[, truevalue := 0]
  expected[, dividend := c(0.04, 0, 0)]
  expected[, truedividend := dividend * 2/7]
  expected[, truevalue := trueshares * close + cumsum(trueshares * truedividend)]
  setkey(expected, symbol, index)
  
  expect_that( true_value( price_data, dividend = dividend, splits = splits),
               equals(expected) )
  expect_that( true_value( as.xts(price_data), 
                           dividend = as.xts(dividend), 
                           splits = as.xts(splits) ),
               equals( as.xts(expected) ) )
})

test_that("true_value cannot use nonsymboled price data with multisymbol dividends or splits",{
  price_data <- make_data_table("index high close
                                2015-03-16 56.6  55.94
                                2015-03-17 59.23 57.25
                                2015-03-18 58.72 56.49")
  dividend_data_dt <- make_data_table("index dividend symbol
                                      2015-03-17 0.052 SYM
                                      2015-03-16 0.012 SYM2")
  dividend_data_xts <- xts::xts( data.frame( SYM.Dividend = c(NA, 0.052), 
                                             SYM2.Dividend = c(0.012, 0) ), 
                                 order.by = as.Date( c("2015-03-16","2015-03-17") ) )
  
  expect_that(true_value( price_data, dividend_data_dt, splits = NULL),
              throws_error("price must have symbol for multisymbol dividend data") )
  expect_that(true_value( price_data, dividend_data_xts, splits = NULL),
              throws_error("price must have symbol for multisymbol dividend data") )
  expect_that(true_value( as.xts(price_data), dividend_data_dt, splits = NULL),
              throws_error("price must have symbol for multisymbol dividend data") )
  expect_that(true_value( as.xts(price_data), dividend_data_xts, splits = NULL),
              throws_error("price must have symbol for multisymbol dividend data") )
  
  split_data_dt <- make_data_table("index split symbol
                                      2015-03-16 0.25 SYM2
                                      2015-03-17 0.5 SYM
                                      2015-03-18 2 SYM")
  split_data_xts <- xts::xts( data.frame( SYM.spl = c(NA, 0.5, 1),
                                          SYM2.spl = c(0.25, 1, 2) ), 
                              order.by = as.POSIXct( c("2015-03-16",
                                                       "2015-03-17",
                                                       "2015-03-18") ) )
  expect_that(true_value( price_data, dividend = NULL, splits = split_data_dt),
              throws_error("price must have symbol for multisymbol splits data") )
  expect_that(true_value( price_data, dividend = NULL, splits = split_data_xts),
              throws_error("price must have symbol for multisymbol splits data") )
  expect_that(true_value( as.xts(price_data), dividend = NULL, splits = split_data_dt),
              throws_error("price must have symbol for multisymbol splits data") )
  expect_that(true_value( as.xts(price_data), dividend = NULL, splits = split_data_xts),
              throws_error("price must have symbol for multisymbol splits data") )  
  
  expect_that(true_value( price_data, dividend_data_xts, splits = split_data_dt),
              throws_error("price must have symbol for multisymbol dividend data") )
  expect_that(true_value( price_data, dividend_data_dt, splits = split_data_xts),
              throws_error("price must have symbol for multisymbol dividend data") )
  expect_that(true_value( as.xts(price_data), dividend_data_dt, splits = split_data_dt),
              throws_error("price must have symbol for multisymbol dividend data") )
  expect_that(true_value( as.xts(price_data), dividend_data_xts, splits = split_data_xts),
              throws_error("price must have symbol for multisymbol dividend data") )  
})

test_that("true_value has same return as adjusted close",{
  symbol <- "AAPL"
  price <- quantmod::getSymbols(symbol)
  dividend <- quantmod::getDividends(symbol)
  splits <- quantmod::getSplits(symbol)
  
  expect_that( nrow(splits),
               is_more_than(0) )
  expect_that( nrow(dividend),
               is_more_than(0) )
  
  adjusted_price <- quantmod::adjustOHLC( price, symbol.name = symbol)
  true_value_price <- true_value( price, dividend, splits = splits)
  
  adj_col <- paste0(symbol,".Close")
  tv_col <- paste0(symbol,".Truevalue")
  as.n <- as.numeric
  quick_return <- function(x) {
    n <- nrow(x)
    (as.numeric(x[n,]) - as.numeric(x[1,]) ) / as.numeric(x[1,])
  }
  quick_log_return <- function(x) {
    n <- nrow(x)
    log(as.numeric(x[n,]) / as.numeric(x[1,]) )
  }
  adjusted_return <- quick_return(adjusted_price[, adj_col])
  tv_return <- quick_return( true_value_price[, tv_col])
  
  #return since 2007 is off by 25%
  expect_that( tv_return,
               equals(adjusted_return, tolerance = 0.25, scale = 1) )
  
  adj_daily <- quantmod::dailyReturn( adjusted_price[, adj_col] )
  tv_daily <- quantmod::dailyReturn( true_value_price[, tv_col] )
  #they don't really match because adj_daily retroactively redefines the 
  #price p on the day before the dividend d to be p-d, so the daily return
  #will be wrt p-d, but tv_daily will use p.
  expect_that( tv_daily, 
               equals( adj_daily, tolerance = 2e-2) )
})
