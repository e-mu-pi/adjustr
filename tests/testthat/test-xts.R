context("xts")
library(xts)
library(data.table)

make_data_table <- function(string_table) {
  data <- read.table(text = string_table, header = TRUE, stringsAsFactors = FALSE)
  #   data$Date <- data.table::as.IDate(data$Date)
  data$index <- as.POSIXct(data$index)
  attr(data$index, "tclass") <- attr(data$index, "class")
  setDT(data)
}

# as.xts.data.table is now part of data.table
# Tests updated to reflect the difference:
#   old version (adjustr) used column named index, POSIXct
#   new version (data.table) uses first column, a POSIXct or Date
test_that("as.xts.data.table transforms data.table with index to xts",{
  data <- make_data_table("index xx yy
                          2013-10-22 1 2
                          2014-11-13 3 4")
  expect_that( as.xts(data),
               equals( xts(data.frame(xx= c(1,3), yy= c(2,4)),
                                order.by = as.POSIXct( c("2013-10-22", 
                                                         "2014-11-13") ) ) ) )
  expect_that( as.data.table( as.xts(data) ),
               equals( data ) )
})

test_that("as.xts.data.table uses symbol column in column names with merge",{
  data <- make_data_table("index symbol xx yy
                          2013-10-22 SYM 1 2
                          2014-11-13 SYM 3 4")
  pre_xts <- spread_symbol(data)
  expect_that( as.xts(pre_xts),
               equals( xts(data.frame(SYM.xx= c(1,3), SYM.yy= c(2,4)),
                                order.by = as.POSIXct( c("2013-10-22", 
                                                         "2014-11-13") ) ) ) )
  expect_that( as.data.table( as.xts(pre_xts) ),
               equals( pre_xts ) )
  data2 <- rbind( data, make_data_table("index xx yy  symbol
                                                      2013-10-22 5 6 SYM2
                                                      2014-11-13 7 8 SYM2") )
  pre_xts2 <- spread_symbol(data2)
  expect_that( as.xts(pre_xts2),
               equals( xts( data.frame(SYM.xx = c(1,3), SYM.yy = c(2,4),
                                            SYM2.xx = c(5,7), SYM2.yy = c(6,8)), 
                                 order.by = as.POSIXct( c("2013-10-22",
                                                          "2014-11-13") ) ) ) )
  expect_that( as.data.table( as.xts( pre_xts2) ),
               equals( pre_xts2 ) )
  data3 <- rbind( data2, make_data_table("index xx yy symbol
                                         2015-01-01 9 10 SYM2"))
  pre_xts3 <- spread_symbol(data3)
  expect_that( as.xts(pre_xts3),
               equals( xts( data.frame(SYM.xx = c(1,3, NA), SYM.yy = c(2,4, NA),
                                            SYM2.xx = c(5,7,9), SYM2.yy = c(6,8, 10)), 
                                 order.by = as.POSIXct( c("2013-10-22",
                                                          "2014-11-13",
                                                          "2015-01-01") ) ) ) )
  expect_that( as.data.table( as.xts(pre_xts3) ),
               equals( pre_xts3 ) )
})

test_that("as.data.table.xts transforms xts to data.table with index",{
  data <- xts(data.frame(Xx= c(1,3), 
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

test_that("as.data.table.xts no longer extracts symbol column",{
  data <- xts(data.frame(a.xx= c(1,3), 
                              a.yy= c(2,4),
                              b.yy = c(5,7),
                              b.zz = c(6,8)),
                   order.by = as.POSIXct( c("2013-10-22", 
                                            "2014-11-13") ) )
  expect_that( as.data.table(data),
               equals( make_data_table("index    a.xx a.yy b.yy b.zz
                                       2013-10-22  1   2    5    6
                                       2014-11-13  3   4    7    8") ) )
  expect_that( gather_symbol(as.data.table(data)),
               equals( make_data_table("index symbol xx yy zz
                                       2013-10-22 a 1 2 NA
                                       2014-11-13 a 3 4 NA
                                       2013-10-22 b NA 5 6
                                       2014-11-13 b NA 7 8") ) )
  expect_that( as.xts( as.data.table(data) ),
               equals( data) )
  expect_that( as.xts( spread_symbol(gather_symbol(as.data.table(data) ) ) ),
               equals( data ) )
})

test_that("as.data.table.xts does not raise internal selfref error",{
  data <- xts(data.frame(a.xx= c(1,3), 
                              a.yy= c(2,4)),
                   order.by = as.POSIXct( c("2013-10-22", 
                                            "2014-11-13") ) )
  data_dt <- gather_symbol(as.data.table(data))
  current_warn_level <- getOption("warn")
  options(warn = 2)
  tryCatch( data_dt[, new_col := xx+yy],
       finally = {
         options(warn = current_warn_level)
       })
})

test_that("getDTSymbols returns getSymbols as data.table with splits and dividends",{
  symbol <- "AAPL"
  actual <- getDTSymbols(symbol)
  
  getSymbols <- quantmod::getSymbols # getSymbols doesn't expect to see the 
  # package name when it retrieves its
  # defaults (gives a warning)
  # Do this rather than attaching quantmod.
  price <- getSymbols(symbol)
  splits <- quantmod::getSplits(symbol)
  dividends <- quantmod::getDividends(symbol)
  raw <- make_raw_value(price, splits, dividends)
  expected <- as.data.table( raw )
  
  expect_that( actual,
               equals( expected ) )
  
  alpha_order <- c("AAPL.Adjusted", "AAPL.Close", 
                   "AAPL.dividend",
                   "AAPL.High", "AAPL.Low", "AAPL.Open", 
                   "AAPL.rawdividend", "AAPL.rawshares", "AAPL.rawvalue",
                   "AAPL.split", "AAPL.Volume")
  expect_that( as.xts(actual), 
               equals(raw[,alpha_order], check.attributes = FALSE) )
  # can't seem to pass attributes using as.xts
  #   expect_that( as.xts(actual, xtsAttributes(raw)), 
  #              equals(raw[,alpha_order]) )
})
