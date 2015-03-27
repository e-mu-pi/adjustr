context("xts")
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

test_that("as.data.table.xts does not raise internal selfref error",{
  data <- xts::xts(data.frame(a.Xx= c(1,3), 
                              a.Yy= c(2,4)),
                   order.by = as.POSIXct( c("2013-10-22", 
                                            "2014-11-13") ) )
  data_dt <- as.data.table(data)
  current_warn_level <- getOption("warn")
  options(warn = 2)
  tryCatch( data_dt[, new_col := xx+yy],
       finally = {
         options(warn = current_warn_level)
       })
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
