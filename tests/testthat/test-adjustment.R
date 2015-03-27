context("Adjustment")
require(xts) # I couldn't figure out how to get xts::as.xts working without attaching

make_data_table <- function(string_table) {
  data <- read.table(text = string_table, header = TRUE, stringsAsFactors = FALSE)
  #   data$Date <- data.table::as.IDate(data$Date)
  data$index <- as.POSIXct(data$index)
  data.table::setDT(data)
}

test_that("unadjust.default removes split adjustment from dividends",{
  unadjusted_dividends <- unadjust(dividend, splits)
  expect_that( TRUE,
               equals(FALSE) )
})

# test_that("unadjust.data.table removes split adjustment from dividends",{
#   unadjusted_data <- unadjust(dividend, splits)
#   #unadjusted_dividend, dividend, split, shares
#   expect_that( TRUE,
#                equals(FALSE) )
# })
# 
# test_that("unadjust.xts removes split adjustment from dividends",{
#   unadjusted_data <- unadjust(dividend, splits)
#   #unadjusted_dividend, dividend, split, shares
#   expect_that( TRUE,
#                equals(FALSE) )
# })

test_that("make_raw_value adds dividend values into price data",{
  price_data <- make_data_table("index symbol high close
                                2015-03-16 SYM 56.6  55.94
                                2015-03-17 SYM 59.23 57.25
                                2015-03-18 SYM 58.72 56.49")
  dividend_data_dt <- make_data_table("index dividend
                                   2015-03-17 0.052")
  dividend_data_xts <- xts::xts(0.052, order.by = as.POSIXct("2015-03-17") )

  expected <- data.table::copy(price_data)
  expected[, split := 1]
  expected[, rawshares := 1]
  expected[, rawvalue := 0]
  expected[, dividend := c(0, 0.052, 0)]
  expected[, rawdividend := dividend]
  expected[, rawvalue := close + cumsum(dividend)]
  
  setkey(expected, symbol, index)
  expect_that( make_raw_value( price_data, dividend_data_dt, splits = NULL),
               equals(expected) )
  expect_that( make_raw_value( price_data, dividend_data_xts, splits = NULL),
               equals(expected) )
  
  expect_that( make_raw_value( as.xts(price_data), dividend_data_dt, splits = NULL),
               equals( as.xts(expected) ) )
  expect_that( make_raw_value( as.xts(price_data), dividend_data_xts, splits = NULL),
               equals( as.xts(expected) ) )
})

test_that("make_raw_value adds dividend values into multisymbol data",{
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
  expected[, rawshares := 1]
  expected[, rawvalue := 0]
  expected[, dividend := 0]
  expected[symbol == "SYM", dividend := c(0, 0.052, 0)]
  expected[symbol == "SYM", rawvalue := close + cumsum(dividend)]
  expected[symbol == "SYM2", dividend := c(0.012, 0, 0)]
  expected[symbol == "SYM2", rawvalue := close + cumsum(dividend)]
  expected[, rawdividend := dividend]
  setkey(expected, symbol, index)
  
  expect_that(make_raw_value( price_data, dividend_data_dt, splits = NULL),
              throws_error("dividend must have symbol for multisymbol price data") )
  expect_that(make_raw_value( price_data, dividend_data_xts, splits = NULL),
              throws_error("dividend must have symbol for multisymbol price data") )
  expect_that(make_raw_value( as.xts(price_data), dividend_data_dt, splits = NULL),
              throws_error("dividend must have symbol for multisymbol price data") )
  expect_that(make_raw_value( as.xts(price_data), dividend_data_xts, splits = NULL),
              throws_error("dividend must have symbol for multisymbol price data") )
  
  dividend_data_dt[, symbol := c("SYM", "SYM2")]
  colnames(dividend_data_xts) <- c("SYM.Dividend", "SYM2.Dividend")
  
  expect_that( make_raw_value( price_data, dividend_data_dt, splits = NULL),
               equals(expected) )
  expect_that( make_raw_value( price_data, dividend_data_xts, splits = NULL),
               equals(expected) )
  
  expect_that( make_raw_value( as.xts(price_data), dividend_data_dt, splits = NULL),
               equals( as.xts(expected) ) )
  expect_that( make_raw_value( as.xts(price_data), dividend_data_xts, splits = NULL),
               equals( as.xts(expected) ) )
})

test_that("make_raw_value factors split values into price data",{
  price_data <- make_data_table("index symbol high close
                                2015-03-16 SYM 56.6  55.94
                                2015-03-17 SYM 29.62 28.63
                                2015-03-18 SYM 29.36 28.24")
  split_dt <- make_data_table("index split
                              2015-03-17 0.5")
  split_xts <- xts::xts( 0.5, order.by = as.Date("2015-03-17") )
  
  expected <- data.table::copy(price_data)
  expected[, split := c(1, 0.5, 1)]
  expected[, rawshares := c(1, 2, 2)]
  expected[, rawvalue := close]
  expected[2:3, rawvalue := 2 * close]
  expected[, dividend := 0]
  expected[, rawdividend := 0]
  
  setkey(expected, symbol, index)
  expect_that( make_raw_value( price_data, dividend = NULL, splits = split_dt),
                 equals(expected) )
  expect_that( make_raw_value( price_data, dividend = NULL, splits = split_xts),
               equals(expected) )
  expect_that( make_raw_value( as.xts(price_data), dividend = NULL, splits = split_dt),
               equals( as.xts(expected) ) )
  expect_that( make_raw_value( as.xts(price_data), dividend = NULL, splits = split_xts),
               equals( as.xts(expected) ) )  
})

test_that("make_raw_value factors split values into multisymbol data",{
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
  expected[, rawshares := 1/cumprod(split), by = symbol]
  expected[, rawvalue := close * rawshares]
  expected[, dividend := 0]
  expected[, rawdividend := 0]
  setkey(expected, symbol, index)
  
  expect_that(make_raw_value( price_data, dividend = NULL, splits = split_data_dt),
              throws_error("splits must have symbol for multisymbol price data") )
  expect_that(make_raw_value( price_data, dividend = NULL, splits = split_data_xts),
              throws_error("splits must have symbol for multisymbol price data") )
  expect_that(make_raw_value( as.xts(price_data), dividend = NULL, splits = split_data_dt),
              throws_error("splits must have symbol for multisymbol price data") )
  expect_that(make_raw_value( as.xts(price_data), dividend = NULL, splits = split_data_xts),
              throws_error("splits must have symbol for multisymbol price data") )
  
  split_data_dt[, symbol := c("SYM2", "SYM", "SYM2")]
  colnames(split_data_xts) <- c("SYM.Split", "SYM2.Split")
  
  expect_that( make_raw_value( price_data, dividend = NULL, splits = split_data_dt),
               equals(expected) )
  expect_that( make_raw_value( price_data, dividend = NULL, splits = split_data_xts),
               equals(expected) )
  
  expect_that( make_raw_value( as.xts(price_data), dividend = NULL, splits = split_data_dt),
               equals( as.xts(expected) ) )
  expect_that( make_raw_value( as.xts(price_data), dividend = NULL, splits = split_data_xts),
               equals( as.xts(expected) ) )
})

test_that("make_raw_value works with empty dividend/split results from quantmod",{
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
  expected_with_div[, rawshares := 1]
  expected_with_div[, rawvalue := 0]
  expected_with_div[, dividend := c(0.013, 0, 0)]
  expected_with_div[, rawdividend := dividend]
  expected_with_div[, rawvalue := close + cumsum(dividend)]
  setkey(expected_with_div, symbol, index)
  
  expected_with_spl <- data.table::copy(price_data)
  expected_with_spl[, split := c(1, 0.5, 1)]
  expected_with_spl[, rawshares := c(1, 2, 2)]
  expected_with_spl[, rawvalue := rawshares * close]
  expected_with_spl[, dividend := 0]
  expected_with_spl[, rawdividend := 0]
  setkey(expected_with_spl, symbol, index)
  
  expect_that( make_raw_value( price_data, dividend = dividend, splits = empty_splits),
               equals(expected_with_div) )
  expect_that( make_raw_value( as.xts(price_data), 
                           dividend = as.xts(dividend), 
                           splits = empty_splits ),
               equals( as.xts(expected_with_div) ) )
  
  expect_that( make_raw_value( price_data, dividend = empty_dividend, splits = splits),
               equals(expected_with_spl) )
  expect_that( make_raw_value( as.xts(price_data), 
                           dividend = empty_dividend, 
                           splits = as.xts(splits) ),
               equals( as.xts(expected_with_spl) ) )
})

test_that("make_raw_value assumes dividends are split adjusted (per Yahoo)",{
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
  expected[, rawshares := c(1,5,5)]
  expected[, rawvalue := 0]
  expected[, dividend := c(0.013, 0, 0)]
  expected[, rawdividend := dividend * 5 / rawshares]
  expected[, rawvalue := rawshares * close + cumsum(rawdividend)]
  setkey(expected, symbol, index)
  
  expect_that( make_raw_value( price_data, dividend = dividend, splits = splits),
               equals(expected) )
  expect_that( make_raw_value( as.xts(price_data), 
                           dividend = as.xts(dividend), 
                           splits = as.xts(splits) ),
               equals( as.xts(expected) ) )
})

test_that("make_raw_value assumes one rawdividend paid per rawshare",{
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
  expected[, rawshares := c(1,2,2,0.5,0.5)]
  expected[, rawvalue := 0]
  expected[, dividend := c(0.45, 0, 0.444, 0, 0.45)]
  expected[, rawdividend := c(0.225, 0, 0.111, 0, 0.45)]
  expected[, rawvalue := rawshares * close + cumsum(rawshares * rawdividend)]
  setkey(expected, symbol, index)
  
  expect_that( make_raw_value( price_data, dividend = dividend, splits = splits),
               equals(expected) )
  expect_that( make_raw_value( as.xts(price_data), 
                           dividend = as.xts(dividend), 
                           splits = as.xts(splits) ),
               equals( as.xts(expected) ) )
  # need to split-adjust the dividends even if the price data does not go all the way to
  # the split
  expect_that( make_raw_value( price_data[1:3,], dividend = dividend, splits = splits),
               equals(expected[1:3,]) )
  expect_that( make_raw_value( as.xts(price_data[1:3,]), 
                           dividend = as.xts(dividend), 
                           splits = as.xts(splits) ),
               equals( as.xts(expected[1:3,]) ) )
})

test_that("make_raw_return.default computes rawclose to rawclose plus properly adjusted dividends",{
  # NOTE: First dividend is not included.
  # Dividends recorded on date t in yahoo data are paid to you if you are a shareholder
  # as of the close t-1, i.e., going into the open on t you are a shareholder
  # (t is the Ex-date, although on yahoo under "Company Events" it will list t-1 as the 
  # Ex-date. that doesn't agree with other sources.)
  
  # In this example, the dividend paid at the first tick is not available to us
  # unless we already owned the stock on the day before this data set starts.
  # The dividend on the 3rd tick would be paid to us if we owned the stock at the close
  # on the 2nd tick.
  
  # For the purpose of calculating returns, the first dividend is not collected, the second is
  # and is used to calculate the return from tick 2 to tick 3. In general, price data
  # needs corresponding shares owned at the close data to determine raw returns if we
  # only hold at a subset of times.
  all_prices <- make_data_table("index close split rawshares rawvalue dividend rawdividend
                                2015-03-17  50    1     1          52        1        2           
                                2015-03-18  52    1     1          55        0.5      1
                                2015-03-19  24    0.5   2          51        0        0           
                                2015-03-20  27    1     2          60        1.5      1.5
                                2015-03-21  26    1     2          59        0.5      0.5")
  make_sure <- dplyr::select(all_prices, index, close)
  splits <- make_data_table("index split
                            2015-03-19 0.5")
  dividend <- make_data_table("index dividend
                              2015-03-17 1
                              2015-03-18 0.5
                              2015-03-20 1.5
                              2015-03-21 0.5")
  raw_prices <- make_raw_value(make_sure, dividend, splits = splits)  
  expect_that( data.table::setkey(all_prices, index), 
               equals(raw_prices) )
  
  whole_period <- make_raw_return( raw_prices[, rawshares], 
                               raw_prices[, rawdividend], 
                               raw_prices[, close]) 
  expect_that( whole_period[1],
               equals(0) )
  expect_that( whole_period[2],
               equals( (52 + 1 - 50) / 50 ) )
  expect_that( whole_period[3],
               equals( (2*(24+0.5)-50) / 50 ) )
  expect_that( whole_period[4],
               equals( (2*(27+0.5+1.5)-50) / 50 ) )
  expect_that( whole_period[5],
               equals( (2*(26+0.5+1.5+0.5)-50)/50 ) )

  alternate_version <- make_raw_return( raw_prices[2:5, rawshares],
                                    raw_prices[2:5, rawdividend],
                                    raw_prices[2:5, close],
                                    raw_prices[1, close * rawshares])
  expect_that( alternate_version,
               equals(whole_period[-1]) )
  partial_period <- make_raw_return( raw_prices[2:5, rawshares], 
                                                 raw_prices[2:5, rawdividend], 
                                                 raw_prices[2:5, close]) 
  expect_that( partial_period[1],
               equals(0) )
  expect_that( partial_period[2],
               equals( (2*24-52) / 52 ) )
  expect_that( partial_period[3],
               equals( (2*(27+1.5)-52) / 52 ) )
  expect_that( partial_period[4],
               equals( (2*(26+1.5+0.5)-52)/52 ) )
  
  post_split <- make_raw_return( raw_prices[3:5, rawshares], 
                                               raw_prices[3:5, rawdividend], 
                                               raw_prices[3:5, close]) 
  post_split_resized <- make_raw_return( c(1,1,1),
                                     raw_prices[3:5, rawdividend],
                                     raw_prices[3:5, close])
  expect_that( post_split,
               equals(post_split_resized) )
  
  
})

test_that("make_raw_return.data.table computed based on matching start/end index",{
  all_prices <- make_data_table("index close split rawshares rawvalue dividend rawdividend
                                2015-03-17  50    1     1          52        1        2           
                                2015-03-18  52    1     1          55        0.5      1
                                2015-03-19  24    0.5   2          50        0        0           
                                2015-03-20  27    1     2          59        1.5      1.5
                                2015-03-21  26    1     2          58        0.5      0.5")
  
  start <- as.POSIXct(all_prices[-5,index])
  end <- as.POSIXct(all_prices[-1,index])
  
  expected_every <- data.table( index = all_prices[-1,index],
                                     rawreturn = c((52+1-50)/50, 
                                                     (48-52)/52, 
                                                     (27+1.5-24)/24,
                                                     (26+0.5-27)/27 ) ) 
  expect_that( make_raw_return(all_prices, 1:4, 2:5),
               equals(expected_every) )
  expect_that( make_raw_return(all_prices, start, end),
               equals(expected_every) )
  expect_that( make_raw_return(all_prices, as.Date(start), as.Date(end) ),
               equals(expected_every) )
  expect_that( make_raw_return(all_prices, as.IDate(start), as.IDate(end) ),
               equals(expected_every) )
  
  start <- as.POSIXct(all_prices[c(1,3),index])
  end <- as.POSIXct(all_prices[c(3,5),index])
  
  expected_sparse <- data.table( index = all_prices[-1,index],
                                  rawreturn = c((52+1-50)/50, 
                                                (48+1-50)/50, 
                                                (27+1.5-24)/24,
                                                (26+1.5+0.5-24)/24 ) ) 
  expect_that( make_raw_return(all_prices, c(1,3), c(3,5)),
               equals(expected_sparse) )
  expect_that( make_raw_return(all_prices, start, end),
               equals(expected_sparse) )
  expect_that( make_raw_return(all_prices, as.Date(start), as.Date(end) ),
               equals(expected_sparse) )
  expect_that( make_raw_return(all_prices, as.IDate(start), as.IDate(end) ),
               equals(expected_sparse) )
  
  start <- as.POSIXct(all_prices[c(1,4),index])
  end <- as.POSIXct(all_prices[c(2,5),index])
  
  expected_sparser <- data.table( index = all_prices[c(2,5),index],
                                rawreturn = c((52+1-50)/50, 
                                              (26+0.5-27)/27 ) ) 
  expect_that( make_raw_return(all_prices, c(1,4), c(2,5)),
               equals(expected_sparser) )
  expect_that( make_raw_return(all_prices, start, end),
               equals(expected_sparser) )
  expect_that( make_raw_return(all_prices, as.Date(start), as.Date(end) ),
               equals(expected_sparser) )
  expect_that( make_raw_return(all_prices, as.IDate(start), as.IDate(end) ),
               equals(expected_sparser) )

  start <- as.POSIXct(all_prices[c(1,2,3),index])
  end <- as.POSIXct(all_prices[c(3,4,5),index])
  
  expected_overlap <- data.table( index = all_prices[c(3,4,5),index],
                                  rawreturn = c((48+1-50)/50, 
                                                ( 2*(27+1.5) - 52) / 52,
                                                (26+1.5+0.5-24)/24 ) ) 
  expect_that( make_raw_return(all_prices, c(1,2,3), c(3,4,5)),
               equals(expected_overlap) )
  expect_that( make_raw_return(all_prices, start, end),
               equals(expected_overlap) )
  expect_that( make_raw_return(all_prices, as.Date(start), as.Date(end) ),
               equals(expected_overlap) )
  expect_that( make_raw_return(all_prices, as.IDate(start), as.IDate(end) ),
               equals(expected_overlap) )
})

test_that("make_raw_return.xts uses similar options as quantmod::periodReturn",{
  all_prices <- make_data_table("index close split rawshares rawvalue dividend rawdividend
                           2015-03-18  50    1     1          52        1        2           
                           2015-03-19  24    0.5   2          50        0        0           
                           2015-03-20  27    1     2          59        1.5      1.5
                           2015-03-21  26    1     2          58        0.5      0.5")
  all_prices <- as.xts(all_prices)
  
  raw_daily <- make_raw_return(all_prices, 'daily')
  
  expected_daily <- xts( data.frame(daily_rawreturn = c(0,
                                                      (48-50)/50, #assume we buy the close, no dividend collected
                                            (54+3-48)/48, #collect the 1.5 dividend on 2 shares
                                            (26+0.5-27)/27)),#same as (53-54)/54
                          order.by = index(all_prices) ) 
  expect_that( raw_daily,
               equals(expected_daily))

  raw_weekly <- make_raw_return(all_prices, 'weekly')
  expected_weekly <- xts( data.frame(weekly_rawreturn = (52+3+1-50)/50 ),
                          order.by = index(all_prices[4,]) )
  
  expect_that( raw_weekly,
               equals(expected_weekly) )
  #Note that weekly returns are not the product or sum of daily returns
})

test_that("make_reinvested_shares computed as if dividends are reinvested in fractional shares at the previous close",{
  all_prices <- make_data_table("index close split rawshares rawvalue dividend rawdividend
                                2015-03-17  50    1     1          52        1        2           
                                2015-03-18  52    1     1          55        0.5      1
                                2015-03-19  24    0.5   2          51        0        0           
                                2015-03-20  27    1     2          60        1.5      1.5
                                2015-03-21  26    1     2          59        0.5      0.5")
  make_sure <- dplyr::select(all_prices, index, close)
  splits <- make_data_table("index split
                            2015-03-19 0.5")
  dividend <- make_data_table("index dividend
                              2015-03-17 1
                              2015-03-18 0.5
                              2015-03-20 1.5
                              2015-03-21 0.5")
  raw_prices <- make_raw_value(make_sure, dividend, splits = splits)  
  expect_that( data.table::setkey(all_prices, index), 
               equals(raw_prices) )
  
  reinvestedshares <- make_reinvested_shares(raw_prices[,close],
                                         raw_prices[,rawshares],
                                         raw_prices[,rawdividend])
  #first tick dividend is not received...must have been holding from the previous day
  expect_that( reinvestedshares[1],
               equals(1) )
  expect_that( reinvestedshares[2],
               equals( 1+ 1/50))
  expect_that( reinvestedshares[3],
               equals( 2*(1+ 1/50) ) )
  expect_that( reinvestedshares[4],
               equals( 2*(1+ 1/50) * (1+1.5/24 ) ) )
  expect_that( reinvestedshares[5],
               equals( 2*(1+ 1/50) * (1+1.5/24) * (1 + 0.5/27) ) )
  
  start_after_split <- make_reinvested_shares(raw_prices[3:5,close],
                                        raw_prices[3:5,rawshares],
                                        raw_prices[3:5,rawdividend])
  resized <- make_reinvested_shares(raw_prices[3:5,close],
                               c(1,1,1),
                               raw_prices[3:5,rawdividend])
  expect_that( start_after_split,
               equals(resized * 2) )
})

test_that("make_reinvested_return.default computes returns on reinvested dividends",{
  # NOTE: First dividend is not included.
  # Dividends recorded on date t in yahoo data are paid to you if you are a shareholder
  # as of the close t-1, i.e., going into the open on t you are a shareholder
  # (t is the Ex-date, although on yahoo under "Company Events" it will list t-1 as the 
  # Ex-date. that doesn't agree with other sources.)
  
  # In this example, the dividend paid at the first tick is not available to us
  # unless we already owned the stock on the day before this data set starts.
  # The dividend on the 3rd tick would be paid to us if we owned the stock at the close
  # on the 2nd tick.
  
  # For the purpose of calculating returns, the first dividend is not collected, the second is
  # and is used to calculate the return from tick 2 to tick 3. In general, price data
  # needs corresponding shares owned at the close data to determine raw returns if we
  # only hold at a subset of times.
  all_prices <- make_data_table("index close split rawshares rawvalue dividend rawdividend
                                2015-03-17  50    1     1          52        1        2           
                                2015-03-18  52    1     1          55        0.5      1
                                2015-03-19  24    0.5   2          51        0        0           
                                2015-03-20  27    1     2          60        1.5      1.5
                                2015-03-21  26    1     2          59        0.5      0.5")
  make_sure <- dplyr::select(all_prices, index, close)
  splits <- make_data_table("index split
                            2015-03-19 0.5")
  dividend <- make_data_table("index dividend
                              2015-03-17 1
                              2015-03-18 0.5
                              2015-03-20 1.5
                              2015-03-21 0.5")
  raw_prices <- make_raw_value(make_sure, dividend, splits = splits)  
  expect_that( data.table::setkey(all_prices, index), 
               equals(raw_prices) )
  
  reinvested_shares <- make_reinvested_shares(raw_prices[,close], 
                                      raw_prices[,rawshares],
                                      raw_prices[,rawdividend])
  
  whole_period <- make_reinvested_return( raw_prices[, close],
                                          raw_prices[, rawshares], 
                                          raw_prices[, rawdividend] )
  expect_that( whole_period[1],
               equals(0) )
  expect_that( whole_period[2],
               equals( (reinvested_shares[2]*52 - 50) / 50 ) )
  expect_that( whole_period[3],
               equals( ( reinvested_shares[3]*24 - 50) / 50 ) )
  expect_that( whole_period[4],
               equals( ( reinvested_shares[4]*27 - 50) / 50 ) )
  expect_that( whole_period[5],
               equals( ( reinvested_shares[5]*26 - 50)/50 ) )
  
  alternate_version <- make_reinvested_return( raw_prices[2:5, close],
                                        raw_prices[2:5, rawshares],
                                        raw_prices[2:5, rawdividend],
                                        raw_prices[1, close],
                                        raw_prices[1, rawshares])
  expect_that( alternate_version,
               equals(whole_period[-1]) )

  alternate_post_split <- make_reinvested_return( raw_prices[3:5, close],
                                               raw_prices[3:5, rawshares],
                                               raw_prices[3:5, rawdividend],
                                               raw_prices[2, close],
                                               raw_prices[2, rawshares])
  expect_that( alternate_post_split,
               equals(  make_reinvested_return( raw_prices[2:5, close],
                                                raw_prices[2:5, rawshares], 
                                                raw_prices[2:5, rawdividend] )[-1]
               ) )
  
  partial_period <- make_reinvested_return( raw_prices[2:5, close], 
                                     raw_prices[2:5, rawshares], 
                                     raw_prices[2:5, rawdividend])
  reinvested_shares <- make_reinvested_shares( raw_prices[2:5, close],
                                               raw_prices[2:5, rawshares],
                                               raw_prices[2:5, rawdividend] )
  expect_that( partial_period[1],
               equals(0) )
  expect_that( partial_period[2],
               equals( ( reinvested_shares[2] * 24 - 52) / 52 ) )
  expect_that( partial_period[3],
               equals( ( reinvested_shares[3] * 27 - 52) / 52 ) )
  expect_that( partial_period[4],
               equals( ( reinvested_shares[4] * 26 - 52)/52 ) )
  
  post_split <- make_reinvested_return( raw_prices[3:5, close], 
                                 raw_prices[3:5, rawshares], 
                                 raw_prices[3:5, rawdividend]) 
  post_split_resized <- make_reinvested_return( raw_prices[3:5, close],
                                                c(1,1,1),
                                         raw_prices[3:5, rawdividend])
  expect_that( post_split,
               equals(post_split_resized) )
})

test_that("make_reinvested_return.data.table computed based on matching start/end index",{
  all_prices <- make_data_table("index close split rawshares rawvalue dividend rawdividend
                                2015-03-17  50    1     1          52        1        2           
                                2015-03-18  52    1     1          55        0.5      1
                                2015-03-19  24    0.5   2          50        0        0           
                                2015-03-20  27    1     2          59        1.5      1.5
                                2015-03-21  26    1     2          58        0.5      0.5")
  
  start <- as.POSIXct(all_prices[-5,index])
  end <- as.POSIXct(all_prices[-1,index])
  
  expected_every <- data.table( index = all_prices[-1,index],
                                reinvested_return = c((52 * ( 1 + 1/50 )-50)/50, 
                                              (48-52)/52, 
                                              (27 * ( 1 + 1.5/24) - 24)/24,
                                              (26 * ( 1 + 0.5/27) - 27)/27 ) ) 
  expect_that( make_reinvested_return(all_prices, 1:4, 2:5),
               equals(expected_every) )
  expect_that( make_reinvested_return(all_prices, start, end),
               equals(expected_every) )
  expect_that( make_reinvested_return(all_prices, as.Date(start), as.Date(end) ),
               equals(expected_every) )
  expect_that( make_reinvested_return(all_prices, as.IDate(start), as.IDate(end) ),
               equals(expected_every) )
  
  start <- as.POSIXct(all_prices[c(1,3),index])
  end <- as.POSIXct(all_prices[c(3,5),index])
  
  expected_sparse <- data.table( index = all_prices[-1,index],
                                 reinvested_return = c((52 * (1+1/50) - 50)/50, 
                                               (48 * (1+1/50) - 50)/50, 
                                               (27 * (1+1.5/24) - 24)/24,
                                               (26 * (1+1.5/24) * (1+0.5/27)-24)/24 ) ) 
  expect_that( make_reinvested_return(all_prices, c(1,3), c(3,5)),
               equals(expected_sparse) )
  expect_that( make_reinvested_return(all_prices, start, end),
               equals(expected_sparse) )
  expect_that( make_reinvested_return(all_prices, as.Date(start), as.Date(end) ),
               equals(expected_sparse) )
  expect_that( make_reinvested_return(all_prices, as.IDate(start), as.IDate(end) ),
               equals(expected_sparse) )
  
  start <- as.POSIXct(all_prices[c(1,4),index])
  end <- as.POSIXct(all_prices[c(2,5),index])
  
  expected_sparser <- data.table( index = all_prices[c(2,5),index],
                                  reinvested_return = c((52 * (1+1/50)-50)/50, 
                                                (26 * (1 +0.5/27) - 27)/27 ) ) 
  expect_that( make_reinvested_return(all_prices, c(1,4), c(2,5)),
               equals(expected_sparser) )
  expect_that( make_reinvested_return(all_prices, start, end),
               equals(expected_sparser) )
  expect_that( make_reinvested_return(all_prices, as.Date(start), as.Date(end) ),
               equals(expected_sparser) )
  expect_that( make_reinvested_return(all_prices, as.IDate(start), as.IDate(end) ),
               equals(expected_sparser) )
  
  start <- as.POSIXct(all_prices[c(1,2,3),index])
  end <- as.POSIXct(all_prices[c(3,4,5),index])
  
  expected_overlap <- data.table( index = all_prices[c(3,4,5),index],
                                  reinvested_return = c((48*(1+1/50)-50)/50, 
                                                ( 2*27*(1+1.5/24) - 52) / 52,
                                                (26 * (1+1.5/24) * (1+0.5/27)-24)/24 ) ) 
  expect_that( make_reinvested_return(all_prices, c(1,2,3), c(3,4,5)),
               equals(expected_overlap) )
  expect_that( make_reinvested_return(all_prices, start, end),
               equals(expected_overlap) )
  expect_that( make_reinvested_return(all_prices, as.Date(start), as.Date(end) ),
               equals(expected_overlap) )
  expect_that( make_reinvested_return(all_prices, as.IDate(start), as.IDate(end) ),
               equals(expected_overlap) )
})

test_that("make_reinvested_return.xts uses similar options as quantmod::periodReturn",{
  all_prices <- make_data_table("index close split rawshares rawvalue dividend rawdividend
                                2015-03-18  50    1     1          52        1        2           
                                2015-03-19  24    0.5   2          50        0        0           
                                2015-03-20  27    1     2          59        1.5      1.5
                                2015-03-21  26    1     2          58        0.5      0.5")
  all_prices <- as.xts(all_prices)
  
  reinvested_daily <- make_reinvested_return(all_prices, 'daily')
  
  expected_daily <- xts( data.frame(daily_reinvested_return = c(0,
                                                        (48-50)/50, #assume we buy the close, no dividend collected
                                                        (54*(1+1.5/24)-48)/48, #collect the 1.5 dividend on 2 shares
                                                        (26*(1+0.5/27)-27)/27)),#same as (53-54)/54
                         order.by = index(all_prices) ) 
  expect_that( reinvested_daily,
               equals(expected_daily))
  
  reinvested_weekly <- make_reinvested_return(all_prices, 'weekly')
  expected_weekly <- xts( data.frame(weekly_reinvested_return = (2*26*(1+1.5/24)*(1+0.5/27)-50)/50 ),
                          order.by = index(all_prices[4,]) )
  
  expect_that( reinvested_weekly,
               equals(expected_weekly) )
  
  #Note that 1+weekly returns ARE the product of 1 + daily returns for compounded returns
  expect_that( as.numeric(1+reinvested_weekly),
               equals( prod(1+reinvested_daily) ) )
})

test_that("make_raw_value ignores dividends/splits outside time period except to adjust dividends",{
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
  expected[, rawshares := c(1, 2, 2)]
  expected[, rawvalue := 0]
  expected[, dividend := c(0.04, 0, 0)]
  expected[, rawdividend := dividend * 2/7]
  expected[, rawvalue := rawshares * close + cumsum(rawshares * rawdividend)]
  setkey(expected, symbol, index)
  
  expect_that( make_raw_value( price_data, dividend = dividend, splits = splits),
               equals(expected) )
  expect_that( make_raw_value( as.xts(price_data), 
                           dividend = as.xts(dividend), 
                           splits = as.xts(splits) ),
               equals( as.xts(expected) ) )
})

test_that("make_raw_value cannot use nonsymboled price data with multisymbol dividends or splits",{
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
  
  expect_that(make_raw_value( price_data, dividend_data_dt, splits = NULL),
              throws_error("price must have symbol for multisymbol dividend data") )
  expect_that(make_raw_value( price_data, dividend_data_xts, splits = NULL),
              throws_error("price must have symbol for multisymbol dividend data") )
  expect_that(make_raw_value( as.xts(price_data), dividend_data_dt, splits = NULL),
              throws_error("price must have symbol for multisymbol dividend data") )
  expect_that(make_raw_value( as.xts(price_data), dividend_data_xts, splits = NULL),
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
  expect_that(make_raw_value( price_data, dividend = NULL, splits = split_data_dt),
              throws_error("price must have symbol for multisymbol splits data") )
  expect_that(make_raw_value( price_data, dividend = NULL, splits = split_data_xts),
              throws_error("price must have symbol for multisymbol splits data") )
  expect_that(make_raw_value( as.xts(price_data), dividend = NULL, splits = split_data_dt),
              throws_error("price must have symbol for multisymbol splits data") )
  expect_that(make_raw_value( as.xts(price_data), dividend = NULL, splits = split_data_xts),
              throws_error("price must have symbol for multisymbol splits data") )  
  
  expect_that(make_raw_value( price_data, dividend_data_xts, splits = split_data_dt),
              throws_error("price must have symbol for multisymbol dividend data") )
  expect_that(make_raw_value( price_data, dividend_data_dt, splits = split_data_xts),
              throws_error("price must have symbol for multisymbol dividend data") )
  expect_that(make_raw_value( as.xts(price_data), dividend_data_dt, splits = split_data_dt),
              throws_error("price must have symbol for multisymbol dividend data") )
  expect_that(make_raw_value( as.xts(price_data), dividend_data_xts, splits = split_data_xts),
              throws_error("price must have symbol for multisymbol dividend data") )  
})

test_that("make_raw_value compared to adjusted close",{
  symbol <- "AAPL"
  getSymbols <- quantmod::getSymbols # getSymbols doesn't expect to see the 
                                     # package name when it retrieves its
                                     # defaults (gives a warning)
                                     # Do this rather than attaching quantmod.
  price <- getSymbols(symbol)
  dividend <- quantmod::getDividends(symbol)
  splits <- quantmod::getSplits(symbol)
  
  expect_that( nrow(splits),
               is_more_than(0) )
  expect_that( nrow(dividend),
               is_more_than(0) )
  
  adjusted_price <- quantmod::adjustOHLC( price, symbol.name = symbol)
  raw_value_price <- make_raw_value( price, dividend, splits = splits)
  
  raw_value_price <- as.data.table(raw_value_price)
  # truly compounded shares would be paid dividends on the compounded shares.
  # this assumes you get paid dividends on your raw shares only.
  # initialize compoundedshares to rawshares at first index (always 1, unless first tick
  # has a split). On dividend, compoundedshares increases by (compoundedshares*rawdividend/close)
  # On split, compounded shares gets divided by split.
  raw_value_price[, compoundedshares := make_reinvested_shares(close, rawshares, rawdividend)]#rawshares * cumprod(1+rawdividend/close)]
#   raw_value_price[, compoundedshares := rawshares + cumsum(rawshares*rawdividend/close)]
  raw_value_price[, compoundedvalue := compoundedshares * close + cumsum(compoundedshares*rawdividend)]
  raw_value_price <- as.xts(raw_value_price)
  
  adj_col <- paste0(symbol,".Close")
  rv_col <- paste0(symbol,".Rawvalue")
  cv_col <- paste0(symbol,".Compoundedvalue")
  
  quick_return <- function(x) {
    n <- nrow(x)
    (as.numeric(x[n,]) - as.numeric(x[1,]) ) / as.numeric(x[1,])
  }
  quick_log_return <- function(x) {
    n <- nrow(x)
    log(as.numeric(x[n,]) / as.numeric(x[1,]) )
  }
  adjusted_return <- quick_return(adjusted_price[, adj_col])
  rv_return <- quick_return( raw_value_price[, rv_col])
  cv_return <- quick_return( raw_value_price[, cv_col])
  
  #return since 2007 is off by 25%
  expect_that( rv_return,
               equals(adjusted_return, tolerance = 0.25, scale = 1) )
  expect_that( cv_return,
               equals(adjusted_return, tolerance = 0.42, scale = 1) )
  
  adj_daily <- quantmod::dailyReturn( adjusted_price[, adj_col] )
  rv_daily <- quantmod::dailyReturn( raw_value_price[, rv_col] )
  cv_daily <- quantmod::dailyReturn( raw_value_price[, cv_col] )
  #they don't really match because adj_daily retroactively redefines the 
  #price p on the day before the dividend d to be p-d, so the daily return
  #will be wrt p-d, but rv_daily will use p.
  expect_that( rv_daily, 
               equals( adj_daily, tolerance = 2e-4, scale = 1) )
  #the daily compounded return matches worse on average
  expect_that( cv_daily, 
               equals( adj_daily, tolerance = 3e-4, scale = 1) )
})
