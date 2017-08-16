context("quantmod")
library(data.table)

test_that("spread_symbol makes symbol prefix on other columns",{
  df <- data.frame( symbol=c("a", "a", "b", "b"),
                    index=c(9,10,9,10),
                    x = c(1,2,3,4),
                    y = c(5,6,7,8))
  expect_equal( spread_symbol(df),
                data.frame(index=c(9,10),
                           a.x = c(1,2), a.y=c(5,6),
                           b.x = c(3,4), b.y=c(7,8)))
  unbalanced <- df[-1,]
  expect_equal( spread_symbol(unbalanced),
                data.frame(index=c(9,10),
                           a.x=c(NA,2), a.y=c(NA,6),
                           b.x=c(3,4), b.y=c(7,8)))
  alt_names <- df
  colnames(alt_names) <- c("syms","t","x","y")
  expect_equal( spread_symbol(alt_names, symbol="syms", index="t", sep="_"),
                data.frame(t=c(9,10),
                           a_x=c(1,2), a_y=c(5,6),
                           b_x=c(3,4), b_y=c(7,8)))
  dt <- as.data.table(df)
  expect_equal( spread_symbol(dt),
                data.table(index=c(9,10),
                           a.x = c(1,2), a.y=c(5,6),
                           b.x = c(3,4), b.y=c(7,8)))
  keeps_index_attr <- copy(dt)
  attr(keeps_index_attr$index, "something") <- 5
  expected <- data.table(index=c(9,10),
                         a.x = c(1,2), a.y=c(5,6),
                         b.x = c(3,4), b.y=c(7,8))
  attr(expected$index, "something") <- 5
  expect_equal( spread_symbol(keeps_index_attr),
                expected)
  
  mixed_price_types <- data.frame(index=c(9,10,9,10),
                  symbol=c("a", "a", "b", "b"),
                  x=c(1,2,NA,NA),
                  y=c(5,6,3,4),
                  z=c(NA, NA, 7,8))
  expect_equal( spread_symbol(mixed_price_types),
                data.frame(index=c(9,10),
                           a.x=c(1,2), a.y=c(5,6),
                           b.y=c(3,4), b.z=c(7,8)))
  expect_equal( spread_symbol(mixed_price_types, drop=FALSE),
                data.frame(index=c(9,10),
                           a.x=c(1,2), a.y=c(5,6), a.z=c(NaN, NaN),
                           b.x=c(NaN,NaN), b.y=c(3,4), b.z=c(7,8)) )

  one_symbol_one_column <- data.frame(index=c(9,10),
                           symbol=c("a","a"),
                           x=c(1,2))
  expect_equal( spread_symbol(one_symbol_one_column),
                data.frame(index=c(9,10),
                           a.x=c(1,2)))
})

test_that("gather_symbol peels off symbols from column names and creates column",{
  make_data_frame <- function(...) {
    data.frame(..., stringsAsFactors = FALSE)
  }
  df <- make_data_frame(index=c(9,10),
             a.x = c(1,2), a.y=c(5,6),
             b.x = c(3,4), b.y=c(7,8))
  expect_equal( gather_symbol(df),
                make_data_frame( index=c(9,10,9,10),
                            symbol=c("a", "a", "b", "b"),
                            x = c(1,2,3,4),
                            y = c(5,6,7,8)) )
  missing_symbol <- make_data_frame(index=c(9,10),
                           a.x=c(NA,2), a.y=c(NA,6),
                           b.x=c(NA,4), b.y=c(7,NA) )
  expect_equal( gather_symbol(missing_symbol),
                make_data_frame( index=c(10,9,10),
                            symbol=c("a", "b", "b"),
                            x = c(2,NA,4),
                            y = c(6,7,NA)) )
  expect_equal( gather_symbol(missing_symbol, drop=FALSE),
                make_data_frame( index=c(9,10,9,10),
                            symbol=c("a", "a", "b", "b"),
                            x = c(NA,2,NA,4),
                            y = c(NA,6,7,NA)) )
  alt_names <- df
  names(alt_names) <- c("t", "a_x", "a_y", "b_x", "b_y")
  expect_equal( gather_symbol(alt_names, index="t", symbol="syms", sep="_"),
                make_data_frame( t=c(9,10,9,10),
                            syms=c("a", "a", "b", "b"),
                            x = c(1,2,3,4),
                            y = c(5,6,7,8)) )
  dt <- as.data.table(df)
  expect_equal( gather_symbol(dt),
                data.table( index=c(9,10,9,10),
                            symbol=c("a", "a", "b", "b"),
                            x = c(1,2,3,4),
                            y = c(5,6,7,8)) )
  keeps_index_attr <- copy(dt)
  attr(keeps_index_attr$index, "something") <- 5
  expected <- data.table( index=c(9,10,9,10),
                          symbol=c("a", "a", "b", "b"),
                          x = c(1,2,3,4),
                          y = c(5,6,7,8))
  attr(expected$index, "something") <- 5
  expect_equal( gather_symbol(keeps_index_attr),
                expected)
  mixed_price_types <- make_data_frame(index=c(9,10),
                        a.x = c(1,2), a.y=c(5,6),
                        b.y = c(3,4), b.z=c(7,8))
  expect_equal( gather_symbol(mixed_price_types),
                make_data_frame(index=c(9,10,9,10),
                                symbol=c("a", "a", "b", "b"),
                                x=c(1,2,NA,NA),
                                y=c(5,6,3,4),
                                z=c(NA, NA, 7,8)))
  one_symbol_one_column <- make_data_frame(index=c(9,10),
                                           a.x=c(1,2))
  expect_equal( gather_symbol(one_symbol_one_column),
                make_data_frame(index=c(9,10),
                                symbol=c("a","a"),
                                x=c(1,2)))
})

get_raw_yahoo <- function(symbol, from, to) {
  # adapated from quantmod
  tmp <- tempfile()
  on.exit(unlink(tmp))
  from.posix <- quantmod:::.dateToUNIX(from)
  to.posix <- quantmod:::.dateToUNIX(to)
  handle <- quantmod:::.getHandle()
  interval <- '1d'
  Symbols.name <- symbol
  yahoo.URL <- quantmod:::.yahooURL(Symbols.name, from.posix, to.posix, 
                         interval, "history", handle)
  dl <- try(curl::curl_download(yahoo.URL, destfile = tmp, handle = handle$ch), silent = TRUE)
  if (inherits(dl, "try-error")) {
    warning(Symbols.name, " download failed; trying again.", 
            call. = FALSE, immediate. = TRUE)
    handle <- quantmod:::.getHandle(force.new = TRUE)
    yahoo.URL <- quantmod:::.yahooURL(Symbols.name, from.posix, 
                           to.posix, interval, "history", handle)
    dl <- try(curl::curl_download(yahoo.URL, destfile = tmp, 
                                  quiet = !verbose, handle = handle$ch), silent = TRUE)
    if (inherits(dl, "try-error")) {
      stop(Symbols.name, " download failed after two attempts. Error", 
           " message:\n", attr(dl, "condition")$message, 
           call. = FALSE)
    }
  }
  fr <- read.csv(tmp, na.strings = "null", stringsAsFactors = FALSE)
  fr[,'Date'] <- as.Date(fr[,'Date'])
  fr
}

test_that("remove_autosplit undoes the automatic split adjusted closes from yahoo",{
  df <- data.frame(Date=as.Date(c('2003-02-14','2003-02-18','2003-02-19','2003-02-20','2003-02-21')),
                   MSFT.Open=c(23.625, 24.62, 24.82, 24.77, 24.29),
                   MSFT.High=c(24.25, 24.99, 24.88, 24.87, 24.8),
                   MSFT.Low=c(23.385, 24.4, 24.17, 24.1, 23.7),
                   MSFT.Close=c(48.30/2, 24.96, 24.53, 24.14, 24.63),
                   MSFT.Volume=c(90446400, 57415500, 46902700, 50897200, 56853200),
                   MSFT.Adjusted=c(16.2062, 16.74977, 16.51414, 16.25158, 16.58146))
  prices <- xts(df[,2:7], order.by=df[,'Date'])
  splits <- xts(data.frame(MSFT.spl = c(0.25, 0.5, 1/7, 1/5)),
                order.by=as.Date(c("1999-01-01", "2003-02-18", "2003-02-20", "2009-02-01")))
  # Yahoo currenty returns closes split adjusted for all splits, not just the ones in range to/from
  df[4:5, "MSFT.Close"] <- 5 * c(24.14, 24.63) # 
  df[2:3, "MSFT.Close"] <- 5 * 7 * c(24.96, 24.53) # 
  df[1, "MSFT.Close"] <- 5 * 7 * 48.30
  expected <- xts(df[,2:7], order.by=df[,1])
  expect_equal( remove_autosplit(prices, splits),
                expected)
})

test_that("Adjusted quantmod output is double adjusted for dividends",{
  library(quantmod)
  symbol <- "MSFT"
  from <- "2003-01-01"
  to <- "2004-12-31"

  data <- getSymbols(symbol, from = from, to=to)
  splits <- getSplits(symbol)
  pre_split_date <- '2003-02-14'
  split_date <- '2003-02-18'
  split_ratio <- 0.5
  expect_equal( as.numeric(splits[split_date]), split_ratio)
  dividends <- getDividends(symbol)
  pre_div_date <- '2004-11-12'
  div_date <- '2004-11-15'
  div_amount <- 3.08
  expect_equal( as.numeric(dividends[div_date]), div_amount)
  
  as.numeric(data[div_date,'MSFT.Adjusted']) - as.numeric(data[pre_div_date,'MSFT.Adjusted'])
  as.numeric(data[div_date,'MSFT.Close']) - as.numeric(data[pre_div_date,'MSFT.Close'])
  
  raw_yahoo <- get_raw_yahoo(symbol, from, to)
  raw_yahoo[raw_yahoo$Date==as.Date(split_date),] 
  
  # quantmod::getSymbols does this to adjust the OHL, which is now unnecessary
  # ohl <- c("Open", "High", "Low")
  # adjusted_ohl <- round(raw_yahoo[,ohl] * drop( raw_yahoo[,"Close"] / raw_yahoo[,"Adj.Close"]), 3)
  # # expect_equal( as.numeric(adjusted_ohl), coredata(data[1:3,]))
  
  # More importantly(?) quantmod expects raw_yahoo to be split but not dividend adjusted,
  # thus does a dividend adjustment...but I don't see where this is happening.
  # raw_yahoo is now already dividend adjusted
  quantmod_rets <- (data - lag(data))/lag(data)
  n <- nrow(raw_yahoo)
  raw_ret <- (raw_yahoo[2:n,'Adj.Close'] - raw_yahoo[1:(n-1),'Adj.Close']) / raw_yahoo[1:(n-1),'Adj.Close']
  expect_equal( drop(coredata(quantmod_rets[2:n,'MSFT.Adjusted'])), # twice adjusted, and rounded
                raw_ret) # once adjusted
  # Due to rounding this could be off more
  # What's the problem?
  # TODO figure out. I think maybe quantmod has no problem with Adjusted. It was just passing it through,
  # but maybe I am doing something wrong. Maybe with close?? Does close need to be unadjusted
  
  # Close appears to be adjusted for splits
  unsplit <- data[,'MSFT.Close']
  unsplit[paste0('/',pre_split_date),] <- data[paste0('/',pre_split_date),'MSFT.Close'] / split_ratio
  
  expect_equal( as.numeric(raw_yahoo[raw_yahoo$Date==pre_split_date, 'Close']),
                as.numeric(unsplit[pre_split_date] * split_ratio)) # this only holds on the last split date...otherwise there's a cumulative effect
})
# double adjustment is happening on OHL, which I'm not using
# so don't worry about this for now
# test_that("Avoiding quantmod double adjustment bug",{
#   symbol <- "MSFT"
# })