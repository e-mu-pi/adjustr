#' @import data.table
NULL

#' Retrieve stock prices as data.table
#' 
#' \code{getDTSymbols} adapts \code{quantmod::getSymbols} to convert
#' results to \code{data.table}. 
#' 
#' @export
getDTSymbols <- function(x, ...) {
  getSymbols <- quantmod::getSymbols # getSymbols doesn't expect to see the 
  # package name when it retrieves its
  # defaults (gives a warning)
  # Do this rather than importing getSymbols.
  data <- getSymbols(x, ...)
  as.data.table(data)
}

#' Make additive price adjustments without lookforward bias.
#' 
#' Apply dividend and split data to price series without changing
#' past data. Changes are applied additively, perhaps multiplicative 
#' will be added in the future. A dividend paid at time t will increase
#' the value at time t and all future times. A split at time t will 
#' apply a factor at time t and all future times. The RawValue and
#' RawShares will be added.
#' 
#' Because one could accidentally pass in the dividend object as the split and
#' vice versa, split must be assigned by name.
#' 
#' Note that splits and dividends that overlap may not be processed correctly.
#' Splits are processed first, as the price will be reported post-split, so
#' we assume the shares have already split, and the dividend is reported 
#' per share. In practice, this may be false, in which case work would need to be done.
#' 
#' TODO: Need to make sure it works when there is a split or dividend on a date with no price.
#' @export
raw_value <- function(price_data, dividend, ..., splits) {
  is_xts <- xts::is.xts(price_data)
  if( is_xts ) {
    xts_attr <- xts::xtsAttributes(price_data)
    price <- as.data.table(price_data)
  } else {
    price <- copy(price_data)
  }
  has_symbol <- function(x) "symbol" %in% names(x)
  has_some <- function(x,field) ! is.null(x) && ! all(is.na(x)) && ! all(is.na(x[,field, with = FALSE]))
  if ( has_symbol(price) ) {
    merge_key <- c("symbol", "index")
    available_symbols <- price[, unique(symbol)]
  } else {
    merge_key <- c("index")
  }
  multi_symbol <- has_symbol(price) && length(available_symbols) > 1
  if( xts::is.xts(dividend) ) {
    dividend <- as.data.table(dividend)
    if( all( names(dividend) %in% c("index", "V1") ) ){
      setnames(dividend, "V1", "dividend")
    }
  }
  normalize <- function(x) {
    if( is.data.table(x) ) {
      if( ! has_symbol(x) ) {
        if ( multi_symbol ) {
          stop(deparse(substitute(x)),
               " must have symbol for multisymbol price data")
        } else if ( has_symbol(price)) {
          x[, symbol := available_symbols]
        }
      } else {
        multi_symbol <<- multi_symbol || x[,length(unique(symbol)) > 1]
        if( multi_symbol && ! has_symbol(price) ) {
          stop("price must have symbol for multisymbol ",
               deparse(substitute(x)),
               " data")
        }
      }
    }
    x
  }
  dividend <- normalize(dividend) # I thought the assignment was unnecessary because
  # data.tables are passed by reference, but it wasn't always working??
  if( xts::is.xts(splits) ) {
    splits <- as.data.table(splits)
    if( "spl" %in% names(splits) ) {
      setnames(splits, "spl", "split")
    }
    if( all( names(splits) %in% c("index", "V1") ) ){
      setnames(splits, "V1", "split")
    }
  }
  splits <- normalize(splits)
  index_class <- class(price[,index])
  is_date <- all( class(price[,index]) == "Date" )
  if( ! is_date ) {
    price[, index := as.Date(index)]
  }
  if( has_some(splits, "split") ) {
    splits[, index := as.Date(index)]
    price <- merge( price, splits, by = merge_key, all.x = TRUE)
    price[is.na(split), split := 1]
    if ( has_symbol(price) ) {
      price[, rawshares := 1 / cumprod(split), by = symbol ]
    } else {
      price[, rawshares := 1 / cumprod(split)]
    }
    price[, rawvalue := rawshares * close]
  } else {
    price[, split := 1]
    price[, rawshares := 1]
    price[, rawvalue := close]
  }
  if( has_some(dividend, "dividend") ) {
    dividend[, index := as.Date(index)]
    if( has_some(splits, "split") ) {
      dividend <- merge(dividend, splits, by = merge_key, all = TRUE)
      dividend[is.na(dividend), dividend := 0]
      dividend[is.na(split), split := 1]
      if( has_symbol(price) ) {
        dividend[, rawshares := 1/cumprod(split), by = symbol]
        dividend[, retroactive_shares := last(rawshares) / rawshares, by = symbol]
        dividend[, rawdividend := retroactive_shares * dividend, by = symbol]
      } else {
        dividend[, rawshares := 1/cumprod(split)]
        dividend[, retroactive_shares := last(rawshares) / rawshares]
        dividend[, rawdividend := retroactive_shares * dividend]
      }
      dividend[, split := NULL]
      dividend[, rawshares := NULL] 
      dividend[, retroactive_shares := NULL] #maybe keep this
    } else {
      dividend[, rawdividend := dividend]
    }
    price <- merge(price, dividend, by = merge_key, all.x = TRUE)
    price[is.na(dividend), dividend := 0]
    price[is.na(rawdividend), rawdividend := 0]
    if( has_symbol( price ) ) {
      price[, rawvalue := rawvalue + cumsum(rawshares * rawdividend), by = symbol]
    } else {
      price[, rawvalue := rawvalue + cumsum(rawshares * rawdividend)]
    }
  } else {
    price[, dividend := 0]
    price[, rawdividend := 0]
  }
  if( ! is_date ) {
    if ( all( index_class == c("POSIXct", "POSIXt") ) ) {
      sorted_attr <- attr(price,"sorted")
      price[, index := as.POSIXct( as.character(index) )]
      attr(price,"sorted") <- sorted_attr
    } else{
      stop("index must be POSIXct or Date currently.")
    }
  }
  if( is_xts ) {
    price <- as.xts(price)
    xts::xtsAttributes(price) <- xts_attr
  }
  price
}

#' Compute period returns as value change plus dividends.
#' 
#' @param raw_value_data A \code{data.table} including columns \code{index, close, rawshares, rawdividends}. 
#' The \code{index} column must be a valid type for \code{xts} indexing. Multi-symbol data
#' is not supported.
#' @param period A string indicating a granularity. See quantmod::periodReturn.
#' 
#' @return An \code{xts} object ordered by the \code{index} column granulated to period. The
#' returns are calculated as close to close values multiplied by rawshares to account for
#' splits and adding in dividends over the period. Note that a dividend on date t in Yahoo 
#' data is only paid to those holding at the close on date t-1. Therefore, there a dividend
#' on date t for which t is also the period start will be recorded in the period ending on date
#' t, not the next period starting on t. In particular, daily returns include a dividend on
#' date t in the return from t-1 to t, recorded as the return on date t.
#' 
#' Note that daily raw returns for one week do not simply convert to weekly returns as prod(1+r).
#' Each day the basis for return is the close, but the return comes from the following close
#' plus dividends. Those dividends are not included in the basis for the following day.
#' 
#' @export
raw_return <- function(x, ...) UseMethod("raw_return")

#' Compute raw return from split adjusted shares, dividends, and closing prices.
#' 
#' Computes the return of buying the given number of shares at the first closing price
#' and exiting at any intermediate closing price including splits and dividends. The
#' number of shares should only change based on splits; no accounting is done to rebase
#' the return for other purchases and sales of shares. The dividends must be as they would
#' historically have been reported, not split adjusted as Yahoo provides. A dividend on 
#' the first tick is not included as Yahoo reports them on the ex-date. 
#' 
#' @param split_adjusted_shares Numeric vector of number of shares accounting
#'   for share splits. Typically, starts with 1 and adjusts for each split to reflect what
#'   that 1 share has become.
#' @param split_unadjusted_dividends Numeric vector of dividends as they would 
#'   have been announced historically. The values returned from Yahoo are split adjusted. 
#'   For example, a dividend of 0.2 that occured before a 2:1 split would be reported as
#'   0.1 by Yahoo (assuming no subsequent splits). This function expects the 0.2. Available
#'   as \code{rawdividend} as returned by \code{raw_value}.
#' @param close The historical closing prices with no adjustments.
#' @param initial_rawclose Optional starting value to measure returns against. Default is
#' NULL, i.e., just use the first ticks from the other data vectors. If used, the first 
#' dividend will be included.
#' 
#' @return A numeric vector the same length as the inputs with the arithmetic return from
#' the first close including dividends.
#' 
#' @export
raw_return.default <- function(split_adjusted_shares, split_unadjusted_dividends, close, 
                                initial_rawclose = NULL) {
  rawclose <- split_adjusted_shares * close
  if( is.null(initial_rawclose) ) {
    initial_rawclose <- rawclose[1]
    paid_dividends <- function(x) c(0, x[-1])
  } else {
    paid_dividends <- function(x) x
  }
  (rawclose + cumsum(split_adjusted_shares * paid_dividends(split_unadjusted_dividends) ) - 
     initial_rawclose)/ initial_rawclose
}

#' @export
raw_return.xts <- function(raw_value_xts, period = 'monthly') {
  period_opts <- list(daily = "days", weekly = "weeks", monthly = "months", 
                      quarterly = "quarters", yearly = "years", annually = "years")
  end_points <- xts::endpoints( index(raw_value_xts), 
                                on = period_opts[[period]])
  end_points <- end_points[-1] #drop initial 0
  if( ! 1 %in% end_points ) end_points <- c(1, end_points)
  n <- length(end_points)
  raw_ret_dt <- raw_return( as.data.table(raw_value_xts), 
                       start = end_points[-n],
                       end = end_points[-1] )
  if ( period == 'daily' ) {
    initial_zero_to_match_quantmod <- data.table(index = index(raw_value_xts[1,]),
                                                 rawreturn = 0)
    raw_ret <- as.xts( rbind( initial_zero_to_match_quantmod,
                              raw_ret_dt ) )
  } else {
    # dt version gives intermediate returns between start and end
    # to match quantmod, only return the values at endpoints
    # end_points[-1]: remove initial starting point
    # -1: indexing is off by 1 because raw_return will not have a return on the
    # first tick
    raw_ret <- as.xts( raw_ret_dt[end_points[-1]-1,] ) 
  }
  colnames(raw_ret) <- paste(period, "rawreturn", sep = "_")
  raw_ret
} 

#' @export
raw_return.data.table <- function(raw_value_data, start, end) {

  n_intervals <- length(end)
  n <- nrow(raw_value_data)

  if( xts::timeBased(end) ) {
    end_index <- raw_value_data[, which(as.IDate(index) %in% as.IDate(end)) ]
  } else {
    end_index <- end
  }
  if( xts::timeBased(start) ) {
    start_index <- raw_value_data[, which(as.IDate(index) %in% as.IDate(start)) ]
  } else {
    start_index <- start
  }
  num_starts <- length(start_index)

  raw_value_data[, period_index := 0]
  raw_value_data[end_index, period_index := 1]
  raw_value_data[start_index, period_index := period_index + 1]
  raw_value_data[, period_index := c(0, cumsum( period_index)[-n]) ]
  raw_value_data[, on_period := period_index %in% period_index[end_index]]

  initial_rawclose <- raw_value_data[start_index, close * rawshares]
  overlap <- any( start_index[-1] < end_index[-n_intervals])
  if( overlap ) {
    overlapped_dividends <- numeric(length(initial_rawclose))  
    for( idx in seq_along(start_index)) {
      overlapped_dividends[idx] <- raw_value_data[(start_index[idx]+1):end_index[idx],
                                                   sum(rawdividend * rawshares / last(rawshares) )]
    }
    raw_ret <- raw_value_data[(on_period),  list(rawreturn = raw_return(last(rawshares), 
                                                      overlapped_dividends[.GRP], 
                                                      last(close), 
                                                      initial_rawclose[.GRP]) ), 
                               by = period_index]
  } else {
    raw_ret <- raw_value_data[(on_period), 
                               list(rawreturn = raw_return(rawshares, 
                                                            rawdividend, 
                                                            close, 
                                                            initial_rawclose[.GRP])), 
                             by = period_index]
  }
  ret <- data.table( index = raw_value_data[(on_period), index],
              rawreturn = raw_ret[,rawreturn])
  raw_value_data[, period_index := NULL]
  raw_value_data[, on_period := NULL]
  ret
  # Dividends are paid to the holder on the previous day in Yahoo data.
  # Therefore, they need to be offset by 1 as to which period they are
  # paid in. A dividend on the initial day should not be included because
  # it would be paid in the previous period. If the initial period is partial,
  # I suppose it should be included, but currently it's not. The first period
  # isn't perfect anyway because we don't have the true starting price.


}

#' Compute shares held based on splits and reinvested dividends.
#' 
#' For a sequence of closing prices, split adjusted shares, and split unadjusted dividends,
#' (i.e., close, raw_shares, and raw_dividends), compute the number or shares held if the
#' dividends are reinvested.
#' 
#' This function assumes that a dividend at tick t is paid to the stock owners at tick t-1 (i.e.,
#' t is the ex-date in accordance with how Yahoo reports dividends). Therefore, a dividend
#' on the initial tick is assumed not to be received.
#' 
#' Per the ordinary dividends received on mutual funds in my TD Ameritrade account, the
#' reinvestment of a dividend on date t is done at a trade price of the close on date t-1.
#' This may be a bad assumption in other cases. And in yet other cases, it is not 
#' practical to reinvest your dividends at all (stocks in my TD Ameritrade account). If 
#' your account is big enough to reinvest dividends, this function may need to be
#' modified to match your reinvestment procedure. Also for large accounts, you may
#' need to find share buyback data to accurately reflect other ways cash will be returned
#' to you (not handled in this function).
#' 
#' @param close A numeric vector of closing prices, not adjusted for splits or dividends.
#' @param split_adjusted_shares A numeric vector of shares held. The shares should already
#' account for splits (e.g., if you start with 1 share and there is a 2:1 split, you should
#' have 2 shares after the split).
#' @param unadjusted_dividends A numeric vector of dividends paid. These should not be adjusted
#' for splits. Yahoo adjusts their dividends for splits. The raw_value function can create
#' raw dividends (i.e., undo the split adjustment).
#' 
#' @return A numeric vector of shares after splits and reinvestment of dividends.
#' 
#' @export
reinvested_shares <- function(close, split_adjusted_shares, unadjusted_dividends) {
  split_adjusted_shares*cumprod(1+unadjusted_dividends/c(Inf,close[-length(close)]))
}