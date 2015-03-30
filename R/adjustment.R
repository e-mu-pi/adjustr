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
  # package name when it retrieves its defaults (gives a warning).
  # Do this rather than importing getSymbols.
  data <- getSymbols(x, ...)
  as.data.table(data)
}

#' Turn splits into evolution of single share.
#' 
#' @param splits A numeric vector of splits. Most values will be 1. A 2:1 split
#' is represented as 0.5. 
#' @return A numeric vector of shares held if splits are applied to an
#' initial position of 1 share. 
#' 
#' @export
make_shares <- function(splits) {
  cumprod(1/splits)
}

#' @export
unadjust <- function(split_adjusted_dividend, splits, ...) UseMethod('unadjust')

#' Turn split adjusted dividends into unadjusted dividends.
#' 
#' Assumes that unadjusted dividends are only accurate to 3 decimal places
#' unless \code{max_decimals} specifies otherwise.
#' 
#' @param split_adjusted_dividend A numeric vector of split adjusted dividends.
#' @param splits A numeric vector of splits.
#' @param max_decimals The number of decimal places that unadjusted dividends may have.
#' @return A numeric vector of unadjusted dividends.
#' 
#' @export
unadjust.default <- function(split_adjusted_dividend, splits, max_decimals = 3) {
  n <- length(splits)
  stopifnot( n == length(split_adjusted_dividend) )
  shares <- make_shares(splits)
  round(split_adjusted_dividend * shares[n] / shares, digits = max_decimals)
}

#' Turn split adjusted dividends into unadjusted dividends.
#' 
#' Assumes that unadjusted dividends are only accurate to 3 decimal places
#' unless \code{max_decimals} specifies otherwise.
#' 
#' @param split_adjusted_dividend A \code{data.table} with a \code{dividend} column and
#' an \code{index} column to merge on. 
#' @param splits A \code{data.table} with columns \code{splits} and \code{index}.
#' @param ... Additional arguments passed to \code{unadjust.default}, for example, \code{max_decimals}.
#' @return A \code{data.table} of merged splits and dividends with new fields \code{shares}
#' and \code{unadjusted_dividend}. The merge matches on the \code{index} field. Missing
#  splits are filled in with 1. Missing dividends are filled in with 0.
#' 
#' @export
unadjust.data.table <- function(split_adjusted_dividend, splits, ...) {
  stopifnot( is.data.table(splits), 
             all( c('index', 'dividend') %in% names(split_adjusted_dividend) ),
             all( c('index', 'splits') %in% names(splits) ) )
  just_splits <- all( names(splits) %in% c('index', 'splits') )
  just_dividends <- all( names(split_adjusted_dividend) %in% c('index', 'dividend') )
  
  div_not_subset_splits <- ! all( split_adjusted_dividend[, index] %in% splits[, index] )
  splits_not_subset_div <- ! all( splits[,index] %in% split_adjusted_dividend[, index] )
  
  if( splits_not_subset_div && ! just_dividends ) {
    stop("unadjust can't fill non-dividend columns when merging with splits")
  } else if( div_not_subset_splits && ! just_splits ) {
    stop("unadjust can't fill non-split columns when merging with dividends")
  } else  { #if( div_not_subset_splits || splits_not_subset_div ) {
    merged_data <- merge(split_adjusted_dividend, splits, by = 'index', all = TRUE)
    merged_data[ is.na(splits), splits := 1]
    merged_data[ is.na(dividend), dividend := 0]
  } 
#   else { # splits and dividends must have the same index 
#     merged_data
#   }
  merged_data[, shares := make_shares(splits)]
  merged_data[, unadjusted_dividend := unadjust(dividend, splits, ...)]
}

#' Turn split adjusted dividends into unadjusted dividends.
#' 
#' Assumes that unadjusted dividends are only accurate to 3 decimal places
#' unless \code{max_decimals} specifies otherwise.
#' 
#' @param split_adjusted_dividend An \code{xts} of dividends, either 1 column or with a column name
#' matching dividend.
#' @param splits An \code{xts} of splits, either 1 column or with a column \code{spl}.
#' @param ... Additional arguments passed to unadjust.default, for example, \code{max_decimals}.
#' @return An \code{xts} of merged splits and dividends with new fields \code{shares}
#' and \code{unadjusted_dividend}. Missing splits are filled in with 1. Missing dividends are filled in with 0.
#' 
#' @export
unadjust.xts <- function(split_adjusted_dividend, splits, ...) {
  if( all(is.na(splits)) ) { #NA to support quantmod::getSplits 
    splits <- xts::xts( numeric(), as.Date( as.character(), tz = 'UTC'))
  }
  stopifnot( xts::is.xts(splits), 
             ncol(split_adjusted_dividend) ==1 || 
               any(grepl("dividend", colnames(split_adjusted_dividend),
                         ignore.case = TRUE)),
             ncol(splits) == 1 ||
               any( grepl("spl", colnames(splits), ignore.case = TRUE ) ) )
  if( ncol(splits) == 1 ) {
    split_col <- colnames(splits) 
  } else {
    split_col <- colnames(splits)[ grepl("spl", colnames(splits), ignore.case = TRUE)]
  }
  if( ncol(split_adjusted_dividend) == 1) {
    dividend_col <- colnames(split_adjusted_dividend)
  } else {
    dividend_col <- colnames(split_adjusted_dividend)[ grepl("dividend", 
                                          colnames(split_adjusted_dividend), 
                                          ignore.case = TRUE)]
  }
  if( is.null(dividend_col) ) { #the case for quantmod::getDividends
    colnames(split_adjusted_dividend) <- "dividend"
    dividend_col <- "dividend"
  }
  if( is.null(split_col) ) { #not a real issue, but to fully support 1 column split xts
    colnames(splits) <- "splits"
    split_col <- "splits"
  }
  if( nrow(splits) == 0 && nrow(split_adjusted_dividend) == 0 ) {
    merged_data <- xts::xts( data.frame( dividend = numeric(),
                                         splits = numeric(),
                                         shares = numeric(),
                                         unadjusted_dividend = numeric() ),
                             order.by = as.Date( as.character(), tz = 'UTC') )
  } else {
    just_splits <- all( colnames(splits) %in% split_col )
    just_dividends <- all( colnames(split_adjusted_dividend) %in% dividend_col )
    
    div_not_subset_splits <- ! all( zoo::index(split_adjusted_dividend) %in% 
                                      zoo::index(splits) )
    splits_not_subset_div <- ! all( zoo::index(splits) %in% 
                                      zoo::index(split_adjusted_dividend) )
    
    if( splits_not_subset_div && ! just_dividends ) {
      stop("unadjust can't fill non-dividend columns when merging with splits")
    } else if( div_not_subset_splits && ! just_splits ) {
      stop("unadjust can't fill non-split columns when merging with dividends")
    } else  { #if( div_not_subset_splits || splits_not_subset_div ) {
        
      if( ! identical( zoo::index(splits), zoo::index(split_adjusted_dividend) ) ) {
        merged_data <- xts::merge.xts(split_adjusted_dividend, splits, fill = 0)
        n <- length(merged_data)
        if( ! dividend_col %in% colnames(merged_data) ) {
          merged_data <- cbind( xts::xts( data.frame( dividend = rep_len(0, n) ),
                                          order.by = zoo::index(merged_data) ),
                                merged_data)
        }
        if( ! split_col %in% colnames(merged_data) ) {
          merged_data <- cbind( merged_data,
                                xts::xts( data.frame( splits = rep_len(1, n) ),
                                          order.by = zoo::index(merged_data) ) )
        } else {
          merged_data[ merged_data[,split_col] ==0, split_col] <- 1
        }
      } else {
        merged_data <- cbind(split_adjusted_dividend, splits)
      }
      merged_data$shares <- xts::xts( make_shares( 
                                        as.numeric(merged_data[,split_col] ) ),
                                      order.by = zoo::index(merged_data) )
      merged_data$unadjusted_dividend <- 
        xts::xts( unadjust( as.numeric( merged_data[,dividend_col]), 
                            as.numeric( merged_data[,split_col]), 
                            ...),
                  order.by = zoo::index(merged_data) )
    }
  }
  merged_data
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
make_raw_value <- function(price_data, dividend, ..., splits) {
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
#' @export
make_raw_return <- function(x, ...) UseMethod("make_raw_return")

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
#'   as \code{rawdividend} as returned by \code{make_raw_value}.
#' @param close The historical closing prices with no adjustments.
#' @param initial_rawclose Optional starting value to measure returns against. Default is
#' NULL, i.e., just use the first ticks from the other data vectors. If used, the first 
#' dividend will be included.
#' 
#' @return A numeric vector the same length as the inputs with the arithmetic return from
#' the first close including dividends.
#' 
#' @export
make_raw_return.default <- function(split_adjusted_shares, split_unadjusted_dividends, close, 
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

#' Compute period returns as value change plus dividends.
#' 
#' @param raw_value_xts An \code{xts} object with columns \code{close, rawshares, rawdividend}
#' or with a symbol \code{SYM}, columns \code{SYM.Close, SYM.Rawshares, SYM.Rawdividend}.
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
make_raw_return.xts <- function(raw_value_xts, period = 'monthly') {
  period_opts <- list(daily = "days", weekly = "weeks", monthly = "months", 
                      quarterly = "quarters", yearly = "years", annually = "years")
  end_points <- xts::endpoints( index(raw_value_xts), 
                                on = period_opts[[period]])
  end_points <- end_points[-1] #drop initial 0
  if( ! 1 %in% end_points ) end_points <- c(1, end_points)
  n <- length(end_points)
  raw_ret_dt <- make_raw_return( as.data.table(raw_value_xts), 
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
    # -1: indexing is off by 1 because make_raw_return will not have a return on the
    # first tick
    raw_ret <- as.xts( raw_ret_dt[end_points[-1]-1,] ) 
  }
  colnames(raw_ret) <- paste(period, "rawreturn", sep = "_")
  raw_ret
} 

#' Compute raw return between start and end.
#' 
#' @param raw_value_data A data.frame of raw_value data as produced by make_raw_data.
#' @param start An index vector (numeric or logical) of starting points.
#' @param end An index vector (numeric or logical) of ending poitns.
#' @return A data.frame of raw returns with columns index and rawreturn. 
#' If the (start,end] ranges do not overlap, the returns are a running calculation from
#' each start indexed from start+1 to end. If there the (start,end] ranges do not partition
#' the index of the input, the missing indexes are omitted (especially useful when looking
#' at trade returns in which you are not always in the market). If there is overlap in
#' the (start,end] ranges, there is just one return calculated for each end indexed by the 
#' index at end.
#' 
#' @export
make_raw_return.data.table <- function(raw_value_data, start, end) {
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
  n_intervals <- length(start_index)

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
    raw_ret <- raw_value_data[(on_period),  list(rawreturn = make_raw_return(last(rawshares), 
                                                      overlapped_dividends[.GRP], 
                                                      last(close), 
                                                      initial_rawclose[.GRP]) ), 
                               by = period_index]
  } else {
    raw_ret <- raw_value_data[(on_period), 
                               list(rawreturn = make_raw_return(rawshares, 
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
#' for splits. Yahoo adjusts their dividends for splits. The make_raw_value function can create
#' raw dividends (i.e., undo the split adjustment).
#' 
#' @return A numeric vector of shares after splits and reinvestment of dividends.
#' 
#' @export
make_reinvested_shares <- function(close, split_adjusted_shares, unadjusted_dividends) {
  split_adjusted_shares*cumprod(1+unadjusted_dividends/c(Inf,close[-length(close)]))
}

#' Compute period returns assuming reinvestment of dividends.
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
make_reinvested_return <- function(x, ...) UseMethod("make_reinvested_return")

#' Compute reinvested return from split adjusted shares, dividends, and closing prices.
#' 
#' Computes the return of buying the given number of shares at the first closing price
#' and exiting at any intermediate closing price including splits and dividends. The
#' number of shares will change based on splits and reinvesting dividends; 
#' no accounting is done to rebase
#' the return for other purchases and sales of shares. The dividends must be as they would
#' historically have been reported, not split adjusted as Yahoo provides. A dividend on 
#' the first tick is not included as Yahoo reports them on the ex-date. 
#' 
#' @param close The historical closing prices with no adjustments.
#' @param split_adjusted_shares Numeric vector of number of shares accounting
#'   for share splits. Typically, starts with 1 and adjusts for each split to reflect what
#'   that 1 share has become.
#' @param unadjusted_dividends Numeric vector of dividends as they would 
#'   have been announced historically. The values returned from Yahoo are split adjusted. 
#'   For example, a dividend of 0.2 that occured before a 2:1 split would be reported as
#'   0.1 by Yahoo (assuming no subsequent splits). This function expects the 0.2. Available
#'   as \code{rawdividend} as returned by \code{make_raw_value}.
#' @param initial_close Optional starting close to measure returns against. Default is
#' NULL, i.e., just use the first tick from the close vector. If used, the first 
#' dividend will be included and initial_shares must be provided. 
#' @param initial_shares Optional starting shares to measure returns against. Default is NULL,
#' i.e., just use the first tick in the split_adjusted_shares vector. If used, the 
#' initial_close must be provided.
#' 
#' @return A numeric vector the same length as the inputs with the arithmetic return from
#' the first close assuming dividends are reinvested in shares per make_reinvested_shares.
#' 
#' @export
make_reinvested_return.default <- function(close, split_adjusted_shares, unadjusted_dividends,  
                                    initial_close = NULL, initial_shares = NULL) {
  if( is.null(initial_close) && is.null(initial_shares) ) {
    reinvested_shares <- make_reinvested_shares( close, split_adjusted_shares, unadjusted_dividends)
    initial_value <- reinvested_shares[1] * close[1]
  } else {
    reinvested_shares <- make_reinvested_shares( c(initial_close, close), 
                                                 c(initial_shares, split_adjusted_shares), 
                                                 c(0, unadjusted_dividends) )[-1]
    initial_value <- initial_shares * initial_close
  }
  (close*reinvested_shares - initial_value ) / initial_value
}

#' Compute reinvested return between start and end.
#' 
#' @param price_data A data.frame of prices as produced by make_raw_data. In particular,
#' requires columns index, close, rawshares, and rawdividend.
#' @param start An index vector (numeric or logical) of starting points.
#' @param end An index vector (numeric or logical) of ending poitns.
#' @return A data.frame of reinvested returns with columns index and reinvested_return. 
#' If the (start,end] ranges do not overlap, the returns are a running calculation from
#' each start indexed from start+1 to end. If there the (start,end] ranges do not partition
#' the index of the input, the missing indexes are omitted (especially useful when looking
#' at trade returns in which you are not always in the market). If there is overlap in
#' the (start,end] ranges, there is just one return calculated for each end indexed by the 
#' index at end.
#' 
#' @export
make_reinvested_return.data.table <- function(price_data, start, end) {
  n <- nrow(price_data)
  
  if( xts::timeBased(end) ) {
    end_index <- price_data[, which(as.IDate(index) %in% as.IDate(end)) ]
  } else {
    end_index <- end
  }
  if( xts::timeBased(start) ) {
    start_index <- price_data[, which(as.IDate(index) %in% as.IDate(start)) ]
  } else {
    start_index <- start
  }
  n_intervals <- length(start_index)  
  
  price_data[, period_index := 0]
  price_data[end_index, period_index := 1]
  price_data[start_index, period_index := period_index + 1]
  price_data[, period_index := c(0, cumsum( period_index)[-n]) ]
  price_data[, on_period := period_index %in% period_index[end_index]]
  
  initial_close <- price_data[start_index, close]
  initial_shares <- price_data[start_index, rawshares]
  overlap <- any( start_index[-1] < end_index[-n_intervals])
  if( overlap ) {
#     overlapped_shares <- numeric(length(initial_rawclose))  
    overlapped_dividend <- numeric(length(initial_close))  
    for( idx in seq_along(start_index)) {
#       overlapped_shares[idx] <- price_data[start_index[idx]:end_index[idx],
#                                            make_reinvested_shares( close, rawshares, rawdividend ) ]
      overlapped_dividend[idx] <- price_data[start_index[idx]:end_index[idx],
               ( last( make_reinvested_shares( close, rawshares, rawdividend) ) /
                  last(rawshares) - 1 ) *
                 initial_close[idx] ]
    }
#     overlapped_dividend <- overlapped_shares
    ret_data <- price_data[(on_period),  list(reinvested_return = 
                                                make_reinvested_return(last(close),
                                                                       last(rawshares), 
                                                                       overlapped_dividend[.GRP], 
                                                                       initial_close[.GRP],
                                                                       initial_shares[.GRP]) ), 
                              by = period_index]
  } else {
    ret_data <- price_data[(on_period), 
                              list(reinvested_return = make_reinvested_return(close, 
                                                                              rawshares, 
                                                                              rawdividend, 
                                                                              initial_close[.GRP],
                                                                              initial_shares[.GRP])), 
                              by = period_index]
  }
  ret <- data.table( index = price_data[(on_period), index],
                     reinvested_return = ret_data[,reinvested_return])
  price_data[, period_index := NULL]
  price_data[, on_period := NULL]
  ret
  # Dividends are paid to the holder on the previous day in Yahoo data.
  # Therefore, they need to be offset by 1 as to which period they are
  # paid in. A dividend on the initial day should not be included because
  # it would be paid in the previous period. If the initial period is partial,
  # I suppose it should be included, but currently it's not. The first period
  # isn't perfect anyway because we don't have the true starting price.
  
  
}

#' Compute period returns by reinvesting within period dividends.
#' 
#' @param price_xts An \code{xts} object with columns \code{close, rawshares, rawdividend}
#' or with a symbol \code{SYM}, columns \code{SYM.Close, SYM.Rawshares, SYM.Rawdividend}.
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
make_reinvested_return.xts <- function(price_xts, period = 'monthly') {
  period_opts <- list(daily = "days", weekly = "weeks", monthly = "months", 
                      quarterly = "quarters", yearly = "years", annually = "years")
  end_points <- xts::endpoints( index(price_xts), 
                                on = period_opts[[period]])
  end_points <- end_points[-1] #drop initial 0
  if( ! 1 %in% end_points ) end_points <- c(1, end_points)
  n <- length(end_points)
  reinvested_ret_dt <- make_reinvested_return( as.data.table(price_xts), 
                                 start = end_points[-n],
                                 end = end_points[-1] )
  if ( period == 'daily' ) {
    initial_zero_to_match_quantmod <- data.table(index = index(price_xts[1,]),
                                                 reinvested_return = 0)
    reinvested_ret <- as.xts( rbind( initial_zero_to_match_quantmod,
                              reinvested_ret_dt ) )
  } else {
    # dt version gives intermediate returns between start and end
    # to match quantmod, only return the values at endpoints
    # end_points[-1]: remove initial starting point
    # -1: indexing is off by 1 because make_reinvested_return will not have a return on the
    # first tick
    reinvested_ret <- as.xts( reinvested_ret_dt[end_points[-1]-1,] ) 
  }
  colnames(reinvested_ret) <- paste(period, "reinvested_return", sep = "_")
  reinvested_ret
} 
