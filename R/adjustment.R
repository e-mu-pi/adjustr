#' @import data.table
NULL

#' Retrieve stock prices as data.table
#' 
#' \code{getDTSymbols} adapts \code{quantmod::getSymbols} to convert
#' results to \code{data.table}. 
#' 
#' @export
getDTSymbols <- function(x, ...) {
  data <- quantmod::getSymbols(x, ...)
  as.data.table(data)
}

#' Make additive price adjustments without lookforward bias.
#' 
#' Apply dividend and split data to price series without changing
#' past data. Changes are applied additively, perhaps multiplicative 
#' will be added in the future. A dividend paid at time t will increase
#' the value at time t and all future times. A split at time t will 
#' apply a factor at time t and all future times. The TrueValue and
#' TrueShares will be added.
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
true_value <- function(price_data, dividend, ..., splits) {
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
      price[, trueshares := 1 / cumprod(split), by = symbol ]
    } else {
      price[, trueshares := 1 / cumprod(split)]
    }
    price[, truevalue := trueshares * close]
  } else {
    price[, split := 1]
    price[, trueshares := 1]
    price[, truevalue := close]
  }
  if( has_some(dividend, "dividend") ) {
    dividend[, index := as.Date(index)]
    if( has_some(splits, "split") ) {
      dividend <- merge(dividend, splits, by = merge_key, all = TRUE)
      dividend[is.na(dividend), dividend := 0]
      dividend[is.na(split), split := 1]
      if( has_symbol(price) ) {
        dividend[, trueshares := 1/cumprod(split), by = symbol]
        dividend[, retroactive_shares := last(trueshares) / trueshares, by = symbol]
        dividend[, truedividend := retroactive_shares * dividend, by = symbol]
      } else {
        dividend[, trueshares := 1/cumprod(split)]
        dividend[, retroactive_shares := last(trueshares) / trueshares]
        dividend[, truedividend := retroactive_shares * dividend]
      }
      dividend[, split := NULL]
      dividend[, trueshares := NULL] 
      dividend[, retroactive_shares := NULL] #maybe keep this
    } else {
      dividend[, truedividend := dividend]
    }
    price <- merge(price, dividend, by = merge_key, all.x = TRUE)
    price[is.na(dividend), dividend := 0]
    price[is.na(truedividend), truedividend := 0]
    if( has_symbol( price ) ) {
      price[, truevalue := truevalue + cumsum(trueshares * truedividend), by = symbol]
    } else {
      price[, truevalue := truevalue + cumsum(trueshares * truedividend)]
    }
  } else {
    price[, dividend := 0]
    price[, truedividend := 0]
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
#' @param true_value_data A \code{data.table} including columns \code{index, close, trueshares, truedividends}. 
#' The \code{index} column must be a valid type for \code{xts} indexing. Multi-symbol data
#' is not supported.
#' @param period A string indicating a granularity. See quantmod::periodReturn.
#' 
#' @return An \code{xts} object ordered by the \code{index} column granulated to period. The
#' returns are calculated as close to close values multiplied by trueshares to account for
#' splits and adding in dividends over the period. Note that a dividend on date t in Yahoo 
#' data is only paid to those holding at the close on date t-1. Therefore, there a dividend
#' on date t for which t is also the period start will be recorded in the period ending on date
#' t, not the next period starting on t. In particular, daily returns include a dividend on
#' date t in the return from t-1 to t, recorded as the return on date t.
#' 
#' Note that daily true returns for one week do not simply convert to weekly returns as prod(1+r).
#' Each day the basis for return is the close, but the return comes from the following close
#' plus dividends. Those dividends are not included in the basis for the following day.
#' 
#' @export
true_return <- function(true_value_data, period = 'monthly') {
  period_opts <- list(daily = "days", weekly = "weeks", monthly = "months", 
                  quarterly = "quarters", yearly = "years", annually = "years")
  end_points <- xts::endpoints(as.xts(true_value_data[,index]), on = period_opts[[period]])
#   end_points <- end_points[-1] # first is always zero, no?
  
#   dividend_paid_points <- end_points-1
  n_points <- length(end_points)
  n <- nrow(true_value_data)
# 
#   if( length(end_points) == 1 ) {
#     
#   }

  true_value_data[, period_index := 0]
  true_value_data[end_points, period_index := 1]
  true_value_data[, period_index := cumsum(period_index)]

  # Dividends are paid to the holder on the previous day in Yahoo data.
  # Therefore, they need to be offset by 1 as to which period they are
  # paid in. A dividend on the initial day should not be included because
  # it would be paid in the previous period. If the initial period is partial,
  # I suppose it should be included, but currently it's not. The first period
  # isn't perfect anyway because we don't have the true starting price.
  true_value_data[, dividend_paid_index := c(0, period_index[-n])]

  true_value_data[, trueclose := close * trueshares]
  true_value_data[, inperiod_dividend := truedividend]
  true_value_data[1, inperiod_dividend := 0]
  true_value_data[, dividends_paid := cumsum(inperiod_dividend * trueshares),
                  by = dividend_paid_index]
  true_value_data[, trueclose_plus_period_dividends := trueclose + dividends_paid]
#   setkey(true_value_data, "period_index")
#   trueclose_plus_div <- div[true_value_data, last(dividends_paid) + last(trueclose),
#                                         by = period_index]
                        
  end_points[1] <- 1
  truereturn <- quantmod::Delt( true_value_data[end_points[-n_points],trueclose], 
                                true_value_data[end_points[-1],trueclose_plus_period_dividends])
  true_value_data[, period_index := NULL]
  true_value_data[, dividend_paid_index := NULL]
  true_value_data[, trueclose := NULL]
  true_value_data[, inperiod_dividend := NULL]
  true_value_data[, trueclose_plus_period_dividends := NULL]

  ret <- xts::xts( truereturn, order.by = true_value_data[end_points[-1], index])
  colnames(ret) <- paste(period,"truereturn", sep = "_")
  ret
# quantmod::periodReturn: 
#   xx <- try.xts(x)
#   if (inherits(x, "ts")) {
#     x <- na.omit(try.xts(x))
#     xtsAttributes(x) <- CLASS(x) <- NULL
#     xx <- x
#     TS <- TRUE
#   }
#   else TS <- FALSE
#   if (has.Op(xx) & has.Cl(xx)) {
#     getFirst <- function(X) Op(X)
#     getLast <- function(X) Cl(X)
#   }
#   else getFirst <- getLast <- function(X) X[, 1]
#   on.opts <- list(daily = "days", weekly = "weeks", monthly = "months", 
#                   quarterly = "quarters", yearly = "years", annually = "years")
#   ep <- endpoints(xx, on = on.opts[[period]])
#   ret <- Delt_(Cl(to_period(xx, period = on.opts[[period]], 
#                             ...)), type = type)
#   if (leading) {
#     firstval <- as.numeric(Delt_(getFirst(xx[1]), getLast(xx[ep[2]]), 
#                                  type = type))
#     ret[1, ] <- firstval
#   }
#   colnames(ret) <- paste(period, "returns", sep = ".")
#   if (TS) 
#     xx <- 1
#   tmp.ret <- reclass(ret, xx[ep[-1]])
#   if (is.null(subset)) 
#     subset <- "/"
#   reclass(as.xts(tmp.ret)[subset])
}
