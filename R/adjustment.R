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
