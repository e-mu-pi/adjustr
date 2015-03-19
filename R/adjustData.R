#' @import data.table
NULL

#' @title as.xts.data.table
#' 
#' @description convert data.table to xts. symbol column
#' will be joined to nonindex field names.
#' 
#' \code{as.xts} is specialized in \code{mydatar}
#' to transform an \code{xts} indexed by a date to 
#' a \code{data.table} with a Date field.
#' 
#' @param x data.table to convert to xts, must have \emph{POSIXct} in field called "index".
#' @export as.xts.data.table
as.xts.data.table <- function(x) {
  stopifnot( requireNamespace("xts"), !missing(x), data.table::is.data.table(x) )
  columns <- names(x)
  if( ! "index" %in% columns ) stop("index must be a field to convert to xts")
  if( "symbol" %in% columns ) {
    `%>%` <- dplyr::`%>%`
    capitalize <- function(s) paste0( toupper(substring(s, 1, 1)),
                                      substring(s, 2) )
    x <- x %>% 
      tidyr::gather(key, value, -index, -symbol)
    # data.table may introduce NA columns if not all symbols have the same price types.
    # remove those here.
    x <- x[, .SD[(!all(is.na(value)))], by = c("symbol", "key")] %>% 
      dplyr::mutate( dirty_column = paste(symbol, capitalize(key), sep = ".")) %>%
      dplyr::select( -symbol, -key) %>%
      tidyr::spread(dirty_column, value)
    columns <- names(x)
  } 
  xts::xts( x[, columns[! columns %in% c("index")], with = FALSE],
            order.by = x[, index] )
}

#' @title as.data.table.xts
#' @description convert xts to data.table with POSIXct index. Split anything 
#' preceding "." as a symbol column.
#' 
#' Soon to be in data.table so can be phased out.
#' 
#' \code{as.data.table} is specialized in \code{mydatar}
#' to transform a \code{data.table} with a Date column to 
#' an \code{xts} indexed by date.
#' 
#' @param x xts to convert to data.table
#' @seealso \link{as.xts.data.table}
#' 
#' @export
as.data.table.xts <- function(x) {
  stopifnot( requireNamespace("xts"), !missing(x), xts::is.xts(x) )
  dt <- as.data.table( as.data.frame(x),
                 keep.rownames = TRUE)
#   dt <- dplyr::rename( dt, index = rn)
  setnames(dt, "rn", "index")
#   if( inherits(dt[,index], "character") ) {
  new_index <- zoo::index(x)
  attr(new_index, "tclass") <- NULL
    dt[, index := new_index] #Dates seem to get changed to characters; POSIXct worked without
#     attr(dt[["index"]], "tclass") <- NULL #allow as.xts and as.data.table to invert each other
#   setattr(dt[,index], "tclass", NULL)
#   }
  has_dot <- grepl("\\.", names(dt))
  key_cols <- "index"
  if( any( has_dot ) ) {
    key_cols <- c("index", "symbol")
    `%>%` <- dplyr::`%>%`
    decapitalize <- function(s) paste0( tolower(substring(s, 1, 1)),
                                      substring(s, 2) )
    dt <- dt %>%
      tidyr::gather(key, value, one_of(names(dt)[has_dot]) ) %>%
      tidyr::extract(key, c("symbol", "price_type"), "(.*)\\.(.*)") %>%
      dplyr::mutate(price_type = decapitalize(price_type) )
    #xts may have inserted NA to get matching index; remove that here
    dt <- dt[, .SD[(!all(is.na(value)))], by = c("index", "symbol")] %>%
      tidyr::spread(price_type, value)
    setorder(dt, symbol, index)
  }
  dt
}

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
#   if( ! "Close" %in% names(price) ) {
#     normalize_price <- FALSE
#     price <- melt( price, )
#   }
  if( is_xts ) {
    xts_attr <- xts::xtsAttributes(price_data)
    price <- as.data.table(price_data)
  } else {
    price <- copy(price_data)
  }
  has_symbol <- function(x) "symbol" %in% names(x)
  if ( has_symbol(price) ) {
    merge_key <- c("symbol", "index")
    available_symbols <- price[, unique(symbol)]
  } else {
    merge_key <- c("index")
  }
  multi_symbol <- has_symbol(price) && length(available_symbols) > 1
#   if ( multi_symbol ) {
#     merge_key <- c("index", "symbol")
#   } else {
#     merge_key <- c("index")
#   }
  if( xts::is.xts(dividend) ) {
    dividend <- as.data.table(dividend)
    if( all( names(dividend) %in% c("index", "V1") ) ){
#       dividend <- dplyr::rename(dividend, dividend = V1)
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
#   dividend <- normalize(dividend)
  normalize(dividend)
  if( xts::is.xts(splits) ) {
    splits <- as.data.table(splits)
    if( "spl" %in% names(splits) ) {
#       splits <- dplyr::rename(splits, split = spl)
      setnames(splits, "spl", "split")
    }
    if( all( names(splits) %in% c("index", "V1") ) ){
#       splits <- dplyr::rename(splits, split = V1)
      setnames(splits, "V1", "split")
    }
  }
#   splits <- normalize(splits)
  normalize(splits)
  index_class <- class(price[,index])
  is_date <- all( class(price[,index]) == "Date" )
  if( ! is_date ) {
    price[, index := as.Date(index)]
  }
  if( ! is.null(splits) && ! all(is.na(splits[,split])) ) {
    splits[, index := as.Date(index)]
    price <- merge( price, splits, by = merge_key, all.x = TRUE)
    price[is.na(split), split := 1]
    if ( has_symbol(price) ) {
      price[, trueshares := 1 / cumprod(split), by = symbol ]
#       price[, trueshares := cumprod(trueshares), by = symbol]
    } else {
#       price[, trueshares := 1 / split]
#       price[, trueshares := cumprod(trueshares)]
      price[, trueshares := 1 / cumprod(split)]
    }
#     na.locf <- function(x) zoo::na.locf(x, na.rm = FALSE)
#     price <- dplyr::mutate_each_(price, dplyr::funs( na.locf ), lazyeval::interp( ~ matches(x), x = "trueshares" ) )
    
    # Alternate TrueShare Calculation...a lot of unnecessary multiplication by 1
#     price[is.na(Split), Split := 1]
#     price <- data.table::merge.data.table( price, split, by = Date, all = TRUE)
#     price[, TrueShare := 1 / cumprod(Split) ]
    price[, truevalue := trueshares * close]
    # Might have split dates with no price data
#     price <- dplyr::mutate_each_(price, dplyr::funs( zoo::na.locf ), ~ends_with("Close") )
  } else {
    price[, split := 1]
    price[, trueshares := 1]
    price[, truevalue := close]
  }
  if( ! is.null(dividend) && ! all(is.na(dividend[,dividend])) ) {
    dividend[, index := as.Date(index)]
    price <- merge(price, dividend, by = merge_key, all.x = TRUE)
    price[is.na(dividend), dividend := 0]
    if( has_symbol( price ) ) {
#       final_shares <- price[, last(true_shares), by = symbol ]
      price[, truedividend := round( dividend * ( last(trueshares) / trueshares ),
                                     3), by = symbol ] # Yahoo gives split-adjusted dividends
      price[, truevalue := truevalue + cumsum(trueshares * truedividend), by = symbol]
#       price[, truevalue := truevalue + cumsum(truedividend), by = symbol]
    } else {
#       final_shares <- price[, last(true_shares)]
      price[, truedividend := round(dividend * (last(trueshares)/trueshares),
                                    3)] # Yahoo gives split-adjusted dividends
#       price[, truevalue := truevalue + cumsum(truedividend)]
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