#' @import data.table
NULL

#' @title as.xts.data.table
#' 
#' @description convert data.table to xts. symbol column
#' will be joined to nonindex field names.
#' 
#' \code{as.xts} is specialized in \code{adjustr}
#' to transform an \code{xts} indexed by a date to 
#' a \code{data.table} with a Date field.
#' 
#' @param x data.table to convert to xts, must have \emph{POSIXct} in field called "index".
#' @param ... additional arguments to pass to xts constructor such as attributes, but not
#' order.by.
#' @export as.xts.data.table
as.xts.data.table <- function(x, ...) {
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
            order.by = x[, index], ... )
}

#' @title as.data.table.xts
#' @description convert xts to data.table with POSIXct index. Split anything 
#' preceding "." as a symbol column.
#' 
#' Soon to be in data.table so can be phased out.
#' 
#' \code{as.data.table} is specialized in \code{adjustr}
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
  setnames(dt, "rn", "index")
  new_index <- zoo::index(x)
  attr(new_index, "tclass") <- NULL
  dt[, index := new_index]
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
