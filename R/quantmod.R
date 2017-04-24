#' @title spread_symbol
#' 
#' @description Spread symbol column across numeric columns
#' 
#'  \code{spread_symbol} moves the symbol column of a 
#'  \code{data.frame} to prefix the numeric columns
#'  so it is not dropped when converting to xts.
#' 
#' @param df  A \code{data.frame} or \code{data.table} with a symbol column
#' @param symbol  The name of the symbol column.
#' @param index  Columns not to rename. Also, attributes of this column are preserved.
#' @param sep  Separator between the symbols and the column names.
#' @param drop If TRUE (the default), symbols with price columns of only NAs will be dropped.
#' 
#' @export
spread_symbol <- function(df, symbol="symbol", index="index", sep=".", drop=TRUE) {
  gather_cols <- names(df)[ ! (names(df) %in% c(symbol, index) )]
  gathered <- tidyr::gather_(df, "key", "value", gather_cols)
  gathered[["__new_column"]] <- paste(gathered[[symbol]], gathered[['key']], sep=sep)
  gathered[[symbol]] <- NULL
  gathered[["key"]] <- NULL
  spreaded <- tidyr::spread_(gathered, "__new_column", "value")
  if( drop ) {
    new_columns <- unique(gathered[["__new_column"]])
    for( new_col in new_columns ) {
      if( all(is.na(spreaded[[new_col]])) ) {
        spreaded[[new_col]] <- NULL
      }
    }
  }
  if( data.table::is.data.table(df) ) {
    spreaded <- setDT(spreaded)
  }
  attributes(spreaded[[index]]) <- attributes(df[[index]])
  spreaded
}

#' @title gather_symbol
#' 
#' @description Create symbol column by peeling prefix from column names
#' 
#'  \code{gather_symbol} creates the symbol column of a 
#'  \code{data.frame} from the prefix of other columns
#'  for use after converting from quantmod xts symbols.
#' 
#' @param df  A \code{data.frame} or \code{data.table} with a symbol column
#' @param symbol  The name of the symbol column.
#' @param index  Columns not to rename. Also, attributes of this column are preserved.
#' @param sep  Separator between the symbol prefix and the column names.
#' @param drop If TRUE (the default), symbols with dates of only NAs will be dropped.
#' 
#' @export
gather_symbol <- function(df, symbol="symbol", index="index", sep=".", drop=TRUE) {
  has_prefix <- names(df)[ grepl(sep, names(df), fixed=TRUE)]
  gathered <- tidyr::gather_(df, "key", "value", has_prefix)
  regex <- paste0("(.*)\\Q", sep, "\\E(.*)")
  extracted <- tidyr::extract_(gathered, "key", c(symbol, "price_type"), regex)
  spreaded <- tidyr::spread_(extracted, "price_type", "value")
  if( drop ) {
    price_cols <- unique(extracted[["price_type"]])
    spreaded <- spreaded[ apply( !is.na(spreaded[, price_cols, drop=FALSE]), 1, any), , drop=FALSE]
  }
  # Now sort by symbol for testing...not really necessary
  by_symbol <- spreaded[order(spreaded[[symbol]]), , drop=FALSE]
  rownames(by_symbol) <- NULL
  # Back to important stuff
  attributes(by_symbol[[index]]) <- attributes(df[[index]])
  if( data.table::is.data.table(df) ) {
    setDT(by_symbol)
  }
  by_symbol
}