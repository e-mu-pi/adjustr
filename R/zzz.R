.onLoad <- function(libname, pkgname) {
  op <- options()
  op_adjustr <- list(
    adjustr.cache_dir = file.path( getwd(), 'adjustr_cache'),
    getSymbols.auto.assign = FALSE,
    loadSymbols.auto.assign = FALSE
    )
  to_set <- !( names(op_adjustr) %in% names(op) )
  if ( any(to_set) ) options( op_adjustr[to_set])
  
  cache_dir <- getOption("adjustr.cache_dir")
  if( ! "adjustr.cache_dir" %in% names(op) ) {
    packageStartupMessage("adjustr defaulting to local cache dir:",
                          cache_dir,
                          "\nSet option adjustr.cache_dir to set different cache directory.")
  }
  
  invisible()
}
