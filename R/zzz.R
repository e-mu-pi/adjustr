.onLoad <- function(libname, pkgname) {
  op <- options()
  op_adjustr <- list(
    adjustr.cache_dir = file.path( getwd(), 'adjustr_cache'),
    getSymbols.auto.assign = FALSE,  # need to explicity override these in our calls
    loadSymbols.auto.assign = FALSE  # rude to stomp on another package
    )
  to_set <- !( names(op_adjustr) %in% names(op) )
  if ( any(to_set) ) options( op_adjustr[to_set])

  invisible()
}

.onAttach <- function(libname, pkgname) {
  if( ! "adjustr.cache_dir" %in% names(options()) ) {
    options(adjustr.cache_dir = file.path( getwd(), 'adjustr_cache'))
    cache_dir <- getOption("adjustr.cache_dir")
    packageStartupMessage("adjustr defaulting to local cache dir:",
                          cache_dir,
                          "\nSet option adjustr.cache_dir to set different cache directory.")
  }
  
  invisible()
}
