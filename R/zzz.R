.onLoad <- function(libname, pkgname) {
  op <- options()
  op_mydatar <- list(
    mydatar.cache_dir = file.path( getwd(), 'mydatar_cache'),
    getSymbols.auto.assign = FALSE,
    loadSymbols.auto.assign = FALSE
    )
  to_set <- !( names(op_mydatar) %in% names(op) )
  if ( any(to_set) ) options( op_mydatar[to_set])
  
  cache_dir <- getOption("mydatar.cache_dir")
  if ( ! file.exists(cache_dir) ) dir.create( cache_dir )
  
  invisible()
}