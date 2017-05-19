# Load cached symbol data
# 
# \code{load_cache} loads data stored in the cache
# 
# @param cache_file The cache file to load data from.
#
# @return data.table of cached results
load_cache <- function(cache_file) {
  loaded_names <- load(cache_file)
  if( ! "data" %in% loaded_names ) {
    stop("No data found in cache file: ", cache_file)
  }
  data
}

# Save symbol data to cache
# 
# \code{save_cache} saves data to the cache
# 
# @param data  The data to save
# @param cache_file The cache file to load data from.
#
save_cache <- function(data, cache_file) {
  # Could manage cache size here.
  save(data, cache_file)
}

# File in which to cache symbol data
#
# \code{get_cache_file} determines the file where symbol data should
# be cached.
#
# @param symbol  The symbol to cache.
# @param start_date  The first date in the file.
#
# @return A file name where the symbol data will be stored.
get_cache_file <- function(symbol, start_date) {
  cache_dir <- get_cache_dir()
  file.path(cache_dir, 
            paste( paste(symbol,
                         format(start_date, format="%Y%m%d"),
                         sep="_"),
                   "Rdata",
                   sep="."))
}

# Get the cache_dir, creating if needed
get_cache_dir <- function() {
  cache_dir <- getOption("adjustr.cache_dir")
  if( ! file.exists(cache_dir) ) {
    dir.create(cache_dir)
  }
  cache_dir
}