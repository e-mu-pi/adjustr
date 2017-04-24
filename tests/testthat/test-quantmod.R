context("quantmod")
library(data.table)

test_that("spread_symbol makes symbol prefix on other columns",{
  df <- data.frame( symbol=c("a", "a", "b", "b"),
                    index=c(9,10,9,10),
                    x = c(1,2,3,4),
                    y = c(5,6,7,8))
  expect_equal( spread_symbol(df),
                data.frame(index=c(9,10),
                           a.x = c(1,2), a.y=c(5,6),
                           b.x = c(3,4), b.y=c(7,8)))
  unbalanced <- df[-1,]
  expect_equal( spread_symbol(unbalanced),
                data.frame(index=c(9,10),
                           a.x=c(NA,2), a.y=c(NA,6),
                           b.x=c(3,4), b.y=c(7,8)))
  alt_names <- df
  colnames(alt_names) <- c("syms","t","x","y")
  expect_equal( spread_symbol(alt_names, symbol="syms", index="t", sep="_"),
                data.frame(t=c(9,10),
                           a_x=c(1,2), a_y=c(5,6),
                           b_x=c(3,4), b_y=c(7,8)))
  dt <- as.data.table(df)
  expect_equal( spread_symbol(dt),
                data.table(index=c(9,10),
                           a.x = c(1,2), a.y=c(5,6),
                           b.x = c(3,4), b.y=c(7,8)))
  keeps_index_attr <- copy(dt)
  attr(keeps_index_attr$index, "something") <- 5
  expected <- data.table(index=c(9,10),
                         a.x = c(1,2), a.y=c(5,6),
                         b.x = c(3,4), b.y=c(7,8))
  attr(expected$index, "something") <- 5
  expect_equal( spread_symbol(keeps_index_attr),
                expected)
  
  mixed_price_types <- data.frame(index=c(9,10,9,10),
                  symbol=c("a", "a", "b", "b"),
                  x=c(1,2,NA,NA),
                  y=c(5,6,3,4),
                  z=c(NA, NA, 7,8))
  expect_equal( spread_symbol(mixed_price_types),
                data.frame(index=c(9,10),
                           a.x=c(1,2), a.y=c(5,6),
                           b.y=c(3,4), b.z=c(7,8)))
  expect_equal( spread_symbol(mixed_price_types, drop=FALSE),
                data.frame(index=c(9,10),
                           a.x=c(1,2), a.y=c(5,6), a.z=c(NaN, NaN),
                           b.x=c(NaN,NaN), b.y=c(3,4), b.z=c(7,8)) )

  one_symbol_one_column <- data.frame(index=c(9,10),
                           symbol=c("a","a"),
                           x=c(1,2))
  expect_equal( spread_symbol(one_symbol_one_column),
                data.frame(index=c(9,10),
                           a.x=c(1,2)))
})

test_that("gather_symbol peels off symbols from column names and creates column",{
  make_data_frame <- function(...) {
    data.frame(..., stringsAsFactors = FALSE)
  }
  df <- make_data_frame(index=c(9,10),
             a.x = c(1,2), a.y=c(5,6),
             b.x = c(3,4), b.y=c(7,8))
  expect_equal( gather_symbol(df),
                make_data_frame( index=c(9,10,9,10),
                            symbol=c("a", "a", "b", "b"),
                            x = c(1,2,3,4),
                            y = c(5,6,7,8)) )
  missing_symbol <- make_data_frame(index=c(9,10),
                           a.x=c(NA,2), a.y=c(NA,6),
                           b.x=c(NA,4), b.y=c(7,NA) )
  expect_equal( gather_symbol(missing_symbol),
                make_data_frame( index=c(10,9,10),
                            symbol=c("a", "b", "b"),
                            x = c(2,NA,4),
                            y = c(6,7,NA)) )
  expect_equal( gather_symbol(missing_symbol, drop=FALSE),
                make_data_frame( index=c(9,10,9,10),
                            symbol=c("a", "a", "b", "b"),
                            x = c(NA,2,NA,4),
                            y = c(NA,6,7,NA)) )
  alt_names <- df
  names(alt_names) <- c("t", "a_x", "a_y", "b_x", "b_y")
  expect_equal( gather_symbol(alt_names, index="t", symbol="syms", sep="_"),
                make_data_frame( t=c(9,10,9,10),
                            syms=c("a", "a", "b", "b"),
                            x = c(1,2,3,4),
                            y = c(5,6,7,8)) )
  dt <- as.data.table(df)
  expect_equal( gather_symbol(dt),
                data.table( index=c(9,10,9,10),
                            symbol=c("a", "a", "b", "b"),
                            x = c(1,2,3,4),
                            y = c(5,6,7,8)) )
  keeps_index_attr <- copy(dt)
  attr(keeps_index_attr$index, "something") <- 5
  expected <- data.table( index=c(9,10,9,10),
                          symbol=c("a", "a", "b", "b"),
                          x = c(1,2,3,4),
                          y = c(5,6,7,8))
  attr(expected$index, "something") <- 5
  expect_equal( gather_symbol(keeps_index_attr),
                expected)
  mixed_price_types <- make_data_frame(index=c(9,10),
                        a.x = c(1,2), a.y=c(5,6),
                        b.y = c(3,4), b.z=c(7,8))
  expect_equal( gather_symbol(mixed_price_types),
                make_data_frame(index=c(9,10,9,10),
                                symbol=c("a", "a", "b", "b"),
                                x=c(1,2,NA,NA),
                                y=c(5,6,3,4),
                                z=c(NA, NA, 7,8)))
  one_symbol_one_column <- make_data_frame(index=c(9,10),
                                           a.x=c(1,2))
  expect_equal( gather_symbol(one_symbol_one_column),
                make_data_frame(index=c(9,10),
                                symbol=c("a","a"),
                                x=c(1,2)))
})