context("data_handling")

test_that("check_format warns about wide tables", {
  wide_table <- tibble::tibble(keys = c('key1', 'key2', 'key3', 'key4'),
                               tp1 = c(20375, 3298, 15031, 14114),
                               tp2 = c(11567, 29733, 5701, 24491),
                               tp3 = c(2518, 12310, 4662, 6200))
  expect_error(check_format(wide_table))
})

test_that("check_format gets long tables", {
  long_table <- tibble::tibble(keys = c('key1', 'key2', 'key3'),
                               days = c(as.POSIXct('2015-02-01'),
                                        as.POSIXct('2015-02-02'),
                                        as.POSIXct('2015-02-02')),
                               values = c(2475, 1338, 4379))
  expect_equal(check_format(long_table), "long")
})


