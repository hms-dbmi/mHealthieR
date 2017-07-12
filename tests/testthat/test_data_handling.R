context("data_handling")

test_that("check_format warns about wide tables", {
  wide_table <- as.table(matrix(c("id-a", 1, 2, 3, "id-b", 4, 5, 6), ncol=4, byrow=TRUE))
  expect_warning(check_format(wide_table))
})

test_that("check_format gets long tables", {
  long_table <- as.table(matrix(c("id-a", as.POSIXct('2001-01-01'), 42), ncol=3, byrow=TRUE))
  expect_equal(check_format(long_table), "long")
})
