context("data_handling")

test_that("check_format warns about wide tables", {
  wide_table <- matrix(c("id-a", 1, "id-b", 4), ncol=2, byrow=TRUE)
  expect_warning(check_format(wide_table))
})
