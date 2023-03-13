context("test-plot_corr")
library(caser)

test_that("errors if an argument is NULL", {
  expect_error(plot_corr(NULL, "test"))
  expect_error(plot_corr(data.frame("one" = 1, "two" = 2), NULL))
})
test_that("creates snapshot", {
  expect_type(
    plot_corr(data.frame("one" = runif(30), "two" = runif(30)), "one"),
    "list"
  )
})
