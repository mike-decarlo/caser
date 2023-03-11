context("test-subset_frame")
library(caser)
df <- data.frame("X" = rnorm(100, 0, 1), "Y" = runif(100, 0, 1))
test_that(
  "num_subs can be NULL",
  {
    expect_silent(subset_frame(.data = df, n = NULL))
  })
test_that(
  "null df values error",
  {
    expect_error(subset_frame(.data = NULL, n = 4))
  })
test_that(
  "non null, non numeric value for num_subs errors",
  {
    expect_error(subset_frame(.data = df, n = "4"))
  })
df <- rnorm(100, 0, 1)
test_that(
  "if df not a data.frame it gets converted and message produced",
  {
    expect_message(subset_frame(.data = rnorm(100, 0, 1), n = 2))
  })
