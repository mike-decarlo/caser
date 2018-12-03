context("test-sub_frame")
library(caseling)

df <- data.frame("X" = rnorm(100, 0, 1), "Y" = runif(100, 0, 1))

test_that("num_subs can be NULL", {
  expect_silent(sub_frame(df = df, num_subs = NULL))
})

test_that("if df not data.frame it gets converted", {
  df <- as.matrix(df)
  expect_warning(sub_frame(df = df, num_subs = 4))
})

test_that("null df values error", {
  expect_error(sub_frame(df = NULL, num_subs = 4))
})
