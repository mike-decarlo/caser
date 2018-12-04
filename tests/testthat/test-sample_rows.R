context("test-sample_rows")
library(caser)

df <- data.frame("X" = rnorm(100, 0, 1), "Y" = runif(100, 0, 1))

test_that("errors when sample size exceeds population, no replacement", {
  expect_error(sample_rows(df = df, n = 200, replace = F))
})

test_that("produces sample of correct size", {
  expect_equal(nrow(sample_rows(df, 2, replace = T)), 2)
  expect_equal(ncol(sample_rows(df, 2, replace = T)), 2)
})