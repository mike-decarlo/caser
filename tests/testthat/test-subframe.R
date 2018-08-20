context("test-subframe")
library(caseling)

df <- data.frame("X" = rnorm(100, 0, 1), "Y" = runif(100, 0, 1))

test_that("no_subs can be NULL", {
  expect_success(subframe(df = df, no_subs = NULL))
})
