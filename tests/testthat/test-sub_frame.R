context("test-sub_frame")
library(caseling)

df <- data.frame("X" = rnorm(100, 0, 1), "Y" = runif(100, 0, 1))

test_that("no_subs can be NULL", {
  expect_silent(sub_frame(df = df, num_subs = NULL))
})
