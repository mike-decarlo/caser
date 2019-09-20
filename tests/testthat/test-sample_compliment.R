context("test-sample_compliment")
library(caser)

orig <- c(1:4)
samp <- sample_rows(orig, 2)

test_that("silent when orig and samp exist", {
  expect_silent(
    sample_compliment(orig = orig, samp = samp)
  )
})

test_that("error if orig == NULL", {
  orig <- NULL
  expect_error(
    sample_rows(orig = orig, samp = samp)
  )
})

test_that("error if samp == NULL", {
  orig <- c(1:4)
  samp <- NULL
  expect_error(
    sample_rows(orig = orig, samp = samp)
  )
})
