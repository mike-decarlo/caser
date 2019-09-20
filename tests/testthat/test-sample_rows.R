context("test-sample_rows")
library(caser)

x <- NULL

test_that("error if NULL x argument or NULL data.frame object", {
    expect_error(
      sample_rows(x = x, n = 50)
    )
    expect_error(
      sample_rows(x = NULL, n = 50)
    )
  })

x <- data.frame("X" = rnorm(100, 0, 1), "Y" = runif(100, 0, 1))

test_that("error if x is not class = data.frame", {
  expect_error(
    sample_rows(x = as.matrix(x), n = 50)
  )
})

test_that("errors when sample size exceeds population, no replacement", {
  expect_error(sample_rows(x = x, n = 200, replace = F))
})

test_that("Seed control works", {
  expect_silent(
    sample_rows(x = x, n = 50, seed = 1, replace = F)
  )
  expect_silent(
    sample_rows(x = x, n = 50, replace = F)
  )
})

test_that("produces sample of correct size", {
  expect_equal(nrow(sample_rows(x = x, n = 2, replace = T)), 2)
  expect_equal(ncol(sample_rows(x = x, n = 2, replace = T)), 2)
})