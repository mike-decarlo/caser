context("test-sample_rows")
library(caser)

df <- NULL

test_that("error if NULL df argument or NULL data.frame object", {
    expect_error(
      sample_rows(df = df, n = 50)
    )
    expect_error(
      sample_rows(df = NULL, n = 50)
    )
  })

df <- data.frame("X" = rnorm(100, 0, 1), "Y" = runif(100, 0, 1))

test_that("error if df is not class = data.frame", {
  expect_error(
    sample_rows(df = as.matrix(df), n = 50)
  )
})

test_that("errors when sample size exceeds population, no replacement", {
  expect_error(sample_rows(df = df, n = 200, replace = F))
})

test_that("Seed control works", {
  expect_silent(
    sample_rows(df = df,n = 50, s = 1, replace = F)
  )
  expect_silent(
    sample_rows(df = df, n = 50, replace = F)
  )
})

test_that("produces sample of correct size", {
  expect_equal(nrow(sample_rows(df = df, n = 2, replace = T)), 2)
  expect_equal(ncol(sample_rows(df = df, n = 2, replace = T)), 2)
})