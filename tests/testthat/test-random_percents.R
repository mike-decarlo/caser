context("test-random_percents")
library(caser)

test_that("errors when non-numeric 'b'", {
  expect_error(
    random_percents(b = NULL)
  )
  expect_error(
    random_percents(b = "b")
  )
})

test_that("errors when non-numeric 's'", {
  expect_error(
    random_percents(b = 2, s = "s")
  )
})

test_that("errors when 0 for 'b'", {
  expect_error(
    random_percents(b = 0)
  )
})

test_that("produces output when acceptable values for 'b' and 's'", {
  expect_equal(
    random_percents(b = 2, s = 1)
    , random_percents(b = 2, s = 1)
  )
})
