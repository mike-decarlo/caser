context("test-random_percents")
library(caser)

test_that("errors when non-numeric 'b'", {
  expect_error(
    random_percents(.bin = NULL)
  )
  expect_error(
    random_percents(.bin = "b")
  )
})

test_that("errors when non-numeric 's'", {
  expect_error(
    random_percents(.bin = 2, .seed = "s")
  )
})

test_that("errors when 0 for 'b'", {
  expect_error(
    random_percents(.bin = 0)
  )
})

test_that("produces output when acceptable values for 'b' and 's'", {
  expect_equal(
    random_percents(.bin = 2, .seed = 1)
    , random_percents(.bin = 2, .seed = 1)
  )
})
