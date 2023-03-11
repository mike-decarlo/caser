context("test-number_digits")
library(caser)

test_that("warns if x is non-numeric format of number", {
  expect_warning(number_digits("1.256"))
})
test_that("errors if x is non-numeric string", {
  expect_error(number_digits("one.twofivesix"))
})
test_that("should produce output otherwise", {
  expect_silent(number_digits(1.021))
  expect_silent(number_digits(1))
})
