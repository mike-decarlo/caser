context("test-randnum")
library(caseling)

test_that("as integer and as numeric are equivalent", {
  random <- randnum(1, 2, "integer")
  expect_equal(as.integer(random), random)
})

test_that("integer with digits is same as without", {
  random1 <- randnum(1, 1, "integer")
  random2 <- randnum(1, 1, "integer", digits = 2)
  expect_equal(random1, random2)
})

test_that("float number characters is correct", {
  random <- randnum(1.1, 1.9, "float", 0)
  expect_equal(nchar(random), 1)
  random <- randnum(1.1, 1.9, "float", 1)
  expect_gte(nchar(random), 3)
  random <- randnum(1.1, 1.9, "float", 2)
  expect_gte(nchar(random), 3)
  random <- randnum(1.1, 1.9, "float", 3)
  expect_gte(nchar(random), 3)
})

test_that("exits on NA error", {
  expect_error(randnum(1, 2, "float"))
})

test_that("max gte min", {
  expect_output(randnum(2, 1))
})