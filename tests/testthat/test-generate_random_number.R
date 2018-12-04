context("test-generate_random_number")
library(caser)

test_that("as integer and as numeric are equivalent", {
  random <- generate_random_number(1, 1, 2, "integer")
  expect_equal(as.integer(random), random)
})

test_that("integer with digits is same as without", {
  random1 <- generate_random_number(1, 1, 1, "integer")
  random2 <- generate_random_number(1, 1, 1, "integer", digits = 2)
  expect_equal(random1, random2)
})

test_that("float number characters is correct", {
  random <- generate_random_number(1, 1.1, 1.9, "float", 0)
  expect_equal(nchar(random), 1)
  random <- generate_random_number(1, 1.1, 1.9, "float", 1)
  expect_gte(nchar(random), 3)
  random <- generate_random_number(1, 1.1, 1.9, "float", 2)
  expect_gte(nchar(random), 3)
  random <- generate_random_number(1, 1.1, 1.9, "float", 3)
  expect_gte(nchar(random), 3)
})

test_that("exits on NA error or non-number for float", {
  expect_error(generate_random_number(1, 1, 2, "float"))
  expect_error(generate_random_number(1, 1, 2, "float", "A"))
  expect_message(generate_random_number(1, 1, 2, "float", "3"))
})

test_that("max gte min", {
  expect_message(
    generate_random_number(1, 2, 1, "integer")
    , paste0(
        "Warning: Min must be less than or equal to max.\n"
        , "Switching values to satisfy requirements.\n"
        )
  )
})

test_that("type argument is either 'integer' or 'float'", {
  expect_error(generate_random_number(1, 1, 2, type = "other"))
})
