context("test-rand_num")
library(caseling)

test_that("as integer and as numeric are equivalent", {
  random <- rand_num(1, 2, "integer")
  expect_equal(as.integer(random), random)
})

test_that("integer with digits is same as without", {
  random1 <- rand_num(1, 1, "integer")
  random2 <- rand_num(1, 1, "integer", digits = 2)
  expect_equal(random1, random2)
})

test_that("float number characters is correct", {
  random <- rand_num(1.1, 1.9, "float", 0)
  expect_equal(nchar(random), 1)
  random <- rand_num(1.1, 1.9, "float", 1)
  expect_gte(nchar(random), 3)
  random <- rand_num(1.1, 1.9, "float", 2)
  expect_gte(nchar(random), 3)
  random <- rand_num(1.1, 1.9, "float", 3)
  expect_gte(nchar(random), 3)
})

test_that("exits on NA error or non-number for float", {
  expect_error(rand_num(1, 2, "float"))
  expect_error(rand_num(1, 2, "float", "A"))
  expect_message(
    rand_num(1, 2, "float", "3")
    , paste0(
      "Non-numeric value entered for argument 'digits'.\n"
      , "Converting to numeric...\n"
      )
    )
})

test_that("max gte min", {
  expect_message(
    rand_num(2, 1)
    , paste0(
        "Warning: Min must be less than or equal to max.\n"
        , "Switching values to satisfy requirements.\n"
        )
  )
})

test_that("type argument is either 'integer' or 'float'", {
  expect_error(rand_num(1, 2, type = "other"))
})
