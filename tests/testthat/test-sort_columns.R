context("test-sort_columns")
test_that(
  "sorting cols works when all inputs are entered correctly",
  {
    x <- data.frame(b = c(6, 7, 8, 9, 10), a = c(1, 2, 3, 4, 5))
    expect_message(sort_columns(.data = x, order = "asc", verbose = TRUE))
    expect_message(sort_columns(.data = x, order = "desc", verbose = TRUE))
    expect_silent(sort_columns(.data = x, order = "asc"))
    expect_silent(sort_columns(.data = x, order = "desc"))
  })
test_that(
  "errors when df null",
  {
    expect_error(sort_columns(order = "asc"))
    expect_error(sort_columns(order = "desc"))
  })
test_that(
  "errors if not 'asc' or 'desc' entered for order",
  {
    x <- data.frame(b = c(6, 7, 8, 9, 10), a = c(1, 2, 3, 4, 5))
    expect_error(sort_columns(.data = x, order = "error"))
  })
test_that(
  "errors if order not a character input",
  {
    x <- data.frame(b = c(6, 7, 8, 9, 10), a = c(1, 2, 3, 4, 5))
    expect_error(sort_columns(.data = x, order = 0))
  })
