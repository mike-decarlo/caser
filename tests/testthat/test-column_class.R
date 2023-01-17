context("test-column_class")
test_that(
  "Error message when 'df' object is NULL",
  {
    x <- NULL
    expect_error(column_class(x))
  })
test_that(
  "Returns all column classes when 'find_class' is NULL",
  {
    x <- data.frame(a = c("a", "b", "c"), b = c(1, 2, 3))
    c_man <- sapply(x, class)
    c_aut <- column_class(x)
    expect_identical(c_aut, c_man)
  })
test_that(
  "Returns select column classes when 'find_class' is single not NULL",
  {
    x <- data.frame(a = c("a", "b", "c"), b = c(1, 2, 3))
    classes <- "integer"
    c_man <- sapply(x, class)
    c_man <- c_man[c_man == classes]
    c_aut <- column_class(x, classes)
    expect_identical(c_aut, c_man)
  })
test_that(
  "Returns select column classes when 'find_class' is multiple not NULL",
  {
    x <- data.frame(a = c("a", "b", "c"), b = c(1, 2, 3))
    classes <- c("integer", "factor")
    c_man <- sapply(x, class)
    c_man <- c_man[c_man %in% classes]
    c_aut <- column_class(x, classes)
    expect_identical(c_aut, c_man)
  })
