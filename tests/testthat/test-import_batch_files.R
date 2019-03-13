context("test-import_batch_files")
d <- data.frame("a" = seq(1, 10), "b" = seq(2, 20, by = 2))
write.csv(d, "./dat.csv")
test_that("NULL values for 'p'/'f' return errors", {
  expect_error(
    import_batch_files(f = read.csv)
  )
  expect_error(
    import_batch_files(p = ".")
  )
})
test_that("Non existent directory for 'p' returns error", {
  expect_error(
    import_batch_files(p = "./data", f = read.csv, ext = ".csv")
  )
})
test_that("Silent when working correctly", {
  expect_silent(
    import_batch_files(p = ".", f = read.csv, ext = ".csv")
  )
})
test_that("Non-character value for 'p' gets resolved", {
  expect_error(
    import_batch_files(p = ., f = read.csv, ext = ".csv")
  )
  expect_error(
    import_batch_files(p = 1, f = read.csv, ext = ".csv")
  )
})
file.remove("./dat.csv")
