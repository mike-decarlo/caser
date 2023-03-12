context("test-generate_experiment")
library(caser)

test_that("silent when entered correctly", {
  expect_silent(generate_experiment(100, 0.2))
})
