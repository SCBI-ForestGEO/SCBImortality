library(here)
library(testthat)

test_that("Some species codes are not valid", {
  
  report_exists  <-  ifelse(file.exists(file.path(here("testthat"), "reports/species_code_error.csv")), TRUE, FALSE)

  expect_false(report_exists)
})
