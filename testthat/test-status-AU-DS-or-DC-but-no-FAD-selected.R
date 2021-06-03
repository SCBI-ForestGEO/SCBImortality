library(here)
library(testthat)

test_that("Some trees are AU, DS or DC but have no FAD selected", {
  
  report_exists  <-  ifelse(file.exists(file.path(here("testthat"), "reports/status_AU_DS_or_DC_but_no_FAD.csv")), TRUE, FALSE)
  
  expect_false(report_exists)
})
