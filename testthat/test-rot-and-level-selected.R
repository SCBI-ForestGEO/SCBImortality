library(here)
library(testthat)

test_that("Some trees have rotting stem but no level is selected or vice versa", {
  
  report_exists  <-  any(ifelse(file.exists(file.path(here("testthat"), "reports/requires_field_fix/rot_but_no_level.csv")), TRUE, FALSE))
  
  expect_false(report_exists)
})
