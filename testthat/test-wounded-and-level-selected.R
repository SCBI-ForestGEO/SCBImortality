library(here)
library(testthat)

test_that("Some trees are wounded but have no Wounded main stem selected or vice versa", {
  
  report_exists  <-  any(ifelse(file.exists(file.path(here("testthat"), "reports/requires_field_fix/wounded_but_no_level.csv")), TRUE, FALSE))
  
  expect_false(report_exists)
})
