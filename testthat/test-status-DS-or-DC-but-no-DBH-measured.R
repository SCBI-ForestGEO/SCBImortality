library(here)
library(testthat)

test_that("Some trees are dead but the DBH was not measured", {
  
  report_exists  <-  ifelse(file.exists(file.path(here("testthat"), "reports/requires_field_fix/status_DS_or_DC_but_DBH_measured.csv")), TRUE, FALSE)
  
  expect_false(report_exists)
})
