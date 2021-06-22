library(here)
library(testthat)

test_that("Some crown position of censused trees were not recorded", {
  
  report_exists  <-  ifelse(file.exists(file.path(here("testthat"), "reports/requires_field_fix/missing_crown_position.csv")), TRUE, FALSE)
  
  expect_false(report_exists)
})
