library(here)
library(testthat)

test_that("Some stems were missed in a quadrat already censused", {
  
  report_exists  <-  ifelse(file.exists(file.path(here("testthat"), "reports/requires_field_fix/quadrat_censused_missing_stems.csv")), TRUE, FALSE)
  
  expect_false(report_exists)
})
