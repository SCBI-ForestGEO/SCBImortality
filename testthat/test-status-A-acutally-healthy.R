library(here)
library(testthat)

test_that("Some trees are 'alive' but with unhealthy attributes", {
  
  report_exists  <-  ifelse(file.exists(file.path(here("testthat"), "reports/status_A_but_unhealthy.csv")), TRUE, FALSE)
  
  expect_false(report_exists)
})
