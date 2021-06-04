library(here)
library(testthat)

test_that("Some trees are 'alive' but with unhealthy attributes or vice versa", {
  
  report_exists  <-  any(ifelse(file.exists(file.path(here("testthat"), c("reports/status_A_but_unhealthy.csv", "reports/unhealthy_but_wrong_status.csv"))), TRUE, FALSE))
  
  expect_false(report_exists)
})
