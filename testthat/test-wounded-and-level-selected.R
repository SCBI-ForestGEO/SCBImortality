library(here)
library(testthat)

test_that("Some trees are wounded but have no Wounded main stem selected or vice versa", {
  
  report_exists  <-  any(ifelse(file.exists(file.path(here("testthat"), c("reports/wounded_but_no_level.csv", "reports/wounded_level_but_wrong_status_or_FAD.csv"))), TRUE, FALSE))
  
  expect_false(report_exists)
})
