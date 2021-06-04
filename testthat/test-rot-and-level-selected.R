library(here)
library(testthat)

test_that("Some trees have rotting stem but no level is selected or vice versa", {
  
  report_exists  <-  any(ifelse(file.exists(file.path(here("testthat"), c("reports/rot_but_no_level.csv", "reports/rot_level_but_wrong_status_or_FAD.csv"))), TRUE, FALSE))
  
  expect_false(report_exists)
})
