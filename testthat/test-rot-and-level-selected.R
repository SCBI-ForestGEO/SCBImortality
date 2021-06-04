library(here)
library(testthat)

test_that("Some trees have rotting stem but no level is selected", {
  
  report_exists  <-  ifelse(file.exists(file.path(here("testthat"), "reports/rot_but_no_level.csv")), TRUE, FALSE)
  
  expect_false(report_exists)
})
