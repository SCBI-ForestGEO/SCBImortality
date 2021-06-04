library(here)
library(testthat)

test_that("Some trees are wounded but have no Wounded main stem selected", {
  
  report_exists  <-  ifelse(file.exists(file.path(here("testthat"), "reports/wounded_but_no_level.csv")), TRUE, FALSE)
  
  expect_false(report_exists)
})
