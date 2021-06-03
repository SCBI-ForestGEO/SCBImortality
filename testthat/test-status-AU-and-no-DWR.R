library(here)
library(testthat)

test_that("Some trees are 'AU' but have DWR (dead with resprouts)  selected", {
  
  report_exists  <-  ifelse(file.exists(file.path(here("testthat"), "reports/status_AU_but_DWR_selected.csv")), TRUE, FALSE)
  
  expect_false(report_exists)
})
