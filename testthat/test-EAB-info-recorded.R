library(here)
library(testthat)

test_that("some FRAM, FRNI, FRPE, FRSP, or CHVI don't have Crown thinning, Epicormic growth, D-shaped exit hole count or Crown position < 10 cm DBH (for stems <10cm).", {
  
  report_exists  <-  file.exists(file.path(here("testthat"), "reports/requires_field_fix/missing_EAB_info.csv"))
  
  expect_false(report_exists)
})
