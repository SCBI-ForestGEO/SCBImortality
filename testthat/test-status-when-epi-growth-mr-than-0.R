library(here)
library(testthat)

test_that("some FRAM, FRNI, FRPE, FRSP, or CHVI have Epicormic growth>0 but status is not AU or dead", {
  
  report_exists  <-  file.exists(file.path(here("testthat"), "reports/requires_field_fix/epicormic_growth_but_not_AU.csv"))
  
  expect_false(report_exists)
})
