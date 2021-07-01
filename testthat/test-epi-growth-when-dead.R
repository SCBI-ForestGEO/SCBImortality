library(here)
library(testthat)

test_that("some FRAM, FRNI, FRPE, FRSP, or CHVI are dead but Epicormic growthis greater than 0", {
  
  report_exists  <-  file.exists(file.path(here("testthat"), "reports/requires_field_fix/dead_but_epicormic_more_than_0.csv"))
  
  expect_false(report_exists)
})
