library(here)
library(testthat)

test_that("some FRAM, FRNI, FRPE, FRSP, or CHVI are dead but crown thinning is less than 5", {
  
  report_exists  <-  file.exists(file.path(here("testthat"), "reports/requires_field_fix/dead_but_crown_thinning_less_than_5.csv"))
  
  expect_false(report_exists)
})
