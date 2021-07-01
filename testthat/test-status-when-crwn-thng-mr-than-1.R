library(here)
library(testthat)

test_that("some FRAM, FRNI, FRPE, FRSP, or CHVI have Crown thinning > 1 but status is not AU or dead", {
  
  report_exists  <-  file.exists(file.path(here("testthat"), "reports/requires_field_fix/crown_thinning_more_than_1_but_not_AU_or_dead.csv"))
  
  expect_false(report_exists)
})
