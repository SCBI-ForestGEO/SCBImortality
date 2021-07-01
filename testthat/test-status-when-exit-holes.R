library(here)
library(testthat)

test_that("some FRAM, FRNI, FRPE, FRSP, or CHVI have D-shaped exit hole count>0 but status is not AU or dead", {
  
  report_exists  <-  file.exists(file.path(here("testthat"), "reports/requires_field_fix/exit_hole_count_but_not_AU_or_dead.csv"))
  
  expect_false(report_exists)
})
