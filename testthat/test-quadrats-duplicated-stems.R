library(here)
library(testthat)

test_that("Some stems duplicated in a quadrat already censused", {
  
  report_exists  <-  ifelse(file.exists(file.path(here("testthat"), "reports/will_auto_fix/quadrat_censused_duplicated_stems.csv")), TRUE, FALSE)
  
  expect_false(report_exists)
})
