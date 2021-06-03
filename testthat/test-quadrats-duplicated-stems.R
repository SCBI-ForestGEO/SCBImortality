library(here)
library(testthat)

test_that("Some stems duplicated in a quadrat already censused", {
  
  report_exists  <-  ifelse(file.exists(file.path(here("testthat"), "reports/quadrat_censused_duplicated_stems")), TRUE, FALSE)
  
  expect_false(report_exists)
})
