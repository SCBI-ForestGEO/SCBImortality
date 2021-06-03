library(here)
library(testthat)

test_that("Some percent of crown living of censused trees were not recorded", {
  
  report_exists  <-  ifelse(file.exists(file.path(here("testthat"), "reports/missing_percent_crown_living.csv")), TRUE, FALSE)
  
  expect_false(report_exists)
})
