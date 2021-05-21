library(here)
library(testthat)

test_that("Some species codes are not valid", {
  
  species_report_exists  <-  ifelse(file.exists(file.path(here("testthat"), "reports/species_code_error.csv")), TRUE, FALSE)

  expect_false(species_report_exists)
})
