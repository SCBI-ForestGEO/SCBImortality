library(here)
library(testthat)

test_that("Some stems were missed in a quadrat already censused", {
  
  species_report_exists  <-  ifelse(file.exists(file.path(here("testthat"), "reports/quadrat_censused_missing_stems.csv")), TRUE, FALSE)
  
  expect_false(species_report_exists)
})
