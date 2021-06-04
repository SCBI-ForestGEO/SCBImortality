library(here)
library(testthat)

test_that("Some trees have canker or swelling but no level is selectedor vice versa", {
  
  report_exists  <-  any(ifelse(file.exists(file.path(here("testthat"), c("reports/canker_but_no_level.csv", "reports/canker_level_but_wrong_status_or_FAD.csv"))), TRUE, FALSE))
  
  expect_false(report_exists)
})
