test_that("Some percent of crown living are larger than percent crown intact", {
  
  report_exists  <-  ifelse(file.exists(file.path(here("testthat"), "reports/crown_living_greater_than_crown_intact.csv")), TRUE, FALSE)
  
  expect_false(report_exists)
})
