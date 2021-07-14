# Generate plots of error rates over time and other error rate analyses for 
# paper as seen here
# https://github.com/SCBI-ForestGEO/continuous_integration/issues/14
library(dplyr)
library(readxl)
library(readr)
library(stringr)
library(here)
library(janitor)
library(lubridate)
library(patchwork)
library(tidyr)
library(ggplot2)



# Load quadrat info and censused stem counts --------------------------------
# Get latest FFF
latest_FFFs <- list.files(here("raw_data/FFF_excel/"), pattern = ".xlsx", full.names = T)

# Get info on all quadrats censused so far
quadrat_info <- read_xlsx(latest_FFFs, sheet = "Root", .name_repair = "minimal" ) %>% 
  clean_names() %>% 
  select(submission_id, quad, date_time, personnel, stem_count) %>% 
  mutate(
    stem_count = as.numeric(stem_count),
    date_time = mdy(date_time)
  )

# Get number of stems censused per day
stems_censused_per_day <- quadrat_info %>% 
  group_by(date_time) %>% 
  summarize(n_stems = sum(stem_count, na.rm=TRUE))



# Load trace of error reports ---------------------------------------
# Field fix errors per day
field_fix_errors <- read_csv("testthat/reports/trace_of_reports/require_field_fix_error_file.csv") %>% 
  clean_names() %>% 
  select(error_name, surveyor_id, submission_id, quad, tag, stem_tag) %>% 
  left_join(
    quadrat_info %>% select(submission_id, date_time), 
    by = "submission_id"
  ) %>% 
  # Determine level of aggregation:
  group_by(date_time) %>% 
  summarize(field_fix = n()) 

# Auto fixes
auto_fix_errors <- read_csv("testthat/reports/trace_of_reports/will_auto_fix_error_file.csv") %>% 
  clean_names() %>% 
  select(error_name, surveyor_id, submission_id, quad, tag, stem_tag) %>% 
  left_join(
    quadrat_info %>% select(submission_id, date_time), 
    by = "submission_id"
  ) %>% 
  # Determine level of aggregation:
  group_by(date_time) %>% 
  summarize(auto_fix = n()) 

# Missing stems
quadrat_censused_missing_stems <- read_csv("testthat/reports/trace_of_reports/quadrat_censused_missing_stems.csv") %>% 
  clean_names() %>% 
  mutate(
    quad = str_pad(quadrat, 4, "left", "0"),
    date_time = mdy(exact_date)
  ) %>% 
  # Determine level of aggregation:
  group_by(date_time) %>% 
  summarize(missing_stems = n()) 

# Duplicated stems
quadrat_censused_duplicated_stems <- read_csv("testthat/reports/trace_of_reports/quadrat_censused_duplicated_stems.csv") %>% 
  clean_names() %>% 
  select(surveyor_id, submission_id, quad, tag, stem_tag) %>% 
  left_join(
    quadrat_info %>% select(submission_id, date_time), 
    by = "submission_id"
  ) %>% 
  # Count any duplicates only once
  group_by(submission_id, date_time, quad, tag, stem_tag) %>% 
  slice(1) %>% 
  # Determine level of aggregation:
  group_by(date_time) %>% 
  summarize(duplicated_stems = n()) 



# Merge all data ---------------------------------------
error_rates <- 
  # Merge error reports
  field_fix_errors %>% 
  left_join(auto_fix_errors, by = "date_time") %>% 
  left_join(quadrat_censused_duplicated_stems, by = "date_time") %>% 
  left_join(quadrat_censused_missing_stems, by = "date_time") %>% 
  replace(is.na(.), 0) %>%
  pivot_longer(cols = -c(date_time), names_to = "error_type", values_to = "n_errors") %>% 
  # Get error rate being sure to account for days where 0 errors occurred
  full_join(stems_censused_per_day, by = "date_time") %>% 
  mutate(errors_per_stem = n_errors/n_stems) %>% 
  complete(date_time, error_type) %>% 
  filter(!is.na(error_type)) %>% 
  replace(is.na(.), 0)




# Generate all plots ---------------------------------------
census_rate_plot <- stems_censused_per_day %>% 
  replace(is.na(.), 0) %>% 
  ggplot(aes(x=date_time, y = n_stems)) + 
  geom_point() +
  geom_line() + 
  labs(
    x = "Census date", y = "# of new stems censused",
    title = "Daily new stems censused"
  )

error_rate_plot <- error_rates %>%
  ggplot(aes(x=date_time, y = errors_per_stem, col = error_type)) +
  geom_point() +
  geom_line() + 
  labs(
    x = "Census date", y = "# of errors per stem censused",
    col = "Error type",
    title = "Daily (new) error rate"
  ) + 
  coord_cartesian(
    ylim = c(0, NA)
  ) +
  theme(legend.position="bottom")

# Merge ggplots using patchwork
progress_plot <- census_rate_plot /
  error_rate_plot 

# Output
filename <- file.path(here("testthat"), "reports/daily_progress.png")
ggsave(filename, plot= progress_plot, device = 'png', width = 16/2, height = 18/2.5, units = "in",dpi = 300 )
