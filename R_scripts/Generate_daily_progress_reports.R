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



# Load census stem counts --------------------------------
# Get stem counts from main census (copied from Generate_reports.R)
main_census <-  read.csv(paste0("https://raw.githubusercontent.com/SCBI-ForestGEO/SCBI-ForestGEO-Data/master/tree_main_census/data/census-csv-files/scbi.stem3.csv"))

## convert dbh to numeric
main_census$dbh <- as.numeric(main_census$dbh)

## only keep trees > 10cm except for fraxinus and Chionanthus virginicus
main_census <-  main_census[grepl("^fr..|^ch..", main_census$sp) | (!is.na(main_census$dbh) & main_census$dbh >= 100), ]

## remove trees that are dead
# main_census <- main_census[!main_census$status %in% "D",]
main_census <- main_census[!grepl("DC|DN|DT", main_census$codes),] 

census_quad_counts <- main_census %>% 
  group_by(quadrat) %>% 
  summarize(n_census = n()) %>% 
  rename(quad = quadrat) %>% 
  mutate(quad = str_pad(quad, width = 4, side = "left", pad = "0"))


# Load quadrat info --------------------------------
# Get latest FFF
latest_FFFs <- list.files(here("raw_data/FFF_excel/"), pattern = ".xlsx", full.names = T)

# Get info on all quadrats censused so far
quadrat_info <- read_xlsx(latest_FFFs, sheet = "Root", .name_repair = "minimal" ) %>% 
  clean_names() %>% 
  select(-submitted_by, -starts_with("form")) %>% 
  mutate(
    stem_count = as.numeric(stem_count),
    date_time = mdy(date_time),
    submitted_on = parse_date_time(submitted_on, "m-d-Y H:M p"),
    quad = str_pad(quad, width = 4, side = "left", pad = "0")
  ) %>% 
  arrange(date_time) %>% 
  # Stem counts from FFF were built off wrong census file, so replace
  # census stem counts from above
  left_join(census_quad_counts, by = "quad") %>% 
  select(-stem_count) %>% 
  rename(stem_count = n_census)

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
  filter(!is.na(date_time)) %>% 
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
  )+
  geom_vline(xintercept = ymd("2021-07-06"), col = "red") +
  theme_bw()

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
  geom_vline(xintercept = ymd("2021-07-06"), col = "red") +
  theme_bw() + 
  theme(legend.position = "bottom")


# Output
filename <- file.path(here("testthat"), "reports/daily_progress.png")
census_rate_plot / error_rate_plot 
ggsave(filename, device = 'png', width = 16/2, height = 18/2.5, units = "in",dpi = 300 )
