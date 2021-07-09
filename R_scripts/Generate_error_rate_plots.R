# Generate plots of error rates over time and other error rate analyses for 
# paper as seen here
# https://github.com/SCBI-ForestGEO/continuous_integration/issues/14
library(tidyverse)
library(readxl)
library(here)
library(janitor)
library(lubridate)

# Read log of field fixes
field_fixes_errors <- read_csv("testthat/reports/trace_of_reports/require_field_fix_error_file.csv") %>% 
  clean_names() %>% 
  group_by(submission_id) %>% 
  summarize(n_errors = n()) 


# Get latest FFF
latest_FFFs <- list.files(here("raw_data/FFF_excel/"), pattern = ".xlsx", full.names = T)

# Dates for each submission
submission_dates <- read_xlsx(latest_FFFs, sheet = "Form Meta Data", .name_repair = "minimal" )%>% 
  clean_names()  %>% 
  mutate(
    # start_form_time_stamp = parse_date_time(start_form_time_stamp, "m-d-Y H:M:S p"),
    # end_form_time_stamp = parse_date_time(end_form_time_stamp, "m-d-Y H:M:S p"),
    date = str_sub(start_form_time_stamp, 1, 10) %>% mdy()
  ) %>% 
  select(submission_id, date)

submissions <- read_xlsx(latest_FFFs, sheet = "Root", .name_repair = "minimal" )%>% 
  clean_names() %>% 
  mutate(stem_count = as.numeric(stem_count)) %>% 
  select(submission_id, quad, stem_count) %>% 
  group_by(submission_id) %>% 
  summarize(n_stems = sum(stem_count)) %>% 
  mutate(n_stems = ifelse(is.na(n_stems), 0, n_stems))



# Every single stem record
stems <- read_xlsx(latest_FFFs, sheet = "subform_1", .name_repair = "minimal" ) %>% 
  clean_names() %>% 
  select(quad, tag, stem_tag)
  group_by(submission_id, quad) %>% 
  summarize(n = n())
  

  

  






submissions_new <- submissions %>% 
  left_join(field_fixes_errors, by = "submission_id") %>% 
  mutate(n_errors = ifelse(is.na(n_errors), 0, n_errors)) %>% 
  mutate(n_stems = ifelse(is.na(n_stems), 0, n_stems)) %>% 
  left_join(submission_dates, by = "submission_id") 
  

submissions_new %>% 
  group_by(date) %>% 
  summarize(
    n_errors = sum(n_errors),
    n_stems = sum(n_stems)
    ) %>% 
  mutate(rate = n_errors/n_stems) %>% 
  filter(rate != Inf ) %>% 
  ggplot(aes(x = date, y = rate)) +
  geom_point() +
  geom_line() +
  labs(
    x = "Date", 
    y = "Error rate: number of field fix errors / number of stems",
    title = "Error rate =  number of field fix errors / number of stems"
    )

# ggsave("testthat/reports/error_rate.png")

