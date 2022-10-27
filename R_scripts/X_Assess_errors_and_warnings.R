# assess 2022 census errors #97

# create list of tag numbers that need replacement see https://github.com/SCBI-ForestGEO/2023census/issues/7

# clear environment
rm(list = ls())

# load libraries
library(ggplot2)

# load data
require_field_fix_error_file <- read.csv("testthat/reports/requires_field_fix/require_field_fix_error_file.csv")
will_auto_fix_error_file <- read.csv("testthat/reports/will_auto_fix/will_auto_fix_error_file.csv")
warnings_file <- read.csv("testthat/reports/warnings/warnings_file.csv")
quadrat_censused_duplicated_stems <- read.csv("testthat/reports/will_auto_fix/quadrat_censused_duplicated_stems.csv")

# Make histogram of figures

ggplot(require_field_fix_error_file, aes(x = error_name, fill = Status.2022)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  labs(title = "Field Fix Errors")

ggplot(will_auto_fix_error_file, aes(x = error_name)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  labs(title = "'Auto-fix' Errors")

ggplot(warnings_file, aes(x = warning_name)) +
  geom_bar() +
  labs(title = "Warnings")

ggplot(require_field_fix_error_file[require_field_fix_error_file$error_name %in% "missing_stem", ], aes(x = DBH)) +
  geom_histogram() +
  facet_wrap(~Species) +
 labs(title = "Missing Stems (Field Fix error)")
