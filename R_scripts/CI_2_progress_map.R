# Progress Map - Mortality Survey Dashboard

library(tidyverse)
library(sf)
library(ggplot2)

# Read in the data
mortality_trees <- read.csv("raw_data/Field_Maps/SCBI_trees_mortality_2025_0.csv")
mortality_stems <- read.csv("raw_data/Field_Maps/SCBI_stems_mortality_2025_1.csv")
errors <- read.csv("QAQC_reports/allErrors.csv")
quadrat_sf <- read_sf("doc/progress_map/20m_grid/20m_grid.shp")
deer_exclosure <- st_read("doc/progress_map/Deer Exclosure/deer_exclosure_2011.shp")

# Prepare the survey data
mortality_data <- mortality_trees %>% 
  mutate(completion_value = NA) %>%
  mutate(completion_value = case_when(mort_census_status == "finished" ~ 0, 
                                      mort_census_status == "in progress" ~ 1, 
                                      mort_census_status == "not started" ~ 2)
                                      )

mortality_data_aggregate <- aggregate(completion_value ~ quadrat, data = mortality_data, FUN = mean)

sf_data <- mortality_data_aggregate %>% 
  select(quadrat, completion_value) %>%
  rename(PLOT = quadrat) %>% 
  mutate(fill = NA) %>%
  mutate(fill = case_when(
                between(completion_value, 0, 0) ~ "Finished", 
                between(completion_value, 0.001, 1.999) ~ "In Progress", 
                between(completion_value, 2, 10) ~ "Not Started") 
  )

# Prepare the error data
error_quadrats <- errors %>%
  filter(errorType == "error") %>%
  filter(errorName != "missedStem") %>%
  select(quadrat) %>%
  rename(PLOT = quadrat)

warning_quadrats <- errors %>%
  filter(errorType == "warning") %>%
  select(quadrat) %>%
  rename(PLOT = quadrat)

### The following "Missed Stem" category has been added for this survey 
  ### to account for stems that were not mapped due to missing location data
missing_stems <- errors %>%
  filter(errorName == "missedStem") %>%
  select(quadrat) %>%
  rename(PLOT = quadrat)


# Combine the error data with the sf_data
sf_data_errors <- sf_data %>%
  mutate(
    fill = case_when(
      PLOT %in% error_quadrats$PLOT ~ "Error",
      PLOT %in% missing_stems$PLOT ~ "Missed Stem",
      PLOT %in% warning_quadrats$PLOT ~ "Warning",
      TRUE ~ fill))

# Join the combined survey and error data with the shapefile
ggplot_data <- quadrat_sf %>%
  left_join(sf_data_errors)

progress_map <- ggplot(data = ggplot_data) +
  geom_sf(aes(fill = fill), color = "black", size = 0.1) +
  geom_sf(data = deer_exclosure, colour = "black", fill = NA,lwd = 1) +
  scale_fill_manual(name = "Completion Status", values = c("Finished" = "#5ab962", "Missed Stem" = "#f7d853", "Error" = "#841F27", "Warning" = "#CC5500", "Not Started" = "#808080"), 
                    breaks = c("Finished", "Missed Stem", "Warning", "Error", "Not Started")) +
  theme_void() +
  theme(axis.text = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 16),
        plot.subtitle =  element_text(hjust = 0.5)) +
  ggtitle("Progress Map",
          subtitle = paste0(round(prop.table(table(mortality_data$mort_census_status)) *100,2)["finished"], "% stems finished"))

progress_map

ggsave(progress_map, filename = "doc/progress_map/progress_map.jpg", units = "in", height = 8, width = 10, dpi = 300)
