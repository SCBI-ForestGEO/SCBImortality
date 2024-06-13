# Progress Map - Mortality Survey Dashboard

library(tidyverse)
library(sf)
library(ggplot2)

# Read in the data
mortality_trees <- read.csv("raw_data/Field_Maps/SCBI_trees_mortality_2024_0.csv")
mortality_stems <- read.csv("raw_data/Field_Maps/SCBI_stems_mortality_2024_1.csv")
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
                between(completion_value, 0, 0) ~ "Finished", #sage = #6fc276, green = #5ab962
                between(completion_value, 0.001, 1.999) ~ "In Progress", #blue = "#99c5c4", yellow = #f7d853
                between(completion_value, 2, 10) ~ "Not Started") #grey = #808080
  )

# Prepare the error data
error_quadrats <- errors %>%
  select(quadrat) %>%
  rename(PLOT = quadrat)

# Combine the error data with the sf_data
sf_data_errors <- sf_data %>% 
  mutate( 
    fill = if_else(PLOT %in% error_quadrats$PLOT, "Error/Warning", fill))   # brick red = #841F27

# Join the combined survey and error data with the shapefile
ggplot_data <- quadrat_sf %>%
  left_join(sf_data_errors)

progress_map <- ggplot(data = ggplot_data) +
  geom_sf(aes(fill = fill), color = "black", size = 0.1) +
  geom_sf(data = deer_exclosure, colour = "black", fill = NA,lwd = 1) +
  scale_fill_manual(name = "Completion Status", values = c("Finished" = "#5ab962", "In Progress" = "#f7d853", "Error/Warning" = "#841F27","Not Started" = "#808080")) +
  theme_bw() +
  theme(axis.text = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 16)) +
  ggtitle("Progress Map")

progress_map

ggsave(progress_map, filename = "doc/progress_map/progress_map.jpg", units = "in", height = 8, width = 10, dpi = 300)

