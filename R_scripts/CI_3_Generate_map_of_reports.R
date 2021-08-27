# Generate map of censused quadrats ####

## this script is run automatically when there is a push 

# clear environment ####
rm(list = ls())

# load libraries ####
library(here)
library(rgdal)
library(readxl)

# load map of quadrats ####
quadrats <- rgdal::readOGR(file.path(here(""),"maps/20m_grid/20m_grid.shp"))

# load latest mortality data ####

## get the name of latest excel form
latest_FFFs <- list.files(here("raw_data/FFF_excel/"), pattern = ".xlsx", full.names = T)
latest_FFFs <- latest_FFFs[which.max(as.numeric(regmatches(latest_FFFs, regexpr("20\\d\\d", latest_FFFs))))] # take the latest file only


## load the latest mortality survey

mort <- as.data.frame(read_xlsx(latest_FFFs, sheet = "subform_1", .name_repair = "minimal" ))
mort <- mort[, unique(names(mort))]# remove repeated columns




# load all report that need to be fixed ####
all_errors_to_be_fixed <- list.files(file.path(here("testthat"), "reports/requires_field_fix/"), pattern = ".csv", full.names = T)
all_warnings_to_be_fixed <- list.files(file.path(here("testthat"), "reports/warnings/"), pattern = ".csv", full.names = T)

all_errors_to_be_fixed <- if(length(all_errors_to_be_fixed) >0) read.csv(all_errors_to_be_fixed) else NULL
all_warnings_to_be_fixed <- if(length(all_warnings_to_be_fixed) > 0) do.call(rbind, lapply(all_warnings_to_be_fixed, read.csv)) else NULL

quadrats_with_error <- unique(all_errors_to_be_fixed$Quad)
quadrats_with_warnings <- unique(all_warnings_to_be_fixed$Quad)


filename <- file.path(here("testthat"), "reports/map_of_error_and_warnings.png")

png(filename, width = 9, height = 8, units = "in", res = 300)
par(mar = c(0,3,0,0))

plot(quadrats)
plot(quadrats[quadrats$PLOT %in% mort$Quad,], col = "grey", add = T)
plot(quadrats[quadrats$PLOT %in%  quadrats_with_error, ], col = "orange", add = T)
plot(quadrats[quadrats$PLOT %in%  quadrats_with_warnings, ], col = "yellow", add = T)
plot(quadrats[quadrats$PLOT %in%  intersect(quadrats_with_warnings, quadrats_with_error), ], col = "red", add = T)
legend("bottomleft", fill = c("grey", "yellow", "orange", "red"), legend = c("done", "warning pending", "error pending", "warning & error pending"), bty = "n")

dev.off()

file.copy(from = filename, to = paste0(gsub("reports/map_of_error_and_warnings.png", "reports/sequence_of_maps/map_of_error_and_warnings_", filename), Sys.Date(), ".png"))
