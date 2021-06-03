# Generate reports looking at latest mortality census raw data ####
## this script is run automatically when there is a push 

# clear environment ####
rm(list = ls())

# load libraries ####
library(here)
library(readxl)

# load latest mortality data ####

## get the name of latest excel form
latest_FFFs <- list.files(here("raw_data/FFF_excel/"), pattern = ".xlsx", full.names = T)


## load the latest mortality survey

mort <- read_xlsx(latest_FFFs, sheet = "subform_1")
mort <- mort[!is.na(mort$Quad), ] # remove empty lines

# load and clean up the 3rd main census ####
main_census <-  read.csv(paste0("https://raw.githubusercontent.com/SCBI-ForestGEO/SCBI-ForestGEO-Data/master/tree_main_census/data/census-csv-files/scbi.stem3.csv"))

## convert dbh to numeric
main_census$dbh <- as.numeric(main_census$dbh)

## only keep trees > 10cm except for fraxinus and Chionanthus virginicus
main_census <-  main_census[grepl("^fr..|^ch..", main_census$sp) | (!is.na(main_census$dbh) & main_census$dbh >= 100), ]

## remove trees that are dead
main_census <- main_census[!main_census$status %in% "D",]

# load species table ####

spptable <- read.csv("https://raw.githubusercontent.com/SCBI-ForestGEO/SCBI-ForestGEO-Data/master/tree_main_census/data/census-csv-files/scbi.spptable.csv")


# check if all species exist in species table, if not save a file, if yes, delete that file ####
filename <- file.path(here("testthat"), "reports/species_code_error.csv")

idx_error <- !mort$Species %in% spptable$sp

if(any(idx_error)) {
  write.csv(mort[idx_error,], file = filename, row.names = F)
} else {
  if(file.exists(filename) ) file.remove(filename)
}


# for each quadrat censused, check all expected trees were censused ####
filename <- file.path(here("testthat"), "reports/quadrat_censused_missing_stems.csv")


idx_quadrat_censused <- main_census$quadrat %in% as.numeric(mort$Quad)


tag_stem_with_error <- paste(main_census$tag, main_census$StemTag)[idx_quadrat_censused] [!paste(main_census$tag, main_census$StemTag)[idx_quadrat_censused] %in% paste(mort$Tag, mort$StemTag)]
table(main_census[paste(main_census$tag, main_census$StemTag) %in% tag_stem_with_error, ]$sp)



if(length(tag_stem_with_error) > 0) {
  write.csv(main_census[paste(main_census$tag, main_census$StemTag) %in% tag_stem_with_error, ], file = filename, row.names = F)
} else {
  if(file.exists(filename) ) file.remove(filename)
}




# for each quadrat censused, check that there is no duplicated stems ####
filename <- file.path(here("testthat"), "reports/quadrat_censused_duplicated_stems.csv")


tag_stem_with_error <- paste(mort$Tag, mort$StemTag)[duplicated(paste(mort$Tag, mort$StemTag))]


if(length(tag_stem_with_error) > 0) {
  write.csv(mort[paste(mort$Tag, mort$StemTag) %in% tag_stem_with_error, ], file = filename, row.names = F)
} else {
  if(file.exists(filename) ) file.remove(filename)
}






# check that all censused trees have a crown position recorded ####
filename <- file.path(here("testthat"), "reports/missing_crown_position.csv") # edit file name here


tag_stem_with_error <- paste(mort$Tag, mort$StemTag)[is.na(mort$'Crown position')]


if(length(tag_stem_with_error) > 0) {
  write.csv(mort[paste(mort$Tag, mort$StemTag) %in% tag_stem_with_error, ], file = filename, row.names = F)
} else {
  if(file.exists(filename) ) file.remove(filename)
}



# check that all censused trees have a percent of crown intact recorded ####
filename <- file.path(here("testthat"), "reports/missing_percent_crown_intact.csv") # edit file name here


tag_stem_with_error <- paste(mort$Tag, mort$StemTag)[is.na(mort$'Percentage of crown intact')]


if(length(tag_stem_with_error) > 0) {
  write.csv(mort[paste(mort$Tag, mort$StemTag) %in% tag_stem_with_error, ], file = filename, row.names = F)
} else {
  if(file.exists(filename) ) file.remove(filename)
}



# check that all censused trees have a percent of crown living recorded ####
filename <- file.path(here("testthat"), "reports/missing_percent_crown_living.csv") # edit file name here


tag_stem_with_error <- paste(mort$Tag, mort$StemTag)[is.na(mort$'Percentage of crown living')]


if(length(tag_stem_with_error) > 0) {
  write.csv(mort[paste(mort$Tag, mort$StemTag) %in% tag_stem_with_error, ], file = filename, row.names = F)
} else {
  if(file.exists(filename) ) file.remove(filename)
}


# check percent of crown living <=  percent of crown intact####
filename <- file.path(here("testthat"), "reports/crown_living_greater_than_crown_intact.csv") # edit file name here


tag_stem_with_error <- paste(mort$Tag, mort$StemTag)[!is.na(mort$'Percentage of crown living') & !is.na(mort$'Percentage of crown intact') & mort$'Percentage of crown living' > mort$'Percentage of crown intact']


if(length(tag_stem_with_error) > 0) {
  write.csv(mort[paste(mort$Tag, mort$StemTag) %in% tag_stem_with_error, ], file = filename, row.names = F)
} else {
  if(file.exists(filename) ) file.remove(filename)
}


# check that newly censuded alive trees have no FAD is selected; no record of wounded main stem, canker, or rotting trunk; DWR (dead with resprouts) not selected ####
filename <- file.path(here("testthat"), "reports/status_A_but_unhealthy.csv") # edit file name here

status_column <- rev(grep("Status", names(mort), value = T))[1]

idx_live_trees <- mort[, status_column] %in% "A"
idx_FAD <- !is.na(mort$FAD)
idx_wound <- !is.na(mort$'Wounded main stem')
idx_canker <- !is.na(mort$'Canker; swelling, deformity')
idx_rot <- !is.na(mort$'Rotting trunk')
idx_DWR <- !mort$'DWR' %in% "False"


tag_stem_with_error <- paste(mort$Tag, mort$StemTag)[idx_live_trees & (idx_FAD | idx_wound | idx_wound | idx_canker | idx_rot | idx_DWR)]


if(length(tag_stem_with_error) > 0) {
  write.csv(mort[paste(mort$Tag, mort$StemTag) %in% tag_stem_with_error, ], file = filename, row.names = F)
} else {
  if(file.exists(filename) ) file.remove(filename)
}



# give a % completion status ####
percent_completion <- round(sum(paste(main_census$tag, main_census$StemTag) %in% paste(mort$Tag, mort$StemTag)) / nrow(main_census) * 100)

png(file.path(here("testthat"), "reports/percent_completion.png"), width = 1, height = 1, units = "in", res = 150)
par(mar = c(0,0,0,0))
plot(0,0, axes = F, xlab = "", ylab = "", type = "n")
text(0,0, paste(percent_completion, "%"))
dev.off()
# write.table(percent_completion, file = file.path(here("testthat"), "reports/percent_completion.txt"),  col.names = F, row.names = F)
