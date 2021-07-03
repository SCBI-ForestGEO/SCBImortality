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

mort <- as.data.frame(read_xlsx(latest_FFFs, sheet = "subform_1", .name_repair = "minimal" ))

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


# fix empty lines in mort ####
idx_empty_line <- which(is.na(mort$Quad))
EAB_columns <- c("Crown thinning","Epicormic growth","EABF","D-shaped exit hole count","Crown position < 10 cm DBH")

# replace empty EAB column of line before by the EAB of empty lines
mort[idx_empty_line-1, EAB_columns] <- mort[idx_empty_line, EAB_columns] 

# remove empty lines
mort <- mort[!is.na(mort$Quad), ] # fix empty lines

# remove repeated columns
mort <- mort[, unique(names(mort))]


# --- PERFORM CHECKS ---- ####
# check if all species exist in species table, if not save a file, if yes, delete that file ####
filename <- file.path(here("testthat"), "reports/species_code_error.csv")

idx_error <- !mort$Species %in% spptable$sp

if(any(idx_error)) {
  write.csv(mort[idx_error,], file = filename, row.names = F)
} else {
  if(file.exists(filename) ) file.remove(filename)
}


# for each quadrat censused, check all expected trees were censused ####
filename <- file.path(here("testthat"), "reports/requires_field_fix/quadrat_censused_missing_stems.csv")


idx_quadrat_censused <- main_census$quadrat %in% as.numeric(mort$Quad)


tag_stem_with_error <- paste(main_census$tag, main_census$StemTag)[idx_quadrat_censused] [!paste(main_census$tag, main_census$StemTag)[idx_quadrat_censused] %in% paste(mort$Tag, mort$StemTag)]
table(main_census[paste(main_census$tag, main_census$StemTag) %in% tag_stem_with_error, ]$sp)



if(length(tag_stem_with_error) > 0) {
  write.csv(main_census[paste(main_census$tag, main_census$StemTag) %in% tag_stem_with_error, ], file = filename, row.names = F)
} else {
  if(file.exists(filename) ) file.remove(filename)
}




# for each quadrat censused, check that there is no duplicated stems ####
filename <- file.path(here("testthat"), "reports/will_auto_fix/quadrat_censused_duplicated_stems.csv")


tag_stem_with_error <- paste(mort$Tag, mort$StemTag)[duplicated(paste(mort$Tag, mort$StemTag))]


if(length(tag_stem_with_error) > 0) {
  write.csv(mort[paste(mort$Tag, mort$StemTag) %in% tag_stem_with_error, ], file = filename, row.names = F)
} else {
  if(file.exists(filename) ) file.remove(filename)
}






# check that all censused trees have a crown position recorded ####
filename <- file.path(here("testthat"), "reports/requires_field_fix/missing_crown_position.csv") # edit file name here


tag_stem_with_error <- paste(mort$Tag, mort$StemTag)[is.na(mort$'Crown position')]


if(length(tag_stem_with_error) > 0) {
  write.csv(mort[paste(mort$Tag, mort$StemTag) %in% tag_stem_with_error, ], file = filename, row.names = F)
} else {
  if(file.exists(filename) ) file.remove(filename)
}



# check that all censused trees have a percent of crown intact recorded ####
filename <- file.path(here("testthat"), "reports/requires_field_fix/missing_percent_crown_intact.csv") # edit file name here


tag_stem_with_error <- paste(mort$Tag, mort$StemTag)[is.na(mort$'Percentage of crown intact')]


if(length(tag_stem_with_error) > 0) {
  write.csv(mort[paste(mort$Tag, mort$StemTag) %in% tag_stem_with_error, ], file = filename, row.names = F)
} else {
  if(file.exists(filename) ) file.remove(filename)
}



# check that all censused trees have a percent of crown living recorded ####
filename <- file.path(here("testthat"), "reports/requires_field_fix/missing_percent_crown_living.csv") # edit file name here


tag_stem_with_error <- paste(mort$Tag, mort$StemTag)[is.na(mort$'Percentage of crown living')]


if(length(tag_stem_with_error) > 0) {
  write.csv(mort[paste(mort$Tag, mort$StemTag) %in% tag_stem_with_error, ], file = filename, row.names = F)
} else {
  if(file.exists(filename) ) file.remove(filename)
}


# check percent of crown living <=  percent of crown intact####
filename <- file.path(here("testthat"), "reports/requires_field_fix/crown_living_greater_than_crown_intact.csv") # edit file name here


tag_stem_with_error <- paste(mort$Tag, mort$StemTag)[!is.na(mort$'Percentage of crown living') & !is.na(mort$'Percentage of crown intact') & mort$'Percentage of crown living' > mort$'Percentage of crown intact']


if(length(tag_stem_with_error) > 0) {
  write.csv(mort[paste(mort$Tag, mort$StemTag) %in% tag_stem_with_error, ], file = filename, row.names = F)
} else {
  if(file.exists(filename) ) file.remove(filename)
}


# check that newly censused alive trees have no FAD selected; no record of wounded main stem, canker, or rotting trunk; DWR (dead with resprouts) not selected ####
filename <- file.path(here("testthat"), "reports/requires_field_fix/status_A_but_unhealthy.csv") # edit file name here

status_column <- rev(grep("Status", names(mort), value = T))[1]

idx_trees <- mort[, status_column] %in% "A"
idx_FAD <- !is.na(mort$FAD)
idx_wound <- !is.na(mort$'Wounded main stem')
idx_canker <- !is.na(mort$'Canker; swelling, deformity')
idx_rot <- !is.na(mort$'Rotting trunk')
idx_DWR <- !mort$'DWR' %in% "False"


tag_stem_with_error <- paste(mort$Tag, mort$StemTag)[idx_trees & (idx_FAD | idx_wound | idx_wound | idx_canker | idx_rot | idx_DWR)]


if(length(tag_stem_with_error) > 0) {
  write.csv(mort[paste(mort$Tag, mort$StemTag) %in% tag_stem_with_error, ], file = filename, row.names = F)
} else {
  if(file.exists(filename) ) file.remove(filename)
}




## and vice-versa ####
filename <- file.path(here("testthat"), "reports/will_auto_fix/unhealthy_but_wrong_status.csv") # edit file name here

status_column <- rev(grep("Status", names(mort), value = T))[1]

idx_trees <- mort[, status_column] %in% c("AU", "DC", "DS")
idx_FAD <- !is.na(mort$FAD)
idx_wound <- !is.na(mort$'Wounded main stem')
idx_canker <- !is.na(mort$'Canker; swelling, deformity')
idx_rot <- !is.na(mort$'Rotting trunk')
idx_DWR <- !mort$'DWR' %in% "False"


tag_stem_with_error <- paste(mort$Tag, mort$StemTag)[!idx_trees & (idx_FAD | idx_wound | idx_wound | idx_canker | idx_rot | idx_DWR)]


if(length(tag_stem_with_error) > 0) {
  write.csv(mort[paste(mort$Tag, mort$StemTag) %in% tag_stem_with_error, ], file = filename, row.names = F)
} else {
  if(file.exists(filename) ) file.remove(filename)
}




# check that status 'AU' does not have 	DWR (dead with resprouts)  selected ####
filename <- file.path(here("testthat"), "reports/requires_field_fix/status_AU_but_DWR_selected.csv") # edit file name here

status_column <- rev(grep("Status", names(mort), value = T))[1]

idx_trees <- mort[, status_column] %in% "AU"
idx_DWR <- !mort$'DWR' %in% "False"


tag_stem_with_error <- paste(mort$Tag, mort$StemTag)[idx_trees & idx_DWR]


if(length(tag_stem_with_error) > 0) {
  write.csv(mort[paste(mort$Tag, mort$StemTag) %in% tag_stem_with_error, ], file = filename, row.names = F)
} else {
  if(file.exists(filename) ) file.remove(filename)
}


# check that status 'DS' or 'DC' have a dbh measured  ####
filename <- file.path(here("testthat"), "reports/requires_field_fix/status_DS_or_DC_but_DBH_measured.csv") # edit file name here

status_column <- rev(grep("Status", names(mort), value = T))[1]

idx_trees <- mort[, status_column] %in% c("DS", "DC")
idx_no_DBH_if_dead <- is.na(mort$'Dead DBH')


tag_stem_with_error <- paste(mort$Tag, mort$StemTag)[idx_trees & idx_no_DBH_if_dead]


if(length(tag_stem_with_error) > 0) {
  write.csv(mort[paste(mort$Tag, mort$StemTag) %in% tag_stem_with_error, ], file = filename, row.names = F)
} else {
  if(file.exists(filename) ) file.remove(filename)
}





# check that status 'DS' or 'DC' have a dbh within 2cm of most recent census DBH  ####
filename <- file.path(here("testthat"), "reports/warnings/DBH_dead_suspicious.csv") # edit file name here

status_column <- rev(grep("Status", names(mort), value = T))[1]

idx_trees <- mort[, status_column] %in% c("DS", "DC")
idx_DBH_ouside_range <- !is.na(mort$'Dead DBH') & (abs(mort$'Dead DBH' - as.numeric(mort$DBH)) > 20)


tag_stem_with_error <- paste(mort$Tag, mort$StemTag)[idx_trees & idx_DBH_ouside_range]


if(length(tag_stem_with_error) > 0) {
  write.csv(mort[paste(mort$Tag, mort$StemTag) %in% tag_stem_with_error, ], file = filename, row.names = F)
} else {
  if(file.exists(filename) ) file.remove(filename)
}




# check that newly censused 'AU', 'DS' or 'DC trees have at least one FAD  selected ####
filename <- file.path(here("testthat"), "reports/requires_field_fix/status_AU_DS_or_DC_but_no_FAD.csv") # edit file name here

status_column <- rev(grep("Status", names(mort), value = T))[1]

idx_trees <- mort[, status_column] %in% c("AU","DS", "DC")
idx_no_FAD <- is.na(mort$FAD)

tag_stem_with_error <- paste(mort$Tag, mort$StemTag)[idx_trees & idx_no_FAD ]


if(length(tag_stem_with_error) > 0) {
  write.csv(mort[paste(mort$Tag, mort$StemTag) %in% tag_stem_with_error, ], file = filename, row.names = F)
} else {
  if(file.exists(filename) ) file.remove(filename)
}



# check that newly censused 'AU', 'DS' or 'DC trees have at one photo taken ####
# filename <- file.path(here("testthat"), "reports/status_AU_DS_or_DC_but_no_photo.csv") # edit file name here
# 
# status_column <- rev(grep("Status", names(mort), value = T))[1]
# 
# idx_trees <- mort[, status_column] %in% c("AU","DS", "DC")
# idx_no_FAD <- is.na(mort$FAD)
# 
# tag_stem_with_error <- paste(mort$Tag, mort$StemTag)[idx_trees & idx_no_FAD ]
# 
# 
# if(length(tag_stem_with_error) > 0) {
#   write.csv(mort[paste(mort$Tag, mort$StemTag) %in% tag_stem_with_error, ], file = filename, row.names = F)
# } else {
#   if(file.exists(filename) ) file.remove(filename)
# }
# 
# 

# check that newly censused 'AU', 'DS' or 'DC with "wound" selected as FAD have selected a level for wounded main stem ####
filename <- file.path(here("testthat"), "reports/requires_field_fix/wounded_but_no_level.csv") # edit file name here

status_column <- rev(grep("Status", names(mort), value = T))[1]

idx_trees <- mort[, status_column] %in% c("AU","DS", "DC")
idx_wounded <- !is.na(mort$FAD) & grepl("W", mort$FAD)
idx_wnd_main_stem <- !is.na(mort$'Wounded main stem')


tag_stem_with_error <- paste(mort$Tag, mort$StemTag)[idx_trees & idx_wounded & !idx_wnd_main_stem ]


if(length(tag_stem_with_error) > 0) {
  write.csv(mort[paste(mort$Tag, mort$StemTag) %in% tag_stem_with_error, ], file = filename, row.names = F)
} else {
  if(file.exists(filename) ) file.remove(filename)
}




## and vice versa ####
filename <- file.path(here("testthat"), "reports/will_auto_fix/wounded_level_but_wrong_status_or_FAD.csv") # edit file name here

status_column <- rev(grep("Status", names(mort), value = T))[1]

idx_trees <- mort[, status_column] %in% c("AU","DS", "DC")
idx_wounded <- !is.na(mort$FAD) & grepl("W", mort$FAD)
idx_wnd_main_stem <- !is.na(mort$'Wounded main stem')


tag_stem_with_error <- paste(mort$Tag, mort$StemTag)[(!idx_trees | !idx_wounded) & idx_wnd_main_stem ]


if(length(tag_stem_with_error) > 0) {
  write.csv(mort[paste(mort$Tag, mort$StemTag) %in% tag_stem_with_error, ], file = filename, row.names = F)
} else {
  if(file.exists(filename) ) file.remove(filename)
}




# check that newly censused 'AU', 'DS' or 'DC with "canker" selected as FAD have selected a level for canker,swelling,deformity ####
filename <- file.path(here("testthat"), "reports/requires_field_fix/canker_but_no_level.csv") # edit file name here

status_column <- rev(grep("Status", names(mort), value = T))[1]

idx_trees <- mort[, status_column] %in% c("AU","DS", "DC")
idx_canker <- !is.na(mort$FAD) & grepl("K", mort$FAD)
idx_ckr_level <- !is.na(mort$'canker,swelling,deformity')


tag_stem_with_error <- paste(mort$Tag, mort$StemTag)[idx_trees & idx_canker & !idx_ckr_level ]


if(length(tag_stem_with_error) > 0) {
  write.csv(mort[paste(mort$Tag, mort$StemTag) %in% tag_stem_with_error, ], file = filename, row.names = F)
} else {
  if(file.exists(filename) ) file.remove(filename)
}




## and vice versa ####
filename <- file.path(here("testthat"), "reports/will_auto_fix/canker_level_but_wrong_status_or_FAD.csv") # edit file name here

status_column <- rev(grep("Status", names(mort), value = T))[1]

idx_trees <- mort[, status_column] %in% c("AU","DS", "DC")
idx_canker <- !is.na(mort$FAD) & grepl("K", mort$FAD)
idx_ckr_level <- !is.na(mort$'canker,swelling,deformity')


tag_stem_with_error <- paste(mort$Tag, mort$StemTag)[(!idx_trees & !idx_canker) & idx_ckr_level ]


if(length(tag_stem_with_error) > 0) {
  write.csv(mort[paste(mort$Tag, mort$StemTag) %in% tag_stem_with_error, ], file = filename, row.names = F)
} else {
  if(file.exists(filename) ) file.remove(filename)
}




# check that newly censused 'AU', 'DS' or 'DC with "rotting stem" selected as FAD have selected a level for rotting main stem ####
filename <- file.path(here("testthat"), "reports/requires_field_fix/rot_but_no_level.csv") # edit file name here

status_column <- rev(grep("Status", names(mort), value = T))[1]

idx_trees <- mort[, status_column] %in% c("AU","DS", "DC")
idx_rot <- !is.na(mort$FAD) & grepl("R\\>", mort$FAD)
idx_rot_level <- !is.na(mort$'rotting main stem')


tag_stem_with_error <- paste(mort$Tag, mort$StemTag)[idx_trees & idx_rot & !idx_rot_level ]


if(length(tag_stem_with_error) > 0) {
  write.csv(mort[paste(mort$Tag, mort$StemTag) %in% tag_stem_with_error, ], file = filename, row.names = F)
} else {
  if(file.exists(filename) ) file.remove(filename)
}




## and vice versa ####
filename <- file.path(here("testthat"), "reports/will_auto_fix/rot_level_but_wrong_status_or_FAD.csv") # edit file name here

status_column <- rev(grep("Status", names(mort), value = T))[1]

idx_trees <- mort[, status_column] %in% c("AU","DS", "DC")
idx_rot <- !is.na(mort$FAD) & grepl("R\\>", mort$FAD)
idx_rot_level <- !is.na(mort$'rotting main stem')


tag_stem_with_error <- paste(mort$Tag, mort$StemTag)[(!idx_trees & !idx_rot) & idx_rot_level ]


if(length(tag_stem_with_error) > 0) {
  write.csv(mort[paste(mort$Tag, mort$StemTag) %in% tag_stem_with_error, ], file = filename, row.names = F)
} else {
  if(file.exists(filename) ) file.remove(filename)
}





# check that newly censused 'A' or 'AU', were A or AU in previous year ####
filename <- file.path(here("testthat"), "reports/warnings/Dead_but_now_alive.csv") # edit file name here

status_column <- rev(grep("Status", names(mort), value = T))[1]
previous_status_column <- rev(grep("Status", names(mort), value = T))[2]


idx_trees <- mort[, status_column] %in% c("AU","A")
idx_previously_dead <- !mort[,previous_status_column] %in% c("AU","A") & !is.na(mort[,previous_status_column])


tag_stem_with_error <- paste(mort$Tag, mort$StemTag)[idx_trees & idx_previously_dead ]


if(length(tag_stem_with_error) > 0) {
  write.csv(mort[paste(mort$Tag, mort$StemTag) %in% tag_stem_with_error, ], file = filename, row.names = F)
} else {
  if(file.exists(filename) ) file.remove(filename)
}


# check that newly censused 'A' or 'AU' or 'DS', were not 'DC' in previous year ####
filename <- file.path(here("testthat"), "reports/warnings/DC_but_now_A_AU_or_DS.csv") # edit file name here

status_column <- rev(grep("Status", names(mort), value = T))[1]
previous_status_column <- rev(grep("Status", names(mort), value = T))[2]


idx_trees <- mort[, status_column] %in% c("AU","A", "DS")
idx_previously_dead <- mort[,previous_status_column] %in% c("DC") & !is.na(mort[,previous_status_column])


tag_stem_with_error <- paste(mort$Tag, mort$StemTag)[idx_trees & idx_previously_dead ]


if(length(tag_stem_with_error) > 0) {
  write.csv(mort[paste(mort$Tag, mort$StemTag) %in% tag_stem_with_error, ], file = filename, row.names = F)
} else {
  if(file.exists(filename) ) file.remove(filename)
}

# check that newly  censused trees (FRAM, FRNI, FRPE, FRSP, or CHVI), have Crown thinning, Epicormic growth, D-shaped exit hole count, Crown position < 10 cm DBH (for stems <10cm) all recorded ####
filename <- file.path(here("testthat"), "reports/requires_field_fix/missing_EAB_info.csv") # edit file name here



idx_trees <- mort$Species %in% c( "fram", "frni", "frpe", "frsp", "chvi")
idx_missing_EAB_info <- !complete.cases(mort[, c("Crown thinning", "Epicormic growth", "D-shaped exit hole count", "Crown position < 10 cm DBH") ])


tag_stem_with_error <- paste(mort$Tag, mort$StemTag)[idx_trees & idx_missing_EAB_info ]


if(length(tag_stem_with_error) > 0) {
  write.csv(mort[paste(mort$Tag, mort$StemTag) %in% tag_stem_with_error, ], file = filename, row.names = F)
} else {
  if(file.exists(filename) ) file.remove(filename)
}




# check that, for newly censused trees (FRAM, FRNI, FRPE, FRSP, or CHVI),	if Epicormic growth>0, tree is AU ####
filename <- file.path(here("testthat"), "reports/will_auto_fix/epicormic_growth_but_not_AU.csv") # edit file name here

status_column <- rev(grep("Status", names(mort), value = T))[1]


idx_trees <- mort$Species %in% c( "fram", "frni", "frpe", "frsp", "chvi")
idx_epicormic <- !is.na(mort$`Epicormic growth`) & mort$`Epicormic growth` > 0
idx_status <- mort[, status_column] %in% c("AU")



tag_stem_with_error <- paste(mort$Tag, mort$StemTag)[idx_trees & idx_epicormic & !idx_status ]


if(length(tag_stem_with_error) > 0) {
  write.csv(mort[paste(mort$Tag, mort$StemTag) %in% tag_stem_with_error, ], file = filename, row.names = F)
} else {
  if(file.exists(filename) ) file.remove(filename)
}


# check that, for newly censused trees (FRAM, FRNI, FRPE, FRSP, or CHVI),	if Crown thinning>1, tree is AU or dead ####
filename <- file.path(here("testthat"), "reports/requires_field_fix/crown_thinning_more_than_1_but_not_AU_or_dead.csv") # edit file name here

status_column <- rev(grep("Status", names(mort), value = T))[1]


idx_trees <- mort$Species %in% c( "fram", "frni", "frpe", "frsp", "chvi")
idx_crown <- !is.na(mort$`Crown thinning`) & mort$`Crown thinning` > 1
idx_status <- mort[, status_column] %in% c("A")



tag_stem_with_error <- paste(mort$Tag, mort$StemTag)[idx_trees & idx_crown & idx_status ]


if(length(tag_stem_with_error) > 0) {
  write.csv(mort[paste(mort$Tag, mort$StemTag) %in% tag_stem_with_error, ], file = filename, row.names = F)
} else {
  if(file.exists(filename) ) file.remove(filename)
}


# check that, for newly censused trees (FRAM, FRNI, FRPE, FRSP, or CHVI),	if any EABF recorded, tree is AU or dead ####
filename <- file.path(here("testthat"), "reports/requires_field_fix/EABF_recorded_but_not_AU_or_dead.csv") # edit file name here

status_column <- rev(grep("Status", names(mort), value = T))[1]


idx_trees <- mort$Species %in% c( "fram", "frni", "frpe", "frsp", "chvi")
idx_EABF <- !is.na(mort$EABF) & !mort$EABF %in% "none"
idx_status <- mort[, status_column] %in% c("A")



tag_stem_with_error <- paste(mort$Tag, mort$StemTag)[idx_trees & idx_EABF & idx_status ]


if(length(tag_stem_with_error) > 0) {
  write.csv(mort[paste(mort$Tag, mort$StemTag) %in% tag_stem_with_error, ], file = filename, row.names = F)
} else {
  if(file.exists(filename) ) file.remove(filename)
}


# check that, for newly censused trees (FRAM, FRNI, FRPE, FRSP, or CHVI),	if D-shaped exit hole count>0, tree is AU or dead ####
filename <- file.path(here("testthat"), "reports/requires_field_fix/exit_hole_count_but_not_AU_or_dead.csv") # edit file name here

status_column <- rev(grep("Status", names(mort), value = T))[1]


idx_trees <- mort$Species %in% c( "fram", "frni", "frpe", "frsp", "chvi")
idx_exit_hole <- mort$`D-shaped exit hole count` > 0 & !is.na(mort$`D-shaped exit hole count`)
idx_status <- mort[, status_column] %in% c("A")



tag_stem_with_error <- paste(mort$Tag, mort$StemTag)[idx_trees & idx_exit_hole & idx_status ]


if(length(tag_stem_with_error) > 0) {
  write.csv(mort[paste(mort$Tag, mort$StemTag) %in% tag_stem_with_error, ], file = filename, row.names = F)
} else {
  if(file.exists(filename) ) file.remove(filename)
}



# check that, for newly censused trees (FRAM, FRNI, FRPE, FRSP, or CHVI),	if tree is dead, Epicormic growth=0 ####
filename <- file.path(here("testthat"), "reports/requires_field_fix/dead_but_epicormic_more_than_0.csv") # edit file name here

status_column <- rev(grep("Status", names(mort), value = T))[1]


idx_trees <- mort$Species %in% c( "fram", "frni", "frpe", "frsp", "chvi")
idx_epicormic <- !is.na(mort$`Epicormic growth`) & mort$`Epicormic growth` > 0
idx_status <- !mort[, status_column] %in% c("A", "AU")



tag_stem_with_error <- paste(mort$Tag, mort$StemTag)[idx_trees & idx_epicormic & idx_status ]


if(length(tag_stem_with_error) > 0) {
  write.csv(mort[paste(mort$Tag, mort$StemTag) %in% tag_stem_with_error, ], file = filename, row.names = F)
} else {
  if(file.exists(filename) ) file.remove(filename)
}


# check that, for newly censused trees (FRAM, FRNI, FRPE, FRSP, or CHVI),	if tree is dead, Crown thinning=5 ####
filename <- file.path(here("testthat"), "reports/will_auto_fix/dead_but_crown_thinning_less_than_5.csv") # edit file name here

status_column <- rev(grep("Status", names(mort), value = T))[1]


idx_trees <- mort$Species %in% c( "fram", "frni", "frpe", "frsp", "chvi")
idx_crown <- !is.na(mort$`Crown thinning`) & mort$`Crown thinning` <5
idx_status <- !mort[, status_column] %in% c("A", "AU")



tag_stem_with_error <- paste(mort$Tag, mort$StemTag)[idx_trees & idx_crown & idx_status ]


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




# KEEP TRACK OF ALL THE ISSUES ####

all_reports <- list.files(here("testthat/reports/", c("requires_field_fix", "will_auto_fix")), recursive = T, pattern = ".csv", full.names = T)


for(f in all_reports) {
  
  new_f <- gsub("/reports/", "/reports/trace_of_reports/", f)
  
  if(file.exists(new_f)) write.csv(unique(rbind(read.csv(new_f), read.csv(f))), file = new_f, row.names = F)
  else write.csv(read.csv(f), file = new_f, row.names = F)
  
}
