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
latest_FFFs <- latest_FFFs[which.max(as.numeric(regmatches(latest_FFFs, regexpr("20\\d\\d", latest_FFFs))))] # take the latest file only


## load the latest mortality survey

mort <- as.data.frame(read_xlsx(latest_FFFs, sheet = "subform_1", .name_repair = "minimal" ))

mort_root <- as.data.frame(read_xlsx(latest_FFFs, sheet = "Root", .name_repair = "minimal" ))
mort <- cbind(SurveyorID = mort_root$Personnel[match(mort$`Submission Id`, mort_root$`Submission Id`)],
              date = mort_root$"Date/Time"[match(mort$`Submission Id`, mort_root$`Submission Id`)],
              mort)

# load and clean up the 3rd main census ####
main_census <-  read.csv(paste0("https://raw.githubusercontent.com/SCBI-ForestGEO/SCBI-ForestGEO-Data/master/tree_main_census/data/census-csv-files/scbi.stem3.csv"))

## convert dbh to numeric
main_census$dbh <- as.numeric(main_census$dbh)

## only keep trees > 10cm except for fraxinus and Chionanthus virginicus
main_census <-  main_census[grepl("^fr..|^ch..", main_census$sp) | (!is.na(main_census$dbh) & main_census$dbh >= 100), ]

## remove trees that are dead
# main_census <- main_census[!main_census$status %in% "D",]
main_census <- main_census[!grepl("DC|DN|DT", main_census$codes),] # see https://github.com/SCBI-ForestGEO/SCBImortality/issues/31#issuecomment-881702404


# load species table ####

spptable <- read.csv("https://raw.githubusercontent.com/SCBI-ForestGEO/SCBI-ForestGEO-Data/master/tree_main_census/data/census-csv-files/scbi.spptable.csv")


# fix empty lines in mort ####
idx_empty_line <- which(is.na(mort$Quad))
EAB_columns <- c("Crown thinning","Epicormic growth","EABF","D-shaped exit hole count","Crown position < 10 cm DBH")

# replace empty EAB column of line before by the EAB of empty lines
mort[idx_empty_line-1, EAB_columns] <- mort[idx_empty_line, EAB_columns] 

# remove empty lines
mort <- mort[!is.na(mort$Quad), ] # fix empty lines
mort <- mort[!is.na(mort$Species), ] # fix empty lines
mort <- mort[!is.na(mort$Tag), ] # fix empty lines

# remove repeated columns
mort <- mort[, unique(names(mort))]


# give a % completion status ####
percent_completion <- round(sum(paste(main_census$tag, main_census$StemTag) %in% paste(mort$Tag, mort$StemTag)) / nrow(main_census) * 100)

png(file.path(here("testthat"), "reports/percent_completion.png"), width = 1, height = 1, units = "in", res = 150)
par(mar = c(0,0,0,0))
plot(0,0, axes = F, xlab = "", ylab = "", type = "n")
text(0,0, paste(percent_completion, "%"))
dev.off()
# write.table(percent_completion, file = file.path(here("testthat"), "reports/percent_completion.txt"),  col.names = F, row.names = F)


# --- PERFORM CHECKS ---- ####

# prepare log files #####
require_field_fix_error_file <- NULL
will_auto_fix_error_file <- NULL
warning_file <- NULL


# for each quadrat censused, check all expected trees were censused ####
# filename <- file.path(here("testthat"), "reports/requires_field_fix/quadrat_censused_missing_stems.csv")
error_name = "missing_stem"


# idx_quadrat_censused <- main_census$quadrat %in% as.numeric(mort$Quad)


idx_errors <- !paste(main_census$tag, main_census$StemTag) %in% paste(mort$Tag, mort$StemTag) & main_census$quadrat %in% as.numeric(mort$Quad)




if(sum(idx_errors) > 0) {
  # write.csv(main_census[paste(main_census$tag, main_census$StemTag) %in% idx_errors, ], file = filename, row.names = F)
  data_to_add <- mort[1:sum(idx_errors),]
  data_to_add[1:sum(idx_errors), ] <- NA
  data_to_add[, c("Quad",
                  "Tag",
                  "StemTag",
                  "Species",
                  "QX",
                  "QY",
                  "DBH",
                  # "Status 2018",
                  "HOM")] <-
    main_census[idx_errors, c("quadrat", "tag", "StemTag", "sp", "gx", "gy", "dbh",  "status")]

  require_field_fix_error_file <- rbind(require_field_fix_error_file, data.frame(data_to_add, error_name))
  
  
  
} 
# else {
#   if(file.exists(filename) ) file.remove(filename)
# }



# remove any tree with current status DN as we don't need to check errors on those ####
status_column <- rev(grep("Status", names(mort), value = T))[1]

idx_trees <- !mort[, status_column] %in% c("DN")

mort <- mort[idx_trees, ]


# check if all species exist in species table, if not save a file, if yes, delete that file ####
error_name <- "species_code_error"

idx_error <- !mort$Species %in% spptable$sp

if(sum(idx_error) > 0) require_field_fix_error_file <- rbind(require_field_fix_error_file, data.frame(mort[idx_error,], error_name))





# for each quadrat censused, check that there is no duplicated stems ####
filename <- file.path(here("testthat"), "reports/will_auto_fix/quadrat_censused_duplicated_stems.csv")


idx_errors <- paste(mort$Tag, mort$StemTag)[duplicated(paste(mort$Tag, mort$StemTag))]


# if(length(idx_errors) > 0) {
  write.csv(mort[paste(mort$Tag, mort$StemTag) %in% idx_errors, ], file = filename, row.names = F)
# } else {
#   if(file.exists(filename) ) file.remove(filename)
# }






# check that all censused trees have a crown position recorded ####
error_name <- "missing_crown_position"


status_column <- rev(grep("Status", names(mort), value = T))[1]

idx_trees <- mort[, status_column] %in% c("A", "AU", "DS")

idx_errors <- is.na(mort$'Crown position') & idx_trees

if(sum(idx_errors) > 0) require_field_fix_error_file <- rbind(require_field_fix_error_file, data.frame(mort[idx_errors, ], error_name))



# check that all censused trees have a percent of crown intact recorded ####
error_name <- "missing_percent_crown_intact"


status_column <- rev(grep("Status", names(mort), value = T))[1]

idx_trees <- mort[, status_column] %in% c("A", "AU", "DS")

idx_errors <- is.na(mort$'Percentage of crown intact') & idx_trees


if(sum(idx_errors) > 0) require_field_fix_error_file <- rbind(require_field_fix_error_file, data.frame(mort[idx_errors, ], error_name))


# check that all censused trees have a percent of crown living recorded ####
error_name <- "missing_percent_crown_living"


status_column <- rev(grep("Status", names(mort), value = T))[1]

idx_trees <- mort[, status_column] %in% c("A", "AU", "DS")

idx_errors <- is.na(mort$'Percentage of crown living') & idx_trees


if(sum(idx_errors) > 0) require_field_fix_error_file <- rbind(require_field_fix_error_file, data.frame(mort[idx_errors, ] , error_name))


# check percent of crown living <=  percent of crown intact####
error_name <- "crown_living_greater_than_crown_intact"


status_column <- rev(grep("Status", names(mort), value = T))[1]

idx_trees <- mort[, status_column] %in% c("A", "AU", "DS")

idx_errors <- !is.na(mort$'Percentage of crown living') & !is.na(mort$'Percentage of crown intact') & mort$'Percentage of crown living' > mort$'Percentage of crown intact' & idx_trees


if(sum(idx_errors) > 0) require_field_fix_error_file <- rbind(require_field_fix_error_file, data.frame(mort[idx_errors, ] , error_name))


# check percent newly censused trees (DS or DC)	have percentage of crown living = 0####
error_name <- "dead_but_crown_living_not_zero"


status_column <- rev(grep("Status", names(mort), value = T))[1]

idx_trees <- mort[, status_column] %in% c("DS", "DC")

idx_errors <- !is.na(mort$'Percentage of crown living') & mort$'Percentage of crown living'> 0 &  idx_trees


if(sum(idx_errors) > 0) will_auto_fix_error_file <- rbind(will_auto_fix_error_file, data.frame(mort[idx_errors, ] , error_name))


# check that newly censused alive trees have no FAD selected; no record of wounded main stem, canker, or rotting trunk; DWR (dead with resprouts) not selected ####
error_name <- "status_A_but_unhealthy"

status_column <- rev(grep("Status", names(mort), value = T))[1]

idx_trees <- mort[, status_column] %in% "A"
idx_FAD <- !is.na(mort$FAD)
idx_wound <- !is.na(mort$'Wounded main stem')
idx_canker <- !is.na(mort$'Canker; swelling, deformity')
idx_rot <- !is.na(mort$'Rotting trunk')
idx_DWR <- !is.na(mort$'DWR') & !mort$'DWR' %in% "False"


idx_errors <- idx_trees & (idx_FAD | idx_wound | idx_wound | idx_canker | idx_rot | idx_DWR)


if(sum(idx_errors) > 0) require_field_fix_error_file <- rbind(require_field_fix_error_file, data.frame(mort[idx_errors, ], error_name))



## and vice-versa ####
# error_name <- "unhealthy_but_wrong_status"
# 
# status_column <- rev(grep("Status", names(mort), value = T))[1]
# 
# idx_trees <- mort[, status_column] %in% c("AU", "DC", "DS")
# idx_FAD <- !is.na(mort$FAD)
# idx_wound <- !is.na(mort$'Wounded main stem')
# idx_canker <- !is.na(mort$'Canker; swelling, deformity')
# idx_rot <- !is.na(mort$'Rotting trunk')
# idx_DWR <- !is.na(mort$'DWR') & !mort$'DWR' %in% "False"
# 
# 
# idx_errors <- !idx_trees & (idx_FAD | idx_wound | idx_wound | idx_canker | idx_rot | idx_DWR)
# 
# if(sum(idx_errors) > 0) will_auto_fix_error_file <- rbind(will_auto_fix_error_file, data.frame(mort[idx_errors, ], error_name))
# 





# check that status 'AU' does not have 	DWR (dead with resprouts)  selected ####
error_name <- "status_AU_but_DWR_selected"

status_column <- rev(grep("Status", names(mort), value = T))[1]

idx_trees <- mort[, status_column] %in% "AU"
idx_DWR <- !is.na(mort$'DWR') & !mort$'DWR' %in% "False"


idx_errors <- idx_trees & idx_DWR

if(sum(idx_errors) > 0) require_field_fix_error_file <- rbind(require_field_fix_error_file, data.frame(mort[idx_errors, ], error_name))


# check that status 'DS' or 'DC' have a dbh measured  ####
error_name <- "status_DS_or_DC_but_DBH_not_measured"

status_column <- rev(grep("Status", names(mort), value = T))[1]
previous_status_column <- rev(grep("Status", names(mort), value = T))[2]

idx_trees <- mort[, status_column] %in% c("DS", "DC")
idx_previously_dead <- !mort[,previous_status_column] %in% c("AU","A") & !is.na(mort[,previous_status_column])
idx_no_DBH_if_dead <- is.na(mort$'Dead DBH')


idx_errors <- idx_trees & idx_no_DBH_if_dead & !idx_previously_dead


if(sum(idx_errors) > 0) require_field_fix_error_file <- rbind(require_field_fix_error_file, data.frame(mort[idx_errors, ], error_name))





# check that status 'DS' or 'DC' have a dbh within 2cm of most recent census DBH  ####
error_name <- "DBH_dead_suspicious"

status_column <- rev(grep("Status", names(mort), value = T))[1]

idx_trees <- mort[, status_column] %in% c("DS", "DC")
idx_DBH_ouside_range <- !is.na(mort$'Dead DBH') & !is.na(as.numeric(mort$DBH)) & (abs(mort$'Dead DBH' - as.numeric(mort$DBH)) > 20)


idx_errors <- idx_trees & idx_DBH_ouside_range

if(sum(idx_errors) > 0) require_field_fix_error_file <- rbind(require_field_fix_error_file, data.frame(mort[idx_errors, ], error_name))

# check that newly censused 'AU', 'DS' or 'DC trees that were alive in previous census have at least one FAD is selected (OR level selected for `wounded main stem`,`canker,swelling,deformity`, `rotting main stem`) ####
error_name <- "status_AU_DS_or_DC_but_no_FAD"

status_column <- rev(grep("Status", names(mort), value = T))[1]
previous_status_column <- rev(grep("Status", names(mort), value = T))[2]

idx_trees <- mort[, status_column] %in% c("AU","DS", "DC")
idx_previously_dead <- idx_previously_dead <- grepl("D", mort[,previous_status_column]) & !is.na(mort[,previous_status_column])

idx_no_FAD <- is.na(mort$FAD)

idx_wound <- is.na(mort$'Wounded main stem')
idx_canker <- is.na(mort$'Canker; swelling, deformity')
idx_rot <- is.na(mort$'Rotting trunk')

# idx_living_crown <- mort$"Percentage of crown living" == 100 # this was for OR **status is AU and `percentage of crown living`<100**  



idx_errors <- idx_trees & (idx_no_FAD & idx_wound & idx_canker & idx_rot)  & !idx_previously_dead

# idx_errors[mort[, status_column] %in% "AU" & !idx_living_crown] <- FALSE # overwrite to FALSE for trees that are AU and crown not intact


if(sum(idx_errors) > 0) require_field_fix_error_file <- rbind(require_field_fix_error_file, data.frame(mort[idx_errors, ], error_name)) # this was for OR **status is AU and `percentage of crown living`<100**  


# check that newly censused 'AU', 'DS' or 'DC trees have at one photo taken ####
# filename <- file.path(here("testthat"), "reports/status_AU_DS_or_DC_but_no_photo.csv") # edit file name here
# 
# status_column <- rev(grep("Status", names(mort), value = T))[1]
# 
# idx_trees <- mort[, status_column] %in% c("AU","DS", "DC")
# idx_no_FAD <- is.na(mort$FAD)
# 
# idx_errors <- idx_trees & idx_no_FAD
# 
# 
# if(sum(idx_errors) > 0) {
#   write.csv(mort[idx_errors, ], file = filename, row.names = F)
# } else {
#   if(file.exists(filename) ) file.remove(filename)
# }
# 
# 

# check that newly censused 'AU', 'DS' or 'DC with "wound" selected as FAD have selected a level for wounded main stem ####
error_name <- "wounded_but_no_level"

status_column <- rev(grep("Status", names(mort), value = T))[1]

idx_trees <- mort[, status_column] %in% c("AU","DS", "DC")
idx_wounded <- !is.na(mort$FAD) & grepl("W", mort$FAD)
idx_wnd_main_stem <- !is.na(mort$'Wounded main stem')


idx_errors <- idx_trees & idx_wounded & !idx_wnd_main_stem

if(sum(idx_errors) > 0) require_field_fix_error_file <- rbind(require_field_fix_error_file, data.frame(mort[idx_errors, ], error_name))




## and vice versa ####
error_name <- "wounded_level_but_wrong_status_or_FAD"

status_column <- rev(grep("Status", names(mort), value = T))[1]

idx_trees <- mort[, status_column] %in% c("AU","DS", "DC")
idx_wounded <- !is.na(mort$FAD) & grepl("W", mort$FAD)
idx_wnd_main_stem <- !is.na(mort$'Wounded main stem')

idx_errors <- idx_trees & !idx_wounded & idx_wnd_main_stem

if(sum(idx_errors) > 0) {
  idx_errors <- (!idx_trees | !idx_wounded) & idx_wnd_main_stem


will_auto_fix_error_file <- rbind(will_auto_fix_error_file, data.frame(mort[idx_errors, ], error_name))
}



# check that newly censused 'AU', 'DS' or 'DC with "canker" selected as FAD have selected a level for canker,swelling,deformity ####
error_name <- "canker_but_no_level"

status_column <- rev(grep("Status", names(mort), value = T))[1]

idx_trees <- mort[, status_column] %in% c("AU","DS", "DC")
idx_canker <- !is.na(mort$FAD) & grepl("K", mort$FAD)
idx_ckr_level <- !is.na(mort$'Canker; swelling, deformity')


idx_errors <- idx_trees & idx_canker & !idx_ckr_level


if(sum(idx_errors) > 0) require_field_fix_error_file <- rbind(require_field_fix_error_file, data.frame(mort[idx_errors, ], error_name))




## and vice versa ####
error_name <- "canker_level_but_wrong_status_or_FAD"

status_column <- rev(grep("Status", names(mort), value = T))[1]

idx_trees <- mort[, status_column] %in% c("AU","DS", "DC")
idx_canker <- !is.na(mort$FAD) & grepl("K", mort$FAD)
idx_ckr_level <- !is.na(mort$'Canker; swelling, deformity')


idx_errors <- (!idx_trees & !idx_canker) & idx_ckr_level


if(sum(idx_errors) > 0) will_auto_fix_error_file <- rbind(will_auto_fix_error_file, data.frame(mort[idx_errors, ], error_name))




# check that newly censused 'AU', 'DS' or 'DC with "rotting stem" selected as FAD have selected a level for rotting main stem ####
error_name <- "rot_but_no_level"

status_column <- rev(grep("Status", names(mort), value = T))[1]

idx_trees <- mort[, status_column] %in% c("AU","DS", "DC")
idx_rot <- !is.na(mort$FAD) & grepl("\\<R\\>", mort$FAD)
idx_rot_level <- !is.na(mort$'Rotting trunk')


idx_errors <- idx_trees & idx_rot & !idx_rot_level


if(sum(idx_errors) > 0) require_field_fix_error_file <- rbind(require_field_fix_error_file, data.frame(mort[idx_errors, ], error_name))



## and vice versa ####
error_name <- "rot_level_but_wrong_status_or_FAD"

status_column <- rev(grep("Status", names(mort), value = T))[1]

idx_trees <- mort[, status_column] %in% c("AU","DS", "DC")
idx_rot <- !is.na(mort$FAD) & grepl("R\\>", mort$FAD)
idx_rot_level <- !is.na(mort$'Rotting trunk')


idx_errors <- (!idx_trees & !idx_rot) & idx_rot_level


if(sum(idx_errors) > 0) will_auto_fix_error_file <- rbind(will_auto_fix_error_file, data.frame(mort[idx_errors, ], error_name))




# check that there is D.shaped.exit.hole.count when EABF is DE ####
error_name <- "DE_but_no_exit_hole_count"

idx_DE <- !is.na(mort$EABF) & grepl("DE", mort$EABF)
idx_exit_count <- !is.na(mort$"D-shaped exit hole count") & mort$"D-shaped exit hole count">0


idx_errors <- idx_DE & !idx_exit_count

if(sum(idx_errors) > 0) require_field_fix_error_file <- rbind(require_field_fix_error_file, data.frame(mort[idx_errors, ], error_name))




## and vice versa ####
error_name <- "exit_hole_count_no_DE_EABF"

idx_errors <- !idx_DE & idx_exit_count

if(sum(idx_errors) > 0) require_field_fix_error_file <- rbind(require_field_fix_error_file, data.frame(mort[idx_errors, ], error_name))


# check that newly censused 'A' or 'AU', were A or AU in previous year ####
warning_name <- "Dead_but_now_alive"

status_column <- rev(grep("Status", names(mort), value = T))[1]
previous_status_column <- rev(grep("Status", names(mort), value = T))[2]


idx_trees <- mort[, status_column] %in% c("AU","A")
idx_previously_dead <- !mort[,previous_status_column] %in% c("AU","A") & !is.na(mort[,previous_status_column])


idx_errors <- idx_trees & idx_previously_dead


if(sum(idx_errors) > 0) warning_file <- rbind(warning_file, data.frame(mort[idx_errors, ], warning_name)) 


# check that newly censused 'A' or 'AU' or 'DS', were not 'DC' in previous year ####
warning_name <- "DC_but_now_A_AU_or_DS"

status_column <- rev(grep("Status", names(mort), value = T))[1]
previous_status_column <- rev(grep("Status", names(mort), value = T))[2]


idx_trees <- mort[, status_column] %in% c("AU","A", "DS")
idx_previously_dead <- mort[,previous_status_column] %in% c("DC") & !is.na(mort[,previous_status_column])


idx_errors <- idx_trees & idx_previously_dead


if(sum(idx_errors) > 0) warning_file <- rbind(warning_file, data.frame(mort[idx_errors, ], warning_name)) 

# check that newly  censused trees (FRAM, FRNI, FRPE, FRSP, or CHVI), have Crown thinning, Epicormic growth, Crown position < 10 cm DBH (for stems <10cm) all recorded ####
error_name <- "missing_EAB_info"


idx_trees <- mort$Species %in% c( "fram", "frni", "frpe", "frsp", "chvi")
idx_missing_EAB_info <- !complete.cases(mort[, c("Crown thinning", "Epicormic growth") ])
idx_missing_crwn_pos <- !complete.cases(mort[, c("Crown position < 10 cm DBH")])
idx_trees_less_10cm <-  !is.na( as.numeric(mort$DBH)) & as.numeric(mort$DBH) <100
     


status_column <- rev(grep("Status", names(mort), value = T))[1]
previous_status_column <- rev(grep("Status", names(mort), value = T))[2]

idx_status <- !mort[, status_column] %in% c("DN")
idx_previously_dead <- idx_previously_dead <- grepl("D", mort[,previous_status_column]) & !is.na(mort[,previous_status_column])


idx_errors <- ((idx_trees & idx_missing_EAB_info) |  (idx_trees & idx_missing_crwn_pos & idx_trees_less_10cm)) & (idx_status& !idx_previously_dead)


if(sum(idx_errors) > 0) require_field_fix_error_file <- rbind(require_field_fix_error_file, data.frame(mort[idx_errors, ], error_name))




# check that, for newly censused trees (FRAM, FRNI, FRPE, FRSP, or CHVI),	if Epicormic growth>0, tree is AU ####
error_name <- "epicormic_growth_but_not_AU"

status_column <- rev(grep("Status", names(mort), value = T))[1]


idx_trees <- mort$Species %in% c( "fram", "frni", "frpe", "frsp", "chvi")
idx_epicormic <- !is.na(mort$`Epicormic growth`) & mort$'Epicormic growth' > 0
idx_status <- mort[, status_column] %in% c("AU")



idx_errors <- idx_trees & idx_epicormic & !idx_status


if(sum(idx_errors) > 0) will_auto_fix_error_file <- rbind(will_auto_fix_error_file, data.frame(mort[idx_errors, ], error_name))

# check that, for newly censused trees (FRAM, FRNI, FRPE, FRSP, or CHVI),	if Crown thinning>1, tree is AU or dead ####
error_name <- "crown_thinning_more_than_1_but_not_AU_or_dead"

status_column <- rev(grep("Status", names(mort), value = T))[1]


idx_trees <- mort$Species %in% c( "fram", "frni", "frpe", "frsp", "chvi")
idx_crown <- !is.na(mort$'Crown thinning') & mort$'Crown thinning' > 1
idx_status <- mort[, status_column] %in% c("A")



idx_errors <- idx_trees & idx_crown & idx_status

if(sum(idx_errors) > 0) require_field_fix_error_file <- rbind(require_field_fix_error_file, data.frame(mort[idx_errors, ], error_name))


# check that, for newly censused trees (FRAM, FRNI, FRPE, FRSP, or CHVI),	if any EABF recorded, tree is AU or dead ####
error_name <- "EABF_recorded_but_not_AU_or_dead"

status_column <- rev(grep("Status", names(mort), value = T))[1]


idx_trees <- mort$Species %in% c( "fram", "frni", "frpe", "frsp", "chvi")
idx_EABF <- !is.na(mort$EABF) & !mort$EABF %in% "none"
idx_status <- mort[, status_column] %in% c("A")



idx_errors <- idx_trees & idx_EABF & idx_status


if(sum(idx_errors) > 0) require_field_fix_error_file <- rbind(require_field_fix_error_file, data.frame(mort[idx_errors, ], error_name))


# check that, for newly censused trees (FRAM, FRNI, FRPE, FRSP, or CHVI),	if D-shaped exit hole count>0, tree is AU or dead ####
error_name <- "exit_hole_count_but_not_AU_or_dead"

status_column <- rev(grep("Status", names(mort), value = T))[1]


idx_trees <- mort$Species %in% c( "fram", "frni", "frpe", "frsp", "chvi")
idx_exit_hole <- mort$'D-shaped exit hole count' > 0 & !is.na(mort$'D-shaped exit hole count')
idx_status <- mort[, status_column] %in% c("A")



idx_errors <- idx_trees & idx_exit_hole & idx_status


if(sum(idx_errors) > 0) require_field_fix_error_file <- rbind(require_field_fix_error_file, data.frame(mort[idx_errors, ], error_name))



# check that, for newly censused trees (FRAM, FRNI, FRPE, FRSP, or CHVI),	if tree is dead, Epicormic growth=0 ####
error_name <- "dead_but_epicormic_more_than_0"

status_column <- rev(grep("Status", names(mort), value = T))[1]
previous_status_column <- rev(grep("Status", names(mort), value = T))[2]

idx_trees <- mort$Species %in% c( "fram", "frni", "frpe", "frsp", "chvi")
idx_epicormic <- !is.na(mort$'Epicormic growth') & mort$'Epicormic growth' > 0
idx_status <- !mort[, status_column] %in% c("A", "AU")
idx_previously_dead <- idx_previously_dead <- grepl("D", mort[,previous_status_column]) & !is.na(mort[,previous_status_column])


idx_errors <- idx_trees & idx_epicormic & (idx_status & !idx_previously_dead)



if(sum(idx_errors) > 0) require_field_fix_error_file <- rbind(require_field_fix_error_file, data.frame(mort[idx_errors, ], error_name))


# check that, for newly censused trees (FRAM, FRNI, FRPE, FRSP, or CHVI),	if tree is dead, Crown thinning=5 ####
error_name <- "dead_but_crown_thinning_less_than_5"

status_column <- rev(grep("Status", names(mort), value = T))[1]
previous_status_column <- rev(grep("Status", names(mort), value = T))[2]

idx_trees <- mort$Species %in% c( "fram", "frni", "frpe", "frsp", "chvi")
idx_crown <- !is.na(mort$'Crown thinning') & mort$'Crown thinning' <5
idx_status <- !mort[, status_column] %in% c("A", "AU")
idx_previously_dead <- grepl("D", mort[,previous_status_column]) & !is.na(mort[,previous_status_column])



idx_errors <- idx_trees & idx_crown & (idx_status & !idx_previously_dead)


if(sum(idx_errors) > 0) require_field_fix_error_file <- rbind(require_field_fix_error_file, data.frame(mort[idx_errors, ], error_name))



# clean and save files ####

## remove empty tags
require_field_fix_error_file <- require_field_fix_error_file[!is.na(require_field_fix_error_file$Tag),]

will_auto_fix_error_file <- will_auto_fix_error_file[!is.na(will_auto_fix_error_file$Tag),]

warning_file <- warning_file[!is.na(warning_file$Tag),]

## order by quadrat and tag
if(!is.null(require_field_fix_error_file)) {
  require_field_fix_error_file <- require_field_fix_error_file[order(as.numeric(require_field_fix_error_file$Quad), require_field_fix_error_file$Tag, require_field_fix_error_file$StemTag),]
}

if(!is.null(will_auto_fix_error_file)) {
  will_auto_fix_error_file <- will_auto_fix_error_file[order(will_auto_fix_error_file$Quad, will_auto_fix_error_file$Tag, will_auto_fix_error_file$StemTag),]
}


if(!is.null(warning_file)) warning_file <- warning_file[order(warning_file$Quad, warning_file$Tag, warning_file$StemTag),]


# if errors/warnings exist save, else delete

csv_mort_filename <-  gsub("xlsx", "csv", gsub("FFF_excel/", "", latest_FFFs))

if(!is.null(require_field_fix_error_file)) {
  if(nrow(require_field_fix_error_file>0)) {
    
    write.csv(
      require_field_fix_error_file[, c(ncol(require_field_fix_error_file), 1:(ncol(require_field_fix_error_file) -1))], 
      file = file.path(here("testthat"), "reports/requires_field_fix/require_field_fix_error_file.csv"), 
      row.names = F
    )
    
    # if  error, delete any existing CSV mort file
    if(file.exists(csv_mort_filename)) file.remove(csv_mort_filename)
    
    # also make quad a factor so other things work
    require_field_fix_error_file$Quad <- factor(require_field_fix_error_file$Quad,  levels = sort(unique(mort$Quad)))
  }  
} else {
  if(file.exists(file.path(here("testthat"), "reports/requires_field_fix/require_field_fix_error_file.csv"))) file.remove(file.path(here("testthat"), "reports/requires_field_fix/require_field_fix_error_file.csv"))
  
  # if not error, save the current mort file as CSV
  write.csv(mort, file  = csv_mort_filename, row.names = F)
  
}



if(!is.null(will_auto_fix_error_file) ) {
  if(nrow(will_auto_fix_error_file) > 0) {
    write.csv(
      will_auto_fix_error_file[, c(ncol(will_auto_fix_error_file), 1:(ncol(will_auto_fix_error_file) -1))], 
      file = file.path(here("testthat"), "reports/will_auto_fix/will_auto_fix_error_file.csv"), 
      row.names = F
    )
    
    # also make quad a factor so other things work
    will_auto_fix_error_file$Quad <- factor(will_auto_fix_error_file$Quad,  levels = sort(unique(mort$Quad)))
  }
} else {
  if(file.exists(file.path(here("testthat"), "reports/will_auto_fix/will_auto_fix_error_file.csv"))) file.remove(file.path(here("testthat"), "reports/will_auto_fix/will_auto_fix_error_file.csv"))
}


if(!is.null(warning_file)) {
   if (nrow(warning_file)>0) {
  write.csv(
    warning_file[, c(ncol(warning_file), 1:(ncol(warning_file) -1))], 
    file = file.path(here("testthat"), "reports/warnings/warnings_file.csv"), 
    row.names = F
  )
  
     # also make quad a factor so other things work
     warning_file$Quad <- factor(warning_file$Quad,  levels = sort(unique(mort$Quad)))
} else {
  if(file.exists(file.path(here("testthat"), "reports/warnings/warnings_file.csv"))) file.remove(file.path(here("testthat"), "reports/warnings/warnings_file.csv"))
  
}
}


# KEEP TRACK OF ALL THE ISSUES ####

all_reports <- list.files(here("testthat/reports/", c("requires_field_fix", "will_auto_fix", "warnings")), recursive = T, pattern = ".csv", full.names = T)


for(f in all_reports) {
  
  new_f <- gsub("/reports/", "/reports/trace_of_reports/", f)
  new_f <- gsub("/requires_field_fix/|/will_auto_fix/|/warnings/", "",new_f)
  
  if(file.exists(new_f)) write.csv(unique(rbind(read.csv(new_f), read.csv(f))), file = new_f, row.names = F)
  else write.csv(read.csv(f), file = new_f, row.names = F)
  
}

# generate a file with summary for each quadrat ####
# quadrat_censused_missing_stems <- read.csv(file.path(here("testthat"), "reports/requires_field_fix/quadrat_censused_missing_stems.csv"))
quadrat_censused_duplicated_stems <- read.csv(file.path(here("testthat"), "reports/will_auto_fix/quadrat_censused_duplicated_stems.csv"))

quad_with_any_issue <- sort(unique(c(as.character(require_field_fix_error_file$Quad), 
                                     as.character(will_auto_fix_error_file$Quad), 
                                     as.character(warning_file$Quad), 
                                     as.character(quadrat_censused_duplicated_stems$quadrat))))

quad_summary <- data.frame(Quad = quad_with_any_issue, 
                           n_tag_error_field_fix = c(table(require_field_fix_error_file$Quad[!require_field_fix_error_file$error_name %in% "missing_stem"]))[as.character(quad_with_any_issue)], 
                           n_tag_error_auto_fix = c(table(will_auto_fix_error_file$Quad))[as.character(quad_with_any_issue)],
                           n_tag_warnings = c(table(warning_file$Quad))[as.character(quad_with_any_issue)],
                           n_missing_tags = c(table(require_field_fix_error_file$Quad[require_field_fix_error_file$error_name %in% "missing_stem"]))[as.character(quad_with_any_issue)],
                           n_duplicated_tags = c(table(quadrat_censused_duplicated_stems$Quad))[as.character(quad_with_any_issue)])

quad_summary$sum_missing_and_errors <- quad_summary$n_missing_tags + quad_summary$n_tag_error_field_fix

write.csv(quad_summary[order(as.numeric(quad_summary$Quad)), ], file.path(here("testthat"), "reports/quadrat_n_errors_summary.csv"), row.names = F)

