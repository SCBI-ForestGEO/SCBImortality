# Generate reports for mortality census prior to 2021

## ***** this script is sourced in script 1 *****


# load and clean up the 3rd main census ####
main_census <-  scbi.stem3

## convert dbh to numeric
main_census$dbh <- as.numeric(main_census$dbh)

## only keep trees > 10cm except for fraxinus and Chionanthus virginicus
main_census <-  main_census[grepl("^fr..|^ch..", main_census$sp) | (!is.na(main_census$dbh) & main_census$dbh >= 100), ]

## remove trees that are dead
# main_census <- main_census[!main_census$status %in% "D",]
main_census <- main_census[!grepl("DC|DN|DT", main_census$codes),] # see https://github.com/SCBI-ForestGEO/SCBImortality/issues/31#issuecomment-881702404



# run through each mortality census

# prepare log files #####
require_field_fix_error_file <- NULL
will_auto_fix_error_file <- NULL
warning_file <- NULL



for(survey_year in survey_years) {
  
  print(survey_year)
  
  
  # load  mortality data ####
  
  mort <- get(paste0("mort", survey_year))
 
  # --- PERFORM CHECKS ---- ####
  
  # for each quadrat censused, check all expected trees were censused ####
  # filename <- file.path(here("testthat"), "reports/requires_field_fix/quadrat_censused_missing_stems.csv")
  error_name = "missing_stem"
  
  
  # idx_quadrat_censused <- main_census$quadrat %in% as.numeric(mort$quadrat)
  
  
  idx_errors <- NA # !paste(main_census$tag, main_census$StemTag) %in% paste(mort$tag, mort$stemtag) & main_census$quadrat %in% as.numeric(mort$quadrat)
  
  require_field_fix_error_file <- rbind(require_field_fix_error_file, data.frame(error_name, n = sum(idx_errors), survey_year))
  
  
  
  # remove any tree with current status DN as we don't need to check errors on those ####
  status_column <- rev(grep("status", names(mort), value = T))[1]
  
  idx_trees <- !mort[, status_column] %in% c("DN")
  
  mort <- mort[idx_trees, ]
  
  # only keep trees that died ####
  
  trees_that_died <- grepl("D", mort$current_year_status) & grepl("A", mort$previous_year_status)
  
  
  # check if all species exist in species table, if not save a file, if yes, delete that file ####
  error_name <- "species_code_error"
  
  idx_errors <- !mort$sp %in% spptable$sp
  
  require_field_fix_error_file <- rbind(require_field_fix_error_file, data.frame(error_name, n = sum(idx_errors), survey_year))
  
  
  
  
  # for each quadrat censused, check that there is no duplicated stems ####
  error_name = "duplicated_stems"
  
  idx_errors <- duplicated(paste(mort$tag, mort$stemtag))
  
  
  require_field_fix_error_file <- rbind(require_field_fix_error_file, data.frame(error_name, n = sum(idx_errors), survey_year))
  
  
  
  
  
  
  
  
  # check that all censused trees have a crown position recorded ####
  error_name <- "missing_crown_position"
  
  
  status_column <- rev(grep("status", names(mort), value = T))[1]
  
  idx_trees <- mort[, status_column] %in% c("A", "AU", "DS")
  
  idx_errors <- NA #( is.na(mort$'crown.position')|mort$'crown.position'=="") & idx_trees
  
  
  require_field_fix_error_file <- rbind(require_field_fix_error_file, data.frame(error_name, n = sum(idx_errors), survey_year))
  
  
  
  
  # check that all censused trees have a percent of crown intact recorded ####
  error_name <- "missing_percent_crown_intact"
  
  
  status_column <- rev(grep("status", names(mort), value = T))[1]
  
  idx_trees <- mort[, status_column] %in% c("A", "AU", "DS")
  
  idx_errors <- NA # is.na(mort$'perc.crown') & idx_trees
  
  
  
  require_field_fix_error_file <- rbind(require_field_fix_error_file, data.frame(error_name, n = sum(idx_errors), survey_year))
  
  
  
  # check that all censused trees have a percent of crown living recorded ####
  error_name <- "missing_percent_crown_living"
  
  
  status_column <- rev(grep("status", names(mort), value = T))[1]
  
  idx_trees <- mort[, status_column] %in% c("A", "AU", "DS")
  
  idx_errors <- NA # is.na(mort$'Percentage of crown living') & idx_trees
  
  
  require_field_fix_error_file <- rbind(require_field_fix_error_file, data.frame(error_name, n = sum(idx_errors), survey_year))
  
  
  # check percent of crown living <=  percent of crown intact####
  error_name <- "crown_living_greater_than_crown_intact"
  
  
  status_column <- rev(grep("status", names(mort), value = T))[1]
  
  idx_trees <- mort[, status_column] %in% c("A", "AU", "DS")
  
  idx_errors <- NA # !is.na(mort$'Percentage of crown living') & !is.na(mort$'perc.crown') & mort$'Percentage of crown living' > mort$'perc.crown' & idx_trees
  
  
  require_field_fix_error_file <- rbind(require_field_fix_error_file, data.frame(error_name, n = sum(idx_errors), survey_year))
  
  
  # check percent newly censused trees (DS or DC)	have percentage of crown living = 0####
  error_name <- "dead_but_crown_living_not_zero"
  
  
  status_column <- rev(grep("status", names(mort), value = T))[1]
  
  idx_trees <- mort[, status_column] %in% c("DS", "DC")
  
  idx_errors <- NA #!is.na(mort$'Percentage of crown living') & mort$'Percentage of crown living'> 0 &  idx_trees
  
  
  will_auto_fix_error_file  <- rbind(will_auto_fix_error_file, data.frame(error_name, n = sum(idx_errors), survey_year))
  
  
  # check that newly censused alive trees have no FAD selected; no record of wounded main stem, canker, or rotting trunk; DWR (dead with resprouts) not selected ####
  error_name <- "status_A_but_unhealthy"
  
  status_column <- rev(grep("status", names(mort), value = T))[1]
  
  idx_trees <- mort[, status_column] %in% "A"
  idx_FAD <- apply(mort[, grep("fad", names(mort))], 1, function(x) !all(is.na(x))) # !is.na(mort$FAD)
  # idx_wound <- !is.na(mort$'Wounded main stem')
  # idx_canker <- !is.na(mort$'Canker; swelling, deformity')
  # idx_rot <- !is.na(mort$'Rotting trunk')
  # idx_DWR <- !mort$'DWR' %in% "False"
  
  
  idx_errors <- idx_trees & (idx_FAD) #  | idx_wound | idx_wound | idx_canker | idx_rot | idx_DWR)
  
  
  require_field_fix_error_file <- rbind(require_field_fix_error_file, data.frame(error_name, n = sum(idx_errors), survey_year))
  
  
  
  ## and vice-versa ####
  error_name <- "unhealthy_but_wrong_status"
  
  status_column <- rev(grep("status", names(mort), value = T))[1]
  
  idx_trees <- mort[, status_column] %in% c("AU", "DC", "DS")
  idx_FAD <- apply(mort[, grep("fad", names(mort))], 1, function(x) !all(is.na(x))) # !is.na(mort$FAD)
  # idx_wound <- !is.na(mort$'Wounded main stem')
  # idx_canker <- !is.na(mort$'Canker; swelling, deformity')
  # idx_rot <- !is.na(mort$'Rotting trunk')
  # idx_DWR <- !mort$'DWR' %in% "False"
  
  
  idx_errors <- idx_trees & (!idx_FAD) #  | idx_wound | idx_wound | idx_canker | idx_rot | idx_DWR)
  
  will_auto_fix_error_file  <- rbind(will_auto_fix_error_file, data.frame(error_name, n = sum(idx_errors), survey_year))
  
  
  
  
  
  
  # check that status 'AU' does not have 	DWR (dead with resprouts)  selected ####
  error_name <- "status_AU_but_DWR_selected"
  
  status_column <- rev(grep("status", names(mort), value = T))[1]
  
  idx_trees <- mort[, status_column] %in% "AU"
  # idx_DWR <- !mort$'DWR' %in% "False"
  
  
  idx_errors <- NA # idx_trees & idx_DWR
  
  require_field_fix_error_file <- rbind(require_field_fix_error_file, data.frame(error_name, n = sum(idx_errors), survey_year))
  
  
  # check that status 'DS' or 'DC' have a dbh measured  ####
  error_name <- "status_DS_or_DC_but_DBH_not_measured"
  
  status_column <- rev(grep("status", names(mort), value = T))[1]
  previous_status_column <- rev(grep("status", names(mort), value = T))[2]
  
  idx_trees <- mort[, status_column] %in% c("DS", "DC")
  idx_previously_dead <- !mort[,previous_status_column] %in% c("AU","A") & !is.na(mort[,previous_status_column])
  idx_no_DBH_if_dead <- is.na(mort$"dbh.if.dead" )
  
  
  idx_errors <- idx_trees & idx_no_DBH_if_dead & !idx_previously_dead
  
  
  require_field_fix_error_file <- rbind(require_field_fix_error_file, data.frame(error_name, n = sum(idx_errors), survey_year))
  
  
  
  
  
  # check that status 'DS' or 'DC' have a dbh within 2cm of most recent census DBH  ####
  warning_name <- "DBH_dead_suspicious"
  
  status_column <- rev(grep("status", names(mort), value = T))[1]
  
  idx_trees <- mort[, status_column] %in% c("DS", "DC")
  idx_DBH_ouside_range <- !is.na(mort$"dbh.if.dead") & !is.na(as.numeric(mort[,rev(grep("dbh\\.\\d", names(mort), value = T))[1]])) & (abs(mort$"dbh.if.dead" - as.numeric(mort[,rev(grep("dbh\\.\\d", names(mort), value = T))[1]])) > 20)
  
  
  idx_errors <- idx_trees & idx_DBH_ouside_range
  
  warning_file <- rbind(warning_file,data.frame(warning_name, n = sum(idx_errors), survey_year))
  
  
  
  # check that newly censused 'AU', 'DS' or 'DC trees that were alive in previous census have at least one FAD is selected (OR level selected for `wounded main stem`,`canker,swelling,deformity`, `rotting main stem`) ####
  error_name <- "status_AU_DS_or_DC_but_no_FAD"
  
  status_column <- rev(grep("status", names(mort), value = T))[1]
  previous_status_column <- rev(grep("status", names(mort), value = T))[2]
  
  idx_trees <- mort[, status_column] %in% c("AU","DS", "DC")
  idx_previously_dead <- idx_previously_dead <- grepl("D", mort[,previous_status_column]) & !is.na(mort[,previous_status_column])
  
  idx_FAD <- apply(mort[, grep("fad", names(mort))], 1, function(x) all(is.na(x))) # !is.na(mort$FAD)
  
  # idx_wound <- is.na(mort$'Wounded main stem')
  # idx_canker <- is.na(mort$'Canker; swelling, deformity')
  # idx_rot <- is.na(mort$'Rotting trunk')
  
  # idx_living_crown <- mort$"Percentage of crown living" == 100 # this was for OR **status is AU and `percentage of crown living`<100**  
  
  
  
  idx_errors <- idx_trees & (idx_FAD)  & !idx_previously_dead # & idx_wound & idx_canker & idx_rot)  & !idx_previously_dead
  
  require_field_fix_error_file <- rbind(require_field_fix_error_file, data.frame(error_name, n = sum(idx_errors), survey_year))
  
  
  
  # check that newly censused 'AU', 'DS' or 'DC with "wound" selected as FAD have selected a level for wounded main stem ####
  error_name <- "wounded_but_no_level"
  
  status_column <- rev(grep("status", names(mort), value = T))[1]
  
  idx_trees <- mort[, status_column] %in% c("AU","DS", "DC")
  # idx_wounded <- !is.na(mort$FAD) & grepl("W", mort$FAD)
  # idx_wnd_main_stem <- !is.na(mort$'Wounded main stem')
  # 
  
  idx_errors <- NA # idx_trees & idx_wounded & !idx_wnd_main_stem
  
  require_field_fix_error_file <- rbind(require_field_fix_error_file, data.frame(error_name, n = sum(idx_errors), survey_year))
  
  
  
  
  ## and vice versa ####
  error_name <- "wounded_level_but_wrong_status_or_FAD"
  
  status_column <- rev(grep("status", names(mort), value = T))[1]
  
  idx_trees <- mort[, status_column] %in% c("AU","DS", "DC")
  # idx_wounded <- !is.na(mort$FAD) & grepl("W", mort$FAD)
  # idx_wnd_main_stem <- !is.na(mort$'Wounded main stem')
  
  idx_errors <- NA 
  
  will_auto_fix_error_file <- rbind(will_auto_fix_error_file, data.frame(error_name, n = sum(idx_errors), survey_year))
  
  
  
  # check that newly censused 'AU', 'DS' or 'DC with "canker" selected as FAD have selected a level for canker,swelling,deformity ####
  error_name <- "canker_but_no_level"
  
  status_column <- rev(grep("status", names(mort), value = T))[1]
  
  idx_trees <- mort[, status_column] %in% c("AU","DS", "DC")
  # idx_canker <- !is.na(mort$FAD) & grepl("K", mort$FAD)
  # idx_ckr_level <- !is.na(mort$'canker,swelling,deformity')
  
  
  
  idx_errors <- NA 
  
  require_field_fix_error_file <- rbind(require_field_fix_error_file, data.frame(error_name, n = sum(idx_errors), survey_year))
  
  
  ## and vice versa ####
  error_name <- "canker_level_but_wrong_status_or_FAD"
  
  status_column <- rev(grep("status", names(mort), value = T))[1]
  
  idx_trees <- mort[, status_column] %in% c("AU","DS", "DC")
  # idx_canker <- !is.na(mort$FAD) & grepl("K", mort$FAD)
  # idx_ckr_level <- !is.na(mort$'canker,swelling,deformity')
  
  idx_errors <- NA 
  
  will_auto_fix_error_file <- rbind(will_auto_fix_error_file, data.frame(error_name, n = sum(idx_errors), survey_year))
  
  
  
  
  # check that newly censused 'AU', 'DS' or 'DC with "rotting stem" selected as FAD have selected a level for rotting main stem ####
  error_name <- "rot_but_no_level"
  
  status_column <- rev(grep("status", names(mort), value = T))[1]
  
  idx_trees <- mort[, status_column] %in% c("AU","DS", "DC")
  # idx_rot <- !is.na(mort$FAD) & grepl("R\\>", mort$FAD)
  # idx_rot_level <- !is.na(mort$'rotting main stem')
  
  
  
  idx_errors <- NA 
  
  require_field_fix_error_file <- rbind(require_field_fix_error_file, data.frame(error_name, n = sum(idx_errors), survey_year))
  
  
  ## and vice versa ####
  error_name <- "rot_level_but_wrong_status_or_FAD"
  
  status_column <- rev(grep("status", names(mort), value = T))[1]
  
  idx_trees <- mort[, status_column] %in% c("AU","DS", "DC")
  # idx_rot <- !is.na(mort$FAD) & grepl("R\\>", mort$FAD)
  # idx_rot_level <- !is.na(mort$'rotting main stem')
  
  idx_errors <- NA 
  
  will_auto_fix_error_file <- rbind(will_auto_fix_error_file, data.frame(error_name, n = sum(idx_errors), survey_year))
  
  
  
  # check that there is D.shaped.exit.hole.count when EABF is DE ####
  error_name <- "DE_but_no_exit_hole_count"
  
  idx_DE <- !is.na(mort$EABF) & grepl("DE", mort$EABF)
  # idx_exit_count <- !is.na(mort$"D-shaped exit hole count") & mort$"D-shaped exit hole count">0
  
  
  idx_errors <- NA 
  
  require_field_fix_error_file <- rbind(require_field_fix_error_file, data.frame(error_name, n = sum(idx_errors), survey_year))
  
  
  ## and vice versa ####
  error_name <- "exit_hole_count_no_DE_EABF"
  
  
  idx_errors <- NA 
  
  require_field_fix_error_file <- rbind(require_field_fix_error_file, data.frame(error_name, n = sum(idx_errors), survey_year))
  
  
  
  # check that newly censused 'A' or 'AU', were A or AU in previous year ####
  warning_name <- "Dead_but_now_alive"
  
  status_column <- rev(grep("status", names(mort), value = T))[1]
  previous_status_column <- rev(grep("status", names(mort), value = T))[2]
  
  
  idx_trees <- mort[, status_column] %in% c("AU","A")
  idx_previously_dead <- !mort[,previous_status_column] %in% c("AU","A") & !is.na(mort[,previous_status_column])
  
  
  idx_errors <- idx_trees & idx_previously_dead
  
  
  warning_file <- rbind(warning_file,data.frame(warning_name, n = sum(idx_errors), survey_year))
  
  
  # check that newly censused 'A' or 'AU' or 'DS', were not 'DC' in previous year ####
  warning_name <- "DC_but_now_A_AU_or_DS"
  
  status_column <- rev(grep("status", names(mort), value = T))[1]
  previous_status_column <- rev(grep("status", names(mort), value = T))[2]
  
  
  idx_trees <- mort[, status_column] %in% c("AU","A", "DS")
  idx_previously_dead <- mort[,previous_status_column] %in% c("DC") & !is.na(mort[,previous_status_column])
  
  
  idx_errors <- idx_trees & idx_previously_dead
  
  
  
  warning_file <- rbind(warning_file,data.frame(warning_name, n = sum(idx_errors), survey_year))
  
  # check that newly  censused trees (FRAM, FRNI, FRPE, FRSP, or CHVI), have Crown thinning, Epicormic growth, Crown position < 10 cm DBH (for stems <10cm) all recorded ####
  error_name <- "missing_EAB_info"
  
  
  idx_trees <- mort$sp %in% c( "fram", "frni", "frpe", "frsp", "chvi")
  idx_missing_EAB_info <- !complete.cases(mort[, c("fraxinus.crown.thinning" , "fraxinus.epicormic.growth") ])
  idx_missing_crwn_pos <- !complete.cases(mort[, c("crown.position"  )])
  idx_trees_less_10cm <-  !is.na( as.numeric(mort[,rev(grep("dbh\\.\\d", names(mort), value = T))[1]])) & as.numeric(mort[,rev(grep("dbh\\.\\d", names(mort), value = T))[1]]) <100
  
  
  
  status_column <- rev(grep("status", names(mort), value = T))[1]
  previous_status_column <- rev(grep("status", names(mort), value = T))[2]
  
  idx_status <- !mort[, status_column] %in% c("DN")
  idx_previously_dead <- idx_previously_dead <- grepl("D", mort[,previous_status_column]) & !is.na(mort[,previous_status_column])
  
  
  idx_errors <- ((idx_trees & idx_missing_EAB_info) |  (idx_trees & idx_missing_crwn_pos & idx_trees_less_10cm)) & (idx_status& !idx_previously_dead)
  
  if(survey_year %in% c(2014, 2015))    idx_errors <- NA
  
  
  require_field_fix_error_file <- rbind(require_field_fix_error_file, data.frame(error_name, n = sum(idx_errors), survey_year))
  
  
  # check that, for newly censused trees (FRAM, FRNI, FRPE, FRSP, or CHVI),	if Epicormic growth>0, tree is AU ####
  error_name <- "epicormic_growth_but_not_AU"
  
  status_column <- rev(grep("status", names(mort), value = T))[1]
  
  
  idx_trees <- mort$sp %in% c( "fram", "frni", "frpe", "frsp", "chvi")
  idx_epicormic <- !is.na(mort$"fraxinus.epicormic.growth") & mort$"fraxinus.epicormic.growth" > 0
  idx_status <- mort[, status_column] %in% c("AU")
  
  
  
  idx_errors <- idx_trees & idx_epicormic & !idx_status
  
  
  will_auto_fix_error_file <- rbind(will_auto_fix_error_file, data.frame(error_name, n = sum(idx_errors), survey_year))
  
  
  # check that, for newly censused trees (FRAM, FRNI, FRPE, FRSP, or CHVI),	if Crown thinning>1, tree is AU or dead ####
  error_name <- "crown_thinning_more_than_1_but_not_AU_or_dead"
  
  status_column <- rev(grep("status", names(mort), value = T))[1]
  
  
  idx_trees <- mort$sp %in% c( "fram", "frni", "frpe", "frsp", "chvi")
  idx_crown <- !is.na(mort$"fraxinus.crown.thinning") & mort$"fraxinus.crown.thinning" > 1
  idx_status <- mort[, status_column] %in% c("A")
  
  
  
  idx_errors <- idx_trees & idx_crown & idx_status
  if(survey_year %in% c(2014, 2015))    idx_errors <- NA
  
  
  require_field_fix_error_file <- rbind(require_field_fix_error_file, data.frame(error_name, n = sum(idx_errors), survey_year))
  
  
  # check that, for newly censused trees (FRAM, FRNI, FRPE, FRSP, or CHVI),	if any EABF recorded, tree is AU or dead ####
  error_name <- "EABF_recorded_but_not_AU_or_dead"
  
  status_column <- rev(grep("status", names(mort), value = T))[1]
  
  
  idx_trees <- mort$sp %in% c( "fram", "frni", "frpe", "frsp", "chvi")
  idx_EABF <- !is.na(mort$EABF) & !mort$EABF %in% "none"
  idx_status <- mort[, status_column] %in% c("A")
  
  
  
  idx_errors <- NA #idx_trees & idx_EABF & idx_status
  
  
  require_field_fix_error_file <- rbind(require_field_fix_error_file, data.frame(error_name, n = sum(idx_errors), survey_year))
  
  
  
  # check that, for newly censused trees (FRAM, FRNI, FRPE, FRSP, or CHVI),	if D-shaped exit hole count>0, tree is AU or dead ####
  error_name <- "exit_hole_count_but_not_AU_or_dead"
  
  status_column <- rev(grep("status", names(mort), value = T))[1]
  
  
  idx_trees <- mort$sp %in% c( "fram", "frni", "frpe", "frsp", "chvi")
  idx_exit_hole <- mort$DE.count > 0 & !is.na(mort$DE.count)
  idx_status <- mort[, status_column] %in% c("A")
  
  
  
  idx_errors <- NA # idx_trees & idx_exit_hole & idx_status
  
  
  require_field_fix_error_file <- rbind(require_field_fix_error_file, data.frame(error_name, n = sum(idx_errors), survey_year))
  
  
  
  # check that, for newly censused trees (FRAM, FRNI, FRPE, FRSP, or CHVI),	if tree is dead, Epicormic growth=0 ####
  error_name <- "dead_but_epicormic_more_than_0"
  
  status_column <- rev(grep("status", names(mort), value = T))[1]
  previous_status_column <- rev(grep("status", names(mort), value = T))[2]
  
  idx_trees <- mort$sp %in% c( "fram", "frni", "frpe", "frsp", "chvi")
  idx_epicormic <- !is.na(mort$fraxinus.epicormic.growth) & mort$fraxinus.epicormic.growth > 0
  idx_status <- !mort[, status_column] %in% c("A", "AU")
  idx_previously_dead <- grepl("D", mort[,previous_status_column]) & !is.na(mort[,previous_status_column])
  
  
  idx_errors <- idx_trees & idx_epicormic & (idx_status & !idx_previously_dead)
  
  
  
  require_field_fix_error_file <- rbind(require_field_fix_error_file, data.frame(error_name, n = sum(idx_errors), survey_year))
  
  
  
  # check that, for newly censused trees (FRAM, FRNI, FRPE, FRSP, or CHVI),	if tree is dead, Crown thinning=5 ####
  error_name <- "dead_but_crown_thinning_less_than_5"
  
  status_column <- rev(grep("status", names(mort), value = T))[1]
  previous_status_column <- rev(grep("status", names(mort), value = T))[2]
  
  idx_trees <- mort$sp %in% c( "fram", "frni", "frpe", "frsp", "chvi")
  idx_crown <- !is.na(mort$"fraxinus.crown.thinning") & mort$"fraxinus.crown.thinning" <5
  idx_status <- !mort[, status_column] %in% c("A", "AU")
  idx_previously_dead <- grepl("D", mort[,previous_status_column]) & !is.na(mort[,previous_status_column])
  
  
  
  idx_errors <- idx_trees & idx_crown & (idx_status & !idx_previously_dead)
  
  require_field_fix_error_file <- rbind(require_field_fix_error_file, data.frame(error_name, n = sum(idx_errors), survey_year))
  
  
  
} # for(survey_year in mort.census.years) {

require_field_fix_error_file

require_field_fix_error_file$error_rate <- round(require_field_fix_error_file$n *100 / sapply(mort.census.years, function(survey_year) nrow( get(paste0("mort", substr(survey_year, 3, 4)))))[match(require_field_fix_error_file$survey_year, mort.census.years)],2)

will_auto_fix_error_file$error_rate <- round(will_auto_fix_error_file$n *100 / sapply(mort.census.years, function(survey_year) nrow( get(paste0("mort", substr(survey_year, 3, 4)))))[match(will_auto_fix_error_file$survey_year, mort.census.years)],2)

warning_file$error_rate <- round(warning_file$n *100 / sapply(mort.census.years, function(survey_year) nrow( get(paste0("mort", substr(survey_year, 3, 4)))))[match(warning_file$survey_year, mort.census.years)],2)




field_fix_errors <- unstack(require_field_fix_error_file, n ~ survey_year)
rownames(field_fix_errors) <-require_field_fix_error_file$error_name[require_field_fix_error_file$survey_year %in% 2014]

write.csv(t(field_fix_errors), file = "R_results/field_fix_errors_priorCI.csv")

round(colSums(field_fix_errors, na.rm = T) *100 / sapply(mort.census.years, function(survey_year) nrow( get(paste0("mort", substr(survey_year, 3, 4))))), 2)

field_fix_errors_rate <- unstack(require_field_fix_error_file, error_rate ~ survey_year)
rownames(field_fix_errors_rate) <-require_field_fix_error_file$error_name[require_field_fix_error_file$survey_year %in% 2014]


auto_fix_errors <- unstack(will_auto_fix_error_file, n ~ survey_year)
rownames(auto_fix_errors) <-will_auto_fix_error_file$error_name[will_auto_fix_error_file$survey_year %in% 2014]

write.csv(t(auto_fix_errors), file = "R_results/auto_fix_errors_priorCI.csv")

auto_fix_error_rate <- unstack(will_auto_fix_error_file, error_rate ~ survey_year)
rownames(auto_fix_error_rate) <-will_auto_fix_error_file$error_name[will_auto_fix_error_file$survey_year %in% 2014]


warnings <- unstack(warning_file, n ~ survey_year)
rownames(warnings) <-warning_file$warning_name[warning_file$survey_year %in% 2014]

write.csv(t(warnings), file = "R_results/warnings_priorCI.csv")

warnings_rate <- unstack(warning_file, error_rate ~ survey_year)
rownames(warnings_rate) <-warning_file$error_name[warning_file$survey_year %in% 2014]


round(colMeans(field_fix_errors_rate, na.rm = T),2)


summary(mort14)

