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


# get the list of all living trees in each mort census to make sure they were all censused the next year ####
## for checking 2014 we need to look at scbi.stem2
                                    
stems_to_sample <- list("2013" = paste(scbi.stem1$tag, scbi.stem1$StemTag)[scbi.stem1$dbh >= 50 & scbi.stem1$status %in% "A"])


for(survey_year in survey_years) {
  mort <- get(paste0("mort", survey_year))
  stems_to_sample[[as.character(survey_year)]] <- paste(mort$tag, mort$StemTag)[(grepl("^fr..|^ch..", mort$sp) | (!is.na(mort$last_main_census_dbh) & mort$last_main_census_dbh >= 100)) & grepl("A", mort$current_year_status)]
}


# run through each mortality census

# prepare log files #####
require_field_fix_error_file <- NULL
will_auto_fix_error_file <- NULL

morts <- lapply(survey_years, function(survey_year){
  mort <- get(paste0("mort", survey_year))

#keep only the ones that died
  mort <- mort[grepl("A", mort$previous_year_status) & grepl("D", mort$current_year_status),]
return(mort)
})

names(morts) <- survey_years

data.frame(n_stem = sapply(morts, nrow),
           n_missing_stems = sapply(morts, function(mort) sum(!stems_to_sample[[as.character(survey_year-1)]] %in% paste(mort$tag, mort$StemTag))),
           n_sp_code_error =sapply(morts, function(mort) sum(!mort$sp %in% scbi.spptable$sp)),
           n_missing_crwn_pos = sapply(morts, function(mort) sum(c(c(is.na(mort$crown_position) & is.na(mort$crown_position_if_dead) & is.na(mort$crown_illumination)))[mort$current_year_status %in% c("A", "AU", "DS")])),
           n_missing_crwn_intact = sapply(morts, function(mort) sum(is.na(mort$score_crown_intact)[mort$current_year_status %in% c("A", "AU", "DS")])),
           n_dbh_if_dead_not_measure = sapply(morts, function(mort) sum(is.na(mort$dbh_if_dead))),
           n_no_fad = sapply(morts, function(mort) sum(is.na(mort$fad) | mort$fad %in% "")))
  
  
  
  
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
  idx_epicormic <- !is.na(mort$`Epicormic growth`) & mort$`Epicormic growth` > 0
  idx_status <- mort[, status_column] %in% c("AU")
  
  
  
  idx_errors <- idx_trees & idx_epicormic & !idx_status
  
  
  if(sum(idx_errors) > 0) will_auto_fix_error_file <- rbind(will_auto_fix_error_file, data.frame(mort[idx_errors, ], error_name))
  
  # check that, for newly censused trees (FRAM, FRNI, FRPE, FRSP, or CHVI),	if Crown thinning>1, tree is AU or dead ####
  error_name <- "crown_thinning_more_than_1_but_not_AU_or_dead"
  
  status_column <- rev(grep("Status", names(mort), value = T))[1]
  
  
  idx_trees <- mort$Species %in% c( "fram", "frni", "frpe", "frsp", "chvi")
  idx_crown <- !is.na(mort$`Crown thinning`) & mort$`Crown thinning` > 1
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
  idx_exit_hole <- mort$`D-shaped exit hole count` > 0 & !is.na(mort$`D-shaped exit hole count`)
  idx_status <- mort[, status_column] %in% c("A")
  
  
  
  idx_errors <- idx_trees & idx_exit_hole & idx_status
  
  
  if(sum(idx_errors) > 0) require_field_fix_error_file <- rbind(require_field_fix_error_file, data.frame(mort[idx_errors, ], error_name))
  
  
  
  # check that, for newly censused trees (FRAM, FRNI, FRPE, FRSP, or CHVI),	if tree is dead, Epicormic growth=0 ####
  error_name <- "dead_but_epicormic_more_than_0"
  
  status_column <- rev(grep("Status", names(mort), value = T))[1]
  previous_status_column <- rev(grep("Status", names(mort), value = T))[2]
  
  idx_trees <- mort$Species %in% c( "fram", "frni", "frpe", "frsp", "chvi")
  idx_epicormic <- !is.na(mort$`Epicormic growth`) & mort$`Epicormic growth` > 0
  idx_status <- !mort[, status_column] %in% c("A", "AU")
  idx_previously_dead <- idx_previously_dead <- grepl("D", mort[,previous_status_column]) & !is.na(mort[,previous_status_column])
  
  
  idx_errors <- idx_trees & idx_epicormic & (idx_status & !idx_previously_dead)
  
  
  
  if(sum(idx_errors) > 0) require_field_fix_error_file <- rbind(require_field_fix_error_file, data.frame(mort[idx_errors, ], error_name))
  
  
  # check that, for newly censused trees (FRAM, FRNI, FRPE, FRSP, or CHVI),	if tree is dead, Crown thinning=5 ####
  error_name <- "dead_but_crown_thinning_less_than_5"
  
  status_column <- rev(grep("Status", names(mort), value = T))[1]
  previous_status_column <- rev(grep("Status", names(mort), value = T))[2]
  
  idx_trees <- mort$Species %in% c( "fram", "frni", "frpe", "frsp", "chvi")
  idx_crown <- !is.na(mort$`Crown thinning`) & mort$`Crown thinning` <5
  idx_status <- !mort[, status_column] %in% c("A", "AU")
  idx_previously_dead <- grepl("D", mort[,previous_status_column]) & !is.na(mort[,previous_status_column])
  
  
  
  idx_errors <- idx_trees & idx_crown & (idx_status & !idx_previously_dead)
  
  
  if(sum(idx_errors) > 0) require_field_fix_error_file <- rbind(require_field_fix_error_file, data.frame(mort[idx_errors, ], error_name))
  
  