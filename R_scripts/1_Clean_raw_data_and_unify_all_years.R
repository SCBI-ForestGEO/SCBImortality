#############################################################################
# Purpose: clean raw mortality census data and unitfy all years to standardized format. Note: Continuous integration and Fast Field forms were implemented/used in 2021
# Developped by: Valentine Herrmann - HerrmannV@si.edu
# R version 4.0.3 (2020-10-10)
##########################################################################

# Clean environment ####
rm(list = ls())

# Set working directory ####
setwd(".")

# Load libraries ####

## to calculate allometries
library(allodb) # remotes::install_github("forestgeo/allodb")


# Load data ####


## bring in main ForestGEO census data ####

for(f in paste0("scbi.stem", 1:3)) {
  print(f)
  x <-  read.csv(paste0("https://raw.githubusercontent.com/SCBI-ForestGEO/SCBI-ForestGEO-Data/master/tree_main_census/data/census-csv-files/", f, ".csv"))
  x$quadrat <-ifelse(nchar(x$quadrat) < 4, paste0("0",   x$quadrat),   x$quadrat)
  x$dbh <- as.numeric(x$dbh) # not numeric because of the "NULL" values
  x$dbh[ x$dbh %in% 0] <- NA # replace dbh 0 by NA... only occuring in second census, not sure why...
  x$gx <- round(x$gx,1)
  x$gy <- round(x$gy,1)
  x$ExactDate <- as.Date(x$ExactDate, format = "%m/%d/%Y")
  assign(f,x)
}


### get the species table ####
f = "scbi.spptable"
assign(f, read.csv(paste0("https://raw.githubusercontent.com/SCBI-ForestGEO/SCBI-ForestGEO-Data/master/tree_main_census/data/census-csv-files/", f, ".csv")))


### bring in table that unifies the column names across years ####
unified_colnames <-  read.csv("raw_data/standardizing_colnames.csv")


### bring in table for fixes we want to do ####
manual_fixes <-  read.csv("raw_data/manual_fixes.csv")
# consistent_fixes <- read.csv("raw_data/consitent_fixes.csv") # now all implemented in manual_fixes or in this script


## bring in raw mortality data + clean up and calculate allometries ####
raw_data_path <- "raw_data/"

survey_files <- list.files(raw_data_path, pattern = "Mortality_Survey_.*csv")
# survey_files <- survey_files[as.numeric(regmatches(survey_files, regexpr("20\\d\\d", survey_files))) <= 2021] # only consider files before 2021 as starting 2021 is the CI files

survey_years <- NULL


eafb_recorded_on_wrong_species <- NULL
A_afterD <- NULL

for(survey_file in survey_files) {
  
  survey_year <- as.numeric(gsub("Mortality_Survey_|\\.csv", "", survey_file))
  
  survey_years <- c(survey_years, survey_year)
  
  cat(paste("cleaning up", survey_year), "...\n")
  
  # load data ####
  mort <- read.csv(paste0(raw_data_path, survey_file), stringsAsFactors = F)
  unified_colnames_yr <- unified_colnames[unified_colnames$survey_year %in% survey_year & unified_colnames$survey_type %in% "mortality", ]
 
  
  # standardize column names ####
  colnames(mort) <- unified_colnames_yr$unified_column.name[match(colnames(mort), unified_colnames_yr$raw_column_name)]
  
  
  ## add columns missing
  mort[, setdiff(unified_colnames[unified_colnames$survey_type %in% "mortality", ]$unified_column.name, colnames(mort))] <- NA
  
  ## delete columns we don't want
  mort[, grep("delete", colnames(mort))] <- NULL
  
  
  # make manual fixes ####
  
  for(i in 1:nrow(manual_fixes)) {
    
    if(survey_year %in% eval(parse(text = manual_fixes$survey_years_to_apply_fix[i]))) {
      cat("Implementing manual fix ", manual_fixes$ID[i], ": ", manual_fixes$issue_name[i], "\n", sep = "")
      eval(parse(text = manual_fixes$fix[i]))
      # print(head(mort))
    }
   
  }
 
  
  # fill in  mort$last_main_census_dbh, last_main_cenus_status and , last_main_cenus_codes ####
  if(survey_year <= 2018) {
    ref_main <- scbi.stem2
  }
  if(survey_year > 2018 & survey_year <= 2022) {
    ref_main <- scbi.stem3
  }
  if(survey_year > 2022) stop("need to code for new main census")
  
  idx <- match(paste(mort$tag, mort$StemTag), paste(ref_main$tag, ref_main$StemTag))
  mort$last_main_census_dbh <- ref_main$dbh[idx]
  mort$last_main_cenus_status <- ref_main$status[idx]
  mort$last_main_census_codes <- ref_main$codes[idx]
   
  # standardize status ####
  
  ## remove spaces
  mort$previous_year_status <- toupper(gsub(" ", "", mort$previous_year_status))
  mort$current_year_status <- toupper(gsub(" ", "", mort$current_year_status))
  
  ## replace empty and "not sampled" by NA
  mort$previous_year_status[grepl("SAMPLED", mort$previous_year_status )| mort$previous_year_status == "" ] <- NA 
  mort$current_year_status[grepl("SAMPLED", mort$current_year_status ) | mort$current_year_status == "" ] <- NA
  
  ## make sure status is defined
  mort$previous_year_status[mort$previous_year_status %in%"DT"] <- "DN" # this if for one case in 2014, where code was wrongly entered in status for that tree
  
  mort$previous_year_status[mort$previous_year_status %in% "M"] <- NA # this are trees that were missed in 2019 and got a code "M" for the "previous_year_status" of 2022 survey
  
  if(!all(na.omit(c(mort$previous_year_status,  mort$current_year_status)) %in% c("A", "AU", "DC", "DN", "DS", "PD"))) stop("some statuses are not defined ")
  
  ## fill in previous_year_status when not there
  if(all(is.na(mort$previous_year_status))) {
    ref_mort <- get(paste0("mort", survey_year-1))
    mort$previous_year_status <- ref_mort$current_year_status[match(paste(mort$tag, mort$StemTag), paste(ref_mort$tag, ref_mort$StemTag))]
  }

  # standardize fraxinus_eabf ####
  
  # make a logical column to say if species is susceptible to emerald ash borer
  mort$sp_affected_by_eab <- grepl("fr|ch", mort$sp)
  
  idx_fr_or_chvi <-  mort$sp_affected_by_eab  # making it an object to simplify coding here 
  
  ## record year and tag of non fr or ch genus that have fraxinus_eabf info
  
  idx_issue <- !idx_fr_or_chvi & !(is.na(mort$fraxinus_eabf) | mort$fraxinus_eabf %in% c("", "none"))
  
  if(sum(idx_issue) > 0 ) eafb_recorded_on_wrong_species <- rbind(  eafb_recorded_on_wrong_species,
                                            data.frame(survey_year, mort[idx_issue,  c("sp", "tag", "fraxinus_eabf")]))
  
  ## remove spaces and change ";" into ","
  
  mort$fraxinus_eabf <- gsub(" ", "", mort$fraxinus_eabf)
  mort$fraxinus_eabf <- gsub(";", ",", mort$fraxinus_eabf)
  
  
  ## if fraxinus or ch, replace "" or NA by "none", otherwise, change "" by NA (leaving "none" or other valuesi there in case we need to change speices ** TO EDIT MAYBE **)

  mort$fraxinus_eabf[idx_fr_or_chvi] <- ifelse(is.na(mort$fraxinus_eabf[idx_fr_or_chvi])| mort$fraxinus_eabf[idx_fr_or_chvi] %in% c(""), "none", mort$fraxinus_eabf[idx_fr_or_chvi])
  
  mort$fraxinus_eabf[!idx_fr_or_chvi] <- ifelse(is.na(mort$fraxinus_eabf[!idx_fr_or_chvi])| mort$fraxinus_eabf[!idx_fr_or_chvi] %in% c(""),NA, mort$fraxinus_eabf[!idx_fr_or_chvi])
  
  ## check that what is in fraxinus_eabf is only what should be in there
  if(!any(unlist(strsplit(mort$fraxinus_eabf, ",")) %in% c("none", "VB", "SS", "AS", "W", "DE"))) stop("some fraxinus_eabf are not defined ")
 
  
  # standardize fad ####
  
  ## combine all fad into one column
  mort$fad <- apply(mort[, sort(grep("fad", colnames(mort), value = T))], 1, paste, collapse = ",")
  mort$fad <- gsub("(,NA){1,}|NA|,,|^,", "",   mort$fad )
  mort$fad <- gsub("^,|,$", "",   mort$fad )
  
  mort[grep("fad\\d", names(mort), value = T)] <- NULL
  
  ## replace "" by NA for trees with current status "A"
  mort$fad[mort$current_year_status %in% "A" &  mort$fad %in% ""] <- NA

  
  # standardize score crown intact and score crown living ####
  mort$score_crown_intact <- gsub("\\D| ", "", mort$score_crown_intact)
  mort$score_crown_intact[mort$score_crown_intact %in% c("", "0")] <- NA
  mort$score_crown_intact <- as.numeric(mort$score_crown_intact)

  mort$score_crown_living <- gsub("\\D| ", "", mort$score_crown_living)
  mort$score_crown_living[mort$score_crown_living %in% c("", "0")] <- NA
  mort$score_crown_living <- as.numeric(mort$score_crown_living)
  
  
  ## translate >= 2021 percent_of_crown_living and percent_of_crown_intact into corresponding scores
  if(all(is.na(mort$score_crown_intact))& !all(is.na(mort$percent_of_crown_intact))) mort$score_crown_intact <- cut(mort$percent_of_crown_intact, breaks = c(0, 25, 50, 75, 100), include.lowest = F, labels = F)
  if(all(is.na(mort$score_crown_living)) & !all(is.na(mort$percent_of_crown_living))) mort$score_crown_living <- cut(mort$percent_of_crown_living, breaks = c(0, 25, 50, 75, 100), include.lowest = F, labels = F)
  
  
  # padd quadrats with 0 ####
  mort$quadrat <- as.character(  mort$quadrat)
  mort$quadrat <- ifelse(nchar(mort$quadrat) < 4, paste0("0",   mort$quadrat),   mort$quadrat)
  
  
  # consider dbh as numeric ####
  mort$last_main_census_dbh[mort$last_main_census_dbh %in% c("", "NULL", "N/A")] <- NA # doing this so if a warning shoes on next line up it means there was another type of character that is not coercible to numeric and we may want to review.
  mort$last_main_census_dbh <- as.numeric( mort$last_main_census_dbh)
  
  mort$dbh_if_dead[mort$dbh_if_dead %in% c("", "NULL", "N/A")] <- NA # doing this so if a warning shoes on next line up it means there was another type of character that is not coercible to numeric and we may want to review.
  mort$dbh_if_dead <- as.numeric( mort$dbh_if_dead)
  
  # if(survey_year >= 2019) mort$dbh.2018 <- as.numeric(mort$dbh.2018) else mort$dbh.2013 <- as.numeric(mort$dbh.2013) 

  # retrieve global coordinates and recalulate local ones ####
  mort$gx <- scbi.stem3$gx[match(paste(mort$tag, mort$StemTag), paste(scbi.stem3$tag, scbi.stem3$StemTag))]
  mort$gy <- scbi.stem3$gy[match(paste(mort$tag, mort$StemTag), paste(scbi.stem3$tag, scbi.stem3$StemTag))]
  
  mort$gx[mort$gx %in% 400] <- 399.9 # can't be 400 really
  mort$gx[mort$gy %in% 640] <- 639.9 # can't be 400 really
  
  
  mort$lx <-  mort$gx - floor( mort$gx / 20)*20
  mort$ly <-  mort$gy - floor( mort$gy / 20)*20
  
  
  # consider date as Date ####
  if(survey_year <= 2019) date_format = "%m/%d/%Y"
  if(survey_year == 2020) date_format = "%m/%d/%y"
  if(survey_year > 2020) date_format = "%m-%d-%Y" 
  mort$ExactDate  <- as.Date(mort$ExactDate, format = date_format)

  # # make consistent fixes #### NOW ALL IMPLEMENTED IN MANUAL FIXES
  # for(i in 1:nrow(consistent_fixes)) {
  #   
  #   if(survey_year %in% eval(parse(text = consistent_fixes$survey_years_to_apply_fix[i]))) {
  #     cat("Implementing consistent  fix ", consistent_fixes$ID[i], ": ", consistent_fixes$issue_name[i], "\n", sep = "")
  #     eval(parse(text = consistent_fixes$fix[i]))
  #     # print(head(mort))
  #   }
  #   
  # }
  
  # find issues of alive after dead 
  idx <-  mort$current_year_status %in% c("A", "AU") & grepl("D", mort$previous_year_status)
  
  if(sum(idx) > 0)   A_afterD <- rbind(A_afterD, cbind(year = survey_year, mort[idx, c("tag", "StemTag", "sp", "previous_year_status", "current_year_status", "dead_with_resprout", "previous_year_comment", "current_year_comment")]))
    
  # save
  assign(paste0("mort", survey_year), mort)

}

warning("check date format after 2022 is correct!")

A_afterD




# check all tags exist in core census data if any problem, add in "manual_fixes.csv" ####

tag_stem_in_order <- paste(scbi.stem3$tag, scbi.stem3$StemTag)

for(survey_year in survey_years) {
  print(survey_year)
  
  mort <- get(paste0("mort", survey_year))
  tag_stems <- paste(mort$tag, mort$StemTag)
  
  if(!all(tag_stems %in% tag_stem_in_order)) {
    print("Not all tags are in core census")
    
    tag_stems[which(!tag_stems %in% tag_stem_in_order)]
    print(mort[ paste(mort$tag, mort$StemTag) %in% tag_stems[which(!tag_stems %in% tag_stem_in_order)], ])
  }
} # should all be empty (only year should show up)



# Now re-order all data to all have same rows in same order + fill in missing info ####
tag_stem_in_order <- paste(scbi.stem3$tag, scbi.stem3$StemTag)

tag_stem_in_order <- tag_stem_in_order[tag_stem_in_order %in% unique(unlist(sapply(survey_years, function(survey_year) {
  mort <- get(paste0("mort", survey_year))
  paste(mort$tag, mort$StemTag)
})))] # only keep the ones that were sampled for mortality at some point

for(survey_year in survey_years) {
  cat(paste("Filling info of missied stems in", survey_year), "...\n")
  
  mort <- get(paste0("mort", survey_year))
  tag_stems <- paste(mort$tag, mort$StemTag)
  
  m <- match(tag_stem_in_order, tag_stems)
  mort <- mort[m, ]
  
  # fill in info of trees that were not sampled
  missing_stems <- tag_stem_in_order[is.na(m)]
  
  if(survey_year <= 2018) {
    ref_main <- scbi.stem2
  }
  if(survey_year > 2018 & survey_year <= 2022) {
    ref_main <- scbi.stem3
  }
  if(survey_year > 2022) stop("need to code for new main census")
  
  idx <- match(missing_stems, paste(ref_main$tag, ref_main$StemTag))
  
  mort[is.na(m), c(
    "tag", "StemTag",
    "sp",
    "quadrat", "gx", "gy",
    "last_main_census_dbh", "hom",
    "last_main_cenus_status", "last_main_census_codes"
  )] <-
    ref_main[idx, c("tag", "StemTag",
                    "sp",
                    "quadrat", "gx", "gy",
                    "dbh", "hom",
                    "status",
                    "codes")]
  
  mort[is.na(m),]$current_year_comment <- "stem not sampled, info automatically filled from previous year info"
  mort[is.na(m),]$sp_affected_by_eab <- grepl("fr|ch", mort[is.na(m),]$sp)
  mort[is.na(m),]$lx <-  mort[is.na(m),]$gx - floor( mort[is.na(m),]$gx / 20)*20
  mort[is.na(m),]$ly <-  mort[is.na(m),]$gy - floor( mort[is.na(m),]$gy / 20)*20
  
  # add info from previous mortality census if we have it
  if(survey_year %in% 2014) {
    previous_year_status <- ref_main$status[idx]
    previous_year_comment <- NA
  }
  if(survey_year> 2014) {
    previous_year_status <- get(paste0("mort", survey_year-1))$current_year_status[is.na(m)]
    previous_year_comment <- get(paste0("mort", survey_year-1))$current_year_comment[is.na(m)]
    
  }
  
  previous_year_status[previous_year_status %in% "G"] <- "D" # stems that are "Gone" are considered dead here... don't know if they are still standing or not so just giving status "D".
  previous_year_status[previous_year_status %in% "P"] # leaving "P" for "Prior" (trees that did not exist yet)
  
  mort[is.na(m),]$previous_year_status <- ifelse(is.na( mort[is.na(m),]$previous_year_status) & !is.na(previous_year_status), previous_year_status, mort[is.na(m),]$previous_year_status)
  
  mort[is.na(m),]$previous_year_comment <- ifelse(is.na( mort[is.na(m),]$previous_year_comment) & !is.na(previous_year_comment), previous_year_comment, mort[is.na(m),]$previous_year_comment)

  
  # add date, assuming it was sampled that year (to help calculate timeint - current_status is NA anyways so it will be excluded of analysis when calculating moratlity rates)
  
 date_per_quad <- tapply(mort$ExactDate, mort$quadrat, function(x) names(sort(table(x), decreasing = T))[1])
  mort$ExactDate[is.na(m)] <- as.Date(date_per_quad[mort$quadrat[is.na(m)]])
  
  
  # save
  
  assign(paste0("mort", survey_year), mort)
  
}



# Calculate allometries ####

## on main census data

for(census in paste0("scbi.stem", 1:3)) {
  
  cat("cleaning and calculating allometries on", census, "...\n")
  
  x <- get(census)
  x$dbh <- as.numeric(x$dbh) # not numeric because of the "NULL" values
  x$genus <- scbi.spptable$Genus[match(x$sp, scbi.spptable$sp)]
  x$species <- scbi.spptable$Species[match(x$sp, scbi.spptable$sp)]
  
  
  x$agb <-
    round(get_biomass(
      dbh = x$dbh/10, # in cm
      genus = x$genus,
      species = x$species,
      coords = c(-78.2, 38.9)
    ) / 1000 ,2) #  / 1000 to change to in Mg
  
  assign(census, x)
}

## on mortality census

for(survey_year in survey_years) {
  
  mort <- get(paste0("mort", survey_year))
  
  ## calculate allometries ####
  
  cat(paste("calculating allometries for", survey_year), "...\n")
  
  if(length(setdiff(mort$sp, scbi.spptable$sp)) > 0) stop ("There is one species that is not in scbi.spptable")
  
  mort$genus <- scbi.spptable$Genus[match(mort$sp, scbi.spptable$sp)]
  mort$species <- scbi.spptable$Species[match(mort$sp, scbi.spptable$sp)]
  
  
  mort$last_main_census_agb_Mg <-
    round(get_biomass(
      dbh = as.numeric(mort$last_main_census_dbh)/10, # in cm
      genus = mort$genus,
      species = mort$species,
      coords = c(-78.2, 38.9) # SCBI coordinates
    ) / 1000 ,2) #  / 1000 to change to in Mg
  
  
  mort$agb_if_dead_Mg <-
    round(get_biomass(
      dbh = as.numeric(mort$dbh_if_dead)/10, # in cm
      genus = mort$genus,
      species = mort$species,
      coords = c(-78.2, 38.9)# SCBI coordinates
    ) / 1000 ,2) #  / 1000 to change to in Mg

  
  # save
  assign(paste0("mort", survey_year), mort)
  
}


# make two first main census data in the same format as mortality census ####

for(census in paste0("scbi.stem", 1:2)) {
  
  cat("making", census, "in same format as mortality...\n")
  survey_year <- switch(census, scbi.stem1 = 2008, scbi.stem2 = 2013)
  survey_years <- sort(c(survey_years, survey_year))
  
  mort <- get(census)
  
  # keep only tags later sampled in mortality
  mort <- mort[paste(mort$tag, mort$StemTag) %in% tag_stem_in_order, ]
  
  
  
  # standardize column names ####
  unified_colnames_yr <- unified_colnames[unified_colnames$survey_type %in% "main", ]
  
  colnames(mort) <- unified_colnames_yr$unified_column.name[match(colnames(mort), unified_colnames_yr$raw_column_name)]
  
  
  ## add columns missing
  mort[, setdiff(unified_colnames[unified_colnames$survey_type %in% "mortality", ]$unified_column.name, colnames(mort))] <- NA
  
  ## delete columns we don't want
  mort[, grep("delete", colnames(mort))] <- NULL
  
  # get previous main census status
  if(census == "scbi.stem1") mort$previous_year_status <- NA
  if(census == "scbi.stem2") mort$previous_year_status <- mort2008$current_year_status
  
  # add a couple missing column that are not in the unified column table as they were created in this script ####
  mort$agb_if_dead_Mg <- NA
  mort$sp_affected_by_eab <- grepl("fr|ch", mort$sp)

  
  assign(paste0("mort", survey_year), mort)
  
}

# calculate time interval between each date the tree was censused ####

for(survey_year in survey_years) {
  
  mort <- get(paste0("mort", survey_year))
  
  
  if(survey_year == 2008) mort$timeint_days <- NA
  if(survey_year > 2008) {
    if(survey_year == 2013) ref_mort <- mort2008    else  ref_mort <- get(paste0("mort", survey_year-1))
    mort$timeint_days <- difftime(mort$ExactDate, ref_mort$ExactDate, units = "days")
    
  }
  
  
  assign(paste0("mort", survey_year), mort)
}
  

# save all the data in the same format ####

## order of columns we want to keep

columns_to_keep <- c("survey_year", # adding this column, it will be createid in the loop that saves the mortality files
                     "tag", "StemTag", "sp", "genus", "species", 
                     "quadrat", "gy", "gx", "ly", "lx",
                     "last_main_census_dbh", "last_main_census_agb_Mg", "hom",
                     "ExactDate", "timeint_days",
                     "previous_year_status", "current_year_status", 
                     "last_main_cenus_status","last_main_census_codes",  
                     "cored",
                     
                     "crown_position", "crown_illumination", 
                     "percent_of_crown_intact", "score_crown_intact",
                     "percent_of_crown_living", "score_crown_living",
                     
                     "fad", 
                     "liana_load", "wounded_main_stem", "rotting_trunk", "canker_swelling_deformity", 
                     "lean_angle_if_greater_than_15_degrees", 
                     "dead_with_resprout", 
                     "crown_position_if_dead", 
                     "dbh_if_dead", "agb_if_dead_Mg",
                     
                     "sp_affected_by_eab",
                     "fraxinus_eabf", "fraxinus_D_shaped_exit_hole_count",
                     "fraxinus_epicormic_growth", "fraxinus_score_crown_living",
                     
                     "surveyor",
                     "current_year_comment", "previous_year_comment", "submission_id")
  




## save mortality files and build up allmort
allmort <- NULL

for (survey_year in survey_years) {
  
  print(paste("Saving final data set for", survey_year))
  
  mort <- get(paste0("mort", survey_year))
  head(mort)
  
  mort$survey_year <- survey_year
  
  mort <- mort[, columns_to_keep]

  assign(paste0("mort", survey_year), mort)
  write.csv(mort, file = paste0("data/mortality_", survey_year, ".csv"), row.names = F)
  
  allmort <- rbind(allmort, mort)
}

## save allmort
write.csv(allmort, "data/allmort.csv", row.names = F)
save(allmort, file = "data/allmort.RData")

