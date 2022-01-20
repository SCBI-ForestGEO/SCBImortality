#############################################################################
# Purpose: clean and merge all mortality census data prior to CI, which was implemented in 2021
# Developped by: Valentine Herrmann - HerrmannV@si.edu
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
  x$gx <- round(x$gx,1)
  x$gy <- round(x$gy,1)
  assign(f,x)
}


### get the species table ####
f = "scbi.spptable"
assign(f, read.csv(paste0("https://raw.githubusercontent.com/SCBI-ForestGEO/SCBI-ForestGEO-Data/master/tree_main_census/data/census-csv-files/", f, ".csv")))


### bring in table that unifies the colomn names across years ####
unified_colnames <-  read.csv("raw_data/standardizing_colnames.csv")


### bring in table for fixes we want to do ####
manual_fixes <-  read.csv("raw_data/manual_fixes.csv")
consistent_fixes <- read.csv("raw_data/consitent_fixes.csv")


## bring in raw mortality data + clean up and calculate allometries ####
raw_data_path <- "raw_data/"

survey_files <- list.files(raw_data_path, pattern = "Mortality_Survey_.*csv")
survey_files <- survey_files[as.numeric(regmatches(survey_files, regexpr("20\\d\\d", survey_files))) <= 2021] # only consider files before 2021 as starting 2021 is the CI files

survey_years <- NULL


eafb_recorded_on_wrong_species <- NULL

for(survey_file in survey_files) {
  
  survey_year <- as.numeric(gsub("Mortality_Survey_|\\.csv", "", survey_file))
  
  survey_years <- c(survey_years, survey_year)
  
  cat(paste("cleaning up", survey_year), "...\n")
  
  # load data ####
  mort <- read.csv(paste0(raw_data_path, survey_file), stringsAsFactors = F)
  unified_colnames_yr <- unified_colnames[unified_colnames$survey_year %in% survey_year, ]
 
  
  # standardize column names ####
  colnames(mort) <- unified_colnames_yr$unified_column.name[match(colnames(mort), unified_colnames_yr$raw_column_name)]
  
  
  ## add columns missing
  mort[, setdiff(unified_colnames$unified_column.name, colnames(mort))] <- NA
  
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
  
  if(!all(na.omit(c(mort$previous_year_status,  mort$current_year_status)) %in% c("A", "AU", "DC", "DN", "DS", "PD"))) stop("some statuses are not defined ")
  
  ## fill in previous_year_status when not there
  if(all(is.na(mort$previous_year_status))) {
    ref_mort <- get(paste0("mort", survey_year-1))
    mort$previous_year_status <- ref_mort$current_year_status[match(paste(mort$tag, mort$StemTag), paste(ref_mort$tag, ref_mort$StemTag))]
  }

  # standardize fraxinus_eabf ####
  
  # make a logical column to say if species is susceptivle to emerald ash borer
  mort$sp_affected_by_eab <- grepl("fr|ch", mort$sp)
  
  idx_fr_or_chvi <-  mort$sp_affected_by_eab  # making it an object to simplify coding here 
  
  ## record year and tag of non fr or ch genus that have fraxinus_eabf info
  
  idx_issue <- !idx_fr_or_chvi & !(is.na(mort$fraxinus_eabf) | mort$fraxinus_eabf %in% c(""))
  
  if(sum(idx_issue) > 0 ) eafb_recorded_on_wrong_species <- rbind(  eafb_recorded_on_wrong_species,
                                            data.frame(survey_year, mort[idx_issue,  c("sp", "tag", "fraxinus_eabf")]))
  
  ## remove spaces and change ";" into ","
  
  mort$fraxinus_eabf <- gsub(" ", "", mort$fraxinus_eabf)
  mort$fraxinus_eabf <- gsub(";", ",", mort$fraxinus_eabf)
  
  
  ## if fracinus or ch, replace "" or NA by "none", otherwise, change "" by NA (leaving "none" or other valuesi there in case we need to change speices ** TO EDIT MAYBE **)

  mort$fraxinus_eabf[idx_fr_or_chvi] <- ifelse(is.na(mort$fraxinus_eabf[idx_fr_or_chvi])| mort$fraxinus_eabf[idx_fr_or_chvi] %in% c(""), "none", mort$fraxinus_eabf[idx_fr_or_chvi])
  
  mort$fraxinus_eabf[!idx_fr_or_chvi] <- ifelse(is.na(mort$fraxinus_eabf[!idx_fr_or_chvi])| mort$fraxinus_eabf[!idx_fr_or_chvi] %in% c(""),NA, mort$fraxinus_eabf[idx_fr_or_chvi])
  
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
  
  # make consistent fixes ####
  for(i in 1:nrow(consistent_fixes)) {
    
    if(survey_year %in% eval(parse(text = consistent_fixes$survey_years_to_apply_fix[i]))) {
      cat("Implementing consistent  fix ", consistent_fixes$ID[i], ": ", consistent_fixes$issue_name[i], "\n", sep = "")
      eval(parse(text = consistent_fixes$fix[i]))
      # print(head(mort))
    }
    
  }
    
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
  
  cat(paste("calculating alomertries for", survey_year), "...\n")
  
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
  

# --------------continue revamping here -----------####


# check all tags exist in core census data if any problem, add in "manual_fixes.csv" ####

tag_stem_in_order <- paste(scbi.stem3$tag, scbi.stem3$StemTag, sep = "_")

for(survey_year in survey_years) {
  print(survey_year)
  
  mort <- get(paste0("mort", survey_year))
  tag_stems <- paste(mort$tag, mort$StemTag, sep = "_")
  
  if(!all(tag_stems %in% tag_stem_in_order)) {
    print("Not all tags are in core census")
    
    tag_stems[which(!tag_stems %in% tag_stem_in_order)]
    print(mort[ paste(mort$tag, mort$StemTag, sep = "_") %in% tag_stems[which(!tag_stems %in% tag_stem_in_order)], ])
  }
} # should all be empty (only year should show up)



# # Now re-order all data to all have same rows ####
# tag_stem_in_order <- paste(scbi.stem3$tag, scbi.stem3$StemTag, sep = "_")
# 
# tag_stem_in_order <- tag_stem_in_order[tag_stem_in_order %in% unique(unlist(sapply(survey_years, function(survey_year) {
#   mort <- get(paste0("mort", survey_year))
#   paste(mort$tag, mort$StemTag, sep = "_")
# })))] # only keep the ones that were mort sampled at some point
# 
# for(survey_year in survey_years) {
#   print(survey_year)
#   
#   mort <- get(paste0("mort", survey_year))
#   tag_stems <- paste(mort$tag, mort$StemTag, sep = "_")
#   
#   m <- match(tag_stem_in_order, tag_stems)
#   mort <- mort[m, ]
#   assign(paste0("mort", survey_year), mort)  
# 
# }





data.2008 <- scbi.stem1[, c("tag", "stemtag",  "stemID","quadrat", "sp", "Latin", "gx", "gy", "hom", "dbh", "codes", "status", "ExactDate", "agb")]
names(data.2008) <- c("tag", "stemtag",  "stemID","quadrat", "sp", "Latin", "gx", "gy", "hom", "dbh.2008", "codes.2008", "status.2008", "date.2008", "agb.2008")

data.2013 <- scbi.stem2[, c("dbh", "codes", "status", "ExactDate", "agb")]
names(data.2013) <- c("dbh.2013", "codes.2013", "status.2013", "date.2013", "agb.2013")

data.2018 <- scbi.stem3[, c("dbh", "codes", "status", "ExactDate", "agb")]
names(data.2018) <- c("dbh.2018", "codes.2018", "status.2018", "date.2018", "agb.2018")


full.census.data <- cbind(data.2008, data.2013, data.2018) # added local coordinates here
head(full.census.data)



# order the columns

full.census.data <- full.census.data[,c("tag", "stemtag", "stemID", "quadrat", "sp", "Latin", "gx", 
                                        "gy", "lx", "ly", "hom", "dbh.2008", "codes.2008", "status.2008", "date.2008", 
                                        "agb.2008", "dbh.2013", "codes.2013", "status.2013", 
                                        "date.2013", "agb.2013", "dbh.2018", "codes.2018", "status.2018", 
                                        "date.2018", "agb.2018")]

# double check order

all(apply(cbind(data.2008$tag, data.2013$tag, mort14$tag, mort15$tag, mort16$tag, mort17$tag, mort18$tag, mort19$tag), 1 , function(x) all( x[!is.na(x)] == x[!is.na(x)][1] ))) # has to be TRUE

all(apply(cbind(data.2008$stemtag, data.2013$stemtag, mort14$stemtag, mort15$stemtag, mort16$stemtag, mort17$stemtag, mort18$stemtag), 1 , function(x) all( x[!is.na(x)] == x[!is.na(x)][1] ))) # has to be TRUE


for (survey_year in survey_years) {
  
  print(paste("Preparing and saving final data set for", survey_year))
  
  mort <- get(paste0("mort", substr(survey_year, 3,4)))
  head(mort)
  
  current.census.data <-  mort[, c(paste0("status.", survey_year), "dbh.if.dead", "agb.if.dead", "perc.crown", "crown.position", "fad1", "fad2", "fad3", "fad4", "DF", "liana.load", "fraxinus.crown.thinning", "fraxinus.epicormic.growth", "EABF", "DE.count", "comments", "date", "surveyors")]
  
  if (!survey_year %in% 2014) {
    previous.census.data <- get(paste0("mort", substr(survey_year-1, 3,4)))[, paste0("status.", survey_year-1)]
    mort <- cbind.data.frame(previous.census.data, current.census.data, stringsAsFactors = FALSE)
    names(mort) <-  gsub("previous.census.data", paste0("status.", survey_year-1), names(mort))
  } else {
    mort <- current.census.data
  }
  
  mort$date <- as.Date(mort$date, format = "%m/%d/%Y")
  names(mort) <- gsub("date$", paste0("date.", survey_year), names(mort))
  names(mort) <- gsub("if.dead", paste0("if.dead.", survey_year), names(mort))
  
  
  final.mort <- cbind(full.census.data, mort)
  final.mort <- final.mort[order(as.numeric(final.mort$tag), as.numeric(final.mort$stemtag)), ]
  
  assign(paste0("final.mort.", survey_year), final.mort)
  write.csv(final.mort, file = paste0("data/mortality_", survey_year, ".csv"), row.names = F)
}

# also save one data frame for 2008, 2013 and 2018 ####
data.2008 <- data.2008[order(as.numeric(data.2008$tag), as.numeric(data.2008$stemtag)), ]
full.census.data <- full.census.data[order(as.numeric(full.census.data$tag), as.numeric(full.census.data$stemtag)), ]
full.census.data$date.2008 <- as.Date(full.census.data$date.2008, format = "%m/%d/%Y")
full.census.data$date.2013 <- as.Date(full.census.data$date.2013, format = "%m/%d/%Y")

write.csv(data.2008, file = "data/mortality_2008.csv", row.names = F)
write.csv(full.census.data[, -grep("2018", names(full.census.data))], file = "data/mortality_2013.csv", row.names = F)


# CREATE allmort.rdata file ####
allmort <- cbind(full.census.data[, c("tag", "stemtag", "stemID", "quadrat", "Latin", "sp", "gx", "gy", "lx", "ly", "dbh.2008", "date.2008", "agb.2008", "status.2008", "dbh.2013", "date.2013", "agb.2013", "status.2013", "dbh.2018", "date.2018", "agb.2018", "status.2018")],

                 do.call(cbind, lapply(survey_years, function(survey_year) {
                   final.mort <- get(paste0("final.mort.", survey_year))
                   final.mort <- final.mort[match(paste(full.census.data$tag, full.census.data$stemtag), paste(final.mort$tag, final.mort$stemtag)), paste0(c("status.", "date.", "dbh.if.dead.", "agb.if.dead."), survey_year)]
                   return(final.mort)
                 })),
                 Latin = full.census.data$Latin
      ) # forgeting about fad and positions

# Add Genus, spsecies, Familly
allmort <- cbind(allmort, scbi.spptable[match(allmort$sp,scbi.spptable$sp), c("Genus", "Species", "Family")])

# change status format
status.columns <- sort(unique(names(allmort)[grepl("status", names(allmort))]))

for(sc in status.columns) {
  allmort[, sc] <- ifelse(is.na(allmort[, sc]), NA,
                          ifelse(allmort[, sc] %in% "P", "Prior",
                                 ifelse(grepl("A", allmort[, sc]), "Live", "Dead")))
}

head(allmort)

write.csv(allmort, "data/allmort.csv", row.names = F) # save as csv file
save(allmort, file ="data/allmort.rdata") # save as Rdata file






# order the columns the way we want it
mort <- mort[, c("quadrat", "tag", "stemtag", "sp", "lx", "ly", ifelse(survey_year >= 2019, "dbh.2018", "dbh.2013"),
                 grep("status", names(mort), value = T), "dbh.if.dead",
                 "perc.crown", "crown.position", "fad1", "fad2", "fad3", "fad4",
                 "DF", "liana.load", "fraxinus.crown.thinning", "fraxinus.epicormic.growth",
                 "EABF","DE.count", "notes", "date", "surveyor")]
