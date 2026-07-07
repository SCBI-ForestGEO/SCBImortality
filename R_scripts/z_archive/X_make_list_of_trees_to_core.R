
#read latest all mort (done separately)

# load allmort
load("data/allmort.rdata")

# load raw yearly mort surveys
raw_data_path <- "raw_data/"

survey_files <- list.files(raw_data_path, pattern = "Mortality_Survey_.*csv")
survey_files <- survey_files[as.numeric(regmatches(survey_files, regexpr("20\\d\\d", survey_files))) < 2021] # only consider files before 2021 as starting 2021 is the CI files

all_morts_by_year <- lapply(survey_files, function(survey_file){
  
  survey_year <- as.numeric(gsub("Mortality_Survey_|\\.csv", "", survey_file))
  
  mort.census.years <- c(mort.census.years, survey_year)
  
  cat(paste("cleaning and calculating allometries on", survey_year), "...\n")
  
  # load
  mort <- read.csv(paste0(raw_data_path, survey_file), stringsAsFactors = F)
  
  # fix column names to be consistant
  names(mort) <- tolower(names(mort))
  
  names(mort) <- gsub("species", "sp", names(mort))
  names(mort) <- gsub("^stem$", "stemtag", names(mort))
  names(mort) <- gsub("previous.condition", paste0("status.", survey_year -1 ), names(mort))
  names(mort) <- gsub("new.condition|new.status", paste0("status.", survey_year ), names(mort))
  names(mort) <- gsub("^.*comments", "notes", names(mort))
  names(mort) <- gsub("surveyors", "surveyor", names(mort))
  names(mort) <- gsub("de.count", "d.shaped.exit.hole.count", names(mort))
  
  # names(mort) <- gsub("codes", "code", names(mort)) # commenting this out to ignor code.2013 that is causing more problem than we need...
  names(mort) <- gsub("status\\.\\.","status.", names(mort))
  names(mort) <- gsub("fad\\.", "fad", names(mort))
  names(mort) <- gsub(" ", ".", names(mort))
  
  
  
  
  # add/remove columns
  mort$old.comments <- NULL
  mort[, c("vb", "ss", "as", "w", "de")] <- NULL # for 2017, no need of those.
  
  if(!any(grepl("lx", names(mort)))) mort$lx <- NA
  if(!any(grepl("ly", names(mort)))) mort$ly <- NA
  if(!any(grepl("dbh.if.dead", names(mort)))) mort$dbh.if.dead <- NA
  if(!any(grepl("fad4", names(mort)))) mort$fad4 <- NA
  if(!any(grepl("DF", names(mort)))) mort$DF <- NA
  if(!any(grepl("fraxinus.crown.thinning", names(mort)))) mort$fraxinus.crown.thinning <- NA
  if(!any(grepl("fraxinus.epicormic.growth", names(mort)))) mort$fraxinus.epicormic.growth <- NA
  if(!any(grepl("EABF", names(mort)))) mort$EABF <- NA
  if(!any(grepl("DE.count", names(mort)))) mort$DE.count <- NA
  if(!any(grepl("fraxinus.crown.thinning", names(mort)))) mort$fraxinus.crown.thinning <- NA
  
  # padd quadrats with 0
  mort$quadrat <- as.character(  mort$quadrat)
  mort$quadrat <- ifelse(nchar(mort$quadrat) < 4, paste0("0",   mort$quadrat),   mort$quadrat)
  
  # remove extra spaces in statuses
  # mort[, paste0("status.", survey_year )] <- trimws(mort[, paste0("status.", survey_year )])
  mort[, grep("status",  names(mort))] <- apply( mort[, grep("status",  names(mort))], 2, trimws)
  
  # remove extra spaces in sp
  mort$sp <- trimws(mort$sp)
  
  # if fraxinus or Chionanthus, fix change dead status from "DS" or "DC" to "AU" if fraxinus.crown.thinning ≤ 5 and fraxinus.epicormic.growth>0 
  idx_sp <- grepl("^fr..|chvi", mort$sp)
  idx_status <- grepl("DS|DC", mort[,paste("status",survey_year, sep = ".")])
  idx_crwnthng <- !is.na(mort$fraxinus.crown.thinning) & mort$fraxinus.crown.thinning <= 5
  idx_epicgrwth <- !is.na(mort$fraxinus.epicormic.growth) & mort$fraxinus.epicormic.growth > 0
  
  mort[idx_sp & idx_status & idx_crwnthng & idx_epicgrwth, which(names(mort) %in% paste0("status.", survey_year))] <- "AU"
  
  # consider dbh as numeric
  if(survey_year >= 2019) mort$dbh.2018 <- as.numeric(mort$dbh.2018) else mort$dbh.2013 <- as.numeric(mort$dbh.2013) 
  
  # order the columns the way we want it
  mort <- mort[, c("quadrat", "tag", "stemtag", "sp", "lx", "ly", ifelse(survey_year >= 2019, "dbh.2018", "dbh.2013"),
                   grep("status", names(mort), value = T), "dbh.if.dead",
                   "perc.crown", "crown.position", "fad1", "fad2", "fad3", "fad4",
                   "DF", "liana.load", "fraxinus.crown.thinning", "fraxinus.epicormic.growth",
                   "EABF","DE.count", "notes", "date", "surveyor")]

  # save
  assign(paste0("mort", substr(survey_year, 3,4)), mort)
})

names(all_morts_by_year) <- gsub("Mortality_Survey_|.csv", "", survey_files)

# load scbi.stem3

f <- "scbi.stem3"
x <-  read.csv(paste0("https://raw.githubusercontent.com/SCBI-ForestGEO/SCBI-ForestGEO-Data/master/tree_main_census/data/census-csv-files/", f, ".csv"))
x$quadrat <-ifelse(nchar(x$quadrat) < 4, paste0("0",   x$quadrat),   x$quadrat)
x$dbh <- as.numeric(x$dbh) # not numeric because of the "NULL" values
x$gx <- round(x$gx,1)
x$gy <- round(x$gy,1)
assign(f,x)


# read dendroband data 
dendro <- read.csv("https://raw.githubusercontent.com/SCBI-ForestGEO/Dendrobands/master/data/scbi.dendroAll_2021.csv?token=AEWDCIIQUI6FLGNPAFUIBYDBIECF4")

# list cored trees 
cored <- read.csv("https://raw.githubusercontent.com/SCBI-ForestGEO/SCBI-ForestGEO-Data/master/tree_cores/list_cored_trees.csv")

# find trees that match criteria for being cored

## looking at allmort
unique(cbind(allmort$status.2020[match(paste(mort$tag, mort$stemtag), paste(allmort$tag, allmort$StemTag))], mort$status.2021))
dput(unique(apply(cbind(allmort$status.2020[match(paste(mort$tag, mort$stemtag), paste(allmort$tag, allmort$StemTag))], mort$status.2021),1 , paste, collapse = " "))) # look at all variations of being alive then dead, looking at 2020 census for prior year

## looking at raw data 2020
mort2020 <- all_morts_by_year$"2020"

unique(cbind(mort2020$status.2020[match(paste(mort$tag, mort$stemtag), paste(mort2020$tag, mort2020$stemtag))], mort$status.2021))
dput(unique(apply(cbind(mort2020$status.2020[match(paste(mort$tag, mort$stemtag), paste(mort2020$tag, mort2020$stemtag))], mort$status.2021),1 , paste, collapse = " "))) # look at all variations of being alive then dead, looking at 2020 census for prior year


idx_status <- apply(cbind(allmort$status.2020[match(paste(mort$tag, mort$stemtag), paste(allmort$tag, allmort$StemTag))], mort$status.2021),1 , paste, collapse = " ") %in% c("Live DS", "Live PD","Live DC") # get the variations we want -- allmort
idx_status <- apply(cbind(mort2020$status.2020[match(paste(mort$tag, mort$stemtag), paste(mort2020$tag, mort2020$stemtag))], mort$status.2021),1 , paste, collapse = " ") %in% c("A DS", "AU DS", "AU DC", "AU PD",   "A DC", "A PD") # get the variations we want -- looking at raw data 2020


idx_dbh <- allmort$dbh.2018[match(paste(mort$tag, mort$stemtag), paste(allmort$tag, allmort$StemTag))] > 100 & !is.na(allmort$dbh.2018[match(paste(mort$tag, mort$stemtag), paste(allmort$tag, allmort$StemTag))] ) # keep only when dbh > 100 mm (10 cm)-- allmort
idx_dbh <- mort2020$dbh.2018[match(paste(mort$tag, mort$stemtag), paste(mort2020$tag, mort2020$stemtag))] > 100 & !is.na(mort2020$dbh.2018[match(paste(mort$tag, mort$stemtag), paste(mort2020$tag, mort2020$stemtag))] ) # keep only when dbh > 100 mm (10 cm)-- looking at raw data 2020


tag_stemtag <- paste(mort$tag, mort$stemtag)[idx_status & idx_dbh] # get the tag and stemtag we want
                                                                                 

x <- scbi.stem3[match(tag_stemtag, paste(scbi.stem3$tag, scbi.stem3$StemTag)), c("tag", "StemTag", "sp", "quadrat", "gx", "gy", "dbh", "hom", "status") ] # take main census data first

names(x) <- gsub("status", "s18main", names(x))
# x[, paste0("s", c(8, 13:20))] <- ifelse(allmort[match(tag_stemtag, paste(allmort$tag, allmort$StemTag)), paste0("status.", c(2008, 2013:2020))]== "Live", "A", "D") # add previous status-- allmort
x[, paste0("s", c(14:20))] <- sapply(names(all_morts_by_year), function(y) {all_morts_by_year[[y]][match(tag_stemtag, paste(all_morts_by_year[[y]]$tag, all_morts_by_year[[y]]$stemtag)), paste0("status.", y)]}) # add previous status-- looking at raw data 2020

x$status.2021 <- mort$status.2021[match(tag_stemtag, paste(mort$tag, mort$stemtag))] # add 2021 status





# add priorities: see this issue: https://github.com/SCBI-ForestGEO/SCBImortality/issues/50

# find out who has dendroband

x$dendroband <- ifelse(paste(x$tag, x$StemTag) %in% paste(dendro$tag, dendro$stemtag), 1, 0)
sum(x$dendroband ) # 32 trees have dendroband, 16 if looking at raw data

# is tree > 50 cm ?
x$above_50cm_dbh <- ifelse(x$dbh>500, 1, 0); sum(x$above_50cm_dbh) # 81 of them, 40 if looking at raw data


# is tree an oak ?
x$oak <- ifelse(grepl("qu", x$sp), 1, 0); sum(x$oak ) # 98 of them, 46 if looking at raw data


# randomly select smaller trees, not oak, no dendro
table(x$dendroband+ x$above_50cm_dbh+ x$oak) # 254 trees have not been prioritized, lets randomly pick ~10 of those
x$small_non_oak_no_dendro_random <- 0
x$small_non_oak_no_dendro_random[c( x$dendroband+ x$above_50cm_dbh+ x$oak) == 0] <- rbinom(n = sum(( x$dendroband+ x$above_50cm_dbh+ x$oak) == 0), size = 1,prob = 10/254)



# check that none of these trees were cored
x$cored_dead <- ifelse(paste(x$tag, x$StemTag) %in% paste(cored$tag, cored$stemtag)[cored$status %in% "dead"], 1, 0) ; sum(x$cored) #50 with allmort 0 of looking at raw data

# save

write.csv(x[c("tag", "StemTag", "sp", "quadrat", "gx", "gy", "dbh", "hom", "s14", "s15", "s16", "s17", "s18main", "s18", "s19", 
              "s20", "status.2021", "dendroband", "above_50cm_dbh", "oak", 
              "small_non_oak_no_dendro_random", "cored_dead")], file = "data/list_trees_to_core_temporary_file.csv", row.names = F)
