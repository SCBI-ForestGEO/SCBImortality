# Generate reports looking at latest mortality census raw data ####
## this script is run automatically when there is a push 

# clear environment ####
rm(list = ls())

# load libraries ####
library(data.table)
library(tidyverse)

# load the list (and code) of checks
checks <- fread("R_scripts/GitHubAction_checks.csv") %>% filter(activateCheck == TRUE)

# load latest mortality data ####

tree <- fread("raw_data/Field_Maps/NZCBI_trees_mortality_2026_0.csv")
stem <- fread("raw_data/Field_Maps/NZCBI_stems_mortality_2026_1.csv") 

# in 2025 columns "CreationDate" "Creator"      "EditDate"     "Editor" were duplicated, second set is to be kept
if(!all(names(tree)[which(duplicated(names(tree)))] == c("CreationDate", "Creator", "EditDate", "Editor"))) stop("set of duplicated columns is not as expected (2025 check)")
tree <- tree[, -which(duplicated(names(tree), fromLast = T)), with = F]


# in 2025 tree, 139 trees were duplicated in the app --> delete those
if(sum(duplicated(tree$tag)) >= 139) stop("different number of duplicates trees than expected (139 in 2025)")
if((tree %>% group_by(tag, mort_census_status) %>% filter(n()>1) %>% nrow()) >= 278) stop("some duplicates have different finish status --> need to figure out which one to keep ")

tree <- tree %>% distinct(tag, .keep_all = TRUE)
 
# collate tree and stem together ####
setdiff(names(tree), names(stem))
setdiff(names(stem), names(tree))

## remove personel  (we only want personel list)
tree$personnel <- NULL

## remove x2 and y2
range(tree[, x - x2])# super tiny, essentially same
range(tree[, y - y2]) # super tiny, essentially same

tree$x2 <- NULL
tree$y2 <- NULL

## double check what is left
setdiff(names(tree), names(stem))
setdiff(names(stem), names(tree))

## copy over c("lx", "ly", "x", "y", "personnel_list", "date_measured") from tree to stem
setdiff(stem$tag, tree$tag)

stem[, c("lx", "ly", "x", "y", "personnel_list", "date_measured")] <- tree[match(stem$tag, tree$tag), c("lx", "ly", "x", "y", "personnel_list", "date_measured")]

## double check what is left
setdiff(names(tree), names(stem)) # should be empty
setdiff(names(stem), names(tree))  # should be empty


## add the stems to the trees
stem <- bind_rows(tree, stem) # now stem is both trees and stems together


# keep a copy of all these stems, and subset stem to the ones censused
stem_all <- stem

stem <- stem %>% filter(mort_census_status %in% "finished")

# in 2026 1 secondary stem was forgotten in the app, adding it manually here so they don't show up in error log
if(grepl(2026, Sys.Date())) {
  # note the stem was not in the app because it was found dead in 2023
  stem <- rbind(stem, 
                structure(list(ObjectID = NA, quadrat = 416L, StemTag = 2L, 
                               sp = "tiam", dbh_2023 = 431L, hom = 1.3, lx = 10.2, ly = 0.800000000000011, 
                               status_2025 = NA, fad_2025 = "", comment_2025 = "", x = -78.14685983, 
                               y = 38.89333384, status_2026 = "LI", mort_census_status = "finished", 
                               fad_choices = "", fad_2026 = "", lean_angle = 1L, liana_load = 2L, 
                               dead_with_resprout = 0L, crown_intact = 5L, crown_living = 5L, 
                               crown_check = NA, personnel_list = "C", date_measured = "6/8/2026 6:49:03 PM", 
                               comment_mortality_2026 = "", GlobalID = NA, 
                               tag = 41016L, BLD = 0L, ParentGlobalID = "c2e0e6a2-5414-4385-9fe8-0421b1142f78"), row.names = c(NA, 
                                                                                                      -1L), class = c("data.table", "data.frame")))
}

# PERFORM CHECKS ------------------------------------------------------
cat("Running main census checks") # this is to troubleshoot CI on GitHub actions (see where errors happen)


allErrors <- NULL

for (i in 1:nrow(checks)) {
  
  # bring all info into environment
  list2env(checks[i, ], .GlobalEnv)
  
  cat(errorDescription, 
      "\n")
  
  # go through the step to find the errors
  referenceTable <- get(referenceTable)
  currentTable <- get(currentTable)
  
  #filter rows
  referenceTable <- referenceTable[eval(str2lang(referenceTableFilter)), ] 
  
  # if(!errorName %in% c("quadratIsNotTheSameInAllStems", "speciesIsNotTheSameInAllStems")) {
  currentTable <- currentTable[eval(str2lang(currentTableFilter)), ]
  # } else {
  #   if(errorName %in% "quadratIsNotTheSameInAllStems") currentTable <- currentTable[,if(uniqueN(quadrat) > 1) .SD else .SD[FALSE, ], by = tag] 
  #   
  #   if(errorName %in% "speciesIsNotTheSameInAllStems") currentTable <- currentTable[,if(uniqueN(species) > 1) .SD else .SD[FALSE, ],, by = tag]
  #   
  # }
  
  
  # select columns
  if(!referenceTableSelect %in% "")  reference <- referenceTable[, eval(str2lang(referenceTableSelect)) ] else reference <- referenceTable
  if(!currentTableSelect %in% "")  current <- currentTable[, eval(str2lang(currentTableSelect)) ] else current <- currentTable
  
  
  idxError <- eval(str2lang(idxError))
  
  if(sum(idxError) > 0) {
    allErrors <- dplyr::bind_rows(allErrors, data.table(errorType, errorName, referenceTable[idxError, ]))
    allErrors$StemTag <- as.integer(allErrors$StemTag)
  }
}

# save the report

write.csv(allErrors, "QAQC_reports/allErrors.csv")

if (nrow(allErrors) == 0) write.csv(stem,file = "raw_data/Mortality_Survey_2026.csv", row.names = F)

# plot the quadrat with errors...


# thank you little VM for doing the job


