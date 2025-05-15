# Generate reports looking at latest mortality census raw data ####
## this script is run automatically when there is a push 

# clear environment ####
rm(list = ls())

# load libraries ####
library(here)
library(data.table)
library(tidyverse)

# load the list (and code) of checks
checks <- fread("R_scripts/GitHubAction_checks.csv")

# load latest mortality data ####

tree <- fread("raw_data/Field_Maps/SCBI_trees_mortality_2024_0.csv")
stem <- fread("raw_data/Field_Maps/SCBI_stems_mortality_2024_1.csv") 


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

## add empty comment_motality_2023 into stem 
stem$comment_mortality_2023 <- ""

## remove dbh_if_dead_2023 from stem table
stem$dbh_if_dead_2023 <- NULL

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




# do more clean up
stem$dbh_if_dead <- NULL

# keep a copy of all these stems, and subset stem to the ones censused
stem_all <- stem

stem <- stem %>% filter(mort_census_status %in% "finished")




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

# plot the quadrat with errors...


# thank you little VM for doing the job


