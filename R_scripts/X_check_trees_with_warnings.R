
#############################################################################
# Purpose: check trees with warnings
# Developped by: Valentine Herrmann - HerrmannV@si.edu
##########################################################################

# Clean environment ####
rm(list = ls())

# Set working directory ####
setwd(".")

# Load libraries ####


# Load data ####

## warning files
w <- read.csv("testthat/reports/warnings/warnings_file.csv")


## previous mortality census files

# mort_files <- list.files("data/", pattern = "\\d\\d.csv", full.names = T)
#
# mort_files <- sapply(mort_files, read.csv)
# names(mort_files) <- regmatches(names(mort_files), regexpr("\\d{4}", names(mort_files))) # give year as name

## load allmort data 
load("C:/Users/herrmannV/Dropbox (Smithsonian)/GitHub/SCBI-ForestGEO/SCBImortality/data/allmort.rdata") # load allmort data

allmort <- reshape(allmort, idvar = c("tag", "StemTag"), timevar = "survey_year", direction = "wide" )


## look at types of warnings #### 
table(w$warning_name) # add code if some are not adressed below

# check status history of status issues ####
x <- w[w$warning_name %in% "Dead_but_now_alive", ]

status_history <- cbind(x[, c("Quad", "Tag", "StemTag", "DBH")], allmort[match(paste(x$Tag, x$StemTag), paste(allmort$tag, allmort$StemTag)), sort(grep("status", names(allmort), value = T))], "/" = "/", x[,grep("Status", names(x))], x["Notes.2022"])

status_history[, grep("previous|last", names(status_history), value = T)] <- NULL

status_history <- status_history[order(status_history$current_year_status.2021), ]

View(status_history)

write.csv(status_history, "temporary_files_for_QA_QC/status_history_Dead_but_now_alive.csv", row.names = F)



# check dbh history of status issues ####
x <- w[w$warning_name %in% c("DBH_dead_suspicious"), ]

dbh_history <- cbind(x[, c("Quad", "Tag", "StemTag")], allmort[match(paste(x$Tag, x$StemTag), paste(allmort$tag, allmort$StemTag)), sort(grep("dbh.\\d", names(allmort), value = T))], "/" = "/", x[, c("DBH", "Dead.DBH", "Notes.2022")])

dbh_history[, grep("previous|last", names(status_history), value = T)] <- NULL


dbh_history <- dbh_history[order(dbh_history$DBH), ]


View(dbh_history)

write.csv(dbh_history, "temporary_files_for_QA_QC/dbh_history_DBH_dead_suspicious.csv", row.names = F)

