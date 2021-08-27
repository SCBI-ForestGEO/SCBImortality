
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
w <- read.csv("testthat/reports/trace_of_reports/warnings_file.csv")


## previous mortality census files

# mort_files <- list.files("data/", pattern = "\\d\\d.csv", full.names = T)
#
# mort_files <- sapply(mort_files, read.csv)
# names(mort_files) <- regmatches(names(mort_files), regexpr("\\d{4}", names(mort_files))) # give year as name

## load allmort data 
load("C:/Users/herrmannV/Dropbox (Smithsonian)/GitHub/SCBI-ForestGEO/SCBImortality/data/allmort.rdata") # load allmort data

## look at types of warnings #### 
table(w$warning_name) # add code if some are not adressed below

# check status history of status issues ####
st <- w[w$warning_name %in% "Dead_but_now_alive", ]

status_history <- cbind(st[, c("Quad", "Tag", "StemTag", "DBH")], allmort[match(paste(st$Tag, st$StemTag), paste(allmort$tag, allmort$StemTag)), sort(grep("status", names(allmort), value = T))], "/" = "/", st[,grep("Status", names(st))], st$Notes.2021)

status_history$status.2018.1 <- NULL

status_history <- status_history[order(status_history$Status.2018), ]

View(status_history)

write.table(status_history, "temporary_files_for_QA_QC/status_history_Dead_but_now_alive.csv", sep = "|", quote = F, row.names = F)
