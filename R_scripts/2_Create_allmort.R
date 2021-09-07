#############################################################################
# Purpose: allmort.rdata file
# Developped by: Valentine Herrmann - HerrmannV@si.edu
##########################################################################

# Clean environment ####
rm(list = ls())

# Set working directory ####
setwd(".")

# Load libraries ####



# Load data ####
data_path <- "data/"

survey_files <- list.files(raw_data_path, pattern = "Mortality_Survey_.*csv", full.names = T)


all_morts <- sapply(survey_files, read.csv)
names(all_morts) <-  regmatches(names(all_morts), regexpr("\\d{4}", names(all_morts)))

View(sort(table(unlist(sapply(all_morts, names)))))

x <- data.frame(names = tolower(unlist(sapply(all_morts, names))))
x$year <- substr(row.names(x), 1, 4)

x$names <- gsub("codes", "code", x$names)
write.csv(table(x$names, x$year), "clipboard")



# CREATE allmort.rdata file ####
allmort <- cbind(full.census.data[, c("tag", "StemTag", "stemID", "quadrat", "Latin", "sp", "gx", "gy", "lx", "ly", "dbh.2008", "date.2008", "agb.2008", "status.2008", "dbh.2013", "date.2013", "agb.2013", "status.2013", "dbh.2018", "date.2018", "agb.2018", "status.2018")],
                 
                 do.call(cbind, lapply(mort.census.years, function(survey_year) {
                   final.mort <- get(paste0("final.mort.", survey_year))
                   final.mort <- final.mort[match(paste(full.census.data$tag, full.census.data$StemTag), paste(final.mort$tag, final.mort$StemTag)), paste0(c("status.", "date.", "dbh.if.dead.", "agb.if.dead."), survey_year)]
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
