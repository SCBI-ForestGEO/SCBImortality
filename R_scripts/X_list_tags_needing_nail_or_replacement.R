# create list of tag numbers that need replacement see https://github.com/SCBI-ForestGEO/2023census/issues/7



THIS IS NOW HAPPENING AT THE END OF SCRIPT CI_1





# # clear environment
# rm(list = ls())
# 
# # load libraries
# 
# 
# # load libraries ####
# library(here)
# library(readxl)
# 
# # load latest mortality data ####
# 
# ## get the name of latest excel form
# # latest_FFFs <- list.files(here("raw_data/FFF_excel/"), pattern = ".xlsx", full.names = T)
# # latest_FFFs <- latest_FFFs[which.max(as.numeric(regmatches(latest_FFFs, regexpr("20\\d\\d", latest_FFFs))))] # take the latest file only
# latest_FFFs <- "raw_data/FFF_excel/SCBI Mortality 2022.xlsx" #update this for cencus 2023
# static_FFFs <- "raw_data/FFF_excel/SCBI Mortality static 2022.xlsx" #this is static form that was used for one week during 2022 that is missing one column 
# 
# 
# ## load the latest mortality survey
# 
# mort1 <- as.data.frame(read_xlsx(latest_FFFs, sheet = "section_1", .name_repair = "minimal" ))
# mort2 <- as.data.frame(read_xlsx(latest_FFFs, sheet = "section_2", .name_repair = "minimal" ))
# 
# mort <- merge(mort1, mort2, by = intersect(names(mort1), names(mort2)))
# 
# mort_root <- as.data.frame(read_xlsx(latest_FFFs, sheet = "Root", .name_repair = "minimal" ))
# mort <- cbind(SurveyorID = mort_root$Personnel[match(mort$`Submission Id`, mort_root$`Submission Id`)],
#               date = mort_root$"Date/Time"[match(mort$`Submission Id`, mort_root$`Submission Id`)],
#               mort)
# 
# static1 <- as.data.frame(read_xlsx(static_FFFs, sheet = "section_1", .name_repair = "minimal" ))
# static2 <- as.data.frame(read_xlsx(static_FFFs, sheet = "section_2", .name_repair = "minimal" ))
# 
# static <- merge(static1, static2, by = intersect(names(static1), names(static2)))
# 
# static_root <- as.data.frame(read_xlsx(static_FFFs, sheet = "Root", .name_repair = "minimal" ))
# static <- cbind(SurveyorID = static_root$Personnel[match(static$`Submission Id`, static_root$`Submission Id`)],
#                 date = static_root$"Date/Time"[match(static$`Submission Id`, static_root$`Submission Id`)],
#                 static)
# 
# #confirm difference between mort and static 
# setdiff(names(mort), names(static))
# setdiff(names(static), names(mort))
# 
# static$"Crown position < 10 cm DBH" <- NA
# 
# #confirm mort and static are same now 
# setdiff(names(mort), names(static))
# setdiff(names(static), names(mort))
# 
# #stack mort and static
# mort <- rbind(mort, static[,names(mort)])
# 
# 
# # get all the lines with a value in Tag maintenance
# # x <- mort[!is.na(mort$`Tag maintenance`),]
# x <- mort[mort$`Tag maintenance` %in% "RT",]
# 
# dput(x$Tag)
# 
# cat(paste(x$Tag, collapse = ","), file = paste0(dirname(getwd()), "/2023census/list_tags_needing_new_tags.txt"), quote = F)
# 
# 
# x <- mort[mort$`Tag maintenance` %in% "NN",]
# dim(x)
# dput(x$Tag)
# 
# cat(paste(x$Tag, collapse = ","), file=paste0(dirname(getwd()), "/2023census/list_tags_needing_nails.txt"),  quote = F)
