
#read latest all mort (done separately)

# load allmort
load("data/allmort.rdata")

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


# find trees that match criteria for being cored

unique(cbind(allmort$status.2020[match(paste(mort$tag, mort$stemtag), paste(allmort$tag, allmort$StemTag))], mort$status.2021))
dput(unique(apply(cbind(allmort$status.2020[match(paste(mort$tag, mort$stemtag), paste(allmort$tag, allmort$StemTag))], mort$status.2021),1 , paste, collapse = " "))) # look at all variations of being alive then dead, looking at 2020 census for prior year

idx_status <- apply(cbind(allmort$status.2020[match(paste(mort$tag, mort$stemtag), paste(allmort$tag, allmort$StemTag))], mort$status.2021),1 , paste, collapse = " ") %in% c("Live DS", "Live PD","Live DC") # get the variations we want

idx_dbh <- allmort$dbh.2018[match(paste(mort$tag, mort$stemtag), paste(allmort$tag, allmort$StemTag))] > 100 & !is.na(allmort$dbh.2018[match(paste(mort$tag, mort$stemtag), paste(allmort$tag, allmort$StemTag))] ) # keep only when dbh > 100 mm (10 cm)


tag_stemtag <- paste(mort$tag, mort$stemtag)[idx_status & idx_dbh] # get the tag and stemtag we want
                                                                                 

x <- scbi.stem3[paste(scbi.stem3$tag, scbi.stem3$StemTag) %in% tag_stemtag, ] # take main census data first

names(x) <- gsub("status", "status.2018", names(x))
x$status.2020 <- allmort$status.2020[paste(allmort$tag, allmort$StemTag) %in% tag_stemtag] # add 2020 status
x$status.2021 <- mort$status.2021[paste(mort$tag, mort$stemtag) %in% tag_stemtag] # add 2021 status


# add priorities: see this issue: https://github.com/SCBI-ForestGEO/SCBImortality/issues/50

# find out who has dendroband

x$dendroband <- ifelse(paste(x$tag, x$StemTag) %in% paste(dendro$tag, dendro$stemtag), 1, 0)
sum(x$dendroband ) # 32 trees have dendroband

# is tree > 50 cm ?
x$above_50cm_dbh <- ifelse(x$dbh>500, 1, 0) # 81 of them


# is tree an oak ?
x$oak <- ifelse(grepl("qu", x$sp), 1, 0) # 98 of them


# randomly select smaller trees, not oak, no dendro
table(x$dendroband+ x$above_50cm_dbh+ x$oak) # 254 trees have not been prioritized, lets randomly pick ~10 of those
x$small_non_oak_no_dendro_random <- 0
x$small_non_oak_no_dendro_random[c( x$dendroband+ x$above_50cm_dbh+ x$oak) == 0] <- rbinom(n = sum(( x$dendroband+ x$above_50cm_dbh+ x$oak) == 0), size = 1,prob = 10/254)



# save

write.csv(x[c("tag", "StemTag", "sp", "quadrat", "gx", "gy", "dbh", "hom", "status.2018", "status.2020", "status.2021", 
              "dendroband", "above_50cm_dbh", "oak", "small_non_oak_no_dendro_random")], file = "data/list_trees_to_core_temporary_file.csv", row.names = F)
