# manually fixing issues in the raw data ####

# *** this script is sourced in script 1 and should only be run from there ***  #####

## FIXING PROBLEMS ####


#### the tag and species ID issue: In full census data, tag 33331 appears 2 times. Looking like a 2-stem quru. In 2014 it was found that the biggest stem was actually tagged with tag 30365. The tag number was changed only in mortality census 2017 and Maya and Ryan ID-ed that second stem as fram. but the species ID was never changed. I am fixing that in all mortality related data now, by changing tag# stemtag and species in all dataframes. BUT, after 2018 main census, it seems that instructions were given to Suzanne Lao to change the tag number but on both stems.... so we end up with the same issue of having tag 30365 appearing 2 times, with same stemtag #1... So I'll, ignore that for now but eventually, this will need to be fixed.
### issue put in github that might explain this better:
# 33331 vs 30365: Up until the 2018 main census, in full census data, tag 33331 appeared 2 times with same stemtag 1, looking like a 2-stem quru with wrong stemtag on smaller stem. In 2014 it was found that the biggest stem was actually tagged with tag 30365. The tag number was changed only in mortality census 2017 and Maya and Ryan ID-ed that second stem as being a fram, but the species ID was never changed in the data sets.
# So it seems that the truth would be:
# 33331 1 quru 161.2
# 30365 1 fram 992.0
#
# BUT, after 2018 main census, it seems that instructions were given to Suzanne Lao to change the tag number and species of BOTH stems, instead of the biggest stem only. So tag 33331 disappeared and we ended up with the same issue of having tag 30365 stemtag 1 appearing 2 times (but this time on a fram).
#
# So:
# Long term solution: tell Suzanne that smaller stem is a quru with tag 33331.
# Short term solution (for building new master mortality census data) changing 33331 quru to 30365 fram AND, for here, put a stemtag = 2 to smaller stem


scbi.stem1[scbi.stem1$tag %in% 33331,]; scbi.stem1[scbi.stem1$tag %in% 30365,]
scbi.stem2[scbi.stem2$tag %in% 33331,]; scbi.stem2[scbi.stem2$tag %in% 30365,]
scbi.stem3[scbi.stem3$tag %in% 33331,]; scbi.stem3[scbi.stem3$tag %in% 30365,]

mort14[mort14$tag %in% 33331, ]; mort14[mort14$tag %in% 30365, ]
mort15[mort15$tag %in% 33331, ]; mort15[mort15$tag %in% 30365, ]
mort16[mort16$tag %in% 33331, ]; mort16[mort16$tag %in% 30365, ]
mort17[mort17$tag %in% 33331, ]; mort17[mort17$tag %in% 30365, ]
mort18[mort18$tag %in% 33331, ]; mort18[mort18$tag %in% 30365, ]
mort19[mort19$tag %in% 33331, ]; mort19[mort19$tag %in% 30365, ]
mort20[mort20$tag %in% 33331, ]; mort20[mort20$tag %in% 30365, ]


# # original and correct fix, before main census were wrongly corrected...
#
# mort14[mort14$tag %in% 33331, ]$stemtag <- c(1, 1)
# mort15[mort15$tag %in% 33331, ]$stemtag <- c(1, 1)
# mort16[mort16$tag %in% 33331, ]$stemtag <- c(1, 1)
#
#
# scbi.stem1[scbi.stem1$tag %in% 33331,]$sp <- c("fram", "quru")
# scbi.stem2[scbi.stem2$tag %in% 33331,]$sp <- c("fram", "quru")
# mort14[mort14$tag %in% 33331, ]$sp <- c("fram", "quru")
# mort15[mort15$tag %in% 33331, ]$sp <- c("quru", "fram")
# mort16[mort16$tag %in% 33331, ]$sp <-  c("quru", "fram")
#
# scbi.stem1[scbi.stem1$tag %in% 33331,]$tag <- c(30365, 33331)
# scbi.stem2[scbi.stem2$tag %in% 33331,]$tag <- c(30365, 33331)
# mort14[mort14$tag %in% 33331, ]$tag <- c(30365, 33331)
# mort15[mort15$tag %in% 33331, ]$tag <- c(33331, 30365)
# mort16[mort16$tag %in% 33331, ]$tag <- c(33331, 30365)

# current fix, until we decide to to give tag 33331 back to truth:
# 33331 1 quru 161.2
# 30365 1 fram 992.0...

mort14[mort14$tag %in% 33331, ]$sp <- c("fram", "fram")
mort15[mort15$tag %in% 33331, ]$sp <- c("fram", "fram")
mort16[mort16$tag %in% 33331, ]$sp <-  c("fram", "fram")
mort17[mort17$tag %in% 33331, ]$sp <-  "fram"
mort18[mort18$tag %in% 33331, ]$sp <-  "fram"
# no need for mort19

mort14[mort14$tag %in% 33331, ]$tag <- 30365
mort15[mort15$tag %in% 33331, ]$tag <- 30365
mort16[mort16$tag %in% 33331, ]$tag <- 30365
mort17[mort17$tag %in% 33331, ]$tag <- 30365
mort18[mort18$tag %in% 33331, ]$tag <- 30365
# no need for mort19



# scbi.stem1[scbi.stem1$tag %in% 30365,]$stemtag <- c(1,2)[order(scbi.stem1[scbi.stem1$tag %in% 30365,]$dbh, decreasing = T)]
# scbi.stem2[scbi.stem2$tag %in% 30365,]$stemtag <- c(1,2)[order(scbi.stem1[scbi.stem1$tag %in% 30365,]$dbh, decreasing = T)]
# scbi.stem3[scbi.stem3$tag %in% 30365,]$stemtag <- c(1,2)[order(scbi.stem1[scbi.stem1$tag %in% 30365,]$dbh, decreasing = T)]○

mort14[mort14$tag %in% 30365, ]$stemtag <- c(1,2)[order(mort14[mort14$tag %in% 30365,]$dbh.2013 , decreasing = T)]
mort15[mort15$tag %in% 30365, ]$stemtag <- c(1,2)[order(mort15[mort15$tag %in% 30365,]$dbh.2013, decreasing = T)]
mort16[mort16$tag %in% 30365, ]$stemtag <- c(1,2)[order(mort16[mort16$tag %in% 30365,]$dbh.2013, decreasing = T)]
mort17[mort17$tag %in% 30365, ]$stemtag <- c(1,2)[order(mort17[mort17$tag %in% 30365,]$dbh.2013, decreasing = T)]
mort18[mort18$tag %in% 30365, ]$stemtag <- c(1,2)[order(mort18[mort18$tag %in% 30365,]$dbh.2013, decreasing = T)]
mort19[mort19$tag %in% 30365, ]$stemtag <- c(1,2)[order(mort19[mort19$tag %in% 30365,]$dbh.2018, decreasing = T)]
mort20[mort20$tag %in% 30365, ]$stemtag <- c(1,2)[order(mort20[mort20$tag %in% 30365,]$dbh.2018, decreasing = T)]



#### "tag 133461 stemtag 1 (bigger stem) (quadrat 1316, comment in 2015: Ojo:this is not caco, it is fram. Not 2 stem! Fram tag=131352)"
## looks like the bigger stem has a different tag and is fram. After 2018 main census, the tag and species were fixed but the species of the smaller stem (tag 133461), which was originally caco, was also changed to fram, not sure if it was made on purpose. I will do the same here...

scbi.stem1[scbi.stem1$tag %in% 133461,]; scbi.stem1[scbi.stem1$tag %in% 131352,]
scbi.stem2[scbi.stem2$tag %in% 133461,]; scbi.stem2[scbi.stem2$tag %in% 131352,]
scbi.stem3[scbi.stem3$tag %in% 133461,]; scbi.stem3[scbi.stem3$tag %in% 131352,]

mort14[mort14$tag %in% 133461, ]; mort14[mort14$tag %in% 131352, ]
mort15[mort15$tag %in% 133461, ]; mort15[mort15$tag %in% 131352, ]
mort16[mort16$tag %in% 133461, ]; mort16[mort16$tag %in% 131352, ]
mort17[mort17$tag %in% 133461, ]; mort17[mort17$tag %in% 131352, ]
mort18[mort18$tag %in% 133461, ]; mort18[mort18$tag %in% 131352, ]
mort19[mort19$tag %in% 133461, ]; mort19[mort19$tag %in% 131352, ]
mort20[mort20$tag %in% 133461, ]; mort20[mort20$tag %in% 131352, ]


# scbi.stem1[scbi.stem1$tag %in% 133461 & scbi.stem1$dbh > 900, ]$sp <- "fram"
# scbi.stem2[scbi.stem2$tag %in% 133461 & scbi.stem2$dbh > 900, ]$sp <- "fram"
# scbi.stem1[scbi.stem1$tag %in% 133461 & scbi.stem1$dbh < 900, ]$stemtag <- 1
# scbi.stem2[scbi.stem2$tag %in% 133461 & scbi.stem2$dbh < 900, ]$stemtag <- 1
# scbi.stem1[scbi.stem1$tag %in% 133461 & scbi.stem1$dbh > 900, ]$tag <- 131352
# scbi.stem2[scbi.stem2$tag %in% 133461 & scbi.stem2$dbh > 900, ]$tag <- 131352

mort14[mort14$tag %in% 133461, ]$sp <- "fram"
mort14[mort14$tag %in% 133461 & mort14$dbh.2013 < 900, ]$stemtag <- 1
mort14[mort14$tag %in% 133461 & mort14$dbh.2013 > 900, ]$tag <- 131352


mort15[mort15$tag %in% 133461, ]$sp <- "fram"
mort15[mort15$tag %in% 133461 & mort15$dbh.2013 < 900, ]$stemtag <- 1
mort15[mort15$tag %in% 133461 & mort15$dbh.2013 > 900, ]$tag <- 131352


mort16[mort16$tag %in% 133461, ]$sp <- "fram"
mort16 <- mort16[!(mort16$tag %in% 133461 & mort16$dbh.2013 > 900), ]
mort16[mort16$tag %in% 133461 & mort16$dbh.2013 < 900, ]$stemtag <- 1

# mort16[mort16$tag %in% 133461 & mort16$dbh.2013 > 900, ]$sp <- "fram"
# mort16[mort16$tag %in% 133461 & mort16$dbh.2013 < 900, ]$stemtag <- 1
# mort16[mort16$tag %in% 133461 & mort16$dbh.2013 > 900, ]$tag <- 131352

mort17[mort17$tag %in% 133461, ]$sp <- "fram"
mort17 <- mort17[!(mort17$tag %in% 133461 & mort17$dbh.2013 > 900), ]
mort17[mort17$tag %in% 133461 & mort17$dbh.2013 < 900, ]$stemtag <- 1

# mort17[mort17$tag %in% 133461 & mort17$dbh.2013 > 900, ]$sp <- "fram"
# mort17[mort17$tag %in% 133461 & mort17$dbh.2013 < 900, ]$stemtag <- 1
# mort17[mort17$tag %in% 133461 & mort17$dbh.2013 > 900, ]$tag <- 131352

mort18[mort18$tag %in% 133461, ]$sp <- "fram"
mort18 <- mort18[!(mort18$tag %in% 131352 & mort18$EABF %in% "AS;W;DE"),]
# mort18[mort18$tag %in% 133461 & mort18$dbh.2013 > 900, ]$sp <- "fram"
# mort18[mort18$tag %in% 133461 & mort18$dbh.2013 < 900, ]$stemtag <- 1
# mort18[mort18$tag %in% 133461 & mort18$dbh.2013 > 900, ]$tag <- 131352


# 170982 is fram and not liriodendron
scbi.stem1[scbi.stem1$tag %in% 170982,]
scbi.stem2[scbi.stem2$tag %in% 170982,]
scbi.stem3[scbi.stem3$tag %in% 170982,]
mort14[mort14$tag %in% 170982, ]$sp <- "fram"
mort15[mort15$tag %in% 170982, ]$sp <- "fram"
mort16[mort16$tag %in% 170982, ]$sp <- "fram"
mort17[mort17$tag %in% 170982, ]$sp <- "fram"
mort18[mort18$tag %in% 170982, ]$sp <- "fram"
mort19[mort19$tag %in% 170982, ]$sp
mort20[mort20$tag %in% 170982, ]$sp

# 80180 is cagl in main census, qupr in mort census --> will change to cagl
scbi.stem1[scbi.stem1$tag %in% 80180,]
scbi.stem2[scbi.stem2$tag %in% 80180,]
scbi.stem3[scbi.stem3$tag %in% 80180,]
mort14[mort14$tag %in% 80180, ]$sp <- "cagl"
mort15[mort15$tag %in% 80180, ]$sp <- "cagl"
mort16[mort16$tag %in% 80180, ]$sp <- "cagl"
mort17[mort17$tag %in% 80180, ]$sp <- "cagl"
mort18[mort18$tag %in% 80180, ]$sp <- "cagl"
mort19[mort19$tag %in% 80180, ]$sp <- "cagl"
mort20[mort20$tag %in% 80180, ]$sp <- "cagl"


# 131272 is fram in all censuses except mort20 where it is juni
mort20[mort20$tag %in% 131272, ]$sp <- "fram"


# 123412 is fram in all main censuses but frpe in mort20 + quadrat is wrong in mort20
mort20[mort20$tag %in% 123412, ]$sp <- "fram"
mort20[mort20$tag %in% 123412, ]$quadrat <- "1204"


# mismatch of 40873and 40874 in mort census ####
## truth in 2013
## 40874 1 fagr 641.3
## 40874 2 fagr 161.8
## 40873 1 quve 325

scbi.stem2[scbi.stem2$tag %in% 40873,]; scbi.stem2[scbi.stem2$tag %in% 40874,]
scbi.stem3[scbi.stem3$tag %in% 40873,]; scbi.stem3[scbi.stem3$tag %in% 40874,]

mort14[mort14$tag %in% 40873, ]; mort14[mort14$tag %in% 40874, ]
mort15[mort15$tag %in% 40873, ]; mort15[mort15$tag %in% 40874, ]
mort16[mort16$tag %in% 40873, ]; mort16[mort16$tag %in% 40874, ]
mort17[mort17$tag %in% 40873, ]; mort17[mort17$tag %in% 40874, ]
mort18[mort18$tag %in% 40873, ]; mort18[mort18$tag %in% 40874, ]
mort19[mort19$tag %in% 40873, ]; mort19[mort19$tag %in% 40874, ]
mort20[mort20$tag %in% 40873, ]; mort20[mort20$tag %in% 40874, ]


mort14[mort14$tag %in% 40873 & mort14$stemtag %in% 1, ]$sp <- "quve"
mort15[mort15$tag %in% 40873 & mort15$stemtag %in% 1, ]$sp <- "quve"
mort16[mort16$tag %in% 40873 & mort16$stemtag %in% 1, ]$sp <- "quve"
mort17[mort17$tag %in% 40873 & mort17$stemtag %in% 1, ]$sp <- "quve"
mort18[mort18$tag %in% 40873 & mort18$stemtag %in% 1, ]$sp <- "quve"
# no need to do after 2018

mort14[mort14$tag %in% 40873 & mort14$stemtag %in% 2, ]$tag  <- 40874
mort15[mort15$tag %in% 40873 & mort15$stemtag %in% 2, ]$tag  <- 40874
mort16[mort16$tag %in% 40873 & mort16$stemtag %in% 2, ]$tag  <- 40874
mort17[mort17$tag %in% 40873 & mort17$stemtag %in% 2, ]$tag  <- 40874
mort18[mort18$tag %in% 40873 & mort18$stemtag %in% 2, ]$tag  <- 40874
# no need to do after 2018


## fixing dbh issues ####
scbi.stem1[scbi.stem1$tag %in% 190691 & scbi.stem1$stemtag == 1, ]$dbh
scbi.stem2[scbi.stem2$tag %in% 190691 & scbi.stem2$stemtag == 1, ]$dbh
scbi.stem3[scbi.stem3$tag %in% 190691 & scbi.stem3$stemtag == 1, ]$dbh
mort14[mort14$tag %in% 190691, ]$dbh.2013
mort15[mort15$tag %in% 190691, ]$dbh.2013
mort16[mort16$tag %in% 190691, ]$dbh.2013 <- 32.8
mort17[mort17$tag %in% 190691, ]$dbh.2013 <- 32.8
mort18[mort18$tag %in% 190691, ]$dbh.2013
mort19[mort19$tag %in% 190691, ]$dbh.2013


scbi.stem2[scbi.stem2$tag %in% 203751, ]$dbh
mort14[mort14$tag %in% 203751, ]$dbh.2013 <- 14.3
mort15[mort15$tag %in% 203751, ]$dbh.2013 <- 14.3
mort16[mort16$tag %in% 203751, ]$dbh.2013 <- 14.3
mort17[mort17$tag %in% 203751, ]$dbh.2013 <- 14.3
mort18[mort18$tag %in% 203751, ]$dbh.2013
mort19[mort19$tag %in% 203751, ]$dbh.2013


scbi.stem1[scbi.stem1$tag %in% 80560, ]
mort14[mort14$tag %in% 80560, ]
mort15[mort15$tag %in% 80560, ]
mort16[mort16$tag %in% 80560, ]
mort17[mort17$tag %in% 80560, ]
mort18[mort18$tag %in% 80560, ]
mort19[mort19$tag %in% 80560, ]

# mort14 <- mort14[-which(mort14$tag %in% 80560 & mort14$stemtag %in% 2), ]
# mort16 <- mort16[-which(mort16$tag %in% 80560 & mort16$stemtag %in% 2), ]
# mort17 <- mort17[-which(mort17$tag %in% 80560 & mort17$stemtag %in% 2), ]
# mort18 <- mort18[-which(mort18$tag %in% 80560 & mort18$stemtag %in% 2), ]
# mort19 <- mort19[-which(mort19$tag %in% 80560 & mort19$stemtag %in% 2), ]
# mort20 <- mort20[-which(mort20$tag %in% 80560 & mort20$stemtag %in% 2), ]


scbi.stem2[scbi.stem2$tag %in% 172262, ]$dbh
scbi.stem3[scbi.stem3$tag %in% 172262, ]$dbh
mort14[mort14$tag %in% 172262, ]$dbh.2013
mort15[mort15$tag %in% 172262, ]$dbh.2013
mort16[mort16$tag %in% 172262, ]$dbh.2013
mort17[mort17$tag %in% 172262, ]$dbh.2013
mort18[mort18$tag %in% 172262, ] # missing
mort19[mort19$tag %in% 172262, ]$dbh.2018
mort20[mort20$tag %in% 172262, ]$dbh.2018

mort17[mort17$tag %in% 172262, ]$dbh.2013 <- 170.7

scbi.stem3[scbi.stem3$tag %in% 172262, ]

row_to_add <- mort17[mort17$tag %in% 172262, ]
row_to_add$date <- unique(mort18[mort18$quadrat %in% 1720, ]$date)
row_to_add$surveyors <- unique(mort18[mort18$quadrat %in% 1720, ]$surveyors)
row_to_add$status.2018 <- "A"
mort18 <- rbind(mort18, row_to_add)


# row_to_add$date <- unique(mort19[mort19$quadrat %in% 1720, ]$date)
# row_to_add$surveyors <- unique(mort19[mort19$quadrat %in% 1720, ]$surveyors)
# row_to_add$status.2019 <- "A"
# names(row_to_add) <- gsub("dbh.2013", "dbh.2018",  names(row_to_add))
# row_to_add$dbh.2018 <- scbi.stem3[scbi.stem3$tag %in% 172262, ]$dbh



scbi.stem1[!is.na(scbi.stem1$dbh) & scbi.stem1$dbh < 1, ]$dbh
# scbi.stem2[!is.na(scbi.stem2$dbh) & scbi.stem2$dbh < 1, ]$dbh <- NA


#### Fixing quadrat issues ####

# not quite sure what is right but will follow main census...

quadrat.317 <- scbi.stem1$quadrat %in% "0317"
quadrat.319 <- scbi.stem1$quadrat %in% "0319"

all(scbi.stem2$quadrat[quadrat.317] %in% "0317") # should be TRUE
all(scbi.stem3$quadrat[quadrat.317] %in% "0317") # should be TRUE

all(scbi.stem2$quadrat[quadrat.319] %in% "0319") # should be TRUE
all(scbi.stem3$quadrat[quadrat.319] %in% "0319") # should be TRUE


all(mort14$quadrat[mort14$tag %in% scbi.stem1$tag[quadrat.317]] %in% "0317") # should be TRUE --> is not
all(mort15$quadrat[mort15$tag %in% scbi.stem1$tag[quadrat.317]] %in% "0317") # should be TRUE
all(mort16$quadrat[mort16$tag %in% scbi.stem1$tag[quadrat.317]] %in% "0317") # should be TRUE
all(mort17$quadrat[mort17$tag %in% scbi.stem1$tag[quadrat.317]] %in% "0317") # should be TRUE
all(mort18$quadrat[mort18$tag %in% scbi.stem1$tag[quadrat.317]] %in% "0317") # should be TRUE
all(mort19$quadrat[mort19$tag %in% scbi.stem1$tag[quadrat.317]] %in% "0317") # should be TRUE

all(mort14$quadrat[mort14$tag %in% scbi.stem1$tag[quadrat.319]] %in% "0319") # should be TRUE--> is not
all(mort15$quadrat[mort15$tag %in% scbi.stem1$tag[quadrat.319]] %in% "0319") # should be TRUE--> is not
all(mort16$quadrat[mort16$tag %in% scbi.stem1$tag[quadrat.319]] %in% "0319") # should be TRUE--> is not
all(mort17$quadrat[mort17$tag %in% scbi.stem1$tag[quadrat.319]] %in% "0319") # should be TRUE--> is not
all(mort18$quadrat[mort18$tag %in% scbi.stem1$tag[quadrat.319]] %in% "0319") # should be TRUE--> is not
all(mort19$quadrat[mort19$tag %in% scbi.stem1$tag[quadrat.319]] %in% "0319") # should be TRUE


mort14$quadrat[mort14$tag %in% scbi.stem1$tag[quadrat.317]] <- "0317"

mort14$quadrat[mort14$tag %in% scbi.stem1$tag[quadrat.319]] <- "0319"
mort15$quadrat[mort15$tag %in% scbi.stem1$tag[quadrat.319]] <- "0319"
mort16$quadrat[mort16$tag %in% scbi.stem1$tag[quadrat.319]] <- "0319"
mort17$quadrat[mort17$tag %in% scbi.stem1$tag[quadrat.319]] <- "0319"
mort18$quadrat[mort18$tag %in% scbi.stem1$tag[quadrat.319]] <- "0319"




# quadrat.317 <- scbi.stem1$quadrat %in% "0317" | scbi.stem1$tag %in% 33339
# quadrat.319 <- scbi.stem1$quadrat %in% "0319" & !scbi.stem1$tag %in% 33339
# scbi.stem1[quadrat.317, ]$quadrat <- "0319"
# scbi.stem1[quadrat.319, ]$quadrat <- "0317"
#
# quadrat.317 <- scbi.stem2$quadrat %in% "0317" | scbi.stem2$tag %in% 33339
# quadrat.319 <- scbi.stem2$quadrat %in% "0319" & !scbi.stem2$tag %in% 33339
# scbi.stem2[quadrat.317, ]$quadrat <- "0319"
# scbi.stem2[quadrat.319, ]$quadrat <- "0317"
#
# quadrat.317 <- mort14$quadrat %in% "0317" | mort14$tag %in% 33339
# quadrat.319 <- mort14$quadrat %in% "0319" & !mort14$tag %in% 33339
# mort14[quadrat.317, ]$quadrat <- "0319"
# mort14[quadrat.319, ]$quadrat <- "0317"
#
# tag.32081.in.317 <- mort15$tag %in% 32081
# mort15$quadrat[tag.32081.in.317] <- "0317"
# tag.32081.in.317 <- mort16$tag %in% 32081
# mort16$quadrat[tag.32081.in.317] <- "0317"
# tag.32081.in.317 <- mort17$tag %in% 32081
# mort17$quadrat[tag.32081.in.317] <- "0317"
# tag.32081.in.317 <- mort18$tag %in% 32081
# mort18$quadrat[tag.32081.in.317] <- "0317"
#
#
# tag.32021.32068.in.319 <- mort15$tag %in% c(32021, 32068)
# mort15$quadrat[tag.32021.32068.in.319] <- "0319"
# tag.32021.32068.in.319 <- mort16$tag %in% c(32021, 32068)
# mort16$quadrat[tag.32021.32068.in.319] <- "0319"
# tag.32021.32068.in.319 <- mort17$tag %in% c(32021, 32068)
# mort17$quadrat[tag.32021.32068.in.319] <- "0319"
# tag.32021.32068.in.319 <- mort18$tag %in% c(32021, 32068)
# mort18$quadrat[tag.32021.32068.in.319] <- "0319"




quadrat.609 <- scbi.stem1$quadrat %in% "0609"
quadrat.608 <- scbi.stem1$quadrat %in% "0608"

all(scbi.stem2$quadrat[quadrat.609] %in% "0609") # should be TRUE
all(scbi.stem3$quadrat[quadrat.609] %in% "0609") # should be TRUE

all(scbi.stem2$quadrat[quadrat.608] %in% "0608") # should be TRUE
all(scbi.stem3$quadrat[quadrat.608] %in% "0608") # should be TRUE


all(mort14$quadrat[mort14$tag %in% scbi.stem1$tag[quadrat.609]] %in% "0609") # should be TRUE
all(mort15$quadrat[mort15$tag %in% scbi.stem1$tag[quadrat.609]] %in% "0609") # should be TRUE
all(mort16$quadrat[mort16$tag %in% scbi.stem1$tag[quadrat.609]] %in% "0609") # should be TRUE
all(mort17$quadrat[mort17$tag %in% scbi.stem1$tag[quadrat.609]] %in% "0609") # should be TRUE
all(mort18$quadrat[mort18$tag %in% scbi.stem1$tag[quadrat.609]] %in% "0609") # should be TRUE
all(mort19$quadrat[mort19$tag %in% scbi.stem1$tag[quadrat.609]] %in% "0609") # should be TRUE

all(mort14$quadrat[mort14$tag %in% scbi.stem1$tag[quadrat.608]] %in% "0608") # should be TRUE
all(mort15$quadrat[mort15$tag %in% scbi.stem1$tag[quadrat.608]] %in% "0608") # should be TRUE
all(mort16$quadrat[mort16$tag %in% scbi.stem1$tag[quadrat.608]] %in% "0608") # should be TRUE
all(mort17$quadrat[mort17$tag %in% scbi.stem1$tag[quadrat.608]] %in% "0608") # should be TRUE--> is not
all(mort18$quadrat[mort18$tag %in% scbi.stem1$tag[quadrat.608]] %in% "0608") # should be TRUE--> is not
all(mort19$quadrat[mort19$tag %in% scbi.stem1$tag[quadrat.608]] %in% "0608") # should be TRUE
all(mort20$quadrat[mort20$tag %in% scbi.stem1$tag[quadrat.608]] %in% "0608") # should be TRUE

mort17$quadrat[mort17$tag %in% scbi.stem1$tag[quadrat.608]] <- "0608"
mort18$quadrat[mort18$tag %in% scbi.stem1$tag[quadrat.608]] <- "0608"


## fixing species ID issues ####
tags_to_fix <- c("92425", "12269", "20655", "30097", "30262", "32197", "32416",
                 "42175", "42349", "42522", "62307", "72059", "80007", "90268",
                 "90281", "90357", "91454", "92444", "100533", "100623", "100647",
                 "112412", "122117", "131272", "150278", "180838", "180973", "190136",
                 "103217")

for(t in tags_to_fix) {
  idx_main <- scbi.stem1$tag %in% t
  if( all(c(scbi.stem1$sp[idx_main], scbi.stem2$sp[idx_main], scbi.stem3$sp[idx_main]) %in% scbi.stem1$sp[idx_main]) & length(unique(scbi.stem1$sp[idx_main] == 1))) {
    mort14$sp[mort14$tag %in% t] <- unique(scbi.stem1$sp[idx_main])
    mort15$sp[mort15$tag %in% t] <- unique(scbi.stem1$sp[idx_main])
    mort16$sp[mort16$tag %in% t] <- unique(scbi.stem1$sp[idx_main])
    mort17$sp[mort17$tag %in% t] <- unique(scbi.stem1$sp[idx_main])
    mort18$sp[mort18$tag %in% t] <- unique(scbi.stem1$sp[idx_main])
    mort19$sp[mort19$tag %in% t] <- unique(scbi.stem1$sp[idx_main])
  } else { stop("species ID are not the same accross main censuses")}
}


## fix some status ####
mort16$status.2016[mort16$tag %in% 92465] <- "PD"
mort16$status.2016[mort16$tag %in% 40853 & mort16$dbh.2013 %in% 297.5] <- "AU"

# mort16.status.2016 should be fixed to "PD" for 92465 (fixed earlier), is correct for 40853 302.5, and should be "AU" for 40853 297.5 (fixed earlier)
