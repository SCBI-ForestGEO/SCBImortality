#############################################################################
# Purpose: Calculate Mortality and Biomass mortality on cleaned data set
# Developped by: Valentine Herrmann - HerrmannV@si.edu
# R version 4.0.3 (2020-10-10)
##########################################################################

# Clean environment ####
rm(list = ls())

# Set working directory ####
setwd(".")

# Load libraries ####
library(progress)

# load data ###
load("data/allmort.RData")

# set parameters ####
hectar = 25.6 #Plot size in hectares
ddiv =  exp(seq(log(100), log(10000), length.out = 12))# c(10, 12.5, 15.5, 19.5, 24, 30, 37.5, 46.5, 58, 72, 90, 112, 140, 174, 217, 270.5, 337, 420, 523, 652, 812.5, 10000) #Approximately log-even DBH size bins
nreps = 1000 #number of bootstrap replicates for calculating CIs
gridsize = 50 # the dimension of the subplots to be used for bootstrapping: it is gridsize x gridsize m

# prepare the data ####

## consider survey year and species as a factor
allmort$survey_year <- factor(allmort$survey_year)
allmort$sp <- factor(allmort$sp)

## only keep data for trees that were alive in previous census
allmort <- allmort[grepl("A", allmort$previous_year_status), ] 

## only keep data we know current census
allmort <- allmort[!is.na(allmort$current_year_status),]

## only keep trees that were >= min dbh class during last main census + where we know DBH
allmort <- allmort[!is.na(allmort$last_main_census_dbh) & allmort$last_main_census_dbh >= ddiv[1], ]

## Did tree died between now and previous census ?
allmort$died <- grepl("A", allmort$previous_year_status) & grepl("D", allmort$current_year_status)

## did tree went from A to AU between now and previous census ?
allmort$AtoAU <- grepl("A$", allmort$previous_year_status) & grepl("AU", allmort$current_year_status)

## time interval between censused, in year
allmort$timeint <- allmort$timeint_days / 365.25

## calculate amount of biomass that could lost each year if died (use exact time interval for biomass)
allmort$agb_yr <- allmort$last_main_census_agb_Mg/allmort$timeint

## get the dbh classes
allmort$dbh_class <- cut(allmort$last_main_census_dbh, breaks = ddiv, include.lowest = T)


## Identify what grid each tree is in (grid gridsize, not quadtat)
allmort$rowcol <- factor(paste("x",floor(allmort$gx/gridsize),"y",floor(allmort$gy/gridsize),sep="")) #factor(allmort$quadrat) #


## Calculate mortality rates per species ####
mort <- allmort

## get number of initial trees per species and year
ninit <- tapply(mort$previous_year_status, mort[, c("survey_year", "sp")], function(x) sum(grepl("A", x)))

## get number of dead trees per species and year
ndead <- tapply(mort$died, mort[, c("survey_year", "sp")], sum, na.rm = T)

## get number of unhealthy trees per species and year
nAtoAU <- tapply(mort$AtoAU, mort[, c("survey_year", "sp")], sum, na.rm = T)


## get mean time interval per species and year
mntime <- tapply(mort$timeint, mort[, c("survey_year", "sp")], mean, na.rm = T)

## calculate mortality rates per species and year
mortrate <- 100 * (1 - ((ninit - ndead) / ninit) ^ (1 / mntime))

## calculate rate of health decline per species and year
AtoAUrate <- 100 * (1 - ((ninit - nAtoAU) / ninit) ^ (1 / mntime))

## get biomass mortality
agbmort <-  tapply(mort$agb_yr[mort$died], mort[mort$died, c("survey_year", "sp")], sum)/hectar
  
## get biomass health decline
agbAtoAU <-  tapply(mort$agb_yr[mort$AtoAU], mort[mort$AtoAU, c("survey_year", "sp")], sum)/hectar


### bootstrap to get CI ####

#### set up what we need for the bootstrap ####
### count number of tree alive in each grid and each size class
dn <- tapply(mort$previous_year_status, mort[, c("survey_year", "sp", "rowcol")], function(x) sum(grepl("A", x)))
dn[is.na(dn)] <- 0

### sum the dbhs for each grid and each size class
dsums <-  tapply(mort$last_main_census_dbh, mort[, c("survey_year", "sp", "rowcol")], sum, na.rm =T)
dsums[is.na(dsums)] <- 0

### sum n dead and n unheathy in each grid and each size class
msums <- tapply(mort$died, mort[, c("survey_year", "sp", "rowcol")], sum, na.rm = T)
msums[is.na(msums)] <- 0

atoausums<- tapply(mort$AtoAU, mort[, c("survey_year", "sp", "rowcol")], sum, na.rm = T)
atoausums[is.na(atoausums)] <- 0

### sum the time interval in each grid and size class
tsums <-  tapply(mort$timeint, mort[, c("survey_year", "sp", "rowcol")], sum, na.rm = T)
tsums[is.na(tsums)] <- 0

### sum agb tree that died, same for unhealthy
magbsum <- tapply(mort$agb_yr[mort$died], mort[mort$died, c("survey_year", "sp", "rowcol")], sum, na.rm = T)/hectar
magbsum[is.na(magbsum)] <- 0

auagbsum <- tapply(mort$agb_yr[mort$AtoAU], mort[mort$AtoAU, c("survey_year", "sp", "rowcol")], sum, na.rm = T)/hectar
auagbsum[is.na(auagbsum)] <- 0

### get number of grid
ngrid = dim(dn)[3]

#### bootstrap the grids (sample ngrid with replacement) ####
boot_mort <- boot_agbmort <- array(NA, dim = c(dim(dn)[1:2], nreps), 
                   dimnames = c(dimnames(dn)[1:2], list(rep_id = 1:nreps)))

boot_AtoAU <- boot_agbAtoAU <- array(NA, dim = c(dim(dn)[1:2], nreps), 
                                   dimnames = c(dimnames(dn)[1:2], list(rep_id = 1:nreps)))

pb <- progress_bar$new(
  format = paste("Calculating 95%CI [:bar] :percent eta: :eta"),
  clear = FALSE, total = nreps, width = 100)

for(i in 1:nreps) {
  pb$tick()
  whichgrids <- sample(ngrid, ngrid, replace = T)
  
  ## get average dbh per size class
  # apply(dsums[,,whichgrids], c(1,2), sum, na.rm = T) / apply(dn[,,whichgrids],  c(1,2), sum, na.rm = T) 
  
  ## get average mortality for each size class
  ###  100 * (1 - ((ninit - ndead) / ninit) ^ (1 / mntime))
  boot_mort[,,i] = 100 * (1 - ((apply(dn[,,whichgrids], c(1,2), sum, na.rm = T) - apply(msums[,,whichgrids], c(1,2), sum, na.rm =T)) / apply(dn[,,whichgrids],  c(1,2), sum, na.rm = T)) ^ (1 / (apply(tsums[,,whichgrids], c(1,2), sum, na.rm = T) / apply(dn[,,whichgrids],  c(1,2), sum, na.rm = T))))
  
  boot_agbmort[,,i]= apply(magbsum[,,whichgrids], c(1,2), sum, na.rm = T)
  
  boot_AtoAU[,,i] = 100 * (1 - ((apply(dn[,,whichgrids], c(1,2), sum, na.rm = T) - apply(atoausums[,,whichgrids], c(1,2), sum, na.rm =T)) / apply(dn[,,whichgrids],  c(1,2), sum, na.rm = T)) ^ (1 / (apply(tsums[,,whichgrids], c(1,2), sum, na.rm = T) / apply(dn[,,whichgrids],  c(1,2), sum, na.rm = T))))
  
  boot_agbAtoAU[,,i]= apply(auagbsum[,,whichgrids], c(1,2), sum, na.rm = T)
  
  
  ## get average timeint
  # apply(tsums[,,whichgrids], c(1,2), sum, na.rm = T) / apply(dn[,,whichgrids],  c(1,2), sum, na.rm = T) 
}

mort_lci <- apply(boot_mort, c(1,2), quantile, probs = 0.025, na.rm = T)
mort_uci <- apply(boot_mort, c(1,2), quantile, probs = 0.975, na.rm = T)
abgmort_lci <- apply(boot_agbmort, c(1,2), quantile, probs = 0.025, na.rm = T)
abgmort_uci <- apply(boot_agbmort, c(1,2), quantile, probs = 0.975, na.rm = T)


AtoAU_lci <- apply(boot_AtoAU, c(1,2), quantile, probs = 0.025, na.rm = T)
AtoAU_uci <- apply(boot_AtoAU, c(1,2), quantile, probs = 0.975, na.rm = T)
agbAtoAU_lci <- apply(boot_agbAtoAU, c(1,2), quantile, probs = 0.025, na.rm = T)
agbAtoAU_uci <- apply(boot_agbAtoAU, c(1,2), quantile, probs = 0.975, na.rm = T)


### put everything together ####
sp_mortrate <- cbind(variable = "mortality_rates", as.data.frame.table(mortrate, responseName = "estimate"))
sp_mortrate$LCI_95 <- as.data.frame.table(mort_lci)$Freq
sp_mortrate$UCI_95 <- as.data.frame.table(mort_uci)$Freq
sp_mortrate$N0 <- as.data.frame.table(ninit)$Freq
sp_mortrate$n_died<- as.data.frame.table(ndead)$Freq
sp_mortrate$mean_timeint_yr<- as.data.frame.table(mntime)$Freq


sp_agbmort <- cbind(variable = "biomass_mortality", as.data.frame.table(agbmort, responseName = "estimate"))
sp_agbmort$LCI_95 <- as.data.frame.table(abgmort_lci)$Freq
sp_agbmort$UCI_95 <- as.data.frame.table(abgmort_uci)$Freq
sp_agbmort$N0 <- as.data.frame.table(ninit)$Freq
sp_agbmort$n_died<- as.data.frame.table(ndead)$Freq
sp_agbmort$mean_timeint_yr<- as.data.frame.table(mntime)$Freq


sp_mortrate_agbmort <- rbind(sp_mortrate, sp_agbmort)
sp_mortrate_agbmort <- sp_mortrate_agbmort[!is.na(sp_mortrate_agbmort$estimate), ]



sp_AtoAUrate <- cbind(variable = "mortality_rates", as.data.frame.table(AtoAUrate, responseName = "estimate"))
sp_AtoAUrate$LCI_95 <- as.data.frame.table(AtoAU_lci)$Freq
sp_AtoAUrate$UCI_95 <- as.data.frame.table(AtoAU_uci)$Freq
sp_AtoAUrate$N0 <- as.data.frame.table(ninit)$Freq
sp_AtoAUrate$n_died<- as.data.frame.table(nAtoAU)$Freq
sp_AtoAUrate$mean_timeint_yr<- as.data.frame.table(mntime)$Freq


sp_agbAtoAU <- cbind(variable = "biomass_mortality", as.data.frame.table(agbAtoAU, responseName = "estimate"))
sp_agbAtoAU$LCI_95 <- as.data.frame.table(agbAtoAU_lci)$Freq
sp_agbAtoAU$UCI_95 <- as.data.frame.table(agbAtoAU_uci)$Freq
sp_agbAtoAU$N0 <- as.data.frame.table(ninit)$Freq
sp_agbAtoAU$n_died<- as.data.frame.table(nAtoAU)$Freq
sp_agbAtoAU$mean_timeint_yr<- as.data.frame.table(mntime)$Freq


sp_AtoAUrate_agbAtoAU <- rbind(sp_AtoAUrate, sp_agbAtoAU)
sp_AtoAUrate_agbAtoAU <- sp_AtoAUrate_agbAtoAU[!is.na(sp_AtoAUrate_agbAtoAU$estimate), ]


# save ####
write.csv(sp_mortrate_agbmort, file = "R_results/mortrates_and_agbmort_by_species.csv", row.names = F)
write.csv(sp_AtoAUrate_agbAtoAU, file = "R_results/AtoAUrates_and_agbAtoAU_by_species.csv", row.names = F)



#### Calculate mortality rates size classes ####

all_mortrate <- NULL
all_AtoAUrate <- NULL

for(sp in c("All", sort(unique(as.character(allmort$sp))))) {

if(sp == "All")  mort <- allmort
if(sp != "All") mort <- allmort[allmort$sp %in% sp,]

## get mean dbh per size class ####
dbh_init <- tapply(mort$last_main_census_dbh, mort[, c("survey_year", "dbh_class")], mean, na.rm = T)

## get number of initial trees per size class ####
ninit <- tapply(mort$previous_year_status, mort[, c("survey_year", "dbh_class")], function(x) sum(grepl("A", x)))

## get number of dead (and AtoAU) trees per size class ####
ndead <- tapply(mort$died, mort[, c("survey_year", "dbh_class")], sum, na.rm = T)
nAtoAU <-  tapply(mort$AtoAU, mort[, c("survey_year", "dbh_class")], sum, na.rm = T)

## get mean time interval per size class ####
mntime <- tapply(mort$timeint, mort[, c("survey_year", "dbh_class")], mean, na.rm = T)

## calculate mortality (and AtuAU ) rates per size class ####
mortrate <- 100 * (1 - ((ninit - ndead) / ninit) ^ (1 / mntime))
AtoAUrate <- 100 * (1 - ((ninit - nAtoAU) / ninit) ^ (1 / mntime))


### bootstrap to get CI ####

## set up what we need for the bootstrap ####
### count number of tree alive in each grid and each size class
dn <- tapply(mort$previous_year_status, mort[, c("survey_year", "dbh_class", "rowcol")], function(x) sum(grepl("A", x)))
dn[is.na(dn)] <- 0

### sum the dbhs for each grid and each size class
dsums <-  tapply(mort$last_main_census_dbh, mort[, c("survey_year", "dbh_class", "rowcol")], sum, na.rm =T)
dsums[is.na(dsums)] <- 0

### sum n dead in each grid and each size class
msums <- tapply(mort$died, mort[, c("survey_year", "dbh_class", "rowcol")], sum, na.rm = T)
msums[is.na(msums)] <- 0

atoausums<- tapply(mort$AtoAU, mort[, c("survey_year", "dbh_class", "rowcol")], sum, na.rm = T)
atoausums[is.na(atoausums)] <- 0

### sum the time interval in each grid and size class
tsums <-  tapply(mort$timeint, mort[, c("survey_year", "dbh_class", "rowcol")], sum, na.rm = T)
tsums[is.na(tsums)] <- 0

### get number of grid
ngrid = dim(dn)[3]

## bootstrap the grids (sample ngrid with replacement) ####
boot_mort <- boot_AtoAU <- array(NA, dim = c(dim(dn)[1:2], nreps), 
                                 dimnames = c(dimnames(dn)[1:2], list(rep_id = 1:nreps)))

pb <- progress_bar$new(
  format = paste("Calculating 95%CI for mort per size class ", sp, "[:bar] :percent eta: :eta"),
  clear = FALSE, total = nreps, width = 100)

for(i in 1:nreps) {
  pb$tick()
  whichgrids <- sample(ngrid, ngrid, replace = T)
  
  ## get average dbh per size class
  # apply(dsums[,,whichgrids], c(1,2), sum, na.rm = T) / apply(dn[,,whichgrids],  c(1,2), sum, na.rm = T) 
  
  ## get average mortality for each size class
  ###  100 * (1 - ((ninit - ndead) / ninit) ^ (1 / mntime))
  boot_mort[,,i] = 100 * (1 - ((apply(dn[,,whichgrids], c(1,2), sum, na.rm = T) - apply(msums[,,whichgrids], c(1,2), sum, na.rm =T)) / apply(dn[,,whichgrids],  c(1,2), sum, na.rm = T)) ^ (1 / (apply(tsums[,,whichgrids], c(1,2), sum, na.rm = T) / apply(dn[,,whichgrids],  c(1,2), sum, na.rm = T))))
  
  boot_AtoAU[,,i] = 100 * (1 - ((apply(dn[,,whichgrids], c(1,2), sum, na.rm = T) - apply(atoausums[,,whichgrids], c(1,2), sum, na.rm =T)) / apply(dn[,,whichgrids],  c(1,2), sum, na.rm = T)) ^ (1 / (apply(tsums[,,whichgrids], c(1,2), sum, na.rm = T) / apply(dn[,,whichgrids],  c(1,2), sum, na.rm = T))))
  ## get average timeint
  # apply(tsums[,,whichgrids], c(1,2), sum, na.rm = T) / apply(dn[,,whichgrids],  c(1,2), sum, na.rm = T) 
}

mort_lci <- apply(boot_mort, c(1,2), quantile, probs = 0.025, na.rm = T)
mort_uci <- apply(boot_mort, c(1,2), quantile, probs = 0.975, na.rm = T)

AtoAU_lci <- apply(boot_AtoAU, c(1,2), quantile, probs = 0.025, na.rm = T)
AtoAU_uci <- apply(boot_AtoAU, c(1,2), quantile, probs = 0.975, na.rm = T)


# put everything together ####
sp_mortrate <-  cbind(Species = sp, as.data.frame.table(mortrate, responseName = "mortality_rate")) 
sp_mortrate$LCI_95 <- as.data.frame.table(mort_lci)$Freq
sp_mortrate$UCI_95 <- as.data.frame.table(mort_uci)$Freq
sp_mortrate$mean_dbh <- as.data.frame.table(dbh_init)$Freq
sp_mortrate$N0 <- as.data.frame.table(ninit)$Freq
sp_mortrate$n_died<- as.data.frame.table(ndead)$Freq
sp_mortrate$mean_timeint_yr<- as.data.frame.table(mntime)$Freq


sp_AtoAUrate <-  cbind(Species = sp, as.data.frame.table(AtoAUrate, responseName = "AtoAU_rate"))
sp_AtoAUrate$LCI_95 <- as.data.frame.table(AtoAU_lci)$Freq
sp_AtoAUrate$UCI_95 <- as.data.frame.table(mort_uci)$Freq
sp_AtoAUrate$mean_dbh <- as.data.frame.table(AtoAU_uci)$Freq
sp_AtoAUrate$N0 <- as.data.frame.table(ninit)$Freq
sp_AtoAUrate$n_died<- as.data.frame.table(nAtoAU)$Freq
sp_AtoAUrate$mean_timeint_yr<- as.data.frame.table(mntime)$Freq



all_mortrate <- rbind(all_mortrate, sp_mortrate)
all_AtoAUrate <- rbind(all_AtoAUrate, sp_AtoAUrate)

}


# remove NAs ####
all_mortrate <- all_mortrate[!is.na(all_mortrate$mortality_rate), ]
all_AtoAUrate <-  all_AtoAUrate[!is.na(all_AtoAUrate$AtoAU_rate), ]

# save ####
write.csv(all_mortrate, file = "R_results/mortrates_by_size_class.csv", row.names = F)
write.csv(all_AtoAUrate, file = "R_results/AtoAUrate_by_size_class.csv", row.names = F)
