# Determine the level of concern for each species, base on this document:
## https://github.com/SCBI-ForestGEO/SCBImortality/blob/main/doc/TreeDiseaseGuide/species_of_concern.md

# clear environment ####
rm(list = ls())

# load libraries ####

# load data ####
mortality_by_species <- read.csv("R_results/mortrates_and_agbmort_by_species.csv")

prop_of_AU <- read.csv("R_results/proportion_AU_trees_per_sp.csv")

species_list <- read.csv("https://raw.githubusercontent.com/SCBI-ForestGEO/SCBI-ForestGEO-Data/master/species_lists/Tree%20ecology/SCBI_ForestGEO_sp_ecology.csv ") # just to get what species we want to fill info with



# BASED ON MORTALITY RATES ONLY

## look at mortality_rates only
mortality_by_species <- mortality_by_species[mortality_by_species$variable %in%"mortality_rates", ]

## number of years the mortality rate (estimate) ≥ 5 % yr-1
n_yr_mort_ovr_5 <- tapply(mortality_by_species$estimate, mortality_by_species$sp, function(x) sum(x>= 5))

## overall trend in mortality rate
pos_trend <-tapply(mortality_by_species$estimate, mortality_by_species$sp,  function(x) {
    m <- lm(x ~ c(1:length(x)))
    return(m$coefficients[[2]] > 0)
  })

## ≥ 5 % yr-1 in the most recent year 
recent_mort_ovr_5 <- tapply(mortality_by_species$estimate, mortality_by_species$sp,  function(x) {
  rev(x)[1] >=5
})

## dominant species  (any year with ≥100 individuals)
dom_sp <- tapply(mortality_by_species$N0, mortality_by_species$sp, function(x) max(x) >= 100)



# get Concern status
High <- n_yr_mort_ovr_5 >1 | (dom_sp & pos_trend & recent_mort_ovr_5)
Medium <- n_yr_mort_ovr_5==1 & !(dom_sp & pos_trend& recent_mort_ovr_5)
Low <- n_yr_mort_ovr_5 == 0

names(High|Medium|Low)[!(High|Medium|Low)] # should be empty


# get a vector of the concern status
mortality_concern_status <- High
mortality_concern_status[] <- NA
mortality_concern_status[High] <- "H"
mortality_concern_status[Medium] <- "M"
mortality_concern_status[Low] <- "L"

table(mortality_concern_status, useNA = "ifany") # should be no NA

# BASED ON UNHEATHY STATUS
## remove NAs

prop_of_AU <- prop_of_AU[!is.na(prop_of_AU$p), ]

## ≥20% of living trees were unhealthy in the latest census
High_AU <- tapply(prop_of_AU$p, prop_of_AU$sp, function(x){
  rev(x)[1] >= 0.2}) 

## ≥10% of living trees were unhealthy in the latest census
Medium_AU <- tapply(prop_of_AU$p, prop_of_AU$sp, function(x){
  rev(x)[1] >= 0.1}) & !High_AU

## <10% of living trees were unhealthy in the latest census
Low_AU  <- tapply(prop_of_AU$p, prop_of_AU$sp, function(x){
  rev(x)[1] < 0.1}) 


names(High_AU|Medium_AU|Low_AU)[!(High_AU|Medium_AU|Low_AU)] # should be empty

# get a vector of the concern status
unhealty_concern_status <- High_AU
unhealty_concern_status[] <- NA
unhealty_concern_status[High_AU] <- "H"
unhealty_concern_status[Medium_AU] <- "M"
unhealty_concern_status[Low_AU] <- "L"

table(unhealty_concern_status, useNA = "ifany") # should be no NA


# put every thing together ####
concern_status <- data.frame(species_list[, c("spcode", "IUCN_red_list_status")], 
           mortality_concern_status = mortality_concern_status[match(species_list$spcode, names(mortality_concern_status))],
           unhealty_concern_status = unhealty_concern_status[match(species_list$spcode, names(unhealty_concern_status))])

## remove when we have NA on the whole line
concern_status <- concern_status[apply(concern_status[,-1], 1, function(x) any(!is.na(x))), ]

View(concern_status)


write.csv(concern_status, "R_results/concern_status.csv", row.names = F)
