#############################################################################
# Purpose: Calculate ratio of unhealthy trees per species
# Developped by: Valentine Herrmann - HerrmannV@si.edu
# R version 4.0.3 (2020-10-10)
##########################################################################

# Clean environment ####
rm(list = ls())

# Set working directory ####
setwd(".")

# Load libraries ####
library(ggplot2)

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


## only keep data for wich we know current year status
allmort <- allmort[!is.na(allmort$current_year_status), ] 

## get the dbh classes
allmort$dbh_class <- cut(allmort$last_main_census_dbh, breaks = ddiv, include.lowest = T)


## for each census and species look at AU/(A+AU)
# see CI of proportion here: https://www.statology.org/confidence-interval-proportion/

propAU <- by(allmort, allmort[, c("sp", "survey_year")], function(x) 
  {
  
  nAorAU <- sum(x$current_year_status %in% c("A", "AU"))
  nAU <- sum(x$current_year_status %in% "AU")
  
  p  <- nAU / nAorAU
  se <- sqrt(p * (1-p) / nAorAU)
  lci <- p - 1.96 * se
  uci<- p + 1.96 * se
  
  return(data.frame(survey_year = x$survey_year[1], sp = x$sp[1], round(data.frame(p, nAU, nAorAU, lci, uci), 2)))
})

propAU <- do.call(rbind, propAU)

# save ####
write.csv(propAU, file = "R_results/proportion_AU_trees_per_sp.csv", row.names = F)


# plot ####

## only keep when N0 is > ncut
ncut = 100
propAU <- propAU[propAU$nAorAU > ncut, ]

## keep year as factor
propAU$survey_year <- factor(propAU$survey_year)

ggplot(propAU, aes(x = sp, y = p, fill= survey_year)) + 
  geom_bar(stat = "identity", position = position_dodge(width = .75)) + 
  geom_linerange(aes(ymin = lci, ymax = uci), position = position_dodge(width = .75)) + 
  labs(x = expression("Species"), 
       fill = "Survey Year\n(n> 100 & dbh > 100 mm)") +
  scale_y_continuous(expand = c(0, 0)) + 
  theme_classic() 
  # 'expression(Mortality ~ Rate ~ ("%" ~y ^ {-1}))', biomass_mortality = "Voltage (V)") ) )  +
  ylab(NULL)


ggsave("R_results/Barplots_Proportion_AU.png", width = 10, height = 7, units = "in")

