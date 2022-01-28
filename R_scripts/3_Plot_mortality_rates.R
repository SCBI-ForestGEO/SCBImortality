#############################################################################
# Purpose: plot calculated Mortality and Biomass mortality
# Developped by: Valentine Herrmann - HerrmannV@si.edu
# R version 4.0.3 (2020-10-10)
##########################################################################

# Clean environment ####
rm(list = ls())

# Set working directory ####
setwd(".")

# Load libraries ####
library(ggplot2)

# load data ####
mortrate_by_size_class <- read.csv("R_results//mortrates_by_size_class.csv")
sp_mortrate_agbmort <- read.csv("R_results/mortrates_and_agbmort_by_species.csv")


# barplot for species mortality ####

## only keep when N0 is > ncut
sp_mortrate_agbmort <- sp_mortrate_agbmort[sp_mortrate_agbmort$N0>100, ]

## keep year as factor
sp_mortrate_agbmort$survey_year <- factor(sp_mortrate_agbmort$survey_year)

## orer variables
sp_mortrate_agbmort$variable <- factor(sp_mortrate_agbmort$variable, levels = unique(sp_mortrate_agbmort$variable), labels = c('Mortality ~ Rate ~ ("%" ~y ^ {-1})', 'Biomass ~ mortality ~(Mg~C~ha^{-1}~y^{-1})'))



  ggplot(sp_mortrate_agbmort, aes(x = sp, y = estimate, fill= survey_year)) + 
  geom_bar(stat = "identity", position = position_dodge(width = .75)) + 
  geom_linerange(aes(ymin = LCI_95, ymax = UCI_95), position = position_dodge(width = .75)) + 
  labs(x = expression("Species"), 
       fill = "Survey Year\n(n> 100 & dbh > 100 mm)") +
  scale_y_continuous(expand = c(0, 0)) + 
  theme_classic() +
  facet_wrap(~variable, ncol = 1,scales = "free_y",
             strip.position = "left", 
             labeller = label_parsed) +
                                           # 'expression(Mortality ~ Rate ~ ("%" ~y ^ {-1}))', biomass_mortality = "Voltage (V)") ) )  +
  ylab(NULL)+
    theme(strip.background = element_blank(),
          strip.placement = "outside")


ggsave("R_results/Barplots_MortalityRates_and_BiomassMortality.tiff")


# plot mortality by size class ####

ggplot(mortrate_by_size_class[mortrate_by_size_class$survey_year %in% c(2014:2021), ], aes(x = mean_dbh, y = mortality_rate, col = survey_year)) +
  geom_point() +
  geom_linerange(aes(ymin = LCI_95, ymax = UCI_95)) +
  scale_x_log10() + scale_y_log10() +
  theme_classic()
  
