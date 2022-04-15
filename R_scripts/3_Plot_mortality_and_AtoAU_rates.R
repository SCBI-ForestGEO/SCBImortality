#############################################################################
# Purpose: plot calculated Mortality and Biomass mortality (also A to AU rates)
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
mortrate_by_size_class <- read.csv("R_results/mortrates_by_size_class.csv")
sp_mortrate_agbmort <- read.csv("R_results/mortrates_and_agbmort_by_species.csv")


AtoAUrate_by_size_class <- read.csv("R_results/AtoAUrate_by_size_class.csv")
sp_AtoAUrate_agbAtoAUrate <- read.csv("R_results/AtoAUrates_and_agbAtoAU_by_species.csv")


# barplot for species mortality and health decline ####

## only keep when N0 is > ncut
ncut = 100
sp_mortrate_agbmort <- sp_mortrate_agbmort[sp_mortrate_agbmort$N0 > ncut, ]
sp_AtoAUrate_agbAtoAUrate <- sp_AtoAUrate_agbAtoAUrate[sp_AtoAUrate_agbAtoAUrate$N0 > ncut,]
  
## keep year as factor
sp_mortrate_agbmort$survey_year <- factor(sp_mortrate_agbmort$survey_year)
sp_AtoAUrate_agbAtoAUrate$survey_year <- factor(sp_AtoAUrate_agbAtoAUrate$survey_year)

## order variables
sp_mortrate_agbmort$variable <- factor(sp_mortrate_agbmort$variable, levels = unique(sp_mortrate_agbmort$variable), labels = c('Mortality ~ rate ~ ("%" ~y ^ {-1})', 'Biomass ~ mortality ~(Mg~C~ha^{-1}~y^{-1})'))

sp_AtoAUrate_agbAtoAUrate$variable <- factor(sp_AtoAUrate_agbAtoAUrate$variable, levels = unique(sp_AtoAUrate_agbAtoAUrate$variable), labels = c('Health ~ decline ~ rate ~ ("%" ~y ^ {-1})', 'Biomass ~ health ~ decline ~(Mg~C~ha^{-1}~y^{-1})'))


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


ggsave("R_results/Barplots_MortalityRates_and_BiomassMortality.tiff", width = 10, height = 7, units = "in")


ggplot(sp_AtoAUrate_agbAtoAUrate, aes(x = sp, y = estimate, fill= survey_year)) + 
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


ggsave("R_results/Barplots_AtuAURates_and_BiomassAtuAU.tiff", width = 10, height = 7, units = "in")


# plot mortality and health decline by size class ####
mortrate_by_size_class$dbh_class <- factor(mortrate_by_size_class$dbh_class, levels = unique(mortrate_by_size_class$dbh_class), ordered = T)
AtoAUrate_by_size_class$dbh_class <- factor(AtoAUrate_by_size_class$dbh_class, levels = unique(AtoAUrate_by_size_class$dbh_class), ordered = T)

ggplot(mortrate_by_size_class[mortrate_by_size_class$survey_year %in% c(2014:2021), ], aes(colour = dbh_class, y = mortality_rate, x = survey_year)) +
  geom_point() +
  geom_linerange(aes(ymin = LCI_95, ymax = UCI_95)) +
  scale_x_log10() + scale_y_log10() +
  theme_classic() +
  facet_wrap(~Species) +
  ylab(bquote(Mortality ~ rate ~ ("%" ~y ^ {-1}))) +
  xlab("Survey year")
  

ggsave("R_results/MortalityRates_per_sp_and_size_classes.tiff", width = 12, height = 10, units = "in")



ggplot(AtoAUrate_by_size_class[AtoAUrate_by_size_class$survey_year %in% c(2014:2021), ], aes(colour = dbh_class, y = AtoAU_rate, x = survey_year)) +
  geom_point() +
  geom_linerange(aes(ymin = LCI_95, ymax = UCI_95)) +
  scale_x_log10() + scale_y_log10() +
  theme_classic() +
  facet_wrap(~Species) +
  ylab(bquote(Health ~ decline ~ rate ~ ("%" ~y ^ {-1})))+
  xlab("Survey year")


ggsave("R_results/AtoAURates_per_sp_and_size_classes.tiff", width = 12, height = 10, units = "in")
