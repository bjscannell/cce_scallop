
# Setting up dataframes ---------------------------------------------------

library(tidyverse)
library(pscl)
library(lme4)
library(DHARMa)
library(lmtest)
library(ggbeeswarm)
library(GLMMadaptive)
library(ggalt)
library(segmented)

#Scallop Population Data Import
raw <- read.csv("D:/Projects/ScallopDieOff/ScallopDensities.csv")


#clean data
Scallop <- raw %>%
  dplyr::select(date, year, season, embayment, site, trans = transect_repl, adults = ad_yr_rawcount, juveniles = juv_yr_density.1) %>%
  filter(!is.na(adults)) %>%
  mutate(adults = as.integer(adults), 
         date = as.Date(date, format = "%d-%b-%y"),
         year = year-2000,
         period = 
           case_when(
             year <= 18 ~ "Before",
             year > 18 ~ "After"))

# Scallop Change Data Import
rawDelta <- read.csv("D:/Projects/ScallopDieOff/ScallopChange.csv")

Delta <- rawDelta %>%
  dplyr::select(Embayment, deltadensity, year) %>%
  mutate(year = year - 2000, period = 
           case_when(
             year <= 18 ~ "Before",
             year > 18 ~ "After")) %>%
  group_by(Embayment, period) %>%
  summarise(n = mean(deltadensity))

#Scallop Spat
rawSpatBefore <- read.csv("D:/Projects/ScallopDieOff/Spat_periodone.csv")
rawSpatAfter <- read.csv("D:/Projects/ScallopDieOff/Spat_periodtwo.csv")

SpatBefore <- rawSpatBefore %>%
  dplyr::select(year, site, spat, spattrans)
SpatAfter <- rawSpatAfter %>%
  dplyr::select(year, site, spat, spattrans)
SpatMaster <- rbind(SpatBefore, SpatAfter)
