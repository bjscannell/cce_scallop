
# Setting up dataframes ---------------------------------------------------

library(tidyverse)
library(pscl)
library(lme4)
library(DHARMa)
library(lmtest)
library(GLMMadaptive)
library(segmented)
#import data
raw <- read.csv("ScallopDensities.csv")


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