### This is my first script in MBIO 612. I am learning to import data ###
### Created by: Linnea Nordli
### Created on 2026-09-04
### Week 2 Assignment 
#################################################

## Install packages ##
install.packages("here")
## Load packages ##
library(here)
library(tidyverse)

## Import data ##
WeightData <- read.csv(here("Week_02","Data", "weightdata.csv"))

## Data analysis ##
head(WeightData) # look at top 6 entries
tail(WeightData) # look at bottom 6 entries
view(WeightData) # look at entire dataframe