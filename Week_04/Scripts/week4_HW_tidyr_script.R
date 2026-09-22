######## HW Assignment: Data wrangling - Tidyr ########
### Created by: Linnea Nordli
### Created on: 2026-09-21
### Week 4
####################################

## Load libraries ##
library(here)
library(tidyverse)

## Import Dataset and Data Dictionary ##
chem_dd <- read.csv(here("Week_04", "Data", "chem_data_dictionary.csv"))
chem_data <- read.csv(here("Week_04", "Data", "chemicaldata_maunalua.csv"))

## Explore data ##
glimpse(chem_data) # includes data type
summary(chem_data)

## Data Wrangling ##
# separate day and tide column into two columns
chem_data_clean <- chem_data |> 
  drop_na() |> # drop NAs from entire dataframe
  separate_wider_delim(cols = Tide_time,
                       delim = "_",
                       names = c("Tide","Time")) #concatenate when more than one value

view(chem_data_clean) # verify correct separation

# filter out data only for the Black Point collection site
BP_data <- chem_data_clean |> 
  filter(Site=="BP")

# pivot from wide to long dataset
BP_data_long <- BP_data |> 
  pivot_longer(cols = Temp_in:percent_sgd, # select columns between temp and %sgd
               names_to = "Variables", # new column with the selected variables 
               values_to = "Values") # new column with respective values

## Data analysis of summary statistics ##
# calculate mean, variance, standard deviation for all variables by time and tide
# export to csv file
BP_data_long |>
  group_by(Variables, Time, Tide) |>
  summarise(Param_means = mean(Values, na.rm = TRUE),
            Param_vars  = var(Values,  na.rm = TRUE),
            Param_sd = sd(Values, na.rm = TRUE)) |> 
  pivot_wider(names_from = Variables,
              values_from = c(Param_means,Param_vars, Param_sd)) |>  # pivot back to wide
  write_csv(here("Week_04","Outputs","HW_summary.csv")) # export summary stats as excel

## Plotting ##
# plot violin charts with facet wrap of all parameters by tide
BP_data_long |> 
  ggplot(mapping = aes(x=Tide, y=Values, fill = Tide, group = Tide))+
  geom_violin() +
  labs(x="Tide", y="Values", #label axes and plot
title = "Biogeochemistry Parameters",
subtitle = "For Black Point, O'ahu, Hawai'i",
caption = "Source: Nyssa Silbiger")+
  facet_wrap(~Variables,
             scales="free")+
  theme_bw()+
  theme(legend.position = "none", plot.title = element_text(size = 20, face = "bold"), axis.title = element_text(size = 17))

# create keys to label individual plots
custom_labeller <- as_labeller(c(
  "NN"="Nitrite and Nitrate (umol/L)",
  "percent_sgd"="Submarine Groundwater Discharge (%)",
  "pH"="pH",
  "Phosphate"="Phosphate (umol/L)",
  "Salinity"="Salinity",
  "Silicate"="Silicate (umol/L)",
  "TA"="Total Alkalinity (umol/Kg)",
  "Temp_in"="Temperature in situ (C)"))

# incorporate modified titles of individual plots
BP_data_long |> 
  ggplot(mapping = aes(x=Tide, y=Values, fill = Tide, group = Tide))+
  geom_violin() +
  labs(x="Tide", y="Values",
       title = "Biogeochemistry Parameters",
       subtitle = "For Black Point, O'ahu, Hawai'i",
       caption = "Source: Nyssa Silbiger")+
  facet_wrap(~Variables,
             scales="free", labeller=custom_labeller)+ #assign labels to custom labeller
  theme_bw()+
  theme(legend.position = "none", plot.title = element_text(size = 20, face = "bold"), axis.title = element_text(size = 17))

## Export plot 
ggsave(here("Week_04","Outputs","HW_plot_chemdata.png"), width = 9, height = 6)
