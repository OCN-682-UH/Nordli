###### Notes from Online Lecture: intro to tidyr using biogeochemistry data from Hawaii ######
### Created by: Linnea Nordli
### Created on: 2026-09-18
### Week 4
####################################

## Load libraries ##
library(here)
library(tidyverse)

## Import Dataset and Data Dictionary ##
chem_dd <- read.csv(here("Week_04", "Data", "chem_data_dictionary.csv"))
chem_data <- read.csv(here("Week_04", "Data", "chemicaldata_maunalua.csv"))
# explore data
glimpse(chem_data) # includes data type
summary(chem_data)
head(chem_data)
# separate day and tide column into two columns
chem_data_clean <- chem_data |> 
  drop_na() |> # drop NAs from entire datafram
  separate_wider_delim(cols = Tide_time,
                       delim = "_",
                       names = c("Tide","Time")) #concatenate when more than one value
# IF want to keep original column 
chem_data_clean <- chem_data |> 
 drop_na() |> # drop NAs from entire datafram
  separate_wider_delim(cols = Tide_time,
                       delim = "_",
                       names = c("Tide","Time"),
                       cols_remove = FALSE) #keep original column
# combine two columns 
chem_data_clean <- chem_data |> 
  drop_na() |> 
  separate_wider_delim(cols = Tide_time,
                       delim = "_",
                       names = c("Tide","Time"),
                       cols_remove = FALSE) |> 
  mutate(Site_Zone = paste(Site, Zone, sep = ".")) #combine site and zone column into one, separating values by .

## Pivoting dataset from wide to long ##
chem_data_long <- chem_data_clean |> 
  pivot_longer(cols = Temp_in:percent_sgd, # select columns to pivot from temp to %sgd
               names_to  = "Variables", # new column for old column names
               values_to = "Values")  # new column for the values)
# calculate mean and variance for all variables at each site
chem_data_long |>
  group_by(Variables, Site) |>
  summarise(Param_means = mean(Values, na.rm = TRUE),
            Param_vars  = var(Values,  na.rm = TRUE))
# calculate mean, variance, standard deviation for all variables by site, zone, and tide 
view(chem_data_long)
chem_data_long |>
  group_by(Variables, Site, Zone, Tide) |>
  summarise(Param_means = mean(Values, na.rm = TRUE),
            Param_vars  = var(Values,  na.rm = TRUE),
            Param_sd = sd(Values, na.rm = TRUE))

## PLOT ## 
# use facet_wrap to create boxplots of every parameter by site
chem_data_long |>
  ggplot(aes(x = Site, y = Values)) + # OPTION: could do x=Tide
  geom_boxplot() +
  facet_wrap(~Variables, #OPTION: Site~Variables if separate by sites and tide on x
             scales="free") # fix axes, releases both so not compared to other parameters' values. use free_y if just release y axis

## Pivot from long to wide dataset
chem_data_wide <- chem_data_long |>
  pivot_wider(names_from  = Variables, #from instead of to
              values_from = Values) #from instead of to

## Full pipeline: summary stats and export
ChemData_clean <- chem_data |> 
  drop_na() |> 
  separate_wider_delim(cols = Tide_time,
                       delim = "_",
                       names = c("Tide","Time"),
                       cols_remove = FALSE)
view(ChemData_clean) # separate day and tide column into two columns, and keep the original column

ChemData_clean <- chem_data |> 
  drop_na() |> 
  separate_wider_delim(cols = Tide_time,
                       delim = "_",
                       names = c("Tide", "Time"),
                       cols_remove = FALSE) |> 
  pivot_longer(cols = Temp_in:percent_sgd,
               names_to = "Variables",
               values_to = "Values")
view(ChemData_clean) # pivot the data longer

ChemData_clean <- chem_data |> 
  drop_na() |> 
  separate_wider_delim(cols = Tide_time,
                       delim = "_",
                       names = c("Tide","Time"),
                       cols_remove = FALSE) |> 
  pivot_longer(cols = Temp_in:percent_sgd,
               names_to = "Variables",
               values_to = "Values") |> 
  group_by(Variables, Site, Time) |> 
  summarise(mean_vals= mean(Values, na.rm = TRUE)) # new column names mean_vals
view(ChemData_clean) #group by Variables, Site, Time and calc means in new table

ChemData_clean <- chem_data |> 
  drop_na() |> 
  separate_wider_delim(cols = Tide_time,
                       delim = "_",
                       names = c("Tide","Time"),
                       cols_remove = FALSE) |> 
  pivot_longer(cols = Temp_in:percent_sgd,
               names_to = "Variables",
               values_to = "Values") |> 
  group_by(Variables, Site, Time) |> 
  summarise(mean_vals= mean(Values, na.rm = TRUE)) |> 
  pivot_wider(names_from = Variables,
              values_from = mean_vals) # pivot back to wide dataset
view(ChemData_clean)

## Export the csv file
ChemData_clean <- chem_data |> 
  drop_na() |> 
  separate_wider_delim(cols = Tide_time,
                       delim = "_",
                       names = c("Tide","Time"),
                       cols_remove = FALSE) |> 
  pivot_longer(cols = Temp_in:percent_sgd,
               names_to = "Variables",
               values_to = "Values") |> 
  group_by(Variables, Site, Time) |> 
  summarise(mean_vals= mean(Values, na.rm = TRUE)) |> 
  pivot_wider(names_from = Variables,
              values_from = mean_vals) |> 
  write_csv(here("Week_04","Outputs", "practice.summary.csv")) # export variable means per site and time in wide dataset

## Totally awesome R package
install.packages("cowsay")
library(cowsay)
say("I love tidy data!", by="cat")
