######## In-Class Assignment: intro to dplyr ########
### Created by: Linnea Nordli
### Created on: 2026-09-15
### Week 4
####################################

## load libraries
library(palmerpenguins)
library(tidyverse)
library(here)

## Load data and data analysis ##
# dataframe is in palmerpenguin package
glimpse(penguins)
head(penguins) # look at variables in dataframe

## Filter the dataframe in different ways using FILTER function ##
filter(penguins, sex=="female") # filters for ONLY female penguins

filter(penguins, year==2008) # filter penguins only measured in the year 2008, working w numbers don't need quotes

filter(penguins, body_mass_g>5000) #penguins with body mass greater than 5000g

filter(penguins, sex=="female", body_mass_g>5000) # females also greater than 5000g

filter(penguins, year==2008 | year==2009) # collected in EITHER 2008 or 2009
penguins|>
  filter(year %in% c(2008,2009)) #same filtering using piping and %in% and grouping

filter(penguins, island!="Dream") # penguins NOT from the island Dream

filter(penguins, species %in% c("Adelie","Gentoo")) # penguins in the species Adelie and Gentoo 
filter(penguins, species == "Adelie" | species=="Gentoo") # or lke this

## using the MUTATE function to add new columns ##
mutate(.data = penguins,
       body_mass_kg = body_mass_g / 1000) # add a new column converting body mass in g to kg
mutate(.data = penguins,
       body_mass_kg = body_mass_g / 1000,
       bill_length_depth = bill_length_mm / bill_depth_mm) # change multiple columns at once
# mutate with IF_ELSE
mutate(.data = penguins,
       after_2008 = if_else(year > 2008, "After 2008", "Before 2008")) # mutate with IF_ELSE
#practice Qs in class
mutate(penguins,
       body_mass_flipper_length = body_mass_g + flipper_length_mm) # combine flipper length and body mass
mutate(penguins,
       body_mass_size = if_else(body_mass_g > 4000, "Big", "Small")) # create new column where body mass is greater than 4000 is labeled big, and less is small
penguins |> 
  mutate(body_mass_size = if_else(body_mass_g > 4000, "Big", "Small")) # SAME as above create new column where body mass is greater than 4000 is labeled big, and less is small, but using piping 
# can also combine these two into the mutate ()

## learning how to PIPE ##
# When you use |> the dataframe carries over so you don’t need to write it out anymore.
penguins |>
  filter(sex == "female") |>
  mutate(log_mass = log(body_mass_g)) # Filter only female penguins and add a new column that calculates the log body mass.
# pipe into a ggplot
penguins |>
  drop_na(sex) |>
  ggplot(aes(x = sex, y = flipper_length_mm)) +
  geom_boxplot()

## Select certain columns to remain in the dataframe using SELECT function ##
 
## SUMMARISE
# create new tables
penguins |>
  summarise(mean_flipper = mean(flipper_length_mm, na.rm = TRUE))

penguins |>
  summarise(mean_flipper = mean(flipper_length_mm, na.rm = TRUE),
            min_flipper  = min(flipper_length_mm, na.rm = TRUE))
# use group_by within summarise()
penguins |>
  group_by(island) |>
  summarise(mean_bill_length = mean(bill_length_mm, na.rm = TRUE),
            max_bill_length  = max(bill_length_mm, na.rm = TRUE),
            n                = n())
# group by both and sex
penguins |>
  group_by(island, sex) |>
  summarise(mean_bill_length = mean(bill_length_mm, na.rm = TRUE),
            max_bill_length  = max(bill_length_mm, na.rm = TRUE))


## COUNTS ##
#count() is a quick shortcut for counting rows per group — no need for group_by() + summarise().
penguins |>
  count(species)
# multiple variables
penguins |>
  count(species, island)


## REMOVE NAs ##
# drop_na() drops rows with NAs from specific column
penguins |>
  drop_na(sex) # drop all rows missing data on sex
#Drop all the rows that are missing data on sex, then calculate mean bill length by island and sex.
penguins |>
  drop_na(sex) |>
  group_by(island, sex) |>
  summarise(mean_bill_length = mean(bill_length_mm, na.rm = TRUE))


# dad joke package
library(dadjokeapi)
groan() # dad joke package
