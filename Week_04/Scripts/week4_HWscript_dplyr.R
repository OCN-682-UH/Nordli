######## HW Assignment: Data wrangling - dplyr ########
### Created by: Linnea Nordli
### Created on: 2026-09-15
### Week 4
####################################

### Load libraries ###
library(palmerpenguins)
library(tidyverse)
library(here)
library(devtools)
library(calecopal)

### Load data and analysis ###
# dataframe is in palmerpenguin package
glimpse(penguins)
head(penguins) # look at variables in dataframe

###### Part 1: ###### 
### Data Wrangling ###
# calculate mean and variance of body mass by species, island, and sex without any NAs
penguins |>
  group_by(species, island, sex) |>
  summarise(mean_body_mass_g = mean(body_mass_g, na.rm = TRUE),
            var_body_mass_g = var(body_mass_g, na.rm = TRUE)) # still getting NAs in sex 
# filter out NAs in sex 
penguins |>
  filter(!is.na(sex)) |>
  group_by(species, island, sex) |>
  summarise(mean_body_mass_g = mean(body_mass_g, na.rm = TRUE),
            var_body_mass_g = var(body_mass_g, na.rm = TRUE))

###### Part 2: ###### 
### Data Wrangling ###
# filters out (i.e. excludes) male penguins
penguins |> 
  filter(sex=="female") # filters for ONLY female penguins
# then calculate the log body mass
penguins |> 
  filter(sex=="female") |>
  mutate(log_mass = log(body_mass_g))
# then select only the columns for species, island, sex, and log body mass
penguins |> 
  filter(sex=="female") |>
  mutate(log_mass = log(body_mass_g)) |>
  select(species, island, sex, log_mass)
# assign dataframe to object
new_peng <- penguins |> 
  filter(sex=="female") |>
  mutate(log_mass = log(body_mass_g)) |>
  select(species, island, sex, log_mass)

### PLOT using the new dataframe ###
# plot log body mass by species as a boxplot
ggplot(new_peng,
       mapping = aes(x=species, y=log_mass, fill = species, 
                     group = species))+
  geom_boxplot()
# remove outliers
ggplot(new_peng,
       mapping = aes(x=species, y=log_mass, fill = species, group = species))+
  geom_boxplot(outlier.shape = NA)
# add jitter for individual observations
ggplot(new_peng,
       mapping = aes(x=species, y=log_mass, fill = species, group = species))+
  geom_boxplot(outlier.shape = NA)+
  geom_jitter(color="black",size=1.1,alpha = 0.6)
# add axis labels and titles and data source caption
ggplot(new_peng,
       mapping = aes(x=species, y=log_mass, fill = species, group = species))+
  geom_boxplot(outlier.shape = NA)+
  geom_jitter(color="black",size=1.1,alpha = 0.6)+
  labs(x="Species", y="Log Body Mass (g)", 
       title = "Log Body Mass Distribution by Species",
       subtitle = "For Female Adelie, Chinstrap, and Gentoo Penguins",
       caption = "Source: Palmer Station LTER / palmerpenguins package")
# add theme, and modify text sizes
ggplot(new_peng,
       mapping = aes(x=species, y=log_mass, fill = species, group = species))+
  geom_boxplot(outlier.shape = NA)+
  geom_jitter(color="black",size=1.1,alpha = 0.6)+
  labs(x="Species", y="Log Body Mass (g)", 
       title = "Log Body Mass Distribution by Species",
       subtitle = "For Female Adelie, Chinstrap, and Gentoo Penguins",
       caption = "Source: Palmer Station LTER / palmerpenguins package")+
  theme_bw()+
  theme(axis.title = element_text(size = 17), axis.text.x=element_text(size = 12), 
        legend.position = "none",plot.title = element_text(size = 20, face = "bold"), 
        plot.subtitle = element_text(size = 15))
# add color palette from colors of california
ggplot(new_peng,
       mapping = aes(x=species, y=log_mass, fill = species, group = species))+
  geom_boxplot(outlier.shape = NA)+
  geom_jitter(color="black",size=1.1,alpha = 0.6)+
  labs(x="Species", y="Log Body Mass (g)", 
       title = "Log Body Mass Distribution by Species",
       subtitle = "For Female Adelie, Chinstrap, and Gentoo Penguins",
       caption = "Source: Palmer Station LTER / palmerpenguins package")+
  theme_bw()+
  theme(axis.title = element_text(size = 17), axis.text.x=element_text(size = 12), 
        legend.position = "none",plot.title = element_text(size = 20, face = "bold"), 
        plot.subtitle = element_text(size = 15))+
  scale_fill_manual(values=cal_palette("coastaldune2"))
# assign plot to object 
plot_log_bodymass_species <- ggplot(new_peng,
                                    mapping = aes(x=species, y=log_mass, fill = species, group = species))+
  geom_boxplot(outlier.shape = NA)+
  geom_jitter(color="black",size=1.1,alpha = 0.6)+
  labs(x="Species", y="Log Body Mass (g)", 
       title = "Log Body Mass Distribution by Species",
       subtitle = "For Female Adelie, Chinstrap, and Gentoo Penguins",
       caption = "Source: Palmer Station LTER / palmerpenguins package")+
  theme_bw()+
  theme(axis.title = element_text(size = 17), axis.text.x=element_text(size = 12), 
        legend.position = "none",plot.title = element_text(size = 20, face = "bold"), 
        plot.subtitle = element_text(size = 15))+
  scale_fill_manual(values=cal_palette("coastaldune2"))

### Save and export plot as png 
ggsave(here("Week_04", "Outputs", "HW_plot_dplyr_logmass_species.png"), width = 8, height=6)