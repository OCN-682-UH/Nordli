######## Homework Assignment: plot penguin data ########
### Created by: Linnea Nordli
### Created on: 2026-09-13
### Week 3 
####################################

## Install packages ##
# install.packages("palmerpenguins")
# install.packages('devtools') # for packages in development from GitHub
# devtools::install_github("an-bui/calecopal") #Colors of California palette


## Load libraries ##
library(devtools)
library(palmerpenguins)
library(tidyverse)
library(here)
library(calecopal)
library(ggthemes)
library(dplyr)

## View data ##
glimpse(penguins)
unique(penguins$species) # view unique values in species
unique(penguins$island) # view values in island
view(penguins$body_mass_g)

## Start Plotting ##
# create boxplot, map body mass vs islands, add axes labels
ggplot(data=penguins, 
       mapping = aes(x=island ,y=body_mass_g, fill = island))+
  geom_boxplot()+
  labs(x="Islands", y="Body Mass (g)")

# add jitter for individual observations
ggplot(data=penguins, 
       mapping = aes(x=island ,y=body_mass_g, fill = island))+
  geom_boxplot()+
  geom_jitter(color = "black")
  labs(x="Islands", y="Body Mass (g)")

  # add title and subtitle, and data source caption
ggplot(data=penguins, 
       mapping = aes(x=island ,y=body_mass_g, fill = island))+
  geom_boxplot()+
  geom_jitter(color="black")+
  labs(title = "Body Mass and Islands",
       subtitle = "For Adelie, Chinstrap, and Gentoo Penguins", x="Islands", y="Body Mass (g)", caption = "Source: Palmer Station LTER / palmerpenguins package")

# group by islands
ggplot(data=penguins, 
       mapping = aes(x=island ,y=body_mass_g, fill = island,
                     group = island))+
  geom_boxplot()+
  geom_jitter(color="black")+
  labs(title = "Body Mass and Islands",
       subtitle = "For Adelie, Chinstrap, and Gentoo Penguins", x="Islands", y="Body Mass (g)", caption = "Source: Palmer Station LTER / palmerpenguins package")

# add color palette by using scale_FILL_manual to fill boxplots
ggplot(data=penguins, 
       mapping = aes(x=island ,y=body_mass_g, fill = island,
                     group = island))+
  geom_boxplot()+
  geom_jitter(color="black")+
  labs(title = "Body Mass and Islands",
       subtitle = "For Adelie, Chinstrap, and Gentoo Penguins", x="Islands", y="Body Mass (g)", caption = "Source: Palmer Station LTER / palmerpenguins package")+
  scale_fill_manual(values = cal_palette("kelp2"))

# add bw theme to plot, and customize axes text sizes
ggplot(data=penguins, 
       mapping = aes(x=island ,y=body_mass_g, fill = island,
                     group = island))+
  geom_boxplot()+
  geom_jitter(color="black")+
  labs(title = "Body Mass and Islands",
       subtitle = "For Adelie, Chinstrap, and Gentoo Penguins", x="Islands", y="Body Mass (g)", caption = "Source: Palmer Station LTER / palmerpenguins package")+
  scale_fill_manual(values = cal_palette("kelp2"))+
  theme_bw()+
  theme(axis.title = element_text(size = 20), axis.text.x=element_text(size = 12))

# remove legend and modify sizes of all titles and axes
ggplot(data=penguins, 
       mapping = aes(x=island ,y=body_mass_g, fill = island,
                     group = island))+
  geom_boxplot()+
  geom_jitter(color="black")+
  labs(title = "Body Mass and Islands",
       subtitle = "For Adelie, Chinstrap, and Gentoo Penguins", x="Islands", y="Body Mass (g)", caption = "Source: Palmer Station LTER / palmerpenguins package")+
  scale_fill_manual(values = cal_palette("kelp2"))+
  theme_bw()+
  theme(axis.title = element_text(size = 17), axis.text.x=element_text(size = 12), legend.position = "none", plot.title = element_text(size = 20), plot.subtitle = element_text(size = 15))

# change size of individual jitter observations
ggplot(data=penguins, 
       mapping = aes(x=island ,y=body_mass_g, fill = island,
                     group = island))+
  geom_boxplot()+
  geom_jitter(color="black",size=1.1, alpha = 0.9)+
  labs(title = "Body Mass and Islands",
       subtitle = "For Adelie, Chinstrap, and Gentoo Penguins", x="Islands", y="Body Mass (g)", caption = "Source: Palmer Station LTER / palmerpenguins package")+
  scale_fill_manual(values = cal_palette("kelp2"))+
  theme_bw()+
  theme(axis.title = element_text(size = 17), axis.text.x=element_text(size = 12), legend.position = "none", plot.title = element_text(size = 20), plot.subtitle = element_text(size = 15))

# found big duplicate points that seem to be plotted with both boxplot and jitter, so remove outliers from boxplot since they're already plotted again with jitter
ggplot(data=penguins, 
       mapping = aes(x=island ,y=body_mass_g, fill = island,
                     group = island))+
  geom_boxplot(outlier.shape = NA)+
  geom_jitter(color="black",size=1.1,alpha = 0.7)+ #also add transparency to points
  labs(title = "Body Mass and Islands",
       subtitle = "For Adelie, Chinstrap, and Gentoo Penguins", x="Islands", y="Body Mass (g)", caption = "Source: Palmer Station LTER / palmerpenguins package")+
  scale_fill_manual(values = cal_palette("kelp2"))+
  theme_bw()+
  theme(axis.title = element_text(size = 17,), axis.text.x=element_text(size = 12), legend.position = "none", plot.title = element_text(size = 20, face = "bold"), plot.subtitle = element_text(size = 15), axis.text.y = element_text(size = 11),
        panel.background = element_rect(fill = "linen")) #change y-axis text label size and add background color, and bold plot title

## Save plot
ggsave(here("Week_03","Outputs","HW_plot_penguin.png"),
       width = 8, height=6)

# assign plot to object
body_mass_island_plot <- ggplot(data=penguins, 
   mapping = aes(x=island ,y=body_mass_g, fill = island,group = island))+
  geom_boxplot(outlier.shape = NA)+
  geom_jitter(color="black",size=1.1,alpha = 0.7)+
  labs(title = "Body Mass and Islands",
       subtitle = "For Adelie, Chinstrap, and Gentoo Penguins", x="Islands", y="Body Mass (g)", caption = "Source: Palmer Station LTER / palmerpenguins package")+
  scale_fill_manual(values = cal_palette("kelp2"))+
  theme_bw()+
  theme(axis.title = element_text(size = 17,), axis.text.x=element_text(size = 12), legend.position = "none", plot.title = element_text(size = 20, face = "bold"), plot.subtitle = element_text(size = 15), axis.text.y = element_text(size = 11),
        panel.background = element_rect(fill = "linen"))