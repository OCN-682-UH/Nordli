######## Intro to Plotting: ggplot ########
### Created by: Linnea Nordli
### Created on: 2026-09-11
### Updated on: 
### Week 3 
####################################

# Install packages
# install.packages("palmerpenguins")

# Load packages
library(palmerpenguins)
library(tidyverse)

# view data
glimpse(penguins)

# Create Plots step by step
ggplot(data = penguins) # starting point

ggplot(data = penguins, 
       mapping = aes(x=bill_depth_mm)) # map bill depth to X-axis

ggplot(data = penguins, 
       mapping = aes(x=bill_depth_mm,
                     y=bill_length_mm)) # map bill length to Y-axis

ggplot(data = penguins, 
       mapping = aes(x=bill_depth_mm,
                     y=bill_length_mm))+
  geom_point() # represents each obs with a point

ggplot(data = penguins, 
       mapping = aes(x=bill_depth_mm,
                     y=bill_length_mm,
                     color=species))+ #map species with color
  geom_point()

ggplot(data = penguins, 
       mapping = aes(x=bill_depth_mm,
                     y=bill_length_mm,
                     color=species))+
  geom_point()+
  labs(title = "Bill depth and lenght",
       subtitle = "Dimensions for Adelie, Chinstrap, and Gentoo Penguins",
       x="Bill depth (mm)", y="Bill length (mm)") # add title and subtitle to plot, and axes labels

ggplot(data = penguins, 
       mapping = aes(x=bill_depth_mm,
                     y=bill_length_mm,
                     color=species))+
  geom_point()+
  labs(title = "Bill depth and lenght",
       subtitle = "Dimensions for Adelie, Chinstrap, and Gentoo Penguins",
       x="Bill depth (mm)", y="Bill length (mm)",
       color="Species",
       caption = "Source: Palmer Station LTER / palmerpenguins package") # label legend and add caption for source of data

ggplot(data = penguins, 
       mapping = aes(x=bill_depth_mm,
                     y=bill_length_mm,
                     color=species))+
  geom_point()+
  labs(title = "Bill depth and lenght",
       subtitle = "Dimensions for Adelie, Chinstrap, and Gentoo Penguins",
       x="Bill depth (mm)", y="Bill length (mm)",
       color="Species",
       caption = "Source: Palmer Station LTER / palmerpenguins package")+
  scale_color_viridis_d() # add color blind discrete color scale
