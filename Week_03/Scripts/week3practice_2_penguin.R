######## Notes from Lecture: Plotting 2; plot penguin data ########
### Created by: Linnea Nordli
### Created on: 2026-09-13
### Week 3 
####################################

## install packages ##
# install.packages("pak") # for packages in development from GitHub
# install.packages('devtools') for packages in development from GitHub
# pak::pkg_install("dill/beyonce") #Beyonce color palette
# devtools::install_github("an-bui/calecopal") #Colors of California palette

## Load libraries ##
library(pak)
library(devtools)
library(palmerpenguins)
library(tidyverse)
library(here)
library(beyonce)
library(calecopal)
library(ggthemes)

## view data ##
glimpse(penguins)
unique(penguins$species) # view unique values in species
unique(penguins$island) # view values in island

## start plotting ##
ggplot(data=penguins, 
       mapping = aes(x=bill_depth_mm ,y=bill_length_mm))+
  geom_point()+
  labs(x="Bill Depth (mm)", y="Bill Length (mm)") #scatter plot with axes labels

# add best fit line (geom_smooth) and make it a linear model (method=)
ggplot(data=penguins, 
       mapping = aes(x=bill_depth_mm ,y=bill_length_mm))+
  geom_point()+
  geom_smooth(method = lm)+
  labs(x="Bill Depth (mm)", y="Bill Length (mm)")
# group lm by species and map species with color
ggplot(data=penguins, 
       mapping = aes(x=bill_depth_mm ,y=bill_length_mm,
       group=species,
       color=species))+
  geom_point()+
  geom_smooth(method = lm)+
  labs(x="Bill Depth (mm)", y="Bill Length (mm)")+
         scale_colour_viridis_d()
# adding Colors of California color palette 
ggplot(data=penguins, 
       mapping = aes(x=bill_depth_mm ,y=bill_length_mm,
                     group=species,
                     color=species))+
  geom_point()+
  geom_smooth(method = lm)+
  labs(x="Bill Depth (mm)", y="Bill Length (mm)")+
  scale_color_manual(values = cal_palette("kelp2"))
# adding theme
ggplot(data=penguins, 
       mapping = aes(x=bill_depth_mm ,y=bill_length_mm,
                     group=species,
                     color=species))+
  geom_point()+
  geom_smooth(method = lm)+
  labs(x="Bill Depth (mm)", y="Bill Length (mm)")+
  scale_color_manual(values = cal_palette("kelp2"))+
  theme_bw()+
  theme(axis.title = element_text(size = 20))

# playing with theme()
# adding border around the legend, changing size of legend texts, and plot background color
ggplot(data=penguins, 
       mapping = aes(x=bill_depth_mm ,y=bill_length_mm,
                     group=species,
                     color=species))+
  geom_point()+
  geom_smooth(method = lm)+
  labs(x="Bill Depth (mm)", y="Bill Length (mm)",color="Species")+
  scale_color_manual(values = cal_palette("kelp2"))+
  theme_bw()+
  theme(axis.title = element_text(size = 20),legend.background = element_rect(color = "black", linewidth = 0.5),legend.text = element_text(size = 12),
        legend.title = element_text(size = 14), panel.background = element_rect(fill = "linen"))

# save plot, adjust format size to 7x5in  
ggsave(here("Week_03","Outputs","practice_penguin.png"),
       width = 7, height=5)
# assign plot to object
practiceplot <- ggplot(data=penguins, 
                       mapping = aes(x=bill_depth_mm ,y=bill_length_mm,
                                     group=species,
                                     color=species))+
  geom_point()+
  geom_smooth(method = lm)+
  labs(x="Bill Depth (mm)", y="Bill Length (mm)",color="Species")+
  scale_color_manual(values = cal_palette("kelp2"))+
  theme_bw()+
  theme(axis.title = element_text(size = 20),legend.background = element_rect(color = "black", linewidth = 0.5),legend.text = element_text(size = 12),
        legend.title = element_text(size = 14), panel.background = element_rect(fill = "linen"))
