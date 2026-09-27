###### Notes from Online Lecture: Advanced Plotting Techniques ######
### Created by: Linnea Nordli
### Created on: 2026-09-27
### Week 5
####################################

## Install new packages #
# install.packages("patchwork")  # for bringing plots together
# install.packages("ggrepel")    # for repelling labels
# install.packages("gganimate")  # smooth animations
# install.packages("gifski")     # for saving gifs
# install.packages("plotly")     # for interactive animations
# install.packages("magick")     # for images

## Load libraries ##
library(patchwork)
library(ggrepel)
library(gganimate)
library(gifski)
library(plotly)
library(magick)
library(tidyverse)
library(palmerpenguins) # palmer penguins dataset
library(here)

## Part 1: patchwork ##
# makes it easy to bring your plots together with simple operations.
# create first plot bill length across body mass, grouped by species
p1 <- penguins |>
  ggplot(aes(x = body_mass_g, 
             y = bill_length_mm, 
             color = species)) +
  geom_point()
# view plot
p1
# create second plot body mass across sex, grouped by species
p2 <- penguins |>
  ggplot(aes(x = sex, 
             y = body_mass_g, 
             color = species)) +
  geom_jitter(width = 0.2)
# view plot
p2
# combine the two plots with +
p1 + p2 +
  plot_layout(guides = 'collect') + #collect legends to one
  plot_annotation(tag_levels = 'A') # add labels (a/b) to plots
# stack plots vertically with /
p1 / p2 +
  plot_layout(guides = 'collect') +
  plot_annotation(tag_levels = 'A')

## Part 2: ggrepel ##
# makes it easy to add clear, non-overlapping labels to your plots
# use the built-in mtcars dataset (car specifications)
head(mtcars)
# plot
ggplot(mtcars, aes(x = wt, 
                   y = mpg, 
                   label = rownames(mtcars))) +
  geom_text() +
  geom_point(color = 'red')
# labels are ovelapping in previous plot, plot with label repel
ggplot(mtcars, aes(x=wt,
                   y=mpg,
                   label=rownames(mtcars)))+
  geom_text_repel()+ # Repel the labels
  geom_point(color='red')
# Use geom_label_repel() for boxes around labels
ggplot(mtcars, aes(x = wt, 
                   y = mpg, 
                   label = rownames(mtcars))) +
  geom_label_repel() +
  geom_point(color = 'red')

## Part 3: gganimate ## 
# lets you create animations that show data changing over time or groups
# static penguin plot bill depth across body mass, grouped by species
penguins |>
  ggplot(aes(x = body_mass_g, 
             y = bill_depth_mm, 
             color = species)) +
  geom_point()
# add transition
penguins |>
  ggplot(aes(x = body_mass_g, 
             y = bill_depth_mm, 
             color = species)) +
  geom_point() +
  transition_states(
    year,
    transition_length = 2,
    state_length = 1
  )
# Add a dynamic title with labs()
penguins |>
ggplot(aes(x = body_mass_g, 
           y = bill_depth_mm, 
           color = species)) +
  geom_point() +
  transition_states(year, 
                    transition_length = 2, 
                    state_length = 1) +
  labs(title = 'Year: {closest_state}') #The placeholder {closest_state} updates with each frame.
# assign plot transition and prep for GIF save
p<-penguins |> 
  ggplot(aes(x = body_mass_g, 
             y = bill_depth_mm, 
             color = species)) +
  geom_point() +
  transition_states(year, 
                    transition_length = 2, 
                    state_length = 1) +
  ease_aes("sine-in-out") +
  labs(title = 'Year: {closest_state}') 
# save plot as GIF
anim_save(here("Week_05", "Outputs", "penguin_animation.gif"), animation = p)

## Part 4: plotly ##
# creates interactive plots with built-in animation that respond to user input. Perfect for exploratory data analysis
# create an interactive plot
penguins |>
  plot_ly(x = ~body_mass_g,
          y = ~bill_depth_mm,
          color = ~species,
          type = "scatter",
          mode = "markers") |>
  layout(title = "Penguin Body Mass vs Bill Depth",
         xaxis = list(title = "Body Mass (g)"),
         yaxis = list(title = "Bill Depth (mm)"))
# animate by species with frame
penguins |>
  plot_ly(x = ~body_mass_g,
          y = ~bill_depth_mm,
          frame = ~species,
          color = ~species,
          type = "scatter",
          mode = "markers",
          marker = list(size = 8)) |>
  layout(title = "Penguin Characteristics",
         xaxis = list(title = "Body Mass (g)"),
         yaxis = list(title = "Bill Depth (mm)"))

## Part 5: magick ##
# advanced image processing
# magick lets you read, process, and composite images programmatically.
# read an image 
penguin <- image_read("https://pngimg.com/uploads/penguin/pinguin_PNG9.png")
# view image
penguin
# save a plot as image, first create and save plot 
penguinplot<-penguins |>
  ggplot(aes(x = body_mass_g, 
             y = bill_depth_mm, 
             color = species)) +
  geom_point() 
# save
ggsave(here("Week_05", "Outputs", "penguinplot.png"))
# view plot
penguinplot
# composite images 
penplot <- image_read(here("Week_05", "Outputs", "penguinplot.png"))
# add image to plot
out <- image_composite(image = penplot,       composite_image = penguin, offset = "+70+30")
# view
out

# You can combine GIFs with images too:
# read image
pengif <- image_read("https://media3.giphy.com/media/H4uE6w9G1uK4M/giphy.gif")
# add gif, plot, and image
outgif <- image_composite(penplot, pengif, gravity = "center")
# animate
animation <- image_animate(outgif, fps = 10, optimize = TRUE)
# view animation
animation
