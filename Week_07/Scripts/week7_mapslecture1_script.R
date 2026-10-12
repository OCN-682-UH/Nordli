######## Lecture Notes: Intro to Maps Part 1 ########
### Created by: Linnea Nordli
### Created on: 2026-10-06
### Week 7
####################################


## Load libraries ##
library(tidyverse)
library(here)
library(maps)
library(mapdata)
library(mapproj)

## Import data ##
# Population in California by county
popdata <- read_csv(here("Week_07", "Data", "CApopdata.csv"))

# Seastar counts at field sites in California
stars <- read_csv(here("Week_07", "Data", "stars.csv"))

# create a new dataframe
world <- map_data("world")
# country
usa <- map_data("usa")
head(usa)

italy <- map_data("italy")
head(italy)

norway <- map_data("norway") # FIGURE THIS OUT
# states 
states <- map_data("state")
head(states)
# counties
counties <- map_data("county")
head(counties)

### PART 1: {maps} package ###
## Start mapping using GGPLOT ## 
# make a map of the world
ggplot() +
  geom_polygon(data = world,
               aes(x = long, y = lat,
                   group = group)) # GROUP IS IMPORTANT
# lines between countries to black
ggplot() +
  geom_polygon(data = world,
               aes(x = long, y = lat,
                   group = group),
               color = "black")
# color fill by region
ggplot() +
  geom_polygon(data = world,
               aes(x = long, y = lat,
                   group = group,
                   fill = region),
               color = "black") +
  guides(fill = "none") # legend would be bigger than plot bc so many countries
# change theme
ggplot() +
  geom_polygon(data = world,
               aes(x = long, y = lat,
                   group = group,
                   fill = region),
               color = "black") +
  guides(fill = "none") +
  theme_minimal()
# color background blue 
ggplot() +
  geom_polygon(data = world,
               aes(x = long, y = lat,
                   group = group,
                   fill = region),
               color = "black") +
  theme_minimal() +
  guides(fill = "none") +
  theme(panel.background =
          element_rect(fill = "lightblue"))
# change map projection bc earth is not flat, more options in mapproj
ggplot() +
  geom_polygon(data = world,
               aes(x = long, y = lat,
                   group = group,
                   fill = region),
               color = "black") +
  theme_minimal() +
  guides(fill = "none") +
  theme(panel.background =
          element_rect(fill = "lightblue")) +
  coord_map(projection = "mercator", #using a Mercator projection
            xlim = c(-180, 180)) #set limits, whole longitude 
# try sinusoidal projection
ggplot() +
  geom_polygon(data = world,
               aes(x = long, y = lat,
                   group = group,
                   fill = region),
               color = "black") +
  theme_minimal() +
  guides(fill = "none") +
  theme(panel.background =
          element_rect(fill = "lightblue")) +
  coord_map(projection = "sinusoidal", #using a sinusoidal projection
            xlim = c(-180, 180)) #set limits, whole longitude 

## Make of map of just California 
# Filter the states dataset just to CA
CA_data <- states |>
  filter(region == "california")
# start mapping
ggplot() +
  geom_polygon(data = CA_data,
               aes(x=long, y=lat,
                   group = group),
               fill="pink", # fill is the region, in this case CA
               color="black")+ # color is the outline
  theme_bw()+
  theme(panel.background = element_rect(fill = "lightblue")) # add blue background

## map CA with counties
# use the counties dataframe bc it also has state
CA_county_data <- counties |>
  filter(region == "california")
# plot CA with counties
ggplot() +
  geom_polygon(data = CA_county_data,
               aes(x = long, y = lat,
                   group = group,
                   fill = region), # INSIDE AES !
               color = "black") +
  theme_minimal() +
  guides(fill = "none") +
  theme(panel.background =
          element_rect(fill = "lightblue"))


## Plot the population of every county in California ##

# First, we need county-level polygons and join them to our population dataset.
# look at the two datasets
head(counties)[1:3, ]

head(popdata)

# Wrangle data# 
# Make column names match before joining
CApop_county <- popdata |>
  select("subregion" = County, Population) |> # rename to match counties
  inner_join(counties) |>
  filter(region == "california") # some county names repeat in other states

# map of CA pop by county
ggplot() +
  geom_polygon(data = CApop_county,
               aes(x = long, y = lat,
                   group = group,
                   fill = Population), # default colors are too similar due to similar populations in counties
               color = "black") +
  coord_map() +
  theme_void() # remove background

# log scale for easier interpretation of colors 
ggplot() +
  geom_polygon(data = CApop_county,
               aes(x = long, y = lat,
                   group = group,
                   fill = Population),
               color = "black") +
  coord_map() +
  theme_void() +
  scale_fill_gradient(transform = "log10") # transform = “log10” compresses the color scale so that less-populated counties are still visible alongside Los Angeles.

# add a layer of stars
head(stars) #The stars dataset has seastar counts (per m²) at field sites in California.
ggplot() +
  geom_polygon(data = CApop_county,
               aes(x = long, y = lat,
                   group = group,
                   fill = Population),
               color = "black") +
  geom_point(data = stars,
             aes(x = long,
                 y = lat)) + # stars different geom than polygon
  coord_map() +
  theme_void() +
  scale_fill_gradient(transform = "log10")

# change Size Points by Seastar Count
ggplot() +
  geom_polygon(data = CApop_county,
               aes(x = long, y = lat,
                   group = group,
                   fill = Population),
               color = "black") +
  geom_point(data = stars,
             aes(x = long, y = lat,
                 size = star_no)) + # inside aes from seastar count, outside if just general size of points
  coord_map() +
  theme_void() +
  scale_fill_gradient(transform = "log10")

# better legend labels
ggplot() +
  geom_polygon(data = CApop_county,
               aes(x = long, y = lat,
                   group = group,
                   fill = Population),
               color = "black") +
  geom_point(data = stars,
             aes(x = long, y = lat,
                 size = star_no)) +
  coord_map() +
  theme_void() +
  scale_fill_gradient(transform = "log10") +
  labs(size = "# stars/m²") 

# save plot
ggsave(here("Week_07", "Outputs", "CApop.pdf")) #no spec reason for pdf format, 

###############################################################

### Part 2: package {sf} ###
## means simple features
# install.packages(c("sf", "rnaturalearth", "rnaturalearthdata")) # install ONCE

# load
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

# create df
world_sf <- ne_countries(scale = "medium", # size 
                         returnclass = "sf") # sf object to work w sf package

# start mapping
ggplot(world_sf) +
  geom_sf(fill = "lightgray",
          color = "white") + # don't need to specify x and y nor group
  theme_void()

# change projections with coord_sf()
ggplot(world_sf) +
  geom_sf(fill = "lightgray", color = "white") +
  coord_sf(crs = "+proj=robin") + # Robinson projection, can also use numbers instead for CRS GPS and Web
  theme_void() +
  labs(title = "Robinson Projection")

## R awesome package
# remotes::install_github("R-CoderDotCom/ggdogs@main") #install once
library(ggdogs)
ggplot(mtcars) +
  geom_dog(aes(mpg, wt), dog = "pug", size = 5)
