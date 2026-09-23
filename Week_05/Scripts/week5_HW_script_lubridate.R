######## HW Assignment: Data wrangling - Joins & Dates with lubridate ########
### Created by: Linnea Nordli
### Created on: 2026-09-22
### Week 5
####################################

## Load libraries ##
library(tidyverse)
library(here)
library(calecopal)

## Import data ##
# import conductivity data
CondData <- read.csv(here("Week_05", "Data", "CondData.csv"))
# import depth data
DepthData <- read.csv(here("Week_05", "Data", "DepthData.csv"))

## Data Wrangling ##
# convert conductivity date columns to datetime format
CondData |> 
  rename(datetime=date) |> # rename date column to datetime
  mutate(datetime = mdy_hms(datetime)) # convert the datetime column to a datetime format
# convert depth date columns to datetime format
DepthDataClean <- DepthData |> 
  rename(datetime = date) |> # rename date column to datetime
  mutate(datetime = ymd_hms(datetime)) # convert date column to datetime format
# round conductivity datetime column to nearest 10 seconds to match the depth dataset
CondDataClean <- CondData |> 
  rename(datetime =date) |> # rename date column to datetime
  mutate(datetime = mdy_hms(datetime)) |> 
  mutate(datetime = round_date(datetime, "10 secs")) # rename date-rounded-column to nearest 10 sec

## Join the two cleaned dataframes 
# Join conductivity and depth
joined_data <- inner_join(CondDataClean, DepthDataClean) # inner_join keeps only complete matches

## Calculate means ## 
# round the datetime column of joined dataframe to nearest minute and calculate means
means_joined <- joined_data |> # assign means to new dataframe
  mutate(datetime = round_date(datetime, "minute")) |> # round to nearest minute
  group_by(datetime) |> # group by datetime
  summarise(mean_depth = mean(Depth, na.rm = TRUE), # calculate means of the three parameters 
            mean_temp = mean(Temperature, na.rm = TRUE),
            mean_sal = mean(Salinity, na.rm = TRUE))

## Plotting ##
# pivot mean dataframe to long dataset
means_long <- means_joined |> 
  pivot_longer(cols = mean_depth:mean_sal, # select columns between depth and salinity
               names_to = "Variables", # new column with the selected variables 
               values_to = "Values") # new column with respective values

# plot means across timeseries using facet wrap
means_long |> 
  ggplot(mapping = aes(x=datetime, y=Values))+
  geom_line()+ # plot line 
  facet_wrap(~Variables,
             scales="free")+
  theme_bw()+
  labs(x="Time", y="Values", title = "Means Over 4-Hour Timeseries")

# create custom labeller for facet wrap plots
custom_labeller <- as_labeller(c(
  "mean_depth" = "Depth (m)",
  "mean_sal" = "Salinity (psu)",
  "mean_temp" = "Temperature (C)"))

# plot with custom labels for facet wrap plots and modify theme of plot
means_long |> 
  ggplot(mapping = aes(x=datetime, y=Values, color=Variables))+
  geom_line()+
  geom_point()+ # add individual observations as points 
  facet_wrap(~Variables, scales="free", labeller=custom_labeller)+
  theme_bw()+
  theme(plot.title = element_text(size = 20, face = "bold"), axis.title = element_text(size = 17), 
        plot.background = element_rect(fill = "linen"), legend.position = "none", # change background color of plot and remove legend
        axis.text.x = element_text(size = 12), # change tick size of x axis
        axis.text.y = element_text(size = 12), # change tick size of y axis
        strip.text = element_text(size = 13))+ # change font size of individual plot titles 
  labs(x="Time", y="Values", title = "Means of Depth, Salinity, and Temperature Over 4-Hour Timeseries")+
  scale_color_manual(values=cal_palette("kelp2")) + # add color palette
  scale_x_datetime(expand = expansion(mult = 0.1)) # expand x-axis limits by 10% space around the data range so that time values stay within the frames of the plot

## Export plot ##
ggsave(here("Week_05","Outputs","HW_means_plot.png"), width = 10, height = 6)