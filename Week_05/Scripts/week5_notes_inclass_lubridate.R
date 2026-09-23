######## In Class Notes: Data wrangling - joins & dates with lubridate ########
### Created by: Linnea Nordli
### Created on: 2026-09-22
### Week 5
####################################

# load libraries
library(tidyverse)
library(here)


# export data

##### PART 1: JOINS ######
# creating sample tibbles
T1 <- tibble(
  Site.ID = c("A","B","C", "D"),
  Temperature = c(14.1, 16.7, 15.3, 12.8)
)
# create second tibble
T2 <- tibble(
  Site.ID = c("A", "B", "D", "E"),
  pH = c(7.3, 7.8, 8.1, 7.9)
)
# keeps all rows from the left (first) dataframe and adds matching rows from the right:
left_join(T1, T2)

# keeps all rows from the right (second) dataframe:
right_join(T1, T2)

#keeps only rows that exist in both dataframes:
inner_join(T1, T2)

# keep all rows from both dataframes:
full_join(T1, T2)

# keeps rows from the first dataframe where there are matches in the second, but only returns columns from the first:
# nyssa never uses
semi_join(T1, T2)

# returns rows in the first dataframe that do not match the second:
anti_join(T1, T2) # finding missing data

# WHAT IF DIFFERENT COLUMN ID NAMES
# new tibble
T3 <- tibble(
  SiteID = c("A", "B", "C", "D"), # note different SiteID
  Chlorophyll = c(2.3, 3.1, 1.9, 2.8)
)
# joining by multiple columns
T4 <- tibble(
  Site.ID = c("A", "A", "B", "B"),
  Year = c(2020, 2021, 2020, 2021),
  Biomass = c(12.5, 15.3, 18.2, 16.9)
)

T5 <- tibble(
  SiteID = c("A", "A", "B"),
  Year = c(2020, 2021, 2021),
  Nutrients = c(8.2, 7.9, 9.1)
)
# join T4 and T5
left_join(T4, T5, by = c("Site.ID" = "SiteID", "Year" = "Year"))

# notes are different and don't want them combined
T6 <- tibble(
  Site.ID = c("A", "B", "C"),
  Notes = c("pristine", "degraded", "moderately impaired")
)

T7 <- tibble(
  Site.ID = c("A", "B", "D"),
  Notes = c("sunny", "shaded", "partially shaded"),
  Quality = c("good", "fair", "poor")
)

# Don't specify how to join — creates ambiguity with 'Notes'
left_join(T6, T7, by = "Site.ID")
# so might want to layer the two notes columns on top of each other by renaming columns before joining 
T6_renamed <- T6 |> 
  rename(Condition_Notes = Notes)

T7_renamed <- T7 |> 
  rename(Habitat_Notes = Notes)

left_join(T6_renamed, T7_renamed, by = "Site.ID")


##### PART 2: Dates and Times w Lubridate ######
now() #print time at time of coding, can timestamp something 
# change timezones
now(tzone = "EST")  # East Coast
now(tzone = "GMT")  # Greenwich Mean Time
now(tzone = "US/Hawaii")  # Hawaii Time
# just the date
today()
# time zone dates
today(tzone = "GMT")
# time checks
am(now())        # Is it morning?
leap_year(now()) # Is it a leap year?

# DATES HAVE TO BE CHARACTERS
# lubridate guesses date formats from character strings using year, month, day abbreviations:

# convert dates
ymd("2021-02-24") # ISO format
mdy("02/24/2021") # US format
mdy("February 24 2021") # written format
dmy("24/02/2021") # european format

# DATE and TIMES
ymd_hms("2021-02-24 10:22:20 PM") # ISO format w time
mdy_hms("02/24/2021 22:22:20") #US format with seconds
mdy_hm("February 24 2021 10:22 PM") # Written month with hour/minute

# create a vector of datetimes
datetimes <- c(
  "02/24/2021 22:22:20", # all in quotes bc HAS to be character
  "02/25/2021 11:21:10",
  "02/26/2021 8:01:52"
)

datetimes
# convert the vector to datetime objects
datetimes <- mdy_hms(datetimes) # no longer characters, converted to POSIXct datetime format

datetimes

# extract info from datetimes
month(datetimes)
# extract as labels
month(datetimes, label = TRUE) # plots 2 and feb abbreviated
# not abbr
month(datetimes, label = TRUE, abbr = FALSE) # full names of month
# extract day of month
day(datetimes)
# day of the week
wday(datetimes, label = TRUE)
# hour
hour(datetimes)
# min
minute(datetimes)
# sec
second(datetimes)

# adding time intervals
# i.e. worked in another country/collected data abroad, but working in HST you can manipulate times
# 's' ADDS hrs 
datetimes + hours(4) # add 4 hrs
datetimes + days(2) # add 2 days
# You can also add minutes(), seconds(), months(), years(), and more!

# Rounding dates
# round to nearest mintues
round_date(datetimes, "minute") # say sensor is off, not calibrated
round_date(datetimes, "5 mins") # round to nearest 5 mins

# timezone awareness
# Create a datetime WITHOUT timezone info
datetime_naive <- mdy_hms("02/24/2021 10:22:20")
datetime_naive
## ASSIGN TZ ##
# View same moment in different timezone
# Use with_tz() to interpret the same clock time in a different timezone:
# take the date time in UTC and show it in HST 
# Assume the naive time is in Hawaii
hawaii_time <- with_tz(datetime_naive, tzone = "US/Hawaii")
hawaii_time
# Same moment, viewed from EST
est_time <- with_tz(hawaii_time, tzone = "EST")
est_time

# Change the timezone label 
#Use force_tz() to reassign a naive datetime to a specific timezone (claim it was collected there):
# take the UTC and CHANGE to HST
# # Claim this was collected in Hawaii (though it was naive)
force_hawaii <- force_tz(datetime_naive, tzone = "US/Hawaii")
force_hawaii
# Now convert to EST (this changes the clock time!)
with_tz(force_hawaii, tzone = "EST")
# Important: force_tz() changes the clock reading because you’re assigning timezone info to a previously naive datetime.



##### CHALLENGE ####
CondData <- read.csv(here("Week_05", "Data", "CondData.csv")) |> 
  mutate(datetime = mdy_hms(date)) # convert the date column to a datetime using the pipe:
view(CondData)

## combine Topt.data and Site.characteristics
# import data
Topt_data <- read.csv(here("Week_05", "Data","Topt_data.csv"))
Site_data <- read.csv(here("Week_05", "Data", "site.characteristics.data.csv")) 

# pivot wider
wide_site_data <- Site_data |> 
  pivot_wider(names_from = "parameter.measured",
              values_from = "values")

glimpse(wide_site_data)

joined_files <- full_join(wide_site_data, Topt_data)



view(joined_files)
