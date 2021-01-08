library(tidyverse)

# DATASET README file - https://figshare.shef.ac.uk/articles/dataset/Hadfield_Green_Roof_5-year_Dataset/11876736?file=25547585
# File IDs
# 25647497 - Sheffield_Climate_5year.csv
# 25647500 - Sheffield_VWC_5ear.csv
# 25647542 - Sheffield_Rain_5year.csv
# 25647836 - Sheffield_Runoff_5year.csv
# 25647494 - Sheffield_Valid_Events.csv

###############################################
################ READ DATA ####################
###############################################

# Read 5 year Sheffield climate data
shefClimate <- read_csv(
  "https://figshare.shef.ac.uk/ndownloader/files/25647497",
  col_types = cols(
    col_datetime("%d-%b-%Y %H:%M:%S"),
    col_double(),
    col_double(),
    col_double(),
    col_double(),
    col_double()
  )
)

# print more rows
shefClimate %>% print(n=20)

#43845
count(shefClimate)



###############################################
########### Data processing ###################
###############################################

# null values / missing value
# apply function sum() to count number of NAs for all columns selected with everything()
# print use summarise
shefClimate %>% summarise(across(everything(), ~sum(is.na(.x) | is.infinite(.x))))

# base package
apply(shefClimate, 2, function(x) sum(is.na(x) | is.infinite(x)))


# check it is indeed the problem of timestamp
shefClimate %>% filter(lubridate::minute(TIMESTAMP) != '0' | lubridate::second(TIMESTAMP) != '0')

# Now check if every hour are present
head(shefClimate)
tail(shefClimate)

allHours <- seq(
  from = as.POSIXct("2011-03-01"),
  to = as.POSIXct("2016-02-29 23:00:00"),
  by = "hour"
)

# some hours didn't have data
missingHours <- allHours[!(allHours %in% shefClimate$TIMESTAMP)]

# add 


# tidyr drop missing values
shefClimate %>% drop_na()

# 

shefClimateNoNA <- shefClimate %>% filter(across(everything(), ~ !(is.na(.x) | is.infinite(.x))))



# Q1 difference of avg temp between consecutive years' winter 28 Nov - 28 Feb
shefClimateNoNA %>% 
  filter(
    between(TIMESTAMP, as.POSIXct("2011-11-28"), as.POSIXct("2012-02-28")) |
    between(TIMESTAMP, as.POSIXct("2012-11-28"), as.POSIXct("2013-02-28")) |
    between(TIMESTAMP, as.POSIXct("2013-11-28"), as.POSIXct("2014-02-28")) |
    between(TIMESTAMP, as.POSIXct("2014-11-28"), as.POSIXct("2015-02-28")) |
    between(TIMESTAMP, as.POSIXct("2015-11-28"), as.POSIXct("2016-02-28")) 
  ) %>% print(n=2000)


