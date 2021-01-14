library(tidyverse)
library(lubridate)
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

# can also provide column name: 
# read_csv(..., col_names = c("Date", "Windspeed", "Air Temp", "Rlt. Humidity", "Solar Rad.", "Pressure"))
# missing value read_csv(..., na=c("1", ".")) indicates interpret 1 and . as na.

# print more rows
shefClimate %>% print(n=20)

#43845
count(shefClimate)


###############################################
########### Data processing ###################
###############################################
shefClimate %>% summarise(airTempSd = sd(AirTC_Avg, na.rm = TRUE))

# null values / missing value
# apply function sum() to count number of NAs for all columns selected with everything()
# print use summarise
shefClimate %>% summarise(across(everything(), ~sum(is.na(.x) | is.infinite(.x))))

# base package
apply(shefClimate, 2, function(x) sum(is.na(x) | is.infinite(x)))



shefClimate %>% filter(across(!TIMESTAMP, ~is.na(.x)))

# check it is indeed the problem of timestamp
shefClimate %>% filter(minute(TIMESTAMP) != '0' | second(TIMESTAMP) != '0')


# tidyr drop missing values
# shefClimate %>% drop_na()
shefClimateNoNA <- shefClimate %>% filter(across(everything(), ~ !(is.na(.x) | is.infinite(.x))))


# Now check if every hour are present
head(shefClimate)
tail(shefClimate)

allHours <- seq(
  from = as.POSIXct("2011-03-01", tz = "UTC"),
  to = as.POSIXct("2016-02-29 23:00:00", tz = "UTC"),
  by = "hour"
)

# some hours didn't have data
missingHours <- allHours[!(allHours %in% shefClimate$TIMESTAMP)]
missingHours


# add missing hours to the dataset
# 1. can use average of other year's data
# or 2. data from last row

imputeClimateData <- function(myDataset, missingHours) {
  newDataset = myDataset
  
  for (missingHour in missingHours) {
    lastHour = missingHour - 3600
    
    if (lastHour %in% myDataset$TIMESTAMP) {
      # add missing hour from last hour
      lastHourData <- myDataset %>% filter(TIMESTAMP == lastHour)
      lastHourData$TIMESTAMP = as.POSIXct(missingHour, origin="1970-01-01", tz = "UTC")
      
      newDataset <- newDataset %>% add_row(lastHourData)
      
    } else {
      # add missing hour using other year's average
      missingHourCT <- as.POSIXct(missingHour, origin="1970-01-01", tz = "UTC")
      
      month = month(missingHourCT) 
      day = day(missingHourCT)
      hour = hour(missingHourCT)
      
      allYearsAvg <- myDataset %>% 
        filter(
          hour(TIMESTAMP) == hour & 
            day(TIMESTAMP) == day & 
            month(TIMESTAMP) == month
        ) %>%
        summarise(across(everything(), ~ mean(.x)))
      
      allYearsAvg$TIMESTAMP = missingHourCT
      
      myDataset <- myDataset %>% add_row(allYearsAvg)
    }
  }
  
  return(newDataset %>% arrange(TIMESTAMP))
}



while (length(missingHours) != 0) {
  shefClimateNoNA <- imputeClimateData(shefClimateNoNA, missingHours)
  missingHours <- allHours[!(allHours %in% shefClimateNoNA$TIMESTAMP)]
}