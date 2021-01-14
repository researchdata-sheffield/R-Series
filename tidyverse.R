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


# check if all hours exists
allHours[!(allHours %in% shefClimateNoNA$TIMESTAMP)]


# shefClimateNoNA %>%
#   filter(
#     between(
#       TIMESTAMP, 
#       as.POSIXct(paste(year(TIMESTAMP), 11, 28, sep = "-")), 
#       as.POSIXct(paste(year(TIMESTAMP)+1, 02, 28, sep = "-"))
#     )
#   ) %>% print(n=4000)

###############################################
################ QUESTIONS ####################
###############################################

# Q1 difference of avg temp between consecutive years' winter 20 Dec - 20 Mar
shefClimateNoNA %>%
  filter(
    between(TIMESTAMP, as.POSIXct("2011-12-20"), as.POSIXct("2012-03-20")) |
    between(TIMESTAMP, as.POSIXct("2012-12-20"), as.POSIXct("2013-03-20")) |
    between(TIMESTAMP, as.POSIXct("2013-12-20"), as.POSIXct("2014-03-20")) |
    between(TIMESTAMP, as.POSIXct("2014-12-20"), as.POSIXct("2015-03-20")) |
    between(TIMESTAMP, as.POSIXct("2015-12-20"), as.POSIXct("2016-02-29"))
  ) %>% 
  mutate(
    season = if_else(
      month(TIMESTAMP) %in% c(11, 12),
      paste(year(TIMESTAMP), year(TIMESTAMP)+1, sep = "-"),
      paste(year(TIMESTAMP)-1, year(TIMESTAMP), sep = "-")
    )
  )  %>%
  group_by(season) %>%
  select(-TIMESTAMP) %>%
  summarise(across(everything(), ~mean(.x))) %>%
  ggplot(data = ., aes(x = season, y = AirTC_Avg)) + 
  geom_col(aes(fill = season)) + 
  scale_fill_brewer(palette = "Blues") +
  labs(
    x = "Winter season", 
    y = "Average air temperature",
    title = 
      "Average air temperature for each winter season from 2011 - 2016",
    subtitle = 
      "NOTE: Winter period is typically 20 Dec - 20 Mar (next year)
      the final date of this dataset is 29 Feb 2016"
  ) +
  theme(
    plot.title = element_text(vjust = 1),
    plot.subtitle = element_text(size = 8, vjust = 4)
  )



# Q2 Is there a relationship between each pair of variables
corMatrix <- shefClimateNoNA %>% 
  select(-TIMESTAMP) %>% 
  cor() %>% 
  round(., 2)

corMatrix

corVars <- rownames(corMatrix)

corMatrix %>% 
  as_tibble() %>% 
  # decrease the number of columns
  # add more rows
  pivot_longer(
    cols=1:5, 
    names_to = "var1", 
    values_to = "value"
  ) %>% 
  mutate(var2 = rep(corVars, each = 5)) %>%
  relocate(var2, .after = var1) %>%
  ggplot(
    aes(x = var1, y = var2, fill = value)
  ) + 
  geom_tile(color="white", size=0.05) +
  scale_fill_gradient(low = "#fedf00", high = "#009640") + 
  geom_text(aes(label = round(value, 1)))


# Q3 Density of each variable
plot1 <- ggplot(data = shefClimateNoNA) + 
  geom_density(
    aes(x = WS_ms_Avg), fill = "#fedf00", color = "#fedf00", alpha = 0.8
  ) + 
  labs(
    x = "Average windspeed (m/s)", 
    title = "Density plot for Average windspeed (m/s)"
  )
  
plot2 <- ggplot(data = shefClimateNoNA) + 
  geom_density(
    aes(x = AirTC_Avg), fill = "#251d5a", color = "#251d5a", alpha = 0.8
  ) + 
  labs(
    x = "Average air temperature (C)", 
    title = "Density plot for Average air temperature (C)"
  )

plot3 <- ggplot(data = shefClimateNoNA) + 
  geom_density(
    aes(x = RH), fill = "#0066b3", color = "#0066b3", alpha = 0.8
  ) +
  labs(
    x = "Relative Humidity (%)", 
    title = "Density plot for Relative Humidity (%)"
  )

plot4 <- ggplot(data = shefClimateNoNA) + 
  geom_density(
    aes(x = Slr_kW), fill = "#009640", color = "#009640", alpha = 0.8
  ) +
  labs(
    x = "Solar Radiation (kW/m2)", 
    title = "Density plot for Solar Radiation (kW/m2)"
  )

plot5 <- ggplot(data = shefClimateNoNA) + 
  geom_density(
    aes(x = BP_mbar), fill = "#ade1f8", color = "#ade1f8", alpha = 0.8
  ) + 
  labs(
    x = "Barometric Pressure (mbar)",
    title = "Density plot for Barometric Pressure (mbar)"
  )


library(gridExtra)

grid.arrange(plot1, plot2, plot3, plot4, plot5, ncol=2)


# Q4 calculate FAO-56 Penman-Monteith 
# For hourly data, divided 900 by 24 and convert radiation into MJ/m2/h
# 1W = 1J/s => 1kw = 1000J/s = 60,000J/min = 3,600,000J/hour = 3.6MJ/hour

# shefClimateNoNA %>% 
#   filter(between(TIMESTAMP, as.POSIXct("2011-03-01"), as.POSIXct("2011-04-01"))) %>%
#   mutate(
#     FAO56 = (AirTC_Avg*Slr_kW + 
#       0.066*2.45*(WS_ms_Avg*(BP_mbar/1000 - (RH/100) * BP_mbar/1000))) /
#       (AirTC_Avg + 0.066)
#   ) %>% summarise(FAO56_Mean = mean(FAO56))


shefClimateNoNA %>% 
  filter(between(TIMESTAMP, as.POSIXct("2011-03-01"), as.POSIXct("2012-03-01"))) %>%
  mutate(
    FAO56 = (0.408 * AirTC_Avg * (Slr_kW * 3.6) + 
               0.066 * (900 / 24 / (AirTC_Avg + 273)) * 
               (WS_ms_Avg * (BP_mbar / 1000 - (RH / 100) * BP_mbar / 1000))
             ) / 
      (AirTC_Avg + 0.066 * (1 + 0.34 * WS_ms_Avg))
  ) %>%
  ggplot(aes(x = TIMESTAMP, y = FAO56)) +
  geom_bin2d()


#%>% summarise(FAO56_Mean = mean(FAO56))



# Q5 Is there a difference in VWC (soil moisture) between top, middle, bottom probe for testbed 1
shefVWC <- read_csv(
  "https://figshare.shef.ac.uk/ndownloader/files/25647500",
  col_types = cols(
    col_datetime("%d-%b-%Y %H:%M:%S"),
    col_double(),
    col_double(),
    col_double(),
    col_double(),
    col_double(),
    col_double(),
    col_double(),
    col_double(),
    col_double(),
    col_double(),
    col_double(),
    col_double()
  )
)

# time series
shefVWC %>% ggplot(aes(x = TIMESTAMP)) + 
  geom_line(aes(y = TB1_T), color = "#0066b3") +
  geom_line(aes(y = TB1_M), color = "#251d5a") + 
  geom_line(aes(y = TB1_B), color = "#009640")


shefVWC %>% 
  pivot_longer(cols = TB1_T:TB1_B, names_to = "TB1", values_to = "TB1value") %>%
  ggplot(aes(x = TIMESTAMP, y = TB1value, group = TB1, color = TB1)) +
  geom_line(size = 0.9) +
  scale_color_manual(values = c("#0066b3", "#251d5a", "#009640")) +
  labs(
    x = "Date", 
    y = "Soil moisture (VWC)",
    title = "Soil moisture for Three probes (Top, Middle, Bottom) of Test Bed 1"
  ) + 
  scale_x_datetime(
    date_breaks = "3 month", 
    date_labels = "%b  %Y", 
    limits = c(as.POSIXct("2011-03-01", tz="UTC"), NA)
  ) +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(color = "#dbdbdb")
  )

  
