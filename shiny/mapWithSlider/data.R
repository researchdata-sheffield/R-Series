# INSERT Libraries HERE
library(tidyverse)
library(lubridate)
library(plotly)

# INSERT Dataset HERE
mapData <- tempfile()
download.file("https://figshare.shef.ac.uk/ndownloader/files/17499005", mapData, mode="wb")
dataBorders <- unz(mapData, "data/borders.csv") %>% read_csv()
dataCastles <- unz(mapData, "data/castles.csv") %>% 
  read_csv() %>% 
  select(-X7) %>% 
  rename(latitude = `latitude N`, longitude = `longitude +E-W`)


dataItinerary <- unz(mapData, "data/itinerary.csv") %>% read_csv()
dataLocations <- unz(mapData, "data/locations.csv") %>% read_csv()

joinDataItinerary <- dataItinerary %>% 
  inner_join(dataLocations) %>% 
  fill(year) %>%
  mutate(Date = as_date(paste(year, month, day, sep = "-"))) %>%
  fill(Date)




