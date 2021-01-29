library(highcharter)

source('./climateData.R')


shefClimateDf <- shefClimateNoNA %>%
  mutate(
    season = year(TIMESTAMP)
  ) %>%
  filter(
    between(TIMESTAMP, as_datetime("2012-01-01"), as_datetime("2013-12-31"))
  ) %>%
  mutate(
    TIMESTAMP = format.Date(TIMESTAMP, "%m-%d")
  )

hchart(shefClimateDf, "line", hcaes(x = TIMESTAMP, y = AirTC_Avg, group = season))


library(DT)



datatable(shefClimateNoNA)

