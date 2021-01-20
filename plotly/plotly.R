library(plotly)

source('./climateData.R')

plot1 <- shefClimateNoNA %>%
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

ggplotly(plot1, tooltip = c("x", "AirTC_Avg"))


########################
## Climate data in 3D
########################
