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


###################
## AirTC Heatmap ##
###################
shefClimateHeatmap <- shefClimateNoNA %>%
  filter(
    between(TIMESTAMP, as.POSIXct("2015-01-01"), as.POSIXct("2016-01-01"))
  ) %>% 
  mutate(
    month = month(TIMESTAMP, label = TRUE),
    day = day(TIMESTAMP)
  ) %>%
  select(AirTC_Avg, month, day) %>% 
  group_by(month, day) %>% 
  summarise(AirTC_Avg = mean(AirTC_Avg)) %>% 
  pivot_wider(names_from = day, values_from = AirTC_Avg) %>%
  ungroup() %>%
  select(-c(month)) %>% 
  as.matrix()

myHeatmap <- plot_ly(
  x = sprintf("%d", 1:31),
  y = month.abb[1:12],
  z = shefClimateHeatmap, 
  type = "heatmap"
) 

myHeatmap %>% 
  layout(
    title = "Daily average temperature in Sheffield (2015)"
  ) %>%
  add_trace(
    showscale = FALSE,
    hovertemplate = paste(
      '<b>Date</b>: %{x} %{y}', 
      '<br><b>Temperature</b>: %{z:.2f} C',
      '<extra></extra>' # add EXTRA tag to hide trace name
    )
  )

