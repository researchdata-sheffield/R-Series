source('./climateData.R')

##############
### ggpubr ###
##############
library(ggpubr)


shefClimateNoNA %>% 
  mutate(season = year(TIMESTAMP)) %>% 
  ggboxplot(
    x = "season", 
    y = "AirTC_Avg", 
    color = "season"
  )

shefClimateNoNA %>% 
  mutate(season = year(TIMESTAMP)) %>% 
  ggviolin(
    x = "season", 
    y = "AirTC_Avg", 
    color = "season",
    fill = "season",
    add = c("boxplot"),
    add.params = list(fill = "white")
  )


#############
## lattice ##
#############
library(lattice)

shefClimateDf <- shefClimateNoNA %>%
  mutate(
    season = paste(year(TIMESTAMP), year(TIMESTAMP)+1, sep = "-")
  ) %>% 
  as.data.frame()

xyplot(
  AirTC_Avg ~ TIMESTAMP, 
  data = shefClimateDf, 
  xlab = "Date",
  ylab = "Average air temperature"
)


xyplot(
  AirTC_Avg ~ TIMESTAMP, 
  data = shefClimateDf, 
  groups = factor(season, labels=c("2011", "2012", "2013", "2014", "2015", "2016")),
  xlab = "Date",
  ylab = "Average air temperature",
  auto.key = list(columns = 6)
)

xyplot(
  AirTC_Avg ~ TIMESTAMP | season, 
  data = shefClimateDf, 
  groups = factor(season, labels=c("2011", "2012", "2013", "2014", "2015", "2016")),
  xlab = "Date",
  ylab = "Average air temperature",
  auto.key = list(columns = 6)
)


##############
## esquisse ##
##############
esquisse::esquisser()



###########
### rgl ###
###########
library(rgl)

shefClimateRgl <- shefClimateNoNA %>% mutate(season = year(TIMESTAMP))

# run this first and drag to expand the graph
with(
  shefClimateRgl %>% filter(
    between(TIMESTAMP, as_datetime('2011-03-01'), as_datetime('2011-04-01')) |
    between(TIMESTAMP, as_datetime('2012-03-01'), as_datetime('2012-04-01')) |
    between(TIMESTAMP, as_datetime('2013-03-01'), as_datetime('2013-04-01'))
  ), 
  plot3d(AirTC_Avg, WS_ms_Avg, RH, col = as.integer(season), type = "s")
)

# then run this to add legend
legend3d(
  "topright", 
  legend = paste('Year', c('2011', '2012', '2013')), 
  pch = 16, 
  col = c(2011, 2012, 2013),
  cex = 1.2
)


