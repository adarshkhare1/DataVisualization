require(datasets)
require(stats)
require(RCUrl)
require(ggplot2)
require(ggthemes)
require(scales)
require(tidyverse)
require(gridExtra)
require(rstudioapi) # make sure you have it installed

rm(list = ls())
setwd(dirname(getActiveDocumentContext()$path ))
#print( getwd() )
source("./covid19_functions.r")

#select countries to plot, support only two countries
countries <- c ("US", "Italy", "Germany", "Spain", "France", "China")
#countries <- c ("India", "Russia", "Australia", "Brazil", "Canada")
#countries <- c("India", "Singapore", "Malaysia", "Japan", "Thailand")

#download the data file
df_confirmed <- build_dataframe("time_series_covid19_confirmed_global.csv", countries)
df_recovered <- build_dataframe("time_series_covid19_recovered_global.csv", countries)

#Calculate active cases and add date row
d_sums <- df_confirmed - df_recovered
dates <- seq(c(as.Date("2020/3/1")), by = "day", length.out = ncol(d_sums))
d_sums <- rbind(date=as.Date(dates), d_sums[1:length(countries),])
d_sums <- data.frame(t(d_sums))

#Create a dataframe for rate of change by skiping date
d_trend <- ma(d_sums[-1,-1]/d_sums[-nrow(d_sums),-1],5)[-1:-5,]
colnames(d_trend) <- colnames(d_sums)[-1]
#add date in trend dataframe
d_trend <- data.frame(t(d_trend))
d_trend <- rbind(date=as.Date(tail(dates,-5)), d_trend[1:length(countries),])
d_trend <- data.frame(t(d_trend))

#create a line plot
yMarker <- 10^ceiling(log10(max(d_sums[-1])))/20
p1 <- d_sums %>% gather(countries,key="Country",value="Value", -date) %>% 
  ggplot(aes(x=as.Date(date, origin = "2020/1/1"),y=Value,col=Country,group=Country)) + 
  geom_line() +
  geom_point() +
  labs(y="Active cases", x = "Date") +
  theme_economist() + 
  scale_y_continuous(breaks = round(seq(0, max(d_sums[-1]), by = yMarker),1)) +
  scale_x_date(date_labels = "%b/%d") 

p2 <- d_trend %>% gather(countries,key="Country",value="Value", -date) %>% 
  ggplot(aes(x=as.Date(date, origin = "2020/1/1"),y=Value,col=Country,group=Country)) + 
  geom_smooth(method="loess", formula = y ~ x, se = FALSE) +
#  geom_line() +
#  geom_point() +
  labs(y="(5 day moving average) growth multiplier", x = "Date") +
  theme_economist() + 
  scale_x_date(date_labels = "%b/%d") 

grid.arrange(p1, p2, nrow = 2, top = "COVID-19 : Active cases and growth rate (Data Source- Johns Hopkins JHU CCSE)")


