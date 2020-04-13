require(datasets)
require(stats)
require(RCUrl)
require(ggplot2)
require(ggthemes)
require(scales)
require(tidyverse)
require(gridExtra)
require(rstudioapi)

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

#create a line plot for active cases
yMarker <- 10^ceiling(log10(max(d_sums[-1])))/20
p1 <- get_base_plot(d_sums, "Active cases") +
  scale_y_continuous(breaks = round(seq(0, max(d_sums[-1]), by = yMarker),1)) +
  geom_line() +
  geom_point() 

#create a smoothen line plot for growth rate multiplier moving average
p2 <- get_base_plot(d_trend, "(5 day moving average) growth multiplier") +
  geom_smooth(method="loess", formula = y ~ x, se = FALSE)

grid.arrange(p1, p2, nrow = 2, top = "COVID-19 : Active cases and growth rate (Data Source- Johns Hopkins JHU CCSE)")

