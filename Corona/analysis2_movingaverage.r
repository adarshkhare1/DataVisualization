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
#countries <- c ("US", "Italy", "Germany", "Spain", "France")
countries <- c("India", "Singapore", "Malaysia", "Japan", "Thailand")

#download the data file
d_sums <- build_dataframe("time_series_covid19_confirmed_global.csv", countries)

#Add dates 
dates <- seq(c(as.Date("2020/3/1")), by = "day", length.out = ncol(d_sums))
d_sums <- rbind(date=as.Date(dates), d_sums[1:length(countries),])
d_sums <- data.frame(t(d_sums))

#Create a dataframe for rate of change
d_trend <- ma(d_sums[-1,-1]/d_sums[-nrow(d_sums),-1],6)[-1:-5,]
colnames(d_trend) <- colnames(d_sums)[-1]
d_trend <- data.frame(t(d_trend))
d_trend <- rbind(date=as.Date(tail(dates,-4)), d_trend[1:length(countries),])

#create a line plot
yMarker <- 10^ceiling(log10(max(d_sums[-1])))/20
p1 <- get_base_plot(d_sums, "Confirmed cases") +
  scale_y_continuous(breaks = round(seq(0, max(d_sums[-1]), by = yMarker),1)) +
  geom_line() +
  geom_point() 

d_trend <- data.frame(t(d_trend))
#d_trend$Value <- replace_na(d_trend$Value, 1)
#d_trend$Value[is.infinite(d_trend$Value)] <- 1 

#create a smoothen line plot for growth rate multiplier trend
p2 <- get_base_plot(d_trend, "(5 day moving average) growth multiplier") +
  geom_smooth(method="loess", formula = y ~ x, se = FALSE)


grid.arrange(p1, p2, nrow = 2, top = "COVID-19:Confirmed cases and growth rate (Data Source- Johns Hopkins JHU CCSE)")
