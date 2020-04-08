require(datasets)
require(stats)
require(RCUrl)
require(ggplot2)
require(ggthemes)
require(scales)
require(tidyverse)
require(gridExtra)
rm(list = ls())

ma <- function(x,n=5){stats::filter(x,rep(1/n,n), sides=1)}

#download the data file
download_url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
download.file(download_url, "corona_cases.csv")
df <- read.csv("corona_cases.csv")

#select countries to plot, support only two countries
#countries <- c ("US", "Italy", "Germany", "Spain", "France")

countries <- c("India", "Singapore", "Malaysia", "Japan", "Thailand")

#Filter the country only cases
d_country <- subset(df, select=-c(1,3,4,5:43), Country.Region %in% countries & !grepl(".*, .*", Province.State))
d_sums <- aggregate(d_country[-1], by=list(Country=d_country$Country.Region), FUN=sum)
rownames(d_sums) <- d_sums$Country
d_sums <- d_sums[-1]
dates <- seq(c(as.Date("2020/3/1")), by = "day", length.out = ncol(d_sums))
d_sums <- rbind(date=as.Date(dates), d_sums[1:length(countries),])
d_sums <- data.frame(t(d_sums))

#Create a dataframe for rate of change
d_diff <- data.frame(diff(data.matrix(d_sums[,-1])))
d_trend <- ma(d_sums[-1,-1]/d_sums[-nrow(d_sums),-1],6)[-1:-5,]
colnames(d_trend) <- colnames(d_sums)[-1]
d_trend <- data.frame(t(d_trend))
d_trend <- rbind(date=as.Date(tail(dates,-4)), d_trend[1:length(countries),])

#create a line plot
yMarker <- 10^ceiling(log10(max(sapply(d_sums[-1], max))))/20
d_sums <- d_sums %>% gather(countries,key="Country",value="Value", -date)
p1 <- d_sums %>% 
  ggplot(aes(x=as.Date(date, origin = "2020/1/1"),y=Value,col=Country,group=Country)) + 
  geom_line() +
  geom_point() +
  labs(y="Confirmed cases", x = "Date") +
  theme_economist() + 
  scale_y_continuous(breaks = round(seq(0, max(sapply(d_sums[3], max)), by = yMarker),1)) +
  scale_x_date(date_labels = "%b/%d") 

d_trend <- data.frame(t(d_trend))
d_trend <- d_trend %>% gather(countries,key="Country",value="Value", -date)
d_trend$Value <- replace_na(d_trend$Value, 1)
d_trend$Value[is.infinite(d_trend$Value)] <- 1 

p2 <- subset(d_trend, d_trend$Value <= 3) %>% 
  ggplot(aes(x=as.Date(date, origin = "2020/1/1"),y=Value,col=Country,group=Country)) + 
  geom_smooth(method="loess", formula = y ~ x, se = FALSE) +
  labs(y="(5 day moving average) growth multiplier", x = "Date") +
  theme_economist() + 
  scale_x_date(date_labels = "%b/%d") 

grid.arrange(p1, p2, nrow = 2, top = "COVID-19:Confirmed cases and growth rate (Data Source- Johns Hopkins JHU CCSE)")

