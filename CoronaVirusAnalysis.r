require(datasets)
require(stats)
require(RCUrl)
require(ggplot2)
require(ggthemes)
require(scales)
require(tidyverse)
require(gridExtra)
rm(list = ls())

#download the data file
download_url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv"
download.file(download_url, "corona_cases.csv")
df <- read.csv("corona_cases.csv")

#select countries to plot, support only two countries
countries <- c("US", "Italy", "France", "Iran", "India")
#countries <- c("US", "Iran", "Italy")

#Filter the country only cases
d_country <- subset(df, select=-c(1,3,4,5:19), Country.Region %in% countries & !grepl(".*, .*", Province.State))
d_sums <- aggregate(d_country[-1], by=list(Country=d_country$Country.Region), FUN=sum)
rownames(d_sums) <- d_sums$Country
d_sums <- d_sums[-1]
dates <- seq(c(as.Date("2020/1/22")), by = "day", length.out = ncol(d_sums))
d_sums <- rbind(date=as.Date(dates, format='%d-%m-%Y'), d_sums[1:length(countries),])
d_sums <- data.frame(t(d_sums))

#Create a dataframe for rate of change
d_trend <- 1+data.frame(diff(data.matrix(d_sums[,-1])))/(d_sums[-1,-1])
d_trend <- data.frame(t(d_trend))
d_trend <- rbind(date=as.Date(dates, format='%d-%m-%Y'), d_trend[1:length(countries),])

#create a line plot
yMarker <- 10^ceiling(log10(max(sapply(d_sums[-1], max))))/20
d_sums <- d_sums %>% gather(countries,key="Country",value="Value", -date)
p1 <- d_sums %>% 
  ggplot(aes(x=as.Date(date, origin = "2020/1/22"),y=Value,col=Country,group=Country)) + 
  geom_line() +
  geom_point() +
  labs(y="Confirmed cases", x = "Date") +
  theme_economist() + 
  scale_colour_economist() + 
  scale_y_continuous(breaks = round(seq(0, max(sapply(d_sums[3], max)), by = yMarker),1)) +
  scale_x_date(date_labels = "%b/%d") 

d_trend <- data.frame(t(d_trend))
d_trend <- d_trend %>% gather(countries,key="Country",value="Value", -date)
d_trend$Value <- replace_na(d_trend$Value, 1)

p2 <- d_trend %>% 
  ggplot(aes(x=as.Date(date, origin = "2020/1/22"),y=Value,col=Country,group=Country)) + 
  geom_smooth(method="loess", formula = y ~ x, se = FALSE) +
  labs(y="Rate of change", x = "Date") +
  theme_economist() + 
  scale_colour_economist() + 
  scale_x_date(date_labels = "%b/%d") 

grid.arrange(p1, p2, nrow = 2, top = "COVID-19 : Confirmed cases and Rate of change")

