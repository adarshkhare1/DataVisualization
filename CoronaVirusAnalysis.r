require(datasets)
require(stats)
require(RCUrl)
require(ggplot2)
require(ggthemes)
require(scales)
require(tidyverse)
rm(list = ls())

#download the data file
download_url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv"
download.file(download_url, "corona_cases.csv")
df <- read.csv("corona_cases.csv")

#select countries to plot, support only two countries
countries <- c("Italy", "US", "India")

#Filter the country only cases
d_country <- subset(df, select=-c(1,3,4), Country.Region %in% countries & !grepl(".*, .*", Province.State))
d_sums <- aggregate(d_country[-1], by=list(Country=d_country$Country.Region), FUN=sum)
rownames(d_sums) <- d_sums$Country
d_sums <- d_sums[-1]
dates <- seq(c(as.Date("2020/1/22")), by = "day", length.out = ncol(d_sums))
d_sums <- rbind(date=as.Date(dates, format='%d-%m-%Y'), d_sums[1:length(countries),])
d_sums <- data.frame(t(d_sums))

#create a line plot
yMarker <- 10^ceiling(log10(max(sapply(d_sums[-1], max)/25)))
d_sums <- d_sums %>% gather(countries,key="country",value="Value", -date)
p <- d_sums %>% 
  ggplot(aes(x=as.Date(date, origin = "2020/1/22"),y=Value,col=country,group=country)) + 
  geom_line() +
  geom_point() +
  labs(y="Number of confirmed cases", x = "Date") +
  theme_economist() + 
  scale_colour_economist() + 
  ggtitle(paste("Corona confirmed cases over time", paste(countries, collapse = ", "))) +
  scale_y_continuous(breaks = round(seq(0, max(sapply(d_sums[3], max)), by = yMarker),1)) +
  scale_x_date(date_labels = "%b/%d") 
p


