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

# Download data file from JHU github url and build filtered dataframe
build_dataframe <- function(cases_file, countries) {
  base_url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/"
  cases_url <- paste(base_url,cases_file, sep = "")
  download.file(cases_url, "corona_cases.csv")
  df_cases <- read.csv("corona_cases.csv")
  df_cases <- subset(df_cases, select=-c(1,3,4,5:43), Country.Region %in% countries & !grepl(".*, .*", Province.State))
  df_cases <- aggregate(df_cases[-1], by=list(Country=df_cases$Country.Region), FUN=sum)
  df_cases
}

#select countries to plot, support only two countries
countries <- c ("US", "Italy", "Germany", "Spain", "France", "China")
#countries <- c ("India", "Russia", "Australia", "Brazil", "Canada")
#countries <- c("India", "Singapore", "Malaysia", "Japan", "Thailand")

#download the data file
df_confirmed <- build_dataframe("time_series_covid19_confirmed_global.csv", countries)
df_recovered <- build_dataframe("time_series_covid19_recovered_global.csv", countries)

#Calculate active cases and add date row
d_sums <- df_confirmed[-1] - df_recovered[-1]
rownames(d_sums) <- df_confirmed$Country
dates <- seq(c(as.Date("2020/3/1")), by = "day", length.out = ncol(d_sums))
d_sums <- rbind(date=as.Date(dates), d_sums[1:length(countries),])
d_sums <- data.frame(t(d_sums))

#Create a dataframe for rate of change
d_diff <- data.frame(diff(data.matrix(d_sums[,-1])))
d_trend <- ma(d_sums[-1,-1]/d_sums[-nrow(d_sums),-1],5)[-1:-5,]
colnames(d_trend) <- colnames(d_sums)[-1]
d_trend <- data.frame(t(d_trend))
d_trend <- rbind(date=as.Date(tail(dates,-5)), d_trend[1:length(countries),])

#create a line plot
yMarker <- 10^ceiling(log10(max(sapply(d_sums[-1], max))))/20
d_sums <- d_sums %>% gather(countries,key="Country",value="Value", -date)
p1 <- d_sums %>% 
  ggplot(aes(x=as.Date(date, origin = "2020/1/1"),y=Value,col=Country,group=Country)) + 
  geom_line() +
  geom_point() +
  labs(y="Active cases", x = "Date") +
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
#  geom_line() +
#  geom_point() +
  labs(y="(5 day moving average) growth multiplier", x = "Date") +
  theme_economist() + 
  scale_x_date(date_labels = "%b/%d") 

grid.arrange(p1, p2, nrow = 2, top = "COVID-19 : Active cases and growth rate (Data Source- Johns Hopkins JHU CCSE)")

