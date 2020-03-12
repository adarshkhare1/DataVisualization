require(datasets)
require(stats)
require(RCUrl)
require(ggplot2)
rm(list = ls())

#download the data file
download_url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv"
download.file(download_url, "corona_cases.csv")
df <- read.csv("corona_cases.csv")

#Filter the US only cases
a_us<-subset(df, Country.Region == "US" & !grepl(".*, .*", Province.State))
d_us <- subset(a_us, select=-c(1,2,3,4))

#create dataset with date and sum of confirmed cases
dates <- seq(c(ISOdate(2020,1,22)), by = "day", length.out = ncol(d_us))
d_plot <- data.frame("date"= dates, "value"=colSums(d_us))

#create a line plot
p <- ggplot(d_plot, aes(x=date, y=value)) + geom_line()
p
