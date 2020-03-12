require(datasets)
require(stats)
require(RCUrl)
require(ggplot2)
require(ggthemes)
require(scales)
rm(list = ls())

#download the data file
download_url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv"
country <- "Italy"
download.file(download_url, "corona_cases.csv")
df <- read.csv("corona_cases.csv")

#Filter the country only cases
a_us<-subset(df, Country.Region == country & !grepl(".*, .*", Province.State))
d_us <- subset(a_us, select=-c(1,2,3,4))

#create dataset with date and sum of confirmed cases
dates <- seq(c(as.Date("2020/1/22")), by = "day", length.out = ncol(d_us))
d_plot <- data.frame("date"= as.Date(dates), "value"=colSums(d_us))

#create a line plot
p <- ggplot(d_plot, aes(x=date, y=value)) + geom_line() + 
  labs(y="Number of confirmed cases", x = "Date") +
  theme_economist() + 
  scale_colour_economist() + 
  ggtitle(paste("Corona confirmed cases over time in ", country)) +
  scale_y_continuous(breaks = round(seq(min(d_plot$value), max(d_plot$value), by = 1000),1)) +
  scale_x_date(date_labels = "%b/%d")
p
# plot(tx)
# plot(log(tx))
