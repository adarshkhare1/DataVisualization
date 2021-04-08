
#file containing functions for Covid-19 analysis

#calculate moving average
ma <- function(x,n=5){stats::filter(x,rep(1/n,n), sides=1)}

# Download data file from JHU github url and build filtered dataframe
build_dataframe <- function(cases_file, countries) {
  base_url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/"
  cases_url <- paste(base_url,cases_file, sep = "")
  download.file(cases_url, "corona_cases.csv")
  df_cases <- read.csv("corona_cases.csv")
  df_cases$Country.Region <- gsub(' ', '_', df_cases$Country.Region)
  df_cases <- subset(df_cases, select=-c(1,3,4,5:57), Country.Region %in% countries & !grepl(".*, .*", Province.State))
  df_cases <- aggregate(df_cases[-1], by=list(Country=df_cases$Country.Region), FUN=sum)
  df_cases$Country <- gsub(' ', '_', df_cases$Country)
  rownames(df_cases) <- df_cases$Country
  df_cases[-1]
}

get_base_plot <- function(d_series, yLabel) {
  p <- d_series %>% gather(countries,key="Country",value="Value", -date) %>% 
    ggplot(aes(x=as.Date(date, origin = "2020/1/1"),y=Value,col=Country,group=Country)) + 
    labs(y=yLabel, x = "") +
    theme_economist() + 
    scale_x_date(date_labels = "%b %y", date_breaks = "1 month")
  p
}
