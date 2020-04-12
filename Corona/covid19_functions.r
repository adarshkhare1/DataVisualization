
#file containing functions for Covid-19 analysis

#calculate moving average
ma <- function(x,n=5){stats::filter(x,rep(1/n,n), sides=1)}

# Download data file from JHU github url and build filtered dataframe
build_dataframe <- function(cases_file, countries) {
  base_url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/"
  cases_url <- paste(base_url,cases_file, sep = "")
  download.file(cases_url, "corona_cases.csv")
  df_cases <- read.csv("corona_cases.csv")
  df_cases <- subset(df_cases, select=-c(1,3,4,5:43), Country.Region %in% countries & !grepl(".*, .*", Province.State))
  df_cases <- aggregate(df_cases[-1], by=list(Country=df_cases$Country.Region), FUN=sum)
  rownames(df_cases) <- df_cases$Country
  df_cases[-1]
}