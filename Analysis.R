
library(readxl)
library(httr)
library(jsonlite)
library(dplyr)
library(gtrendsR)
library(lubridate)


###### Load S&P 500 data #########
returns_data <- read_excel("S&P500_returndata.xlsx", sheet = "Monthly")
#generate a valid date column
returns_data <- returns_data %>%
  mutate(date = ym(yyyymm))


######## Load the Wikipedia Pageviews
war_df <- get_wikipedia_pageviews("War", "18710101", "20250501")
pandemic_df <- get_wikipedia_pageviews("Pandemic", "18710101", "20250501")


###### Load Google Trends data
gtrends_war <- gtrends(keyword = "war", geo = "US", time = "1990-01-01 2025-04-30")$interest_over_time
gtrends_war <- gtrends_war %>%
  transmute(date = as.Date(date), war_trend = as.numeric(hits))





