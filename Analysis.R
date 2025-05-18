#install.packages(c("readxl","dplyr","sandwich","lmtest","httr","jsonlite","zoo","lmtest","lubridate","gtrendsR"))  # run once
library(readxl)
library(httr)
library(jsonlite)
library(dplyr)
library(gtrendsR)
library(lubridate)
library(dplyr)
library(zoo)
library(sandwich)
library(lmtest)


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



#------------------------------------------------------------------------------------------------------------------

# 1. Load your returns data
df_base <- read_excel("S&P500_returndata.xlsx", sheet = "Monthly") %>%
  rename(date_num = yyyymm, R = ret) %>%
  mutate(
    date  = as.Date(paste0(as.character(date_num), "01"), "%Y%m%d"),
    Rlead = lead(R, 1)
  ) %>%
  select(date, R, Rlead) %>%
  filter(!is.na(Rlead))

# 2. Define your Wikipedia topics
topics <- c("War", "Pandemic", "Trade_wars", "Tariffs","Trump","China","Trade War")  

# 3. Helper to fetch & tidy one series
fetch_wv <- function(article, df_dates) {
  start   <- paste0(format(min(df_dates), "%Y%m"), "01")
  end     <- paste0(format(max(df_dates), "%Y%m"), "01")
  url     <- paste0(
    "https://wikimedia.org/api/rest_v1/metrics/pageviews/per-article/",
    "en.wikipedia/all-access/user/", URLencode(article),
    "/monthly/", start, "/", end
  )
  resp    <- GET(url, user_agent("R script"))
  stop_for_status(resp)
  js      <- content(resp, "text", encoding="UTF-8") %>% fromJSON()
  tibble(
    date = as.Date(paste0(substr(js$items$timestamp,1,6), "01"), "%Y%m%d"),
    !!article := js$items$views
  )
}

# 4. Build a wide table of all pageviews
wv_list <- lapply(topics, fetch_wv, df_dates = df_base$date)
wv_all  <- Reduce(function(x,y) full_join(x,y, by="date"), wv_list)

# 5. Keep only actual Wikipedia‐data dates (>= July 2015)
wv_all <- wv_all %>%
  filter(date >= as.Date("2015-07-01"))

# 6. Show the pageviews table
print(wv_all)

# 7. Merge pageviews into returns and run OLS loop
results <- data.frame(Topic=character(), Beta=numeric(), t_NW=numeric(), R2=numeric(),
                      stringsAsFactors=FALSE)

for(article in topics) {
  df <- df_base %>%
    inner_join(wv_all, by="date") %>%         # inner_join drops pre-2015 automatically
    filter(!is.na(.data[[article]])) %>%
    mutate(X = as.numeric(scale(.data[[article]])))
  
  mod      <- lm(Rlead ~ X, data=df)
  r2       <- summary(mod)$r.squared
  nw_vcov  <- NeweyWest(mod, lag=3, prewhite=FALSE)
  nw_test  <- coeftest(mod, vcov.=nw_vcov)
  
  results <- rbind(results, data.frame(
    Topic = article,
    Beta  = round(coef(mod)["X"],4),
    t_NW  = round(nw_test["X","t value"],4),
    R2    = round(r2,4),
    stringsAsFactors=FALSE
  ))
}

# 8. Print regression summary
print(results)
