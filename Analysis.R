
#Load all required packages for the analysis

#install.packages(c("readxl","dplyr","sandwich","lmtest","httr","jsonlite","zoo","lmtest","lubridate","gtrendsR"))  # run once
library(readxl)
library(httr)
library(jsonlite)
library(dplyr)
library(gtrendsR)
library(lubridate)
library(broom)
library(dplyr)
library(zoo)
library(sandwich)
library(lmtest)
library(stargazer)

#run all the functions from "functions.R"
source("Functions.R")


#------------------------------------------------------------------------------------------------------------------

# Load your returns data
df_base <- read_excel("data/SP500_returndata.xlsx", sheet = "Monthly") %>%
  rename(date_num = yyyymm, R = ret) %>%
  mutate(
    date  = as.Date(paste0(as.character(date_num), "01"), "%Y%m%d"),
    Rlead = lead(R, 1)
  ) %>%
  select(date, R, Rlead) %>%
  filter(!is.na(Rlead))

#-------------------------------------------------------------------------------------------------------------------

# Define the Wikipedia pageviews topics
wiki_topics <- c("War", "Pandemic", "Trade_wars", "Tariffs","Trump","China","Trade War")  

# Build a wide table of all pageviews
wv_list <- lapply(wiki_topics, fetch_wv, df_dates = df_base$date) #uses the function in Functions.R
wv_all  <- Reduce(function(x,y) full_join(x,y, by="date"), wv_list)

# Keep only actual Wikipedia‐data dates (>= July 2015)
wv_all <- wv_all %>%
  filter(date >= as.Date("2015-07-01"))

# Show the pageviews table
wv_all

# Merge pageviews into returns and run OLS loop
results_pageviews <- data.frame(Topic=character(), Beta=numeric(), t_NW=numeric(), R2=numeric(),
                      stringsAsFactors=FALSE)

for(article in wiki_topics) {
  df <- df_base %>%
    inner_join(wv_all, by="date") %>%         # inner_join drops pre-2015 automatically
    filter(!is.na(.data[[article]])) %>%
    mutate(X = as.numeric(scale(.data[[article]])))
  
  mod      <- lm(Rlead ~ X, data=df)
  r2       <- summary(mod)$r.squared
  nw_vcov  <- NeweyWest(mod, lag=3, prewhite=FALSE)
  nw_test  <- coeftest(mod, vcov.=nw_vcov)
  
  results_pageviews <- rbind(results_pageviews, data.frame(
    Topic = article,
    Beta  = round(coef(mod)["X"],4),
    t_NW  = round(nw_test["X","t value"],4),
    R2    = round(r2,4),
    stringsAsFactors=FALSE
  ))
}

# 8. Print regression summary
results_pageviews

#-------------------------------------------------------

#Version with Google Trends Data - Doesn't work atm because of problems with connection to API

# Full monthly index
full_dates <- data.frame(date = seq(
  from = as.Date("2004-01-01"),
  to   = max(df_base$date),
  by   = "month"
))

gt_pandemic  <- read_gt("pandemic_trends.csv", "Pandemic")
gt_trade_war <- read_gt("trade_war_trends.csv", "Trade_war")
gt_tariffs   <- read_gt("tariffs_trends.csv", "Tariffs")

# Define google trends topics 
gt_topics <- c("War", "Pandemic", "Trade war", "Tariffs","Trump", "Tariff War","Reciprocal Tariffs")
gt_time <- "2004-01-01 2025-05-18"

# Fetch all into one wide table
gt_all <- full_dates

for(term in gt_topics) {
  df_t <- fetch_gt_term(term) #function defined in Functions.R
  gt_all <- left_join(gt_all, df_t, by="date")
} #close for-loop

# Keep only months with any data (post-2004)
gt_all <- gt_all %>%
  filter(date >= as.Date("2004-01-01"),
         rowSums(!is.na(select(., all_of(gt_topics)))) > 0)

# Inspect your final raw Trends table
cat("==== Final monthly Google Trends hits ====\n")
gt_all

# Loop through topics: merge, standardize, regress + NW
gt_results <- data.frame(Topic=character(), Beta=numeric(), t_NW=numeric(), R2=numeric(),
                      stringsAsFactors=FALSE)

for(term in gt_topics) {
  df <- df_base %>%
    inner_join(gt_all, by="date") %>%
    filter(!is.na(.data[[term]])) %>%
    mutate(X = scale(.data[[term]]))
  
  mod     <- lm(Rlead ~ X, data=df)
  r2      <- summary(mod)$r.squared
  nw      <- NeweyWest(mod, lag=3, prewhite=FALSE)
  nw_test <- coeftest(mod, vcov.=nw)
  
  gt_results <- rbind(gt_results, data.frame(
    Topic = term,
    Beta  = round(coef(mod)["X"],4),
    t_NW  = round(nw_test["X","t value"],4),
    R2    = round(r2,4),
    stringsAsFactors=FALSE
  ))
}

# Show regression summary
cat("==== Predictive Regression Results ====\n") 
gt_results


#--------------------------------

#GOOGLE TRENDS WITH MANUAL DATA INPUT

#manually downloading the google trends data from https://trends.google.com/trends/explore?date=all&q=Tariffs&hl=en

#The csv-data is stored in the "data" folder

#Loading the csv files into R:

# 1) Read returns
df_returns <- read_returns("SP500_returndata.xlsx")  # or inline read + mutate

# 2) Load trends
trends_list <- list(
  War       = read_trends("war_trends.csv",       "War"),
  Pandemic  = read_trends("pandemic_trends.csv",  "Pandemic"),
  Trade_war = read_trends("trade_war_trends.csv","Trade_war"),
  Tariffs   = read_trends("tariffs_trends.csv",   "Tariffs")
)

# 3) Merge
df_trends <- merge_all_trends(df_returns, trends_list)

# 4) Forecast
results <- run_all_forecasts(df_returns, df_trends,
                             topics = names(trends_list))

print(results)


#----------- 

#Analysis with Google trends data


analysis_df <- left_join(df_returns, df_trends, by = "date")

#predictive regression
model_war <- lm(Rlead ~ War, data = analysis_df)
model_pandemic <- lm(Rlead ~ Pandemic, data = analysis_df)
model_trade_war <- lm(Rlead ~ Trade_war, data = analysis_df)
model_tariffs <- lm(Rlead ~ Tariffs, data = analysis_df)

# Newey-West robust standard errors (lag = 4 is standard for monthly data)
se_war <- sqrt(diag(NeweyWest(model_war, lag = 4)))
se_pandemic <- sqrt(diag(NeweyWest(model_pandemic, lag = 4)))
se_tariffs  <- sqrt(diag(NeweyWest(model_tariffs,  lag = 4)))
se_tradewar <- sqrt(diag(NeweyWest(model_trade_war, lag = 4)))

# Robust coefficient test
coeftest(model_war, vcov. = nw_se)

# R-squared
summary(model_war)$r.squared

#replicating table 3 from the paper: can be extended with other google trends data
stargazer(
  model_war, model_pandemic, model_tariffs, model_trade_war,
  se = list(se_war, se_pandemic, se_tariffs, se_tradewar),
  title = "Predictive Power of Media Discourse Topics for Excess Returns",
  dep.var.labels = "Excess Return $R_{t+1}$",
  covariate.labels = c("War", "Pandemic", "Tariffs", "Trade War"),
  column.labels = c("War", "Pandemic", "Tariffs", "Trade War"),
  type = "latex",
  keep.stat = c("n", "rsq"),
  digits = 3,
  no.space = TRUE,
  out = "table3.tex"
)

#this table was stored locally and can be included in our latex-document with: \input{table3.tex}
