
#Load all required packages for the analysis

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

library(dplyr)       # data manipulation
library(lubridate)   # date handling
library(readxl)      # read Excel files
library(sandwich)    # Newey–West covariance estimator
library(lmtest)      # coeftest for robust SEs

# 1. Helper: locate a file in "data/" or project root ----
find_path <- function(fname) {
  paths <- c(file.path("data", fname), fname)
  exists <- vapply(paths, file.exists, logical(1))
  if (!any(exists)) stop("File not found: ", fname)
  return(paths[which(exists)[1]])
}

# 2. Read & prepare returns data ----
ret_file   <- find_path("SP500_returndata.xlsx")
df_returns <- read_excel(ret_file, sheet = "Monthly") %>%
  rename(date_num = yyyymm, R = ret) %>%
  mutate(
    date  = as.Date(paste0(as.character(date_num), "01"), "%Y%m%d"),
    Rlead = lead(R, 1)
  ) %>%
  select(date, R, Rlead) %>%
  filter(!is.na(Rlead))

# 3. Helper: read Google Trends CSVs ----
#    Expects CSVs in data/ or root, skip the first metadata line
read_trends <- function(fname, series_name) {
  path <- find_path(fname)
  raw  <- read.csv(path, skip = 1, stringsAsFactors = FALSE, check.names = FALSE)
  df   <- raw[, 1:2]
  colnames(df) <- c("Monat", series_name)
  df %>%
    transmute(
      date = as.Date(paste0(Monat, "-01"), "%Y-%m-%d"),
      !!series_name := as.numeric(.data[[series_name]])
    )
}
# 4. Load all Trends ----
gt_war      <- read_trends("war_trends.csv",       "War")
gt_pandemic <- read_trends("pandemic_trends.csv",  "Pandemic")
gt_trade    <- read_trends("trade_war_trends.csv", "Trade_war")
gt_tariffs  <- read_trends("tariffs_trends.csv",   "Tariffs")

# 5. Merge into one wide table ----
df_trends <- df_returns %>%
  select(date) %>%
  left_join(gt_war,      by = "date") %>%
  left_join(gt_pandemic, by = "date") %>%
  left_join(gt_trade,    by = "date") %>%
  left_join(gt_tariffs,  by = "date") %>%
  filter(if_any(War:Tariffs, ~ !is.na(.)))

cat("=== Raw monthly Google Trends indices ===\n")
print(head(df_trends, 10))

# 6. One‐Month‐Ahead OLS + Newey–West ----
topics <- c("War","Pandemic","Trade_war","Tariffs")
results <- tibble(Topic=character(), Beta=numeric(), t_NW=numeric(), R2=numeric())

for (term in topics) {
  df <- df_returns %>%
    inner_join(df_trends, by = "date") %>%
    filter(!is.na(.data[[term]])) %>%
    mutate(
      X = as.numeric(scale(.data[[term]])),  # standardize predictor
      Y = Rlead * 100                         # convert to percent
    )
  if (nrow(df) < 12) next
  
  fit <- lm(Y ~ X, data = df)
  r2  <- summary(fit)$r.squared
  nw_vcov <- NeweyWest(fit, lag = 3, prewhite = FALSE)
  nw_test <- coeftest(fit, vcov. = nw_vcov)
  
  results <- results %>%
    add_row(
      Topic = term,
      Beta  = round(coef(fit)["X"],    4),
      t_NW  = round(nw_test["X","t value"], 4),
      R2    = round(r2,               4)
    )
}

cat("=== One‐Month‐Ahead Forecast Results ===\n")
print(results)

