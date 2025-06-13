

# ---------------
# Replication and Extension of Hirhleifer et al. (2024)
# Media Discourse and Excess Returns using Google Trends
# --------------


# Load Packages
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

# Load self-written Functions from Functions_Clean.R
source("Functions_Clean.R")

# Read return data from S&P 500
df_returns <- read_excel("data/SP500_returndata.xlsx", sheet = "Monthly") %>%
  rename(date_num = yyyymm, R = ret) %>%
  mutate(
    date  = as.Date(paste0(as.character(date_num), "01"), "%Y%m%d"),
    Rlead = lead(R, 1)
  ) %>%
  select(date, R, Rlead) %>%
  filter(!is.na(Rlead))


# --------------------
# Part A: Wikipedia Analysis
# ----------------

wiki_articles <- c("War", "Pandemic", "Boycott", "Panic", "Tariff", "Trade_war", "Consumption", "Stock_bubble")

wiki_list <- lapply(wiki_articles, fetch_wv, df_dates = df_returns$date) #uses the function in Functions_Clean.R
names(wiki_list) <- wiki_articles

df_wiki <- merge_all_trends(df_returns, wiki_list)

wiki_in <- run_all_forecasts(df_returns, df_wiki, wiki_articles)

wiki_out <- purrr::map_dfr(wiki_articles, function(topic) {
  run_oos_forecast(df_returns, df_wiki, topic, start_year = 2016)
})


wiki_combined <- wiki_in %>% 
  inner_join(wiki_out, by = "Topic") %>% 
  select(Topic, Beta, t_NW, R2, R2_OS, n)


# ----------------
# Part B: Google Trends Analysis
# ---------------


gt_topics <- c(
  "War" = "war_trends.csv",
  "Pandemic" = "pandemic_trends.csv",
  "Boycott" = "boycott_trends.csv",
  "Panic" = "panic_trends.csv",
  "Tariff" = "tariffs_trends.csv",
  "Trade_war" = "trade_war_trends.csv",
  "Consumption" = "consumption_trends.csv",
  "Stock_bubble" = "stock_bubble_trends.csv"
)

gt_list <- lapply(names(gt_topics), function(name) {
  read_trends(gt_topics[[name]], name)
})
names(gt_list) <- names(gt_topics)

df_gt <- merge_all_trends(df_returns, gt_list)

gt_in <- run_all_forecasts(df_returns, df_gt, names(gt_topics))
gt_out <- purrr::map_dfr(names(gt_topics), function(topic) {
  run_oos_forecast(df_returns, df_gt, topic, start_year = 2006)
})

gt_combined <- gt_in %>% 
  inner_join(gt_out, by = "Topic") %>% 
  select(Topic, Beta, t_NW, R2, R2_OS, n)

# --------------
# Part C: Comparison of Wikipedia and Google Trends Data 
# -------------

wiki_combined$Source <- "Wikipedia"
gt_combined$Source <- "Google Trends"

comparison <- bind_rows(wiki_combined, gt_combined) %>%
  arrange(desc(R2_OS))






