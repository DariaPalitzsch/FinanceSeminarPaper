

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
library(ggplot2)
library(xtable)
library(tidyr)
library(scales)
library(patchwork)
library(rlang)

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

wiki_articles <- c("War", "Pandemic", "Panic", "Bank_run", "Business_confidence", "Poverty", "Savings", "Consumption", "Inflation", "Unemployment", "Technology", "Real_estate_bubble", "Crash", "Stock_bubble", 
                   "Speculation", "Bear_market", "Boycott", "Wage", "Tariff", "Trade_war")

wiki_list <- lapply(wiki_articles, fetch_wv, df_dates = df_returns$date) #uses the function in Functions_Clean.R
names(wiki_list) <- wiki_articles

df_wiki <- merge_all_trends(df_returns, wiki_list) # time series from 07/15 until 11/24

wiki_in <- run_all_forecasts(df_returns, df_wiki, wiki_articles) # in sample analysis (Standard predictive model)

########### In-Sample Analysis: 

# Replication of Figure 2: 
# Instead of creating word clouds as in Figure 2, use a simplified Topic Attention Measure based on Wikipedia data to construct a figure of which topics captured public attention over time.
# scaled pageviews on y axis and years on x axis
plot_wikipedia_topic_attention(
  df_wiki, topics = wiki_articles,
  scale_each = TRUE, 
  output_file = "figure2_topics.pdf"
)

# Replication of Table 2:
# see table, additionally q1, q3 and AC1
wiki_summary <- generate_topic_summary_table(df_wiki, file_out = "table2_wikipedia.tex")


# Replication of Figure 4 for Tariffs and Trade_war
# peaks coudld use some explaination which event happened that caused it
plot_figure_4(df_returns, df_wiki, "Tariff", output_file = "figure4_tariffs_wiki.pdf")
plot_figure_4(df_returns, df_wiki, "Trade_war", output_file = "figure4_tradewar_wiki.pdf")


#Replication of Table 3

# Table 3 replication - Pre-2020 period (Historical, replication)
table3_wiki_pre2020 <- replicate_table3(
  df_returns, df_wiki,
  topics = wiki_articles,
  start = "2015-01-01", end = "2019-12-31"
)

# Table 3 replication - Post-2020 period (Own contribution)
table3_wiki_post2020 <- replicate_table3(
  df_returns, df_wiki,
  topics = wiki_articles,
  start = "2020-01-01", end = "2024-12-31"
)

table3_combined <- full_join(
  table3_wiki_pre2020 %>% rename(Beta_Pre2020 = Beta, t_Pre2020 = t_stat, R2_Pre2020 = R2),
  table3_wiki_post2020 %>% rename(Beta_Post2020 = Beta, t_Post2020 = t_stat, R2_Post2020 = R2),
  by = "Topic"
)


####Out-of-sample: 

df_wiki_oos <- inner_join(df_returns, df_wiki, by = "date")

wiki_out <- purrr::map_dfr(wiki_articles, function(topic) {
  run_oos_forecast(df_wiki_oos, topic, start_year = 2016)
})




###### Replication of Table 8 ###########

#Step 1) Load Goyal & Welch predictors
df_goyal <- read_excel("data/Data2024_GoyalWelch.xlsx", sheet = "Monthly") %>% 
  rename(date_num = yyyymm) %>% 
  mutate(
    date = as.Date(paste0(date_num, "01"), format = "%Y%m%d")
  ) %>%
  select(date, DP = `d/p`, DY = `d/y`, EP = `e/p`, DE = `d/e`, TBL = tbl)
  
# Step 2) Merge with return data
df_econ <- df_returns %>%
  inner_join(df_goyal, by = "date") #%>% 
  #select(-Rlead)

# Step 3) Run OOS forecast for each economic predictor
#econ_predictors <- intersect(c("DP", "DY", "EP", "DE", "TBL"), colnames(df_econ))
econ_predictors <- names(df_econ)[c(2:7)]  # 

econ_out <- purrr::map_dfr(econ_predictors, function(p) {
  run_oos_forecast(df_econ, p, start_year = 2016)
})

# Step 4: Combine with Wikipedia Predictors and Format Table 8

table8_combined <- bind_rows(
  wiki_out %>% mutate(Source = "Wikipedia"),
  econ_out %>% mutate(Source = "Economic")
) %>%
  select(Source, Topic, R2_OS) %>% 
  arrange(desc(R2_OS)) %>%
  mutate(R2_OS = round(R2_OS, 3))
  
 # Step 5: Export Latex Table
xtable(table8_combined,
       caption = "Out-of-Sample $R^2$ for Wikipedia and Economic Predictors (2016–2024)",
       label = "tab:table8_combined",
       digits = c(0, 0, 0, 3)) %>%
  print(type = "latex", include.rownames = FALSE, file = "table8_combined.tex")



##############


##### CSPE Plot: 

plot_cspe_oos(df_returns, df_wiki, topic = "War", start_year = 2016, output_file = "cspe_war_wiki.pdf")


wiki_combined <- wiki_in %>% 
  inner_join(wiki_out, by = "Topic") %>% 
  select(Topic, Beta, t_NW, R2, R2_OS, n)


#Strong performers (R2_OS > 0.15): Bear_market, Speculation, Crash, Consumption, War, Real_estate_bubble, Panic, Stock_bubble
#Moderate Topics (R2_OS between 0.10 an 0.15): Business_Confidence, Poverty, Savings, Unemployment, Technology, Inflation, Tariff, Wage




# ----------------
# Part B: Google Trends Analysis
#Google Trends Analysis for Topics that:
  # - have a weak R2 in Wikipedia analysis
  # - could more likely be good indicators for google search than for a Wikipedia page
  # - strong performance for wikipedia pageviews (benchmark it for Google Trends data)
# ---------------

#Pandemic: Strong wiki_pageviews performance 
#Panic: low R2 in wiki_pageviews, but could be a plausible GT signal
#Bank_run: Good R2 in Table 3 pre-2020 -> test consistency in GT
#Boycott: High t and R2 -< test generality
#Tariff: moderate performance for wiki_pageviews (R2 = 0.009) -> GT may yield stronger results
#Trade_war: Weak in wiki_pageviews but visible trend spikes 
#Bear_market: Strongest overall signal in Wikipedia _> test if GT can match
#Speculation: Moderate in Wikipedia; possibly more popular in GT searches
#Stock_bubble: Low R2 in wikipedia_pageviews but intuitively GT relevant
#Crash: very weak wikipedia-pageviews signal, but crisis events (e.g. 2020) may show GT strength


gt_topics <- c(
  "Pandemic" = "pandemic_trends.csv",
  "Panic" = "panic_trends.csv",
  "Bank_run" = "bank_run_trends.csv",
  "Boycott" = "boycott_trends.csv",
  "Tariff" = "tariffs_trends.csv",
  "Trade_war" = "trade_war_trends.csv",
  "Bear_market" = "bear_market_trends.csv",
  "Speculation" = "speculation_trends.csv",
  "Stock_bubble" = "stock_bubble_trends.csv",
  "Crash" = "crash_trends.csv"
)



gt_list <- lapply(names(gt_topics), function(name) {
  read_trends(gt_topics[[name]], name)
})
names(gt_list) <- names(gt_topics)

df_gt <- merge_all_trends(df_returns, gt_list)

gt_in <- run_all_forecasts(df_returns, df_gt, names(gt_topics))


# --------------
# Part C: Comparison of Wikipedia and Google Trends Data 
# -------------

wiki_combined$Source <- "Wikipedia"
gt_combined$Source <- "Google Trends"

comparison <- bind_rows(wiki_combined, gt_combined) %>%
  arrange(desc(R2_OS))


# ---------
# Part D: Visual Analysis: 
# ---------


# #Plot 1 to show how media attention to war evolves over time 
# 
# df_plot_wiki_war <- df_wiki %>%
#   select(date, War) %>% 
#   mutate(War_z = scale(War))
# 
# df_plot_gt_war <- df_gt %>% 
#   select(date, War) %>% 
#   mutate(War_z = scale(War))
# 
# events <- data.frame(
#   date = as.Date(c("2003-03-01", "2008-08-01", "2014-03-01", "2020-01-01", "2022-02-01")),
#   label = c("Iraq War", "Georgia War", "Crimea", "US–Iran", "Ukraine Invasion")
# )
# 
# ggplot() +
#   geom_line(data = df_plot_wiki_war, aes(x=date, y=War_z), color = "steelblue" ) +
#   geom_line(data = df_plot_gt_war, aes(x=date, y = War_z), color = "darkgreen") +
#   geom_vline(data = events, aes(xintercept = date), linetype = "dashed", color = "darkred") +
#   geom_text(data = events, aes(x = date, y = 2, label = label), angle = 90, vjust = -0.5, hjust = 0, size = 3.2) +
#   labs(title = "Standardized War Topic Index Over Time",
#        subtitle = "Wikipedia pageviews (Standardized z-score)",
#        y = "Standardized Index", x = "Date") + 
#   scale_x_date(date_labels = "%Y", date_breaks = "2 years") +
#   theme_minimal()
# 
# 
# #Plot 2 to show which topics significantly predict returns (visualisation of Table 3)
# #visualizes the estimated regression coefficients from our predictive model
# #each bar shows the size and direction of the effect 
# #the label on the bay shors the Newey-West t-statistic
# 
# df_beta <- wiki_in 
# 
# ggplot(df_beta, aes(x = reorder(Topic, Beta), y = Beta)) +
#   geom_col(fill = "darkred") +
#   geom_text(aes(label = round(t_NW, 2)), vjust = -0.5) +
#   geom_hline(yintercept = 0, linetype = "dashed") +
#   labs(title = "Estimated β Coefficients and t-Statistics", y = "β Estimate", x = "Topic") +
#   coord_flip() +
#   theme_minimal()
# 
# 
# #Plot 3 to compare the forecasting power of different discourse themes
# 
# df_r2 <- wiki_out
# 
# ggplot(df_r2, aes(x = reorder(Topic, R2_OS), y = R2_OS)) +
#   geom_col(fill = "darkgreen") +
#   geom_text(aes(label = round(R2_OS, 3)), vjust = -0.5) +
#   labs(title = "Out-of-Sample R2 by Topic",
#         y = "R2 (Out-of-Sample)", x = "Topic") +
#   coord_flip() +
#   theme_minimal()
# 
# 
