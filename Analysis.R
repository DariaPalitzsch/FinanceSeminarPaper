
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

#Version with Google Trends Data

# Full monthly index
full_dates <- data.frame(date = seq(
  from = as.Date("2004-01-01"),
  to   = max(df_base$date),
  by   = "month"
))


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


# 5-year sub-windows from start to end
make_windows <- function(start, end, span_years=5) {
  starts <- seq(start, end, by = paste0(span_years, " years"))
  ends   <- pmin(starts %m+% years(span_years) - days(1), end)
  data.frame(s = starts, e = ends)
}

fetch_gt_term <- function(term, overall_start, overall_end) {
  wins <- make_windows(overall_start, overall_end, span_years=5)
  all_chunks <- list()
  
  for(i in seq_len(nrow(wins))) {
    t0 <- wins$s[i]; t1 <- wins$e[i]
    window_str <- paste0(format(t0, "%Y-%m-%d"), " ", format(t1, "%Y-%m-%d"))
    
    res <- try(
      gtrends(keyword=term, time=window_str, geo="US", gprop="web")$interest_over_time,
      silent=TRUE
    )
    if(inherits(res, "try-error") || nrow(res)==0) next
    
    chunk <- res %>%
      mutate(hits = as.character(hits),
             hits = ifelse(hits == "<1", 0.5, as.numeric(hits)),
             date = as.Date(date)) %>%
      select(date, hits)
    
    # Rescale chunk so its max matches the overall max you'll see
    # We'll align all chunks later by dividing by their own max and multiplying by the global max.
    chunk_max <- max(chunk$hits, na.rm=TRUE)
    chunk <- chunk %>% mutate(hits_rescaled = hits / chunk_max)
    all_chunks[[length(all_chunks)+1]] <- chunk
  }
  if(length(all_chunks)==0) {
    warning("No Google Trends data for term ", term)
    return(tibble(date=as.Date(character()), !!term := numeric()))
  }
  
  # bind all chunks and then scale them so the full series max is 100
  full <- bind_rows(all_chunks) %>%
    arrange(date) %>%
    group_by(date) %>%
    summarise(h = mean(hits_rescaled, na.rm=TRUE)) %>%
    ungroup() %>%
    mutate(h = h / max(h, na.rm=TRUE) * 100,
           month = floor_date(date, "month")) %>%
    group_by(month) %>%
    summarise(!!term := mean(h, na.rm=TRUE)) %>%
    rename(date = month)
  
  return(full)
}

# Example usage:
overall_start <- as.Date("2004-01-01")
overall_end   <- as.Date("2025-05-01")
df_war <- fetch_gt_term("War", overall_start, overall_end)
head(df_war)










#-----------------------------------
#manually downloading the google trends data from https://trends.google.com/trends/explore?date=all&q=Tariffs&hl=en

#The csv-data is stored in the "data" folder

#Loading the csv files into R:

pandemic_trends <- read.csv("data/pandemic_trends.csv")
tariffs_trends <- read.csv("data/tariffs_trends.csv")
trade_war_trends <- read.csv("data/trade_war_trends.csv")
war_trends <- read.csv("data/war_trends.csv")

#merge all into one file: 
gt_analysis <- cbind(pandemic_trends, tariffs_trends, trade_war_trends, war_trends)
#remove first row
gt_analysis <- gt_analysis[-1,]
#give meaningful column names
colnames(gt_analysis) <- c("pandemic", "tariffs", "trade_war", "war")

gt_analysis <- gt_analysis %>% 
  mutate(pandemic = ifelse(pandemic == "<1", 0, as.numeric(pandemic)), #change the column values to numeric 
         tariffs = ifelse(tariffs == "<1", 0, as.numeric(tariffs)),
         trade_war = ifelse(trade_war == "<1", 0, as.numeric(trade_war)),
         war = ifelse(war == "<1", 0, as.numeric(war))
         )


  









