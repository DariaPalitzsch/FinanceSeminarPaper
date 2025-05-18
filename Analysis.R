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

#----------------------------------------------


#Version with Google Trends Data

# install.packages(c("gtrendsR","dplyr","lubridate","readxl","sandwich","lmtest"))
library(gtrendsR)
library(dplyr)
library(lubridate)
library(readxl)
library(sandwich)
library(lmtest)
# 1) Load returns data and compute R_{t+1}
df_base <- read_excel("S&P500_returndata.xlsx", sheet="Monthly") %>%
  rename(date_num = yyyymm, R = ret) %>%
  mutate(
    date  = as.Date(paste0(as.character(date_num), "01"), "%Y%m%d"),
    Rlead = lead(R, 1)
  ) %>%
  select(date, R, Rlead) %>%
  filter(!is.na(Rlead))

# 2) Full monthly index
full_dates <- data.frame(date = seq(
  from = as.Date("2004-01-01"),
  to   = max(df_base$date),
  by   = "month"
))


# 2. Define topics
topics <- c("War", "Pandemic", "Trade war", "Tariffs","Trump", "Tariff War","Reciprocal Tariffs")
gt_time <- "2004-01-01 2025-05-18"

# 4) Helper to pull & clean one term
fetch_gt_term <- function(term) {
  raw <- try(
    gtrends(keyword=term, time=gt_time, geo="US", gprop="web")$interest_over_time,
    silent=TRUE
  )
  if(inherits(raw, "try-error") || nrow(raw)==0) {
    warning("No data for ", term)
    return(tibble(date=as.Date(character()), !!term:=numeric()))
  }
  
  # force hits to character, map "<1"→0.5, then numeric
  raw <- raw %>%
    mutate(hits = as.character(hits),
           hits_num = ifelse(hits=="<1", 0.5, as.numeric(hits))) %>%
    mutate(month = floor_date(date, "month"))
  
  # show first few rows so you can confirm what you got
  cat("---- raw data for", term, "----\n")
  print(head(raw, 6))
  
  # aggregate to monthly
  out <- raw %>%
    group_by(month) %>%
    summarise(!!term := mean(hits_num, na.rm=TRUE)) %>%
    rename(date = month)
  return(out)
}

# 5) Fetch all into one wide table
gt_all <- full_dates
for(term in topics) {
  df_t <- fetch_gt_term(term)
  gt_all <- left_join(gt_all, df_t, by="date")
}

# 6) Keep only months with any data (post-2004)
gt_all <- gt_all %>%
  filter(date >= as.Date("2004-01-01"),
         rowSums(!is.na(select(., all_of(topics)))) > 0)

# 7) Inspect your final raw Trends table
cat("==== Final monthly Google Trends hits ====\n")
print(gt_all)

# 8) Loop through topics: merge, standardize, regress + NW
results <- data.frame(Topic=character(), Beta=numeric(), t_NW=numeric(), R2=numeric(),
                      stringsAsFactors=FALSE)

for(term in topics) {
  df <- df_base %>%
    inner_join(gt_all, by="date") %>%
    filter(!is.na(.data[[term]])) %>%
    mutate(X = scale(.data[[term]]))
  
  mod     <- lm(Rlead ~ X, data=df)
  r2      <- summary(mod)$r.squared
  nw      <- NeweyWest(mod, lag=3, prewhite=FALSE)
  nw_test <- coeftest(mod, vcov.=nw)
  
  results <- rbind(results, data.frame(
    Topic = term,
    Beta  = round(coef(mod)["X"],4),
    t_NW  = round(nw_test["X","t value"],4),
    R2    = round(r2,4),
    stringsAsFactors=FALSE
  ))
}

# 9) Show regression summary
cat("==== Predictive Regression Results ====\n")
print(results)






















#install.packages("gtrendsR")
library(gtrendsR)
library(dplyr)
library(lubridate)

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