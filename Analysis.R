install.packages(c("readxl","dplyr","sandwich","lmtest","httr","jsonlite","zoo","lmtest","lubridate","gtrendsR"))  # run once
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


#----------------------------------------------------------------------------------------
# 1. Load data
df <- read_excel("S&P500_returndata.xlsx", sheet = "Monthly") %>%
  rename(
    date_num = yyyymm,
    R = ret,
    dp = `d/p`,
    dy = `d/y`,
    ep = `e/p`,
    de = `d/e`,
    svar = svar,
    tbl = tbl
  ) %>%
  # convert YYYYMM numeric to Date at first of month
  mutate(
    date = as.Date(paste0(as.character(date_num), "01"), format = "%Y%m%d")
  ) %>%
  select(date, R, dp, dy, ep, de, svar, tbl)

# 2. Create one‐month‐ahead return
df <- df %>%
  arrange(date) %>%
  mutate(R_lead = lead(R, 1)) %>%
  filter(!is.na(R_lead))

# 3. Specify your predictor
pred <- "dp"   # change to "dy", "ep", "svar", "tbl", etc.

# 4. Fit OLS model R_{t+1} ~ X_t
form <- as.formula(paste0("R_lead ~ ", pred))
mod  <- lm(form, data = df)

# 5. Extract in‐sample R²
r2 <- summary(mod)$r.squared

# 6. Compute Newey–West SEs (lag = 3)
nw_vcov <- NeweyWest(mod, lag = 3, prewhite = FALSE)
nw_test  <- coeftest(mod, vcov. = nw_vcov)

# 7. Print results
cat("Predictor:", pred, "\n")
cat("In‐sample R²:", round(r2, 4), "\n\n")
print(nw_test)

#---------------------------------------------------
# 1. Lade deine Rendite- und Makrodaten
df <- read_excel("S&P500_returndata.xlsx", sheet = "Monthly") %>%
  rename(
    date_num = yyyymm,
    R = ret
  ) %>%
  mutate(
    date = as.Date(paste0(as.character(date_num), "01"), format = "%Y%m%d"),
    R_lead = lead(R, 1)
  ) %>%
  select(date, R, R_lead) %>%
  filter(!is.na(R_lead))

# 2. Define article and date range
article    <- "War"  # change to your topic title, spaces as underscores if needed
start_ym   <- format(min(df$date), "%Y%m")  # e.g. "201501"
end_ym     <- format(max(df$date), "%Y%m")  # e.g. "202504"
start      <- paste0(start_ym, "01")  # YYYYMMDD
end        <- paste0(end_ym, "01")    # YYYYMMDD

# 3. Fetch pageviews from Wikimedia REST API
url <- paste0(
  "https://wikimedia.org/api/rest_v1/metrics/pageviews/per-article/",
  "en.wikipedia/all-access/user/", URLencode(article), 
  "/monthly/", start, "/", end
)

resp <- GET(url, user_agent("R-script for academic research"))
stop_for_status(resp)
data <- content(resp, as = "text", encoding = "UTF-8") %>% fromJSON()

# 4. Parse and aggregate to a data.frame
wv_monthly <- data$items %>%
  as_tibble() %>%
  transmute(
    date = as.Date(paste0(substr(timestamp, 1, 6), "01"), "%Y%m%d"),
    X_war = views
  )

# 5. Merge into main df and standardize
df2 <- df %>%
  left_join(wv_monthly, by = "date") %>%
  filter(!is.na(X_war)) %>%
  mutate(X_war_std = as.numeric(scale(X_war)))

# 6. Run OLS with Newey–West SEs
mod <- lm(R_lead ~ X_war_std, data = df2)
r2  <- summary(mod)$r.squared
nw  <- NeweyWest(mod, lag = 3, prewhite = FALSE)
nw_test <- coeftest(mod, vcov. = nw)

# 7. Print results
cat("Wikipedia topic:", article, "\n")
cat("In-sample R²:", round(r2, 4), "\n\n")
print(nw_test)


