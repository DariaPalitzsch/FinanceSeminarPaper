
###### This Script contains all functions needed for our analysis
#The functions are sorted by use in "Analysis_Clean.R"

#Helper to fetch & tidy one series for wikipedia pageviews
fetch_wv <- function(article, df_dates) {
  
  start   <- paste0(format(min(df_dates), "%Y%m"), "01") # Create the start date string in "yyyymm01" format using the minimum date in df_dates
  end     <- paste0(format(max(df_dates), "%Y%m"), "01") # Create the end date string in "yyyymm01" format using the maximum date in df_dates
  
  url     <- paste0(
    "https://wikimedia.org/api/rest_v1/metrics/pageviews/per-article/",
    "en.wikipedia/all-access/user/", URLencode(article),
    "/monthly/", start, "/", end
  ) # Construct the API URL to fetch monthly pageviews for the given article
  
  resp    <- GET(url, user_agent("R script")) # Send a GET request to the Wikimedia API with a custom user agent
  
  stop_for_status(resp)  # Check if the request was successful; stop with an error if not
  
  js      <- content(resp, "text", encoding="UTF-8") %>% fromJSON() # Parse the JSON response into an R list
  
  tibble(
    date = as.Date(paste0(substr(js$items$timestamp,1,6), "01"), "%Y%m%d"),
    !!article := js$items$views
  )  # Extract and return a tibble with the date and corresponding pageviews
  
} #end function fetch_wv

# Merge a list of trends data frames into one wide table
merge_all_trends <- function(df_dates, trends_list) {
  out <- df_dates %>% 
    select(date)
  
  for (nm in names(trends_list)) {
    out <- left_join(out, trends_list[[nm]], by = "date")
  }
  out %>% 
    filter(if_any(all_of(names(trends_list)), ~ !is.na(.)))
}

# Run forecasts for all topics and return combined results
run_all_forecasts <- function(df_returns, df_trends, topics, lag_nw = 3) {
  purrr::map_dfr(topics,
                 function(x) fit_topic_ols(df_returns, df_trends, x, lag_nw))
}

# Fit one-month-ahead OLS for a topic and return summary
fit_topic_ols <- function(df_returns, df_trends, topic, lag_nw = 3) {
  df <- df_returns %>%
    dplyr::inner_join(df_trends, by = "date") %>%
    dplyr::filter(!is.na(.data[[topic]])) %>%
    dplyr::mutate(
      X = as.numeric(scale(.data[[topic]])),
      Y = Rlead * 100
    )
  if (nrow(df) < 12) return(NULL)
  fit <- stats::lm(Y ~ X, data = df)
  r2  <- summary(fit)$r.squared
  vc  <- sandwich::NeweyWest(fit, lag = lag_nw, prewhite = FALSE)
  tst <- lmtest::coeftest(fit, vcov. = vc)
  tibble::tibble(
    Topic = topic,
    Beta  = round(coef(fit)["X"], 4),
    t_NW  = round(tst["X","t value"], 4),
    R2    = round(r2, 4)
  )
}

# Out-of-Sample Forecasting with Expanding Window

run_oos_forecast <- function(df_returns, df_trends, topic, start_year = 2016) {
  df <- df_returns %>%
    dplyr::inner_join(df_trends, by = "date") %>%
    dplyr::filter(!is.na(.data[[topic]])) %>%
    dplyr::mutate(
      X = as.numeric(scale(.data[[topic]])),
      Y = Rlead * 100
    )
  
  df <- df %>% dplyr::arrange(date)
  oos_start <- which(lubridate::year(df$date) == start_year)[1]
  
  if (is.na(oos_start) || (nrow(df) - oos_start) < 12) {
    warning("Insufficient data for topic: ", topic, " — check date range or start_year.")
    return(tibble::tibble(Topic = topic, R2_OS = NA, n = NA))
  }
  
  preds <- actuals <- numeric()
  
  for (i in oos_start:(nrow(df) - 1)) {
    train <- df[1:i, ]
    test  <- df[i + 1, ]
    
    model <- stats::lm(Y ~ X, data = train)
    pred  <- predict(model, newdata = test)
    
    preds <- c(preds, pred)
    actuals <- c(actuals, test$Y)
  }
  
  mse_model <- mean((actuals - preds)^2)
  mse_bench <- mean((actuals - mean(df$Y[1:(oos_start - 1)]))^2)
  r2_os <- 1 - mse_model / mse_bench
  
  tibble::tibble(Topic = topic, R2_OS = round(r2_os, 4), n = length(actuals))
}

# Locate file in data/ or project root // helper function for read_trends()
find_path <- function(fname) {
  paths <- c(file.path("data", fname), fname)
  exists <- vapply(paths, file.exists, logical(1))
  if (!any(exists)) stop("File not found: ", fname)
  paths[which(exists)[1]]
}

# Read Google Trends CSVs saved in data/
read_trends <- function(filename, series_name) {
  path <- find_path(filename)
  raw  <- utils::read.csv(path, skip = 1, stringsAsFactors = FALSE, check.names = FALSE)
  df   <- raw[, 1:2]
  colnames(df) <- c("Monat", series_name)
  df %>%
    dplyr::transmute(
      date = as.Date(paste0(Monat, "-01"), "%Y-%m-%d"),
      !!series_name := as.numeric(.data[[series_name]])
    )
}






