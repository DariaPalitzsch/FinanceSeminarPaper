
###### This Script contains all functions needed for our analysis

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


#Helper to pull & clean one term for google trends data

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
  
} #end function fetch_gt_term

#-----------------------------------------------------------------------------------------------------------------------
# 1. Locate file in data/ or project root
find_path <- function(fname) {
  paths <- c(file.path("data", fname), fname)
  exists <- vapply(paths, file.exists, logical(1))
  if (!any(exists)) stop("File not found: ", fname)
  paths[which(exists)[1]]
}

# 2. Read returns from Excel and compute one-month-ahead return
read_returns <- function(fname, sheet = "Monthly") {
  path <- find_path(fname)
  df   <- readxl::read_excel(path, sheet = sheet)
  df   <- df %>%
    dplyr::rename(date_num = yyyymm, R = ret) %>%
    dplyr::mutate(
      date  = as.Date(paste0(as.character(date_num), "01"), "%Y%m%d"),
      Rlead = dplyr::lead(R, 1)
    ) %>%
    dplyr::select(date, R, Rlead) %>%
    dplyr::filter(!is.na(Rlead))
  return(df)
}

# 3. Read Google Trends CSVs saved in data/
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

# 4. Merge a list of trends data frames into one wide table
merge_all_trends <- function(df_dates, trends_list) {
  out <- df_dates %>% dplyr::select(date)
  for (nm in names(trends_list)) {
    out <- dplyr::left_join(out, trends_list[[nm]], by = "date")
  }
  out %>% dplyr::filter(dplyr::if_any(dplyr::all_of(names(trends_list)), ~ !is.na(.)))
}

# 5. Fit one-month-ahead OLS for a topic and return summary
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

# 6. Run forecasts for all topics and return combined results
run_all_forecasts <- function(df_returns, df_trends, topics, lag_nw = 3) {
  purrr::map_dfr(topics,
                 function(x) fit_topic_ols(df_returns, df_trends, x, lag_nw))
}

