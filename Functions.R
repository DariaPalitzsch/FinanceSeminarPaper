
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






