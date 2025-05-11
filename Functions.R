
###### Function to download the pageviews data from wikipedia #######

get_wikipedia_pageviews <- function(article, start, end) {
  base_url <- "https://wikimedia.org/api/rest_v1/metrics/pageviews/per-article"
  url <- paste0(base_url,
                "/en.wikipedia.org/all-access/user/",
                URLencode(article),
                "/monthly/", start, "/", end)
  
  response <- GET(url)
  data <- content(response, "parsed", simplifyVector = TRUE)
  
  if (is.null(data$items)) return(NULL)
  
  df <- as.data.frame(data$items)
  df <- df %>%
    transmute(
      date = as.Date(paste0(substr(timestamp, 1, 6), "01"), format = "%Y%m%d"),
      views = views
    )
  colnames(df)[2] <- paste0(tolower(article), "_views")
  return(df)
}
