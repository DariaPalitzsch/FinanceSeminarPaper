
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

# Generate A Plot similar to Figure 2 from Hirshleifer et al. (2024)

plot_wikipedia_topic_attention <- function(df_wiki, topics, scale_each = TRUE, output_file = NULL) {
  
  df_long <- df_wiki %>% 
    select(date, all_of(topics)) %>% 
    pivot_longer(cols = -date, names_to = "Topic", values_to = "Pageviews")
  
  if (scale_each) {
    df_long <- df_long %>% 
      group_by(Topic) %>% 
      mutate(Pageviews_scaled = scale(Pageviews)) %>%
      ungroup() 
    
    y_col <- "Pageviews_scaled"
    y_label <- "Standardized Pageviews"
  
  } else {
    y_col <- "Pageviews"
    y_label <- "Raw Pageviews"
  }
  
  p <- ggplot(df_long, aes(x = date, y = .data[[y_col]], color = Topic)) +
    geom_line(size = 0.8) +
    facet_wrap(~Topic, scales = "free_y", ncol = 3) +
    theme_minimal(base_size = 12) +
    labs(
      title = "Wikipedia Topic Attention Over Time", 
      y = y_label, 
      x = "Date"
    ) +
    theme(legend.position = "none")
  
  if (!is.null(output_file)) {
    ggsave(output_file, plot = p, width = 10, height = 6)
  }
  
  return(p)
  
  
}


# Regenerate Table 2 from the Hirshleifer et al. (2024)
generate_topic_summary_table <- function(df_topics, date_col = "date", file_out = NULL) {
  
  topic_cols <- setdiff(colnames(df_topics), date_col)
  
  # Helper to compute statistics for one topic
  summarize_topic <- function(topic_name) {
    x <- df_topics[[topic_name]]
    x <- x[!is.na(x)]
    
    tibble(
      Topic = topic_name,
      N = length(x),
      Mean = mean(x), 
      Median = median(x), 
      Q1 = quantile(x, 0.25),
      Q3 = quantile(x, 0.75), 
      SD = sd(x),
      AC1 = if (length(x) > 1) cor(x[-1], x[-length(x)], use = "complete.obs") else NA_real_
    )
  }
  
  #Apply to all topics columns
  results <- purrr::map_dfr(topic_cols, summarize_topic) %>%
    mutate(across(where(is.numeric), ~ round(., 3)))
  
  #output latex table
  if (!is.null(file_out)) {
    xtable(results, 
           caption = "Summary Statistics for Topic Attention Indices",
           label = "tab:topic_summary", 
           digits = c(0, rep(3, 8))) %>% 
      print(type = "latex", include.rownames = FALSE, file = file_out)
  }
  
  return(results)
  
}



# Regenerate Figure 4 from Hirshleifer et al. (2024)

plot_figure_4 <- function(df_returns, df_trends, topic, scale_attention = TRUE, output_file = NULL) {
  
  df <- df_returns %>% 
    inner_join(df_trends, by = "date") %>% 
    select(date, Rlead, !!sym(topic)) %>% 
    drop_na()
  
  if (scale_attention) {
    df <- df %>% 
      mutate(Attention = scale(.data[[topic]]))
  } else {
    df <- df %>% 
      mutate(Attention = .data[[topic]])
  }
    
  df <- df %>% 
    mutate(ExcessReturn = Rlead * 100)
  
  p1 <- ggplot(df, aes(x = date, y = Attention)) +
    geom_line(color = "steelblue", size = 0.8) +
    labs(y = "Standardized Attention", x = NULL,
         title = paste("Attention to", topic)) +
    theme_minimal(base_size = 12)
  
  p2 <- ggplot(df, aes(x = date, y = ExcessReturn)) +
    geom_col(fill = "darkred") +
    labs(y = expression(R[t+1] ~"(%)"),
         x = "Date", 
         title = "S&P 500 Excess Return") + 
    theme_minimal(base_size = 12)
  
  final_plot <- p1 / p2 + plot_layout(heights = c(1, 1))
  
  if (!is.null(output_file)) {
    ggsave(output_file, plot = final_plot, width = 10, height = 6)
  }
  
  return(final_plot)

  
}

#helper function for Table 3 from Hirshleifer et al. (2024)

fit_topic_subperiod <- function(df_returns, df_trends, topic, start_date, end_date, lag_nw = 3) {
  df <- df_returns %>% 
    inner_join(df_trends, by = "date") %>% 
    filter(date >= as.Date(start_date) & date <= as.Date(end_date)) %>% 
    filter(!is.na(.data[[topic]])) %>% 
    mutate(
      X = scale(.data[[topic]]),
      Y = Rlead * 100
    )
  
  if(nrow(df) < 12) return(NULL)
  
  fit <- lm(Y ~ X, data = df)
  vc <- sandwich::NeweyWest(fit, lag = lag_nw, prewhite = FALSE)
  tst <- lmtest::coeftest(fit, vcov. = vc)
  
  tibble(
    Topic = topic,
    Beta  = round(coef(fit)["X"], 4),
    t_stat = round(tst["X", "t value"], 2),
    R2 = round(summary(fit)$r.squared * 100, 2)
  )
}

#Table 3

replicate_table3 <- function(df_returns, df_trends, topics, start, end) {
  purrr::map_dfr(topics,
                 ~fit_topic_subperiod(df_returns, df_trends, .x, start, end)) %>%
    arrange(desc(R2))
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

run_oos_forecast <- function(df, pred, start_year = 2016) {
  # Ensure the predictor name is a string
  stopifnot(is.character(pred), length(pred) == 1)
  
  # Turn the predictor name into a symbol (for tidy eval)
  pred_sym <- rlang::sym(pred)
  
  df <- df %>%
    filter(!is.na(!!pred_sym)) %>%
    filter(!is.na(Rlead)) %>%
    mutate(
      X = scale(!!pred_sym),
      Y = Rlead * 100
    )
  
  if (nrow(df) < 24) {
    return(tibble(Topic = pred, R2_OS = NA, n = nrow(df)))
  }
  
  oos_start <- as.Date(paste0(start_year, "-01-01"))
  df <- df %>% arrange(date)
  
  actuals <- preds <- numeric()
  dates_oos <- df$date[df$date >= oos_start]
  
  for (i in seq_along(dates_oos)) {
    date_i <- dates_oos[i]
    df_train <- df %>% filter(date < date_i)
    df_test  <- df %>% filter(date == date_i)
    if (nrow(df_train) < 12 || nrow(df_test) == 0) next
    
    model <- lm(Y ~ X, data = df_train)
    pred_i <- predict(model, newdata = df_test)
    
    preds <- c(preds, pred_i)
    actuals <- c(actuals, df_test$Y)
  }
  
  r2_os <- 1 - sum((actuals - preds)^2) / sum((actuals - mean(actuals))^2)
  
  tibble(
    Topic = pred,
    R2_OS = round(r2_os, 3),
    n = length(actuals)
  )
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

#CSPE plot function for out-of-sample analysis

plot_cspe_oos <- function(df_returns, df_trends, topic, start_year = 2016, benchmark = "mean", output_file = NULL) {
  library(dplyr)
  library(ggplot2)
  library(rlang)
  
  pred_sym <- sym(topic)
  
  df <- df_returns %>%
    inner_join(df_trends, by = "date") %>%
    filter(!is.na(Rlead), !is.na(!!pred_sym)) %>%
    mutate(
      X = scale(!!pred_sym),
      Y = Rlead * 100
    ) %>%
    arrange(date)
  
  oos_start <- as.Date(paste0(start_year, "-01-01"))
  oos_dates <- df$date[df$date >= oos_start]
  
  model_spe <- benchmark_spe <- dates_used <- numeric()
  
  for (i in seq_along(oos_dates)) {
    date_i <- oos_dates[i]
    df_train <- df %>% filter(date < date_i)
    df_test  <- df %>% filter(date == date_i)
    if (nrow(df_train) < 12 || nrow(df_test) == 0) next
    
    model <- lm(Y ~ X, data = df_train)
    pred_model <- predict(model, newdata = df_test)
    spe_model <- (df_test$Y - pred_model)^2
    
    if (benchmark == "mean") {
      pred_bench <- mean(df_train$Y)
    } else {
      stop("Unsupported benchmark")
    }
    spe_bench <- (df_test$Y - pred_bench)^2
    
    model_spe <- c(model_spe, spe_model)
    benchmark_spe <- c(benchmark_spe, spe_bench)
    dates_used <- c(dates_used, date_i)
  }
  
  df_plot <- tibble(
    date = as.Date(dates_used, origin = "1970-01-01"),
    Model_CSPE = cumsum(model_spe),
    Benchmark_CSPE = cumsum(benchmark_spe)
  )
  
  p <- ggplot(df_plot, aes(x = date)) +
    geom_line(aes(y = Benchmark_CSPE), color = "gray40", linetype = "dashed") +
    geom_line(aes(y = Model_CSPE), color = "steelblue", size = 1) +
    labs(title = paste("Out-of-Sample CSPE –", topic),
         x = "Date", y = "Cumulative Squared Prediction Error") +
    theme_minimal(base_size = 12)
  
  if (!is.null(output_file)) {
    ggsave(output_file, p, width = 10, height = 5)
  }
  
  return(p)
}

