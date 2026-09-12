# ==============================================================================
# Cryptocurrency Time Series Analysis using CCF and DTW
# ==============================================================================

options(warn=-1)
Sys.setenv(LANGUAGE="en")

# Load required libraries
library(jsonlite)
library(tidyverse)
library(dplyr)
library(plotly)
library(patchwork)
library(hrbrthemes)
library(IncDTW)
library(xtable)
options(warn=0)

# ==============================================================================
# 1. HELPER FUNCTIONS
# ==============================================================================

# Function to find k at max CCF
Find_Max_CCF <- function(a, b, max_k, na_action) {
  d <- ccf(a, b, plot = FALSE, lag.max = max_k, type ="correlation", na.action = na_action)
  cor = d$acf[,,1]
  lag = d$lag[,,1]
  
  res = data.frame(cor, lag)
  res_max = res[which.max(res$cor),]
  rownames(res_max) <- NULL
  
  return(res_max)
} 

# Function to print first N rows in LaTeX format for the paper
first_n_latex <- function(n, df) {
  odd_rows <- seq(1, n*2, 2)
  latex_table <- df[odd_rows, ]
  rownames(latex_table) <- NULL
  print(xtable(latex_table))
}

# ==============================================================================
# 2. DATA IMPORT & CLEANING
# ==============================================================================

# Read JSON files
data_directory <- paste0(getwd(), "/binance")
file_list <- list.files(data_directory, pattern = ".json", full.names = TRUE)

df <- data.frame()
for (file in file_list) {
  crypto <- gsub("_USDT-1d.json", "", basename(file))
  data <- fromJSON(file)
  temp_df <- as.data.frame(data) 
  colnames(temp_df) <- c('time','open' ,'high','low','close','volume')
  temp_df$coin <- crypto
  df <- rbind(df, temp_df)
}

# Format dates
df$time <- as.POSIXct(df$time/1000, origin="1970-01-01")
df$time <- format(df$time, "%d-%m-%Y")
df$date <- as.Date(df$time, format="%d-%m-%Y")

# Update renamed coins
name_change_coins <- list(
  c('NPXS','PUNDIX'),
  c('STRAT','STRAX'),
  c('XZC','FIRO'),
  c('TVK','VANRY')
)

for (coin_pair in name_change_coins) {
  df <- df %>% 
    mutate(coin = ifelse(coin == coin_pair[1], coin_pair[2], coin))
}

# Remove delisted coins (coins where the max date doesn't match the global max date)
summarized_dates <- df %>% 
  group_by(coin) %>% 
  summarize(min_date = min(date), max_date = max(date))

max_date <- max(summarized_dates$max_date)
deleted_coins <- summarized_dates %>% filter(max_date < !!max_date) %>% pull(coin)

df <- df %>% filter(!coin %in% deleted_coins)
summarized_dates <- summarized_dates %>% filter(!coin %in% deleted_coins)

# Fill missing dates with NA to maintain time series integrity
min_time <- min(df$date, na.rm = TRUE)
max_time <- max(df$date, na.rm = TRUE)

min_max_time_df <- data.frame(time = seq.Date(from = min_time, to = max_time, by = "day"))
min_max_time_df$time <- format(min_max_time_df$time, "%d-%m-%Y")

coin_list <- unique(df$coin)
df_with_na <- df

for (coin_name in coin_list) {
  tmp <- filter(df, coin == coin_name)
  notavailable_times <- filter(min_max_time_df, !time %in% tmp$time)
  
  if (nrow(notavailable_times) > 0) {
    columns <- c('open','high','low','close','volume')
    notavailable_times <- cbind(notavailable_times, setNames(lapply(columns, function(x) NA), columns))
    notavailable_times$coin <- coin_name
    notavailable_times$date <- as.Date(notavailable_times$time, format="%d-%m-%Y")
    df_with_na <- rbind(df_with_na, notavailable_times)
  }
}

# Sort by coin and date
df_with_na <- df_with_na %>% arrange(coin, date)

# Feature Engineering: Calculate custom max_change metric
df_with_na <- df_with_na %>%
  mutate(max_change = ifelse((high-low)/low > 2, 1, (high-low)/low)) %>%
  mutate(max_change = ifelse(close-open > 0, max_change, -max_change))

# Save cleaned dataset
saveRDS(df_with_na, "df_with_na_cleaned.rds")

# ==============================================================================
# 3. ANALYSIS FUNCTIONS (CCF & DTW)
# ==============================================================================

calculate_ccf_dtw <- function(df_data, metric, st_date, end_date, sum_dates) {
  df_filtered <- df_data %>% filter(date >= st_date & date <= end_date)
  frame_length <- as.integer(as.Date(end_date) - as.Date(st_date)) + 1
  
  # Filter out coins with > 80% missing data in this timeframe
  na_summary <- df_filtered %>% 
    group_by(coin) %>% 
    summarise(na_sum = sum(is.na(close)) / frame_length)
  
  keep_coins <- na_summary %>% filter(na_sum <= 0.8) %>% pull(coin)
  df_filtered <- df_filtered %>% filter(coin %in% keep_coins)
  
  df_coin_pivot <- df_filtered %>% pivot_wider(id_cols = date, names_from = coin, values_from = all_of(metric))
  index_date <- df_coin_pivot$date
  active_coin_list <- colnames(df_coin_pivot)[-1]
  
  result_df <- data.frame()
  start_time <- Sys.time()
  
  for (i in 1:length(active_coin_list)) {
    coin1st <- active_coin_list[i]
    first_min_date <- sum_dates %>% filter(coin == coin1st) %>% pull(min_date)
    
    for (j in 1:length(active_coin_list)) {
      coin2nd <- active_coin_list[j]
      
      if (coin1st != coin2nd) {
        second_min_date <- sum_dates %>% filter(coin == coin2nd) %>% pull(min_date)
        filter_date <- max(first_min_date, second_min_date)
        
        first_coin_metric <- df_coin_pivot[index_date >= filter_date, ][[coin1st]]
        second_coin_metric <- df_coin_pivot[index_date >= filter_date, ][[coin2nd]]
        
        # Intersection length must be at least 20% of the timeframe
        if (length(first_coin_metric) < frame_length / 5) next
        
        # Check if symmetric pair already calculated
        tmp <- result_df[result_df$first_coin == coin2nd & result_df$second_coin == coin1st, ]
        
        if (nrow(tmp) > 0) {
          result_df <- bind_rows(result_df, list(
            first_coin = coin1st, second_coin = coin2nd,
            ccf.cor = tmp$ccf.cor, ccf.lag = -tmp$ccf.lag,
            dtw_dist = tmp$dtw_dist
          ))
        } else {
          tryCatch({
            # Lag window is set to 10 days
            ccf_result <- Find_Max_CCF(first_coin_metric, second_coin_metric, 10, na.omit)
            if (ccf_result$cor > 0.2) {
              dtw_dist <- IncDTW::dtw2vec(first_coin_metric, second_coin_metric)$normalized_distance
              result_df <- bind_rows(result_df, list(
                first_coin = coin1st, second_coin = coin2nd, 
                ccf.cor = ccf_result$cor, ccf.lag = ccf_result$lag,
                dtw_dist = dtw_dist
              ))
            }
          }, error = function(e) {})
        }
      }
    }
  }
  
  end_time <- Sys.time()
  print(paste("Execution time:", round(end_time - start_time, 2)))
  
  return(result_df %>% arrange(desc(ccf.cor), desc(dtw_dist)))
}

# ==============================================================================
# 4. EXECUTE ANALYSIS FOR DIFFERENT PERIODS
# ==============================================================================

# Period 1
result_per1st <- calculate_ccf_dtw(df_with_na, "max_change", "2020-03-12", "2021-11-10", summarized_dates)
saveRDS(result_per1st, "result_per1st.rds")
cat("\nTop Results - Period 1:\n")
first_n_latex(10, result_per1st)

# Period 2
result_per2nd <- calculate_ccf_dtw(df_with_na, "max_change", "2021-11-11", "2023-10-12", summarized_dates)
saveRDS(result_per2nd, "result_per2nd.rds")
cat("\nTop Results - Period 2:\n")
first_n_latex(10, result_per2nd)

# Period 3
result_per3rd <- calculate_ccf_dtw(df_with_na, "max_change", "2023-10-13", "2024-04-15", summarized_dates)
saveRDS(result_per3rd, "result_per3rd.rds")
cat("\nTop Results - Period 3:\n")
first_n_latex(10, result_per3rd)

# ==============================================================================
# 5. VISUALIZATION
# ==============================================================================

plot_coins <- function(first_coin, second_coin, st_date, end_date, data_source, metric = "close") {
  
  coin_data <- data_source %>% 
    filter(date >= st_date & date <= end_date) %>%
    filter(coin %in% c(first_coin, second_coin)) %>% 
    as_tibble() 
  
  summarised_data <- coin_data %>%
    filter(!is.na(!!sym(metric))) %>%
    group_by(coin) %>%
    summarize(
      min_date = min(date),
      max_date = max(date)
    )
  
  min_date1 <- summarised_data %>% filter(coin == first_coin) %>% pull(min_date)
  min_date2 <- summarised_data %>% filter(coin == second_coin) %>% pull(min_date)
  graph_start_date <- max(min_date1, min_date2)
  
  s1 <- coin_data %>% filter(date == graph_start_date & coin == first_coin) %>% pull(!!sym(metric))
  s2 <- coin_data %>% filter(date == graph_start_date & coin == second_coin) %>% pull(!!sym(metric))
  y_factor <- s1 / s2
  
  graph_data <- coin_data %>% 
    filter(date > graph_start_date) %>%
    mutate(!!metric := ifelse(coin == second_coin, !!sym(metric) * y_factor, !!sym(metric)))
  
  p <- ggplot(graph_data, aes(x = date, y = !!sym(metric), colour = coin)) +
    geom_path() +
    scale_y_continuous(name = first_coin, sec.axis = sec_axis(~./y_factor, name = second_coin), trans = "log") +
    scale_x_date(limits = c(max(summarised_data$min_date), max(summarised_data$max_date))) +
    labs(title = paste(first_coin, "/", second_coin, "Daily", tools::toTitleCase(metric))) +
    theme_modern_rc() +
    theme(axis.text.x = element_text(angle = 90))
  
  # Ensure directory exists before saving
  if(!dir.exists("pngs")) dir.create("pngs")
  
  file_name <- paste0("pngs/", paste(first_coin, second_coin, st_date, end_date, sep="_"), ".png")
  ggsave(file_name, plot = p, width = 8, height = 5)
  
  return(p)
}

# Example Plot Usage:
# p1 <- plot_coins("BTC", "ETH", "2021-11-11", "2023-10-12", df_with_na, metric="close")
# print(p1)
