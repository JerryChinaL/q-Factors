library(dplyr)
library(tidyr)

# Load the data
factors <- readRDS("data/final_portfolios_fixed.rds")

# Define the function to assign portfolios
assign_portfolio <- function(data, sorting_variable, percentiles) {
  if (all(is.na(data[[deparse(substitute(sorting_variable))]]))) {
    return(rep(NA, nrow(data)))
  }
  breakpoints <- data %>%
    filter(PRIMEXCH == "N") %>%
    filter(!is.na({{ sorting_variable }})) %>%
    pull({{ sorting_variable }}) %>%
    quantile(probs = percentiles, na.rm = TRUE, names = FALSE)
  
  assigned_portfolios <- findInterval(data[[deparse(substitute(sorting_variable))]], breakpoints, all.inside = TRUE)
  
  return(assigned_portfolios)
}

# Apply the portfolio assignment
portfolios_5x5 <- factors %>%
  # filter(!is.na(VOL), !is.na(VOLM), !is.na(EP), !is.na(EMP)) %>%
  group_by(monthly_date) %>%
  mutate(
    portfolio_vol = assign_portfolio(
      data = pick(everything()),
      sorting_variable = VOL,
      percentiles = c(0, 0.2, 0.4, 0.6, 0.8, 1)
    ),
    portfolio_roe = assign_portfolio(
      data = pick(everything()),
      sorting_variable = ROE,
      percentiles = c(0, 0.2, 0.4, 0.6, 0.8, 1)
    ),
    portfolio_ia = assign_portfolio(
      data = pick(everything()),
      sorting_variable = IA,
      percentiles = c(0, 0.2, 0.4, 0.6, 0.8, 1)
    )
  ) %>%
  ungroup() %>%
  select(KYPERMNO, YYYYMM = monthly_date, MTHRET, VOL, portfolio_vol, portfolio_roe, portfolio_ia)

# Add the excess return column by appending rf rate then subtracting.
rf_data <- read.csv("data/monthly_rf.csv")
portfolios_5x5 <- portfolios_5x5 %>% 
  mutate(rf_date = format(as.Date(YYYYMM), "%Y%m")) %>%
  left_join(rf_data %>% mutate(rf_date = as.character(X)) %>% select(rf_date, RF), by = c("rf_date")) %>%
  select(-c(rf_date)) %>%
  mutate(MTHRET = MTHRET - (RF / 100))

# List of variables to loop through
variables <- c("ia", "roe")

for (var in variables) {
  # Filter for non-NA values at the beginning of the loop
  portfolios_5x5_filtered <- portfolios_5x5 %>%
    filter(!is.na(!!sym(paste0("portfolio_", var))) & !is.na(VOL) & !is.na(MTHRET),
           YYYYMM >= as.Date("1968-01-01") & YYYYMM <= as.Date("2018-12-31"))
  
  # Calculate the average monthly return for each quantile pair for each month
  monthly_grid <- portfolios_5x5_filtered %>%
    group_by(YYYYMM, portfolio_vol, !!sym(paste0("portfolio_", var))) %>%
    mutate(VOL_weight = ifelse(is.na(VOL), 0, VOL)) %>%
    summarize(avg_monthly_return = weighted.mean(MTHRET, VOL_weight, na.rm = TRUE), .groups = "drop")
  
  # Pivot the data to a wider format
  monthly_grid_wide <- monthly_grid %>%
    unite("quantile", portfolio_vol, !!sym(paste0("portfolio_", var)), sep = "") %>%
    pivot_wider(names_from = quantile, values_from = avg_monthly_return) %>%
    rename_with(~ paste0("f", .), -YYYYMM)
  
  # Sort the columns by name
  sorted_columns <- c("YYYYMM", sort(names(monthly_grid_wide)[-1]))
  monthly_grid_wide <- monthly_grid_wide %>% 
    select(all_of(sorted_columns))
  
  # Calculate the average of each column to get the final grid
  final_grid <- monthly_grid_wide %>%
    summarize(across(-YYYYMM, \(x) mean(x, na.rm = TRUE)))
  
  # Calculate the t-statistics for each column
  t_stats <- monthly_grid_wide %>%
    summarize(across(-YYYYMM, ~ t.test(.x)$statistic))
  
  # Convert the final grid to a 5x5 matrix
  final_matrix <- matrix(
    as.numeric(final_grid[1, ]),
    nrow = 5, ncol = 5, byrow = TRUE,
    dimnames = list(paste0("VOL_", 1:5), paste0(var, "_", 1:5))
  )
  
  t_stat_matrix <- matrix(
    as.numeric(t_stats[1, ]),
    nrow = 5, ncol = 5, byrow = TRUE,
    dimnames = list(paste0("VOL_", 1:5), paste0(var, "_", 1:5))
  )
  
  print("5x5 grid:")
  print(100 * final_matrix)
  
  # Print t-statistics
  print("tstat:")
  print(t_stat_matrix)
  
  # Save the files
  write.csv(format(as.data.frame(monthly_grid_wide), scientific = FALSE), paste0("fixed/data/5x5_monthly_vol_", var, ".csv"), row.names = FALSE)
  write.csv(format(as.data.frame(100 * final_matrix), scientific = FALSE), paste0("fixed/data/5x5_vol_", var, ".csv"), row.names = TRUE)
  write.csv(format(as.data.frame(t_stat_matrix), scientific = FALSE), paste0("fixed/data/5x5_tstat_vol_", var, ".csv"), row.names = TRUE)
}

     