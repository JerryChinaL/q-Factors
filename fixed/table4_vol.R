rm(list = ls())
library(dplyr)
library(tidyr)
library(broom)
library(xtable)

date_min <- as.Date("1968-01-01")
date_max <- as.Date("2018-12-31")

# Load necessary libraries and data
MKT_data <- read.csv("data/monthly_rf.csv") %>%
  mutate(monthly_date = as.Date(as.character(X * 100 + 1), format = "%Y%m%d")) %>%
  select(monthly_date, r_mkt = Mkt.RF)

factors <- readRDS("data/final_portfolios_fixed.rds") 

r_factors <- read.csv("data/3-factors-fixed_vol.csv") %>%
  mutate(monthly_date = as.Date(monthly_date)) %>%
  select(monthly_date, r_vol = SMB, r_roe = HML, r_ia = CMA) %>%
  left_join(MKT_data, by = "monthly_date") %>%
  filter(monthly_date >= date_min & monthly_date <= date_max)

rf_data <- read.csv("data/monthly_rf.csv")

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
  group_by(monthly_date) %>%
  mutate(
    portfolio_vol = assign_portfolio(
      data = pick(everything()),
      sorting_variable = VOL,
      percentiles = c(0, 0.2, 0.4, 0.6, 0.8, 1)
    ),
    portfolio_ia = assign_portfolio(
      data = pick(everything()),
      sorting_variable = IA,
      percentiles = c(0, 0.2, 0.4, 0.6, 0.8, 1)
    ),
    portfolio_roe = assign_portfolio(
      data = pick(everything()),
      sorting_variable = ROE,
      percentiles = c(0, 0.2, 0.4, 0.6, 0.8, 1)
    )
  ) %>%
  ungroup() %>%
  select(KYPERMNO, YYYYMM = monthly_date, MTHRET, VOL, portfolio_vol, portfolio_ia, portfolio_roe)

# Add the excess return column by appending rf rate then subtracting.
portfolios_5x5 <- portfolios_5x5 %>% 
  mutate(rf_date = format(as.Date(YYYYMM), "%Y%m")) %>%
  left_join(rf_data %>% mutate(rf_date = as.character(X)) %>% select(rf_date, RF), by = c("rf_date")) %>%
  select(-c(rf_date)) %>%
  mutate(MTHRET = MTHRET * 100 - RF)

# List of variables to loop through
variables <- c("ia", "roe")

# Initialize a list to store regression results
regression_results <- list()
t_test_results <- list()
r_squared_results <- list()

for (var in variables) {
  # Filter for non-NA values at the beginning of the loop
  portfolios_5x5_filtered <- portfolios_5x5 %>%
    filter(!is.na(!!sym(paste0("portfolio_", var))) & !is.na(VOL) & !is.na(MTHRET),
           YYYYMM >= date_min & YYYYMM <= date_max)
  
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
  
  # Merge with r_factors
  joined_data <- monthly_grid_wide %>% left_join(r_factors, by = c("YYYYMM" = "monthly_date"))
  
  saveRDS(joined_data, paste0("fixed/data/table6_vol_", var, "_ret.rds"))
  
  # Perform regressions for each quantile pair
  reg_results <- lapply(names(joined_data)[2:26], function(quantile_col) {
    formula <- as.formula(paste(quantile_col, "~ r_mkt + r_ia + r_roe + r_vol"))
    lm(formula, data = joined_data)
  })
  
  # Extract coefficients and organize into matrices
  coef_names <- c("(Intercept)", "r_mkt", "r_ia", "r_roe", "r_vol")
  for (coef_name in coef_names) {
    coef_matrix <- matrix(nrow = 5, ncol = 5)
    t_test_matrix <- matrix(nrow = 5, ncol = 5)
    dimnames(coef_matrix) <- list(paste0("VOL_", 1:5), paste0(var, "_", 1:5))
    dimnames(t_test_matrix) <- list(paste0("VOL_", 1:5), paste0(var, "_", 1:5))
    for (i in 1:5) {
      for (j in 1:5) {
        idx <- (i - 1) * 5 + j
        coef_matrix[i, j] <- round(coef(summary(reg_results[[idx]]))[coef_name, "Estimate"], 2)
        t_test_matrix[i, j] <- round(coef(summary(reg_results[[idx]]))[coef_name, "t value"], 2)
      }
    }
    regression_results[[paste0(var, "_", coef_name)]] <- coef_matrix
    t_test_results[[paste0(var, "_", coef_name, "_t_value")]] <- t_test_matrix
  }
  
  # Extract R-squared values and organize into a matrix
  r_squared_matrix <- matrix(nrow = 5, ncol = 5)
  dimnames(r_squared_matrix) <- list(paste0("VOL_", 1:5), paste0(var, "_", 1:5))
  for (i in 1:5) {
    for (j in 1:5) {
      idx <- (i - 1) * 5 + j
      r_squared_matrix[i, j] <- round(summary(reg_results[[idx]])$r.squared, 2)
    }
  }
  r_squared_results[[paste0(var, "_R_squared")]] <- r_squared_matrix
}

# Function to format column names for LaTeX
format_colnames <- function(names) {
  return(gsub("_", "\\\\_", names))
}

# Combine results into 5x15 matrices
combine_results <- function(coef_name) {
  combined_matrix <- matrix(NA, nrow = 5, ncol = 10)
  colnames(combined_matrix) <- c(paste0("ia_", 1:5), paste0("roe_", 1:5))
  rownames(combined_matrix) <- paste0("VOL_", 1:5)
  
  for (var in variables) {
    start_col <- match(paste0(var, "_1"), colnames(combined_matrix))
    end_col <- start_col + 4
    if (coef_name == "R_squared") {
      combined_matrix[, start_col:end_col] <- r_squared_results[[paste0(var, "_", coef_name)]]
    } else {
      combined_matrix[, start_col:end_col] <- regression_results[[paste0(var, "_", coef_name)]]
    }
  }
  print(combined_matrix)
  return(combined_matrix)
}


# Combine t-stat results into 5x15 matrices
combine_t_stats <- function(coef_name) {
  combined_matrix <- matrix(NA, nrow = 5, ncol = 10)
  colnames(combined_matrix) <- c(paste0("ia_", 1:5), paste0("roe_", 1:5))
  rownames(combined_matrix) <- paste0("VOL_", 1:5)
  
  for (var in variables) {
    start_col <- match(paste0(var, "_1"), colnames(combined_matrix))
    end_col <- start_col + 4
    combined_matrix[, start_col:end_col] <- t_test_results[[paste0(var, "_", coef_name, "_t_value")]]
  }
  return(combined_matrix)
}

# Function to format a string by wrapping the part after the underscore in curly braces if it contains an underscore
format_string_if_contains_underscore <- function(string) {
  if (grepl("_", string)) {
    parts <- strsplit(string, "_")[[1]]
    return(paste0(parts[1], "_{", parts[2], "}"))
  } else {
    return(string)  # Return the original string if it does not contain an underscore
  }
}


# Generate the LaTeX code with bold formatting for significant coefficients
generate_latex_table <- function(coef_name, combined_matrix, t_test_matrix) {
  name <- format_string_if_contains_underscore(coef_name)
  body <- paste0(" & \\multicolumn{5}{c|}{$", name, "$} & \\multicolumn{5}{c}{$", name, "$}  \\\\\n")
  rownames <- c("Illiquid", "2", "3", "4", "Liquid")
  for (i in 1:5) {
    body <- paste0(body, rownames[i], " & ")
    for (j in 1:10) {
      value <- combined_matrix[i, j]
      if (!is.na(t_test_matrix[i, j]) && abs(t_test_matrix[i, j]) > 1.96) {
        value <- paste0("\\textbf{", value, "}")
      }
      body <- paste0(body, value)
      if (j %% 5 == 0 && j != 10) {
        body <- paste0(body, " & ")
      } else if (j != 10) {
        body <- paste0(body, " & ")
      }
    }
    body <- paste0(body, " \\\\\n")
  }
  return(body)
}


# Write the LaTeX code to a file
output_file <- "fixed/data/table4_results_vol.tex"
file_conn <- file(output_file, open = "wt")

header <- "\\begin{table}[H]\n\\tiny\n\\centering\n\\begin{tabular}{lccccc|ccccc}\n\\hline\n& \\multicolumn{10}{c}{Five Factors} \\\\ \\hline\n& \\multicolumn{10}{c}{\\tiny $R_{i,t} - R_{F,t} = \\alpha_i+\\beta_i*MKT_{R,t} + \\phi_iVOL_{R,t}+\\pi_iBM_{R,t} + \\delta_iOP_{R,t}+\\gamma_iINV_{R,t} + \\epsilon_{i,t}$} \\\\ \\hline\nLiquidity & \\multicolumn{5}{c|}{\\textbf{Panel A: IA}} & \\multicolumn{5}{c}{\\textbf{Panel B: ROE}} \\\\\nQuintiles & Low & 2 & 3 & 4 & High & Low & 2 & 3 & 4 & High \\\\  \\hline\n"
footer <- "\\end{tabular}\n\\end{table}\n"

cat(header, file = file_conn)

for (coef_name in c("(Intercept)", "r_mkt", "r_ia", "r_roe", "r_vol")) {
  combined_matrix <- combine_results(coef_name)
  t_test_matrix <- combine_t_stats(coef_name)
  latex_table <- generate_latex_table(coef_name, combined_matrix, t_test_matrix)
  cat(latex_table, file = file_conn)
}

# Add R_squared separately since it doesn't have t-values
combined_matrix <- combine_results("R_squared")
latex_table <- generate_latex_table("R_squared", combined_matrix, matrix(NA, nrow = 5, ncol = 15))
cat(latex_table, file = file_conn)

cat(footer, file = file_conn, append = TRUE)

close(file_conn)
cat("Regression results saved to", output_file, "\n")