library(dplyr)
library(xtable)

# Load the data
# Load necessary libraries and data
MKT_data <- read.csv("data/monthly_rf.csv") %>%
  mutate(monthly_date = as.Date(as.character(X * 100 + 1), format = "%Y%m%d")) %>%
  select(monthly_date, r_mkt = Mkt.RF)

r_factors <- read.csv("data/3-factors-fixed_vol.csv") %>%
  mutate(monthly_date = as.Date(monthly_date)) %>%
  select(monthly_date, r_vol = SMB, r_roe = HML, r_ia = CMA) %>%
  left_join(MKT_data, by = "monthly_date") 
# %>%
#   filter(monthly_date >= as.Date("1968-01-01") & monthly_date <= as.Date("2018-12-31"))

# Initialize matrices for coefficients and t-statistics
variables <- c("r_mkt", "r_vol", "r_ia", "r_roe")
n_vars <- length(variables)
coefficients_matrix <- matrix(NA, nrow = n_vars, ncol = n_vars + 2, dimnames = list(variables, c(variables, "(Intercept)", "R^2")))
t_stats_matrix <- matrix(NA, nrow = n_vars, ncol = n_vars + 2, dimnames = list(variables, c(variables, "(Intercept)", "R^2")))

# Perform the regression
for (i in 1:n_vars) {
  for (j in 1:n_vars) {
    if (i != j) {
      formula <- as.formula(paste(variables[i], "~", paste(variables[-i], collapse = " + ")))
      model <- lm(formula, data = r_factors)
      summary_model <- summary(model)
      coefficients_matrix[i, variables[j]] <- summary_model$coefficients[variables[j], "Estimate"]
      t_stats_matrix[i, variables[j]] <- summary_model$coefficients[variables[j], "t value"]
    }
  }
  coefficients_matrix[i, "(Intercept)"] <- summary_model$coefficients["(Intercept)", "Estimate"]
  t_stats_matrix[i, "(Intercept)"] <- summary_model$coefficients["(Intercept)", "t value"]
  coefficients_matrix[i, "R^2"] <- summary_model$r.squared
  t_stats_matrix[i, "R^2"] <- summary_model$r.squared
}

# Round to 2 decimal places
coefficients_matrix <- round(coefficients_matrix, 2)
t_stats_matrix <- round(t_stats_matrix, 2)

# Print the matrices
print(coefficients_matrix)
print(t_stats_matrix)

# Save results to file
output_file <- "tables/table5_regression_results.txt"
file_conn <- file(output_file, open = "wt")

# Function to write matrices to file
write_matrix_to_file <- function(mat, title) {
  cat(title, ":\n", file = file_conn, append = TRUE)
  print(mat, file = file_conn, append = TRUE, quote = FALSE, col.names = NA)
  cat("\n", file = file_conn, append = TRUE)
}

write_matrix_to_file(xtable(coefficients_matrix), "Coefficients-latex")
write_matrix_to_file(xtable(t_stats_matrix), "T-Statistics-latex")

write_matrix_to_file(coefficients_matrix, "Coefficients")
write_matrix_to_file(t_stats_matrix, "T-Statistics")

# Close the file connection
close(file_conn)


cat("Regression results and t-test results saved to", output_file)
