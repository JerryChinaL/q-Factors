library(dplyr)
library(ggplot2)
library(tidyr)
library(gridExtra)

# Read the CSV files
q_original <- read.csv("data/q5_factors_monthly_2023.csv") %>%
  mutate(sort_date = as.Date(sprintf("%d-%02d-01", year, month), "%Y-%m-%d"))

q3 <- read.csv("data/3-factors.csv") %>%
  mutate(sort_date = as.Date(sort_date)) %>%
  select(sort_date, R_ME = SMB, R_ROE = HML, R_IA = CMA)

# Join the datasets on the common YYYYMM values
combined_data <- inner_join(q_original, q3, by = "sort_date", suffix = c("_original", "_replicated"))

combined_data <- combined_data %>%
  filter(sort_date >= as.Date("1971-11-01"))

# Convert percentage values to decimals
combined_data <- combined_data %>%
  mutate(
    R_IA_original = R_IA_original / 100,
    R_IA_replicated = R_IA_replicated / 100,
    R_ME_original = R_ME_original / 100,
    R_ME_replicated = R_ME_replicated / 100,
    R_ROE_original = R_ROE_original / 100,
    R_ROE_replicated = R_ROE_replicated / 100
  )

# Calculate the compounded return for each factor
combined_data <- combined_data %>%
  arrange(sort_date) %>%
  mutate(
    compound_IA_original = cumprod(1 + R_IA_original) - 1,
    compound_ME_original = cumprod(1 + R_ME_original) - 1,
    compound_ROE_original = cumprod(1 + R_ROE_original) - 1,
    compound_IA_replicated = cumprod(1 + R_IA_replicated) - 1,
    compound_ME_replicated = cumprod(1 + R_ME_replicated) - 1,
    compound_ROE_replicated = cumprod(1 + R_ROE_replicated) - 1
  )

# Define a function to plot and show correlation
plot_compound <- function(data, factor) {
  original_col <- paste0("R_", factor, "_original")
  replicated_col <- paste0("R_", factor, "_replicated")
  compound_original_col <- paste0("compound_", factor, "_original")
  compound_replicated_col <- paste0("compound_", factor, "_replicated")
  
  original_correlation <- cor(data[[original_col]], data[[replicated_col]], use = "complete.obs")
  compound_correlation <- cor(data[[compound_original_col]], data[[compound_replicated_col]], use = "complete.obs")
  
  ggplot(data) +
    geom_line(aes(x = sort_date, y = !!sym(compound_original_col), color = "Original")) +
    geom_line(aes(x = sort_date, y = !!sym(compound_replicated_col), color = "Replicated")) +
    labs(title = paste("Compounded Returns of", factor),
         subtitle = paste("Factor Correlation:", round(original_correlation, 4), ", Compound Correlation:", round(compound_correlation, 4)),
         x = "Date", y = "Compounded Return") +
    scale_color_manual(name = "Series", values = c("Original" = "blue", "Replicated" = "red")) +
    theme_minimal()
}

# Plot compounded returns and show correlation for each factor
plot_list <- lapply(c("IA", "ME", "ROE"), function(factor) {
  plot_compound(combined_data, factor)
})

# Display the plots
do.call(grid.arrange, c(plot_list, ncol = 2))
