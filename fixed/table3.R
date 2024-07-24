library(dplyr)

factors_replicated <- read.csv("data/3-factors-fixed.csv") %>%
  mutate(monthly_date = as.Date(monthly_date)) %>%
  select(monthly_date, r_vol = SMB, r_roe = HML, r_ia = CMA)

rf_data <- read.csv("data/monthly_rf.csv") %>%
  mutate(monthly_date = as.Date(as.character(X*100+1), format = "%Y%m%d")) %>%
  select(monthly_date, r_mkt = Mkt.RF)

factors_filtered <- factors_replicated %>%
  left_join(rf_data, by = "monthly_date") %>%
  filter(monthly_date >= as.Date("1968-01-01") & monthly_date <= as.Date("2018-12-31"))

# Calculate summary statistics
mean_r_ia <- mean(factors_filtered$r_ia, na.rm = TRUE)
mean_r_roe <- mean(factors_filtered$r_roe, na.rm = TRUE)
mean_r_vol <- mean(factors_filtered$r_vol, na.rm = TRUE)
mean_r_mkt <- mean(factors_filtered$r_mkt, na.rm = TRUE)

sd_r_ia <- sd(factors_filtered$r_ia, na.rm = TRUE)
sd_r_roe <- sd(factors_filtered$r_roe, na.rm = TRUE)
sd_r_vol <- sd(factors_filtered$r_vol, na.rm = TRUE)
sd_r_mkt <- sd(factors_filtered$r_mkt, na.rm = TRUE)

t_r_ia <- as.numeric(t.test(factors_filtered$r_ia)$statistic)
t_r_roe <- as.numeric(t.test(factors_filtered$r_roe)$statistic)
t_r_vol <- as.numeric(t.test(factors_filtered$r_vol)$statistic)
t_r_mkt <- as.numeric(t.test(factors_filtered$r_mkt)$statistic)

# Create Panel A
summary_stats_table <- data.frame(
  Statistic = c("Arithmetic Mean (%)", "Standard Deviation (%)", "t-statistic"),
  r_mkt = c(mean_r_mkt, sd_r_mkt, t_r_mkt),
  r_vol = c(mean_r_vol, sd_r_vol, t_r_vol),
  r_ia = c(mean_r_ia, sd_r_ia, t_r_ia),
  r_roe = c(mean_r_roe, sd_r_roe, t_r_roe)
)

# Panel B: Correlation Matrix for the Five Factors
cor_matrix_five_factors <- factors_filtered %>%
  select(r_mkt, r_vol, r_ia, r_roe) %>%
  cor(use = "pairwise.complete.obs")

# Print Panel A
cat("Panel A: Summary Statistics of the Five Factors\n")
print(summary_stats_table)

# Print Panel B
cat("\nPanel B: Correlation Matrix for the Five Factors\n")
print(cor_matrix_five_factors)

# Save the tables to CSV files
write.csv(summary_stats_table, "fixed/data/table3_Panel_A_Summary_Statistics.csv", row.names = FALSE)
write.csv(cor_matrix_five_factors, "fixed/data/table3_Panel_B_Correlation_Matrix_Five_Factors.csv")
