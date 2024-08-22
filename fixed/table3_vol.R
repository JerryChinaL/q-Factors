rm(list = ls())
library(dplyr)

min_date <- as.Date("1968-01-01")
max_date <- as.Date("2018-12-31")

# Load the factors data
factors_replicated <- read.csv("data/3-factors-fixed.csv") %>%
  mutate(monthly_date = as.Date(monthly_date)) %>%
  select(monthly_date, r_vol = SMB, r_roe = HML, r_ia = CMA)

rf_data <- read.csv("data/monthly_rf.csv") %>%
  mutate(monthly_date = as.Date(as.character(X*100+1), format = "%Y%m%d")) %>%
  select(monthly_date, r_mkt = Mkt.RF)

factors_filtered <- factors_replicated %>%
  left_join(rf_data, by = "monthly_date") %>%
  filter(monthly_date >= min_date & monthly_date <= max_date)

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

# Panel B: Correlation Matrix for the Factors
cor_matrix_five_factors <- factors_filtered %>%
  select(r_mkt, r_vol, r_ia, r_roe) %>%
  cor(use = "pairwise.complete.obs")

# Format numbers to two decimal places
summary_stats_table_formatted <- summary_stats_table %>%
  mutate(across(-Statistic, ~ sprintf("%.2f", .)))

cor_matrix_five_factors_formatted <- apply(cor_matrix_five_factors, 2, function(x) sprintf("%.2f", x))

# LaTeX Table Generation
latex_output <- paste0("
\\documentclass{article}
\\usepackage{geometry}
\\geometry{letterpaper, margin=1in}
\\usepackage{array}
\\usepackage{booktabs}
\\usepackage{float}

\\begin{document}

Factor return summary 1968-2018

\\begin{table}[H]
\\small
\\begin{tabular}{p{4.5cm} p{2cm} p{2cm} p{2cm} p{2cm}}
\\hline
\\multicolumn{5}{l}{Panel A: Summary statistics of the factors} \\\\
 & $MKT_R$ & $VOL_R$ & $IA_R$ & $ROE_R$ \\\\
 \\hline
Arithmetic mean & ", paste(summary_stats_table_formatted$r_mkt[1], summary_stats_table_formatted$r_vol[1], summary_stats_table_formatted$r_ia[1], summary_stats_table_formatted$r_roe[1], sep = " & "), " \\\\
Standard deviation & ", paste(summary_stats_table_formatted$r_mkt[2], summary_stats_table_formatted$r_vol[2], summary_stats_table_formatted$r_ia[2], summary_stats_table_formatted$r_roe[2], sep = " & "), " \\\\
T-statistic & ", paste(summary_stats_table_formatted$r_mkt[3], summary_stats_table_formatted$r_vol[3], summary_stats_table_formatted$r_ia[3], summary_stats_table_formatted$r_roe[3], sep = " & "), " \\\\
\\\\
\\multicolumn{5}{l}{Panel B: Correlation matrix for the factors} \\\\
 & $MKT_R$ & $VOL_R$ & $IA_R$ & $ROE_R$ \\\\
 \\hline
$MKT_R$ & ", paste(cor_matrix_five_factors_formatted[1, ], collapse = " & "), " \\\\
$VOL_R$ & ", paste(cor_matrix_five_factors_formatted[2, ], collapse = " & "), " \\\\
$IA_R$ & ", paste(cor_matrix_five_factors_formatted[3, ], collapse = " & "), " \\\\
$ROE_R$ & ", paste(cor_matrix_five_factors_formatted[4, ], collapse = " & "), " \\\\
\\hline
\\end{tabular}
\\end{table}

\\end{document}
")

# Print the LaTeX table
cat(latex_output)

# Print Panel A
cat("Panel A: Summary Statistics of the Factors\n")
print(summary_stats_table)

# Print Panel B
cat("\nPanel B: Correlation Matrix for the Factors\n")
print(cor_matrix_five_factors)

# Save the tables to CSV files
write.csv(summary_stats_table, "fixed/data/table3_Panel_A_Summary_Statistics.csv", row.names = FALSE)
write.csv(cor_matrix_five_factors, "fixed/data/table3_Panel_B_Correlation_Matrix_Five_Factors.csv")
