library(dplyr)

# Load your data
final_df <- read.csv("data/four_factors_excret.csv")

# Ensure the necessary columns are numeric and remove NA rows for SIZE, bm, op, and inv
final_df <- final_df %>%
  mutate(
    bm = as.numeric(bm),
    SIZE = as.numeric(SIZE),
    MTHRET = as.numeric(MTHRET),
    op = as.numeric(op),
    inv = as.numeric(inv)
  ) %>%
  filter(!is.na(SIZE) & !is.na(bm) & !is.na(op) & !is.na(inv))

# Calculate the number of stocks (n) for each company
final_df <- final_df %>%
  group_by(KYGVKEY, sort_date) %>%
  mutate(n_stocks = n()) %>%
  ungroup()

# Adjust SIZE by dividing by the number of stocks (n)
final_df <- final_df %>%
  mutate(adjusted_SIZE = SIZE / n_stocks)

# Define the function to assign portfolios
assign_portfolio <- function(data, sorting_variable, percentiles) {
  breakpoints <- data %>%
    filter(PRIMEXCH == "N") %>%
    filter(!is.na({{ sorting_variable }})) %>%
    pull({{ sorting_variable }}) %>%
    quantile(probs = percentiles, na.rm = TRUE, names = FALSE)
  
  assigned_portfolios <- findInterval(data[[deparse(substitute(sorting_variable))]], breakpoints, all.inside = TRUE)
  
  return(assigned_portfolios)
}

# Apply the portfolio assignment
portfolios <- final_df %>%
  group_by(sort_date) %>%
  mutate(
    portfolio_size = assign_portfolio(
      data = pick(everything()),
      sorting_variable = SIZE,
      percentiles = c(0, 0.5, 1)
    ),
    portfolio_bm = assign_portfolio(
      data = pick(everything()),
      sorting_variable = bm,
      percentiles = c(0, 0.3, 0.7, 1)
    ),
    portfolio_op = assign_portfolio(
      data = pick(everything()),
      sorting_variable = op,
      percentiles = c(0, 0.3, 0.7, 1)
    ),
    portfolio_inv = assign_portfolio(
      data = pick(everything()),
      sorting_variable = inv,
      percentiles = c(0, 0.3, 0.7, 1)
    )
  ) %>%
  ungroup() %>%
  select(permno = KYPERMNO, sort_date, portfolio_size, portfolio_bm, portfolio_op, portfolio_inv, excess_return, SIZE, return_date, adjusted_SIZE) %>%
  mutate(YYYYMM = format(as.Date(return_date), "%Y%m"))

# Calculate the factors according to the provided formula using weighted mean with SIZE
factors_replicated <- portfolios %>%
  group_by(YYYYMM) %>%
  summarize(
    SH = weighted.mean(excess_return[portfolio_size == 1 & portfolio_bm == 3], SIZE[portfolio_size == 1 & portfolio_bm == 3], na.rm = TRUE),  # Small size, High B/M
    SN_bm = weighted.mean(excess_return[portfolio_size == 1 & portfolio_bm == 2], SIZE[portfolio_size == 1 & portfolio_bm == 2], na.rm = TRUE),  # Small size, Neutral B/M
    SL = weighted.mean(excess_return[portfolio_size == 1 & portfolio_bm == 1], SIZE[portfolio_size == 1 & portfolio_bm == 1], na.rm = TRUE),  # Small size, Low B/M
    BH = weighted.mean(excess_return[portfolio_size == 2 & portfolio_bm == 3], SIZE[portfolio_size == 2 & portfolio_bm == 3], na.rm = TRUE),  # Big size, High B/M
    BN_bm = weighted.mean(excess_return[portfolio_size == 2 & portfolio_bm == 2], SIZE[portfolio_size == 2 & portfolio_bm == 2], na.rm = TRUE),  # Big size, Neutral B/M
    BL = weighted.mean(excess_return[portfolio_size == 2 & portfolio_bm == 1], SIZE[portfolio_size == 2 & portfolio_bm == 1], na.rm = TRUE),  # Big size, Low B/M
    
    SR = weighted.mean(excess_return[portfolio_size == 1 & portfolio_op == 3], SIZE[portfolio_size == 1 & portfolio_op == 3], na.rm = TRUE),  # Small size, High OP
    SN_op = weighted.mean(excess_return[portfolio_size == 1 & portfolio_op == 2], SIZE[portfolio_size == 1 & portfolio_op == 2], na.rm = TRUE),  # Small size, Neutral OP
    SW = weighted.mean(excess_return[portfolio_size == 1 & portfolio_op == 1], SIZE[portfolio_size == 1 & portfolio_op == 1], na.rm = TRUE),  # Small size, Low OP
    BR = weighted.mean(excess_return[portfolio_size == 2 & portfolio_op == 3], SIZE[portfolio_size == 2 & portfolio_op == 3], na.rm = TRUE),  # Big size, High OP
    BN_op = weighted.mean(excess_return[portfolio_size == 2 & portfolio_op == 2], SIZE[portfolio_size == 2 & portfolio_op == 2], na.rm = TRUE),  # Big size, Neutral OP
    BW = weighted.mean(excess_return[portfolio_size == 2 & portfolio_op == 1], SIZE[portfolio_size == 2 & portfolio_op == 1], na.rm = TRUE),  # Big size, Low OP
    
    SC = weighted.mean(excess_return[portfolio_size == 1 & portfolio_inv == 1], SIZE[portfolio_size == 1 & portfolio_inv == 1], na.rm = TRUE),  # Small size, Low INV
    SN_inv = weighted.mean(excess_return[portfolio_size == 1 & portfolio_inv == 2], SIZE[portfolio_size == 1 & portfolio_inv == 2], na.rm = TRUE),  # Small size, Neutral INV
    SA = weighted.mean(excess_return[portfolio_size == 1 & portfolio_inv == 3], SIZE[portfolio_size == 1 & portfolio_inv == 3], na.rm = TRUE),  # Small size, High INV
    BC = weighted.mean(excess_return[portfolio_size == 2 & portfolio_inv == 1], SIZE[portfolio_size == 2 & portfolio_inv == 1], na.rm = TRUE),  # Big size, Low INV
    BN_inv = weighted.mean(excess_return[portfolio_size == 2 & portfolio_inv == 2], SIZE[portfolio_size == 2 & portfolio_inv == 2], na.rm = TRUE),  # Big size, Neutral INV
    BA = weighted.mean(excess_return[portfolio_size == 2 & portfolio_inv == 3], SIZE[portfolio_size == 2 & portfolio_inv == 3], na.rm = TRUE)  # Big size, High INV
  ) %>%
  mutate(
    SMB_bm = (SH + SN_bm + SL)/3 - (BH + BN_bm + BL)/3,
    SMB_op = (SR + SN_op + SW)/3 - (BR + BN_op + BW)/3,
    SMB_inv = (SC + SN_inv + SA)/3 - (BC + BN_inv + BA)/3,
    SMB = (SMB_bm + SMB_op + SMB_inv)/3,
    HML = ((SH + BH)/2 - (SL + BL)/2),
    RMW = ((SR + BR)/2 - (SW + BW)/2),
    CMA = ((SC + BC)/2 - (SA + BA)/2)
  ) %>%
  ungroup() %>%
  mutate(
    SMB = SMB * 100,
    HML = HML * 100,
    RMW = RMW * 100,
    CMA = CMA * 100
  ) %>%
  select(YYYYMM, SMB, HML, RMW, CMA)

# View the replicated factors
print(head(factors_replicated))

# Save the factors to a CSV file if needed
write.csv(factors_replicated, "data/ff5.csv", row.names = FALSE)
