rm(list = ls())
library(readxl)
library(dplyr)
library(lubridate)
library(tidyr)

roe_file <- readRDS("data/ROE_IA.rds")
mktcap <- read.csv("../FF5_Replciation/mkt_cap/mktcap_combined.csv")
mthret <- readRDS("data/mthret_filtered2.rds")
ia_size_portfolios <- readRDS("data/ia_size_portfolios.rds")

roe <- roe_file %>%
  mutate(
    YYYYMM = ymd(paste0(YYYYMM, "01")),
    RDQ = ymd(as.character(RDQ)),
    # RDQ = if_else(is.na(RDQ), YYYYMM + months(6) + days(2), ymd(as.character(RDQ))),
    RDQ_next = ceiling_date(RDQ, "month")
  ) %>%
  filter(!is.na(ROE) & !is.na(RDQ)) %>%
  select(KYGVKEY, YYYYMM, ROE, RDQ, RDQ_next, FYYYYQ)

# Function to create monthly sequence with an additional 6 months
create_monthly_sequence <- function(min_date, max_date) {
  max_date <- max_date + months(6)
  seq.Date(from = min_date, to = max_date, by = "month")
}

# Generate monthly dates for each KYGVKEY
monthly_dates <- roe %>%
  group_by(KYGVKEY) %>%
  summarise(monthly_date = list(create_monthly_sequence(min(RDQ_next), max(RDQ_next)))) %>%
  unnest(cols = c(monthly_date))

# Since we have complete dates, we could forward fill ROE values, along with its date
# Then it's easy to compare if this forward-filled date is within 6 month of current date
roe_with_dates <- monthly_dates %>%
  left_join(roe, by = c("KYGVKEY", "monthly_date" = "RDQ_next")) %>%
  arrange(KYGVKEY, monthly_date) %>%
  group_by(KYGVKEY) %>%
  fill(ROE, YYYYMM, FYYYYQ, .direction = "down") %>%
  ungroup() %>%
  filter(monthly_date - months(6) <= YYYYMM & !is.na(ROE))

# Handle reports of different quarters in the same date, by using the later quarter.
roe_with_dates <- roe_with_dates %>%
  group_by(KYGVKEY, monthly_date) %>%
  filter(FYYYYQ == max(FYYYYQ)) %>%
  ungroup()

# Merge with permno_data
roe_extended <- mthret %>%
  mutate(monthly_date = floor_date(as.Date(return_date), unit = "month")) %>%
  select(-MTHRET) %>%
  inner_join(roe_with_dates, by = c("KYGVKEY", "monthly_date"))
 
value <- mktcap %>%
  mutate(SIZE = coalesce(MKVALTQ, MTHCAP, CSHOQ_PRCCM)) %>%
  filter(!is.na(SIZE) & SIZE != 0) %>%
  select(KYGVKEY, YYYYMM, SIZE) %>%
  group_by(KYGVKEY, YYYYMM) %>%
  summarize(SIZE = mean(SIZE, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(
    YYYYMM_date = ymd(paste0(YYYYMM, "01")),  # Convert YYYYMM to a Date object by appending "01"
    sort_date = ceiling_date(YYYYMM_date, unit = "month")  # Add one month to YYYYMM_date
  ) %>%
  select(-YYYYMM_date)

roe_extended <- roe_extended %>%
  inner_join(value, by = c("KYGVKEY", "monthly_date" = "sort_date"))

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

roe_extended <- roe_extended %>%
  mutate(sort_date = case_when(
    month(monthly_date) <= 6 ~ paste0(year(monthly_date) - 1, "-07-01"),
    month(monthly_date) >= 7 ~ paste0(year(monthly_date), "-07-01")
  ),
  sort_date = as.Date(sort_date))

# # Apply the portfolio assignment
portfolios <- roe_extended %>%
  left_join(ia_size_portfolios, by = c("KYPERMNO", "KYGVKEY" , "sort_date")) %>%
  group_by(monthly_date) %>%
  mutate(
    portfolio_roe = assign_portfolio(
      data = pick(everything()),
      sorting_variable = ROE,
      percentiles = c(0, 0.3, 0.7, 1)
    )
  ) %>%
  ungroup() %>%
  select(KYPERMNO, KYGVKEY, monthly_date, portfolio_roe, portfolio_size, portfolio_ia, SIZE = SIZE.x)
  
  # Apply the portfolio assignment
# portfolios <- roe_extended %>%
#   left_join(ia_size_portfolios, by = c("KYPERMNO", "KYGVKEY" , "sort_date")) %>%
#   group_by(monthly_date) %>%
#   mutate(
#     portfolio_size = assign_portfolio(
#       data = pick(everything()),
#       sorting_variable = SIZE.x,
#       percentiles = c(0, 0.5, 1)
#     )
#   ) %>%
#   group_by(portfolio_size) %>%
#   mutate(
#     portfolio_roe = assign_portfolio(
#       data = pick(everything()),
#       sorting_variable = ROE,
#       percentiles = c(0, 0.3, 0.7, 1)
#     ),
#     portfolio_ia = assign_portfolio(
#       data = pick(everything()),
#       sorting_variable = IA,
#       percentiles = c(0, 0.3, 0.7, 1)
#     )
#   ) %>%
#   ungroup() %>%
#   select(KYPERMNO, KYGVKEY, monthly_date, portfolio_roe, portfolio_size, portfolio_ia, SIZE = SIZE.x)

saveRDS(portfolios, "data/final_portfolios.rds")







