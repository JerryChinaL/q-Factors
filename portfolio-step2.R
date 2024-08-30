rm(list = ls())
library(readxl)
library(dplyr)
library(lubridate)
library(tidyr)

roe_file <- readRDS("data/ROE_IA.rds")
mktcap <- read.csv("../FF5_Replciation/mkt_cap/mktcap_combined.csv")
ia_size_portfolios <- readRDS("data/ia_size_portfolios_fixed.rds")

primiss <- readRDS("../FF5_Replciation/four_factor_combined/data/primiss.rds") %>%
  mutate(monthly_date = floor_date(DATADATE, "month")) %>%
  filter(PRIMISS == "P") %>%
  distinct()

mkt_data <- readRDS("../ELM/data/sfz_agg_mth_short.rds") %>%
  mutate(monthly_date = floor_date(MCALDT, "month"),
         VOL = MTHVOL * MTHPRC) %>%
  filter(PRIMEXCH %in% c("N", "A", "Q"))

# Return should be for this month, but VOL, as a factor, should be from last month
mkt_data <- mkt_data %>%
  select(VOL, KYPERMNO, monthly_date) %>%
  mutate(monthly_date = monthly_date + months(1)) %>%
  right_join(mkt_data %>% select(-VOL), by = c("KYPERMNO", "monthly_date")) %>%
  select(PRIMEXCH, VOL, MTHRET, monthly_date, KYPERMNO) %>%
  inner_join(primiss %>% select(monthly_date, KYGVKEY, KYPERMNO), by = c("monthly_date", "KYPERMNO"))

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

value <- mktcap %>%
  mutate(SIZE = coalesce(MKVALTQ, MTHCAP, CSHOQ_PRCCM)) %>%
  filter(!is.na(SIZE) & SIZE != 0) %>%
  select(KYGVKEY, YYYYMM, SIZE) %>%
  group_by(KYGVKEY, YYYYMM) %>%
  summarize(SIZE = mean(SIZE, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(
    YYYYMM_date = ymd(paste0(YYYYMM, "01")),  # Convert YYYYMM to a Date object by appending "01"
    monthly_date = ceiling_date(YYYYMM_date, unit = "month")  # Add one month to YYYYMM_date
  ) %>%
  select(-YYYYMM_date)

roe_mktcap <- roe_with_dates %>%
  full_join(value, by = c("KYGVKEY", "monthly_date"))

permno_roe_mktcap <- mkt_data %>%
  left_join(roe_mktcap %>% select(KYGVKEY, monthly_date, ROE, SIZE), by = c("KYGVKEY", "monthly_date"))


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

permno_roe_mktcap <- permno_roe_mktcap %>%
  mutate(sort_date = case_when(
    month(monthly_date) <= 6 ~ paste0(year(monthly_date) - 1, "-07-01"),
    month(monthly_date) >= 7 ~ paste0(year(monthly_date), "-07-01")
  ),
  sort_date = as.Date(sort_date))

# # Apply the portfolio assignment
portfolios <- permno_roe_mktcap %>%
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
  select(KYPERMNO, PRIMEXCH, KYGVKEY, monthly_date, portfolio_roe, portfolio_size, portfolio_vol, portfolio_ia, SIZE = SIZE.x, VOL = VOL.x, ROE, IA, MTHRET)

saveRDS(portfolios, "data/final_portfolios_fixed.rds")







