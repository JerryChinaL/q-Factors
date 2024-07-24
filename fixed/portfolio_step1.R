library(readxl)
library(dplyr)
library(lubridate)

roe_file <- readRDS("data/ROE_IA.rds")
mktcap <- read.csv("../FF5_Replciation/mkt_cap/mktcap_combined.csv")
primiss <- readRDS("../ELM/data/primiss.rds") %>%
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
  right_join(mkt_data %>% select(-VOL), by = c("KYPERMNO", "monthly_date")) %>% # Return must be present -> right join
  select(PRIMEXCH, VOL, MTHRET, monthly_date, KYPERMNO) %>%
  inner_join(primiss %>% select(monthly_date, KYGVKEY, KYPERMNO), by = c("monthly_date", "KYPERMNO")) # Both return and primiss are needed -> inner join

exchange_data <- mkt_data %>%
  select(KYGVKEY, PRIMEXCH, monthly_date) %>%
  distinct()

ia <- roe_file %>%
  select(KYGVKEY, YYYYMM, IA) %>%
  filter(!is.na(IA)) %>%
  mutate(sort_date = 10000* (round(YYYYMM / 100) + 1) + 701,
         sort_date = as.Date(as.character(sort_date), format = "%Y%m%d"))

mktcap <- mktcap %>%
  mutate(SIZE = coalesce(MKVALTQ, MTHCAP, CSHOQ_PRCCM)) %>%
  filter(!is.na(SIZE) & SIZE != 0 & YYYYMM %% 100 == 6) %>%
  select(KYGVKEY, YYYYMM, SIZE) %>%
  group_by(KYGVKEY, YYYYMM) %>%
  summarize(SIZE = mean(SIZE, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(sort_date = 10000* (round(YYYYMM / 100)) + 701,
         sort_date = as.Date(as.character(sort_date), format = "%Y%m%d"))

ia_size <- mktcap %>%
  full_join(ia, by = c("sort_date", "KYGVKEY")) %>%
  select(KYGVKEY, sort_date, IA, SIZE)

# Left join because exchange code is from mkt_data
ia_size_extended <- mkt_data %>%
  select(KYPERMNO, KYGVKEY, monthly_date, PRIMEXCH, VOL) %>% 
  filter(month(monthly_date) == 6) %>%
  mutate(sort_date = monthly_date + months(1)) %>%
  left_join(ia_size, by = c("KYGVKEY", "sort_date"))

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
ia_size_portfolios <- ia_size_extended %>%
  group_by(sort_date) %>%
  mutate(
    portfolio_size = assign_portfolio(
      data = pick(everything()),
      sorting_variable = SIZE,
      percentiles = c(0, 0.5, 1)
    ),
    portfolio_vol = assign_portfolio(
      data = pick(everything()),
      sorting_variable = VOL,
      percentiles = c(0, 0.5, 1)
    ),
    portfolio_ia = assign_portfolio(
      data = pick(everything()),
      sorting_variable = IA,
      percentiles = c(0, 0.3, 0.7, 1)
    )
  ) %>%
  ungroup() %>%
  select(KYPERMNO, KYGVKEY, sort_date, portfolio_size, portfolio_vol, portfolio_ia, SIZE, IA, VOL)

saveRDS(ia_size_portfolios, "data/ia_size_portfolios_fixed.rds")
