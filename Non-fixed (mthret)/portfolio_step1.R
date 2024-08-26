rm(list = ls())

library(readxl)
library(dplyr)

roe_file <- readRDS("data/ROE_IA.rds")
mktcap <- read.csv("../FF5_Replciation/mkt_cap/mktcap_combined.csv")
mthret <- readRDS("data/mthret_filtered2.rds")

# View(roe_file %>% filter(!is.na(ROE) & !is.na(IA)))

permno_data <- mthret %>%
  filter(round(YYYYMM %% 100) == 7)

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
  left_join(ia, by = c("sort_date", "KYGVKEY")) %>%
  select(KYGVKEY, sort_date, IA, SIZE)

ia_size_extended <- permno_data %>%
  mutate(sort_date = as.Date(sort_date)) %>%
  select(-c("MTHRET")) %>%
  inner_join(ia_size, by = c("KYGVKEY", "sort_date"))

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
    portfolio_ia = assign_portfolio(
      data = pick(everything()),
      sorting_variable = IA,
      percentiles = c(0, 0.3, 0.7, 1)
    )
  ) %>%
  ungroup() %>%
  select(KYPERMNO, KYGVKEY, sort_date, portfolio_size, portfolio_ia, SIZE, IA)

saveRDS(ia_size_portfolios, "data/ia_size_portfolios.rds")
