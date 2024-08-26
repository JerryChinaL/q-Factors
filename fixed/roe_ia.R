rm(list = ls())
###################

# This file is used to prepare data for HXZ

###################

library(readxl)
library(dplyr)
library(tidyr)

# All variables needed for ROE
roe_files <- c("./data/6200_2_qi.xlsx", "./data/0024_2_qi.xlsx")
roe <- bind_rows(lapply(roe_files, read_excel))

at <- read_excel("data/AT_2_ai.xlsx")

calculate_yyyymm_vectorized <- function(FYYYYQ, FYRQ, data_source = NULL) {
  year <- floor(FYYYYQ)
  quarter <- round((FYYYYQ - year) * 10)
  month <- case_when(
    quarter == 4 ~ FYRQ,
    quarter == 3 ~ FYRQ - 3,
    quarter == 2 ~ FYRQ - 6,
    quarter == 1 ~ FYRQ - 9
  )
  year <- ifelse(month < 1, year - 1, year)
  month <- ifelse(month < 1, month + 12, month)
  year <- ifelse(FYRQ < 6, year + 1, year)
  as.numeric(sprintf("%04d%02d", year, month))
}

# Build BE and date in the form of YYYYMM
roe <- roe %>%
  mutate(SHE = coalesce(SEQQ , CEQQ + PSTKQ , ATQ - LTQ),
         BVPS = coalesce(PSTKRQ, PSTKQ),
         TXDITCQ = coalesce(TXDITCQ, 0),
         BE = ifelse((SHE + TXDITCQ - BVPS) > 0, (SHE + TXDITCQ - BVPS), NaN)) %>%
  mutate(YYYYMM = calculate_yyyymm_vectorized(FYYYYQ, FYRQ))


roe <- roe %>%
  arrange(KYGVKEY, YYYYMM) %>%
  group_by(KYGVKEY) %>%
  mutate(segment = cumsum(FYRQ != lag(FYRQ, default = first(FYRQ)))) %>%
  ungroup() %>%
  group_by(KYGVKEY, segment) %>%
  mutate(
    be_lag1 = lag(BE),
    FYYYYQ_lag = lag(FYYYYQ),
    # Convert FYYYYQ to numeric quarters
    quarter_current = floor(FYYYYQ) * 4 + (FYYYYQ %% 1) * 10,
    quarter_lag = floor(FYYYYQ_lag) * 4 + (FYYYYQ_lag %% 1) * 10,
    be_lag1 = ifelse((quarter_current - quarter_lag) > 1.5, NA, be_lag1)
  ) %>%
  ungroup() %>%
  select(-quarter_current, -quarter_lag) # Remove temporary columns


at <- at %>%
  mutate(
    FYYYY = case_when(
      FYRA < 6 ~ FYYYY + 1,
      TRUE ~ FYYYY
    ),
    YYYYMM = round(FYYYY * 100 + FYRA)) %>%
  filter(!is.na(YYYYMM))


at <- at %>%
  arrange(KYGVKEY, YYYYMM) %>% # use cumsum to mark places of FYRQ changes
  group_by(KYGVKEY) %>%
  mutate(segment = cumsum(FYRA != lag(FYRA, default = first(FYRA)))) %>%
  ungroup() %>%
  group_by(KYGVKEY, segment) %>% # Within each gvkey and period of stable FYRQ, lag AT
  mutate(
    at_lag1 = lag(AT),
    FYYYY_lag = lag(FYYYY),
    at_lag1 = ifelse(FYYYY - FYYYY_lag > 1, NA, at_lag1)
  ) %>%
  ungroup() %>%
  select(KYGVKEY, YYYYMM, AT, at_lag1)

final_output <- roe %>%
  mutate(ROE = ifelse(be_lag1 == 0, NA, IBQ / be_lag1)) %>%
  filter(!is.na(ROE)) %>%
  full_join(at, by = c("KYGVKEY", "YYYYMM")) %>%
  mutate(IA = ifelse(is.na(at_lag1) | at_lag1 == 0, NA, AT / at_lag1)) %>%
  select(KYGVKEY, YYYYMM, ROE, IA, RDQ, FYYYYQ) %>%
  filter(!is.na(YYYYMM))


saveRDS(final_output, "data/ROE_IA.rds")
