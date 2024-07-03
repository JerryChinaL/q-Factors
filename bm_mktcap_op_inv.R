##############

# This file joins the be_op_inv output with the mktcap data to calculate the SIZE and bm factor.
# It also joins with the mthret and rf-return data to calculate the excess return. 

#The final dataframe only contains rows with non-empty returns, stocks in NAQ exchanges, and primary stocks.

##############

library(readxl)
library(dplyr)
library(lubridate)

mktcap <- read.csv("../FF5_Replciation/mkt_cap/mktcap_combined.csv")
roe_ia <- read.csv("data/ROE_IA.csv")
mthret <- read.csv("../FF5_Replciation/four_factor_combined/data/mthret.csv")
exch <- readRDS("../FF5_Replciation/mkt_cap/data/sfz_agg_mth_short.rds") %>% 
  select(KYPERMNO, YYYYMM, PRIMEXCH)
primiss_files <- c("6070", "7078", "7885", "8590", "9095", "9500", "0004", "0408", "p0812", "p1216","p1620","p2023")
primiss_files <- paste0("../FF5_Replciation/four_factor_combined/primiss/", primiss_files, "_ms.xlsx")
primiss <- bind_rows(lapply(primiss_files, read_excel))

primiss <- primiss %>%
  mutate(
    DATADATE = as.Date(as.character(DATADATE), "%Y-%m-%d"),
    YYYYMM = as.numeric(format(DATADATE, "%Y%m"))
  ) %>% 
  filter(PRIMISS == 'P') %>% 
  select(KYPERMNO, YYYYMM) %>% 
  distinct()

mthret <- mthret %>%
  left_join(exch, by = c("KYPERMNO", "YYYYMM")) %>%
  filter(PRIMEXCH == "N" | PRIMEXCH == "A" | PRIMEXCH == "Q") %>%
  inner_join(primiss, by = c("KYPERMNO", "YYYYMM")) %>% # use primary stock
  select(KYPERMNO, KYGVKEY, YYYYMM, MTHRET, return_date = MCALDT, PRIMEXCH)

size <- mktcap %>%
  mutate(SIZE = coalesce(MKVALTQ, MTHCAP, CSHOQ_PRCCM)) %>%
  filter(!is.na(SIZE) & SIZE != 0) %>%
  select(KYGVKEY, YYYYMM, SIZE) %>%
  group_by(KYGVKEY, YYYYMM) %>%
  summarize(SIZE = mean(SIZE, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(size_factor)

# Full join (be, op, inv) and mktcap on KYGVKEY and YYYYMM
combined_df <- size %>%
  left_join(roe_ia, by = c("KYGVKEY", "YYYYMM"))

final_df <- left_join(mthret, combined_df, by = c("KYGVKEY", "sort_date"))

# add the excess return column by appending rf rate then subtracting.
rf_data <- read.csv("../FF5_Replciation/monthly_rf.csv")
hxz_factors_excet <- final_df %>% 
  mutate(rf_date = format(as.Date(return_date), "%Y%m")) %>%
  left_join(rf_data %>% mutate(rf_date = as.character(X)) %>% select(rf_date, RF), by = c("rf_date")) %>%
  select(-c(rf_date)) %>%
  mutate(excess_return = MTHRET - (RF/100))

write.csv(four_factors_excret, "data/four_factors_excret.csv", row.names = FALSE)

# View the final dataframe for inspection
View(final_df %>% filter(KYGVKEY == 1690))

