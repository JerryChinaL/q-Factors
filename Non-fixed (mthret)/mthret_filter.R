rm(list = ls())

library(readxl)
library(dplyr)
library(lubridate)

mthret <- read.csv("../FF5_Replciation/four_factor_combined/data/mthret.csv")
exch <- readRDS("../FF5_Replciation/four_factor_combined/data/sfz_agg_mth_short.rds") %>% 
  select(KYPERMNO, YYYYMM, PRIMEXCH)
# primiss_files <- c("6070", "7078", "7885", "8590", "9095", "9500", "0004", "0408", "p0812", "p1216","p1620","p2023")
# primiss_files <- paste0("../HXZ/primiss/", primiss_files, "_ms.xlsx")
# primiss <- bind_rows(lapply(primiss_files, read_excel))

primiss <- readRDS("../FF5_Replciation/four_factor_combined/data/primiss.rds")

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
  mutate(
    year = floor(YYYYMM / 100),
    month = round(YYYYMM %% 100),
    YYYYMM = year * 100 + month
  ) %>%
  select(KYPERMNO, KYGVKEY, MTHRET, return_date = MCALDT, PRIMEXCH, YYYYMM)

# mthret %>%
#   group_by(KYPERMNO, YYYYMM) %>%
#   filter(n() > 1) %>%
#   ungroup() %>%
#   select(KYPERMNO, KYGVKEY) %>%
#   distinct() %>%
#   View("mthret duplicates")

saveRDS(mthret, "data/mthret_filtered2.rds")
