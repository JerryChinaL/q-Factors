library(dplyr)

mthret <- readRDS("data/mthret_filtered.rds")
portfolios <- readRDS("data/final_portfolios.rds")

portfolios_w_return <- mthret %>%
  select(KYPERMNO, KYGVKEY, return_date, MTHRET, YYYYMM) %>%
  mutate(sort_date = as.Date(as.character(YYYYMM*100+1), "%Y%m%d")) %>%
  left_join(portfolios, by = c("KYPERMNO", "KYGVKEY", "sort_date" = "monthly_date")) %>%
  select(KYPERMNO, KYGVKEY, sort_date, MTHRET, portfolio_size, portfolio_ia, portfolio_roe, SIZE)

# SIZE: S,B ; I/A: a,m,c ; ROE:  H, M, L
# Calculate the factors according to the provided formula using weighted mean with SIZE
factors_replicated <- portfolios_w_return %>%
  group_by(sort_date) %>%
  summarize(
    SAH = weighted.mean(MTHRET[portfolio_size == 1 & portfolio_ia == 3 & portfolio_roe == 3], 
                        SIZE[portfolio_size == 1 & portfolio_ia == 3 & portfolio_roe == 3], na.rm = TRUE),  # small, aggressive, high roe
    SAM = weighted.mean(MTHRET[portfolio_size == 1 & portfolio_ia == 3 & portfolio_roe == 2],
                        SIZE[portfolio_size == 1 & portfolio_ia == 3 & portfolio_roe == 2], na.rm = TRUE),  # small, aggressive, medium roe
    SAL = weighted.mean(MTHRET[portfolio_size == 1 & portfolio_ia == 3 & portfolio_roe == 1], 
                        SIZE[portfolio_size == 1 & portfolio_ia == 3 & portfolio_roe == 1], na.rm = TRUE),  # small, aggressive, low roe
    SCH = weighted.mean(MTHRET[portfolio_size == 1 & portfolio_ia == 1 & portfolio_roe == 3], 
                        SIZE[portfolio_size == 1 & portfolio_ia == 1 & portfolio_roe == 3], na.rm = TRUE),  # small, conservative, high roe
    SCM = weighted.mean(MTHRET[portfolio_size == 1 & portfolio_ia == 1 & portfolio_roe == 2],
                        SIZE[portfolio_size == 1 & portfolio_ia == 1 & portfolio_roe == 2], na.rm = TRUE),  # small, conservative, medium roe
    SCL = weighted.mean(MTHRET[portfolio_size == 1 & portfolio_ia == 1 & portfolio_roe == 1],
                        SIZE[portfolio_size == 1 & portfolio_ia == 1 & portfolio_roe == 1], na.rm = TRUE),  # small, conservative, low roe
    SMH = weighted.mean(MTHRET[portfolio_size == 1 & portfolio_ia == 2 & portfolio_roe == 3], 
                        SIZE[portfolio_size == 1 & portfolio_ia == 2 & portfolio_roe == 3], na.rm = TRUE),  # medium, aggressive, high roe
    SMM = weighted.mean(MTHRET[portfolio_size == 1 & portfolio_ia == 2 & portfolio_roe == 2],
                        SIZE[portfolio_size == 1 & portfolio_ia == 2 & portfolio_roe == 2], na.rm = TRUE),  # medium, aggressive, medium roe
    SML = weighted.mean(MTHRET[portfolio_size == 1 & portfolio_ia == 2 & portfolio_roe == 1],
                        SIZE[portfolio_size == 1 & portfolio_ia == 2 & portfolio_roe == 1], na.rm = TRUE),  # medium, aggressive, low roe
    
    # Do the same starting with B
    BAH = weighted.mean(MTHRET[portfolio_size == 2 & portfolio_ia == 3 & portfolio_roe == 3], 
                        SIZE[portfolio_size == 2 & portfolio_ia == 3 & portfolio_roe == 3], na.rm = TRUE),  # big, aggressive, high roe
    BAM = weighted.mean(MTHRET[portfolio_size == 2 & portfolio_ia == 3 & portfolio_roe == 2],
                        SIZE[portfolio_size == 2 & portfolio_ia == 3 & portfolio_roe == 2], na.rm = TRUE),  # big, aggressive, medium roe
    BAL = weighted.mean(MTHRET[portfolio_size == 2 & portfolio_ia == 3 & portfolio_roe == 1],
                        SIZE[portfolio_size == 2 & portfolio_ia == 3 & portfolio_roe == 1], na.rm = TRUE),  # big, aggressive, low roe
    BCH = weighted.mean(MTHRET[portfolio_size == 2 & portfolio_ia == 1 & portfolio_roe == 3],
                        SIZE[portfolio_size == 2 & portfolio_ia == 1 & portfolio_roe == 3], na.rm = TRUE),  # big, conservative, high roe
    BCM = weighted.mean(MTHRET[portfolio_size == 2 & portfolio_ia == 1 & portfolio_roe == 2],
                        SIZE[portfolio_size == 2 & portfolio_ia == 1 & portfolio_roe == 2], na.rm = TRUE),  # big, conservative, medium roe
    BCL = weighted.mean(MTHRET[portfolio_size == 2 & portfolio_ia == 1 & portfolio_roe == 1],
                        SIZE[portfolio_size == 2 & portfolio_ia == 1 & portfolio_roe == 1], na.rm = TRUE),  # big, conservative, low roe
    BMH = weighted.mean(MTHRET[portfolio_size == 1 & portfolio_ia == 2 & portfolio_roe == 3], 
                        SIZE[portfolio_size == 1 & portfolio_ia == 2 & portfolio_roe == 3], na.rm = TRUE),  # big, moderate, high roe
    BMM = weighted.mean(MTHRET[portfolio_size == 1 & portfolio_ia == 2 & portfolio_roe == 2],
                        SIZE[portfolio_size == 1 & portfolio_ia == 2 & portfolio_roe == 2], na.rm = TRUE),  # big, moderate, medium roe
    BML = weighted.mean(MTHRET[portfolio_size == 1 & portfolio_ia == 2 & portfolio_roe == 1],
                        SIZE[portfolio_size == 1 & portfolio_ia == 2 & portfolio_roe == 1], na.rm = TRUE),  # big, moderate, low roe
  ) %>%
  mutate(
    SMB = (SAH + SAM + SAL + SCH + SCM + SCL + SMH + SMM + SML)/9 - (BAH + BAM + BAL + BCH + BCM + BCL + BMH + BMM + BML)/9,
    HML = (SAH + SCH + SMH + BAH + BCH + BMH)/6 - (SAL + SCM + SML + BAL + BCM + BML)/6,
    CMA = (SCH + SCM + SCL + BCH + BCM + BCL)/6 - (SAH + SAM + SAL + BAH + BAM + BAL)/6
  ) %>%
  ungroup() %>%
  mutate(
    SMB = SMB * 100,
    HML = HML * 100,
    CMA = CMA * 100
  ) %>%
  select(sort_date, SMB, HML, CMA)

# View the replicated factors
print(head(factors_replicated))

# Save the factors to a CSV file if needed
write.csv(factors_replicated, "data/3-factors.csv", row.names = FALSE)
