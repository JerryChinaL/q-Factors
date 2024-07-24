library(dplyr)

portfolios <- readRDS("data/final_portfolios_fixed.rds")

# SIZE: S,B ; I/A: a,m,c ; ROE: H, M, L
# Calculate the factors according to the provided formula using weighted mean with SIZE
factors_replicated <- portfolios %>%
  group_by(monthly_date) %>%
  mutate(SIZE = ifelse(is.na(SIZE), 0, SIZE)) %>%
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
                        SIZE[portfolio_size == 1 & portfolio_ia == 2 & portfolio_roe == 3], na.rm = TRUE),  # small, moderate, high roe
    SMM = weighted.mean(MTHRET[portfolio_size == 1 & portfolio_ia == 2 & portfolio_roe == 2],
                        SIZE[portfolio_size == 1 & portfolio_ia == 2 & portfolio_roe == 2], na.rm = TRUE),  # small, moderate, medium roe
    SML = weighted.mean(MTHRET[portfolio_size == 1 & portfolio_ia == 2 & portfolio_roe == 1],
                        SIZE[portfolio_size == 1 & portfolio_ia == 2 & portfolio_roe == 1], na.rm = TRUE),  # small, moderate, low roe
    
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
    BMH = weighted.mean(MTHRET[portfolio_size == 2 & portfolio_ia == 2 & portfolio_roe == 3], 
                        SIZE[portfolio_size == 2 & portfolio_ia == 2 & portfolio_roe == 3], na.rm = TRUE),  # big, moderate, high roe
    BMM = weighted.mean(MTHRET[portfolio_size == 2 & portfolio_ia == 2 & portfolio_roe == 2],
                        SIZE[portfolio_size == 2 & portfolio_ia == 2 & portfolio_roe == 2], na.rm = TRUE),  # big, moderate, medium roe
    BML = weighted.mean(MTHRET[portfolio_size == 2 & portfolio_ia == 2 & portfolio_roe == 1],
                        SIZE[portfolio_size == 2 & portfolio_ia == 2 & portfolio_roe == 1], na.rm = TRUE)   # big, moderate, low roe
  ) %>%
  mutate(
    SMB = (SAH + SAM + SAL + SCH + SCM + SCL + SMH + SMM + SML)/9 - (BAH + BAM + BAL + BCH + BCM + BCL + BMH + BMM + BML)/9,
    HML = (SAH + SCH + SMH + BAH + BCH + BMH)/6 - (SAL + SCL + SML + BAL + BCL + BML)/6,
    CMA = (SCH + SCM + SCL + BCH + BCM + BCL)/6 - (SAH + SAM + SAL + BAH + BAM + BAL)/6
  ) %>%
  ungroup() %>%
  mutate(
    SMB = SMB * 100,
    HML = HML * 100,
    CMA = CMA * 100
  ) %>%
  select(monthly_date, SMB, HML, CMA)

# View the replicated factors
print(head(factors_replicated))

# Save the factors to a CSV file if needed
write.csv(factors_replicated, "data/3-factors-fixed.csv", row.names = FALSE)
