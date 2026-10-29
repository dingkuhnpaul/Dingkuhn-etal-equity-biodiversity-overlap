## =========================================================
## Descriptive statistics of the sample
## =========================================================
# Produces one clean summary table with:
#   (A) number of firms in the sample (listed parents + subsidiaries)
#       and their market capitalisation
#   (B) number of ultimate shareholders and the market-cap value of the
#       equity attributed to them
#   (C) the same shareholder statistics for the top-1000 shareholders used
#       in the overlap analysis (Part VI)
#
# Market capitalisation is in million USD, inflation-adjusted to 2024
# (as carried in Orbis_cleaned). Only listed parents carry a market cap.
## =========================================================

#setwd("")
rm(list = ls())

library(dplyr)
library(openxlsx)

out_dir <- "Results/Data_Figures_final"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

## ---------------------------------------------------------
## 0. Inputs
## ---------------------------------------------------------

Orbis_cleaned      <- readRDS("Data/Processed_Data/Orbis_cleaned.RDS")
company_division   <- readRDS("Data/Processed_Data/Company_division_GLORIA.RDS")
ultimate_ownership <- readRDS("Data/Processed_Data/Ultimate_ownership_GLORIA.RDS")
Exposure_Data      <- readRDS("Data/Processed_Data/Exposure_Data.RDS")

# Approx. global equity market cap, 2024 (million USD) for share reporting
GLOBAL_MCAP_2024_MUSD <- 126700000
n_top <- 1000                                   # overlap-analysis top-N

## ---------------------------------------------------------
## 1. Firm counts: listed parents + subsidiaries
## ---------------------------------------------------------
# Orbis extract: each parent (BvD ID) carries a market cap; subsidiaries
# (SUB - BvD ID) expand the geographic footprint but are consolidated into
# their parent for the production allocation.

parents_orbis <- Orbis_cleaned %>%
  transmute(Entity_ID  = as.character(`BvD ID number`),
            Entity_name = `Company name Latin alphabet`,
            Market_cap  = as.numeric(`Market capitalisation\nm USD Last avail. yr`)) %>%
  filter(!is.na(Entity_ID)) %>%
  group_by(Entity_ID, Entity_name) %>%
  summarise(Market_cap = if (all(is.na(Market_cap))) NA_real_ else max(Market_cap, na.rm = TRUE),
            .groups = "drop")

n_subsidiaries <- Orbis_cleaned %>%
  transmute(SUB_ID = as.character(`SUB - BvD ID number`)) %>%
  filter(!is.na(SUB_ID), SUB_ID != "") %>%
  distinct() %>% nrow()

# Firms that actually enter the analysis: matched between ultimate ownership
# and the production (company_division) table.
included_firms <- ultimate_ownership %>%
  inner_join(company_division, by = c("Owned_name" = "Company_name")) %>%
  distinct(Owned_ID, Owned_name) %>%
  mutate(Owned_ID = as.character(Owned_ID)) %>%
  left_join(parents_orbis, by = c("Owned_ID" = "Entity_ID"))

n_parents          <- n_distinct(parents_orbis$Entity_ID)
n_firms_included   <- nrow(included_firms)
mcap_included_musd  <- sum(included_firms$Market_cap, na.rm = TRUE)
mcap_coverage_pct   <- 100 * mean(!is.na(included_firms$Market_cap))

## ---------------------------------------------------------
## 2. Market-cap value of equity attributed to ultimate shareholders
## ---------------------------------------------------------
# Identified equity_ij = Ownership_Total_ij / 100 * MarketCap_j

uo_mcap <- ultimate_ownership %>%
  mutate(Owned_ID = as.character(Owned_ID),
         Ownership_Total = as.numeric(Ownership_Total)) %>%
  inner_join(included_firms %>% select(Owned_ID, Market_cap), by = "Owned_ID") %>%
  filter(!is.na(Market_cap), !is.na(Ownership_Total), Ownership_Total > 0) %>%
  mutate(Identified_equity = (Ownership_Total / 100) * Market_cap)

shareholders <- uo_mcap %>%
  group_by(Owner_name) %>%
  summarise(identified_equity_musd = sum(Identified_equity, na.rm = TRUE), .groups = "drop")

n_shareholders        <- nrow(shareholders)
equity_total_musd     <- sum(shareholders$identified_equity_musd, na.rm = TRUE)

## ---------------------------------------------------------
## 3. Top-1000 shareholders used in the overlap analysis (Part VI)
## ---------------------------------------------------------
# Same ranking rule as Part VI: usable, known-region investors, ranked by
# total allocated exposure (turnover).

# Allocated corporate turnover per shareholder (same filters as Part VI)
owner_turnover <- Exposure_Data %>%
  filter(!is.na(country), !is.na(sector), !is.na(Owner_region), Total_exposure > 0) %>%
  group_by(Owner_name) %>%
  summarise(total_exp = sum(Total_exposure, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(total_exp))

top_owners <- owner_turnover %>% slice_head(n = n_top) %>% pull(Owner_name)

# Total allocated corporate turnover, and the share held by the top 1,000 (kUSD)
total_alloc_turnover_kusd <- sum(owner_turnover$total_exp, na.rm = TRUE)
top_alloc_turnover_kusd   <- owner_turnover %>%
  slice_head(n = n_top) %>% summarise(v = sum(total_exp, na.rm = TRUE)) %>% pull(v)

top_equity_musd <- shareholders %>%
  filter(Owner_name %in% top_owners) %>%
  summarise(v = sum(identified_equity_musd, na.rm = TRUE)) %>% pull(v)

n_top_matched <- sum(top_owners %in% shareholders$Owner_name)   # top-1000 with mcap-valued equity

## ---------------------------------------------------------
## 4. Clean summary table
## ---------------------------------------------------------

# All values are coerced to character so the column has a single type.
tn   <- function(musd) format(round(musd / 1e6, 2), nsmall = 2)  # million USD -> "trillion USD"
tn_k <- function(kusd) format(round(kusd / 1e9, 1), nsmall = 1)  # thousand USD -> "trillion USD"
num  <- function(x)     format(x, big.mark = ",")                # integer with thousands sep
pct  <- function(x)     format(round(x, 1), nsmall = 1)          # one-decimal percentage

descriptive_stats <- tibble::tribble(
  ~Metric,                                                        ~Value,
  "Listed companies (parents)",                                    num(n_parents),
  "Subsidiaries",                                                  num(n_subsidiaries),
  "Firms in analysis (matched)",                                   num(n_firms_included),
  "Market cap. of firms in analysis (USD tn, 2024)",               tn(mcap_included_musd),
  "  - share of global market cap. (%)",                           pct(100 * mcap_included_musd / GLOBAL_MCAP_2024_MUSD),
  "Ultimate shareholders (all)",                                   num(n_shareholders),
  "Equity attributed to shareholders (USD tn, 2024)",              tn(equity_total_musd),
  "Equity of top-1,000 shareholders (USD tn, 2024)",               tn(top_equity_musd),
  "  - share of total attributed equity (%)",                      pct(100 * top_equity_musd / equity_total_musd),
  "Total allocated corporate turnover (USD tn, 2024)",             tn_k(total_alloc_turnover_kusd),
  "Allocated turnover of top-1,000 shareholders (USD tn, 2024)",   tn_k(top_alloc_turnover_kusd),
  "  - share of total allocated turnover (%)",                     pct(100 * top_alloc_turnover_kusd / total_alloc_turnover_kusd)
)

print(descriptive_stats, n = Inf)

write.xlsx(descriptive_stats, file.path(out_dir, "Descriptive_statistics.xlsx"), overwrite = TRUE)
write.csv(descriptive_stats, file.path(out_dir, "Descriptive_statistics.csv"), row.names = FALSE)

## Also keep the shareholder-level equity table (all + top-1000 flag) for reuse
shareholders_out <- shareholders %>%
  mutate(in_top_1000 = Owner_name %in% top_owners) %>%
  arrange(desc(identified_equity_musd))
saveRDS(shareholders_out, "Data/Processed_Data/Shareholder_identified_equity.RDS")
