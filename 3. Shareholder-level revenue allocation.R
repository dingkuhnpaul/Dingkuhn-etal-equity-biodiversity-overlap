## =========================================================
## Part III: Shareholder-level revenue allocation (investor exposure)
## =========================================================
# Verbatim port of the original Financial_Data_1.R exposure block. The C/D
# ultimate ownership and the standalone-firm append are produced in Part II
# (files Ultimate_ownership_GLORIA.RDS and Company_division_GLORIA.RDS); here
# we only compute each investor's exposure to firms' HQ country–sector nodes
# as ownership × allocated revenue, aggregated by owner. Reproduces the
# original Exposure_Data.RDS.
#
# ---- NOTE TO REVIEWERS — mapping to the Methods -----------------------
# This script implements the investor-attribution part of "Turnover
# allocation to shareholders". Combining the ultimate equity stakes θ_i,f
# (Part II §4) with the firm turnover distribution Turnover_f,h,s (Part II §3),
# it produces investor-attributed turnover at the firm's headquarter node:
#     Exposure_{i,h,s} = Σ_f θ_i,f · Turnover_f,h,s
# i.e. the θ·Turnover core of eq. 8. The geographic re-allocation by the MRIO
# weights W^IPF_{h,c,s} (the remaining Σ_h W term of eq. 8) is applied later,
# in Parts IV–VI, so that direct (HQ) and total (W-allocated) footprints can
# both be reported. Direct vs. Total ownership give the direct/indirect
# exposure split used downstream.
# ----------------------------------------------------------------------

#setwd("")
rm(list = ls())

library(dplyr)
library(purrr)
library(openxlsx)

## ---------------------------------------------------------
## 0. Load harmonised inputs
## ---------------------------------------------------------

ultimate_ownership   <- readRDS("Data/Processed_Data/Ultimate_ownership_GLORIA.RDS")
company_division     <- readRDS("Data/Processed_Data/Company_division_GLORIA.RDS")
Table_Country_Region <- read.xlsx("Data/Raw_Data/Concordance_tables/country_to_region.xlsx")

## ---------------------------------------------------------
## 1. Owner chunks (to limit RAM)
## ---------------------------------------------------------

all_owners   <- unique(ultimate_ownership$Owner_name)
chunk_size   <- 10000
owner_chunks <- split(all_owners, ceiling(seq_along(all_owners) / chunk_size))

## ---------------------------------------------------------
## 2. Process each owner chunk
## ---------------------------------------------------------

chunk_results <- map(owner_chunks, ~ {
  these_owners <- .x

  uo_chunk <- ultimate_ownership %>% filter(Owner_name %in% these_owners)

  owned_companies_chunk <- unique(uo_chunk$Owned_name)
  cd_chunk <- company_division %>% filter(Company_name %in% owned_companies_chunk)

  owner_node_chunk <- uo_chunk %>%
    inner_join(cd_chunk, by = c("Owned_name" = "Company_name")) %>%
    mutate(
      Direct_exposure   = revenue * Ownership_Direct   / 100,
      Indirect_exposure = revenue * Ownership_Indirect / 100,
      Total_exposure    = revenue * Ownership_Total    / 100
    )

  owner_node_chunk %>%
    group_by(Owner_name, Owner_type, Owner_country, country, sector) %>%
    summarise(
      total_revenue     = sum(revenue,           na.rm = TRUE),
      Direct_exposure   = sum(Direct_exposure,   na.rm = TRUE),
      Indirect_exposure = sum(Indirect_exposure, na.rm = TRUE),
      Total_exposure    = sum(Total_exposure,    na.rm = TRUE),
      .groups = "drop"
    )
})

## ---------------------------------------------------------
## 3. Combine chunks
## ---------------------------------------------------------

owner_country_sector <- bind_rows(chunk_results) %>%
  group_by(Owner_name, Owner_type, Owner_country, country, sector) %>%
  summarise(
    total_revenue     = sum(total_revenue,     na.rm = TRUE),
    Direct_exposure   = sum(Direct_exposure,   na.rm = TRUE),
    Indirect_exposure = sum(Indirect_exposure, na.rm = TRUE),
    Total_exposure    = sum(Total_exposure,    na.rm = TRUE),
    .groups = "drop"
  )

## ---------------------------------------------------------
## 4. Add owner / owned world regions and save
## ---------------------------------------------------------

region <- setNames(Table_Country_Region$World_region, Table_Country_Region$Country)

owner_country_sector$Owner_region <- region[owner_country_sector$Owner_country]
owner_country_sector$Owned_region <- region[owner_country_sector$country]

saveRDS(owner_country_sector, "Data/Processed_Data/Exposure_Data.RDS")
