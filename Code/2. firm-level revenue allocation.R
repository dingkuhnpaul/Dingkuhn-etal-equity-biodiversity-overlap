## =========================================================
## Part II: Firm-level revenue allocation
## =========================================================
# Verbatim port of the original Financial_Data_1.R logic (Orbis cleaning →
# ownership edges → company divisions → C/D ultimate ownership → GLORIA
# harmonisation → standalone-firm append → revenue-allocation matrix W),
# reorganised into the two-file structure and using the current
# concordance-table names. Reproduces the original results.
#
# Order matters: standalone firms are appended from the ultimate-ownership
# result (§5), and W (§6) is built from the final company_divisions, exactly
# as in the original pipeline.
#
# ---- NOTE TO REVIEWERS — mapping to the Methods -----------------------
# This script implements the "Firm-level revenue allocation" subsection and
# the ownership part of "Turnover allocation to shareholders":
#   §1     Orbis firm/turnover data, CPI-adjusted to 2024 USD (ref. 10, 52).
#   §3     STEP 1 — allocation by subsidiary structure. Subsidiary turnover is
#          reconciled with parent consolidated accounts (IFRS 10, ref. 53):
#          if Σ subs > parent, subs are scaled to the consolidated total; if
#          parent > Σ subs, the residual is a parent node. Yields Turnover_f,h,s.
#          NACE Rev. 2 → GLORIA via ref. 54 (§5).
#   §4     Ultimate shareholders via the C/D decomposition of the direct
#          ownership matrix; provides direct + indirect equity stakes θ_i,f
#          (the "computed indirect equity stakes" of the Methods; see SI). The
#          ~400,000 ultimate shareholders are classified following Battiston
#          et al. (ref. 56) in §5.1.
#   §5     GLORIA country/sector harmonisation of firms and owners.
#   §6     STEP 2 — MRIO sourcing shares W_{h,c,s} from Z and Y            (eq. 1).
#   §7     STEP 3 — IPF/RAS reconciliation with GLORIA gross output:
#            R_{h,s} = Σ_f Turnover_f,h,s                                  (eq. 2)
#            T_{c,s} = R_s x_{c,s} / Σ_c' x_{c',s}                         (eq. 3)
#            A⁰_{h,c,s} = W_{h,c,s} R_{h,s}                                (eq. 4)
#            row scaling to R_{h,s} (eq. 5) alternated with column scaling
#            to T_{c,s} (eq. 6) until convergence; W^IPF = A / R_{h,s}     (eq. 7).
#          Only the top-50 allocation countries per (h,s) are kept and
#          renormalised, as stated at the end of Step 3.
# The investor-attributed turnover Y_{i,s,c} (eq. 8) is assembled in Part III.
# ----------------------------------------------------------------------

#setwd("")
rm(list = ls())

library(readxl)
library(data.table)
library(dplyr)
library(tidyr)
library(purrr)
library(Matrix)

## ---------------------------------------------------------
## 0. Inputs
## ---------------------------------------------------------

CPI <- read_excel("Data/Raw_Data/CPI_US.xlsx", sheet = "Sheet1")
CPI$Year                     <- as.double(gsub("[^0-9]",   "", trimws(as.character(CPI$Year))))
CPI$`Annual Average CPI(-U)` <- as.double(gsub("[^0-9.]", "", trimws(as.character(CPI$`Annual Average CPI(-U)`))))

cpi_2024 <- CPI$`Annual Average CPI(-U)`[CPI$Year == 2024]

## ---------------------------------------------------------
## 1. Load Orbis data, deduplicate names, save
## ---------------------------------------------------------

cols_to_num <- c(
  "SUB - Operating revenue (Turnover)\nm USD",
  "Market capitalisation\nm USD Last avail. yr",
  "Operating revenue (Turnover)\nth USD Last avail. yr"
)

Orbis_cleaned <- lapply(1:21, function(i) {

  df <- read_excel(paste0("Data/Raw_Data/ORBIS_20251127/d", i, ".xlsx"), sheet = "Results")

  cpi_year <- CPI$`Annual Average CPI(-U)`[match(df$`Last avail. year`, CPI$Year)]
  if (any(is.na(cpi_year))) warning(paste("Batch", i, ": unmatched CPI years for some rows"))

  for (cl in cols_to_num) {
    df[[cl]] <- as.numeric(gsub("[^0-9.-]", "", df[[cl]])) * (cpi_2024 / cpi_year)
  }

  df$`SUB - Operating revenue (Turnover)\nth USD` <-
    df$`SUB - Operating revenue (Turnover)\nm USD` * 1000

  df <- df %>%
    mutate(across(
      c(`SH - Type`, `Type of entity`),
      ~ ifelse(. == "Mutual and pension fund/Nominee/Trust/Trustee",
               "Mutual and pension fund, nominee, trust, trustee",
               .)
    ))

  df
}) %>% bind_rows()

# Drop duplicated company names: keep the record with a NACE code, then the
# largest market cap / turnover (as in the original pipeline). Ownership
# percentages are left as raw strings and coerced to numeric later.
dup_flagged <- Orbis_cleaned %>%
  filter(!is.na(...1)) %>%
  group_by(`Company name Latin alphabet`) %>%
  filter(n() > 1) %>%
  mutate(
    .has_nace = !is.na(`NACE Rev. 2, core code (4 digits)`),
    .mcap     = suppressWarnings(as.numeric(`Market capitalisation\nm USD Last avail. yr`)),
    .turnover = suppressWarnings(as.numeric(`Operating revenue (Turnover)\nth USD Last avail. yr`))
  ) %>%
  arrange(`Company name Latin alphabet`, desc(.has_nace), desc(.mcap), desc(.turnover)) %>%
  mutate(winner = row_number() == 1) %>%
  ungroup() %>%
  filter(!winner)

Orbis_cleaned <- Orbis_cleaned %>%
  filter(!`BvD ID number` %in% dup_flagged$`BvD ID number`)

saveRDS(Orbis_cleaned, "Data/Processed_Data/Orbis_cleaned.RDS")

## ---------------------------------------------------------
## 2. Build ownership edge list
## ---------------------------------------------------------

rm(list = ls())
library(dplyr)

Orbis_cleaned <- readRDS("Data/Processed_Data/Orbis_cleaned.RDS")

edges_sh <- data.frame(
  Owner_ID                 = Orbis_cleaned$`SH - BvD ID number`,
  Owner_name               = Orbis_cleaned$`SH - Name`,
  Owner_NACE_code          = Orbis_cleaned$`SH - NACE,Core code`,
  Owner_NACE_description   = Orbis_cleaned$`SH - NACE,text description`,
  Owner_type               = Orbis_cleaned$`SH - Type`,
  Owner_country            = Orbis_cleaned$`SH - Country ISO code`,
  Owner_consolidation_code = rep(NA, nrow(Orbis_cleaned)),
  Owned_ID                 = Orbis_cleaned$`BvD ID number`,
  Owned_name               = Orbis_cleaned$`Company name Latin alphabet`,
  Owned_NACE_code          = Orbis_cleaned$`NACE Rev. 2, core code (4 digits)`,
  Owned_NACE_description   = Orbis_cleaned$`NACE Rev. 2, core code - description`,
  Owned_type               = Orbis_cleaned$`Type of entity`,
  Owned_GUO                = Orbis_cleaned$`Entity type`,
  Owned_country            = Orbis_cleaned$`Country ISO code`,
  Owned_turnover           = Orbis_cleaned$`Operating revenue (Turnover)\nth USD Last avail. yr`,
  Ownership_Direct         = Orbis_cleaned$`SH - Direct %`,
  Ownership_Total          = Orbis_cleaned$`SH - Total %`
)

edges_sub <- data.frame(
  Owner_ID                 = Orbis_cleaned$`BvD ID number`,
  Owner_name               = Orbis_cleaned$`Company name Latin alphabet`,
  Owner_NACE_code          = Orbis_cleaned$`NACE Rev. 2, core code (4 digits)`,
  Owner_NACE_description   = Orbis_cleaned$`NACE Rev. 2, core code - description`,
  Owner_type               = Orbis_cleaned$`Type of entity`,
  Owner_country            = Orbis_cleaned$`Country ISO code`,
  Owner_consolidation_code = Orbis_cleaned$`Consolidation code`,
  Owned_ID                 = Orbis_cleaned$`SUB - BvD ID number`,
  Owned_name               = Orbis_cleaned$`SUB - Name`,
  Owned_NACE_code          = Orbis_cleaned$`SUB - NACE,Core code`,
  Owned_NACE_description   = Orbis_cleaned$`SUB - NACE,text description`,
  Owned_type               = rep("Corporate", nrow(Orbis_cleaned)),
  Owned_GUO                = rep("Controlled subs.", nrow(Orbis_cleaned)),
  Owned_country            = Orbis_cleaned$`SUB - Country ISO code`,
  Owned_turnover           = Orbis_cleaned$`SUB - Operating revenue (Turnover)\nth USD`,
  Ownership_Direct         = Orbis_cleaned$`SUB - Direct %`,
  Ownership_Total          = Orbis_cleaned$`SUB - Total %`
)

Ownership_edges <- rbind(edges_sh, edges_sub)
rm(edges_sh, edges_sub)

# Remove rows with missing IDs or with no ownership value (strings still)
Ownership_edges <- subset(Ownership_edges, !is.na(Owner_ID) & !is.na(Owned_ID))
Ownership_edges <- subset(Ownership_edges, !is.na(Ownership_Direct) | !is.na(Ownership_Total))

# Self-ownership correction
idx <- Ownership_edges$Owner_ID == "ZZ*J0088500"
Ownership_edges$Owner_ID[idx] <- Ownership_edges$Owned_ID[idx]
rm(idx)

# Remove non-positive owned turnover
Ownership_edges <- subset(Ownership_edges, Owned_turnover > 0)

# Remove duplicate edges, keeping those with a consolidation code
Ownership_edges <- Ownership_edges %>%
  group_by(Owner_ID, Owned_ID) %>%
  arrange(is.na(Owner_consolidation_code)) %>%
  slice(1) %>%
  ungroup()

# "-" -> NA on NACE, drop edges with unknown owned sector
Ownership_edges <- Ownership_edges %>%
  mutate(
    Owner_NACE_code        = na_if(Owner_NACE_code, "-"),
    Owner_NACE_description = na_if(Owner_NACE_description, "-"),
    Owned_NACE_code        = na_if(Owned_NACE_code, "-"),
    Owned_NACE_description = na_if(Owned_NACE_description, "-")
  ) %>%
  filter(!is.na(Owned_NACE_code))

# Ensure ownership numeric (non-numeric provider codes -> NA)
Ownership_edges$Ownership_Total  <- as.numeric(Ownership_edges$Ownership_Total)
Ownership_edges$Ownership_Direct <- as.numeric(Ownership_edges$Ownership_Direct)

saveRDS(Ownership_edges, "Data/Processed_Data/Ownership_edges.RDS")

## ---------------------------------------------------------
## 3. Deal with subsidiary double counting; company divisions
## ---------------------------------------------------------
# REVIEWERS: STEP 1 of the revenue allocation. `controlled_pairs` selects
# majority-owned (>=50%) consolidated subsidiaries (C1/C2). The allocation
# rule (subs_allocations / parent_residuals) implements the IFRS-consistent
# reconciliation of eq. text: subs scaled down when Σ subs > parent, residual
# assigned to the parent node otherwise. Output `company_divisions` = the
# initial geographic turnover distribution Turnover_f,h,s.

rm(list = ls())
library(dplyr); library(tidyr)

Ownership_edges <- readRDS("Data/Processed_Data/Ownership_edges.RDS")

Ownership_edges <- Ownership_edges %>%
  mutate(
    Owner_ID         = as.character(Owner_ID),
    Owned_ID         = as.character(Owned_ID),
    Owned_turnover   = as.numeric(Owned_turnover),
    Ownership_Direct = as.numeric(Ownership_Direct),
    Ownership_Total  = as.numeric(Ownership_Total)
  )

all_owned_ids   <- unique(Ownership_edges$Owned_ID)
ultimate_owners <- setdiff(unique(Ownership_edges$Owner_ID), all_owned_ids)

Ownership_edges_firm <- Ownership_edges %>%
  filter(!Owner_ID %in% ultimate_owners)

firm_pairs <- Ownership_edges_firm %>%
  group_by(Owner_ID, Owned_ID) %>%
  summarise(
    Ownership_Direct         = if (all(is.na(Ownership_Direct))) NA_real_ else max(Ownership_Direct, na.rm = TRUE),
    Ownership_Total          = if (all(is.na(Ownership_Total)))  NA_real_ else max(Ownership_Total,  na.rm = TRUE),
    Owner_consolidation_code = first(Owner_consolidation_code),
    Owned_GUO                = first(Owned_GUO),
    .groups = "drop"
  ) %>%
  mutate(
    Ownership_Direct    = replace_na(Ownership_Direct, 0),
    Ownership_Total     = replace_na(Ownership_Total, 0),
    Ownership_Effective = pmax(Ownership_Direct, Ownership_Total)
  )

firm_turnover <- Ownership_edges_firm %>%
  filter(!is.na(Owned_turnover)) %>%
  group_by(Owned_ID) %>%
  summarise(turnover = max(Owned_turnover, na.rm = TRUE), .groups = "drop") %>%
  rename(Firm_ID = Owned_ID)

firm_attr <- Ownership_edges_firm %>%
  select(Owned_ID, Owned_NACE_description, Owned_NACE_code, Owned_country) %>%
  distinct() %>%
  rename(Firm_ID = Owned_ID, sector = Owned_NACE_description,
         sector_code = Owned_NACE_code, country = Owned_country)

firm_names <- Ownership_edges_firm %>%
  select(Firm_ID = Owned_ID, Company_name = Owned_name) %>%
  distinct()

controlled_pairs <- firm_pairs %>%
  left_join(firm_turnover, by = c("Owned_ID" = "Firm_ID")) %>%
  rename(turnover_sub = turnover) %>%
  left_join(firm_turnover, by = c("Owner_ID" = "Firm_ID")) %>%
  rename(turnover_parent = turnover) %>%
  filter(
    Ownership_Effective >= 50,
    !(Owned_GUO %in% c("Independent co", "GUO")),
    Owner_consolidation_code %in% c("C1", "C2"),
    !is.na(turnover_sub), !is.na(turnover_parent),
    turnover_sub < turnover_parent
  ) %>%
  select(Owner_ID, Owned_ID, Ownership_Effective, turnover_sub, turnover_parent)

controlled_sub_ids <- unique(controlled_pairs$Owned_ID)

subs_info <- controlled_pairs %>%
  left_join(firm_attr, by = c("Owned_ID" = "Firm_ID")) %>%
  rename(sector_sub = sector, sector_code_sub = sector_code, country_sub = country) %>%
  left_join(firm_attr, by = c("Owner_ID" = "Firm_ID")) %>%
  rename(sector_parent = sector, sector_code_parent = sector_code, country_parent = country)

parent_summaries <- subs_info %>%
  group_by(Owner_ID) %>%
  summarise(
    turnover_parent    = first(turnover_parent),
    sum_sub_turnover   = sum(turnover_sub, na.rm = TRUE),
    sector_parent      = first(sector_parent),
    sector_code_parent = first(sector_code_parent),
    country_parent     = first(country_parent),
    .groups = "drop"
  ) %>%
  mutate(turnover_parent = ifelse(is.na(turnover_parent), sum_sub_turnover, turnover_parent))

subs_with_parent <- subs_info %>%
  left_join(parent_summaries, by = "Owner_ID", suffix = c("", "_group"))

subs_allocations <- subs_with_parent %>%
  mutate(
    Tm = turnover_parent, S = sum_sub_turnover,
    weight = ifelse(S > 0, turnover_sub / S, 0),
    revenue_node = case_when(
      is.na(Tm)                      ~ turnover_sub,
      Tm > S  & !is.na(turnover_sub) ~ turnover_sub,
      Tm <= S & S > 0                ~ weight * Tm,
      TRUE                           ~ 0
    )
  ) %>%
  transmute(Company_ID = Owner_ID, country = country_sub,
            sector = sector_sub, sector_code = sector_code_sub, revenue = revenue_node)

parent_residuals <- parent_summaries %>%
  mutate(residual = ifelse(!is.na(turnover_parent) & turnover_parent > sum_sub_turnover,
                           turnover_parent - sum_sub_turnover, 0)) %>%
  filter(residual > 0) %>%
  transmute(Company_ID = Owner_ID, country = country_parent,
            sector = sector_parent, sector_code = sector_code_parent, revenue = residual)

non_sub_firms <- setdiff(firm_turnover$Firm_ID, controlled_sub_ids)

company_divisions <- bind_rows(subs_allocations, parent_residuals) %>%
  filter(Company_ID %in% non_sub_firms) %>%
  group_by(Company_ID, country, sector, sector_code) %>%
  summarise(revenue = sum(revenue, na.rm = TRUE), .groups = "drop")

company_divisions <- company_divisions %>%
  left_join(firm_names, by = c("Company_ID" = "Firm_ID")) %>%
  group_by(Company_ID, Company_name) %>%
  mutate(
    total_revenue    = sum(revenue, na.rm = TRUE),
    main_country     = { w <- ifelse(is.na(country),     -Inf, revenue); if (all(is.infinite(w))) NA_character_ else country[which.max(w)] },
    main_sector      = { w <- ifelse(is.na(sector),      -Inf, revenue); if (all(is.infinite(w))) NA_character_ else sector[which.max(w)] },
    main_sector_code = { w <- ifelse(is.na(sector_code), -Inf, revenue); if (all(is.infinite(w))) NA_character_ else sector_code[which.max(w)] },
    country          = ifelse(is.na(country),     main_country,     country),
    sector           = ifelse(is.na(sector),      main_sector,      sector),
    sector_code      = ifelse(is.na(sector_code), main_sector_code, sector_code),
    share            = ifelse(total_revenue > 0, revenue / total_revenue, NA_real_)
  ) %>%
  ungroup() %>%
  select(Company_name, country, sector, sector_code, revenue, share)

firm_edges_clean     <- Ownership_edges_firm %>% filter(!Owned_ID %in% controlled_sub_ids)
ultimate_owner_edges <- Ownership_edges      %>% filter(Owner_ID  %in% ultimate_owners)
Ownership_edges_clean <- bind_rows(ultimate_owner_edges, firm_edges_clean) %>% distinct()

saveRDS(Ownership_edges_clean, "Data/Processed_Data/Ownership_edges_subs_out.RDS")
saveRDS(company_divisions,     "Data/Processed_Data/Company_division.RDS")

## ---------------------------------------------------------
## 4. Ultimate ownership (C/D decomposition)
## ---------------------------------------------------------
# REVIEWERS: this recovers ultimate shareholders and their total (direct +
# indirect) equity stakes θ_i,f by disentangling chains of direct ownership
# (the "computed indirect equity stakes" of the Methods; full derivation in
# the Supplementary Information). U = D (I − C)⁻¹, where C is firm-to-firm
# cross-holdings and D is ultimate-owner-to-firm ownership. θ_i,f then enters
# the investor-attributed turnover Y_{i,s,c} (eq. 8, computed in Part III).

rm(list = ls())
library(dplyr); library(tidyr); library(Matrix)

Ownership_edges <- readRDS("Data/Processed_Data/Ownership_edges_subs_out.RDS")

Ownership_edges_direct <- Ownership_edges %>%
  filter(!is.na(Ownership_Direct), Ownership_Direct != 0) %>%
  select(-Ownership_Total) %>%
  mutate(Owner_ID = as.character(Owner_ID), Owned_ID = as.character(Owned_ID))

nodes      <- sort(unique(c(Ownership_edges_direct$Owner_ID, Ownership_edges_direct$Owned_ID)))
node_index <- setNames(seq_along(nodes), nodes)

A <- sparseMatrix(
  i = node_index[Ownership_edges_direct$Owner_ID],
  j = node_index[Ownership_edges_direct$Owned_ID],
  x = Ownership_edges_direct$Ownership_Direct / 100,
  dims = c(length(nodes), length(nodes)),
  dimnames = list(nodes, nodes)
)

self_share        <- diag(A)
denom             <- 1 - self_share
denom[denom <= 0] <- 1
A_self       <- A %*% Diagonal(x = 1 / denom)
diag(A_self) <- 0

all_owners      <- unique(Ownership_edges_direct$Owner_ID)
all_owned       <- unique(Ownership_edges_direct$Owned_ID)
ultimate_owners <- setdiff(all_owners, all_owned)
firms           <- setdiff(nodes, ultimate_owners)

ult_idx  <- match(ultimate_owners, nodes); ult_idx  <- ult_idx[!is.na(ult_idx)]
firm_idx <- match(firms,           nodes); firm_idx <- firm_idx[!is.na(firm_idx)]

if (length(ult_idx) == 0 || length(firm_idx) == 0) {
  warning("No ultimate owners or firms; cannot compute C/D.")
  ultimate_ownership_df <- tibble()
} else {
  C <- A_self[firm_idx, firm_idx, drop = FALSE]
  rownames(C) <- colnames(C) <- nodes[firm_idx]
  D <- A_self[ult_idx, firm_idx, drop = FALSE]
  rownames(D) <- nodes[ult_idx]; colnames(D) <- nodes[firm_idx]

  U <- D %*% solve(Diagonal(nrow(C)) - C)

  ultimate_ownership_df <- summary(U) %>%
    as.data.frame() %>%
    rename(row_index = i, col_index = j, Ownership_fraction = x) %>%
    mutate(
      Owner_ID          = rownames(U)[row_index],
      Owned_ID          = colnames(U)[col_index],
      Ownership_percent = Ownership_fraction * 100
    ) %>%
    select(Owner_ID, Owned_ID, Ownership_percent)
}

provider_df <- Ownership_edges %>%
  transmute(
    Owner_ID = as.character(Owner_ID), Owned_ID = as.character(Owned_ID),
    Ownership_Direct = as.numeric(Ownership_Direct),
    Ownership_Total_provider = as.numeric(Ownership_Total)
  )

ultimate_ownership_df <- ultimate_ownership_df %>%
  left_join(provider_df, by = c("Owner_ID", "Owned_ID")) %>%
  mutate(
    Ownership_Total  = pmax(Ownership_percent, Ownership_Direct, Ownership_Total_provider, na.rm = TRUE),
    Ownership_Direct = replace_na(Ownership_Direct, 0),
    Ownership_Total  = replace_na(Ownership_Total, 0)
  ) %>%
  select(Owner_ID, Owned_ID, Ownership_Direct, Ownership_Total)

ultimate_ownership_df_full <- ultimate_ownership_df %>%
  left_join(
    Ownership_edges %>%
      select(Owner_ID, Owner_consolidation_code, Owner_name, Owner_NACE_code,
             Owner_NACE_description, Owner_type, Owner_country) %>%
      distinct(Owner_ID, .keep_all = TRUE),
    by = "Owner_ID"
  ) %>%
  left_join(
    Ownership_edges %>%
      select(Owned_ID, Owned_name, Owned_NACE_code, Owned_NACE_description,
             Owned_type, Owned_GUO, Owned_country, Owned_turnover) %>%
      distinct(Owned_ID, .keep_all = TRUE),
    by = "Owned_ID"
  ) %>%
  mutate(Ownership_Indirect = Ownership_Total - Ownership_Direct)

saveRDS(ultimate_ownership_df_full, "Data/Processed_Data/Ultimate_ownership_df.RDS")

## ---------------------------------------------------------
## 5. Harmonise classifications and append standalone firms
## ---------------------------------------------------------

rm(list = ls())
library(dplyr); library(readxl); library(tidyr)

company_division   <- readRDS("Data/Processed_Data/Company_division.RDS")
ultimate_ownership <- readRDS("Data/Processed_Data/Ultimate_ownership_df.RDS")

# ---- 5.1 Reclassify owner types (Battiston 2017) ----
owner_type_map <- c(
  "Corporate"                                        = "Industrial Company",
  "Mutual and pension fund, nominee, trust, trustee" = "Investment funds",
  "Mutual and pension fund/Nominee/Trust/Trustee"    = "Investment funds",
  "Bank"                                             = "Banks",
  "One or more named individuals or families"        = "Individuals",
  "Financial company"                                = "Other Financial Services",
  "Insurance company"                                = "Insurance and Pension funds",
  "Venture capital"                                  = "Investment funds",
  "Private equity firm"                              = "Investment funds",
  "Public authority, state, government"              = "Public authority, state, government",
  "Foundation, research Institute"                   = "Foundation/Research institute",
  "Foundation/Research institute"                    = "Foundation/Research institute",
  "Other unnamed shareholders, aggregated"           = "Unnamed private shareholders, aggregated",
  "Unnamed private shareholders, aggregated"         = "Unnamed private shareholders, aggregated",
  "Employees, managers, directors"                   = "Employees, managers, directors",
  "Public"                                           = "Individuals",
  "Hedge fund"                                       = "Investment funds"
)

ultimate_ownership <- ultimate_ownership %>%
  mutate(
    Owner_type = case_when(
      !is.na(Owner_NACE_code) & Owner_NACE_code != "-" & grepl("^641",                     Owner_NACE_code) ~ "Banks",
      !is.na(Owner_NACE_code) & Owner_NACE_code != "-" & grepl("^(642|643|6491|6612|663)", Owner_NACE_code) ~ "Investment funds",
      !is.na(Owner_NACE_code) & Owner_NACE_code != "-" & grepl("^(651|653|662)",           Owner_NACE_code) ~ "Insurance and Pension funds",
      !is.na(Owner_NACE_code) & Owner_NACE_code != "-" & grepl("^(6492|6499)",             Owner_NACE_code) ~ "Other Credit Institutions",
      !is.na(Owner_NACE_code) & Owner_NACE_code != "-" & grepl("^(6611|6619)",             Owner_NACE_code) ~ "Other Financial Services",
      !is.na(Owner_NACE_code) & Owner_NACE_code != "-"                                                      ~ "Industrial Company",
      Owner_type %in% names(owner_type_map)                                                                 ~ owner_type_map[Owner_type],
      TRUE ~ NA_character_
    )
  )

# ---- 5.2 Country: ISO -> name -> GLORIA region ----
Country_Code_to_Name <- readRDS("Data/Processed_Data/Orbis_cleaned.RDS")
Country_Code_to_Name <- unique(Country_Code_to_Name[, c("Country", "Country ISO code")])
Country_Code_to_Name_2 <- read_excel("Data/Raw_Data/Concordance_tables/ISO_Orbis_to_country_GLORIA.xlsx",
                                     sheet = "Country_ISO-GLORIA")

iso_to_name_gloria <- function(x) {
  primary  <- Country_Code_to_Name$Country[match(x, Country_Code_to_Name$`Country ISO code`)]
  fallback <- Country_Code_to_Name_2$Country_Gloria[match(x, Country_Code_to_Name_2$ISO_Orbis)]
  coalesce(primary, fallback)
}

ultimate_ownership$Owned_country <- iso_to_name_gloria(ultimate_ownership$Owned_country)
ultimate_ownership$Owner_country <- iso_to_name_gloria(ultimate_ownership$Owner_country)
company_division$country          <- iso_to_name_gloria(company_division$country)

Country_Orbis_Gloria <- read_excel("Data/Raw_Data/Concordance_tables/ISO_Orbis_to_country_GLORIA.xlsx")
Country_Gloria       <- read_excel("Data/Raw_Data/GLORIA_060/GLORIA_ReadMe_060.xlsx", sheet = "Regions")

to_gloria_region <- function(x) {
  in_gloria  <- x %in% Country_Gloria$Region_names
  m          <- match(x, Country_Orbis_Gloria$Orbis_country)
  ifelse(!is.na(in_gloria) & in_gloria, x,
         ifelse(!is.na(m), Country_Orbis_Gloria$GLORIA_match[m], NA))
}

ultimate_ownership$Owner_country <- to_gloria_region(ultimate_ownership$Owner_country)
ultimate_ownership$Owned_country <- to_gloria_region(ultimate_ownership$Owned_country)
company_division$country          <- to_gloria_region(company_division$country)

# ---- 5.3 Sector: NACE -> GLORIA ----
NACE_GLORIA <- read_excel("Data/Raw_Data/Concordance_tables/NACE_to_GLORIA_sectors.xlsx") %>%
  mutate(NACE.Code.and.Level = gsub("[^0-9]", "", NACE.Code.and.Level),
         NACE.class = trimws(NACE.class), GLORIA = trimws(GLORIA))

unmatched_solve <- read_excel("Data/Raw_Data/Concordance_tables/unmatched_NACE_to_GLORIA_sectors.xlsx") %>%
  mutate(Owned_NACE_description = trimws(Owned_NACE_description), GLORIA = trimws(GLORIA))

lookup_code   <- NACE_GLORIA %>% select(NACE.Code.and.Level, GLORIA) %>% distinct()
lookup_nace   <- NACE_GLORIA %>% select(NACE.class, GLORIA) %>% distinct()
lookup_gloria <- NACE_GLORIA %>% select(GLORIA) %>% distinct()

# Owned_sector in ultimate_ownership
ultimate_ownership <- ultimate_ownership %>%
  mutate(Owned_NACE_code = gsub("[^0-9]", "", Owned_NACE_code),
         Owned_NACE_description = trimws(Owned_NACE_description)) %>%
  left_join(lookup_code %>% rename(Owned_sector_code = GLORIA),
            by = c("Owned_NACE_code" = "NACE.Code.and.Level")) %>%
  left_join(lookup_nace %>% rename(Owned_sector_nace = GLORIA),
            by = c("Owned_NACE_description" = "NACE.class")) %>%
  left_join(lookup_gloria %>% transmute(Owned_NACE_description = GLORIA, GLORIA_from_gloria = GLORIA),
            by = "Owned_NACE_description") %>%
  mutate(Owned_sector = coalesce(Owned_sector_code, Owned_sector_nace, GLORIA_from_gloria)) %>%
  select(-Owned_sector_code, -Owned_sector_nace, -GLORIA_from_gloria)

idx_uo     <- match(ultimate_ownership$Owned_NACE_description, unmatched_solve$Owned_NACE_description)
to_fill_uo <- is.na(ultimate_ownership$Owned_sector) & !is.na(idx_uo)
ultimate_ownership$Owned_sector[to_fill_uo] <- unmatched_solve$GLORIA[idx_uo[to_fill_uo]]

# GLORIA sector in company_division
company_division <- company_division %>%
  mutate(sector_code = gsub("[^0-9]", "", sector_code), sector = trimws(sector)) %>%
  left_join(lookup_code %>% rename(GLORIA_from_code = GLORIA),
            by = c("sector_code" = "NACE.Code.and.Level")) %>%
  left_join(lookup_nace %>% rename(GLORIA_from_nace = GLORIA),
            by = c("sector" = "NACE.class")) %>%
  left_join(lookup_gloria %>% transmute(sector = GLORIA, GLORIA_from_gloria = GLORIA),
            by = "sector") %>%
  mutate(GLORIA = coalesce(GLORIA_from_code, GLORIA_from_nace, GLORIA_from_gloria)) %>%
  select(-GLORIA_from_code, -GLORIA_from_nace, -GLORIA_from_gloria)

idx_cd     <- match(company_division$sector, unmatched_solve$Owned_NACE_description)
to_fill_cd <- is.na(company_division$GLORIA) & !is.na(idx_cd)
company_division$GLORIA[to_fill_cd] <- unmatched_solve$GLORIA[idx_cd[to_fill_cd]]

company_division <- company_division %>% relocate(GLORIA, .after = sector_code)

# ---- 5.4 Append standalone firms (from ultimate ownership), as in the original ----
missing_names <- setdiff(ultimate_ownership$Owned_name, company_division$Company_name)
missing_rows  <- ultimate_ownership[ultimate_ownership$Owned_name %in% missing_names, ]

new_rows <- data.frame(
  Company_name = missing_rows$Owned_name,
  country      = missing_rows$Owned_country,
  sector_code  = missing_rows$Owned_NACE_code,
  GLORIA       = missing_rows$Owned_sector,
  sector       = NA,
  revenue      = missing_rows$Owned_turnover,
  share        = 1
)

company_division <- rbind(company_division, new_rows)

company_division <- company_division |>
  within({ sector <- NULL; sector_code <- NULL })
names(company_division)[names(company_division) == "GLORIA"] <- "sector"

company_division <- company_division %>% distinct()

saveRDS(company_division,   "Data/Processed_Data/Company_division_GLORIA.RDS")
saveRDS(ultimate_ownership, "Data/Processed_Data/Ultimate_ownership_GLORIA.RDS")

## ---------------------------------------------------------
## 6. MRIO revenue-allocation matrix W (W_IPF_50)
## ---------------------------------------------------------
# REVIEWERS: STEP 2 then STEP 3.
#  §6 builds the MRIO sourcing shares W_{h,c,s} = (Z + Y) share over producer
#     countries c for each (h,s) — eq. 1 — used as the IPF prior.
#  §7 runs the IPF/RAS reconciliation: row targets R_{h,s} (eq. 2), column
#     targets T_{c,s} from GLORIA gross output (eq. 3), initial matrix
#     A⁰ = W·R (eq. 4), alternating row/column scaling (eqs. 5–6) to
#     convergence, W^IPF = A / R (eq. 7), then top-50 countries per (h,s)
#     renormalised. Output W_IPF_50 is used in Parts III–VI.

rm(list = ls())
library(dplyr); library(data.table)

year <- 2024

Z       <- readRDS(paste0("Data/Processed_Data/Created_matrices/Z_",       year, ".rds"))
Y       <- readRDS(paste0("Data/Processed_Data/Created_matrices/Y_",       year, ".rds"))
label_Z <- readRDS(paste0("Data/Processed_Data/Created_matrices/label_Z_", year, ".rds"))

N <- nrow(Z)
stopifnot(ncol(Z) == N, nrow(label_Z) == N)

countries      <- label_Z[, 2]
sectors        <- label_Z[, 3]
country_factor <- factor(countries)
country_id     <- as.integer(country_factor)
country_labels <- levels(country_factor)
n_countries    <- length(country_labels)
sector_factor  <- factor(sectors)
sector_id      <- as.integer(sector_factor)
sector_labels  <- levels(sector_factor)
n_sectors      <- length(sector_labels)

Z_by_h <- t(rowsum(t(Z), group = country_id))

n_fd_cat       <- 6L
Y_use          <- Y[seq_len(N), , drop = FALSE]
n_countries_fd <- ncol(Y_use) / n_fd_cat
n_h            <- min(n_countries, n_countries_fd)

B <- matrix(0, nrow = ncol(Y_use), ncol = n_h)
for (h in seq_len(n_h)) {
  cols_h       <- ((h - 1L) * n_fd_cat + 1L):(h * n_fd_cat)
  B[cols_h, h] <- 1
}
Y_by_h <- Y_use %*% B

out_list <- vector("list", n_sectors)
for (s in seq_len(n_sectors)) {
  rows_s <- which(sector_id == s)
  if (!length(rows_s)) next
  int_s <- rowsum(Z_by_h[rows_s, , drop = FALSE], group = country_id[rows_s])
  fd_s  <- rowsum(Y_by_h[rows_s, , drop = FALSE], group = country_id[rows_s])
  W_s   <- int_s[, seq_len(n_h), drop = FALSE] + fd_s[, seq_len(n_h), drop = FALSE]
  dt <- as.data.table(as.table(W_s))
  setnames(dt, c("supplier_country_id", "consumer_country_id", "W_hsc"))
  dt[, supplier_country_id := as.integer(as.character(supplier_country_id))]
  dt[, consumer_country_id := as.integer(as.character(consumer_country_id))]
  dt <- dt[W_hsc > 0]
  dt[, s_id := s]
  out_list[[s]] <- dt
}

W_dt <- rbindlist(out_list, use.names = TRUE, fill = TRUE)
W_dt[, h := country_labels[consumer_country_id]]
W_dt[, s := sector_labels[s_id]]
W_dt[, c := country_labels[supplier_country_id]]
W_dt[, total_hs  := sum(W_hsc), by = .(h, s)]
W_dt[, weight_hs := W_hsc / total_hs]

W_df_all <- W_dt[, .(h, s, c, supplier_country_id, W_hsc, weight_hs)]
setorder(W_df_all, h, s, -weight_hs)
stopifnot(all(abs(W_df_all[, .(sw = sum(weight_hs)), by = .(h, s)]$sw - 1) < 1e-6))

x <- readRDS(paste0("Data/Processed_Data/Created_matrices/x_", year, ".rds"))
stopifnot(is.numeric(x), length(x) >= N)
x_use <- as.numeric(x[seq_len(N)])

company_divisions <- as.data.table(readRDS("Data/Processed_Data/Company_division_GLORIA.RDS"))

df_rev <- company_divisions[, .(R_hs = sum(revenue, na.rm = TRUE)), by = .(h = country, s = sector)][R_hs > 0]

df_x <- data.table(c = countries, s = sectors, x_node = x_use)[
  , .(x_cs = sum(x_node, na.rm = TRUE)), by = .(c, s)][x_cs > 0]

df_Rs      <- df_rev[, .(R_s = sum(R_hs)), by = s]
df_Xs      <- df_x[,   .(X_s = sum(x_cs)), by = s]
df_targets <- merge(df_x, df_Rs, by = "s", all.x = TRUE)
df_targets <- merge(df_targets, df_Xs, by = "s", all.x = TRUE)
df_targets[, T_cs := fifelse(!is.na(R_s) & X_s > 0, R_s * x_cs / X_s, NA_real_)]
df_targets <- df_targets[!is.na(T_cs) & T_cs > 0, .(c, s, T_cs)]

df_prior <- W_df_all[, .(w_hcs = sum(weight_hs)), by = .(h, s, c)]
df_prior[, p_prior := w_hcs / sum(w_hcs), by = .(h, s)]
df_prior <- df_prior[is.finite(p_prior) & p_prior > 0]
df_prior <- merge(df_prior, df_rev,     by = c("h", "s"), all = FALSE)
df_prior <- merge(df_prior, df_targets, by = c("c", "s"), all = FALSE)
df_prior[, A_init := p_prior * R_hs]

ipf_one_sector <- function(df_sector, max_iter = 500, tol = 1e-6) {
  hs <- unique(df_sector$h); cs <- unique(df_sector$c); s_cur <- unique(df_sector$s)
  row_targets <- df_sector[, .(R_hs = unique(R_hs)), by = h][match(hs, h), R_hs]
  col_targets <- df_sector[, .(T_cs = unique(T_cs)), by = c][match(cs, c), T_cs]
  A  <- matrix(0, nrow = length(hs), ncol = length(cs), dimnames = list(hs, cs))
  ih <- match(df_sector$h, hs); ic <- match(df_sector$c, cs)
  for (k in seq_len(nrow(df_sector))) A[ih[k], ic[k]] <- A[ih[k], ic[k]] + df_sector$A_init[k]
  converged <- FALSE; iter_done <- max_iter
  for (iter in seq_len(max_iter)) {
    col_f <- ifelse(colSums(A) > 0, col_targets / colSums(A), 1); col_f[!is.finite(col_f)] <- 1
    A <- sweep(A, 2, col_f, "*")
    row_f <- ifelse(rowSums(A) > 0, row_targets / rowSums(A), 1); row_f[!is.finite(row_f)] <- 1
    A <- sweep(A, 1, row_f, "*")
    if (max(abs(colSums(A) - col_targets), abs(rowSums(A) - row_targets), na.rm = TRUE) < tol) {
      converged <- TRUE; iter_done <- iter; break
    }
  }
  out <- as.data.table(as.table(A))
  setnames(out, c("h", "c", "W_hcs"))
  out[, `:=`(s = s_cur, iter = iter_done, converged = converged)]
  out[]
}

W_IPF_full <- rbindlist(lapply(unique(df_prior$s), function(s_cur) {
  df_sec <- df_prior[s == s_cur, .(h, c, s, A_init, R_hs, T_cs)]
  if (nrow(df_sec) == 0) return(NULL)
  ipf_one_sector(df_sec)
}), use.names = TRUE, fill = TRUE)[W_hcs > 0]

W_IPF_full[, total_hs  := sum(W_hcs), by = .(h, s)]
W_IPF_full <- W_IPF_full[total_hs > 0]
W_IPF_full[, weight_hs := W_hcs / total_hs]

W_IPF_50 <- W_IPF_full[order(-weight_hs), .SD[1:min(.N, 50L)], by = .(h, s)]
W_IPF_50[, weight_hs := weight_hs / sum(weight_hs), by = .(h, s)]
setorder(W_IPF_50, h, s, -weight_hs)

saveRDS(W_IPF_50, paste0("Data/Processed_Data/W_IPF_50_", year, ".rds"))
