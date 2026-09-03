## =========================================================
## Part V: Shareholder exposure to ecosystem services
## =========================================================
# Builds the ENCORE dependency matrix D, propagates dependencies through
# the supply chain to obtain per-ES dependency intensities by supplier
# country/region, then combines with exposure to produce dependency
# heatmaps and the top-20 node data (Fig 4b).
#
# ---------------------------------------------------------------------------
# REORGANISED OUTPUTS (design and results unchanged — only file naming).
# Figures  -> Results/Figures_final
# Data     -> Results/Data_Figures_final
#
#   Fig 5   (main) : Fig5_Heatmap_Dep_main.png       (absolute)
#   Fig 5   (HQ)   : Fig5_Heatmap_Dep_HQ.png         (absolute, appendix)
#   Fig S2  (main) : FigS2_Heatmap_Dep_rel_main.png  (relative)
#   Data           : Data_Fig4-5_main.xlsx , Data_Fig4-5_HQ.xlsx   (heatmap df)
#                    Data_Fig4b_main.xlsx  , Data_Fig4b_HQ.xlsx    (top nodes)
#
# main = exposure geographically reallocated via W_IPF_50
# HQ   = exposure kept at the investee headquarters (appendix)
#
#   D                                          binary material dependency
#   Ã        = L − diag(L)                     intermediate requirements
#   Ã_norm   = Ã column-normalised             share of intermediate input use
#   Total_(c,(h,s),e) = (Direct + Indirect) / max(1, Σ_c'(Direct + Indirect))
## =========================================================

#setwd("")
rm(list = ls()); gc()

library(readxl)
library(dplyr)
library(tidyr)
library(data.table)
library(Matrix)
library(ggplot2)
library(viridis)
library(openxlsx)

year <- 2024

# Reorganised output folders
fig_dir  <- "Results/Figures_final"
data_dir <- "Results/Data_Figures_final"
dir.create(fig_dir,  recursive = TRUE, showWarnings = FALSE)
dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)

## ---------------------------------------------------------
## 1. Build dependency matrix D  (ENCORE)
## ---------------------------------------------------------
# ENCORE rates each sector's dependency on each ecosystem service on a
# 0–5 scale. We keep only *material* dependencies (scores 4–5 → 1, else 0)
# and collapse to one row per GLORIA sector (max across sub-categories).

ENC <- read_excel("Data/Raw_Data/Concordance_tables/GLORIA_NACE_ENCORE_059a.xlsx")

# Keep the 14 retained ES explicitly (by name, not by column position): the
# ENCORE sheet holds 25 ES and a positional drop leaves several excluded ones
# in place, which then propagate as spurious dep*.rds downstream.
es_keep_cols <- c(
  "Air filtration", "Biological control", "Biomass provisioning", "Flood control",
  "Local climate regulation", "Nursery population and habitat maintenance", "Pollination",
  "Rainfall pattern regulation", "Soil and sediment retention", "Soil quality regulation",
  "Solid waste remediation", "Storm mitigation", "Water flow regulation", "Water purification"
)
ENC <- ENC %>% select(gloria_sec, all_of(es_keep_cols))

ENC <- ENC %>%
  group_by(gloria_sec) %>%
  summarise(across(everything(), ~ suppressWarnings(max(.x, na.rm = TRUE))),
            .groups = "drop")

# Recode qualitative scores to binary material dependency (4–5 = material)
ENC <- ENC %>%
  mutate(across(-gloria_sec, ~ case_when(
    .x %in% c("N/A", "ND") ~ 0,
    .x %in% c("0", "1", "2", "3", 0, 1, 2, 3) ~ 0,
    .x %in% c("4", "5", 4, 5)                 ~ 1,
    TRUE ~ NA_real_
  )))

# D: ES (rows) × GLORIA sector (cols), binary
D <- as.matrix(ENC[, -1])
rownames(D) <- ENC$gloria_sec
D <- t(D)                                  # ES × sector
saveRDS(D, paste0("Data/Processed_Data/Created_matrices/D_", year, ".rds"))

## ---------------------------------------------------------
## 2. Dependency intensities by supplier country  (D → dep*.rds)
## ---------------------------------------------------------

L       <- readRDS(paste0("Data/Processed_Data/Created_matrices/L_",       year, ".rds"))
label_Z <- readRDS(paste0("Data/Processed_Data/Created_matrices/label_Z_", year, ".rds"))

countries <- factor(label_Z[, 2])
sectors   <- as.character(label_Z[, 3])
n         <- ncol(L)

# Map each node to its sector's row in D (sector × ES)
D_sec    <- t(D)                           # sector × ES
sec_idx  <- match(sectors, rownames(D_sec))
es_names <- colnames(D_sec)

# Ã: intermediate requirements excluding own-sector, column-normalised
A_tilde <- L
diag(A_tilde) <- 0                         # remove own-sector production
col_tot <- colSums(A_tilde)
col_tot[col_tot == 0] <- 1                 # guard against empty columns
A_norm  <- sweep(A_tilde, 2, col_tot, "/") # share of intermediate input use
rm(A_tilde); gc()

# Supplier-country one-hot; t(G_sup)[c, j] = 1 iff node j is in country c
G_sup <- sparse.model.matrix(~ 0 + countries)
colnames(G_sup) <- levels(countries)
tG    <- t(G_sup)

region_lookup <- read.xlsx("Data/Raw_Data/Concordance_tables/country_to_region.xlsx")
region <- setNames(region_lookup$World_region, region_lookup$Country)

dir.create("Data/Processed_Data/dependency intensities",
           recursive = TRUE, showWarnings = FALSE)

# Remove dep*.rds from earlier runs: list.files() downstream reads whatever is
# in the folder, so stale files (e.g. ES no longer retained) would resurface.
old_dep <- list.files("Data/Processed_Data/dependency intensities",
                      pattern = "^dep.*\\.rds$", full.names = TRUE)
if (length(old_dep)) file.remove(old_dep)

for (e in seq_along(es_names)) {

  d_node <- D_sec[sec_idx, es_names[e]]    # length n, 0/1 per node
  if (all(d_node == 0, na.rm = TRUE)) next

  # Indirect: ES-dependent upstream shares aggregated by supplier country
  indirect <- tG %*% sweep(A_norm, 1, d_node, `*`)        # R × n

  # Direct: domestic only — node's own dependency attributed to its country
  direct   <- sweep(as.matrix(tG), 2, d_node, `*`)        # R × n

  # Total dependency (eq. 19): (direct + indirect) normalised across supplier
  # countries by max(1, Σ_c'(direct + indirect)).
  total_raw <- as.matrix(direct) + as.matrix(indirect)     # R × n
  denum     <- pmax(1, colSums(total_raw))                 # Σ_c' per node, floored at 1
  total     <- sweep(total_raw, 2, denum, "/")             # bounded [0,1]

  # Long form for BOTH the direct and the total component (Part VI needs both)
  to_long <- function(mat, valname) {
    dt <- as.data.table(as.matrix(mat))
    setnames(dt, paste0("n", seq_len(n)))
    dt[, supplier_country := colnames(G_sup)]
    dt <- melt(dt, id.vars = "supplier_country",
               variable.name = "consumer_node", value.name = valname)
    dt[, consumer_idx := as.integer(sub("n", "", consumer_node))]
    dt[, consumer_node := NULL]
    dt
  }

  dep_t <- to_long(total,             "total_dependency")
  dep_d <- to_long(as.matrix(direct), "direct_dependency")

  dep <- merge(dep_t, dep_d, by = c("supplier_country", "consumer_idx"))
  dep[, consumer_country := as.character(countries)[consumer_idx]]
  dep[, consumer_sector  := sectors[consumer_idx]]
  dep[, consumer_idx := NULL]
  dep <- dep[total_dependency > 0 | direct_dependency > 0]
  dep[, supplier_region := region[supplier_country]]

  saveRDS(dep, sprintf("Data/Processed_Data/dependency intensities/dep%s.rds",
                       es_names[e]))
  rm(indirect, direct, total, total_raw, dep, dep_t, dep_d); gc()
}

rm(A_norm, G_sup, tG, L); gc()

## ---------------------------------------------------------
## 3. Load exposure (main = W-allocated, HQ = headquarters) + dep tables
## ---------------------------------------------------------

# Main: geographically reallocated exposure (from Part IV)
expo_main <- readRDS("Data/Processed_Data/Key_Data_Frames/Exposure_final_long.rds")

# HQ: exposure kept at the investee headquarters, built from Exposure_Data
Exposure_Data <- readRDS("Data/Processed_Data/Exposure_Data.RDS")
expo_HQ <- Exposure_Data %>%
  group_by(Owner_country, Owner_region, allocated_country = country, sector) %>%
  summarise(
    Total_exposure_alloc    = sum(Total_exposure,    na.rm = TRUE),
    Direct_exposure_alloc   = sum(Direct_exposure,   na.rm = TRUE),
    Indirect_exposure_alloc = sum(Indirect_exposure, na.rm = TRUE),
    .groups = "drop"
  )
rm(Exposure_Data)

dir_path  <- "Data/Processed_Data/dependency intensities"
dep_files <- list.files(dir_path, pattern = "^dep.*\\.rds$", full.names = TRUE)
es_list   <- setNames(lapply(dep_files, readRDS),
                      gsub("\\.rds$", "", basename(dep_files)))

## ---------------------------------------------------------
## 4. Label maps (region and ecosystem-service abbreviations)
## ---------------------------------------------------------

region_map <- c(
  "China"                                  = "China",
  "Eastern Europe"                         = "East. Eur.",
  "Middle East"                            = "Mid. East",
  "Far East and Central Asia - Exc. China" = "FECA",
  "North America"                          = "N. Am.",
  "Africa"                                 = "Africa",
  "Oceania"                                = "Oceania",
  "Western Europe"                         = "W. Eur.",
  "South and Central America"              = "SCA"
)

es_map <- c(
  "depAir filtration"                             = "Air filt.",
  "depBiological control"                         = "Biological cont.",
  "depBiomass provisioning"                       = "Biomass prov.",
  "depFlood control"                              = "Flood ctrl.",
  "depLocal climate regulation"                   = "Local clim.",
  "depNursery population and habitat maintenance" = "Nursery & habitat",
  "depPollination"                                = "Pollination",
  "depRainfall pattern regulation"                = "Rainfall reg.",
  "depSoil and sediment retention"                = "Soil & sed. ret.",
  "depSoil quality regulation"                    = "Soil quality",
  "depSolid waste remediation"                    = "Solid waste rem.",
  "depStorm mitigation"                           = "Storm mitig.",
  "depWater flow regulation"                      = "Water flow",
  "depWater purification"                         = "Water purif."
)

# Shared, enlarged heatmap theme
hm_theme <- theme_minimal(base_size = 20) +
  theme(panel.grid   = element_blank(),
        axis.text.x  = element_text(angle = 45, hjust = 1, size = 16),
        axis.text.y  = element_text(size = 16),
        axis.title   = element_text(size = 22),
        strip.text   = element_text(face = "bold", size = 18),
        legend.title = element_text(size = 18),
        legend.text  = element_text(size = 16))

## ---------------------------------------------------------
## 5. Heatmaps per allocation (main + HQ)
## ---------------------------------------------------------
# Fig 5  = absolute heatmap (produced for main and HQ)
# Fig S2 = relative heatmap (produced for main only)
# `tag` = "main" or "HQ". `save_rel_fig` controls the Fig S2 output.

make_heatmaps <- function(Exposure, tag, save_rel_fig) {

  Exposure_Total <- Exposure %>%
    select(-Direct_exposure_alloc, -Indirect_exposure_alloc) %>%
    group_by(across(-Total_exposure_alloc)) %>%
    summarise(Exposure_kUSD = sum(Total_exposure_alloc, na.rm = TRUE), .groups = "drop")

  # Keep only the ES listed in es_map and validate columns
  need_cols <- c("consumer_country", "consumer_sector", "supplier_region", "total_dependency")
  es_keep   <- es_list[intersect(names(es_map), names(es_list))]
  es_keep   <- es_keep[vapply(es_keep, function(x) all(need_cols %in% names(x)), logical(1))]

  Exposure_Total_dt <- as.data.table(Exposure_Total)

  # Edges: exposure × dependency, aggregated to supplier-region (memory-safe)
  edges_all <- rbindlist(lapply(names(es_keep), function(es_nm) {
    dep_df <- as.data.table(es_keep[[es_nm]])[
      , .(total_dependency = sum(total_dependency, na.rm = TRUE)),
      by = .(consumer_country, consumer_sector, supplier_region)
    ]
    Exposure_Total_dt[
      dep_df,
      on = c(allocated_country = "consumer_country", sector = "consumer_sector"),
      nomatch = 0, allow.cartesian = TRUE
    ][
      , .(Exposure_kUSD = sum(Exposure_kUSD, na.rm = TRUE),
          Dep_total     = sum(Exposure_kUSD * total_dependency, na.rm = TRUE)),
      by = .(Owner_country, Owner_region, allocated_country, sector, supplier_region)
    ][, es := es_nm][]
  }))

  # ---- Relative heatmap (Fig S2) ----
  Exp_owner_region <- Exposure_Total %>%
    group_by(Owner_region) %>%
    summarise(Exp_sum = sum(Exposure_kUSD, na.rm = TRUE), .groups = "drop")

  df_hm_rel <- edges_all %>%
    group_by(Owner_region, supplier_region, es) %>%
    summarise(Dep_sum = sum(Dep_total, na.rm = TRUE), .groups = "drop") %>%
    left_join(Exp_owner_region, by = "Owner_region") %>%
    mutate(
      Dep_rel              = Dep_sum / Exp_sum,
      Owner_region_abbr    = region_map[as.character(Owner_region)],
      supplier_region_abbr = region_map[as.character(supplier_region)],
      es_abbr              = es_map[as.character(es)]
    ) %>%
    filter(!is.na(Owner_region_abbr), !is.na(supplier_region_abbr), !is.na(es_abbr))

  es_order_rel <- df_hm_rel %>%
    group_by(es_abbr) %>% summarise(total = sum(Dep_sum, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(total)) %>% pull(es_abbr)
  region_order_rel <- df_hm_rel %>%
    group_by(Owner_region_abbr) %>% summarise(total = sum(Dep_sum, na.rm = TRUE), .groups = "drop") %>%
    arrange(total) %>% pull(Owner_region_abbr)

  df_hm_rel <- df_hm_rel %>%
    mutate(
      es_abbr              = factor(es_abbr, levels = es_order_rel),
      Owner_region_abbr    = factor(Owner_region_abbr, levels = region_order_rel),
      supplier_region_abbr = factor(supplier_region_abbr, levels = region_order_rel)
    )

  p_rel <- ggplot(df_hm_rel, aes(supplier_region_abbr, Owner_region_abbr, fill = Dep_rel)) +
    geom_tile() +
    facet_wrap(~ es_abbr, ncol = 5) +
    coord_equal() +
    scale_x_discrete(limits = rev(levels(df_hm_rel$supplier_region_abbr))) +
    scale_fill_viridis_c(option = "turbo", direction = 1,
                         limits = c(min(df_hm_rel$Dep_rel, na.rm = TRUE),
                                    max(df_hm_rel$Dep_rel, na.rm = TRUE)),
                         oob = scales::squish) +
    hm_theme +
    labs(x = "Ecosystem region", y = "Shareholder region", fill = "Exposure\n(% portfolio)")

  if (save_rel_fig) {
    ggsave(file.path(fig_dir, "FigS2_Heatmap_Dep_rel_main.png"),
           p_rel, width = 16, height = 12, dpi = 300)
  }
  # Heatmap data (absolute Dep_sum + relative Dep_rel) for both allocations
  write.xlsx(df_hm_rel, file.path(data_dir, sprintf("Data_Fig4-5_%s.xlsx", tag)),
             overwrite = TRUE)

  # ---- Absolute heatmap (Fig 5) — same ordering as the relative one ----
  df_hm_abs <- edges_all %>%
    group_by(Owner_region, supplier_region, es) %>%
    summarise(Dep_total = sum(Dep_total, na.rm = TRUE), .groups = "drop") %>%
    mutate(
      Owner_region_abbr    = region_map[as.character(Owner_region)],
      supplier_region_abbr = region_map[as.character(supplier_region)],
      es_abbr              = es_map[as.character(es)]
    ) %>%
    filter(!is.na(Owner_region_abbr), !is.na(supplier_region_abbr), !is.na(es_abbr)) %>%
    mutate(
      es_abbr              = factor(es_abbr, levels = levels(df_hm_rel$es_abbr)),
      Owner_region_abbr    = factor(Owner_region_abbr, levels = levels(df_hm_rel$Owner_region_abbr)),
      supplier_region_abbr = factor(supplier_region_abbr, levels = levels(df_hm_rel$supplier_region_abbr))
    )

  p_abs <- ggplot(df_hm_abs, aes(supplier_region_abbr, Owner_region_abbr, fill = Dep_total)) +
    geom_tile() +
    facet_wrap(~ es_abbr, ncol = 5) +
    coord_equal() +
    scale_x_discrete(limits = rev(levels(df_hm_abs$supplier_region_abbr))) +
    scale_fill_viridis_c(option = "turbo", trans = "log10", direction = 1,
                         limits = c(min(df_hm_abs$Dep_total, na.rm = TRUE),
                                    max(df_hm_abs$Dep_total, na.rm = TRUE)),
                         oob = scales::squish) +
    hm_theme +
    labs(x = "Ecosystem region", y = "Shareholder region", fill = "Exposure (USDk)")

  ggsave(file.path(fig_dir, sprintf("Fig5_Heatmap_Dep_%s.png", tag)),
         p_abs, width = 16, height = 12, dpi = 300)

  invisible(Exposure_Total)
}

Exposure_Total_main <- make_heatmaps(expo_main, "main", save_rel_fig = TRUE)   # Fig 5 + Fig S2
Exposure_Total_HQ   <- make_heatmaps(expo_HQ,   "HQ",   save_rel_fig = FALSE)  # Fig 5 HQ

## ---------------------------------------------------------
## 6. Top-20 ES–country nodes (Fig 4b data) — main and HQ
## ---------------------------------------------------------
# Dep_total[owner_country, supplier_country] = Σ_node exposure × dependency.
# Sparse matrix product: the node summation happens inside the multiply,
# so the cartesian intermediate (≈179M rows) is never materialized.

make_fig4b <- function(Exposure_Total, tag) {

  Exposure_Total_dt <- as.data.table(Exposure_Total)

  # ---- 6.1 Exposure as a sparse matrix: owner country × node ----------
  Exp_oc <- Exposure_Total_dt[
    !is.na(Owner_country) & !is.na(allocated_country) & !is.na(sector),
    .(Exposure_kUSD = sum(Exposure_kUSD, na.rm = TRUE)),
    by = .(Owner_country, allocated_country, sector)
  ]
  Exp_oc[, node := paste(allocated_country, sector, sep = "||")]

  dep_nodes <- unique(unlist(lapply(es_list, function(d) {
    d <- d[!is.na(d$consumer_country) & !is.na(d$consumer_sector), ]
    paste(d$consumer_country, d$consumer_sector, sep = "||")
  })))
  node_lvls <- unique(c(Exp_oc$node, dep_nodes))
  node_idx  <- setNames(seq_along(node_lvls), node_lvls)

  oc_lvls <- unique(Exp_oc$Owner_country)
  oc_idx  <- setNames(seq_along(oc_lvls), oc_lvls)

  Exp_mat <- sparseMatrix(
    i    = oc_idx[Exp_oc$Owner_country],
    j    = node_idx[Exp_oc$node],
    x    = Exp_oc$Exposure_kUSD,
    dims = c(length(oc_lvls), length(node_lvls))
  )

  # ---- 6.2 Sparse product per ES -------------------------------------
  # Restrict to the ES listed in es_map (as the heatmaps do), so a stale or
  # unexpected dep*.rds cannot produce rows with es = NA.
  es_keep_names <- intersect(names(es_map), names(es_list))

  edges_all_node <- rbindlist(lapply(es_keep_names, function(es_nm) {

    d <- as.data.table(es_list[[es_nm]])
    d <- d[!is.na(supplier_country) & !is.na(consumer_country) & !is.na(consumer_sector)]
    d[, node := paste(consumer_country, consumer_sector, sep = "||")]

    sc_lvls <- unique(d$supplier_country)
    sc_idx  <- setNames(seq_along(sc_lvls), sc_lvls)

    Dep_mat <- sparseMatrix(
      i    = sc_idx[d$supplier_country],
      j    = node_idx[d$node],
      x    = d$total_dependency,
      dims = c(length(sc_lvls), length(node_lvls))
    )

    R <- as(Exp_mat %*% t(Dep_mat), "TsparseMatrix")   # owner_country × supplier_country
    data.table(
      Owner_country    = oc_lvls[R@i + 1L],
      supplier_country = sc_lvls[R@j + 1L],
      Dep_total        = R@x,
      es               = es_map[es_nm]
    )
  }))

  # ---- 6.3 Final shape and save --------------------------------------
  edges_all_node <- edges_all_node %>%
    mutate(supplier_country_es = paste(supplier_country, es, sep = " - ")) %>%
    group_by(across(-where(is.numeric))) %>%
    summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)), .groups = "drop")

  write.xlsx(edges_all_node, file.path(data_dir, sprintf("Data_Fig4b_%s.xlsx", tag)),
             overwrite = TRUE)
}

make_fig4b(Exposure_Total_main, "main")   # W allocation
make_fig4b(Exposure_Total_HQ,   "HQ")     # headquarter allocation (appendix)
