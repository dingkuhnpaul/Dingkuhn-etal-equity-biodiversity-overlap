## =========================================================
## Part VI: Geographic overlap between biodiversity impact
##          and ecosystem-service dependency
## =========================================================
# Variants : main (W allocation, PDF, 14 ES), HQ (headquarter allocation),
#            core (W, PDF, 8-ES robustness subset).
#
# ---------------------------------------------------------------------------
# REORGANISED OUTPUTS (design and results unchanged — only file naming and the
# fact that the two overlap maps are now saved as SEPARATE files 6c / 6d).
# Figures  -> Results/Figures_final
# Data     -> Results/Data_Figures_final
#
#   Fig 6a-b (main) : Fig6ab_DotPanel_main.png     (shareholder dot panel: a=direct, b=total)
#   Fig 6c   (main) : Fig6c_Map_Direct_main.png    (country map, direct overlap)
#   Fig 6d   (main) : Fig6d_Map_Total_main.png     (country map, total overlap)
#   Fig 6a-b (HQ)   : Fig6ab_DotPanel_HQ.png       + Fig6c_Map_Direct_HQ.png / Fig6d_Map_Total_HQ.png
#   Fig 6a-b (core) : Fig6ab_DotPanel_core.png     + Fig6c_Map_Direct_core.png / Fig6d_Map_Total_core.png
#   Fig S3a-b       : FigS3ab_Country_DotPanel_main.png   (country dot panel)
#
#   Data            : Data_Fig6_main.xlsx , Data_Fig6_HQ.xlsx , Data_Fig6_core.xlsx
#                     Data_FigS3_country_main.xlsx
## =========================================================

#setwd("")
rm(list = ls()); gc()

library(dplyr); library(tidyr); library(data.table); library(Matrix)
library(ggplot2); library(scales); library(openxlsx); library(patchwork)
library(sf); library(rnaturalearth); library(readxl); library(stringr)

year <- 2024

# Reorganised output folders
fig_dir  <- "Results/Figures_final"
data_dir <- "Results/Data_Figures_final"
dir.create(fig_dir,  recursive = TRUE, showWarnings = FALSE)
dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)

## ---- shared aesthetics ----------------------------------------------
region_colors <- c(
  "China"                                  = "#C75C5C",
  "Eastern Europe"                         = "#A6C0D4",
  "Middle East"                            = "#F1D08C",
  "Far East and Central Asia - Exc. China" = "#EFA5AD",
  "North America"                          = "#8FB1EC",
  "Africa"                                 = "#E2B36D",
  "Oceania"                                = "#9BE4DB",
  "Western Europe"                         = "#C7B1E0",
  "South and Central America"              = "#A7D7A9"
)

# Overlap map / dot colours (direct = red, total = blue), used throughout
col_direct <- "#e31a1c"; col_direct_low <- "#fde0df"
col_total  <- "#1f78b4"; col_total_low  <- "#dbe7f3"
map_leg    <- "Share of impact overlapping\nportfolio dependency"

es_core <- c(
  "depPollination", "depBiological control",
  "depNursery population and habitat maintenance",
  "depSoil quality regulation", "depSoil and sediment retention",
  "depBiomass provisioning", "depLocal climate regulation",
  "depStorm mitigation"
)

## ---------------------------------------------------------
## 0. Load inputs + build impact intensities (PDF)
## ---------------------------------------------------------

W_long        <- readRDS(paste0("Data/Processed_Data/W_IPF_50_", year, ".rds"))
Exposure_Data <- readRDS("Data/Processed_Data/Exposure_Data.RDS")
Direct_LU     <- readRDS(paste0("Data/Processed_Data/Key_Data_Frames/Impact_Direct_Long_", year, ".rds"))
LU_total      <- readRDS(paste0("Data/Processed_Data/Key_Data_Frames/LU_impact_Total_Long_", year, ".rds"))

# ---- Impact tables (PDF), standard 'val' column -----------------------
direct_tbl <- as.data.frame(as.data.table(Direct_LU)[
  , .(val = sum(PDF_yr_kUSD, na.rm = TRUE)), by = .(consumer_country, consumer_sector)])
total_small <- as.data.table(LU_total)[
  , .(val = sum(PDF_yr_kUSD, na.rm = TRUE)),
  by = .(consumer_country, consumer_sector, supplier_country)]
setkey(total_small, consumer_country, consumer_sector)
rm(Direct_LU, LU_total); gc()

## ---------------------------------------------------------
## 1. Clean sample, then top-N investors + region of origin
## ---------------------------------------------------------
# Drop unusable / zero nodes and keep only known-region investors BEFORE
# ranking, so W and HQ share the same sample and the top-1000 is drawn from
# usable, known-region shareholders (removes the artificial exposure floor).

Exposure_Data <- Exposure_Data %>%
  filter(!is.na(country), !is.na(sector), !is.na(Owner_region), Total_exposure > 0)

n_top <- 1000

# One region per investor: a given Owner_name can appear under several
# Owner_country (hence Owner_region) in the exposure table, so a plain
# distinct(Owner_name, Owner_region) would duplicate investors when joined
# back to DF_overlap. Keep the region where the investor's exposure is largest.
owner_region <- Exposure_Data %>%
  group_by(Owner_name, Owner_region) %>%
  summarise(e = sum(Total_exposure, na.rm = TRUE), .groups = "drop_last") %>%
  slice_max(e, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(Owner_name, Owner_region)

top_owners <- Exposure_Data %>%
  group_by(Owner_name) %>%
  summarise(total_exp = sum(Total_exposure, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(total_exp)) %>%
  slice_head(n = n_top) %>%
  pull(Owner_name)

Exposure_topN <- Exposure_Data %>% filter(Owner_name %in% top_owners)
rm(Exposure_Data); gc()

# GLORIA country -> world region (for the country dot panel)
Table_Country_Region <- read.xlsx("Data/Raw_Data/Concordance_tables/country_to_region.xlsx") %>%
  transmute(allocated_country = as.character(Country), World_region = as.character(World_region))

## ---------------------------------------------------------
## 2. Allocation-dependent share objects (PDF impact)
## ---------------------------------------------------------

build_shares <- function(alloc = c("W", "HQ")) {
  alloc <- match.arg(alloc)

  Exposure_alloc <- if (alloc == "W") {
    Exposure_topN %>%
      left_join(W_long, by = c("country" = "h", "sector" = "s")) %>%
      transmute(Ultimate_shareholder = Owner_name, sector,
                allocated_country = c, Exposure_kUSD = Total_exposure * weight_hs)
  } else {
    Exposure_topN %>%
      transmute(Ultimate_shareholder = Owner_name, sector,
                allocated_country = country, Exposure_kUSD = Total_exposure)
  }
  Exposure_alloc <- Exposure_alloc %>%
    group_by(Ultimate_shareholder, sector, allocated_country) %>%
    summarise(Exposure_kUSD = sum(Exposure_kUSD, na.rm = TRUE), .groups = "drop")

  Exposure_totals_owner <- Exposure_alloc %>%
    group_by(Ultimate_shareholder) %>%
    summarise(total_Exposure_kUSD = sum(Exposure_kUSD, na.rm = TRUE), .groups = "drop")

  # Country economic size (for the country dot panel)
  country_exposure <- Exposure_alloc %>%
    group_by(allocated_country) %>%
    summarise(total_Exposure_kUSD = sum(Exposure_kUSD, na.rm = TRUE), .groups = "drop")

  Exposure_alloc_dt <- as.data.table(Exposure_alloc)
  setkey(Exposure_alloc_dt, allocated_country, sector)

  owners        <- unique(Exposure_alloc_dt$Ultimate_shareholder)
  owner_batches <- split(owners, ceiling(seq_along(owners) / 100))

  # p_ic direct: impact by allocated (production) country
  PDF_direct_ic <- Exposure_alloc %>%
    inner_join(direct_tbl, by = c("allocated_country" = "consumer_country",
                                  "sector"            = "consumer_sector")) %>%
    mutate(PDF_contrib = Exposure_kUSD * val) %>%
    group_by(Ultimate_shareholder, allocated_country) %>%
    summarise(PDF_direct = sum(PDF_contrib, na.rm = TRUE), .groups = "drop") %>%
    group_by(Ultimate_shareholder) %>%
    mutate(PDF_direct_share = PDF_direct / sum(PDF_direct, na.rm = TRUE)) %>%
    ungroup()

  # p_ic total: impact by supplier country (chunked)
  PDF_total_ic <- rbindlist(lapply(owner_batches, function(b) {
    xb <- Exposure_alloc_dt[Ultimate_shareholder %in% b,
                            .(Ultimate_shareholder, allocated_country, sector, Exposure_kUSD)]
    j <- merge(xb, total_small, by.x = c("allocated_country", "sector"),
               by.y = c("consumer_country", "consumer_sector"), allow.cartesian = TRUE)
    j[, PDF_contrib := Exposure_kUSD * val]
    j[, .(PDF_total = sum(PDF_contrib, na.rm = TRUE)),
      by = .(Ultimate_shareholder, supplier_country)]
  }))[, .(PDF_total = sum(PDF_total, na.rm = TRUE)),
      by = .(Ultimate_shareholder, supplier_country)]
  PDF_total_ic[, PDF_total_share := PDF_total / sum(PDF_total, na.rm = TRUE),
               by = Ultimate_shareholder]

  PDF_ic <- full_join(PDF_direct_ic, as.data.frame(PDF_total_ic),
                      by = c("Ultimate_shareholder", "allocated_country" = "supplier_country")) %>%
    mutate(PDF_direct = coalesce(PDF_direct, 0), PDF_total = coalesce(PDF_total, 0))

  Exposure_dep_dt <- Exposure_alloc_dt[, .(Ultimate_shareholder,
                                           consumer_country = allocated_country, consumer_sector = sector, Exposure_kUSD)]
  setkey(Exposure_dep_dt, consumer_country, consumer_sector)

  list(PDF_ic = PDF_ic, Exposure_dep_dt = Exposure_dep_dt,
       Exposure_totals_owner = Exposure_totals_owner, owner_batches = owner_batches,
       country_exposure = country_exposure)
}

## ---------------------------------------------------------
## 3. Overlap pipeline (shares + ES set)
## ---------------------------------------------------------

compute_overlap <- function(shares, es_keep = NULL) {

  PDF_ic                <- shares$PDF_ic
  Exposure_dep_dt       <- shares$Exposure_dep_dt
  Exposure_totals_owner <- shares$Exposure_totals_owner
  owner_batches         <- shares$owner_batches

  dep_files <- list.files("Data/Processed_Data/dependency intensities",
                          pattern = "^dep.*\\.rds$", full.names = TRUE)
  if (!is.null(es_keep)) {
    dep_files <- dep_files[gsub("\\.rds$", "", basename(dep_files)) %in% es_keep]
  }

  dep_list <- lapply(dep_files, function(f) {
    d <- as.data.table(readRDS(f))
    if (!all(c("consumer_country","consumer_sector","supplier_country",
               "total_dependency","direct_dependency") %in% names(d))) return(NULL)
    d <- d[, .(total_dependency  = sum(total_dependency,  na.rm = TRUE),
               direct_dependency = sum(direct_dependency, na.rm = TRUE)),
           by = .(consumer_country, consumer_sector, supplier_country)]
    setkey(d, consumer_country, consumer_sector); d
  })
  dep_list <- dep_list[!vapply(dep_list, is.null, logical(1))]
  if (length(dep_list) == 0)
    stop("No dependency-intensity tables found with both total_dependency and ",
         "direct_dependency — run Part V first.")

  Owner_ES_ic <- rbindlist(lapply(owner_batches, function(b) {
    xb <- Exposure_dep_dt[Ultimate_shareholder %in% b]
    setkey(xb, consumer_country, consumer_sector)
    rbindlist(lapply(dep_list, function(dep_df) {
      j <- merge(xb, dep_df, by = c("consumer_country","consumer_sector"), allow.cartesian = TRUE)
      j[, .(DEP_total_kUSD  = sum(Exposure_kUSD * total_dependency,  na.rm = TRUE),
            DEP_direct_kUSD = sum(Exposure_kUSD * direct_dependency, na.rm = TRUE)),
        by = .(Ultimate_shareholder, supplier_country)]
    }))[, .(DEP_total_kUSD  = sum(DEP_total_kUSD,  na.rm = TRUE),
            DEP_direct_kUSD = sum(DEP_direct_kUSD, na.rm = TRUE)),
        by = .(Ultimate_shareholder, supplier_country)]
  }))[, .(DEP_total_kUSD  = sum(DEP_total_kUSD,  na.rm = TRUE),
          DEP_direct_kUSD = sum(DEP_direct_kUSD, na.rm = TRUE)),
      by = .(Ultimate_shareholder, supplier_country)]
  Owner_ES_ic[, `:=`(
    DEP_total_share  = DEP_total_kUSD  / sum(DEP_total_kUSD,  na.rm = TRUE),
    DEP_direct_share = DEP_direct_kUSD / sum(DEP_direct_kUSD, na.rm = TRUE)
  ), by = Ultimate_shareholder]

  pdf_shares <- PDF_ic %>%
    transmute(Ultimate_shareholder, country = allocated_country,
              p_direct = PDF_direct_share, p_total = PDF_total_share, PDF_direct, PDF_total)
  dep_shares <- as.data.frame(Owner_ES_ic) %>%
    transmute(Ultimate_shareholder, country = supplier_country,
              q_direct = DEP_direct_share, q_total = DEP_total_share)

  shares_merged <- full_join(pdf_shares, dep_shares,
                             by = c("Ultimate_shareholder", "country")) %>%
    mutate(PDF_direct = coalesce(PDF_direct, 0), PDF_total = coalesce(PDF_total, 0))

  DF_overlap <- shares_merged %>%
    group_by(Ultimate_shareholder) %>%
    summarise(
      total_PDF_direct = sum(PDF_direct, na.rm = TRUE),
      total_PDF_total  = sum(PDF_total,  na.rm = TRUE),
      overlap_direct = if (all(is.na(p_direct)) || all(is.na(q_direct))) NA_real_
      else sum(pmin(p_direct, q_direct), na.rm = TRUE),
      overlap_total  = if (all(is.na(p_total))  || all(is.na(q_total)))  NA_real_
      else sum(pmin(p_total, q_total),   na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(Exposure_totals_owner, by = "Ultimate_shareholder") %>%
    left_join(owner_region, by = c("Ultimate_shareholder" = "Owner_name"))

  # Country-level index (eq. 27): share of the impact located in a country that
  # is matched by a dependency of the portfolios to which it is allocated.
  country_direct_pdf <- shares_merged %>%
    group_by(Ultimate_shareholder) %>%
    mutate(FBF_i = sum(PDF_direct, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(matched = pmin(p_direct, q_direct) * FBF_i) %>%
    group_by(country) %>%
    summarise(value = sum(matched, na.rm = TRUE) /
                sum(PDF_direct, na.rm = TRUE), .groups = "drop") %>%
    rename(allocated_country = country)
  country_total_pdf <- shares_merged %>%
    group_by(Ultimate_shareholder) %>%
    mutate(FBF_i = sum(PDF_total, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(matched = pmin(p_total, q_total) * FBF_i) %>%
    group_by(country) %>%
    summarise(value = sum(matched, na.rm = TRUE) /
                sum(PDF_total, na.rm = TRUE), .groups = "drop") %>%
    rename(allocated_country = country)

  list(DF_overlap = DF_overlap,
       country_direct_pdf = country_direct_pdf,
       country_total_pdf  = country_total_pdf)
}

## ---------------------------------------------------------
## 4. Figure helpers
## ---------------------------------------------------------

# ---- 4.1 Fig 6a-b: shareholder dot panel (a = direct, b = total) ----
make_dot_panel <- function(DF_overlap, tag, size_lab = "PDF (direct / total)") {
  df_long <- DF_overlap %>%
    filter(!is.na(Owner_region)) %>%
    transmute(Ultimate_shareholder, Owner_region, total_Exposure_kUSD,
              total_PDF_total, total_PDF_direct, overlap_total, overlap_direct) %>%
    pivot_longer(c(overlap_total, overlap_direct),
                 names_to = "type", values_to = "overlap") %>%
    mutate(type = recode(type, overlap_total = "Total overlap", overlap_direct = "Direct overlap"),
           PDF_size = ifelse(type == "Direct overlap", total_PDF_direct, total_PDF_total)) %>%
    filter(is.finite(overlap), is.finite(total_Exposure_kUSD), is.finite(PDF_size),
           total_Exposure_kUSD > 0, PDF_size >= 0, overlap >= 0, overlap <= 1)

  panel_stats <- df_long %>%
    group_by(type) %>%
    summarise(n = n(),
              mean_overlap      = mean(overlap, na.rm = TRUE),
              sd_overlap        = sd(overlap, na.rm = TRUE),
              overlap_pdf_wmean = sum(overlap * PDF_size, na.rm = TRUE) /
                sum(PDF_size, na.rm = TRUE), .groups = "drop")

  p <- ggplot(df_long, aes(overlap, total_Exposure_kUSD, size = PDF_size, color = type)) +
    geom_point(alpha = 0.5) +
    facet_wrap(~ type, ncol = 2) +
    scale_color_manual(values = c("Direct overlap" = col_direct,
                                  "Total overlap"  = col_total),
                       guide = "none") +
    scale_x_continuous(limits = c(0, 1)) +
    scale_y_continuous(trans = "log10", labels = label_number(scale_cut = cut_short_scale())) +
    scale_size_continuous(range = c(0.5, 12),
                          labels = label_number(scale_cut = cut_short_scale()),
                          name = size_lab) +
    geom_text(data = panel_stats, inherit.aes = FALSE, hjust = 1, vjust = 1.2, size = 3.5,
              aes(x = 0.98, y = Inf,
                  label = paste0("n = ", n,
                                 "\nmean overlap = ", round(mean_overlap, 3),
                                 "\nSD = ", round(sd_overlap, 3),
                                 "\nimpact-wtd mean = ", round(overlap_pdf_wmean, 3)))) +
    theme_minimal(base_size = 16) +
    theme(axis.text    = element_text(size = 12),
          axis.title   = element_text(size = 14),
          strip.text   = element_text(size = 13, face = "bold"),
          legend.title = element_text(size = 13),
          legend.text  = element_text(size = 11)) +
    labs(x = "Overlap score",
         y = "Economic size (allocated turnover, USD k) [log10]")

  ggsave(file.path(fig_dir, sprintf("Fig6ab_DotPanel_%s.png", tag)),
         p, width = 13, height = 6.5, dpi = 300)
}

# ---- 4.2 Fig S3a-b: country dot panel (points = countries) ----
make_country_dot_panel <- function(res, shares, tag) {

  country_pdf <- shares$PDF_ic %>%
    group_by(allocated_country) %>%
    summarise(total_PDF_direct = sum(PDF_direct, na.rm = TRUE),
              total_PDF_total  = sum(PDF_total,  na.rm = TRUE), .groups = "drop")

  df <- bind_rows(
    res$country_direct_pdf %>%
      transmute(allocated_country, type = "Direct overlap (PDF-weighted)", overlap = value) %>%
      left_join(country_pdf %>% select(allocated_country, PDF_size = total_PDF_direct),
                by = "allocated_country"),
    res$country_total_pdf %>%
      transmute(allocated_country, type = "Total overlap (PDF-weighted)", overlap = value) %>%
      left_join(country_pdf %>% select(allocated_country, PDF_size = total_PDF_total),
                by = "allocated_country")
  ) %>%
    left_join(shares$country_exposure, by = "allocated_country") %>%
    left_join(Table_Country_Region, by = "allocated_country") %>%
    mutate(World_region = ifelse(is.na(World_region) | World_region == "", "Unknown", World_region)) %>%
    filter(is.finite(overlap), overlap >= 0, overlap <= 1,
           is.finite(total_Exposure_kUSD), total_Exposure_kUSD > 0,
           is.finite(PDF_size), PDF_size >= 0)

  one_panel <- function(d, ttl, size_title) {
    st <- d %>%
      summarise(n                 = n(),
                mean_overlap      = mean(overlap, na.rm = TRUE),
                sd_overlap        = sd(overlap, na.rm = TRUE),
                overlap_pdf_wmean = sum(overlap * PDF_size, na.rm = TRUE) /
                                    sum(PDF_size, na.rm = TRUE))

    ggplot(d, aes(overlap, total_Exposure_kUSD, size = PDF_size, fill = World_region)) +
      geom_point(alpha = 0.9, shape = 21, colour = "black", stroke = 0.35) +
      geom_text(data = st, inherit.aes = FALSE, hjust = 1, vjust = 1.2, size = 3.5,
                aes(x = 0.98, y = Inf,
                    label = paste0("n = ", n,
                                   "\nmean overlap = ", round(mean_overlap, 3),
                                   "\nSD = ", round(sd_overlap, 3),
                                   "\nimpact-wtd mean = ", round(overlap_pdf_wmean, 3)))) +
      scale_x_continuous(limits = c(0, 1)) +
      scale_y_continuous(trans = "log10", labels = label_number(scale_cut = cut_short_scale())) +
      scale_size_continuous(range = c(1, 14),
                            labels = label_number(scale_cut = cut_short_scale()),
                            name = size_title) +
      scale_fill_manual(values = region_colors, breaks = names(region_colors),
                        na.value = "grey60", name = "World region") +
      theme_minimal(base_size = 16) +
      theme(plot.title   = element_text(size = 14),
            axis.text    = element_text(size = 12),
            axis.title   = element_text(size = 14),
            legend.title = element_text(size = 13),
            legend.text  = element_text(size = 11)) +
      labs(title = ttl,
           x = "Share of country impact matched by portfolio dependencies",
           y = "Economic size (allocated turnover, USD k) [log10]")
  }

  p <- one_panel(filter(df, type == "Direct overlap (PDF-weighted)"),
                 "Direct overlap (PDF-weighted)", "Country PDF (direct)") +
       one_panel(filter(df, type == "Total overlap (PDF-weighted)"),
                 "Total overlap (PDF-weighted)",  "Country PDF (total)") +
       plot_layout(ncol = 2, guides = "collect") & theme(legend.position = "right")

  ggsave(file.path(fig_dir, sprintf("FigS3ab_Country_DotPanel_%s.png", tag)),
         p, width = 15, height = 7, dpi = 300)
}

# ---- 4.3 Fig 6c / 6d: overlap maps (two SEPARATE single-country maps) ----
region_codes <- read_excel("Data/Raw_Data/GLORIA_060/GLORIA_ReadMe_060.xlsx", sheet = "Regions") %>%
  transmute(allocated_country = as.character(Region_names),
            iso3 = toupper(as.character(Region_acronyms)))
world <- ne_countries(scale = "medium", returnclass = "sf")
world_keys <- world %>% st_drop_geometry() %>%
  transmute(iso_a3, iso_a3_eh = as.character(iso_a3_eh),
            name_key_long = str_squish(str_to_lower(as.character(name_long))),
            name_key_name = str_squish(str_to_lower(as.character(name))))

make_map_df <- function(df_val) {
  fold <- function(m, fb, sfx) m %>% left_join(fb, by = "iso_a3", suffix = c("", sfx)) %>%
    mutate(value = coalesce(value, .data[[paste0("value", sfx)]])) %>% select(-all_of(paste0("value", sfx)))
  agg <- function(d) d %>% filter(!is.na(iso_a3)) %>% group_by(iso_a3) %>%
    summarise(value = mean(value, na.rm = TRUE), .groups = "drop")
  dc <- df_val %>% left_join(region_codes, by = "allocated_country") %>%
    transmute(iso3, region_name = allocated_country, value = as.numeric(value)) %>%
    filter(!is.na(value)) %>% group_by(iso3) %>%
    summarise(value = mean(value, na.rm = TRUE), region_name = first(na.omit(region_name)), .groups = "drop") %>%
    filter(!is.na(iso3), iso3 != "")
  m <- world %>% left_join(dc %>% select(iso3, value), by = c("iso_a3" = "iso3"))
  u1 <- dc %>% anti_join(world_keys %>% select(iso_a3), by = c("iso3" = "iso_a3"))
  m <- fold(m, agg(u1 %>% left_join(world_keys %>% select(iso_a3, iso_a3_eh), by = c("iso3" = "iso_a3_eh"))), "_eh")
  u2 <- u1 %>% anti_join(world_keys %>% select(iso_a3_eh), by = c("iso3" = "iso_a3_eh")) %>%
    mutate(region_key = str_squish(str_to_lower(region_name)))
  m <- fold(m, agg(u2 %>% left_join(world_keys %>% select(iso_a3, name_key_long), by = c("region_key" = "name_key_long"))), "_nl")
  u3 <- u2 %>% anti_join(world_keys %>% select(name_key_long), by = c("region_key" = "name_key_long"))
  m <- fold(m, agg(u3 %>% left_join(world_keys %>% select(iso_a3, name_key_name), by = c("region_key" = "name_key_name"))), "_nm")
  m
}

# One single country map. eq. 27 is a share, so the scale is fixed to [0, 1]
# and comparable across variants. Low share = pale, high share = saturated
# column colour (red = direct, blue = total). Antarctica cropped, ocean white.
one_overlap_map <- function(df, ttl, low, high) {
  ggplot(make_map_df(df)) +
    geom_sf(aes(fill = value), color = "grey55", linewidth = 0.03) +
    scale_fill_gradient(low = low, high = high, limits = c(0, 1),
                        breaks = c(0, 0.5, 1),
                        labels = scales::label_number(accuracy = 0.01),
                        oob = scales::squish, na.value = "grey90", name = map_leg) +
    coord_sf(ylim = c(-56, 84), expand = FALSE) +
    theme_minimal(base_size = 15) + labs(title = ttl) +
    theme(plot.title   = element_text(size = 14),
          axis.text    = element_blank(),
          panel.grid   = element_blank(),
          panel.background = element_rect(fill = "white", colour = NA),
          legend.title = element_text(size = 12),
          legend.text  = element_text(size = 10),
          legend.position = "right")
}

make_maps <- function(res, tag) {
  m_d <- one_overlap_map(res$country_direct_pdf, "Direct overlap", col_direct_low, col_direct)
  m_t <- one_overlap_map(res$country_total_pdf,  "Total overlap",  col_total_low,  col_total)
  ggsave(file.path(fig_dir, sprintf("Fig6c_Map_Direct_%s.png", tag)),
         m_d, width = 11, height = 5, dpi = 300)
  ggsave(file.path(fig_dir, sprintf("Fig6d_Map_Total_%s.png", tag)),
         m_t, width = 11, height = 5, dpi = 300)
}

## ---------------------------------------------------------
## 5. Runs
## ---------------------------------------------------------
# main : W, PDF, 14 ES     HQ : HQ, PDF, 14 ES     core : W, PDF, 8-ES

shares_W_pdf  <- build_shares("W")
shares_HQ_pdf <- build_shares("HQ")

res_main <- compute_overlap(shares_W_pdf,  es_keep = NULL)
res_core <- compute_overlap(shares_W_pdf,  es_keep = es_core)
res_HQ   <- compute_overlap(shares_HQ_pdf, es_keep = NULL)

# Fig 6a-b: shareholder dot panels
make_dot_panel(res_main$DF_overlap, "main")
make_dot_panel(res_HQ$DF_overlap,   "HQ")
make_dot_panel(res_core$DF_overlap, "core")

# Fig 6c / 6d: overlap maps (separate direct + total per variant)
make_maps(res_main, "main")
make_maps(res_HQ,   "HQ")
make_maps(res_core, "core")

# Fig S3a-b: country dot panel (main)
make_country_dot_panel(res_main, shares_W_pdf, "main")

## ---------------------------------------------------------
## 6. Export data
## ---------------------------------------------------------

save_overlap_data <- function(res, tag) {
  write.xlsx(
    list(Overlap        = res$DF_overlap,
         Country_direct = res$country_direct_pdf,
         Country_total  = res$country_total_pdf),
    file.path(data_dir, sprintf("Data_Fig6_%s.xlsx", tag)), overwrite = TRUE
  )
}
save_overlap_data(res_main, "main")
save_overlap_data(res_HQ,   "HQ")
save_overlap_data(res_core, "core")

# Fig S3 country dot-panel underlying data (main)
country_pdf_main <- shares_W_pdf$PDF_ic %>%
  group_by(allocated_country) %>%
  summarise(total_PDF_direct = sum(PDF_direct, na.rm = TRUE),
            total_PDF_total  = sum(PDF_total,  na.rm = TRUE), .groups = "drop")
country_S3 <- bind_rows(
  res_main$country_direct_pdf %>% transmute(allocated_country, type = "Direct", overlap = value),
  res_main$country_total_pdf  %>% transmute(allocated_country, type = "Total",  overlap = value)
) %>%
  left_join(country_pdf_main, by = "allocated_country") %>%
  left_join(shares_W_pdf$country_exposure, by = "allocated_country") %>%
  left_join(Table_Country_Region, by = "allocated_country")
write.xlsx(country_S3, file.path(data_dir, "Data_FigS3_country_main.xlsx"), overwrite = TRUE)

wmean <- function(df, ov, w) sum(df[[ov]] * df[[w]], na.rm = TRUE) / sum(df[[w]], na.rm = TRUE)
cat("Impact-wtd mean overlap (total)  ",
    "main:", round(wmean(res_main$DF_overlap, "overlap_total", "total_PDF_total"), 3),
    " core:", round(wmean(res_core$DF_overlap, "overlap_total", "total_PDF_total"), 3),
    " HQ:",   round(wmean(res_HQ$DF_overlap,   "overlap_total", "total_PDF_total"), 3), "\n")
