## =========================================================
## Part IV: Biodiversity footprint of portfolios (land use)
## ALT VERSION: Fig S1 is a PANEL of kha maps, one per land-use pressure,
## instead of a single aggregated kha map. Everything else is identical to
## the main reorganised Part IV.
## =========================================================

setwd("D:/Paul/Paper 3/Code ocean capsule")
rm(list = ls())

library(readxl)
library(Matrix)
library(data.table)
library(dplyr)
library(tidyr)
library(openxlsx)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(stringr)
library(circlize)
library(scales)
library(patchwork)      # <- added for the per-pressure map panel

year  <- 2024
n_sec <- 120

fig_dir  <- "Results/Figures_final"
data_dir <- "Results/Data_Figures_final"
dir.create(fig_dir,  recursive = TRUE, showWarnings = FALSE)
dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)

## ---------------------------------------------------------
## 0. Load data
## ---------------------------------------------------------

F             <- readRDS(paste0("Data/Processed_Data/Created_matrices/F_",        year, ".rds"))
x             <- readRDS(paste0("Data/Processed_Data/Created_matrices/x_",        year, ".rds"))
L             <- readRDS(paste0("Data/Processed_Data/Created_matrices/L_",        year, ".rds"))
label_Z       <- readRDS(paste0("Data/Processed_Data/Created_matrices/label_Z_",  year, ".rds"))
label_F       <- readRDS(paste0("Data/Processed_Data/Created_matrices/label_F_",  year, ".rds"))
zero_idx      <- readRDS(paste0("Data/Processed_Data/Created_matrices/zero_idx_", year, ".rds"))

Exposure_Data <- readRDS("Data/Processed_Data/Exposure_Data.RDS")
W_long        <- readRDS(paste0("Data/Processed_Data/W_IPF_50_", year, ".rds"))

Table_Country_Region <- read.xlsx("Data/Raw_Data/Concordance_tables/country_to_region.xlsx")
region <- setNames(Table_Country_Region$World_region, Table_Country_Region$Country)

## ---------------------------------------------------------
## 1. Characterization factor matrix B (land use)
## ---------------------------------------------------------

B_LU <- read_excel(
  "Data/Raw_Data/LC-IMPACT/land_stress_default_CFs/CFs_land_Use_average.xlsx",
  sheet = "occupation average country", skip = 1
)

B_LU_continent <- read_excel(
  "Data/Raw_Data/LC-IMPACT/land_stress_default_CFs/CFs_land_Use_average.xlsx",
  sheet = "occupation average continent", skip = 1
) %>%
  slice(2:8) %>%
  filter(...1 != "Australia")

B_LU <- bind_rows(B_LU, B_LU_continent)
rm(B_LU_continent)

colnames(B_LU)[1] <- "Country"

B_LU <- B_LU[-1, ] %>%
  select(
    Country,
    `Annual crops`, `Permanent crops`, `Pasture`,
    `Intensive forestry`, `Extensive forestry`     # order matches label_F
  ) %>%
  mutate(across(-Country, ~ as.numeric(.) * 1e7))

names(B_LU) <- c("Region_names", as.character(label_F[, 2]))

stopifnot(identical(names(B_LU)[-1], as.character(label_F[, 2])))

concordance <- read_excel("Data/Raw_Data/Concordance_tables/Regions_GLORIA_LC_IMPACT.xlsx")

B_matched <- merge(
  B_LU, concordance,
  by.x = "Region_names", by.y = "Region_names_CF_IMPACT", all = FALSE
)
B_matched$Region_names <- NULL
colnames(B_matched)[colnames(B_matched) == "Region_names_GLORIA"] <- "Region_names"

B_matched <- B_matched[, c((ncol(B_matched) - 2):ncol(B_matched),
                           1:(ncol(B_matched) - 3))]
B_matched <- B_matched[order(B_matched$Lfd_Nr), ]
rownames(B_matched) <- NULL
B_matched <- B_matched[, -c(1, 2, 3)]

B <- t(B_matched[rep(seq_len(nrow(B_matched)), each = n_sec), ])
B <- B[, -zero_idx]

stopifnot(dim(B) == dim(F))
saveRDS(B, paste0("Data/Processed_Data/Created_matrices/B_", year, ".rds"))

## ---------------------------------------------------------
## 2. Direct biodiversity intensity S  (eq 9)
## ---------------------------------------------------------

BF <- B * F
S  <- sweep(BF, 2, x, "/")
saveRDS(S, paste0("Data/Processed_Data/Created_matrices/S_", year, ".rds"))
rm(BF); gc()

## ---------------------------------------------------------
## 3. Exposure tables: W-allocated (main) and HQ (appendix)
## ---------------------------------------------------------

Exposure_agg <- Exposure_Data %>%
  select(-total_revenue, -Owner_name, -Owner_type) %>%
  group_by(Owner_country, Owner_region, country, Owned_region, sector) %>%
  summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)), .groups = "drop")

Exposure_long <- Exposure_agg %>%
  left_join(W_long, by = c("country" = "h", "sector" = "s")) %>%
  mutate(
    Total_exposure_alloc    = Total_exposure    * weight_hs,
    Direct_exposure_alloc   = Direct_exposure   * weight_hs,
    Indirect_exposure_alloc = Indirect_exposure * weight_hs
  ) %>%
  rename(allocated_country = c) %>%
  group_by(Owner_country, Owner_region, sector, allocated_country) %>%
  summarise(
    Total_exposure_alloc    = sum(Total_exposure_alloc,    na.rm = TRUE),
    Direct_exposure_alloc   = sum(Direct_exposure_alloc,   na.rm = TRUE),
    Indirect_exposure_alloc = sum(Indirect_exposure_alloc, na.rm = TRUE),
    .groups = "drop"
  )
saveRDS(Exposure_long, "Data/Processed_Data/Key_Data_Frames/Exposure_final_long.rds")

Exposure_Total <- Exposure_long %>%
  group_by(Owner_country, Owner_region, sector, allocated_country) %>%
  summarise(Exposure_kUSD = sum(Total_exposure_alloc, na.rm = TRUE), .groups = "drop")

Exposure_Total_HQ <- Exposure_agg %>%
  group_by(Owner_country, Owner_region, sector, allocated_country = country) %>%
  summarise(Exposure_kUSD = sum(Total_exposure, na.rm = TRUE), .groups = "drop")

rm(Exposure_Data, W_long); gc()

## ---------------------------------------------------------
## 4. Biodiversity intensities (economic -> ecological)
## ---------------------------------------------------------

countries <- factor(label_Z[, 2])
sectors   <- factor(label_Z[, 3])
n         <- ncol(S)

S_node <- colSums(S)

G_sup <- sparse.model.matrix(~ 0 + countries)
colnames(G_sup) <- levels(countries)

GS    <- t(G_sup) %*% Diagonal(x = S_node)
M_reg <- GS %*% L

M <- as.data.table(as.matrix(M_reg))
setnames(M, paste0("n", seq_len(n)))
M[, supplier_country := colnames(G_sup)]
M <- melt(M, id.vars = "supplier_country",
          variable.name = "consumer_node", value.name = "PDF_yr_kUSD")
M[, consumer_idx     := as.integer(sub("n", "", consumer_node))]
M[, consumer_country := as.character(countries)[consumer_idx]]
M[, consumer_sector  := as.character(sectors)[consumer_idx]]
M[, c("consumer_node", "consumer_idx") := NULL]
M <- M[PDF_yr_kUSD != 0]
M[, supplier_region := region[supplier_country]]
M[, consumer_region := region[consumer_country]]
saveRDS(M, paste0("Data/Processed_Data/Key_Data_Frames/LU_impact_Total_Long_", year, ".rds"))

rm(GS, M_reg); gc()

S_long <- data.table(
  consumer_country = as.character(countries),
  consumer_sector  = as.character(sectors),
  PDF_yr_kUSD      = as.numeric(S_node)
)
S_long[, consumer_region := region[consumer_country]]
saveRDS(S_long, paste0("Data/Processed_Data/Key_Data_Frames/Impact_Direct_Long_", year, ".rds"))

M_region  <- as.data.table(M)[
  , .(PDF_yr_kUSD = sum(PDF_yr_kUSD, na.rm = TRUE)),
  by = .(supplier_region, consumer_country, consumer_sector)]
S_long_dt <- as.data.table(S_long)

## ---------------------------------------------------------
## 5. Figures: shared setup
## ---------------------------------------------------------

world <- ne_countries(scale = "medium", returnclass = "sf")
world_keys <- world %>%
  st_drop_geometry() %>%
  transmute(
    iso_a3,
    iso_a3_eh     = as.character(iso_a3_eh),
    name_key_long = str_squish(str_to_lower(as.character(name_long))),
    name_key_name = str_squish(str_to_lower(as.character(name)))
  )

attach_value_to_world <- function(df_country) {
  fold <- function(map_df, fb, sfx) {
    map_df %>%
      left_join(fb, by = "iso_a3", suffix = c("", sfx)) %>%
      mutate(value = coalesce(value, .data[[paste0("value", sfx)]])) %>%
      select(-all_of(paste0("value", sfx)))
  }
  agg <- function(d) d %>% filter(!is.na(iso_a3)) %>%
    group_by(iso_a3) %>% summarise(value = sum(value, na.rm = TRUE), .groups = "drop")

  map_df <- world %>% left_join(df_country %>% select(iso3, value), by = c("iso_a3" = "iso3"))
  u1 <- df_country %>% anti_join(world_keys %>% select(iso_a3), by = c("iso3" = "iso_a3"))
  map_df <- fold(map_df, agg(u1 %>% left_join(world_keys %>% select(iso_a3, iso_a3_eh),
                                              by = c("iso3" = "iso_a3_eh"))), "_eh")
  u2 <- u1 %>% anti_join(world_keys %>% select(iso_a3_eh), by = c("iso3" = "iso_a3_eh")) %>%
    mutate(region_key = str_squish(str_to_lower(region_name)))
  map_df <- fold(map_df, agg(u2 %>% left_join(world_keys %>% select(iso_a3, name_key_long),
                                              by = c("region_key" = "name_key_long"))), "_nl")
  u3 <- u2 %>% anti_join(world_keys %>% select(name_key_long), by = c("region_key" = "name_key_long"))
  map_df <- fold(map_df, agg(u3 %>% left_join(world_keys %>% select(iso_a3, name_key_name),
                                              by = c("region_key" = "name_key_name"))), "_nm")
  map_df
}

region_codes <- read_excel("Data/Raw_Data/GLORIA_060/GLORIA_ReadMe_060.xlsx", sheet = "Regions") %>%
  transmute(allocated_country = as.character(Region_names),
            iso3              = toupper(as.character(Region_acronyms)))

## ---- Journal artwork settings (Nature: 180 mm width, Arial 5-7 pt, vector PDF)
fig_w_mm <- 180
theme_journal <- function() {
  theme_minimal(base_size = 7, base_family = "Arial") +
    theme(plot.title   = element_text(size = 7, face = "bold"),
          legend.title = element_text(size = 6),
          legend.text  = element_text(size = 5.5),
          axis.text    = element_text(size = 5.5))
}
# Standard vector PDF device (more reliable on Windows than cairo_pdf).
# "Arial" is mapped to Helvetica, the PDF base font accepted by Nature.
if (!"Arial" %in% names(grDevices::pdfFonts())) grDevices::pdfFonts(Arial = grDevices::pdfFonts()$Helvetica)
pdf_dev <- function(filename, width, height, ...)
  grDevices::pdf(filename, width = width, height = height, useDingbats = FALSE, ...)
# Saves a vector PDF (for submission) and a PNG (for the manuscript draft)
save_fig <- function(p, name, height_mm, width_mm = fig_w_mm) {
  ggsave(file.path(fig_dir, paste0(name, ".pdf")), p,
         width = width_mm, height = height_mm, units = "mm", device = pdf_dev, bg = "white")
  ggsave(file.path(fig_dir, paste0(name, ".png")), p,
         width = width_mm, height = height_mm, units = "mm", dpi = 300, bg = "white")
}

# Colour-blind-safe sequential scale (replaces green-yellow-red): light yellow to
# dark red, monotonic in lightness. The scale spans 8 orders of magnitude below
# the maximum (smaller values shown in the lightest colour). Colour steps are
# placed at quantiles of the country values, so each step covers a similar number
# of countries; this keeps the map light and separates the top countries.
fill_log_scale <- function(label, vals, decades = 8) {
  top  <- max(vals, na.rm = TRUE); lo <- top / 10^decades
  cols <- c("#FFFFCC", "#FFEDA0", "#FED976", "#FEB24C", "#FD8D3C",
            "#FC4E2A", "#E31A1C", "#BD0026", "#800026")
  lv   <- log10(pmax(vals[is.finite(vals) & vals > 0], lo))
  pos  <- (quantile(lv, seq(0, 1, length.out = length(cols)), names = FALSE) - log10(lo)) / decades
  pos  <- cummax(pos); pos[1] <- 0; pos[length(pos)] <- 1
  for (k in 2:length(pos)) pos[k] <- max(pos[k], pos[k - 1] + 1e-4)
  pos  <- pos / pos[length(pos)]
  scale_fill_gradientn(
    colours = cols, values = pos,
    trans   = "log10", limits = c(lo, top), oob = squish,
    labels  = label_log(), na.value = "grey90", name = label)
}

# Latitude/longitude graticule and labels (required on maps)
latlon <- list(
  scale_x_continuous(breaks = seq(-120, 120, 60)),
  scale_y_continuous(breaks = seq(-40, 80, 40)),
  coord_sf(ylim = c(-56, 84), expand = FALSE)
)
latlon_theme <- theme(axis.text  = element_text(size = 5, colour = "grey30"),
                      panel.grid = element_line(colour = "grey85", linewidth = 0.1))

draw_map <- function(map_df, label) {
  ggplot(map_df) +
    geom_sf(aes(fill = value_plot), color = "black", linewidth = 0.05) +
    fill_log_scale(label, map_df$value_plot) +
    latlon +
    theme_journal() + latlon_theme +
    theme(legend.key.width = unit(2.5, "mm"), legend.key.height = unit(6, "mm"))
}

# Colour-blind-safe region palette keeping intuitive regional families
# (Asia = reds, Americas = blue/green, Europe = light blue/lavender,
#  Middle East/Africa = yellow/ochre). Minimum pairwise distance >= 9 (CAM02-UCS)
# under simulated deuteranopia, protanopia and tritanopia.
colors <- c(
  "China"                                  = "#B2182B",
  "Far East and Central Asia - Exc. China" = "#D6604D",
  "North America"                          = "#1F4E99",
  "South and Central America"              = "#1B9E77",
  "Western Europe"                         = "#56B4E9",
  "Eastern Europe"                         = "#9970AB",
  "Middle East"                            = "#F0E442",
  "Africa"                                 = "#E69F00",
  "Oceania"                                = "#7FDBDA"
)
region_order <- c("North America", "South and Central America", "Western Europe",
                  "Eastern Europe", "Middle East", "China",
                  "Far East and Central Asia - Exc. China", "Oceania", "Africa")

edges_to_matrix <- function(edge_df, from_col, to_col) {
  xtabs(as.formula(paste("PDF_total ~", from_col, "+", to_col)), data = edge_df)
}

plot_chord <- function(mat, file) {
  mat <- as.matrix(mat)
  miss_r <- setdiff(region_order, rownames(mat))
  if (length(miss_r)) mat <- rbind(mat, matrix(0, length(miss_r), ncol(mat),
                                               dimnames = list(miss_r, colnames(mat))))
  miss_c <- setdiff(region_order, colnames(mat))
  if (length(miss_c)) mat <- cbind(mat, matrix(0, nrow(mat), length(miss_c),
                                               dimnames = list(rownames(mat), miss_c)))
  mat <- mat[region_order, region_order, drop = FALSE]

  draw <- function() {
    circos.clear()
    circos.par(gap.after = rep(2, length(region_order)),
               track.height = 0.0001, cell.padding = rep(5e-4, 4))
    chordDiagram(
      x = mat, grid.col = adjustcolor(colors[region_order], alpha.f = 0.8),
      transparency = 0.03, self.link = 1, annotationTrack = "grid",
      preAllocateTracks = 1, grid.border = NA, reduce = 0
    )
  }
  # Vector PDF at half page width (two chords side by side in Fig 3), plus PNG
  pdf_dev(sub("\\.png$", ".pdf", file), width = 85 / 25.4, height = 85 / 25.4, bg = "white")
  draw(); dev.off()
  png(file, width = 85, height = 85, units = "mm", res = 300, bg = "white")
  draw(); dev.off()
}

build_edges <- function(impact_dt, expo_dt, group_cols) {
  impact_dt[
    expo_dt,
    on = c(consumer_country = "allocated_country", consumer_sector = "sector"),
    nomatch = 0, allow.cartesian = TRUE
  ][
    !is.na(Owner_region),
    .(PDF_total = sum(Exposure_kUSD * PDF_yr_kUSD, na.rm = TRUE)),
    by = group_cols
  ]
}

## ---------------------------------------------------------
## 6. Fig 2 (impact map) + Fig 3a/3b (chords), main + HQ
## ---------------------------------------------------------

make_lu_figures <- function(expo, tag) {

  expo_dt <- as.data.table(expo)

  df_country <- expo %>%
    inner_join(S_long, by = c("allocated_country" = "consumer_country",
                              "sector"            = "consumer_sector")) %>%
    mutate(PDF_contrib = Exposure_kUSD * PDF_yr_kUSD) %>%
    group_by(allocated_country) %>%
    summarise(value = sum(PDF_contrib, na.rm = TRUE), .groups = "drop") %>%
    left_join(region_codes, by = "allocated_country") %>%
    transmute(iso3, region_name = allocated_country, value = as.numeric(value)) %>%
    filter(!is.na(value)) %>%
    group_by(iso3) %>%
    summarise(value = sum(value, na.rm = TRUE),
              region_name = first(na.omit(region_name)), .groups = "drop") %>%
    filter(!is.na(iso3), iso3 != "")

  map_pdf <- attach_value_to_world(df_country) %>%
    mutate(value_plot = ifelse(is.na(value), NA_real_, ifelse(value <= 0, 1e-12, value)))

  save_fig(draw_map(map_pdf, "PDF"), sprintf("Fig2_Impact_Map_%s", tag), height_mm = 80)
  write.xlsx(df_country, file.path(data_dir, sprintf("Data_Fig2_%s.xlsx", tag)), overwrite = TRUE)

  LU_edges_ET <- build_edges(M_region,  expo_dt, c("Owner_region", "supplier_region"))
  LU_edges_ED <- build_edges(S_long_dt, expo_dt, c("Owner_region", "consumer_region"))

  plot_chord(edges_to_matrix(LU_edges_ED, "Owner_region", "consumer_region"),
             file.path(fig_dir, sprintf("Fig3a_Chord_Direct_%s.png", tag)))
  plot_chord(edges_to_matrix(LU_edges_ET, "Owner_region", "supplier_region"),
             file.path(fig_dir, sprintf("Fig3b_Chord_Total_%s.png", tag)))

  LU_edges_ET_country <- build_edges(M_region, expo_dt, c("Owner_country", "supplier_region"))

  write.xlsx(list(ED_region = LU_edges_ED, ET_region = LU_edges_ET,
                  ET_country = LU_edges_ET_country),
             file.path(data_dir, sprintf("Data_Fig3_%s.xlsx", tag)), overwrite = TRUE)
}

make_lu_figures(Exposure_Total,    "main")
make_lu_figures(Exposure_Total_HQ, "HQ")

## ---------------------------------------------------------
## 7. Fig S1 (ALT): land-use footprint PANEL, one map per pressure (kha, main)
## ---------------------------------------------------------
# Instead of a single aggregated kha map, one map per land-use pressure
# (the rows of F): Annual crops, Permanent crops, Pasture, Intensive
# forestry, Extensive forestry.

F_int      <- sweep(F, 2, x, "/")            # n_pressures x n, kha per kUSD
pressures  <- as.character(label_F[, 2])     # 5 land-use pressure names
pressures_lab <- gsub("_", " ", pressures)   # display labels without underscores
node_key   <- data.table(Country_name = label_Z[, 2], Sector = label_Z[, 3])

# Country-level kha footprint attributed to shareholders, for one pressure
build_pressure_country <- function(p_idx) {
  kha_int_p <- data.table(
    Country_name = node_key$Country_name,
    Sector       = node_key$Sector,
    kha_per_kUSD = as.numeric(F_int[p_idx, ])
  )[, .(kha_per_kUSD = sum(kha_per_kUSD, na.rm = TRUE)), by = .(Country_name, Sector)]

  Exposure_Total %>%
    inner_join(kha_int_p, by = c("allocated_country" = "Country_name", "sector" = "Sector")) %>%
    mutate(val = Exposure_kUSD * kha_per_kUSD) %>%
    group_by(allocated_country) %>%
    summarise(value = sum(val, na.rm = TRUE), .groups = "drop") %>%
    left_join(region_codes, by = "allocated_country") %>%
    transmute(iso3, region_name = allocated_country, value = as.numeric(value)) %>%
    filter(!is.na(value)) %>%
    group_by(iso3) %>%
    summarise(value = sum(value, na.rm = TRUE),
              region_name = first(na.omit(region_name)), .groups = "drop") %>%
    filter(!is.na(iso3), iso3 != "")
}

make_pressure_map <- function(p_idx) {
  dfc <- build_pressure_country(p_idx)
  mp  <- attach_value_to_world(dfc) %>%
    mutate(value_plot = ifelse(is.na(value), NA_real_, ifelse(value <= 0, 1e-6, value)))
  ggplot(mp) +
    geom_sf(aes(fill = value_plot), color = "black", linewidth = 0.05) +
    fill_log_scale("kha", mp$value_plot) +
    latlon +
    labs(title = pressures_lab[p_idx]) +
    theme_journal() + latlon_theme +
    theme(panel.background = element_rect(fill = "white", colour = NA),
          legend.key.width = unit(2, "mm"), legend.key.height = unit(4, "mm"))
}

pressure_panel <- wrap_plots(lapply(seq_along(pressures), make_pressure_map), ncol = 2)

save_fig(pressure_panel, "FigS1_Impact_Map_kha_by_pressure", height_mm = 130)

# Underlying data: one sheet per pressure
kha_pressure_data <- setNames(
  lapply(seq_along(pressures), build_pressure_country), pressures)
write.xlsx(kha_pressure_data,
           file.path(data_dir, "Data_FigS1_kha_by_pressure.xlsx"), overwrite = TRUE)
