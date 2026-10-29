## =========================================================
## Part I: Key matrices from the GLORIA MRIO database
## =========================================================
# Constructs the core input-output matrices from the GLORIA MRIO table:
#   x  : industry output vector
#   Z  : intermediate use matrix         (industries x industries)
#   Y  : final demand matrix             (industries x final demand categories)
#   F  : land use satellite matrix       (indicators x industries)
#   A  : technical coefficients matrix   (industries x industries)
#   L  : Leontief inverse (I - A)^-1     (industries x industries)
#
# Zero-output industries are removed before A and L are computed so all
# downstream scripts load clean matrices directly.
#
# Dimensions (before cleaning):
#   164 regions x 120 sectors = 19,680 industries
#   T is stored as 39,360 x 39,360 (120 products + 120 industries per region)
## =========================================================
##
## ---- NOTE TO REVIEWERS — mapping to the Methods -----------------------
## This script builds the GLORIA MRIO objects that the rest of the pipeline
## relies on; it corresponds to the "Biodiversity footprint assessment"
## Methods subsection, specifically the definitions of:
##   - the transaction matrix Z and gross-output vector x
##   - the environmental-pressure matrix F (land-use pressures, eq. 9)
##   - the technical-coefficients matrix A = Z x̂⁻¹                 (eq. 10)
##   - the Leontief inverse L = (I − A)⁻¹                          (eq. 11)
## The final-demand matrix Y is also assembled here; it feeds the MRIO-based
## revenue-allocation weights W (eq. 1) built in Part II.
## Modelling choices stated in the Methods and implemented below:
##   - n = 163 × 120 = 19,560 country–sector nodes after removing
##     zero-output industries (§6);
##   - columns of A with sum > 1 are set to zero before inverting L, as they
##     reflect estimation noise in weakly constrained economies (§7.1).
## ----------------------------------------------------------------------
## =========================================================

#setwd("")
rm(list = ls())

library(readxl)
library(data.table)
library(dplyr)
library(Matrix)

year  <- 2024
n_reg <- 164   # regions
n_sec <- 120   # sectors per region
n_fd  <- 6     # final demand categories per region
n_ind <- n_reg * n_sec         # 19,680 industries
n_T   <- n_reg * n_sec * 2    # 39,360 rows/cols in T

## ---------------------------------------------------------
## 0. Load raw GLORIA data
## ---------------------------------------------------------

# Final demand table — small enough to read directly
Y_raw <- read.csv(
  paste0("Data/Raw_Data/GLORIA_060/GLORIA_MRIOs_60_", year,
         "/20251217_120secMother_AllCountries_002_Y-Results_",
         year, "_060_Markup001(full).csv"),
  header = FALSE
)

# Transaction matrix T — too large to read at once, split into 3 parts
file_path_T <- paste0(
  "Data/Raw_Data/GLORIA_060/GLORIA_MRIOs_60_", year,
  "/20251217_120secMother_AllCountries_002_T-Results_",
  year, "_060_Markup001(full).csv"
)

part_size <- ceiling(n_T / 3)

T1 <- as.matrix(fread(file_path_T, nrows = part_size,                    select = 1:n_T, header = FALSE))
T2 <- as.matrix(fread(file_path_T, skip = part_size,   nrows = part_size, select = 1:n_T, header = FALSE))
T3 <- as.matrix(fread(file_path_T, skip = 2*part_size,                   select = 1:n_T, header = FALSE))

T <- rbind(T1, T2, T3)
rm(T1, T2, T3); gc()

## ---------------------------------------------------------
## 1. Output vector x
## ---------------------------------------------------------
# x[i] = total output of industry i = sum over all uses of its product column.
# Sanity check: column sums (x) must equal row sums (x2) in a balanced table.

prod_rows <- function(i) ((i - 1) * 2 * n_sec + 1)        : ((i - 1) * 2 * n_sec + n_sec)
ind_cols  <- function(i) ((i - 1) * 2 * n_sec + n_sec + 1): (i * 2 * n_sec)
ind_idx   <- function(i) ((i - 1) * n_sec + 1)            : (i * n_sec)
fd_idx    <- function(j) ((j - 1) * n_fd  + 1)            : (j * n_fd)

x  <- numeric(n_ind)
x2 <- numeric(n_ind)

for (i in 1:n_reg) {
  x[ind_idx(i)]  <- colSums(T[, ind_cols(i)])
  x2[ind_idx(i)] <- rowSums(T[prod_rows(i), ])
}

stopifnot(sum((x - x2)^2) < 1e-6)
message("Balance check passed: sum of squared deviations = ", round(sum((x - x2)^2), 10))

rm(x2)

## ---------------------------------------------------------
## 2. Intermediate use matrix Z
## ---------------------------------------------------------
# Z[i, j] = intermediate deliveries from industry i to industry j.
# Extracted from the industry sub-block of T for each region pair.

Z <- matrix(NA_real_, nrow = n_ind, ncol = n_ind)

for (i in 1:n_reg) {
  for (j in 1:n_reg) {
    Z[ind_idx(i), ind_idx(j)] <- T[ind_cols(i), prod_rows(j)]
  }
}

rm(T); gc()

## ---------------------------------------------------------
## 3. Final demand matrix Y
## ---------------------------------------------------------
# Y[i, j] = final demand for product of industry i by demand category j.
# Extracted from the product rows of Y_raw (6 categories per region).

Y_raw <- as.matrix(Y_raw)
Y     <- matrix(NA_real_, nrow = n_ind, ncol = n_fd * n_reg)

for (i in 1:n_reg) {
  for (j in 1:n_reg) {
    Y[ind_idx(i), fd_idx(j)] <- Y_raw[ind_cols(i), fd_idx(j)]
  }
}

rm(Y_raw)

## ---------------------------------------------------------
## 4. Land use satellite matrix F
## ---------------------------------------------------------

# ---- 4.1 Select land use indicators ----------------------
# Retain only land use pressure categories from the satellite accounts.

readme <- "Data/Raw_Data/GLORIA_060/GLORIA_ReadMe_060.xlsx"

gloria_labels_F <- as.data.frame(read_excel(readme, sheet = 6))

gloria_nature_labels <- gloria_labels_F %>%
  filter(Sat_head_indicator == "Land use",
         Sat_indicator != "Urban")

matching_rows_F <- gloria_nature_labels$Lfd_Nr
n_sat           <- length(matching_rows_F)

# ---- 4.2 Extract selected rows from satellite file -------

sat_file <- sprintf(
  "Data/Raw_Data/GLORIA_060/GLORIA_SatelliteAccounts_060_2024/%s_120secMother_AllCountries_002_%s-Results_2024_060_Markup001(full).csv",
  "20240417", "TQ"
)

sat_raw    <- fread(sat_file)
sat_subset <- as.matrix(sat_raw[matching_rows_F, , drop = FALSE])
fwrite(as.data.table(sat_subset), "Data/Processed_Data/20251111_nature_TF_2024.csv")

rm(sat_raw)

# ---- 4.3 Build F -----------------------------------------
# F[k, i] = land use pressure k associated with industry i.
# Extracted from the product columns of each region block.

F <- matrix(NA_real_, nrow = n_sat, ncol = n_ind)

for (i in 1:n_reg) {
  F[1:n_sat, ind_idx(i)] <- sat_subset[1:n_sat, prod_rows(i)]
}

rm(sat_subset)

## ---------------------------------------------------------
## 5. Labels
## ---------------------------------------------------------

Regions    <- read_excel(readme, sheet = "Regions")
Sectors    <- read_excel(readme, sheet = "Sectors")
Satellites <- read_excel(readme, sheet = "Satellites")
VA_FD      <- read_excel(readme, sheet = "Value added and final demand")

# ---- 5.1 Row labels for Z (and x, F) --------------------

label_Z <- matrix(NA_character_, nrow = n_ind, ncol = 3)

for (i in 1:n_reg) {
  label_Z[ind_idx(i), 1] <- as.character(as.matrix(Regions)[i, 2])
  label_Z[ind_idx(i), 2] <- as.character(as.matrix(Regions)[i, 3])
  label_Z[ind_idx(i), 3] <- as.character(as.matrix(Sectors)[, 2])
}

# ---- 5.2 Column labels for Y ----------------------------

label_Y <- matrix(NA_character_, nrow = n_fd * n_reg, ncol = 3)

for (i in 1:n_reg) {
  label_Y[fd_idx(i), 1] <- as.character(as.matrix(Regions)[i, 2])
  label_Y[fd_idx(i), 2] <- as.character(as.matrix(Regions)[i, 3])
  label_Y[fd_idx(i), 3] <- as.character(as.matrix(VA_FD)[, 3])
}

# ---- 5.3 Row labels for F --------------------------------

label_F <- as.matrix(
  Satellites %>%
    filter(Sat_head_indicator == "Land use",
           Sat_indicator != "Urban") %>%
    select(2:4)
)

## ---------------------------------------------------------
## 6. Remove zero-output industries
## ---------------------------------------------------------
# Industries with x = 0 have no production and cause division-by-zero
# in the Leontief inversion downstream. Removed here once so all
# downstream scripts load clean matrices directly.

zero_idx <- which(x == 0)
message(length(zero_idx), " zero-output industries removed")

x       <- x[-zero_idx]
Z       <- Z[-zero_idx, -zero_idx]
Y       <- Y[-zero_idx, ]
F       <- F[, -zero_idx]
label_Z <- label_Z[-zero_idx, ]

## ---------------------------------------------------------
## 7. Technical coefficients matrix A and Leontief inverse L
## ---------------------------------------------------------

# ---- 7.1 Build A ----------------------------------------
# A[i, j] = share of industry j's output sourced from industry i.
# Columns with sum > 1 indicate data anomalies (inputs exceed output);
# those industries are zeroed out rather than propagated into L.
#
# REVIEWERS: this is A = Z x̂⁻¹ of Methods eq. 10 (a_ij = z_ij / x_j). The
# column-sum > 1 zeroing is the safeguard described after eq. 10 ("Sectors
# with column sums of A exceeding unity were set to zero prior to computing
# the Leontief inverse").

A <- Z %*% Diagonal(x = 1 / x)

col_sums <- colSums(A)
bad_idx  <- which(col_sums > 1)

if (length(bad_idx) > 0) {
  message(length(bad_idx), " industries with column sum > 1 — zeroed out")
  print(data.frame(
    region    = label_Z[bad_idx, 1],
    sector    = label_Z[bad_idx, 3],
    output    = x[bad_idx],
    col_sum_A = col_sums[bad_idx]
  ))
  A[, bad_idx] <- 0
}

# ---- 7.2 Build L ----------------------------------------
# L[i, j] = total output of industry i required (directly and indirectly)
# to deliver one unit of final demand from industry j.
#
# REVIEWERS: this is the Leontief inverse L = (I − A)⁻¹ of Methods eq. 11,
# i.e. the series I + A + A² + … used to propagate both biodiversity
# intensities (eq. 12–13) and ES dependencies (eq. 16–17) upstream.

L <- solve(diag(ncol(A)) - A)

## ---------------------------------------------------------
## 8. Save
## ---------------------------------------------------------
saveRDS(zero_idx, paste0("Data/Processed_Data/Created_matrices/zero_idx_", year, ".rds"))
saveRDS(x,       paste0("Data/Processed_Data/Created_matrices/x_",       year, ".rds"))
saveRDS(Z,       paste0("Data/Processed_Data/Created_matrices/Z_",       year, ".rds"))
saveRDS(Y,       paste0("Data/Processed_Data/Created_matrices/Y_",       year, ".rds"))
saveRDS(F,       paste0("Data/Processed_Data/Created_matrices/F_",       year, ".rds"))
saveRDS(A,       paste0("Data/Processed_Data/Created_matrices/A_",       year, ".rds"))
saveRDS(L,       paste0("Data/Processed_Data/Created_matrices/L_",       year, ".rds"))
saveRDS(label_Z, paste0("Data/Processed_Data/Created_matrices/label_Z_", year, ".rds"))
saveRDS(label_Y, paste0("Data/Processed_Data/Created_matrices/label_Y_", year, ".rds"))
saveRDS(label_F, paste0("Data/Processed_Data/Created_matrices/label_F_", year, ".rds"))