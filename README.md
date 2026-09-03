# Dingkuhn-etal-equity-biodiversity-overlap
Analysis code for the paper *"Mapping the Geographic (Mis)alignment of Biodiversity Impacts and Ecosystem-Service Dependencies in Global Equity Portfolios"* (Nature Communications).
# equity-biodiversity-overlap

Analysis code for the paper *"Mapping the Geographic (Mis)alignment of Biodiversity
Impacts and Ecosystem-Service Dependencies in Global Equity Portfolios"*
(Nature Communications).

The code links global public-equity ownership to the geographic distribution of
land-use biodiversity impacts and ecosystem-service (ES) dependencies, and
quantifies their overlap within portfolios. It combines Moody's Orbis ownership
and financial data with the GLORIA multi-regional input–output model, LC-IMPACT
characterization factors and ENCORE dependency scores.

This repository contains **only the code**. The input data are not included and
must be obtained from their original sources (see "Data" below).

## Requirements

- R (version 4.5.2)
- R packages: dplyr, tidyr, data.table, Matrix, ggplot2, sf, rnaturalearth,
  circlize, openxlsx, readxl, stringr, scales, patchwork, viridis

## Scripts

Run in numbered order; each builds on the objects saved by the previous ones.

1. **`1. Create key MRIO matrices.R`** — Loads the GLORIA MRIO database and builds
   the key matrices used throughout (transaction and final-demand matrices, gross
   output, the Leontief inverse, and the land-use satellite matrix), with
   region/sector labels, trimming zero-output nodes.
2. **`2. firm-level revenue allocation.R`** — Cleans the Orbis data, builds the
   ownership network, reconciles parent and subsidiary turnover, derives ultimate
   ownership (C/D decomposition), harmonises sectors to GLORIA, and builds the
   geographic revenue-allocation weights via iterative proportional fitting (RAS).
3. **`3. Shareholder-level revenue allocation.R`** — Attributes allocated firm
   turnover to ultimate shareholders according to their equity stakes.
4. **`4. Biodiversity analysis.R`** — Builds LC-IMPACT characterization factors and
   biodiversity-impact intensities (PDF), combines them with shareholder exposure,
   and produces the impact figures (Fig. 2, Fig. 3, Fig. S1).
5. **`5. Dependency analysis.R`** — Builds the ENCORE dependency matrix, propagates
   dependencies along supply chains (Leontief inverse), and produces the dependency
   figures (Fig. 4, Fig. 5).
6. **`6. Overlap analysis.R`** — Computes the country-level overlap between impacts
   and dependencies within portfolios and produces the overlap figures (Fig. 6,
   Fig. S3).
7. **`7. Descriptive statistics.R`** — Computes the descriptive statistics of the
   sample (firms, market capitalisation, shareholders, attributed equity).

Paths at the top of each script (`setwd(...)`) must be adjusted to the local
directory structure.

## Data (not included)

The code is provided for transparency and inspection. Because the ownership data
are proprietary, the pipeline cannot be run end-to-end from public data alone.
The inputs must be obtained from their original sources:

- **Orbis** (firm ownership and financial data) — proprietary; requires the
  purchase of a license from Moody's / Bureau van Dijk (https://www.bvdinfo.com).
  These data cannot be redistributed.
- **GLORIA** (MRIO database) — available from the Global MRIO Lab
  (https://ielab.info); licensed CC BY-NC-SA 4.0.
- **LC-IMPACT** (characterization factors) — available at https://www.lc-impact.eu;
  licensed CC BY.
- **ENCORE dependency ratings** — the GLORIA–NACE–ENCORE concordance table
  (`GLORIA_NACE_ENCORE_059a.xlsx`), which incorporates the ENCORE materiality
  ratings used here, was kindly provided by the author team of the AFD study
  *"Assessing economic exposure to nature-related risks"*
  (https://www.afd.fr/en/resources/assessing-economic-exposure-nature-related-risks).
  ENCORE data are available at https://encorenature.org.

## License

Code released under the MIT License (see LICENSE).

## Citation

Please cite the associated paper (reference to be added upon publication).
