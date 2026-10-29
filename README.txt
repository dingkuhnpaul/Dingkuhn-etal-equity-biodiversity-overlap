=========================================================
Geographic Alignment of Biodiversity Impacts and Dependencies in Global Equity Portfolios
=========================================================

Code and data accompanying the paper (Nature Communications).
All analyses were performed in R (version 4.5.2), using the packages readxl,
openxlsx, dplyr, tidyr, stringr, data.table, Matrix, ggplot2, scales, viridis,
patchwork, circlize, sf and rnaturalearth.


REPOSITORY STRUCTURE
--------------------
Dingkuhn_etal_biodiversity_equity_portfolios_code_data/
   Code/                   R scripts (run in numbered order)
   Data/
      Raw_Data/            input data provided with the repository (see DATA below)
         Concordance_tables/
         CPI_US.xlsx
      Processed_Data/      data underlying the figures of the paper (see DATA below)
         Source Data.xlsx

When the scripts are run, they also create Data/Processed_Data/ subfolders for
intermediate objects and a Results/ folder for the figure outputs (see OUTPUTS).


CODE
----
The scripts are run in order; each builds on the objects saved by the previous ones.

1. "1. Create key MRIO matrices.R"
   Loads the GLORIA multi-regional input-output database and constructs the key
   matrices used throughout (transaction matrix, final demand, gross output, the
   Leontief inverse, and the land-use satellite matrix), together with the
   region/sector labels, trimming zero-output nodes.

2. "2. firm-level revenue allocation.R"
   Cleans the Orbis firm data, builds the ownership edge list, reconciles parent
   and subsidiary turnover into company divisions, derives ultimate ownership
   through the direct/indirect (C/D) decomposition, harmonises sectors to the
   GLORIA classification, and constructs the geographic revenue-allocation
   weights via iterative proportional fitting (IPF/RAS).

3. "3. Shareholder-level revenue allocation.R"
   Attributes allocated firm turnover to ultimate shareholders according to their
   equity stakes, producing the shareholder-level exposure table.

4. "4. Biodiversity_analysis.R"
   Builds the LC-IMPACT characterization-factor matrix and the biodiversity-impact
   intensities (PDF), combines them with shareholder exposure, and produces the
   impact maps (Fig. 2; Supplementary Figs. 1 and 4) and the cross-regional
   impact chord diagrams (Fig. 3a,b; Supplementary Fig. 5a,b), together with the
   data underlying Fig. 3c and Supplementary Fig. 5c.

5. "5. Dependency_analysis.R"
   Builds the ENCORE-based dependency matrix, propagates dependencies through the
   supply chain (Leontief inverse), combines them with exposure, and produces the
   dependency heatmaps (Fig. 5; Supplementary Figs. 2 and 7), together with the
   data underlying Fig. 4 and Supplementary Fig. 6.

6. "6. Overlap_analysis.R"
   Computes the country-level overlap between biodiversity impacts and ES
   dependencies within portfolios and produces the overlap figures (Fig. 6;
   Supplementary Figs. 3, 8 and 9).

7. "7. Descriptive statistics.R"
   Computes the descriptive statistics of the sample (number of firms, market
   capitalisation, number of ultimate shareholders and attributed equity).


OUTPUTS
-------
Scripts 4 to 6 save figure panels to Results/Figures_final/ and the data
underlying each figure to Results/Data_Figures_final/. File names carry a suffix
indicating the specification:
   _main   geographic allocation of firm activity (main results)
   _HQ     headquarter allocation (robustness check, Supplementary Figs. 4 to 8)
   _core   dependencies restricted to the eight land-use core ES
           (robustness check, Supplementary Fig. 9)
Figure panels are saved as vector PDF (Arial/Helvetica, 180 mm wide figures)
and as PNG (300 dpi). The bar charts in Figs. 3c and 4 (and Supplementary
Figs. 5c and 6) are drawn from the exported Excel data, and the final figures
are assembled from these panels.


DATA
----
Raw data provided (Data/Raw_Data/):

- Concordance_tables/: concordance tables used to link the datasets:
     country_to_region.xlsx                 GLORIA countries to world regions
     ISO_Orbis_to_country_GLORIA.xlsx       Orbis country codes to GLORIA countries
     NACE_to_GLORIA_sectors.xlsx            NACE Rev. 2 sectors to GLORIA sectors
     unmatched_NACE_to_GLORIA_sectors.xlsx  manual matches for remaining NACE codes
     Regions_GLORIA_LC_IMPACT.xlsx          LC-IMPACT regions to GLORIA regions

- CPI_US.xlsx: US consumer price index (Federal Reserve Bank of Minneapolis),
  used to adjust firm revenues to 2024 USD.

Processed data provided (Data/Processed_Data/):

- Source Data.xlsx: the data underlying all main and Supplementary figures of the
  paper, with one sheet per figure. The same file is published as the Source
  Data file accompanying the paper.

Data NOT provided, and how to obtain them:

- Orbis data are proprietary and are NOT included. Firm-level ownership and
  financial data are obtained from Moody's Orbis (Bureau van Dijk) and require the
  purchase of a license; they cannot be redistributed. The scripts that depend on
  these data therefore cannot be run without a valid Orbis license.

- The concordance table "GLORIA_NACE_ENCORE_059a.xlsx", which links GLORIA
  sectors to NACE sectors and to the ENCORE materiality ratings, is NOT included
  because it was provided by the French Development Agency (AFD). It was developed
  for Maurin, J., Calas, J. & Godin, A. Assessing Economic Exposure to
  Nature-Related Risks (AFD, 2025;
  https://www.afd.fr/en/resources/assessing-economic-exposure-nature-related-risks).
  Requests for this table should be addressed to the authors of that paper.
  Script 5 expects it in Data/Raw_Data/Concordance_tables/.

- ENCORE materiality scores (version July 2024) are available from
  https://www.encorenature.org.

- The GLORIA database (release 060) is large and must be obtained from the Global
  MRIO Lab (https://ielab.info/labs/ielab-gloria); see script 1 for the expected
  files. Scripts 4 and 6 also read the GLORIA ReadMe file
  (Data/Raw_Data/GLORIA_060/GLORIA_ReadMe_060.xlsx).

- LC-IMPACT characterization factors (version 1.3) are available from
  https://lc-impact.eu. Script 4 expects the land-use factors in
  Data/Raw_Data/LC-IMPACT/land_stress_default_CFs/CFs_land_Use_average.xlsx.

- Country boundaries used in the maps are from Natural Earth (1:50m Admin 0 -
  Countries), loaded through the R package rnaturalearth. Natural Earth data are
  in the public domain (https://www.naturalearthdata.com/about/terms-of-use).


NOTES
-----
The code is provided for transparency and inspection. Because the Orbis inputs are
proprietary, the pipeline cannot be executed end-to-end from public data alone.
All paths in the scripts are relative to the repository root. The setwd(...) line
at the top of each script is commented out: either open the scripts from the
repository root (e.g. as an RStudio project) or uncomment it and set it to the
local path of the repository.
