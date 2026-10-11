# OSA_LST_analyses_R

This study assesses (i) how LST of forests and open land vary across elevation, (ii) in which point in time is there the biggest difference between the LST of the landcovers, (iii) how is the cooling effect of forests influence by increasing air temperatures during the El Niño event?

research_project/
├── README.md
├── .gitignore
├── research_project.Rproj
├── 00_data/
│   ├── raw/
│   ├── intermediate_analyses/
│   ├── processed/
│   ├── vis_layers/
├── 01_functions/
│   ├── functions.R
├── 02_analysis/
│   ├── 00_preprocess_raw_data_layers.qmd
│   ├── 01_sampling_pairs_open_land_vs_forest.qmd
│   ├── 02_cooling_stats_and_elevation_correlation.qmd
│   └── 03_climate_cooling_correlation.qmd
├── 03_results/
|    ├── figures/
└── 99_supporting_information/