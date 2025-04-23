# Armande Aboudrar-Méda, Thesis Replication Package 

Welcome! This repository contains the complete replication set for my Master's Thesis titled *"From Reality to Perception: Trust, Socio-Political Attitudes, and Evolving Inequalities in French Healthcare"*. All code, data and documentation needed to reproduce every figure, table and model are here.

The analysis combines survey data: the Barometer from the DREES with 82,332 observations, reduced to 4002 observations for the year 2021 and objective data from the French Health Insurance, as well as geographic shapefiles and datasets allowing to get a geographic final dataset with a geometry by ZEAT region. Results include exploratory/confirmatory factor analysis, clustering, multilevel mixed models, multi-group SEMs, and a fully reproducible workflow.

---

## Repository Structure

ThesisReplicationSet/ 
├── **Data/** 
│ ├── **barometre/** 
│ │ └── barometre2000_2022_diff.csv     # DREES survey data 
│ ├── **APL/**                              # APL Excel files + 'commune' csv  
│ │ ├── APL_mg_2021.xlsx 
│ │ ├── APL_inf_2021.xlsx 
│ │ ├── APL_mk_2021.xlsx 
│ │ ├── APL_sf_2021.xlsx 
│ │ ├── APL_cd_2021.xlsx 
│ │ └── v_commune_2024.csv 
│ ├── **departements-20140306-5m-shp/**     # Shapefile for departments
│ │ ├── departements-20140306-5m.shp 
│ │ ├── departements-20140306-5m.dbf 
│ │ ├── departements-20140306-5m.shx 
│ │ └── … (other components) 
│ ├── ZEAT_Reg_Dep.csv                   # For the mapping departments to ZEAT
│ ├── popsdregagegender_2021_simple.csv  # French 2021 population by age and gender
│ ├── lits-de-reanimation-de-soins-intensifs-et-de-surveillance-continue-en-france0.csv                                 # Objective data on number of beds 
│ ├── demographieexercicesliberaux.csv   # Objective healthcare professionals count data 
│ ├── honoraires.csv                     # Objective fees data 
│ └── dep_santé.csv                      # Expenditure in health data by age/gender
│ ├── **Scripts/** 
│ ├── Aboudrar-Méda_Armande_Thesis_DS.Rmd # the master RMarkdown knitting everything  
│ ├── 01_load_data.R                      # data loading + geometry setup  
│ ├── 02_preprocess.R                     # recoding & missingness  
│ ├── 03_eda.R                            # descriptive tables 
│ ├── 04_efa.R                            # exploratory factor analysis 
│ ├── 05_cfa.R                            # confirmatory factor analysis 
│ ├── 06_clustering.R                     # k-means & Ward clustering
│ ├── 07_objective.R                      # preprocessing objective data 
│ ├── 08_modelingShortage.R               # mixed-effects for shortages
│ ├── 09_modelingInequity.R               # mixed & ordinal models for inequity
│ └── 10_SEMShortage.R                    # structural equation models  
│ ├── **Docs/** 
│ ├── Dictionary.xlsx                     # post-edited dictionary  
│ └── My_Final_Variables_Dictionary.xlsx  # raw variables dictionary
│ ├── config.R                            # loads packages, defines paths & helpers
├── ThesisReplicationSet.Rproj  # RStudio project file
└── README.md # you are here 

---

## Project Overview

This project replicates the analyses described in my Master's Thesis. The key components include:

- **Geographic Data Processing:**  
  Importing and processing shapefiles for French administrative boundaries.

- **Subjective Data Handling:**  
  Loading and cleaning survey (barometer) data, including detailed recoding of variables and handling missing data.

- **Objective Data Integration:**  
  Merging various administrative datasets (population, healthcare expenditure, effectifs, honoraires) and standardising variables per 100,000 population.

- **Factor Analysis:**  
  Conducting exploratory and confirmatory factor analyses (EFA/CFA) using polychoric correlations.

- **Clustering:**  
  Using k-means and Ward-based clustering to identify respondent profiles.

- **Modeling and SEM:**  
  Fitting mixed-effects models and structural equation models (SEM) to examine determinants of healthcare perceptions and inequality.

---

## Environment Setup & Installation

### Requirements

- **Software:**
  - R version 4.2.2 (2022-10-31)
  - RStudio Version 2023.03.0+386 (2023.03.0+386)

- **R Packages:**  
  The analysis requires the following R packages (and their dependencies):
  `here`, `readxl`, `dplyr`, `tidyr`, `readr`, `writexl`, `sf`, `ggplot2`,  
  `viridis`, `stringr`, `knitr`, `psych`, `naniar`, `REdaS`, `GPArotation`,
  `dagitty`, `lavaan`, `ordinal`, `patchwork`, `performance`, `lme4`, `car`,
  `broom.mixed`, `kableExtra`

### Installation Instructions

1. **Clone the Repository:**
   - Run the following command in your terminal:
     ```bash
     git clone https://github.com/yourusername/Aboudrar-Méda_Armande_MasterThesis_DS.git
     ```
   - Alternatively, download and extract the ZIP archive.

2. **Open the Project in RStudio:**
   - Open the `ThesisReplicationSet.Rproj` file. This ensures here::here() resolves correctly.

3. **Run the Configuration Script:**
   - In the R console, run:
     ```r
     source("config.R")
     ```
     
     This will install (if needed) and load all required packages and set up the directory paths.

4. **Knit the Analysis Document:**
   - Open the RMarkdown file located at `Scripts/Aboudrar-Méda_Armande_Thesis_DS.Rmd` in RStudio.
   - Click the **Knit** button to generate the full analysis report.
The document is organized into sections covering:
   - Library loading and configuration setup
   - Data loading and pre-processing (subjective and geographic data)
   - Exploratory Data Analysis (recoding, missingness checks, correlations)
   - Factor analysis (EFA and CFA)
   - Clustering and descriptive statistics
   - Mixed-effects modeling and SEM with diagnostic checks and model comparisons
   
### Data Access

- **Included Data:**  
  All non-sensitive data files are stored in the **Data/** folder. This includes subdirectories for:
  - APL Excel files
  - Barometer CSV data
  - Geographic shapefiles  
    …and CSV files for population, healthcare expenditure, effectifs (healthcare practitioners count dataset), and honoraires (reimbursed and out-of-pocket fees).

### Datasets Overview

| id | Short name / file(s)                       | What it contains (granularity)                                | Years used |  Source    |Public link ↗︎ | Licence   |where to find?|
|----|--------------------------------------------|---------------------------------------------------------------|------------|------------|----------------|---------|--------------|
| 1  | `barometre2000_2022_diff.csv`              | **DREES Barometer** survey‑data – individual level |    2000‑2022 (we use 2021 wave) | DREES | <https://drees2-sgsocialgouv.opendatasoft.com/explore/dataset/431_le-barometre-d-opinion/information/> | Public |**Data/barometre/**|
| 2  | `departements‑20140306‑5m.shp`             | Metropolitan French department polygons(EPSG2154)           | 2014       | ? | <https://www.data.gouv.fr/en/datasets/contours-des-departements-francais-issus-d-openstreetmap/#/resources> |  Public |**Data/departements-20140306-5m-shp/**|
| 3  | `ZEAT_Reg_Dep.csv`                         | Mapping **DEPTtoZEAT** codes                                 | 2024       | created by me |  | Public |**Data/...**|
| 4  | `v_commune_2024.csv`                       | Mapping **CommunesToZEAT**| 2024       | INSEE|<https://www.insee.fr/fr/information/7766585>|Public |**Data/...**|
| 5  | `lits‑de‑reanimation‑*.csv`                | Critical care beds in French healthcare institutions         | 2021       | DREES |  <https://data.drees.solidarites-sante.gouv.fr/explore/dataset/lits-de-reanimation-de-soins-intensifs-et-de-surveillance-continue-en-france0/information/> | Public |**Data/...**|
| 6  | `demographieexercicesliberaux.csv`         | Self-employed healthcare professionals: workforce by type of practice and by area (department, region) | 2021 | CNAM | <https://data.ameli.fr/explore/dataset/demographie-exercices-liberaux/information/?disjunctive.region&disjunctive.departement> | Public |**Data/...**|
| 7  | `honoraires.csv`                           | Self-employed healthcare professionals: fee levels by area (department, region) (department, region) | 2021 | CNAM | <https://data.ameli.fr/explore/dataset/honoraires/information/?disjunctive.region&disjunctive.departement> | Public |**Data/...**|
| 8  | `APL_*_2021.xlsx`                          | **APL** accessibility indices (GP, nurse, physio,…)          | 2021       | DREES |    <https://data.drees.solidarites-sante.gouv.fr/explore/dataset/530_l-accessibilite-potentielle-localisee-apl/information/> | Public | **Data/APL/** subfolder |
| 9  | `popsdregagegender_2021_simple.csv`        | Population by ZEAT age-gender structure (standardised)                 | 2021       | INSEE | <https://www.insee.fr/fr/statistiques/8200787?geo=FE-1&sommaire=8200811> |  Public |**Data/...**|
| 10 | `dep_santé.csv`                            | Expenditures in healthcare              | 2021       | DREES | <https://drees.shinyapps.io/depenses_et_rac/> |  Public |**Data/...**|


For a complete description of the main variables used in the analysis — including variable definitions, ... - please refer to the **Docs/** folder. The comprehensive variables dictionary (`Dictionary.xlsx`) gives a description of the main variables. 

### Contact

For any questions or further assistance with reproducing the analysis, please contact:

**Armande Aboudrar-Méda**  
Email: [223158@students.hertie-school.org]
