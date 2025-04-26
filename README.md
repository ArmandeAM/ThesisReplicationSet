# Armande Aboudrar-Méda, Thesis Replication Package 

Welcome! This repository contains the complete replication set for my Master's Thesis titled *"Perception vs. Provision: Trust, Ideology, and Subjective Healthcare Inequalities in France"*. All code, data and documentation needed to reproduce every figure, table and model are here.

The analysis combines survey data: the Barometer from the DREES with 82,332 observations, reduced to 4002 observations for the year 2021 and objective data from the French Health Insurance, as well as geographic shapefiles and datasets allowing to get a geographic final dataset with a geometry by ZEAT region. Results include exploratory/confirmatory factor analysis, clustering, multilevel mixed models, multi-group SEMs, and a fully reproducible workflow.

---

## Repository Structure

```text
ThesisReplicationSet/
├── Data/
│   ├── barometre/
│   │   └── barometre2000_2022_diff.csv               # DREES survey data
│   ├── APL/                                          # APL Excel files + 'commune' csv
│   │   ├── APL_mg_2021.xlsx
│   │   ├── APL_inf_2021.xlsx
│   │   ├── APL_mk_2021.xlsx
│   │   ├── APL_sf_2021.xlsx
│   │   ├── APL_cd_2021.xlsx
│   │   └── v_commune_2024.csv
│   ├── departements-20140306-5m-shp/                 # Shapefiles for French departments
│   │   ├── departements-20140306-5m.shp
│   │   ├── departements-20140306-5m.dbf
│   │   ├── departements-20140306-5m.shx
│   │   └── ... (other shapefile components)
│   ├── ZEAT_Reg_Dep.csv                              # Mapping departments to ZEAT
│   ├── popsdregagegender_2021_simple.csv             # 2021 population by age/gender
│   ├── lits-de-reanimation-de-soins-intensifs...csv  # ICU bed availability
│   ├── demographieexercicesliberaux.csv              # Healthcare professionals count
│   ├── honoraires.csv                                # Medical fees data
│   └── dep_santé.csv                                 # Expenditure by age/gender
│
├── Scripts/                                          # Main workflow scripts
│   ├── 01_load_data.R                                # Load data + GIS setup
│   ├── 02_preprocess.R                               # Recoding & handling missingness
│   ├── 03_eda.R                                      # Descriptive statistics
│   ├── 04_efa.R                                      # Exploratory Factor Analysis
│   ├── 05_cfa.R                                      # Confirmatory Factor Analysis
│   ├── 06_clustering.R                               # Clustering (K-means, Ward)
│   ├── 07_objective.R                                # Processing objective indicators
│   ├── 08_modelingShortage.R                         # Mixed-effects for shortages
│   ├── 09_modelingInequity.R                         # Mixed & ordinal models for inequity
│   └── 10_SEMShortage.R                              # Structural equation modeling
│
├── Aboudrar-Méda_Armande_Thesis_DS.Rmd              # Main RMarkdown document
├── config.R                                          # Loads packages, paths, and helpers
│
├── Docs/
│   ├── Dictionary.xlsx                               # Final variable dictionary
│   └── My_Final_Variables_Dictionary.xlsx            # Raw annotated dictionary
│
├── ThesisReplicationSet.Rproj                        # RStudio project file
└── README.md                                         # This file
```

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
  `broom.mixed`, `kableExtra`, `clusterCrit`

### Installation Instructions

1. **Clone the Repository:**
   - Run the following command in your terminal:
     ```bash
     git clone https://github.com/ArmandeAM/ThesisReplicationSet.git
     ```
   - Alternatively, download and extract the ZIP archive.

2. **Open the Project in RStudio:**
   - Open the `ThesisReplicationSet.Rproj` file. This ensures here::here() resolves correctly.

3. **Run the Configuration Script:**
   - In the R console, run:
     ```r
     source("config.R")
     ```
     
     This will install (if needed) and load all required packages and set up
     the directory paths.

4. **Knit the Analysis Document:**
   - Open the RMarkdown file located at `Scripts/Aboudrar-Méda_Armande_Thesis_DS.Rmd` in RStudio.
   - Click the **Knit** button to generate the full analysis report.
The document is organised into sections covering:
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
| 1  | `barometre2000_2022_diff.csv`              | **DREES Barometer** – annual face-to-face opinion survey (quota method), individual-level |    2000‑2022 (uses 2021 wave) | DREES | <https://drees2-sgsocialgouv.opendatasoft.com/explore/dataset/431_le-barometre-d-opinion/information/> | Open License v2.0 (Etalab) – “Source: DREES, Baromètre d’opinion” |**Data/barometre/**|
| 2  | `departements‑20140306‑5m.shp`             | Metropolitan French department polygons (EPSG2154, simplified 5m)           | 2014       | OpenStreetMap contributors (via data.gouv.fr) | <https://www.data.gouv.fr/fr/datasets/contours-des-departements-francais-issus-d-openstreetmap/#/resources/ab21d892-aa39-466a-a3ed-dc25ff076b78> |  ODbL – “© les contributeurs d’OpenStreetMap sous licence ODbL” ([osm.org/copyright](http://osm.org/copyright)) |**Data/departements-20140306-5m-shp/**|
| 3  | `ZEAT_Reg_Dep.csv`                         | Mapping from departments to ZEAT regions (NUTS-1 equivalent)                                 | 2024       | Author (compiled manually from INSEE and Wikipedia sources) | [INSEE](https://www.insee.fr/fr/metadonnees/definition/c1910), [Wikipedia](https://fr.wikipedia.org/wiki/Zone_d%27%C3%A9tudes_et_d%27am%C3%A9nagement_du_territoire)  | Public Domain |**Data/...**|
| 4  | `v_commune_2024.csv`                       | Mapping **CommunesToZEAT** (2024 administrative boundaries)| 2024       | INSEE|[INSEE – Fichier GEOFLA Communes 2024]<https://www.insee.fr/fr/information/7766585>|Public – Open License (Etalab) |**Data/...**|
| 5  | `lits‑de‑reanimation‑*.csv`                | Number of critical care beds (ICU, intensive care, continuous monitoring) by department         | 2023 (used 2021)       | DREES – SAE statistical bases |  [data.drees] <https://data.drees.solidarites-sante.gouv.fr/explore/dataset/lits-de-reanimation-de-soins-intensifs-et-de-surveillance-continue-en-france0/information/> | Etalab Open License v2.0 – “Source: DREES, SAE statistical bases” |**Data/...**|
| 6  | `demographieexercicesliberaux.csv`         | Self-employed healthcare professionals by type of practice (exclusive/mixed) and by department or region | 2010–2023 (uses 2021) | CNAM – Caisse nationale de l'Assurance Maladie | [data.ameli.fr – Demographie des exercices libéraux] <https://data.ameli.fr/explore/dataset/demographie-exercices-liberaux/information/?disjunctive.region&disjunctive.departement> | ODbL – “© les contributeurs, CNAM, sous licence ODbL” |**Data/...**|
| 7  | `honoraires.csv`                           | Fee amounts of self-employed healthcare professionals by department and region (total, excess, and average fees) |  2010–2023 (uses 2021) | CNAM – Caisse nationale de l'Assurance Maladie | [data.ameli.fr – Honoraires des professionnels de santé] <https://data.ameli.fr/explore/dataset/honoraires/information/?disjunctive.region&disjunctive.departement> | ODbL – “© les contributeurs, CNAM, sous licence ODbL” |**Data/...**|
| 8  | `APL_*_2021.xlsx`                          | **APL** Localised Potential Accessibility (APL) indicators for first-line healthcare professionals (commune level) | 2021       | DREES – Direction de la recherche, des études, de l’évaluation et des statistiques |    [data.drees – Accessibilité potentielle localisée (APL)]<https://data.drees.solidarites-sante.gouv.fr/explore/dataset/530_l-accessibilite-potentielle-localisee-apl/information/> |  Etalab Open License v2.0 – “Source: DREES, APL indicators”  | **Data/APL/** subfolder |
| 9  | `popsdregagegender_2021_simple.csv`        | Population structure by ZEAT, age group, and gender (based on 2021 census, 2024 geography) | 2021       | INSEE – Institut national de la statistique et des études économiques  | [INSEE – Recensement de la population 2021]<https://www.insee.fr/fr/statistiques/8200787?geo=FE-1&sommaire=8200811> |   Etalab Open License v2.0 – “Source: INSEE, Recensement 2021” |**Data/...**|
| 10 | `dep_santé.csv`                            | Public healthcare expenditure by age and gender groups, national level | 2021       | DREES – Direction de la recherche, des études, de l’évaluation et des statistiques | [DREES – Dépenses de santé] <https://drees.shinyapps.io/depenses_et_rac/> |  Etalab Open License v2.0 – “Source: DREES, Health Expenditure App” |**Data/...**|

For a complete description of the main variables used in the analysis — including variable definitions, ... - please refer to the **Docs/** folder. The comprehensive variables dictionary (`Dictionary.xlsx`) gives a description of the main variables. 

### Contact

For any questions or further assistance with reproducing the analysis, please contact:

**Armande Aboudrar-Méda**  
Email: [223158@students.hertie-school.org]
