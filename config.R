# config.R: 

# 1. Package loading 
required_pkgs <- c(
  "here", "readxl", "dplyr", "tidyr", "readr", "writexl", "sf",
  "ggplot2", "viridis", "stringr", "knitr", "psych", "naniar",
  "REdaS", "GPArotation", "dagitty", "lavaan", "ordinal", "patchwork",
  "performance", "lme4", "car", "broom.mixed", "kableExtra", "clusterCrit", "cluster"
)
for(pkg in required_pkgs) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
  library(pkg, character.only = TRUE)
}

# 2. Directory paths
DATA_DIR       <- here("Data")
DOCS_DIR       <- here("Docs")
SHP_DIR        <- file.path(DATA_DIR, "departements-20140306-5m-shp")
BAROMETRE_DIR  <- file.path(DATA_DIR, "barometre")
APL_DIR        <- file.path(DATA_DIR, "APL")
# Others 
# LITS_DIR     <- file.path(DATA_DIR, "Lits")
# EFFECTIF_DIR <- file.path(DATA_DIR, "effectifact")
# THESIS_DS_DIR <- file.path(DATA_DIR, "thesisDS")

# 3. Helper functions

## Check CRS
ensure_crs <- function(sf_object, crs = 2154) {
  if (sf::st_crs(sf_object)$epsg != crs) {
    sf_object <- sf::st_transform(sf::st_as_sf(sf_object), crs = crs)
  }
  sf_object
}



