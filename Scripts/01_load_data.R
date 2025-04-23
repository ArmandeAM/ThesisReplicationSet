# 1. Subjective Data Loading

# (1) Loading geographic data

# Read departments shapefile (excluding overseas, DOM/TOM)
departments <- sf::st_read(file.path(SHP_DIR, "departements-20140306-5m.shp")) %>%
  dplyr::filter(!code_insee %in% c("971","972","973","974","976")) %>% # exclude overseas DOM/TOM departments
  dplyr::select(code_insee, geometry) # keep only the department code and its geometry

# Table of correspondence ZEAT <-> département
zeat_reg_dep <- readr::read_csv(file.path(DATA_DIR, "ZEAT_Reg_Dep.csv")) %>%
  tidyr::separate(
    col  = `ZEAT_Nb;ZEAT_Name;Reg_Nb;Reg_Name;code_insee;Dep_Name`,
    into = c("ZEAT_Nb","ZEAT_Name","Reg_Nb","Reg_Name","code_insee","Dep_Name"),
    sep  = ";"
  ) # split the semicolon-delimited correspondence table into separate fields 

# Communes (for APL data)
v_commune <- readr::read_csv(file.path(APL_DIR, "v_commune_2024.csv")) %>%
  dplyr::select(COM, DEP)

# Merge ZEAT and department data (with geometry)
zeat_reg_dep_geo <- zeat_reg_dep %>%
  dplyr::left_join(departments, by = "code_insee") # attach spatial geometry 

# Convert to sf and aggregate geometries by ZEAT_Nb
zeat_reg_dep_geo <- sf::st_as_sf(zeat_reg_dep_geo) # ensure the data frame is an sf object

zeat_geometry <- zeat_reg_dep_geo %>%
  dplyr::filter(!st_is_empty(geometry)) %>% # drop any rows without valid geometries
  dplyr::group_by(ZEAT_Nb) %>% # group by ZEAT region 
  dplyr::summarise(geometry = st_union(geometry)) %>% # merge all department geometries into one per ZEAT
  ungroup() %>%
  ensure_crs(2154)  # reproject to Lambert-93 (EPSG:2154)

# (2) Loading Barometer Data

# Read the Barometer CSV: BAROMETRE_DIR
Barometre_2021 <- read.csv(
  file.path(BAROMETRE_DIR, "barometre2000_2022_diff.csv"),
  sep    = ";",
  header = TRUE
) %>%
  dplyr::select(ident, annee, poids, habitat, sdreg, sdsexe, sdage, sdagetr, sdsitua, sdstat, sdpcs7,
         sdpcs10, sdprsitua, sdpract, sdprstat, sdprpcs7, sdprpcs10, og01, og03_1, og03_2, 
         og04_1, og04_3, og04_4, og04_5, og04_6, og04_7, og04_8, og04_9, og05_1, og05_4, og05_5, 
         og05_6, og05_7, og05_8, og07, og13bis_ac_1, og13bis_ac_2, og13bis_ac_3, og13bis_ac_4, 
         og13bis_ac_5, og13bis_ac_6, og13bis_ac_7, og13bis_ac_8, og13bis_ac_9, og13bis_ac_10, 
         og13bis_ac_11, og13bis_ac_12, og13ter_ac_1, og13ter_ac_2, og13ter_ac_3, og13ter_ac_4, 
         og13ter_ac_5, og13ter_ac_6, og13ter_ac_7, og13ter_ac_8, og13ter_ac_9, og13ter_ac_10, 
         og13ter_ac_11, og13ter_ac_12, og13_b_1, og13_b_2, og13_b_3, og13_b_4, og13_b_5, og13_b_6, 
         og13_b_7, og13_b_8, og13_b_9, og13_b_10, og13_b_11, og13_b_12,
         og13bis_b_1, og13bis_b_2, og13bis_b_3, og13bis_b_4, og13bis_b_5, og13bis_b_6, og13bis_b_7, 
         og13bis_b_8, og13bis_b_9, og13bis_b_10, og13bis_b_11, og13bis_b_12, og13ter_b_1, og13ter_b_2, 
         og13ter_b_3, og13ter_b_4, og13ter_b_5, og13ter_b_6, og13ter_b_7, og13ter_b_8, og13ter_b_9, 
         og13ter_b_10, og13ter_b_11, og13ter_b_12, og13_d_1, og13_d_2, og13_d_3, og13_d_4, og13_d_5, 
         og13_d_6, og13_d_7, og13_d_8, og13_d_10, og13_d_11, og13_d_12, og13bis_d_1, og13bis_d_2, 
         og13bis_d_3, og13bis_d_4, og13bis_d_5, og13bis_d_6, og13bis_d_7, og13bis_d_8, og13bis_d_10, 
         og13bis_d_11, og13bis_d_12, og13ter_d_1, og13ter_d_2, og13ter_d_3, og13ter_d_4, og13ter_d_5, 
         og13ter_d_6, og13ter_d_7, og13ter_d_8, og13ter_d_10, og13ter_d_11, og13ter_d_12, in01, in02, 
         in03, in04, in05, in08, in09, pe15, pe18, ps01_1, ps01_2, ps01_3, ps01_4,
         ps02, ps03, ps13_ab_1, ps13_ab_2, ps13_ab_3, ps13_ab_4, ps13_ab_5, ps13_ab_6, 
         ps13_ab_7, ps13_cd_1, ps13_cd_2, ps13_cd_3, ps13_cd_4, ps13_cd_5, ps13_cd_6, 
         ps13_cd_7, ps15_1, ps15_2, ps15_3, ps18, sa01, sa03_det, sa03, sa04, sa05, 
         sa06_2, sa06_4, sa06_5, sa06_6, sa06_7, sa06_8, sa06_9, sa06_10, sa07_ac_2, 
         sa07_ac_5, sa07_ac_6, sa07_ac_7, sa07_ac_9, sa07_bd_2, sa07_bd_5, sa07_bd_6, 
         sa07_bd_7, sa07_bd_9, sa27, sa09_1, sa09_2, sa09_3, sa09_4, sa09_5, sa09_6,
         sa13_1, sa13_2, sa13_3, sa13_4, sa13_5, sa13_6, sa22_a_2, sa22_a_5, sa22_a_6, 
         sa22_a_7, sa22_a_9, sa22_c_2, sa22_c_5, sa22_c_6, sa22_c_7, sa22_c_9, sa22_bd_2, 
         sa22_bd_5, sa22_bd_6, sa22_bd_7, sa22_bd_9, cs18, cs19_1, cs19_2, sdmut_ab, sdmut_cd, 
         sdnivie, sddipl_det, sddipl, sdpol, sdpoltr, sdnat, sdpnaistr
         ) %>%
  dplyr::mutate(sdreg = as.character(sdreg)) %>%
  dplyr::left_join(zeat_geometry, by = c("sdreg" = "ZEAT_Nb")) %>% # merge Barometer data with the ZEAT spatial polygons
  dplyr::filter(annee == 2021) # keep only the 2021 wave (4,002 obs × 225 vars)
