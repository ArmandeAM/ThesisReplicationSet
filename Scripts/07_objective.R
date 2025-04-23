# 7. Objective data 

# ------------------------------
# STEP 1: Process Population and Expenditure Data
# ------------------------------

# 7.1.1 Load population by ZEAT, age, and gender
df_zeat_age_gender <- read.csv(file.path(DATA_DIR, "popsdregagegender_2021_simple.csv"),
                               sep = ";", header = TRUE)

# reshape to long: one row per ZEAT × sex × age_class
pop_sdreg_long <- df_zeat_age_gender %>%
  pivot_longer(
    cols = c("Hommes_0_19", "Hommes_20_39", "Hommes_40_59", "Hommes_60_74", "Hommes_75more",
             "Femmes_0_19", "Femmes_20_39", "Femmes_40_59", "Femmes_60_74", "Femmes_75more"),
    names_to = c("sex", "age_class"),
    names_pattern = "(Hommes|Femmes)_(.*)",
    values_to = "population"
  ) %>%  # standardise age_class labels
  mutate(
    age_class = dplyr::recode(age_class,
                              "0_19" = "0-19",
                              "20_39" = "20-39",
                              "40_59" = "40-59",
                              "60_74" = "60-74",
                              "75more" = "75+")
  )

# 7.1.2 Load health expenditure and clean up codes & age bins (dep_santé CSV in Data folder)
dep_sante <- read.csv(file.path(DATA_DIR, "dep_santé.csv"),
                      sep = ";", header = TRUE) %>%
  mutate(Moyenne = as.numeric(gsub(",", ".", Moyenne))) %>% # convert comma decimals to numeric
  filter(sexe %in% c(1, 2), classe_age_10 != "Ensemble", ald != "Ensemble") %>% # keep only explicit sex and non‐aggregate age/ald groups
  mutate(
    age_class = case_when( # map 10‐year bins into our 5 categories
      classe_age_10 %in% c("00 - 10 ans", "11 - 20 ans") ~ "0-19",
      classe_age_10 %in% c("21 - 30 ans", "31 - 40 ans") ~ "20-39",
      classe_age_10 %in% c("41 - 50 ans", "51 - 60 ans") ~ "40-59",
      classe_age_10 %in% c("61 - 70 ans") ~ "60-74",
      classe_age_10 %in% c("71 - 80 ans", "plus de 80 ans") ~ "75+",
      TRUE ~ NA_character_
    )
  ) %>%
  mutate(sex = ifelse(sexe == 1, "Hommes", "Femmes")) # recode sex 

# 7.1.3 Compute mean expenditure per sex x age cell
dep_sante_simplifie <- dep_sante %>%
  group_by(sex, age_class) %>%
  summarise(Moyenne = mean(Moyenne, na.rm = TRUE), .groups = "drop")

# 7.1.4 Join population and expenditure
pop_sdreg_long <- pop_sdreg_long %>%
  left_join(dep_sante_simplifie, by = c("sex", "age_class")) %>%
  mutate(
    population = parse_number(gsub("[^0-9\\.-]", "", population)) # strip non‐numeric chars and parse as number
  )

# 7.1.5 National weighted average expenditure per person
national_avg <- sum(pop_sdreg_long$population * pop_sdreg_long$Moyenne, na.rm = TRUE) /
  sum(pop_sdreg_long$population, na.rm = TRUE)

# 7.1.6 Relative weight = cell mean / national average
pop_sdreg_long <- pop_sdreg_long %>%
  mutate(relative_weight = Moyenne / national_avg)

# 7.1.7 Standardised population per ZEAT using relative weights
pop_sdreg_standardized <- pop_sdreg_long %>%
  group_by(sdreg) %>%
  summarise(pop_std = sum(population * relative_weight, na.rm = TRUE), .groups = "drop")


# ------------------------------
# STEP 2: Load and Process Additional Objective Data
# ------------------------------

# 7.2.1 Load bed data, split out public vs. private
ResourcesBeds <- read_csv(file.path(DATA_DIR, "lits-de-reanimation-de-soins-intensifs-et-de-surveillance-continue-en-france0.csv")) %>%
  separate(col = `id_geo;statut_etab;annee;uni;nb_lit`,
           into = c("id_geo", "statut_etab", "annee", "uni", "nb_lit"),
           sep = ";") %>%
  mutate(nb_lit = as.numeric(nb_lit)) %>%
  mutate(statut_etab_type = case_when(   # classify establishment type
    statut_etab == "1_Public" ~ "public",
    statut_etab %in% c("2_Prive_BL", "3_Prive_BNL") ~ "private",
    TRUE ~ statut_etab
  ))

# 7.2.1b Filter to 2021, extract department code
ResourcesBeds2021 <- ResourcesBeds %>%
  filter(annee == 2021) %>%
  mutate(code_insee = sub("_.*", "", id_geo))

zeat_reg_dep_geo_subset <- zeat_reg_dep_geo %>%
  select(code_insee, sdreg = ZEAT_Nb, geometry)

ResourcesBeds2021geo <- ResourcesBeds2021 %>%
  left_join(zeat_reg_dep_geo_subset, by = "code_insee") %>%
  filter(!is.na(sdreg))

# 7.2.1c Sum beds by ZEAT and public/private
beds_sum <- ResourcesBeds2021geo %>%
  group_by(sdreg) %>%
  summarise(
    nb_lit_public = sum(nb_lit[statut_etab_type == "public"], na.rm = TRUE),
    nb_lit_private = sum(nb_lit[statut_etab_type == "private"], na.rm = TRUE),
    .groups = "drop"
  )

# 7.2.2 Load practitioner counts, restrict to Métropole departments (exclude overseas)
effectifs <- read.csv(file.path(DATA_DIR, "demographieexercicesliberaux.csv"),
                      sep = ";", header = TRUE) %>%
  filter(X.annee == 2021) %>%
  filter(as.numeric(departement) >= 1 & as.numeric(departement) <= 95)

effectifs_zeat <- effectifs %>%
  left_join(zeat_reg_dep %>% select(code_insee, ZEAT_Nb), by = c("departement" = "code_insee")) %>%
  rename(sdreg = ZEAT_Nb)

# join to ZEAT and aggregate by profession
total_effectifs_zeat <- effectifs_zeat %>%
  group_by(sdreg, profession_sante) %>%
  summarise(total_effectif = sum(effectif, na.rm = TRUE), .groups = "drop")

# widen so each profession is a separate column
effectifs_wide <- total_effectifs_zeat %>%
  pivot_wider(names_from = profession_sante, values_from = total_effectif)

# 7.2.3 Load honoraires (fees), filter, convert and summarise fees per ZEAT
honoraires <- read.csv(file.path(DATA_DIR, "honoraires.csv"),
                       sep = ";", header = TRUE) %>%
  filter(X.annee == 2021) %>%
  filter(as.numeric(departement) >= 1 & as.numeric(departement) <= 95)

honoraires_zeat <- honoraires %>%
  left_join(zeat_reg_dep %>% select(code_insee, ZEAT_Nb), by = c("departement" = "code_insee")) %>%
  rename(sdreg = ZEAT_Nb) %>%
  mutate_at(vars(hono_sans_depassement_totaux, depassements_totaux,
                 hono_sans_depassement_moyens, depassements_moyens), as.numeric)

honoraires_summary <- honoraires_zeat %>%
  group_by(sdreg, profession_sante) %>%
  summarise(
    total_hono_sans_depassement = sum(hono_sans_depassement_totaux, na.rm = TRUE),
    total_depassements = sum(depassements_totaux, na.rm = TRUE),
    avg_hono_sans_depassement = mean(hono_sans_depassement_moyens, na.rm = TRUE),
    avg_depassements = mean(depassements_moyens, na.rm = TRUE),
    .groups = "drop"
  )

honoraires_summary_wide <- honoraires_summary %>%
  pivot_wider(
    names_from = profession_sante,
    values_from = c(total_hono_sans_depassement, total_depassements,
                    avg_hono_sans_depassement, avg_depassements)
  )

# 7.2.4 Helper to process each APL Excel file: join commune codes, keep key cols
# These files are stored in the APL subfolder.
process_APL_file <- function(file_path) {
  read_xlsx(file_path) %>%
    left_join(v_commune, by = c("Code commune" = "COM")) %>%
    select(`Code commune`, `Libellé de la commune`, DEP, everything())
}

APL_mg_2021 <- process_APL_file(file.path(APL_DIR, "APL_mg_2021.xlsx"))
APL_inf_2021 <- process_APL_file(file.path(APL_DIR, "APL_inf_2021.xlsx"))
APL_mk_2021 <- process_APL_file(file.path(APL_DIR, "APL_mk_2021.xlsx"))
APL_sf_2021 <- process_APL_file(file.path(APL_DIR, "APL_sf_2021.xlsx"))
APL_cd_2021 <- process_APL_file(file.path(APL_DIR, "APL_cd_2021.xlsx"))

# Weighted APL per ZEAT: join on DEP and compute consumption-weighted mean
compute_weighted_APL <- function(zeat_data, apl_data, apl_col, new_name) {
  zeat_data %>%
    rename(sdreg = ZEAT_Nb) %>%
    left_join(apl_data, by = c("code_insee" = "DEP")) %>%
    group_by(sdreg) %>%
    summarise(weighted_APL = sum(!!sym(apl_col) * `Population standardisée par la consommation de soins par tranche d'âge`, na.rm = TRUE) /
                sum(`Population standardisée par la consommation de soins par tranche d'âge`, na.rm = TRUE),
              .groups = "drop") %>%
    ungroup() %>%
    filter(!is.na(weighted_APL)) %>%
    rename(!!new_name := weighted_APL)
}

APL_mg_2021_geo <- compute_weighted_APL(zeat_reg_dep, APL_mg_2021,
                                        "APL aux médecins généralistes (sans borne d'âge)", "WeightedAPL_mg")
APL_inf_2021_geo <- compute_weighted_APL(zeat_reg_dep, APL_inf_2021,
                                         "APL 2021 aux infirmières de 65 ans et moins", "WeightedAPL_inf")
APL_mk_2021_geo <- compute_weighted_APL(zeat_reg_dep, APL_mk_2021,
                                        "APL 2021 aux masseuses-kinésithérapeutes de 65 ans et moins", "WeightedAPL_mk")
APL_sf_2021_geo <- compute_weighted_APL(zeat_reg_dep, APL_sf_2021,
                                        "APL 2021 aux sages-femmes", "WeightedAPL_sf")
APL_cd_2021_geo <- compute_weighted_APL(zeat_reg_dep, APL_cd_2021,
                                        "APL 2021 aux chirurgiens-dentistes de 65 ans et moins", "WeightedAPL_cd")

APL_final_2021 <- APL_mg_2021_geo %>%
  left_join(APL_inf_2021_geo, by = "sdreg") %>%
  left_join(APL_mk_2021_geo,  by = "sdreg") %>%
  left_join(APL_sf_2021_geo,  by = "sdreg") %>%
  left_join(APL_cd_2021_geo,  by = "sdreg")


# ------------------------------
# STEP 3: Merge All Objective Datasets
# ------------------------------

# 7.3.1 Merge all objective data sets by ZEAT (sdreg)
merge_objective_data <- function(pop_std, beds, apl, effectifs, honoraires) {
  pop_std <- pop_std %>% mutate(sdreg = as.character(sdreg))
  merged_data <- pop_std %>%
    left_join(beds, by = "sdreg") %>%
    left_join(apl, by = "sdreg") %>%
    left_join(effectifs, by = "sdreg") %>%
    left_join(honoraires, by = "sdreg")
  return(merged_data)
}

merged_data <- merge_objective_data(pop_sdreg_standardized, beds_sum, APL_final_2021,
                                    effectifs_wide, honoraires_summary_wide)

# ------------------------------
# 4: select just the key columns for modeling and combine dentist categories
# ------------------------------

prepare_merged_data_small <- function(merged_data) {
  merged_small <- merged_data %>%
    select(
      sdreg, pop_std, nb_lit_public, nb_lit_private,
      WeightedAPL_mg, WeightedAPL_inf, WeightedAPL_mk, WeightedAPL_sf, WeightedAPL_cd,
      `Ensemble des médecins`, `Ensemble des médecins généralistes`,
      `Ensemble des médecins spécialistes (hors généralistes)`, Infirmiers,
      `Masseurs-kinésithérapeutes`, `Sages-femmes`,
      `Chirurgiens-dentistes (hors spécialistes d'orthopédie dento-faciale - ODF)`,
      `Chirurgiens-dentistes spécialistes d'orthopédie dento-faciale (ODF)`,
      `total_hono_sans_depassement_Ensemble des médecins`,
      `total_hono_sans_depassement_Ensemble des médecins généralistes`,
      `total_hono_sans_depassement_Ensemble des médecins spécialistes (hors généralistes)`,
      `total_hono_sans_depassement_Infirmiers`,
      `total_hono_sans_depassement_Masseurs-kinésithérapeutes`,
      `total_hono_sans_depassement_Sages-femmes`,
      `total_hono_sans_depassement_Chirurgiens-dentistes (hors spécialistes d'orthopédie dento-faciale - ODF)`,
      `total_hono_sans_depassement_Chirurgiens-dentistes spécialistes d'orthopédie dento-faciale (ODF)`,
      `total_depassements_Ensemble des médecins`,
      `total_depassements_Ensemble des médecins généralistes`,
      `total_depassements_Ensemble des médecins spécialistes (hors généralistes)`,
      `total_depassements_Infirmiers`,
      `total_depassements_Masseurs-kinésithérapeutes`,
      `total_depassements_Sages-femmes`,
      `total_depassements_Chirurgiens-dentistes (hors spécialistes d'orthopédie dento-faciale - ODF)`,
      `total_depassements_Chirurgiens-dentistes spécialistes d'orthopédie dento-faciale (ODF)`,
      `total_depassements_Ensemble des médecins`
    ) %>%
    mutate(
      # Combine dentist columns
      `Chirurgiens-dentistes` =
        `Chirurgiens-dentistes (hors spécialistes d'orthopédie dento-faciale - ODF)` +
        `Chirurgiens-dentistes spécialistes d'orthopédie dento-faciale (ODF)`,
      total_hono_sans_depassement_Chirurgiens_dentistes =
        `total_hono_sans_depassement_Chirurgiens-dentistes (hors spécialistes d'orthopédie dento-faciale - ODF)` +
        `total_hono_sans_depassement_Chirurgiens-dentistes spécialistes d'orthopédie dento-faciale (ODF)`,
      total_depassements_Chirurgiens_dentistes =
        `total_depassements_Chirurgiens-dentistes (hors spécialistes d'orthopédie dento-faciale - ODF)` +
        `total_depassements_Chirurgiens-dentistes spécialistes d'orthopédie dento-faciale (ODF)`
    )
  return(merged_small)
}

merged_data_small <- prepare_merged_data_small(merged_data)

# ------------------------------
# STEP 5: Compute per-100k rates for beds & practitioners, per capita per fees (with and without extra fees)
# ------------------------------

standardize_per100k <- function(data) {
  data %>%
    mutate(
      nb_lit_public_per_100k  = (nb_lit_public / pop_std) * 100000,
      nb_lit_private_per_100k = (nb_lit_private / pop_std) * 100000,
      medecins_per_100k        = (`Ensemble des médecins` / pop_std) * 100000,
      generalistes_per_100k    = (`Ensemble des médecins généralistes` / pop_std) * 100000,
      specialistes_per_100k    = (`Ensemble des médecins spécialistes (hors généralistes)` / pop_std) * 100000,
      infirmiers_per_100k      = (Infirmiers / pop_std) * 100000,
      kines_per_100k           = (`Masseurs-kinésithérapeutes` / pop_std) * 100000,
      sages_femmes_per_100k    = (`Sages-femmes` / pop_std) * 100000,
      chirurgiens_dentistes_per_100k = (`Chirurgiens-dentistes` / pop_std) * 100000,
      total_hono_sans_depassement_per_capita = `total_hono_sans_depassement_Ensemble des médecins` / pop_std,
      total_depassements_per_capita = `total_depassements_Ensemble des médecins` / pop_std
    ) %>%
    select(
      sdreg, pop_std, nb_lit_public_per_100k, nb_lit_private_per_100k,
      WeightedAPL_mg, WeightedAPL_inf, WeightedAPL_mk, WeightedAPL_sf, WeightedAPL_cd,
      medecins_per_100k, generalistes_per_100k, specialistes_per_100k,
      infirmiers_per_100k, kines_per_100k, sages_femmes_per_100k,
      chirurgiens_dentistes_per_100k,
      total_hono_sans_depassement_per_capita, total_depassements_per_capita
    )
}

merged_per100000 <- standardize_per100k(merged_data_small)

# ------------------------------
# STEP 6: Join to ZEAT spatial geometry for mapping
# ------------------------------

merged_map <- left_join(zeat_geometry, merged_per100000, by = c("ZEAT_Nb" = "sdreg"))
