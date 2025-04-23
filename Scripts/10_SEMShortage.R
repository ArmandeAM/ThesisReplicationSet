# 10. SEM Shortage

# 10.1 SEM with controls on full dataset, no clusters group
# Dummy‐code categorical controls for SEM
Barometre_2021_final <- Barometre_2021_final %>%
  # habitat: reference = “1” (Rural), create 4 dummies for levels 2–5
  mutate(
    habitat2_dummy = ifelse(habitat == "2", 1, 0),
    habitat3_dummy = ifelse(habitat == "3", 1, 0),
    habitat4_dummy = ifelse(habitat == "4", 1, 0),
    habitat5_dummy = ifelse(habitat == "5", 1, 0)
  ) %>%
  # socioeconomic quintile (sdnivie): ref = “1”, create dummies for 2–5
  mutate(
    sdnivie2_dummy = ifelse(sdnivie == "2", 1, 0),
    sdnivie3_dummy = ifelse(sdnivie == "3", 1, 0),
    sdnivie4_dummy = ifelse(sdnivie == "4", 1, 0),
    sdnivie5_dummy = ifelse(sdnivie == "5", 1, 0)
  ) %>%
  # education level (sddipl): ref = “1” (No diploma), create dummies for 2–8
  mutate(
    sddipl2_dummy = ifelse(sddipl == "2", 1, 0),
    sddipl3_dummy = ifelse(sddipl == "3", 1, 0),
    sddipl4_dummy = ifelse(sddipl == "4", 1, 0),
    sddipl5_dummy = ifelse(sddipl == "5", 1, 0),
    sddipl6_dummy = ifelse(sddipl == "6", 1, 0),
    sddipl7_dummy = ifelse(sddipl == "7", 1, 0),
    sddipl8_dummy = ifelse(sddipl == "8", 1, 0)
  ) %>%
  # socio-professional category (sdpcs7): ref = “1”, create 6 dummies for levels 2–7
  mutate(
    sdpcs72_dummy = ifelse(sdpcs7 == "2", 1, 0),
    sdpcs73_dummy = ifelse(sdpcs7 == "3", 1, 0),
    sdpcs74_dummy = ifelse(sdpcs7 == "4", 1, 0),
    sdpcs75_dummy = ifelse(sdpcs7 == "5", 1, 0),
    sdpcs76_dummy = ifelse(sdpcs7 == "6", 1, 0),
    sdpcs77_dummy = ifelse(sdpcs7 == "7", 1, 0)
  )

# 10.2 Define SEM measurement & structural model
model_full <- '
  # Measurement model: latent DV "f_healthShortage" measured by the SA09 items
  f_healthShortage =~ sa09_1_recoded + sa09_2_recoded + sa09_3_recoded +
                      sa09_4_recoded + sa09_5_recoded + sa09_6_recoded
  
  # Structural model: regress latent DV on predictors and controls
  f_healthShortage ~ trustMedSci_index_std + trustPolPub_index_std + trustOnline_index_std +
                     benefits_all_index_std + socialProtection_index_std
                     + healthcare_availability_index_scaled +
                     healthcare_cost_index_scaled +
                     habitat2_dummy + habitat3_dummy + habitat4_dummy + habitat5_dummy +
                     sdnivie2_dummy + sdnivie3_dummy + sdnivie4_dummy + sdnivie5_dummy +
                     sddipl2_dummy + sddipl3_dummy + sddipl4_dummy + sddipl5_dummy +
                     sddipl6_dummy + sddipl7_dummy + sddipl8_dummy +
                     sdpcs72_dummy + sdpcs73_dummy + sdpcs74_dummy + sdpcs75_dummy +
                     sdpcs76_dummy + sdpcs77_dummy
'

# 10.3 Run SEM with DWLS estimator for ordinal indicators
fit_sem <- sem(model_full, data = Barometre_2021_final,
               ordered = c("sa09_1_recoded", "sa09_2_recoded", "sa09_3_recoded",
                           "sa09_4_recoded", "sa09_5_recoded", "sa09_6_recoded"),
               estimator = "WLSMV")


# 10.4 Prepare data for multi‐group SEM by Ward‐k3 clusters
sem_data <- Barometre_2021_final %>%
  filter(!is.na(cluster_ward_k3)) # drop rows without cluster assignment

sem_data$cluster_ward_k3 <- factor(sem_data$cluster_ward_k3) # ensure grouping var is factor

# 10.5 Fit multi‐group SEM to compare across clusters
fit_mg <- sem(
  model_full,
  data      = sem_data,
  group     = "cluster_ward_k3",
  ordered   = c("sa09_1_recoded","sa09_2_recoded","sa09_3_recoded",
                "sa09_4_recoded","sa09_5_recoded","sa09_6_recoded"),
  estimator = "WLSMV"
)

summary(fit_mg, fit.measures = TRUE, standardized = TRUE) # display fit and standardised estimates