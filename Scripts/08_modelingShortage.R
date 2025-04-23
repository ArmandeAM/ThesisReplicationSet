# 8. Modeling Shortage

# 8.1 Compute health-care availability and cost indices
merged_per100000 <- merged_per100000 %>%
  mutate(
    # average number of providers per 100k across six profession types
    healthcare_availability_index = (
      generalistes_per_100k +
        specialistes_per_100k +
        infirmiers_per_100k +
        kines_per_100k +
        sages_femmes_per_100k +
        chirurgiens_dentistes_per_100k
    ) / 6,
    # use total out-of-pocket spending per capita as cost index
    healthcare_cost_index = total_depassements_per_capita,
    # z-score standardise both indices for modeling
    healthcare_availability_index_scaled = scale(healthcare_availability_index),
    healthcare_cost_index_scaled = scale(healthcare_cost_index)
  )

# 8.1 Join standardised indices back into the survey data by region
indices_df <- merged_per100000 %>%
  select(sdreg, healthcare_availability_index_scaled, healthcare_cost_index_scaled)

Barometre_2021_final <- Barometre_2021 %>%
  left_join(indices_df, by = "sdreg")

# 8.1 Different specifications 

# Model 1: three indices for trust, two whole variables for socio-political attitudes 

Barometre_2021_final <- Barometre_2021_final %>% 
  mutate(
    # trust indices: simple averages of pairs of items (from the EFA/CFA)
    trustMedSci_index = (sa13_1_recoded + sa13_2_recoded) / 2,
    trustPolPub_index = (sa13_3_recoded + sa13_4_recoded) / 2,
    trustOnline_index = (sa13_5_recoded + sa13_6_recoded) / 2,
    # standardise
    trustMedSci_index_std = as.numeric(scale(trustMedSci_index)),
    trustPolPub_index_std = as.numeric(scale(trustPolPub_index)),
    trustOnline_index_std = as.numeric(scale(trustOnline_index)),
    # single-item socio-political, standardised
    socio_attitude1_std   = as.numeric(scale(ps01_1_recoded)),
    socio_attitude2_std   = as.numeric(scale(ps03_recoded)),
    inequality_trend_scaled  = as.numeric(scale(inequality_trend)), # already computed inequality_trend, now standardise
    # convert categorical controls to factors with specified reference levels
    habitat   = relevel(as.factor(habitat), ref = "1"), # rural as reference 
    sdnivie   = relevel(as.factor(sdnivie), ref = "1"),    # first quintile as reference 
    sddipl    = relevel(as.factor(sddipl_recoded), ref = "1"), # lowest diploma as reference 
    sdpcs7    = as.factor(sdpcs7), 
    sdreg     = as.factor(sdreg)
  ) 

# Model 2: creating indices for socio-political attitudes, based on the EFA 
Barometre_2021_final <- Barometre_2021_final %>%
  mutate(
    # Creating indices
    familyHealth_index = (ps01_1_recoded + ps01_3_recoded) / 2,
    socialProtection_index = (og07_recoded + ps02_recoded + ps03_recoded + cs18_recoded) / 4,
    unemploymentPensions_index = (ps01_2_recoded + ps01_4_recoded) / 2,
    # Standardising indices 
    familyHealth_index_std = as.numeric(scale(familyHealth_index)),
    socialProtection_index_std = as.numeric(scale(socialProtection_index)),
    unemploymentPensions_index_std = as.numeric(scale(unemploymentPensions_index))
  )

# Model 3: all the benefits index together, besides the health one to study it alone 
Barometre_2021_final <- Barometre_2021_final %>%
  mutate(
    benefits_index = (ps01_2_recoded + ps01_3_recoded + ps01_4_recoded) / 3,
    # standardising 
    benefits_index_std = as.numeric(scale(benefits_index))
  )

# Final model: all the benefits together 
Barometre_2021_final <- Barometre_2021_final %>%
  mutate(
    benefits_all_index = (ps01_1_recoded + ps01_2_recoded + ps01_3_recoded + ps01_4_recoded) / 4,
    # standardising
    benefits_all_index_std = as.numeric(scale(benefits_all_index))
  )

# Defining the complete case dataset for modeling using only needed predictors
model_vars <- c(
  "healthShortage", "trustMedSci_index_std", "trustPolPub_index_std", "trustOnline_index_std",
  "benefits_all_index_std", "socialProtection_index_std", "inequality_trend_scaled",
  "healthcare_availability_index_scaled", "healthcare_cost_index_scaled",
  "habitat", "sdnivie", "sddipl", "sdpcs7", "sdreg"
)

# Subset the rows with no NA for these variables, but retain all columns
Barometre_2021_final_nomiss <- Barometre_2021_final[complete.cases(Barometre_2021_final[, model_vars]), ]

# Model number 1: random intercept for region, trust + socio-political + controls 

model_lmm <- lmer(
  healthShortage ~ trustMedSci_index_std + trustPolPub_index_std + trustOnline_index_std +
    socio_attitude1_std + socio_attitude2_std +
    inequality_trend_scaled + healthcare_availability_index_scaled +
    healthcare_cost_index_scaled + habitat + sdnivie + sddipl + sdpcs7 +
    (1 | sdreg),
  data = Barometre_2021_final_nomiss
)

# Checks Model 1 
# ----------------------------
# Model 1 diagnostics: pairwise correlations among fixed predictors
fixed_vars <- Barometre_2021_final_nomiss %>% 
  select(trustMedSci_index_std, trustPolPub_index_std, trustOnline_index_std, 
         socio_attitude1_std, socio_attitude2_std, inequality_trend_scaled,
         healthcare_availability_index_scaled, healthcare_cost_index_scaled)
cor_matrix <- cor(fixed_vars, use = "pairwise.complete.obs")
print(round(cor_matrix, 2))

# Compute VIFs via an OLS proxy to check multicollinearity
lm_model <- lm(healthShortage ~ trustMedSci_index_std + trustPolPub_index_std + trustOnline_index_std +
                 socio_attitude1_std + socio_attitude2_std + inequality_trend_scaled +
                 healthcare_availability_index_scaled + healthcare_cost_index_scaled +
                 habitat + sdnivie + sddipl + sdpcs7,
               data = Barometre_2021_final_nomiss)
vifs <- vif(lm_model)

# Nested model comparison (REML=FALSE) for testing inequality_trend_scaled necessity
df_used <- Barometre_2021_final_nomiss

model_lmm_full <- lmer(
  healthShortage ~ trustMedSci_index_std + trustPolPub_index_std + trustOnline_index_std +
    socio_attitude1_std + socio_attitude2_std + inequality_trend_scaled +
    healthcare_availability_index_scaled + healthcare_cost_index_scaled + habitat +
    sdnivie + sddipl + sdpcs7 + (1 | sdreg),
  data = df_used,
  REML = FALSE
)

model_lmm_simple <- lmer(
  healthShortage ~ trustMedSci_index_std + trustPolPub_index_std + trustOnline_index_std +
    socio_attitude1_std + socio_attitude2_std +
    healthcare_availability_index_scaled + healthcare_cost_index_scaled + habitat +
    sdnivie + sddipl + sdpcs7 + (1 | sdreg),
  data = df_used,
  REML = FALSE
)


# Model number 2: random intercept for region, trust indices + socio-political indices + controls 

model_lmm2 <- lmer(
  healthShortage ~ trustMedSci_index_std + trustPolPub_index_std + trustOnline_index_std + 
    familyHealth_index_std + socialProtection_index_std + unemploymentPensions_index_std + 
    healthcare_availability_index_scaled +
    healthcare_cost_index_scaled + habitat + sdnivie + sddipl + sdpcs7 +
    (1 | sdreg),
  data = Barometre_2021_final_nomiss
)

# Checks Model 2
# ----------------------------
# Model 2 diagnostics: pairwise correlations among fixed predictors
fixed_vars <- Barometre_2021_final_nomiss %>% 
  select(trustMedSci_index_std, trustPolPub_index_std, trustOnline_index_std, 
         familyHealth_index_std, socialProtection_index_std, unemploymentPensions_index_std, 
         healthcare_availability_index_scaled, healthcare_cost_index_scaled)
cor_matrix <- cor(fixed_vars, use = "pairwise.complete.obs")
print(round(cor_matrix, 2))

# Model 2 Diagnostics — 2) VIF via OLS proxy to assess multicollinearity
lm_model2 <- lm(healthShortage ~ trustMedSci_index_std + trustPolPub_index_std + trustOnline_index_std +
                  familyHealth_index_std + socialProtection_index_std + unemploymentPensions_index_std +
                  healthcare_availability_index_scaled + healthcare_cost_index_scaled +
                  habitat + sdnivie + sddipl + sdpcs7,
                data = Barometre_2021_final_nomiss)
vifs <- vif(lm_model2)

# Model 3: random intercept for region with benefits_index_std 

model_lmm3 <- lmer(
  healthShortage ~ trustMedSci_index_std + trustPolPub_index_std + trustOnline_index_std + 
    benefits_index_std + socialProtection_index_std + socio_attitude1_std + 
    + healthcare_availability_index_scaled +
    healthcare_cost_index_scaled + habitat + sdnivie + sddipl + sdpcs7 +
    (1 | sdreg),
  data = Barometre_2021_final_nomiss
)

# Checks Model 3 
# ----------------------------
# Model 3 diagnostics: pairwise correlations among fixed predictors
fixed_vars <- Barometre_2021_final_nomiss %>% 
  select(trustMedSci_index_std, trustPolPub_index_std, trustOnline_index_std, 
         benefits_index_std, socialProtection_index_std, socio_attitude1_std, 
         healthcare_availability_index_scaled, healthcare_cost_index_scaled)
cor_matrix <- cor(fixed_vars, use = "pairwise.complete.obs")
print(round(cor_matrix, 2))

# Model 3 Diagnostics — 2) VIF via OLS to check multicollinearity
lm_model3 <- lm(healthShortage ~ trustMedSci_index_std + trustPolPub_index_std + trustOnline_index_std +
                  benefits_index_std + socialProtection_index_std + socio_attitude1_std +
                  healthcare_availability_index_scaled + healthcare_cost_index_scaled +
                  habitat + sdnivie + sddipl + sdpcs7,
                data = Barometre_2021_final_nomiss)
vifs <- vif(lm_model3)

# Final Model 4: all‐benefits index + socialProtection + (1 | sdreg)

model_lmm4 <- lmer(
  healthShortage ~ trustMedSci_index_std + trustPolPub_index_std + trustOnline_index_std + 
    benefits_all_index_std + socialProtection_index_std + 
    healthcare_availability_index_scaled +
    healthcare_cost_index_scaled + habitat + sdnivie + sddipl + sdpcs7 +
    (1 | sdreg),
  data = Barometre_2021_final_nomiss
)

summary(model_lmm4)

# Checks Model 4
# ----------------------------
# Model 4 diagnostics — Outlier/Influence: approximate via leverage from OLS
lm_approx <- lm(healthShortage ~ trustMedSci_index_std + trustPolPub_index_std + 
                  trustOnline_index_std + benefits_all_index_std + 
                  socialProtection_index_std +
                  healthcare_availability_index_scaled + healthcare_cost_index_scaled,
                data = Barometre_2021_final_nomiss) # use hatvalues(lm_approx) or influence.measures(lm_approx) to identify high‐leverage points

# Model 4 Diagnostics — Multicollinearity: pairwise correlations & VIF for final predictors
predictors <- Barometre_2021_final_nomiss[, c("trustMedSci_index_std", "trustPolPub_index_std",
                                              "trustOnline_index_std", "benefits_all_index_std",
                                              "socialProtection_index_std",
                                              "healthcare_availability_index_scaled", "healthcare_cost_index_scaled")]

round(cor(predictors, use = "pairwise.complete.obs"), 2)

lm_for_vif <- lm(
  healthShortage ~ trustMedSci_index_std + trustPolPub_index_std + trustOnline_index_std +
    benefits_all_index_std + socialProtection_index_std +
    healthcare_availability_index_scaled + healthcare_cost_index_scaled,
  data = Barometre_2021_final_nomiss
)

# Additional check:

# Final Model Comparison — test random intercept by LRT (REML=FALSE)
model_lmm4_ML <- lmer(
  healthShortage ~ trustMedSci_index_std + trustPolPub_index_std + trustOnline_index_std + 
    benefits_all_index_std + socialProtection_index_std + 
    healthcare_availability_index_scaled +
    healthcare_cost_index_scaled + habitat + sdnivie + sddipl + sdpcs7 +
    (1 | sdreg),
  data = Barometre_2021_final,
  REML = FALSE
)

model_noRE <- lm(
  healthShortage ~ trustMedSci_index_std + trustPolPub_index_std + trustOnline_index_std + 
    benefits_all_index_std + socialProtection_index_std + 
    healthcare_availability_index_scaled +
    healthcare_cost_index_scaled + habitat + sdnivie + sddipl + sdpcs7,
  data = Barometre_2021_final
)

# extract log‐likelihood of the model without random intercept
ll_noRE    <- logLik(model_noRE)      # 'lm' object
# extract log‐likelihood of the model with random intercept
ll_withRE  <- logLik(model_lmm4_ML)   # 'lmerMod' object

# compute likelihood‐ratio test statistic (difference in deviance)
D <- -2 * (as.numeric(ll_noRE) - as.numeric(ll_withRE))

# degrees of freedom difference = number of additional parameters (random intercept = 1)
df_diff <- 1

# compute p‐value from chi‐square distribution to test if random intercept adds significant improvement
p_val <- pchisq(D, df_diff, lower.tail = FALSE)

