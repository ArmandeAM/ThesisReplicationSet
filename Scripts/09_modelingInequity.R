# 9. Modeling Inequity

# fit a linear mixed‐effects model 

model_lmmInequa <- lmer(
  healthInequity ~ trustMedSci_index_std + trustPolPub_index_std + trustOnline_index_std + 
    benefits_all_index_std + socialProtection_index_std + 
    inequality_trend_scaled + healthcare_availability_index_scaled +
    healthcare_cost_index_scaled + habitat + sdnivie + sddipl + sdpcs7 +
    (1 | sdreg), # random intercept for region
  data = Barometre_2021_final_nomiss
)

# create an ordered factor with three levels 

Barometre_2021_final_nomiss$healthInequity_cat <- cut(
  Barometre_2021_final_nomiss$healthInequity,
  breaks = c(-Inf, -0.5, 0.5, Inf), # thresholds for perceive equity/neutral/inequality
  labels = c("Perceive equity", "Neutral", "Perceive inequality"),
  ordered_result = TRUE
)

# check the decile distribution of the continuous score to justify chosen cutpoints
quantile(Barometre_2021_final_nomiss$healthInequity, probs = seq(0, 1, 0.1), na.rm = TRUE)

# fit a cumulative link mixed‐effects model for the ordinal inequity outcome
model_clmm <- clmm(
  healthInequity_cat ~ trustMedSci_index_std + trustPolPub_index_std + trustOnline_index_std +
    benefits_all_index_std + socialProtection_index_std +
    inequality_trend_scaled 
  + healthcare_availability_index_scaled +
    healthcare_cost_index_scaled + habitat + sdnivie + sddipl + sdpcs7 +
    (1 | sdreg),
  data = Barometre_2021_final_nomiss
)
