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

# Additional check:

# Final Model Comparison — test random intercept by LRT (REML=FALSE)
model_lmmInequa_ML <- lmer(
  healthInequity ~ trustMedSci_index_std + trustPolPub_index_std + trustOnline_index_std + 
    benefits_all_index_std + socialProtection_index_std + 
    healthcare_availability_index_scaled +
    healthcare_cost_index_scaled + habitat + sdnivie + sddipl + sdpcs7 +
    (1 | sdreg),
  data = Barometre_2021_final_nomiss,
  REML = FALSE
)

model_Inequa_noRE <- lm(
  healthInequity ~ trustMedSci_index_std + trustPolPub_index_std + trustOnline_index_std + 
    benefits_all_index_std + socialProtection_index_std + 
    healthcare_availability_index_scaled +
    healthcare_cost_index_scaled + habitat + sdnivie + sddipl + sdpcs7,
  data = Barometre_2021_final_nomiss
)

# extract log‐likelihood of the model without random intercept
ll_noRE_Ineq    <- logLik(model_Inequa_noRE)      # 'lm' object
# extract log‐likelihood of the model with random intercept
ll_withRE_Ineq <- logLik(model_lmmInequa_ML)   # 'lmerMod' object

# compute likelihood‐ratio test statistic (difference in deviance)
D2 <- -2 * (as.numeric(ll_noRE_Ineq) - as.numeric(ll_withRE_Ineq))

# degrees of freedom difference = number of additional parameters (random intercept = 1)
df_diff <- 1

# compute p‐value from chi‐square distribution to test if random intercept adds significant improvement
p_val2 <- pchisq(D2, df_diff, lower.tail = FALSE)


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
