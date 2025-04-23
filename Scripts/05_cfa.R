# 5 Confirmatory Factor Analysis

# helper to run CFA models: prints fit measures & standardised loadings
run_and_summarize_cfa <- function(model_spec, data, ordered_vars, estimator = "WLSMV", ...) {
  fit <- cfa(model = model_spec, data = data, ordered = ordered_vars, estimator = estimator, ...)
  summary(fit, fit.measures = TRUE, standardized = TRUE)
  return(fit)
}

# ——— Healthcare Perceptions (2 factors) ———
# define latent structure: shortages vs inequities

model_health <- '
  # defining latent variables for Healthcare Perceptions
  healthShortage =~ sa09_1_recoded + sa09_2_recoded + sa09_3_recoded + sa09_4_recoded + sa09_5_recoded + sa09_6_recoded
  healthInequity =~ sa06_2_recoded + sa06_4_recoded + sa06_5_recoded + sa06_6_recoded
'

# fit and summarise the 2-factor healthcare model
fit_health <- run_and_summarize_cfa(model_health, 
                                    data = Barometre_2021, 
                                    ordered_vars = c("sa09_1_recoded", "sa09_2_recoded", "sa09_3_recoded", 
                                                     "sa09_4_recoded", "sa09_5_recoded", "sa09_6_recoded",
                                                     "sa06_2_recoded", "sa06_4_recoded", "sa06_5_recoded", "sa06_6_recoded"))
# ——— Healthcare Perceptions (split inequity) ———
# split inequity into income vs location 

model_health_3items2 <- '
  # defining latent variables for Healthcare Perceptions
  healthShortage =~ sa09_1_recoded + sa09_2_recoded + sa09_3_recoded + sa09_4_recoded + sa09_5_recoded + sa09_6_recoded
  healthInequityIncome =~ sa06_2_recoded + sa06_5_recoded 
  healthInequityLocation =~ sa06_4_recoded + sa06_6_recoded
'
fit_health3 <- run_and_summarize_cfa(model_health_3items2, 
                                     data = Barometre_2021, 
                                     ordered_vars = c("sa09_1_recoded", "sa09_2_recoded", "sa09_3_recoded", 
                                                      "sa09_4_recoded", "sa09_5_recoded", "sa09_6_recoded",
                                                      "sa06_2_recoded", "sa06_4_recoded", "sa06_5_recoded", "sa06_6_recoded"))

# ——— Trust (3 factors) ———
# separate trust into Medical/Scientific, Political/Journalistic, Online/SocialMedia
model_trust_3factor <- '
  # Factor 1: Medical/Scientific
  trustMedSci =~ sa13_1_recoded + sa13_2_recoded
  
  # Factor 2: Political/Public
  trustPolPub =~ sa13_3_recoded + sa13_4_recoded
  
  # Factor 3: Online/Social
  trustOnline =~ sa13_5_recoded + sa13_6_recoded
'
fit_trust_3f <- run_and_summarize_cfa(model_trust_3factor,
                                      data = Barometre_2021,
                                      ordered_vars = c("sa13_1_recoded", "sa13_2_recoded", 
                                                       "sa13_3_recoded", "sa13_4_recoded",
                                                       "sa13_5_recoded", "sa13_6_recoded"))

# ——— Socio-Political Attitudes ———
# test both 3‐factor and 2‐factor solutions
model_socio3 <- ' # (Family/Unemployment, SocialProtection, Health/Pensions)
  # Factor 1: Family & Unemployment
  familyHealth =~ ps01_1_recoded + ps01_3_recoded
  
  # Factor 2: State Role & Funding
  SocialProtection =~ og07_recoded + ps02_recoded + ps03_recoded + cs18_recoded
  
  # Factor 3: Health & Pensions
  UnemploymentPensions =~ ps01_2_recoded + ps01_4_recoded
'
fit_socio3 <- run_and_summarize_cfa(model_socio3,
                                    data = Barometre_2021,
                                    ordered_vars = c("ps01_1_recoded", "ps01_2_recoded", 
                                                     "ps01_3_recoded", "ps01_4_recoded", 
                                                     "og07_recoded", "ps02_recoded", 
                                                     "ps03_recoded", "cs18_recoded"))

model_socio2 <- ' # (Benefits, SocialProtection)
  # Factor 1: Benefits 
  Benefits =~ ps01_1_recoded + ps01_3_recoded + ps01_2_recoded + ps01_4_recoded
  
  # Factor 2: State Role & Funding
  SocialProtection =~ og07_recoded + ps02_recoded + ps03_recoded + cs18_recoded
'
fit_socio2 <- run_and_summarize_cfa(model_socio2,
                                    data = Barometre_2021,
                                    ordered_vars = c("ps01_1_recoded", "ps01_2_recoded", 
                                                     "ps01_3_recoded", "ps01_4_recoded", 
                                                     "og07_recoded", "ps02_recoded", 
                                                     "ps03_recoded", "cs18_recoded"))


# Labeling the factors to clearer names
Barometre_2021 <- Barometre_2021 %>% 
  rename(
    healthShortage = health_MR1,           # Perceptions of shortages
    healthInequity = health_MR2,           # Perceptions of inequities 
    trustOnline = trust_MR1,               # Trust in websites/blogs & social media
    trustMedSci = trust_MR2,               # Trust in doctors/researchers
    trustPolPub = trust_MR3,               # Trust in Politicians/Journalists
    familyHealth = sociopol3_MR3,          # Family allowances & Health insurance
    SocialProtection2 = sociopol3_MR2,     # State intervention and funding (if 3 factors)
    UnemploymentPensions = sociopol3_MR1,  # Unemployment benefits & pensions 
    benefits = sociopol2_MR1,              # Benefits composite
    SocialProtection = sociopol2_MR2,      # State intervention and funding (if 2 factors)
    Inequalities = inequa_MR1              # Trend of inequalities
  )

# alternative inequality index: average of the two items, then z-score

Barometre_2021$inequality_trend <- rowMeans(
  Barometre_2021[, c("in02_recoded", "in03_recoded")],
  na.rm = TRUE
)

Barometre_2021 <- Barometre_2021 %>%
  mutate(inequality_trend = as.numeric(scale(inequality_trend)))
