# 4. Exploratory Factor analysis 

# 4.1 Item Matrices

# Defining items Matrices
# Healthcare Perceptions Items Matrix
health_items <- data.frame(
  Item = c(
    # Equality (SA06 items)
    "sa06_2_recoded", "sa06_4_recoded", "sa06_5_recoded", "sa06_6_recoded",
    # Availability (SA09 items)
    "sa09_1_recoded", "sa09_2_recoded", "sa09_3_recoded", "sa09_4_recoded", "sa09_5_recoded", "sa09_6_recoded"
  ),
  Description = c(
    # SA06: Equality/Fairness in Care
    "Equality: care quality same, regardless of income",
    "Equality: care quality same, regardless of location",
    "Equality: waiting times same, regardless of income",
    "Equality: waiting times same, regardless of location",
    # SA09: Availability / local shortages
    "Availability: Are there enough general practitioners?",
    "Availability: Are there enough specialist doctors?",
    "Availability: Are there enough nurses?",
    "Availability: Are there enough masseurs-physiotherapists?",
    "Availability: Are there enough dentists?",
    "Availability: Are there enough pharmacists?"
  ),
  Subdomain = c(
    # For SA06 items
    rep("Equality", 4),
    # For SA09 items
    rep("Availability", 6)
  ),
  Domain = rep("Subjective Healthcare Perceptions", 4+6),
  Scale_Notes = c(
    # For SA06 items: ( 1 = Agree, 2 = Disagree)
    rep("Ordinal (1 = Agree, 2 = Disagree) [NSP->NA]", 4),
    # For SA09 items: categorical where 1 = Too many, 2 = Enough, 3 = Not enough
    rep("Ordinal (1=TooMany, 2=Enough, 3=NotEnough) [NSP->NA]", 6)
  ),
  stringsAsFactors = FALSE
)

# Trust Items Matrix
trust_items <- data.frame(
  Item = c(
    "sa13_1_recoded",  # Trust in doctors
    "sa13_2_recoded",  # Trust in researchers
    "sa13_3_recoded",  # Trust in journalists
    "sa13_4_recoded",  # Trust in Politicians
    "sa13_5_recoded",  # Trust in websites/blogs 
    "sa13_6_recoded"   # Trust in social media 
  ),
  Description = c(
    "Trust in Doctors",
    "Trust in Researchers",
    "Trust in Journalists",
    "Trust in Politicians",
    "Trust in websites/blogs",
    "Trust in social media"
  ),
  Subdomain = c(
    "Trust in Medical and Scientific Experts",
    "Trust in Medical and Scientific Experts",
    "Trust in Journalists/Politians",
    "Trust in Journalists/Politians",
    "Trust in Media and Online Sources",
    "Trust in Media and Online Sources"
  ),
  Domain = rep("Trust in Institutions and Sources", 6),
  Scale_Notes = rep("Ordinal (1=No trust at all, 4=Full trust) [NSP -> NA]", 6),
  stringsAsFactors = FALSE
)

# Socio Political Attitudes Items Matrix
socio_political_items <- data.frame(
  Item = c(
    "og07_recoded",             # State intervention opinion
    "ps01_1_recoded",           # Health insurance beneficiaries
    "ps01_2_recoded",           # Pensions beneficiaries 
    "ps01_3_recoded",           # Family allowances beneficiaries 
    "ps01_4_recoded",           # Unemployment benefits beneficiaries
    "ps02_recoded",             # Opinion on companies' contribution
    "ps03_recoded",             # Opinion on social protection spending
    "cs18_recoded"              # Opinion on state intervention for the deprived
  ),
  Description = c(
    "Opinion on state intervention in economic/social matters (e.g., 1=Too much, 2=Enough, 3=Not enough)",
    "Health insurance beneficiaries",
    "Pensions beneficiaries ",
    "Family allowances beneficiaries ",
    "Unemployment benefits beneficiaries",
    "Opinion on companies' contributions to social protection",
    "Opinion on whether social protection spending is excessive, normal, or insufficient",
    "Opinion: Public institutions do what they should"
  ),
  Subdomain = c(
    "State Role & Social Policy",
    rep("Social Policy Opinions", 4),
    rep("Social Policy Opinions", 2),
    rep("Cynicism / Trust in Social Protection", 1)
  ),
  Domain = rep("Trust and Socio‑Political Attitudes", 8),
  Scale_Notes = c(
    "Ordinal (e.g., 1=Too much, 2=Enough, 3=Not enough) [NSP -> NA]",
    rep("Categorical (1=Only those who contribute; 2=Only those who cannot help themselves; 3=More for contributors with a minimum protection; 4=For all) [NSP -> NA]", 4),
    "Ordinal (1=Less contribution, 2=No change, 3=More contribution) [NSP -> NA]",
    "Ordinal (1=Excessive, 2=Normal, 3=Insufficient) [NSP -> NA]",
    "Ordinal (1=Too much, 2=Just right, 3=Not enough) [NSP -> NA]"
  ),
  stringsAsFactors = FALSE
)

# Inequalities items matrix 
inequa_items <- data.frame(
  Item = c(
    "in02_recoded", # did the inequalities raised or decreased, 
    "in03_recoded"
  ),
  Description = c(
    "Evolution of inequalities",
    "Future Evolution of inequalities"
  ),
  Domain = rep("inequalities", 2),
  Scale_Notes = c(
    "Ordinal (1=decreases, 2=stayed stable, 3=increased)", 
    "Ordinal (e.g., 1=will decrease, 2=will be stable, 3=will increase)"
  ),
  stringsAsFactors = FALSE
)

# region related items matrix 
region_items <- data.frame(
  Item = c(
    "habitat", #urban, rural, inhb
    "sdreg" # ZEAT 
  ),
  Description = c(
    "Habitat", 
    "ZEAT"
  ),
  Domain = rep("region", 2),
  Scale_Notes = c(
    "Categorical (1=Rural, 2=<20,000, 3=20,000-99,999, 4=100,000, 5=parisianagglo)",
    "Categorical (1=Paris region, 2=Eastern Paris Basin, 3=Western Paris Basin, 4=Nord, 5=East, 6=West, 7=South-West,
    8=Centre-East, 9=Mediterranean"
  ),
  stringsAsFactors = FALSE
)

# 4.2 Missingness Check 
# For Healthcare Perceptions Items (Block A)
health_missing <- Barometre_2021 %>%
  select(all_of(health_items$Item)) %>%
  summarise(across(everything(), ~ mean(is.na(.)) * 100, .names = "NA_pct_{.col}")) # compute % missing for each health perception item

# For Trust Items (Block B)
trust_missing <- Barometre_2021 %>%
  select(all_of(trust_items$Item)) %>%
  summarise(across(everything(), ~ mean(is.na(.)) * 100, .names = "NA_pct_{.col}"))

# For Socio-Political Attitudes (Block C)
sociopol_missing <- Barometre_2021 %>%
  select(all_of(socio_political_items$Item)) %>%
  summarise(across(everything(), ~ mean(is.na(.)) * 100, .names = "NA_pct_{.col}"))

# For Inequalities (Block D)
inequalities_missing <- Barometre_2021 %>%
  select(all_of(inequa_items$Item)) %>%
  summarise(across(everything(), ~ mean(is.na(.)) * 100, .names = "NA_pct_{.col}"))

# 4.3 Polychoric Correlations 

# Selecting the items in my health matrix 
health_data <- Barometre_2021 %>% select(all_of(health_items$Item))
# Computing the polychoric correlation matrix for healthcare perceptions items
health_poly <- psych::polychoric(na.omit(health_data))$rho # polychoric corr matrix for health items

# Selecting the trust items from my trust items matrix 
trust_data <- Barometre_2021 %>% select(all_of(trust_items$Item))
# Computing the polychoric correlation matrix for trust
trust_poly <- psych::polychoric(na.omit(trust_data))$rho

# Selecting the socio-political items from the socio-political items matrix 
sociopol_data <- Barometre_2021 %>% select(all_of(socio_political_items$Item))
# Computing the polychoric correlation matrix socio-political items
sociopol_poly <- psych::polychoric(na.omit(sociopol_data))$rho

# Selecting the inequalities items from the inequalities matrix 
inequa_data <- Barometre_2021 %>% select(all_of(inequa_items$Item))
# Computing the polychoric correlation matrix for trust/socio-political items
inequa_poly <- psych::polychoric(na.omit(inequa_data))$rho

# 4.4 Scree plots

plot_scree <- function(cor_matrix, title) { # compute eigenvalues and plot factor number vs eigenvalue
  eigen_res <- eigen(cor_matrix)
  scree_df <- data.frame(Factor = 1:length(eigen_res$values),
                         Eigenvalue = eigen_res$values)
  ggplot(scree_df, aes(x = Factor, y = Eigenvalue)) +
    geom_line() + geom_point() +
    theme_minimal() +
    labs(title = title, x = "Factor Number", y = "Eigenvalue")
}

# 4.5 EFA on polychoric matrices: choosing the rotation method 

# Running EFAs on polychoric matrices (not on raw data, conversely to the step after) to inspect the factor correlations and choose between varimax and oblimin. 
# We would expect the health ones to be not so correlated: very different dimension: one on resources shortages, the other on inequality by income and location => varimax, and the other matrices to have very correlated items: oblimin 

# Factor Analysis for Healthcare Perceptions
health_fa_oblimin <- fa(health_poly, nfactors = 2, rotate = "oblimin")
print("Factor Analysis for Healthcare Perceptions:")
print(health_fa_oblimin) # inspect factor loadings under oblique rotation

# Factor Analysis for Trust
trust_fa_oblimin <- fa(trust_poly, nfactors = 3, rotate = "oblimin")
print("Factor Analysis for Trust:")
print(trust_fa_oblimin)

# Factor Analysis for Socio-Political Attitudes
sociopol_fa_oblimin <- fa(sociopol_poly, nfactors = 3, rotate = "oblimin") # Use 3 factors as suggested
print("Factor Analysis for Socio-Political Attitudes:")
print(sociopol_fa_oblimin)

# Factor Analysis for Socio-Political Attitudes: Sensitivity Check 
sociopol_fa_oblimin2 <- fa(sociopol_poly, nfactors = 2, rotate = "oblimin") # Use 2 factors as sensitivity check
print("Factor Analysis for Socio-Political Attitudes 2:")
print(sociopol_fa_oblimin2) # Better than with three factors because no ultra-Heywood case

# Factor Analysis for Inequa
inequa_fa_oblimin <- fa(inequa_poly, nfactors = 1, rotate = "oblimin") # Use 3 factors as suggested
print("Factor Analysis for Inequa:")
print(inequa_fa_oblimin)

# 4.6 Function to extract the factors on the raw data
extract_and_merge_fa <- function(data, item_vars, n_factors, rotation_method, prefix) {
  # 1) select 'ident' and the specified items
  subset_data <- data %>% select(ident, all_of(item_vars))
  
  # 2) subset to complete cases (ignoring the ident column)
  complete_data <- subset_data[complete.cases(subset_data[,-1]), ]
  
  # 3) run EFA on the item columns (excluding ident), using polychoric correlations and regression scores
  fa_final <- fa(complete_data[,-1],
                 nfactors = n_factors,
                 rotate = rotation_method,
                 cor = "poly",
                 scores = "regression")
  print(fa_final)
  
  # 4) convert the factor scores to a data frame
  scores_df <- as.data.frame(fa_final$scores)
  
  # 5) rename the factor score columns using a consistent prefix 
  colnames(scores_df) <- paste0(prefix, "_MR", 1:n_factors)
  
  # 6) reattach the 'ident' / merge back on 'ident'
  scores_df$ident <- complete_data$ident
  
  # 7) merge the factor scores back into the main dataset by 'ident'
  updated_data <- left_join(data, scores_df, by = "ident")
  
  # we return both the full factor analysis object and the updated dataset
  return(list(fa_final = fa_final, updated_data = updated_data))
}

health_result <- extract_and_merge_fa(Barometre_2021, health_items$Item, 2, "varimax", "health")
health_fa_final <- health_result$fa_final    
Barometre_2021 <- health_result$updated_data  

trust_result <- extract_and_merge_fa(Barometre_2021, trust_items$Item, 3, "oblimin", "trust")
trust_fa_final <- trust_result$fa_final
Barometre_2021 <- trust_result$updated_data

sociopol3_result <- extract_and_merge_fa(Barometre_2021, socio_political_items$Item, 3, "oblimin", "sociopol3")
sociopol_fa_final <- sociopol3_result$fa_final
Barometre_2021 <- sociopol3_result$updated_data

sociopol2_result <- extract_and_merge_fa(Barometre_2021, socio_political_items$Item, 2, "oblimin", "sociopol2")
sociopol_fa_final2 <- sociopol2_result$fa_final
Barometre_2021 <- sociopol2_result$updated_data

inequa_result <- extract_and_merge_fa(Barometre_2021, inequa_items$Item, 1, "oblimin", "inequa")
inequa_fa_final <- inequa_result$fa_final
Barometre_2021 <- inequa_result$updated_data

# 4.7 Comparison of extraction method for health perceptions items 

# Comparison of extraction methods
health_fa_minres <- fa(health_poly, nfactors = 2, rotate = "varimax", fm = "minres") # minimum residual
health_fa_pa     <- fa(health_poly, nfactors = 2, rotate = "varimax", fm = "pa")     # principal axis
health_fa_ml     <- fa(health_poly, nfactors = 2, rotate = "varimax", fm = "ml")     # maximum likelihood

# 4.8 Reliability of factors

# Function to calculate Cronbach's Alpha for each factor
calculate_alpha <- function(data, fa_result, factor_name) {
  # Convert loadings to a matrix and then to a data frame
  loadings <- as.data.frame(unclass(fa_result$loadings))
  
  # Identify items with significant loadings for the given factor
  factor_items <- rownames(loadings)[which(abs(loadings[[factor_name]]) > 0.3)] # extract items loading > .3 on factor_name, then compute Cronbach’s alpha
  
  # Subset the data to include only these items
  subset_data <- data[, factor_items]
  
  # Calculate Cronbach's Alpha
  alpha <- psych::alpha(subset_data)
  return(list(alpha_value = alpha$total$raw_alpha, items = factor_items))
}

# Healthcare Perceptions
health_alpha <- calculate_alpha(health_poly, health_fa_final, "MR1")
cat("Healthcare Perceptions Factor 1 Cronbach's Alpha:", health_alpha$alpha_value, "\n")

# Trust
trust_alpha <- calculate_alpha(trust_poly, trust_fa_final, "MR1")
cat("Trust Factor 1 Cronbach's Alpha:", trust_alpha$alpha_value, "\n")

# Socio-Political Attitudes 3 factors 
sociopol_alpha <- calculate_alpha(sociopol_poly, sociopol_fa_final, "MR1")
cat("Socio-Political Attitudes Factor 1 Cronbach's Alpha:", sociopol_alpha$alpha_value, "\n")

# Socio-Political Attitudes 2 factors 
sociopol_alpha2 <- calculate_alpha(sociopol_poly, sociopol_fa_final2, "MR1")
cat("Socio-Political Attitudes Factor 1 Cronbach's Alpha:", sociopol_alpha2$alpha_value, "\n")

# Visualize Cronbach's Alpha
visualize_alpha <- function(alpha_values, factor_labels, title) {
  df <- data.frame(Factor = factor_labels, CronbachAlpha = alpha_values)
  ggplot(df, aes(x = reorder(Factor, CronbachAlpha), y = CronbachAlpha, fill = CronbachAlpha)) +
    geom_bar(stat = "identity", color = "black") +
    theme_minimal() +
    labs(title = title, x = "Factors", y = "Cronbach's Alpha") +
    geom_text(aes(label = round(CronbachAlpha, 2)), vjust = -0.5) +
    theme(legend.position = "none")
}

# Combining alpha values for visualisation
alpha_values <- c(
  health_alpha$alpha_value,
  trust_alpha$alpha_value,
  sociopol_alpha$alpha_value, 
  sociopol_alpha2$alpha_value
)
factor_labels <- c("Healthcare Perceptions", "Trust", "Socio-Political Attitudes", "Socio-Political Attitudes 2 fac")

# 4.9 Distribution factors

# Function to create the individual plots and compute the normality test
plot_factor_distribution <- function(data, var_name, sample_size = 500, seed = 123) { # build histogram, density, boxplot; sample for Shapiro-Wilk normality test
  # Histogram
  p_hist <- ggplot(data, aes_string(x = var_name)) +
    geom_histogram(bins = 30, fill = "steelblue", color = "white") +
    theme_minimal() +
    labs(title = paste("Histogram of", var_name),
         x = var_name, y = "Count")
  
  # Density Plot
  p_density <- ggplot(data, aes_string(x = var_name)) +
    geom_density(fill = "lightblue", alpha = 0.5) +
    theme_minimal() +
    labs(title = paste("Density of", var_name),
         x = var_name, y = "Density")
  
  # Boxplot
  p_box <- ggplot(data, aes_string(y = var_name)) +
    geom_boxplot(fill = "lightgreen") +
    theme_minimal() +
    labs(title = paste("Boxplot of", var_name),
         y = var_name)
  
  # Normality Test on a Random Sample
  set.seed(seed)
  sample_vals <- sample(na.omit(data[[var_name]]), sample_size)
  shapiro_result <- shapiro.test(sample_vals)
  
  return(list(histogram = p_hist,
              density = p_density,
              boxplot = p_box,
              shapiro_test = shapiro_result))
}

# arrange the three plots side-by-side and print normality test 
display_factor_distribution <- function(data, var_name, sample_size = 500, seed = 123) {
  results <- plot_factor_distribution(data, var_name, sample_size, seed)
  
  # Arrange the three plots in one row using patchwork
  combined_plot <- results$histogram + results$density + results$boxplot + 
    plot_layout(ncol = 3)
  
  print(combined_plot)
  
  # Print the normality test result
  cat("\nShapiro-Wilk Normality Test for", var_name, ":\n")
  print(results$shapiro_test)
  
  invisible(results)
}

# Alternative Factors for Health Inequality perceptions: 3 factors

health_fa_oblimin_3fac <- psych::fa(health_poly, nfactors = 3, rotate = "oblimin")

health_fa_oblimin_3fac <- psych::fa(health_poly, nfactors = 3, rotate = "oblimin")

factor_correlation_health_3fac <- health_fa_oblimin_3fac$r.scores

# 1) Select ident + health items
health_data <- Barometre_2021 %>%
  select(ident, all_of(health_items$Item))

# 2) Subset to complete cases across these items
health_complete <- health_data[complete.cases(health_data[ , -1]), ]

# 3) Run EFA on item columns only (excluding ident),
#    specifying polychoric correlations + regression scores
health_fa_final_3fac <- fa(
  health_complete[ , -1],   # Only the item columns
  nfactors = 3,
  rotate = "varimax",
  cor = "poly",
  scores = "regression"
)

# 4) Convert factor scores to a data frame
health_scores_3fac <- as.data.frame(health_fa_final_3fac$scores)

# 5) Rename the factor score columns (so they don't clash with other domains)
#    e.g. "MR1" -> "health_MR1", "MR2" -> "health_MR2"
colnames(health_scores_3fac) <- c("health_MR1_3fac", "health_MR2_3fac", "health_MR3_3fac")

# 6) Reattach the same ident from health_complete
#    (the row order in $scores matches health_complete[ , -1])
health_scores_3fac$ident <- health_complete$ident

# 7) Merge once with the main data by ident
Barometre_2021 <- left_join(Barometre_2021, health_scores_3fac, by = "ident")