# 2. Pre-processing 

recode_nsp <- function(x, nsp_codes) {
  ifelse(x %in% nsp_codes, NA, x)
} # replace specified "don't know"/"other" response codes with NA 

# Defining NSP codes for each variable that should be recoded to NA
nsp_map <- list(
  # SA06: Equality/Fairness in Care items 
  sa06_2 = 3, sa06_4 = 3, sa06_5 = 3, sa06_6 = 3, 
  
  # SA09: Local shortages / Availability items
  sa09_1 = 4, sa09_2 = 4, sa09_3 = 4, sa09_4 = 4, sa09_5 = 4, sa09_6 = 4, 
  
  # SA13: Trust items  
  sa13_1 = 5, sa13_2 = 5, sa13_3 = 5, sa13_4 = 5, sa13_5 = 5, sa13_6 = 5,
  
  # Other items 
  og07 = 4,
  ps01_1 = 5, ps01_2 = 5, ps01_3 = 5, ps01_4 = 5, 
  ps02 = 4,
  ps03 = 4, 
  cs18 = 4, 
  og01 = 5, 
  og03_1 = 5, og03_2 = 5,
  sdpoltr = c(6,7),
  sdnat = 4, 
  sdpnaistr = 6,
  in01 = 3, 
  in04 = 10, 
  in05 = 10, 
  sddipl = c(9,10)
)

# Applying recoding for all variables defined in nsp_map 
Barometre_2021 <- Barometre_2021 %>%
  mutate(across(
    .cols = all_of(names(nsp_map)),
    .fns = ~ recode_nsp(.x, nsp_codes = nsp_map[[cur_column()]]),
    .names = "{.col}_recoded" # suffix recoded variables with “_recoded” 
  )) %>%
  mutate(
    # Reverse or modified coding for selected items 
    in02_recoded = case_when(
      in02 == 1 ~ 3,  # Reverse code for higher = stronger inequalities
      in02 == 2 ~ 1,
      in02 == 3 ~ 2,
      in02 == 4 ~ NA_real_, # treat code “4” as missing
      TRUE ~ NA_real_
    ),
    in03_recoded = case_when(
      in03 == 1 ~ 3,  # Reverse code for higher = stronger inequalities
      in03 == 2 ~ 1,
      in03 == 3 ~ 2,
      in03 == 4 ~ NA_real_, # treat code “4” as missing
      TRUE ~ NA_real_
    ),
    sa13_1_recoded = case_when( # reverse trust scale: 1→4, 4→1
      sa13_1 == 1 ~ 4,  # Reverse code for higher = more trust
      sa13_1 == 2 ~ 3,
      sa13_1 == 3 ~ 2,
      sa13_1 == 4 ~ 1,
      sa13_1 == 5 ~ NA_real_, # recode NSP (5) to NA
      TRUE ~ NA_real_
    ),
    sa13_2_recoded = case_when(
      sa13_2 == 1 ~ 4,  # Reverse code for higher = more trust
      sa13_2 == 2 ~ 3,
      sa13_2 == 3 ~ 2,
      sa13_2 == 4 ~ 1,
      sa13_2 == 5 ~ NA_real_, # recode NSP (5) to NA
      TRUE ~ NA_real_
    ),
    sa13_3_recoded = case_when(
      sa13_3 == 1 ~ 4,  # Reverse code for higher = more trust
      sa13_3 == 2 ~ 3,
      sa13_3 == 3 ~ 2,
      sa13_3 == 4 ~ 1,
      sa13_3 == 5 ~ NA_real_, # recode NSP (5) to NA
      TRUE ~ NA_real_
    ), 
    sa13_4_recoded = case_when(
      sa13_4 == 1 ~ 4,  # Reverse code for higher = more trust
      sa13_4 == 2 ~ 3,
      sa13_4 == 3 ~ 2,
      sa13_4 == 4 ~ 1,
      sa13_4 == 5 ~ NA_real_, # recode NSP (5) to NA
      TRUE ~ NA_real_
    ),
    sa13_5_recoded = case_when(
      sa13_5 == 1 ~ 4,  # Reverse code for higher = more trust
      sa13_5 == 2 ~ 3,
      sa13_5 == 3 ~ 2,
      sa13_5 == 4 ~ 1,
      sa13_5 == 5 ~ NA_real_, # recode NSP (5) to NA
      TRUE ~ NA_real_ 
    ),
    sa13_6_recoded = case_when(
      sa13_6 == 1 ~ 4,  # Reverse code for higher = more trust
      sa13_6 == 2 ~ 3,
      sa13_6 == 3 ~ 2,
      sa13_6 == 4 ~ 1,
      sa13_6 == 5 ~ NA_real_, # recode NSP (5) to NA
      TRUE ~ NA_real_ 
    ),
    ps02_recoded = case_when(
      ps02_recoded == 1 ~ 3,  # Reverse code for higher = more social protection
      ps02_recoded == 2 ~ 1, 
      ps02_recoded == 3 ~ 2,  
      TRUE ~ ps02_recoded    
    ), 
    ps01_1_recoded = case_when(
      ps01_1_recoded == 1 ~ 1, # Reverse code for higher = more social protection
      ps01_1_recoded == 2 ~ 2,  
      ps01_1_recoded == 3 ~ 4,  
      ps01_1_recoded == 4 ~ 3,
      TRUE ~ ps01_1_recoded  
    ),
    ps01_2_recoded = case_when(
      ps01_2_recoded == 1 ~ 1,  # Reverse code for higher = more social protection
      ps01_2_recoded == 2 ~ 2,  
      ps01_2_recoded == 3 ~ 4,  
      ps01_2_recoded == 4 ~ 3,
      TRUE ~ ps01_2_recoded  
    ),
    ps01_3_recoded = case_when(
      ps01_3_recoded == 1 ~ 1,   # Reverse code for higher = more social protection
      ps01_3_recoded == 2 ~ 2,  
      ps01_3_recoded == 3 ~ 4,  
      ps01_3_recoded == 4 ~ 3,
      TRUE ~ ps01_3_recoded  
    ), 
    ps01_4_recoded = case_when(
      ps01_4_recoded == 1 ~ 1,   # Reverse code for higher = more social protection
      ps01_4_recoded == 2 ~ 2,  
      ps01_4_recoded == 3 ~ 4,  
      ps01_4_recoded == 4 ~ 3,
      TRUE ~ ps01_4_recoded  
    )
  )
