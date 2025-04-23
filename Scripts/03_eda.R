# 3. Exploratory data analysis 

# 1) SA06: Inequity items overall 

sa06_items <- c("sa06_2_recoded", "sa06_4_recoded",
                "sa06_5_recoded", "sa06_6_recoded")

sa06_table <- Barometre_2021 %>%
  select(all_of(sa06_items)) %>%
  pivot_longer(                    # reshape to long for item‐level summaries
    cols      = everything(),
    names_to  = "Item",            # store variable names in “Item”
    values_to = "Value"            # store responses in “Value”
  ) %>%
  group_by(Item) %>%
  summarise(
    prop_agree    = sum(Value == 1, na.rm = TRUE) / n(), # % who agree
    prop_disagree = sum(Value == 2, na.rm = TRUE) / n(), # % who disagree
    prop_NA       = sum(is.na(Value))      / n()         # % missing
  ) %>%
  ungroup() %>%
  mutate(
    Item_label = dplyr::recode(Item,
                        sa06_2_recoded = "Same quality of care (income)",
                        sa06_4_recoded = "Same quality of care (location)",
                        sa06_5_recoded = "Same waiting times (income)",
                        sa06_6_recoded = "Same waiting times (location)"
    ),
    prop_agree    = round(100 * prop_agree,    1),       # format as % with 1 decimal
    prop_disagree = round(100 * prop_disagree, 1), 
    prop_NA       = round(100 * prop_NA,       1)
  ) %>%
  select(Item_label, prop_agree, prop_disagree, prop_NA) # keep only final columns


# --- 2) SA06: by region ---
sa06_by_region <- Barometre_2021 %>%
  select(sdreg, all_of(sa06_items)) %>%
  group_by(sdreg) %>%
  summarise(across(
    all_of(sa06_items),
    ~ round(100 * mean(. == 1, na.rm = TRUE), 1),         # % agree per region
    .names = "perc_{.col}"
  )) %>%
  ungroup() %>%
  rename(
    `Same quality care (income)`   = perc_sa06_2_recoded, # rename for clarity
    `Same quality care (location)` = perc_sa06_4_recoded,
    `Same waiting times (income)`  = perc_sa06_5_recoded,
    `Same waiting times (location)`= perc_sa06_6_recoded
  )



# 3) SA09: accessibility items

sa09_items <- c(
  "sa09_1_recoded", # GPs
  "sa09_2_recoded", # Specialists
  "sa09_3_recoded", # Nurses
  "sa09_4_recoded", # Physio
  "sa09_5_recoded", # Dentists
  "sa09_6_recoded"  # Pharmacists
)

sa09_table <- Barometre_2021 %>%
  select(all_of(sa09_items)) %>%
  pivot_longer(  # long format for count summaries
    cols      = everything(),
    names_to  = "Item",
    values_to = "Value"
  ) %>%
  group_by(Item) %>%
  summarise(
    total          = n(),                            # total responses
    count_toomany   = sum(Value == 1, na.rm = TRUE), # count per category
    count_enough    = sum(Value == 2, na.rm = TRUE),
    count_notenough = sum(Value == 3, na.rm = TRUE),
    count_NA        = sum(is.na(Value))
  ) %>%
  ungroup() %>%
  mutate(
    prop_toomany   = round(100 * count_toomany   / total, 1), # % too many
    prop_enough    = round(100 * count_enough    / total, 1), # % enough
    prop_notenough = round(100 * count_notenough / total, 1), # % not enough 
    prop_NA        = round(100 * count_NA        / total, 1), # % missing
    Item_label = dplyr::recode(Item,                          # descriptive labels
                        sa09_1_recoded = "General Practitioners",
                        sa09_2_recoded = "Specialists",
                        sa09_3_recoded = "Nurses",
                        sa09_4_recoded = "Masseurs-Physio",
                        sa09_5_recoded = "Dentists",
                        sa09_6_recoded = "Pharmacists"
    )
  ) %>%
  select(Item_label, prop_toomany, prop_enough, prop_notenough, prop_NA)

# 4) SA09: by region 
sa09_table_sdreg <- Barometre_2021 %>%
  select(sdreg, all_of(sa09_items)) %>%
  pivot_longer(
    cols      = all_of(sa09_items),
    names_to  = "Item",
    values_to = "Value"
  ) %>%
  group_by(sdreg, Item) %>%
  summarise(
    total          = n(),
    count_toomany   = sum(Value == 1, na.rm = TRUE),
    count_enough    = sum(Value == 2, na.rm = TRUE),
    count_notenough = sum(Value == 3, na.rm = TRUE),
    count_NA        = sum(is.na(Value))
  ) %>%
  ungroup() %>%
  mutate(
    prop_toomany   = round(100 * count_toomany   / total, 1),
    prop_enough    = round(100 * count_enough    / total, 1),
    prop_notenough = round(100 * count_notenough / total, 1),
    prop_NA        = round(100 * count_NA        / total, 1),
    Item_label = dplyr::recode(Item,
                        sa09_1_recoded = "General Practitioners",
                        sa09_2_recoded = "Specialists",
                        sa09_3_recoded = "Nurses",
                        sa09_4_recoded = "Masseurs-Physio",
                        sa09_5_recoded = "Dentists",
                        sa09_6_recoded = "Pharmacists"
    )
  ) %>%
  select(sdreg, Item_label, prop_toomany, prop_enough, prop_notenough, prop_NA)

# 5) Pivot wide (for later joins and maps)
sa09_wide <- sa09_table_sdreg %>%
  pivot_wider(
    id_cols     = "sdreg",   # keep region as key
    names_from  = "Item_label",
    values_from = c("prop_toomany", "prop_enough", "prop_notenough", "prop_NA"),
    names_glue  = "{Item_label}_{.value}" # glue label+metric into name
  )
