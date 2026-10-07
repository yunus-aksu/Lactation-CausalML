# ==============================================================================
# Script: 00_data_merging.R
# Description: Reads raw Excel files, corrects entry errors (ID offsets, typos), 
#              safely merges them using relational joins, and engineers 
#              features for downstream DML causal analyses.
# ==============================================================================

library(readxl)
library(dplyr)
library(stringr)
library(janitor) 


cat("\n🚀 STARTING RAW DATA INTEGRATION...\n")

# --- 1. LOAD RAW DATA AND STANDARIZE TYPES ---
# Note: Ensure the 3 raw Excel files downloaded from Mendeley are placed inside the 'data/' directory.

# Explicitly converting ID columns to numeric to prevent join mismatches

df_blood <- read_excel("data/Blood metabolites.xlsx") %>%
  mutate(`Animal ID` = as.numeric(as.character(`Animal ID`)))

df_dmi <- read_excel("data/DMI, milk yield and composition.xlsx") %>%
  mutate(`Animal ID` = as.numeric(as.character(`Animal ID`)))

df_physio <- read_excel("data/physiological responses.xlsx")

# --- 2. DATA CLEANING AND ERROR CORRECTION (PHYSIO DATASET) ---
df_physio_clean <- df_physio %>%
  # Fix Animal ID offset issue in physiological dataset
  mutate(`Animal ID` = as.numeric(as.character(`Animal ID`)) + 1) %>%
  
  # Standardize column naming
  rename(`Genetic Group` = `Genetic group`) %>%
  
  # Clean inconsistent entries (e.g., HF_50 to HF50) and extract only THI category (T0, T1, T2)
  mutate(
    `Genetic Group` = str_replace_all(`Genetic Group`, "_", ""),
    `THI Range` = str_extract(`THI Range`, "^T[0-9]")
  )

# Fix potential trailing spaces in the DMI dataset column names
df_dmi <- df_dmi %>%
  rename_with(~ str_trim(.), .cols = everything())

# --- 3. RELATIONAL JOINS ---
join_keys <- c("Animal ID", "Genetic Group", "THI Range", "Replication No")

# Ensure THI Range matching by extracting clean codes for blood and DMI datasets as well
df_blood <- df_blood %>% mutate(`THI Range` = str_extract(str_to_upper(`THI Range`), "T[0-9]"))
df_dmi <- df_dmi %>% mutate(`THI Range` = str_extract(str_to_upper(`THI Range`), "T[0-9]"))


df_merged <- df_dmi %>%
  inner_join(df_blood, by = join_keys) %>%
  inner_join(df_physio_clean, by = join_keys)

# --- 4. STANDARDIZE COLUMN NAMES TO SNAKE_CASE ---
df_merged_clean <- df_merged %>%
  clean_names()

# --- 5. FEATURE ENGINEERING FOR CAUSAL INFERENCE (DML) ---
df_final <- df_merged_clean %>%
  mutate(
    thi_range = toupper(thi_range),
    genetic_group = str_to_title(genetic_group),
    genetic_group = str_replace_all(genetic_group, "Hf", "HF"),
    
    # Unit Transformation: Convert Liters to Kilograms using specific gravity (1.032)
    # This aligns the outcome variable with the mass-based input (DMI kg)
    milk_yield_y = milk_yield_l_day_cow * 1.032,
    
    # Scale Somatic Cell Count for stable parameter estimation
    scc_100k_d = scc_cells_per_m_l / 100000
  )

# --- 6. EXPORT MASTER DATASET ---
write.csv(df_final, "data/Final_Merged_Data.csv", row.names = FALSE)

cat("✅ Data merging successfully completed. 'Final_Merged_Data.csv' is saved.\n")
cat("Total Rows:", nrow(df_final), "\n")
cat("Total Columns:", ncol(df_final), "\n")
