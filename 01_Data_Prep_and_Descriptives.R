# ==============================================================================
# Script 1: Data Preparation and Descriptive Statistics
# Description: Loads raw data, removes derived/leaky variables (e.g., post-treatment 
#              milk composition and redundant units), and generates Table 1.
# ==============================================================================

library(dplyr)
library(tidyr)

cat("Starting Data Preparation...\n")

# 1. Load Raw Data
# Note: Ensure the raw dataset is placed inside the 'data/' directory.

df_raw <- read.csv("data/Final_Merged_Data.csv")

# 2. Prevent Data Leakage & Collider Bias
# Removing milk components (fat, protein, etc.) as they are co-produced with milk yield.
# Removing 'milk_yield_l_day_cow' (mathematically tied to Y).
# Removing 'scc_cells_per_m_l' to prevent multicollinearity with their counterparts.

leakage_cols <- c("milk_yield_l_day_cow", "fat_percent", "snf_percent", 
                  "protein_percent", "salt_percent", "lactose_percent", "p_h", 
                  "scc_cells_per_m_l")

df_clean <- df_raw %>% select(-any_of(leakage_cols))

# Convert categorical and hierarchical variables to factors
df_clean$animal_id <- as.factor(df_clean$animal_id)
df_clean$genetic_group <- factor(df_clean$genetic_group, levels = c("Local", "HF50", "HF62.5", "HF75", "HF87.5"))
df_clean$thi_range <- factor(df_clean$thi_range, levels = c("T0", "T1", "T2"))

# Save cleaned dataset for subsequent modeling scripts
saveRDS(df_clean, "data/Cleaned_Data.rds")
write.csv(df_clean, "data/Cleaned_Data.csv", row.names = FALSE)
cat("Data cleaning completed. 'Cleaned_Data.csv' saved in data/ folder.\n")

# 3. Generate Descriptive Statistics (Table 1)
# Group by Genetic Group and THI Range
table1_descriptives <- df_clean %>%
  group_by(genetic_group, thi_range) %>%
  summarise(
    N_Observations = n(),
    Milk_Yield_kg = sprintf("%.2f ± %.2f", mean(milk_yield_y), sd(milk_yield_y)),
    DMI_kg = sprintf("%.2f ± %.2f", mean(dmi_kg), sd(dmi_kg)),
    SCC_100k = sprintf("%.2f ± %.2f", mean(scc_100k_d), sd(scc_100k_d)),
    Glucose_mmol_L = sprintf("%.2f ± %.2f", mean(glucose_mmol_l), sd(glucose_mmol_l)),
    Cortisol_mg_dL = sprintf("%.2f ± %.2f", mean(cortisol_mg_d_l), sd(cortisol_mg_d_l)),
    Rectal_Temp_F = sprintf("%.2f ± %.2f", mean(rectal_temp_f), sd(rectal_temp_f))
  ) %>%
  ungroup()

# Save Table 1
write.csv(table1_descriptives, "outputs/Table1_Descriptive_Statistics.csv", row.names = FALSE)
cat("Table 1 generated and saved in outputs/ folder.\n")
