### ==============================================================================
### Script 2: Correlation and Variance Inflation Factor (VIF) Diagnostics
### ==============================================================================
library(dplyr)
library(car)
library(stats)

cat("Starting Correlation and VIF Diagnostics...\n")

# 1. Load Cleaned Data
df <- readRDS("data/Cleaned_Data.rds")

# 2. Isolate Numeric Predictors for Correlation (excluding IDs and design variables)
numeric_vars <- df %>% select(where(is.numeric), -replication_no, -animal_id)

# Compute Pearson Correlation Matrix
cor_matrix <- cor(numeric_vars, use = "complete.obs", method = "pearson")
write.csv(round(cor_matrix, 3), "outputs/Correlation_Matrix.csv", row.names = TRUE)
cat("Correlation matrix calculated and saved to outputs/.\n")

# 3. VIF (Multicollinearity) Diagnosis
cat("Running VIF Diagnostics...\n")
vif_model <- lm(milk_yield_y ~ genetic_group + thi_range + dmi_kg + scc_100k_d + 
                  glucose_mmol_l + total_protein_g_d_l + uric_acid_mg_d_l + 
                  cholesterol_mg_d_l + calcium_mg_d_l + hdl_mg_d_l + ast_u_i + 
                  alt_u_i + cortisol_mg_d_l + rectal_temp_f + pulse_rate_bpm + 
                  respiration_rate_bpm, data = df)

vif_values <- car::vif(vif_model)

if(is.matrix(vif_values)) {
  vif_df <- data.frame(
    Feature = rownames(vif_values),
    GVIF = round(vif_values[, 1], 2),
    DF = vif_values[, 2],
    Adjusted_VIF = round(vif_values[, 3]^2, 2)
  )
} else {
  vif_df <- data.frame(
    Feature = names(vif_values),
    VIF = round(vif_values, 2)
  )
}

write.csv(vif_df, "outputs/VIF_Diagnostics.csv", row.names = FALSE)
cat("VIF diagnostics completed and saved.\n")
