# ==============================================================================
# Script 5: Cluster-Robust Double Machine Learning & Causal Forests
# Description: Isolates the causal effect (ATE) for the Four Pillars with 95% CIs
#              and computes Heterogeneous Treatment Effects (CATE) using grf.
# ==============================================================================

library(DoubleML)
library(mlr3)
library(mlr3learners)
library(grf)
library(dplyr)
lgr::get_logger("mlr3")$set_threshold("warn")

cat("Starting Causal Inference (DML & Causal Forest)...\n")

# 1. Load Cleaned Data
df <- readRDS("data/Cleaned_Data.rds")

# Prepare Treatment Variables
# Create binary treatment for Heat Stress (T2 vs others) and Genetics (HF vs Local)
df$is_severe_hs <- ifelse(df$thi_range == "T2", 1, 0)
df$is_hf_d <- ifelse(df$genetic_group == "Local", 0, 1)
df$animal_id <- as.factor(df$animal_id)


df$hf_50   <- ifelse(df$genetic_group == "HF50", 1, 0)
df$hf_625  <- ifelse(df$genetic_group == "HF62.5", 1, 0)
df$hf_75   <- ifelse(df$genetic_group == "HF75", 1, 0)
df$hf_875  <- ifelse(df$genetic_group == "HF87.5", 1, 0)


y_col <- "milk_yield_y"
x_cols <- c("dmi_kg", "scc_100k_d", "glucose_mmol_l", "total_protein_g_d_l", 
            "uric_acid_mg_d_l", "cholesterol_mg_d_l", "calcium_mg_d_l", 
            "hdl_mg_d_l", "ast_u_i", "alt_u_i", "cortisol_mg_d_l", 
            "rectal_temp_f", "pulse_rate_bpm", "respiration_rate_bpm",
            "hf_50", "hf_625", "hf_75", "hf_875")

# 2. Cluster-Robust Double Machine Learning (ATE) for FOUR PILLARS
cat("Running DML for all 4 Pillars (Genetics, Environment, Nutrition, Health)...\n")

treatments <- list(
  list(col = "is_severe_hs", name = "Environment: Severe Heat Stress (-)", type = "binary"),
  list(col = "is_hf_d", name = "Genetics: HF Crossbred (+)", type = "binary"),
  list(col = "dmi_kg", name = "Nutrition: Dry Matter Intake (+)", type = "continuous"),
  list(col = "scc_100k_d", name = "Health: Somatic Cell Count (-)", type = "continuous")
)

learner_rf_regr <- lrn("regr.ranger", num.trees = 300)
learner_rf_classif <- lrn("classif.ranger", num.trees = 300, predict_type = "prob")

dml_results_list <- list()

for (trt in treatments) {
  cat(sprintf("Evaluating Causal Effect of %s...\n", trt$name))
  
  d_col_current <- trt$col
  # Exclude the current treatment and other target variants from the confounder (X) list
  x_cols_current <- setdiff(c(x_cols, "is_hf_d", "is_severe_hs"), d_col_current)
  
  if (d_col_current == "is_hf_d") {
    x_cols_current <- setdiff(x_cols_current, c("hf_50", "hf_625", "hf_75", "hf_875"))
  }
  
  
  dml_data <- DoubleMLClusterData$new(df, y_col = y_col, d_cols = d_col_current, 
                                      x_cols = x_cols_current, cluster_cols = "animal_id")
  set.seed(2026)
  
  # Use IRM for binary treatments and PLR for continuous treatments
  if (trt$type == "binary") {
    dml_obj <- DoubleMLIRM$new(dml_data, ml_g = learner_rf_regr, ml_m = learner_rf_classif, n_folds = 5)
  } else {
    dml_obj <- DoubleMLPLR$new(dml_data, ml_l = learner_rf_regr, ml_m = learner_rf_regr, n_folds = 5)
  }
  
  dml_obj$fit()
  
  # Calculate 95% Confidence Intervals
  ci <- dml_obj$confint(level = 0.95)
  
  dml_results_list[[length(dml_results_list) + 1]] <- data.frame(
    Treatment = trt$name,
    ATE_Estimate = dml_obj$coef,
    CI_Lower_95 = ci[, 1],
    CI_Upper_95 = ci[, 2],
    P_Value = dml_obj$pval
  )
}

dml_summary <- do.call(rbind, dml_results_list)
write.csv(dml_summary, "outputs/DML_ATE_Results.csv", row.names = FALSE)
cat("DML Results for Four Pillars saved.\n")


# 3. Causal Forest for Heterogeneous Treatment Effects (HTE)
cat("Running Causal Forest for Heterogeneous Effects by Genetics...\n")

X_raw <- df %>% select(all_of(x_cols))
X_cf <- as.matrix(X_raw)

set.seed(2026)

cf_model <- causal_forest(X = X_cf, Y = df[[y_col]], W = df[["is_severe_hs"]], clusters = as.numeric(df$animal_id), num.trees = 1000)

# Extract CATE (Conditional Average Treatment Effect)
df$CATE_Heat_Stress <- predict(cf_model)$predictions

# 4. Export Results
write.csv(df %>% select(animal_id, genetic_group, CATE_Heat_Stress), 
          "outputs/CATE_Predictions.csv", row.names = FALSE)
cat("Causal analysis completed and saved to outputs/ folder.\n")
