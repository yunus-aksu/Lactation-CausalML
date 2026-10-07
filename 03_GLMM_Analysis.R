# ==============================================================================
# Script 3: Generalized Linear Mixed Model (GLMM) Analysis
# Description: Baseline predictive modeling using classical GLMM.
#              Includes advanced metrics (MAE, rRMSE, CCC, AIC, BIC).
# ==============================================================================

library(lme4)
library(lmerTest)
library(MuMIn)
library(dplyr)
library(Metrics)
library(epiR)

cat("Starting GLMM Analysis...\n")

df <- readRDS("data/Cleaned_Data.rds")

glmm_model <- lmer(milk_yield_y ~ genetic_group + thi_range + dmi_kg + scc_100k_d + 
                     glucose_mmol_l + total_protein_g_d_l + uric_acid_mg_d_l + 
                     cholesterol_mg_d_l + calcium_mg_d_l + hdl_mg_d_l + ast_u_i + 
                     alt_u_i + cortisol_mg_d_l + rectal_temp_f + pulse_rate_bpm + 
                     respiration_rate_bpm + (1 | animal_id), 
                   data = df, REML = TRUE)

glmm_predictions <- predict(glmm_model, df)
actuals <- df$milk_yield_y

# Metrics Calculation
glmm_rmse <- sqrt(mean((actuals - glmm_predictions)^2))
glmm_mae <- mean(abs(actuals - glmm_predictions))
glmm_rrmse <- (glmm_rmse / mean(actuals)) * 100

# Lin's Concordance Correlation Coefficient (CCC)
ccc <- function(y_true, y_pred) {
  2 * cov(y_true, y_pred) / (var(y_true) + var(y_pred) + (mean(y_true) - mean(y_pred))^2)
}
glmm_ccc <- ccc(actuals, glmm_predictions)
glmm_r2 <- r.squaredGLMM(glmm_model) 

# Information Criteria
glmm_aic <- AIC(glmm_model)
glmm_bic <- BIC(glmm_model)

results_glmm <- data.frame(
  Model = "GLMM",
  RMSE = glmm_rmse, MAE = glmm_mae, rRMSE = glmm_rrmse, CCC = glmm_ccc,
  Marginal_R2 = glmm_r2[1, "R2m"], Conditional_R2 = glmm_r2[1, "R2c"],
  AIC = glmm_aic, BIC = glmm_bic
)

write.csv(results_glmm, "outputs/GLMM_Metrics.csv", row.names = FALSE)
saveRDS(glmm_predictions, "outputs/GLMM_Predictions.rds")
cat("GLMM analysis completed with full metrics.\n")

# ==============================================================================
# GLMM 5-FOLD ANIMAL-LEVEL CLUSTER CV
# ==============================================================================
library(lme4)
library(dplyr)

set.seed(42)

unique_animals <- unique(df$animal_id)
folds <- sample(rep(1:5, length.out = length(unique_animals)))
animal_fold_map <- data.frame(animal_id = unique_animals, fold = folds)

if (!"fold" %in% colnames(df)) {
  df <- df %>% left_join(animal_fold_map, by = "animal_id")
}

df$pred_glmm_cv <- NA

# 5-Fold Cluster CV
for (k in 1:5) {
  train_df <- df %>% filter(fold != k)
  test_df  <- df %>% filter(fold == k)
  
  glmm_fold <- lmer(milk_yield_y ~ genetic_group + thi_range + dmi_kg + scc_100k_d + 
                      glucose_mmol_l + total_protein_g_d_l + uric_acid_mg_d_l + 
                      cholesterol_mg_d_l + calcium_mg_d_l + hdl_mg_d_l + 
                      ast_u_i + alt_u_i + cortisol_mg_d_l + rectal_temp_f + 
                      pulse_rate_bpm + respiration_rate_bpm + (1 | animal_id), 
                    data = train_df, REML = TRUE)
  
  df$pred_glmm_cv[df$fold == k] <- predict(glmm_fold, newdata = test_df, allow.new.levels = TRUE)
}

# 3. Out-of-Sample CV Metrics
glmm_cv_rmse  <- sqrt(mean((df$milk_yield_y - df$pred_glmm_cv)^2))
glmm_cv_mae   <- mean(abs(df$milk_yield_y - df$pred_glmm_cv))
glmm_cv_rrmse <- (glmm_cv_rmse / mean(df$milk_yield_y)) * 100
glmm_cv_ccc   <- ccc(df$milk_yield_y, df$pred_glmm_cv)
glmm_cv_r2    <- 1 - (sum((df$milk_yield_y - df$pred_glmm_cv)^2) / sum((df$milk_yield_y - mean(df$milk_yield_y))^2))

results_glmm_all <- data.frame(
  Model = c("GLMM (In-Sample)", "GLMM (5-Fold CV)"),
  RMSE = c(glmm_rmse, glmm_cv_rmse),
  MAE = c(glmm_mae, glmm_cv_mae),
  rRMSE = c(glmm_rrmse, glmm_cv_rrmse),
  CCC = c(glmm_ccc, glmm_cv_ccc),
  R2 = c(glmm_r2[1, "R2c"], glmm_cv_r2),
  AIC = c(glmm_aic, NA),
  BIC = c(glmm_bic, NA)
)

write.csv(results_glmm_all, "outputs/GLMM_Metrics.csv", row.names = FALSE)
cat(">>> Script 3: GLMM 5-Fold Cluster CV has been completed, and outputs/GLMM_Metrics.csv has been updated.\n")
