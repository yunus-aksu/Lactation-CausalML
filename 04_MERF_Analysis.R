# ==============================================================================
# Script 4: Mixed-Effects Random Forest (MERF) Analysis
# Description: Predictive modeling using MERF with Q1 validation metrics.
# ==============================================================================

library(ranger)
library(lme4)

cat("Starting MERF Analysis...\n")

df <- readRDS("data/Cleaned_Data.rds")

target <- "milk_yield_y"
features <- c("genetic_group", "thi_range", "dmi_kg", "scc_100k_d", 
              "glucose_mmol_l", "total_protein_g_d_l", "uric_acid_mg_d_l", 
              "cholesterol_mg_d_l", "calcium_mg_d_l", "hdl_mg_d_l", "ast_u_i", 
              "alt_u_i", "cortisol_mg_d_l", "rectal_temp_f", "pulse_rate_bpm", 
              "respiration_rate_bpm")

b_i <- rep(0, length(unique(df$animal_id)))
names(b_i) <- unique(df$animal_id)

max_iter <- 10; tolerance <- 1e-4; last_mse <- Inf

set.seed(2026)

for (iter in 1:max_iter) {
  df$y_star <- df[[target]] - b_i[as.character(df$animal_id)]
  rf_formula <- as.formula(paste("y_star ~", paste(features, collapse = " + ")))
  rf_model <- ranger(rf_formula, data = df, num.trees = 300, importance = "permutation")
  df$rf_pred <- rf_model$predictions
  df$residuals <- df[[target]] - df$rf_pred
  
  lmm_res <- lmer(residuals ~ 1 + (1 | animal_id), data = df, control = lmerControl(calc.derivs = FALSE))
  b_i_new <- ranef(lmm_res)$animal_id[, 1]
  names(b_i_new) <- rownames(ranef(lmm_res)$animal_id)
  
  current_mse <- mean((df[[target]] - (df$rf_pred + b_i_new[as.character(df$animal_id)]))^2)
  if (abs(last_mse - current_mse) < tolerance) { break }
  b_i <- b_i_new; last_mse <- current_mse
}

merf_predictions <- df$rf_pred + b_i[as.character(df$animal_id)]
actuals <- df[[target]]

# Metrics Calculation
merf_rmse <- sqrt(mean((actuals - merf_predictions)^2))
merf_mae <- mean(abs(actuals - merf_predictions))
merf_rrmse <- (merf_rmse / mean(actuals)) * 100
merf_r2 <- 1 - (sum((actuals - merf_predictions)^2) / sum((actuals - mean(actuals))^2))

ccc <- function(y_true, y_pred) {
  2 * cov(y_true, y_pred) / (var(y_true) + var(y_pred) + (mean(y_true) - mean(y_pred))^2)
}
merf_ccc <- ccc(actuals, merf_predictions)

importance_df <- data.frame(Feature = names(rf_model$variable.importance), Importance = rf_model$variable.importance)
results_merf <- data.frame(
  Model = "MERF", RMSE = merf_rmse, MAE = merf_mae, rRMSE = merf_rrmse, CCC = merf_ccc, R2 = merf_r2
)

write.csv(results_merf, "outputs/MERF_Metrics.csv", row.names = FALSE)
write.csv(importance_df, "outputs/MERF_Feature_Importance.csv", row.names = FALSE)
saveRDS(merf_predictions, "outputs/MERF_Predictions.rds")
cat("MERF analysis completed with full metrics.\n")

# ==============================================================================
# MERF 5-FOLD ANIMAL-LEVEL CLUSTER CV
# ==============================================================================
library(ranger)
library(lme4)
library(dplyr)

set.seed(42)

unique_animals <- unique(df$animal_id)
folds <- sample(rep(1:5, length.out = length(unique_animals)))
animal_fold_map <- data.frame(animal_id = unique_animals, fold = folds)

if (!"fold" %in% colnames(df)) {
  df <- df %>% left_join(animal_fold_map, by = "animal_id")
}

df$pred_merf_cv <- NA

# 5-Fold CV
for (k in 1:5) {
  train_df <- df %>% filter(fold != k)
  test_df  <- df %>% filter(fold == k)
  
  b_i_tr <- rep(0, length(unique(train_df$animal_id)))
  names(b_i_tr) <- unique(train_df$animal_id)
  max_iter <- 10; tolerance <- 1e-4; last_mse <- Inf
  
  for (iter in 1:max_iter) {
    train_df$y_star <- train_df[[target]] - b_i_tr[as.character(train_df$animal_id)]
    rf_formula <- as.formula(paste("y_star ~", paste(features, collapse = " + ")))
    rf_fold <- ranger(rf_formula, data = train_df, num.trees = 300)
    train_df$rf_pred <- rf_fold$predictions
    train_df$residuals <- train_df[[target]] - train_df$rf_pred
    
    lmm_fold <- lmer(residuals ~ 1 + (1 | animal_id), data = train_df, control = lmerControl(calc.derivs = FALSE))
    b_i_new <- ranef(lmm_fold)$animal_id[, 1]
    names(b_i_new) <- rownames(ranef(lmm_fold)$animal_id)
    
    current_mse <- mean((train_df[[target]] - (train_df$rf_pred + b_i_new[as.character(train_df$animal_id)]))^2)
    if (abs(last_mse - current_mse) < tolerance) { break }
    b_i_tr <- b_i_new
    last_mse <- current_mse
  }
  
  rf_test_pred <- predict(rf_fold, data = test_df)$predictions
  df$pred_merf_cv[df$fold == k] <- rf_test_pred
}

# Out-of-Sample CV Metrics
merf_cv_rmse  <- sqrt(mean((df[[target]] - df$pred_merf_cv)^2))
merf_cv_mae   <- mean(abs(df[[target]] - df$pred_merf_cv))
merf_cv_rrmse <- (merf_cv_rmse / mean(df[[target]])) * 100
merf_cv_ccc   <- ccc(df[[target]], df$pred_merf_cv)
merf_cv_r2    <- 1 - (sum((df[[target]] - df$pred_merf_cv)^2) / sum((df[[target]] - mean(df[[target]]))^2))

results_merf_all <- data.frame(
  Model = c("MERF (In-Sample)", "MERF (5-Fold CV)"),
  RMSE = c(merf_rmse, merf_cv_rmse),
  MAE = c(merf_mae, merf_cv_mae),
  rRMSE = c(merf_rrmse, merf_cv_rrmse),
  CCC = c(merf_ccc, merf_cv_ccc),
  R2 = c(merf_r2, merf_cv_r2)
)

write.csv(results_merf_all, "outputs/MERF_Metrics.csv", row.names = FALSE)
cat(">>> Script 4: The MERF 5-Fold Cluster CV has been completed, and outputs/MERF_Metrics.csv has been updated.\n")
