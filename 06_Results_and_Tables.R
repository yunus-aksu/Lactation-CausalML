# ==============================================================================
# Script 6: Generate Publication-Ready Tables
# Description: Compiles models into APA-formatted tables, including 
#              MAE, rRMSE, CCC, AIC, BIC, and 95% Confidence Intervals.
# ==============================================================================

library(dplyr)

cat("Starting Table Generation...\n")

glmm_metrics <- read.csv("outputs/GLMM_Metrics.csv")
merf_metrics <- read.csv("outputs/MERF_Metrics.csv")

# TABLE 2: Comparison Table
performance_table <- data.frame(
  Model = c(glmm_metrics$Model, merf_metrics$Model),
  RMSE = round(c(glmm_metrics$RMSE, merf_metrics$RMSE), 3),
  MAE = round(c(glmm_metrics$MAE, merf_metrics$MAE), 3),
  rRMSE_percent = round(c(glmm_metrics$rRMSE, merf_metrics$rRMSE), 2),
  CCC = round(c(glmm_metrics$CCC, merf_metrics$CCC), 3),
  R_Squared = round(c(glmm_metrics$R2, merf_metrics$R2), 3),
  AIC = c(round(glmm_metrics$AIC[1], 1), NA, NA, NA),
  BIC = c(round(glmm_metrics$BIC[1], 1), NA, NA, NA)
)

print(performance_table)
write.csv(performance_table, "outputs/Table2_Model_Performance.csv", row.names = FALSE)

# TABLE 3: DML Causal Effects (with 95% Confidence Interval)
dml_results <- read.csv("outputs/DML_ATE_Results.csv")
ate_table <- dml_results %>%
  mutate(
    ATE_Estimate_kg = round(ATE_Estimate, 3),
    CI_95 = paste0("[", round(CI_Lower_95, 3), ", ", round(CI_Upper_95, 3), "]"),
    P_Value = signif(P_Value, 3),
    Significance = ifelse(P_Value < 0.001, "***", ifelse(P_Value < 0.01, "**", ifelse(P_Value < 0.05, "*", "ns")))
  ) %>%
  select(Treatment, ATE_Estimate_kg, CI_95, P_Value, Significance)

print(ate_table)
write.csv(ate_table, "outputs/Table3_DML_ATE_Results.csv", row.names = FALSE)

# TABLE 4: Causal Forest Heterogeneous Distribution
cate_df <- read.csv("outputs/CATE_Predictions.csv")

cate_df$genetic_group <- factor(cate_df$genetic_group, 
                                levels = c("Local", "HF50", "HF62.5", "HF75", "HF87.5"))

cate_summary <- cate_df %>%
  group_by(genetic_group) %>%
  summarize(
    N_Cows = n_distinct(animal_id), Mean_Milk_Loss_kg = round(mean(CATE_Heat_Stress), 3),
    SD_Loss = round(sd(CATE_Heat_Stress), 3), Min_Loss = round(min(CATE_Heat_Stress), 3),
    Max_Loss = round(max(CATE_Heat_Stress), 3)
  ) %>% arrange(genetic_group)

write.csv(cate_summary, "outputs/Table4_CATE_by_Genetics.csv", row.names = FALSE)
cat("\nAll comprehensive tables successfully generated.\n")
