### ==============================================================================
### Script 7: Generate Publication-Quality Figures
### Description: Creates high-resolution (300 DPI) figures for the manuscript,
### including prediction correlation heatmap, scatter plots, feature importance, and
### CATE distributions (Causal Forest) with professional academic labels & units.
### ==============================================================================
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggcorrplot)

cat("Starting Professional Figure Generation...\n")

### ------------------------------------------------------------------------------
### 0. Academic Dictionary
### ------------------------------------------------------------------------------

clean_feature_labels <- c(
  "milk_yield_y"         = "Daily Milk Yield\n(kg/day)",
  "dmi_kg"               = "Dry Matter Intake\n(kg/day)",
  "scc_100k_d"           = "Somatic Cell Count\n(x10\u2075 cells/mL)", 
  "glucose_mmol_l"       = "Glucose\n(mmol/L)",
  "total_protein_g_d_l"  = "Total Protein\n(g/dL)",
  "uric_acid_mg_d_l"     = "Uric Acid\n(mg/dL)",
  "cholesterol_mg_d_l"   = "Cholesterol\n(mg/dL)",
  "calcium_mg_d_l"       = "Calcium\n(mg/dL)",
  "hdl_mg_d_l"           = "HDL Cholesterol\n(mg/dL)",
  "ast_u_i"              = "AST (U/L)",
  "alt_u_i"              = "ALT (U/L)",
  "cortisol_mg_d_l"      = "Cortisol\n(\u00b5g/dL)",                    
  "rectal_temp_f"        = "Rectal Temp\n(\u00b0F)",                    
  "pulse_rate_bpm"       = "Pulse Rate\n(bpm)",
  "respiration_rate_bpm" = "Respiration Rate\n(breaths/min)",           
  "thi_range"            = "THI Range",
  "genetic_group"        = "Genetic Group"
)

### Set global theme for all plots (APA style base)
theme_set(theme_classic(base_size = 14) + 
            theme(plot.title = element_text(face = "bold", hjust = 0.5), 
                  legend.position = "bottom"))

# Load original cleaned data
df_clean <- readRDS("data/Cleaned_Data.rds")

### ------------------------------------------------------------------------------
### FIGURE 1: Correlation Heatmap
### ------------------------------------------------------------------------------
cat("Generating Correlation Heatmap...\n")
cor_matrix <- read.csv("outputs/Correlation_Matrix.csv", row.names = 1)
cor_matrix <- as.matrix(cor_matrix)

rownames(cor_matrix) <- ifelse(rownames(cor_matrix) %in% names(clean_feature_labels), 
                               clean_feature_labels[rownames(cor_matrix)], rownames(cor_matrix))
colnames(cor_matrix) <- ifelse(colnames(cor_matrix) %in% names(clean_feature_labels), 
                               clean_feature_labels[colnames(cor_matrix)], colnames(cor_matrix))

fig1 <- ggcorrplot(cor_matrix, 
                   hc.order = TRUE, 
                   type = "lower", 
                   lab = TRUE,
                   lab_size = 3, 
                   colors = c("#0072B2", "white", "#D55E00"), 
                   title = "Pearson Correlation Matrix of Physiological & Production Variables", 
                   ggtheme = ggplot2::theme_minimal()) + 
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10))

ggsave("outputs/Figure1_Correlation_Heatmap.png", plot = fig1, width = 11, height = 11, dpi = 300, bg = "white")
cat("Figure 1 (Correlation Heatmap) saved successfully.\n")

### ------------------------------------------------------------------------------
### FIGURE 2: Prediction Accuracy (GLMM vs MERF)
### ------------------------------------------------------------------------------
cat("Generating Prediction Accuracy Plot...\n")
glmm_pred <- readRDS("outputs/GLMM_Predictions.rds")
merf_pred <- readRDS("outputs/MERF_Predictions.rds")

plot_data <- data.frame(
  Actual = rep(df_clean$milk_yield_y, 2),
  Predicted = c(glmm_pred, merf_pred),
  Model = factor(rep(c("1. GLMM", "2. MERF"), each = nrow(df_clean)))
)

fig2 <- ggplot(plot_data, aes(x = Actual, y = Predicted, color = Model)) + 
  geom_point(alpha = 0.4, size = 2) + 
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black", linewidth = 1) + 
  facet_wrap(~Model) + 
  scale_color_manual(values = c("#E69F00", "#56B4E9")) + 
  labs(title = "Model Benchmarking: Actual vs. Predicted Milk Yield", 
       x = "Actual Milk Yield (kg/day)", 
       y = "Predicted Milk Yield (kg/day)") + 
  theme(legend.position = "none", 
        strip.text = element_text(face = "bold", size = 14))

ggsave("outputs/Figure2_Prediction_Accuracy.png", plot = fig2, width = 10, height = 5, dpi = 300, bg = "white")
cat("Figure 2 (Prediction Accuracy) saved.\n")

### ------------------------------------------------------------------------------
### FIGURE 3: MERF Feature Importance
### ------------------------------------------------------------------------------
cat("Generating MERF Feature Importance Plot...\n")
importance_df <- read.csv("outputs/MERF_Feature_Importance.csv")

importance_df <- importance_df %>%
  mutate(Feature_Clean = ifelse(Feature %in% names(clean_feature_labels), 
                                clean_feature_labels[Feature], as.character(Feature))) %>%
  arrange(Importance) %>%
  mutate(Feature_Clean = factor(Feature_Clean, levels = Feature_Clean))

fig3 <- ggplot(importance_df, aes(x = Feature_Clean, y = Importance)) + 
  geom_segment(aes(x = Feature_Clean, xend = Feature_Clean, y = 0, yend = Importance), color = "gray50") + 
  geom_point(size = 4, color = "#009E73") + 
  coord_flip() + 
  labs(title = "Feature Importance in MERF Model", 
       subtitle = "Permutation Importance for Milk Yield Prediction", 
       x = "Predictor Variables", 
       y = "Importance Score (Mean Decrease in Accuracy)") + 
  theme_minimal(base_size = 14) + 
  theme(panel.grid.major.y = element_blank(), 
        plot.title = element_text(face = "bold", hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5),
        # lineheight = 0.8 ile iki satırlı etiketlerin kendi içindeki boşluğu daralttık
        axis.text.y = element_text(face = "bold", size = 11, color = "black", lineheight = 0.8))

ggsave("outputs/Figure3_Feature_Importance.png", plot = fig3, width = 10, height = 8, dpi = 300, bg = "white")
cat("Figure 3 (Feature Importance) saved successfully with optimized spacing.\n")

### ------------------------------------------------------------------------------
### FIGURE 4: DML Causal Effects (Forest Plot)
### ------------------------------------------------------------------------------
cat("Generating DML Forest Plot...\n")
dml_plot_data <- read.csv("outputs/DML_ATE_Results.csv")

dml_plot_data <- dml_plot_data %>% 
  mutate(
    Treatment_Clean = case_when(
      Treatment == "Environment: Severe Heat Stress (-)" ~ "Environment:\nSevere Heat Stress (-)",
      Treatment == "Genetics: HF Crossbred (+)"        ~ "Genetics:\nHF Crossbred (+)",
      Treatment == "Nutrition: Dry Matter Intake (+)"   ~ "Nutrition:\nDry Matter Intake (+)",
      Treatment == "Health: Somatic Cell Count (-)"     ~ "Health:\nSomatic Cell Count (-)",
      TRUE ~ Treatment
    ),
    Significance = case_when(
      P_Value < 0.05 & ATE_Estimate > 0 ~ "Positive Effect (p < 0.05)",
      P_Value < 0.05 & ATE_Estimate < 0 ~ "Negative Effect (p < 0.05)",
      TRUE ~ "Insignificant (p > 0.05)"
    )
  ) %>%
 
  mutate(Treatment_Clean = factor(Treatment_Clean, levels = rev(Treatment_Clean)))


fig4 <- ggplot(dml_plot_data, aes(x = ATE_Estimate, y = Treatment_Clean, color = Significance)) + 
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", linewidth = 1) + 
  geom_errorbar(aes(xmin = CI_Lower_95, xmax = CI_Upper_95), width = 0.2, linewidth = 1.2) + 
  geom_point(size = 5, shape = 18) + 
  geom_text(aes(label = sprintf("%.3f kg", ATE_Estimate)), vjust = -1.5, size = 4.5, show.legend = FALSE) + 
  scale_color_manual(values = c(
    "Negative Effect (p < 0.05)" = "#D55E00",
    "Positive Effect (p < 0.05)" = "#009E73",
    "Insignificant (p > 0.05)"  = "#999999"
  )) + 
  labs(
    title = "Isolated Causal Effects on Milk Yield via Double Machine Learning", 
    subtitle = "Impact of the Four Pillars (Genetics, Environment, Nutrition, Health)\nError bars represent 95% Confidence Intervals (CI).", 
    x = "Average Treatment Effect (ATE) on Daily Milk Yield (kg/day)", 
    y = "", 
    color = "Statistical Significance"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5), 
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "grey30"), 
    axis.text.y = element_text(face = "bold", size = 11, color = "black", lineheight = 0.9),
    axis.text.x = element_text(size = 12, color = "black"), 
    axis.title.x = element_text(face = "bold", margin = margin(t = 15)), 
    legend.position = "bottom", 
    panel.grid.minor = element_blank(), 
    panel.grid.major.y = element_blank()
  )

ggsave("outputs/Figure4_DML_Forest_Plot.png", plot = fig4, width = 10, height = 6, dpi = 300, bg = "white")
cat("Figure 4 (DML Forest Plot) saved successfully.\n")

### ------------------------------------------------------------------------------
### FIGURE 5: Heterogeneous Treatment Effects (CATE) by Genetics
### ------------------------------------------------------------------------------
cat("Generating Causal Forest HTE Plot...\n")
cate_df <- read.csv("outputs/CATE_Predictions.csv")

cate_df$genetic_group <- factor(cate_df$genetic_group, 
                                levels = c("Local", "HF50", "HF62.5", "HF75", "HF87.5"),
                                labels = c("Local", "50% HF", "62.5% HF", "75% HF", "87.5% HF"))

fig5 <- ggplot(cate_df, aes(x = genetic_group, y = CATE_Heat_Stress, fill = genetic_group)) + 
  geom_boxplot(alpha = 0.7, outlier.shape = 21, outlier.size = 2) + 
  geom_jitter(width = 0.1, alpha = 0.3, color = "black") + 
  scale_fill_brewer(palette = "Set2") + 
  labs(title = "Heterogeneous Milk Yield Loss under Severe Heat Stress (T2)", 
       subtitle = "Estimated via Cluster-Robust Causal Forest", 
       x = "Genetic Group (Crossbreeding Level)", 
       y = "Estimated Causal Milk Loss (kg/day)") + 
  theme(legend.position = "none", 
        plot.title = element_text(face = "bold", hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(face = "bold", size = 12, color = "black"),
        axis.text.y = element_text(size = 12, color = "black"),
        axis.title.x = element_text(face = "bold", margin = margin(t = 10)),
        axis.title.y = element_text(face = "bold", margin = margin(r = 10)))

ggsave("outputs/Figure5_Heterogeneous_Effects.png", plot = fig5, width = 8, height = 6, dpi = 300, bg = "white")
cat("Figure 5 (Causal Forest HTE) saved successfully.\n")

cat("\nAll professional, high-resolution figures successfully generated and saved to outputs/.\n")
