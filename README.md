# Decoding Milk Yield Dynamics: Evaluating Genetics, Environment, Nutrition, and Health via Double Machine Learning and Causal Forests

This repository contains the custom R scripts, the 7-stage analytical pipeline, and the model outputs supporting the reproducibility of the study.

## Authors
* Yunus Aksu

## Overview
This study benchmarked predictive models (GLMM and MERF) against causal frameworks (Double Machine Learning [DML] and Causal Forests) to evaluate milk yield dynamics under physiological confounding. The dataset consists of 750 panel observations from 50 crossbred and Local dairy cows across three different THI periods.

## Repository Structure
* `data/`: Contains the empirical dataset obtained from the Mendeley Data open-access platform (Eshtiak Ahamed, 2026)(https://doi.org/10.17632/954f6g36sb.2).
* `outputs/`: Stores model outputs, generated descriptive statistics tables, and figures.
* `00_data_merging.R`: Initial data merging operations.
* `01_Data_Prep_and_Descriptives.R`: Feature engineering, variable elimination to prevent data leakage, and calculation of descriptive statistics.
* `02_Correlation_and_VIF.R`: Pearson correlation analysis and Adjusted Variance Inflation Factor (VIF) diagnostics.
* `03_GLMM_Analysis.R`: Generalized Linear Mixed Models (GLMM) incorporating animal-specific random intercepts via the `lme4` package.
* `04_MERF_Analysis.R`: Mixed-Effects Random Forest (MERF) algorithm modeling non-linear relationships via the `ranger` package.
* `05_DML_Causal_Forest_Analysis.R`: Cluster-Robust Double Machine Learning (DML) and Causal Forest models for isolated effect estimation via the `DoubleML` and `grf` packages.
* `06_Results_and_Tables.R`: Scripts to export final analysis results and tables.
* `07_Data_Visualization.R`: Code for plotting correlation heatmaps, actual vs predicted milk yield scatter plots, MERF feature importance, DML isolated effects, and heterogeneous milk yield loss under severe heat stress.

## Software Requirements
All analytical steps and stochastic processes were conducted using the R programming language.

*[dataset] Eshtiak Ahamed, P. (2026). Physiological responses, Dry matter Intake, milk yield, composition and blood metabolites of HF Cross cows (Version V2) Mendeley Data. https://doi.org/10.17632/954f6g36sb.2
