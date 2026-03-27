library(mgcv)
library(ggeffects)
library(gratia)
library(dplyr)

#WD
setwd("/Volumes/Hera/Dan/ncanda/R")

#Data
#full_putamen <- read.csv("clean_data/full_putamen_seed_020425.csv")
#full_caudate <- read.csv("clean_data/full_caudate_seed_020425.csv")
full_nacc <- read.csv("clean_data/full_nacc_seed_020425.csv")

#Making factors for sex and cluster variable
#full_putamen$subject_fac <- as.factor(full_putamen$subject)
#full_putamen$sex <- as.factor(full_putamen$sex)
#full_caudate$subject_fac <- as.factor(full_caudate$subject)
#full_caudate$sex <- as.factor(full_caudate$sex)
full_nacc$subject_fac <- as.factor(full_nacc$subject)
full_nacc$sex <- as.factor(full_nacc$sex)

# Define the list of column names for growth models. 
# I want variables with "harm" in name but not "iron".
# Names are similar across data frames so just using putamen for indexing. 
response_columns <- names(full_nacc)[grepl("_harm$", names(full_nacc)) & 
                                          !grepl("iron", names(full_nacc))]


#
# PUTAMEN MODELS
#
# Initialize lists for summaries and model results
#model_summaries_putamen <- list()
#model_results_putamen <- data.frame(response = character(),
#                                    edf_age = numeric(),
#                                    F_age = numeric(),
#                                    p_value_age = numeric(),
#                                    stringsAsFactors = FALSE)
#
## Loop through each response variable and fit the model
#for (response in response_columns) {
#  
#  # Create formula dynamically
#  formula <- as.formula(paste(response, 
#                              "~ 1 + sex + s(age) + s(subject_fac, bs = 're') + 
#                              s(subject_fac, age, bs = 're')"))
#  
#  # Fit the model
#  model <- bam(formula, data = full_putamen, 
#               na.action = na.exclude, method = "fREML", discrete = TRUE)
#  
#  # Save the model object as an .rds file
#  saveRDS(model, file = paste0("results/gam_models_putamen/models/", response, "_model.rds"))
#  
#  # Save the model summary
#  summary_model <- summary(model)
#  saveRDS(summary_model, file = paste0("results/gam_models_putamen/summaries/", response, "_summary.rds"))
#  
#  # Generate predictions and save as CSV
#  predictions <- ggpredict(model, terms = c("age")) %>% as.data.frame()
#  predictions$response <- response
#  write.csv(predictions, paste0("results/gam_models_putamen/predictions/", 
#                                response, 
#                                "_predictions.csv"), 
#            row.names = FALSE)
#  
#  # Compute first derivatives and save as CSV
#  derivs <- derivatives(model, interval = "simultaneous", n_sim = 10000, n = 200) %>% as.data.frame()
#  derivs$response <- response
#  derivs <- derivs %>% mutate(sig = !(0 > .lower_ci & 0 < .upper_ci))
#  write.csv(derivs, paste0("results/gam_models_putamen/derivatives/", 
#                           response, 
#                           "_derivatives.csv"), 
#            row.names = FALSE)
#  
#  # Extract beta, EDF, F-statistic, and p-value for s(age)
#  summary_model <- summary(model)
#  edf_age <- summary_model$s.table["s(age)", "edf"]
#  f_age <- summary_model$s.table["s(age)", "F"]
#  p_value_age <- summary_model$s.table["s(age)", "p-value"]
#  
#  # Append results to model results dataframe
#  model_results_putamen <- rbind(model_results_putamen,
#                                 data.frame(response = response, 
#                                            edf_age = edf_age,
#                                            F_age = f_age,
#                                            p_value_age = p_value_age))
#}
#
## Correct for multiple comparisons across all tests using FDR
#model_results_putamen$p_fdr <- p.adjust(model_results_putamen$p_value_age, method = "fdr")
#
## Save overall model summaries and results
#write.csv(model_results_putamen, "results/gam_models_putamen/gam_model_results_putamen.csv", row.names = FALSE)
#

#
# Caudate MODELS
#
# Initialize lists for summaries and model results
#model_summaries_caudate <- list()
#model_results_caudate <- data.frame(response = character(),
#                                    edf_age = numeric(),
#                                    F_age = numeric(),
#                                    p_value_age = numeric(),
#                                    stringsAsFactors = FALSE)
#
## Loop through each response variable and fit the model
#for (response in response_columns) {
#  
#  # Create formula dynamically
#  formula <- as.formula(paste(response, 
#                              "~ 1 + sex + s(age) + s(subject_fac, bs = 're') + 
#                              s(subject_fac, age, bs = 're')"))
#  
#  # Fit the model
#  model <- bam(formula, data = full_caudate, 
#               na.action = na.exclude, method = "fREML", discrete = TRUE)
#  
#  # Save the model object as an .rds file
#  saveRDS(model, file = paste0("results/gam_models_caudate/models/", response, "_model.rds"))
#  
#  # Save the model summary
#  summary_model <- summary(model)
#  saveRDS(summary_model, file = paste0("results/gam_models_caudate/summaries/", response, "_summary.rds"))
#  
#  # Generate predictions and save as CSV
#  predictions <- ggpredict(model, terms = c("age")) %>% as.data.frame()
#  predictions$response <- response
#  write.csv(predictions, paste0("results/gam_models_caudate/predictions/", 
#                                response, 
#                                "_predictions.csv"), 
#            row.names = FALSE)
#  
#  # Compute first derivatives and save as CSV
#  derivs <- derivatives(model, interval = "simultaneous", n_sim = 10000, n = 200) %>% as.data.frame()
#  derivs$response <- response
#  derivs <- derivs %>% mutate(sig = !(0 > .lower_ci & 0 < .upper_ci))
#  write.csv(derivs, paste0("results/gam_models_caudate/derivatives/", 
#                           response, 
#                           "_derivatives.csv"), 
#            row.names = FALSE)
#  
#  # Extract beta, EDF, F-statistic, and p-value for s(age)
#  summary_model <- summary(model)
#  edf_age <- summary_model$s.table["s(age)", "edf"]
#  f_age <- summary_model$s.table["s(age)", "F"]
#  p_value_age <- summary_model$s.table["s(age)", "p-value"]
#  
#  # Append results to model results dataframe
#  model_results_caudate <- rbind(model_results_caudate,
#                                 data.frame(response = response, 
#                                            edf_age = edf_age,
#                                            F_age = f_age,
#                                            p_value_age = p_value_age))
#}
#
## Correct for multiple comparisons across all tests using FDR
#model_results_caudate$p_fdr <- p.adjust(model_results_caudate$p_value_age, method = "fdr")
#
## Save overall model summaries and results
#write.csv(model_results_caudate, "results/gam_models_caudate/gam_model_results_caudate.csv", row.names = FALSE)



#
# NAcc MODELS
#
# Initialize lists for summaries and model results
model_summaries_nacc <- list()
model_results_nacc <- data.frame(response = character(),
                                    edf_age = numeric(),
                                    F_age = numeric(),
                                    p_value_age = numeric(),
                                    stringsAsFactors = FALSE)

# Loop through each response variable and fit the model
for (response in response_columns) {
  
  # Create formula dynamically
  formula <- as.formula(paste(response, 
                              "~ 1 + sex + s(age) + s(subject_fac, bs = 're') + 
                              s(subject_fac, age, bs = 're')"))
  
  # Fit the model
  model <- bam(formula, data = full_nacc, 
               na.action = na.exclude, method = "fREML", discrete = TRUE)
  
  # Save the model object as an .rds file
  saveRDS(model, file = paste0("results/gam_models_nacc/models/", response, "_model.rds"))
  
  # Save the model summary
  summary_model <- summary(model)
  saveRDS(summary_model, file = paste0("results/gam_models_nacc/summaries/", response, "_summary.rds"))
  
  # Generate predictions and save as CSV
  predictions <- ggpredict(model, terms = c("age")) %>% as.data.frame()
  predictions$response <- response
  write.csv(predictions, paste0("results/gam_models_nacc/predictions/", 
                                response, 
                                "_predictions.csv"), 
            row.names = FALSE)
  
  # Compute first derivatives and save as CSV
  derivs <- derivatives(model, interval = "simultaneous", n_sim = 10000, n = 200) %>% as.data.frame()
  derivs$response <- response
  derivs <- derivs %>% mutate(sig = !(0 > .lower_ci & 0 < .upper_ci))
  write.csv(derivs, paste0("results/gam_models_nacc/derivatives/", 
                           response, 
                           "_derivatives.csv"), 
            row.names = FALSE)
  
  # Extract beta, EDF, F-statistic, and p-value for s(age)
  summary_model <- summary(model)
  edf_age <- summary_model$s.table["s(age)", "edf"]
  f_age <- summary_model$s.table["s(age)", "F"]
  p_value_age <- summary_model$s.table["s(age)", "p-value"]
  
  # Append results to model results dataframe
  model_results_nacc <- rbind(model_results_nacc,
                                 data.frame(response = response, 
                                            edf_age = edf_age,
                                            F_age = f_age,
                                            p_value_age = p_value_age))
}

# Correct for multiple comparisons across all tests using FDR
model_results_nacc$p_fdr <- p.adjust(model_results_nacc$p_value_age, method = "fdr")

# Save overall model summaries and results
write.csv(model_results_nacc, "results/gam_models_nacc/gam_model_results_nacc.csv", row.names = FALSE)
