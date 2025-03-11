#### Robustness 1. 
ATT_sim_dyn_CS_robust = function(outcome, pre_start_year){
  outcome = paste0('TOT_', outcome)
  
  # Subset data based on pre-treatment start year
  subset_data <- merged_data[merged_data$YEAR_INFO >= pre_start_year, ]
  
  # Estimate the Callaway & Sant'Anna staggered DiD
  did_result <- att_gt(yname = outcome, # Outcome variable
                       tname = "YEAR_INFO", # Time variable
                       idname = "codigo_dane_sede", # School ID
                       gname = "transition_year", # Treatment group (year of transition)
                       control_group = "notyettreated",
                       data = subset_data,
                       panel = F,
                       xformla = ~1, # No covariates in this example
                       est_method = "dr") # "dr" for doubly robust estimation
  
  agg.simple <- aggte(did_result, type = "simple", na.rm = TRUE)
  
  agg.dynamic <- aggte(did_result, type = "dynamic", na.rm = TRUE)
  
  agg.calendar <- aggte(did_result, type = "calendar", na.rm = TRUE)
  
  return(list(simple = agg.simple, 
              dynamic = agg.dynamic, 
              calendar = agg.calendar))
}

# Original pre-treatment period
stem_results_original <- ATT_sim_dyn_CS_robust("STEM", 2012) # Assuming your original start year was 2012
summary(stem_results_original[["dynamic"]])
plot_ev_CS(stem_results_original, field_study='' )
# Shorter pre-treatment period
stem_results_shorter <- ATT_sim_dyn_CS_robust("STEM", 2014) # Example: starting from 2014
summary(stem_results_shorter[["dynamic"]])

plot_ev_CS(stem_results_shorter, field_study='' )
# Longer pre-treatment period
stem_results_longer <- ATT_sim_dyn_CS_robust("STEM", 2013) # Example: starting from 2010
summary(stem_results_longer[["dynamic"]])

plot_ev_CS(stem_results_longer, field_study='' )

png(paste0(graphs_dir,'CS_stem_dynamic_effects_2014.png'),  width = 1030, height = 598)
plot_ev_CS(stem_results_shorter, field_study= paste0( get_var_label(outcomes[12]), "\n Sensitivity to Pre-Treatment Periods 2014-2020" ) )
dev.off() 

#### Robustness 2. 



ATT_sim_dyn_CS_robust2 = function(outcome, treatment_year_var){
  outcome = paste0('TOT_', outcome)
  
  # Identify unique treatment years in the data (including placebo years)
  unique_treatment_years <- unique(merged_data[[treatment_year_var]])
  
  # Calculate the earliest year for EACH treatment group 
  earliest_year_by_group <- tapply(merged_data$YEAR_INFO, merged_data[[treatment_year_var]], min)
  
  # Filter out treatment groups that don't have pre-treatment data
  valid_treatment_years <- names(earliest_year_by_group)[earliest_year_by_group < unique_treatment_years]
  
  # Filter the data to only include valid treatment groups
  subset_data <- merged_data[merged_data[[treatment_year_var]] %in% valid_treatment_years, ]
  
  
  # Create a table to count observations for each group-time combination
  obs_table <- table(subset_data[[treatment_year_var]], subset_data$YEAR_INFO)
  
  # Identify valid group-time combinations with enough observations 
  valid_combinations <- which(obs_table > 1, arr.ind = TRUE) # At least 2 observations
  
  # Filter the data to only include valid group-time combinations
  subset_data <- subset_data[
    subset_data[[treatment_year_var]] %in% rownames(obs_table)[valid_combinations[,1]] &
      subset_data$YEAR_INFO %in% colnames(obs_table)[valid_combinations[,2]], 
  ]
  
  # Estimate the Callaway & Sant'Anna staggered DiD 
  did_result <- att_gt(yname = outcome,
                       tname = "YEAR_INFO", 
                       idname = "codigo_dane_sede", 
                       gname = treatment_year_var,
                       control_group = "notyettreated",
                       data = subset_data, # Use the filtered data
                       panel = F,
                       xformla = ~1,
                       est_method = "dr")
    
  
  agg.simple <- aggte(did_result, type = "simple", na.rm = TRUE)
  
  agg.dynamic <- aggte(did_result, type = "dynamic", na.rm = TRUE)
  
  agg.calendar <- aggte(did_result, type = "calendar", na.rm = TRUE)
  
  return(list(simple = agg.simple, 
              dynamic = agg.dynamic, 
              calendar = agg.calendar))
}



unique(merged_data$transition_year)

# Create placebo treatment variables
# Create placebo treatment variable for 2013
merged_data$placebo_treatment_year_2013 <- ifelse(merged_data$treated == 1, 2013, merged_data$transition_year)

# Run the analysis for STEM with original treatment and placebo 2013
print(get_var_label("STEM"))

# Original treatment
out_original <- ATT_sim_dyn_CS_robust2("STEM", "transition_year") 
print("Original Treatment:")
summary(out_original[["dynamic"]])

# Placebo 2013
out_placebo_2013 <- ATT_sim_dyn_CS_robust2("STEM", "placebo_treatment_year_2013") 
print("Placebo 2013:")
summary(out_placebo_2013[["dynamic"]])




# Robust 3
library(did)

sunab_stem_results <- feols(
  TOT_STEM ~ sunab(transition_year, YEAR_INFO, -1) | codigo_dane_sede + YEAR_INFO, 
  data = merged_data, 
)


summary(sunab_stem_results, agg = 'att')

# Estimate Sun & Abraham ATT for STEM

print(summary(sunab_stem_results))

 

# Borusyak, Jaravel, and Spiess (2021) Estimator

library("didimputation")

did_multiplegt(merged_data, 'TOT_STEM', 'transition_year')

attgt_results <- did_imputation(
  yname = "TOT_STEM",               # Outcome variable
  gname = "transition_year",        # Group variable (when units receive treatment)
  tname = "YEAR_INFO",              # Time variable
  idname = "codigo_dane_sede",      # Unit identifier
  data = merged_data, first_stage = ~ 0 | codigo_dane_sede + YEAR_INFO,pretrends = T
)
merged_data$treated


install.packages("TwoWayFEWeights")
library(TwoWayFEWeights)
CH =twowayfeweights(merged_data,
                "TOT_STEM",
                 "transition_year",
                "YEAR_INFO",
                "treated", 
                type = "feTR", 
                summary_measures    = TRUE  )
summary(CH)



install.packages("DIDmultiplegt", force = TRUE)
library(DIDmultiplegtDYN)
