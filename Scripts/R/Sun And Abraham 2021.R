source("Scripts/R/genereal_settings.R", echo=TRUE)
source("./Scripts/R/functions.R", echo=TRUE)


# 1.  Data Reading and Preparation ####
## 1.1 Data For Never Treated Estimation ####
merged_data_never_treated = arrow::read_parquet("Data/never_treated_data.parquet")
merged_data_never_treated$`Female Proportion` = (merged_data_never_treated$TOT_FEMALE/merged_data_never_treated$TOT_STU)
merged_data_never_treated$`Female Proportion`  <- as.numeric( scale(merged_data_never_treated$`Female Proportion` , center = TRUE, scale = TRUE) )

merged_data_never_treated = summarize_data_staggered(merged_data_never_treated)
merged_data_never_treated$YEAR_INFO = as.numeric(merged_data_never_treated$YEAR_INFO)
control_group = "nevertreated"

## 1.2 Data For Not-Yet-Treated Estimation ####
merged_data_non_treated_yet = arrow::read_parquet("Data/no_yet_treated_data.parquet")
merged_data_non_treated_yet=subset(merged_data_non_treated_yet, is.na(merged_data_non_treated_yet$transition_year)==F)
merged_data_non_treated_yet$`Female Proportion` = (merged_data_non_treated_yet$TOT_FEMALE/merged_data_non_treated_yet$TOT_STU)
merged_data_non_treated_yet$`Female Proportion`  <- as.numeric( scale(merged_data_non_treated_yet$`Female Proportion` , center = TRUE, scale = TRUE) )

merged_data_non_treated_yet = summarize_data_staggered(merged_data_non_treated_yet)
merged_data_non_treated_yet$YEAR_INFO = as.numeric(merged_data_non_treated_yet$YEAR_INFO)
summary(merged_data_non_treated_yet$`Female Proportion`)
control_group = "notyettreated"

merged_data_non_treated_yet$intensity_event_time <- merged_data_non_treated_yet$`Female Proportion`  * as.numeric(merged_data_non_treated_yet$YEAR_INFO - merged_data_non_treated_yet$transition_year >= 0)
merged_data_non_treated_yet$intensity_event_time <- as.numeric(scale(merged_data_non_treated_yet$intensity_event_time))

# merged_data_non_treated_yet = merged_data_non_treated_yet[merged_data_non_treated_yet$rel_year<6, ]
merged_data_never_treated
# 2. Staggered Estimation ####
outcomes = c(
  "STEM Fields", "Non-STEM Fields", "Not Continuing Education",  
  "Mathematics/Natural Sciences",   "Engineering/Architecture",
  "Law", "Social Sciences/Humanities","Economics/Business",
  "Education Sciences", "Fine Arts","Agronomy/Veterinary"  , 
  "Health Sciences", 'Medicine', 
  'STEM - Science', 'STEM - Technology','STEM - Engineering', 'STEM - Mathematics',
  'Enrollment in University', "Enrollment in Technical", "Enrollment in Technological"
)
# We exclude all the observations affected by COVID 19
temp = merged_data_non_treated_yet[merged_data_non_treated_yet$YEAR_INFO<=2021,]
mean(merged_data_non_treated_yet[merged_data_non_treated_yet$rel_year==-2 ,]$`STEM Fields`)
temp$Y = 1-temp$`STEM Fields`
temp$X = 1-temp$`Female Proportion`
  temp$Y = 1-temp$`STEM - Technology`
  temp$Y = 1- temp$`STEM - Science`
  temp$Y = 1- temp$`STEM - Mathematics`
  temp$Y = 1- temp$`STEM - Engineering`
?sunab 
sunab_stem = fixest::feols(Y~ sunab(transition_year,
                                    YEAR_INFO,
                                    bin.c = 2019:2021,
                                    ref.p =c(-2:-1)
                                    ) |
                             codigo_dane_sede+YEAR_INFO   , temp)

summary(sunab_stem, agg = "ATT")
summary(sunab_stem, agg = "cohort")
 
sunab_result_stem = as.data.frame(sunab_stem$coeftable)
# Convert row names to a column
sunab_result_stem$year_info <- rownames(sunab_result_stem)

# Remove 'YEAR_INFO::' from the 'year_info' column
sunab_result_stem$year_info <- gsub("YEAR_INFO::", "", sunab_result_stem$year_info)
sunab_result_stem$year_info <-as.numeric(sunab_result_stem$year_info)
sunab_result_stem$estimator =  "Sun and Abraham (2021)"
sunab_result_stem$estimate = sunab_result_stem$Estimate
sunab_result_stem$std.error =sunab_result_stem$`Std. Error` 

sunab_result_stem$term = sunab_result_stem$year_info

sunab_result_stem[ , c('term', 'Estimate',  'std.error' )] 


save_img_sunab_dynamic(sunab_result_stem, "STEM Fields")
model=sunab_result_stem
field_study = "STEM Fields"
filename_ = gsub(gsub("STEM Fields", pattern = "/", replace = " and or "), pattern = "\n", replace = "_")
print(filename_)
png(paste0(graphs_dir,'SA_',filename_,".png"),  width = 1030, height = 598)
event_study_plot(model, TITULO = field_study)
dev.off() 


############################
 
# ----------------------------------------------------------------------
#   Average cumulative (total) effect per treatment unit
# ----------------------------------------------------------------------
#   Estimate        SE     LB CI     UB CI         N Switchers 
# 0.04217   0.17064  -0.29228   0.37663       147        32 
