### Placebo
source("Scripts/R/genereal_settings.R", echo=TRUE)
source("./Scripts/R/functions.R", echo=TRUE)

 

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
temp = merged_data_non_treated_yet[merged_data_non_treated_yet$YEAR_INFO<=2019,]
mean(merged_data_non_treated_yet[merged_data_non_treated_yet$rel_year==-2 ,]$`STEM Fields`)
temp$transition_year = sample(2012:2020, size = nrow(temp), replace = TRUE)


### 2.1.1  "STEM Fields"   ########
 
stem = ATT_sim_dyn_CS( "STEM Fields", temp )
save_img_callaway_dynamic(stem, "Placebo - STEM Fields")
wald_test_att(stem)
