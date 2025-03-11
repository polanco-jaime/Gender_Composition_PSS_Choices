source("Scripts/R/genereal_settings.R", echo=TRUE)
source("./Scripts/R/functions.R", echo=TRUE)
source("./Scripts/R/read_data.R", echo=TRUE)

# colnames(data)


# 1. Descriptive Statistics  ######### 
## 1.1 Data Preparation ######### 

"Create numeric sequence for the classroom, 
It is because in some schools the sequence can be A,B,C... and so on. 
While in others schools can be a numeric one... 1,2,3,etc
" 
data <- data %>%
  dplyr::group_by(codigo_dane_sede, YEAR_INFO) %>%
  dplyr::mutate(grupo_numeric = as.numeric(factor(grupo, levels = sort(unique(grupo), decreasing = TRUE))),
                grupo_numeric = ifelse(grupo_numeric >= 10, 10, grupo_numeric)) %>%
  # Calculate the number of unique groups in each year per codigo_dane_sede
  dplyr::mutate(num_unique_grupos = n_distinct(grupo)) %>%
  # Create the dummy variable
  dplyr::mutate(dummy_single_grupo = ifelse(num_unique_grupos == 1, 1, 0)) %>%
  dplyr::ungroup() #%>%
# Optionally, remove the intermediate variable if it's no longer needed
# dplyr::select(-num_unique_grupos)


data$school_id = paste0(data$grupo_numeric ,"-",  data$codigo_dane_sede)


## 1.2 Classroom summarization ######### 

classroom_summary <- data %>%
  group_by(school_id, YEAR_INFO) %>%
  dplyr::summarize(
    TOT_MALE = sum(genero == 'M', na.rm = TRUE),
    TOT_FEMALE = sum(genero == 'F', na.rm = TRUE),
    TOT_STU = dplyr::n(),  # More efficient way to count students
    AVG_AGE = mean(EDAD, na.rm = TRUE),
    TOT_STEM = sum(STEM == 1   , na.rm = TRUE), #  
    TOT_STEM_F = sum(STEM == 1 &  genero == 'F' , na.rm = TRUE), #   
    TOT_NO_STEM = sum(NO_STEM == 1   , na.rm = TRUE),
    TOT_NO_STEM_F = sum(NO_STEM == 1 &  genero == 'F' , na.rm = TRUE), # 
    TOT_ECONOMICS_BUSINESS_RELATED = sum(ECONOMICS_BUSINESS_RELATED == 1  &  genero == 'F' , na.rm = TRUE),
    TOT_ENG_ARCH_RELATED = sum(ENG_ARCH_RELATED == 1 &  genero == 'F' , na.rm = TRUE),
    TOT_FINE_ARTS = sum(FINE_ARTS == 1 &  genero == 'F' , na.rm = TRUE),
    TOT_MATHEMATICS_NATURAL_SCIENCES = sum(MATHEMATICS_NATURAL_SCIENCES == 1 &  genero == 'F' , na.rm = TRUE),
    TOT_SOCIAL_SCIENCES_HUMANITIES = sum(SOCIAL_SCIENCES_HUMANITIES == 1 &  genero == 'F' , na.rm = TRUE),
    TOT_AGRONOMY_VETERINARY_RELATED = sum(AGRONOMY_VETERINARY_RELATED == 1, na.rm = TRUE),
    TOT_EDUCATION_SCIENCES = sum(EDUCATION_SCIENCES == 1, na.rm = TRUE),
    TOT_HEALTH_SCIENCES = sum(HEALTH_SCIENCES == 1, na.rm = TRUE),
    TOT_MEDICINE = sum(MEDICINE == 1, na.rm = TRUE),
    TOT_LAW = sum(LAW == 1, na.rm = TRUE),
    TOT_NO_STUDIES = sum(NO_STUDIES == 1, na.rm = TRUE), 
    TOT_ENGINEERING = sum(Ingenieria == 1, na.rm = TRUE), 
    TOT_SCIENCE = sum(Ciencia == 1, na.rm = TRUE), 
    TOT_TECHNOLOGY = sum(Tecnologia == 1, na.rm = TRUE), 
    TOT_MATHEMATICS = sum(Matematicas == 1, na.rm = TRUE),
    TOT_UNIVERSITY= sum(University==1, na.rm=T) ,
    TOT_TECHNICAL= sum(Technical==1, na.rm=T) ,
    TOT_TECHNOLOGICAL= sum(Technological==1, na.rm=T) , 
    TOT_GROUPS = max(grupo_numeric) ) %>%
  ungroup()
# hist(classroom_summary$TOT_GROUPS)

## 1.3 Socio-Demographic Information ######### 
###### Joining SocioDemographic information
object_join = data %>%
  group_by(school_id, YEAR_INFO) %>% 
  dplyr::select( school_id, YEAR_INFO, codigo_dane_sede, grupo )%>%
  dplyr::distinct() %>%
  dplyr::rename(school_id_ = school_id)  %>%
  dplyr::rename(YEAR_INFO_ = YEAR_INFO) %>%
  dplyr::rename(codigo_dane_sede__ = codigo_dane_sede) %>%
  dplyr::rename(grupo__ = grupo) 

classroom_summary = sqldf::sqldf("
             SELECT *  FROM classroom_summary A
             INNER JOIN object_join B
             ON A.school_id = B.school_id_ and  A.YEAR_INFO=B.YEAR_INFO_
             ")
data_se = arrow::read_parquet('Data/simat_so_eco.parquet')

colnames(data_se)[1] = 'INFO_YEAR'

classroom_summary = sqldf::sqldf("
             SELECT *  FROM classroom_summary A
             INNER JOIN data_se B
             ON A.codigo_dane_sede__ = B.codigo_dane_sede and  A.grupo__ = B.grupo and   A.YEAR_INFO=B.INFO_YEAR 
             ")


colnames(classroom_summary)[39] = "TOT_STU_se"
colnames(classroom_summary)[30] = "school_id__"
classroom_summary_unique_classroom = classroom_summary[classroom_summary$TOT_GROUPS==1,]

## 1.4 Find Transitions schools ######### 
gc()
# Pipeline for schools that transitioned to coeducational
library(dplyr)
transition_years <- classroom_summary %>%
  dplyr::group_by( school_id) %>%
  dplyr::arrange(YEAR_INFO) %>%
  dplyr::mutate(
    transition_condition = TOT_FEMALE >= 0 &  # Current year has males
      dplyr::lead(TOT_FEMALE, n = 1, default = NA) > 0 & # One year after
      dplyr::lag(TOT_FEMALE, n = 2, default = NA) == 0 & # Two years prior had no males
      dplyr::lag(TOT_FEMALE, n = 1, default = NA) == 0 & # Previous year had no males
      TOT_STU > 8, # At least 7 students
    
    transition_year = ifelse(transition_condition == TRUE, YEAR_INFO, NA),
  ) %>%
  dplyr::filter(!is.na(transition_year))  %>%
  # group_by(codigo_dane_sede) %>% # Group by school
  dplyr::filter( transition_year != 2021 )  %>%  # transition_year != 2012 & min(YEAR_INFO) Exclude transitions in the first year observed for the school
  dplyr::ungroup() %>% #Ungroup school level.
  dplyr::select( school_id, transition_year )%>%
  dplyr::distinct()

# Pipeline for schools that did not transition (remained female-only)
No_transitioned_schools <- classroom_summary %>%
  dplyr::group_by(school_id) %>%
  dplyr::arrange(YEAR_INFO) %>%
  dplyr::mutate(
    transition_condition = TOT_FEMALE == 0 &  # Current year has no males
      dplyr::lead(TOT_FEMALE, n = 4, default = NA) == 0 & # 4 years after
      dplyr::lead(TOT_FEMALE, n = 3, default = NA) == 0 & # 3 years after
      dplyr::lead(TOT_FEMALE, n = 2, default = NA) == 0 & # 2 years after
      dplyr::lead(TOT_FEMALE, n = 1, default = NA) == 0 & # 1 year after
      dplyr::lag(TOT_FEMALE, n = 4, default = NA) == 0 &  # 4 years prior had no males
      dplyr::lag(TOT_FEMALE, n = 3, default = NA) == 0 &  # 3 years prior had no males
      dplyr::lag(TOT_FEMALE, n = 2, default = NA) == 0 &  # 2 years prior had no males
      dplyr::lag(TOT_FEMALE, n = 1, default = NA) == 0 &   # Previous year had no males
      TOT_STU > 8,
    male_schools = ifelse(transition_condition == TRUE, 0, 1)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::filter(male_schools == 0) %>%
  dplyr::select(school_id, male_schools) %>%
  dplyr::distinct() %>%
  dplyr::anti_join(transition_years, by = "school_id")  # Exclude schools that transitioned


# %%%%%%

No_transitioned_schools$transition_year = NaN

schools_status = rbind(transition_years,No_transitioned_schools[, c(1,3)] )
schools_status$transition_year = as.numeric(schools_status$transition_year)
# Merge transition years back into the classroom summary
merged_data <- classroom_summary %>%
  inner_join(schools_status, by = "school_id")

length(unique(merged_data$school_id))
length(unique(schools_status$school_id))



# Create treatment and relative year variables
merged_data <- merged_data %>%
  mutate(
    treated_nty = ifelse( as.numeric(YEAR_INFO) >= as.numeric(transition_year), 1, 0) ,
    treated_nt = ifelse( is.na(transition_year)==T,0 , 1) ,
    rel_year = as.numeric(YEAR_INFO) -  as.numeric(transition_year)
  )

table(merged_data$rel_year)

# Rename column (if needed)

merged_data <- merged_data %>%
  dplyr::rename(school_id_ = school_id)

merged_data$LOW_STRATA =(merged_data$ESTRATO_1+merged_data$ESTRATO_2) / merged_data$TOT_STU_se
merged_data$MID_STRATA = (merged_data$ESTRATO_3+merged_data$ESTRATO_4) / merged_data$TOT_STU_se
merged_data$HIG_STRATA = (merged_data$ESTRATO_5 +merged_data$ESTRATO_6) / merged_data$TOT_STU_se
merged_data$ESTRATO_1 =merged_data$ESTRATO_1 / merged_data$TOT_STU_se
merged_data$ESTRATO_2 =merged_data$ESTRATO_2 / merged_data$TOT_STU_se
merged_data$ESTRATO_3 =merged_data$ESTRATO_3 / merged_data$TOT_STU_se
merged_data$ESTRATO_4 =merged_data$ESTRATO_4 / merged_data$TOT_STU_se
merged_data$ESTRATO_5 =merged_data$ESTRATO_5 / merged_data$TOT_STU_se
merged_data$ESTRATO_6 =merged_data$ESTRATO_6 / merged_data$TOT_STU_se

merged_data_never_treated = merged_data
merged_data_non_treated_yet=subset(merged_data, is.na(merged_data$transition_year)==F)
merged_data_non_treated_yet = summarize_data_staggered(merged_data_never_treated)
merged_data_non_treated_yet$YEAR_INFO = as.numeric(merged_data_non_treated_yet$YEAR_INFO)
# 2. Save the data used. ####
"
1. no_yet_treated_data.parquet is the dataset that contains only those treated in any moment divided by cohorts (YEAR_INFO)
2.never_treated_data.parquet is the dataset that contains the transitioned school panel data and panel for those 
  schools that for the end of the sample holds their status as female schoools
"
arrow::write_parquet(merged_data_non_treated_yet, "Data/no_yet_treated_data_ex_males.parquet")
arrow::write_parquet(merged_data_never_treated, "Data/never_treated_data_ex_males.parquet")

gc()
summary(merged_data)
####################################
"End Data Preparation"
####################################


table(merged_data_non_treated_yet$transition_year)
# Calculate cumulative proportion of schools transitioned (if needed)
transition_summary <- merged_data_non_treated_yet[is.na(merged_data_non_treated_yet$transition_year)==F,] %>%
  group_by(transition_year) %>%
  summarize(TOT = n_distinct(school_id_) ) %>%
  mutate(TOT_fraction =TOT/sum(TOT)  )  %>%
  mutate(Cumulative_TOT = cumsum(TOT_fraction),
         no_treated_yet = 1 - Cumulative_TOT)




library(tidyr)
# Reshape the data to long format for stacked plotting
plot_data_long <- transition_summary[, c(1,4, 5)] %>%
  pivot_longer(cols = c('Cumulative_TOT', 'no_treated_yet'),
               names_to = "Status",
               values_to = "Proportion")


# Creating the stacked bar plot
plot = ggplot(plot_data_long, aes(x = factor(transition_year), y = Proportion, fill = Status)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("Cumulative_TOT" =  "lightgray", "no_treated_yet" = "steelblue" ),
                    labels = c("Treated", "Not Treated Yet")) +
  labs(x = "Transition Year", y = "Proportion of Classrooms",
       # title = "Proportion of Treated and Not-Yet-Treated Schools by Transition Year"
  ) +
  theme_minimal()
print(plot)

save_plot_png(plot, 'stagered_progression_ex_males')

#############################################################
####                ####
#############################################################
merged_data_non_treated_yet = arrow::read_parquet("Data/no_yet_treated_data_ex_males.parquet")
# merged_data_non_treated_yet = arrow::read_parquet("Data/never_treated_data_ex_males.parquet")


merged_data_non_treated_yet = merged_data_non_treated_yet[ is.na(merged_data_non_treated_yet$rel_year)==F, ]
merged_data_non_treated_yet$`STEM Fields`= 1- (merged_data_non_treated_yet$TOT_STEM_F/merged_data_non_treated_yet$TOT_STU)
merged_data_non_treated_yet$Female_fract= merged_data_non_treated_yet$TOT_FEMALE/merged_data_non_treated_yet$TOT_STU
summary(lm(data = merged_data_non_treated_yet[merged_data_non_treated_yet$rel_year>=0,] ,  `STEM Fields`~Female_fract+factor(rel_year)  ) )
merged_data_non_treated_yet$YEAR_INFO = as.numeric(merged_data_non_treated_yet$YEAR_INFO)

temp =merged_data_non_treated_yet
##########
# Estimate the Callaway & Sant'Anna staggered DiD
temp = temp[temp$rel_year<=7 | is.na(temp$rel_year),  ]
did_result <- att_gt(yname = 'STEM Fields', # Outcome variable
                     tname = "YEAR_INFO", # Time variable
                     idname = "codigo_dane_sede", # School ID
                     gname = "transition_year", # Treatment group (year of transition)
                     control_group =  "notyettreated",
                     data = temp,
                     base_period = "universal",
                     # base_period = -2,
                     panel = F,
                     # xformla = ~intensity_event_time, #1, # No covariates in this example (you can add them)
                     est_method = "dr",  # "dr" for doubly robust estimation
                     bstrap = TRUE,
                     biters = 10000, # Recommended: increase for more reliable p-values
                     cband = TRUE
) 

print(summary(did_result))
print(ggdid(did_result))
agg.simple <- aggte(did_result, type = "simple", na.rm = TRUE)
# print(summary(agg.simple))
agg.dynamic <- aggte(did_result, type = "dynamic"  , min_e = 0, max_e = 6, na.rm = TRUE ) #
# print(summary(agg.dynamic))
# ggdid(agg.dynamic)

agg.calendar <- aggte(did_result, type = "calendar", na.rm = TRUE)
STEM_ex_males =list(simple = agg.simple, 
     dynamic = agg.dynamic, 
     calendar = agg.calendar, 
     conditional_pretest = did_result)

#####
STEM_ex_males = ATT_sim_dyn_CS( 'STEM Fields', temp )
save_img_callaway_dynamic(STEM_ex_males,  'STEM Fields\nEx male-schools')

