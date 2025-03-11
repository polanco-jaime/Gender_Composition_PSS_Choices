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

# Pipeline for schools that transitioned to coeducational
library(dplyr)
transition_years <- classroom_summary %>%
  dplyr::group_by( school_id) %>%
  dplyr::arrange(YEAR_INFO) %>%
  dplyr::mutate(
    transition_condition = TOT_MALE >= 0 &  # Current year has males
      # dplyr::lead(TOT_MALE, n = 2, default = NA) > 0 & # Two year after
      dplyr::lead(TOT_MALE, n = 1, default = NA) > 0 & # One year after
      # dplyr::lag(TOT_MALE, n = 3, default = NA) > 0 & # Three years prior had no males
      dplyr::lag(TOT_MALE, n = 2, default = NA) == 0 & # Two years prior had no males
      dplyr::lag(TOT_MALE, n = 1, default = NA) == 0 & # Previous year had no males
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
    transition_condition = TOT_MALE == 0 &  # Current year has no males
      dplyr::lead(TOT_MALE, n = 4, default = NA) == 0 & # 4 years after
      dplyr::lead(TOT_MALE, n = 3, default = NA) == 0 & # 3 years after
      dplyr::lead(TOT_MALE, n = 2, default = NA) == 0 & # 2 years after
      dplyr::lead(TOT_MALE, n = 1, default = NA) == 0 & # 1 year after
      dplyr::lag(TOT_MALE, n = 4, default = NA) == 0 &  # 4 years prior had no males
      dplyr::lag(TOT_MALE, n = 3, default = NA) == 0 &  # 3 years prior had no males
      dplyr::lag(TOT_MALE, n = 2, default = NA) == 0 &  # 2 years prior had no males
      dplyr::lag(TOT_MALE, n = 1, default = NA) == 0 &   # Previous year had no males
      TOT_STU > 8,
    Female_schools = ifelse(transition_condition == TRUE, 0, 1)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::filter(Female_schools == 0) %>%
  dplyr::select(school_id, Female_schools) %>%
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
arrow::write_parquet(merged_data_non_treated_yet, "Data/no_yet_treated_data.parquet")
arrow::write_parquet(merged_data_never_treated, "Data/never_treated_data.parquet")

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

save_plot_png(plot, 'stagered_progression')

# / n_distinct(merged_data $codigo_dane_sede_)) %>% # Proportion of all transitioning schools
#   mutate(Cumulative_TOT = cumsum(TOT))
#############################################################
#
#############################################################


library(dplyr)
library(tidyr)
library(knitr)

# Filter data for one year prior to transition
 
pre_transition_data <- merged_data %>%
  filter(as.numeric(YEAR_INFO) == as.numeric(transition_year)  - 1) %>%
  filter(as.numeric(TOT_MALE) ==0) 
colnames(pre_transition_data)

# pre_transition_data <- pre_transition_data %>%
#   mutate(classroom_id = paste0(codigo_dane_sede_, "_", grupo_numeric.x))
table(pre_transition_data$YEAR_INFO)


# pre_transition_data = pre_transition_data[ , c(1:20,22:24)]
colnames(pre_transition_data)
# Function to calculate summary statistics and proportions

#             summarize_data(pre_transition_data)
#             # summarize_data_if_exist(pre_transition_data)
#             summary_by_year <- pre_transition_data %>%
#               group_by(transition_year) %>%  
#               summarize_data_if_exist() %>%
#               ungroup() %>%
#               group_by(transition_year) 
# # pre_transition_data = pre_transition_data[   pre_transition_data['TOT_STU'] <=100 , ]
#  

summary_by_year <- pre_transition_data %>%
  group_by(transition_year) %>%  
  summarize_data() %>%
  ungroup() %>%
  group_by(transition_year) # %>%
  # summarize(across(where(is.numeric), ~ round(mean(., na.rm = TRUE), 3) )) %>% # Calculate mean only for numeric columns
  # ungroup()
summary_by_year
library(kableExtra)
 
# Reshape the data for the table
summary_table <- summary_by_year %>%
  tidyr::pivot_longer(cols = -transition_year, names_to = "Variable", values_to = "Mean") %>%
  tidyr::pivot_wider(names_from = transition_year, values_from = Mean)
print(summary_table)
# Number of schools transitioned by year (adjust as needed based on your transition logic)
schools_transitioned <- pre_transition_data %>%
  group_by(transition_year) %>%
  summarize(`Number of Schools Transitioned` = as.character(n_distinct(codigo_dane))  ) %>% # Use school_id_ if that's your school identifier
  tidyr::pivot_wider(names_from = transition_year, values_from = `Number of Schools Transitioned`)

# Add row for N classrooms
n_classrooms<-pre_transition_data %>%
  group_by(transition_year) %>%
  summarize(`N (Classrooms)` = as.character( n_distinct(school_id_)) ) %>% # Use classroom_id
  tidyr::pivot_wider(names_from = transition_year, values_from = `N (Classrooms)`)

schools_transitioned

# Combine all rows for the table
final_table_data <- bind_rows(schools_transitioned, n_classrooms, summary_table)


# Create the kable table with improved formatting
final_table <- final_table_data[ , c(8, 1:7)] %>%
  kable(format = "latex", booktabs = TRUE, digits = 2, linesep = "", # linesep = "" removes extra spacing
        caption = "Descriptive Statistics of Ex-Female Schools Two Year Prior to Their Transition to Coeducational School (Cohorts from 2012-2020)", 
        label = "baseline_desc") %>%
  kable_styling(latex_options =  "basic" ) %>% # Add styling options like striped rows, scale_down
  add_header_above(c(" " = 1, "Transition Year" = (ncol(final_table_data) - 1))) %>% # Header for years
  pack_rows("Classroom Attributes", 2, 3, latex_gap_space = "0.5em") %>%        # Grouping rows
  pack_rows("Post-secondary Major Choice Proportion (\\%)", 4, 16, latex_gap_space = "0.5em") %>% # Grouping rows
  footnote(general = "Values represent the mean for Classrooms \\textit{two year prior to their transition} to coeducational status, with standard deviations in parentheses. The ``Number of Schools Transitioned'' row indicates the number of schools that transitioned in the following year (e.g., the value in the 2011 column represents schools that transitioned in 2013). Percentages indicate the proportion of students choosing a specific field among those continuing their education. Data sources: SIMAT and SNIES.",
           threeparttable = TRUE, escape = FALSE) # Custom footnote

final_table_data
tot_schools=85+59+46+47+52+79+68 
tot_classrooms = 90+62+46+49+56+83+73      
# Print or save the table
print(final_table)
stem_ = (0.177 +0.23 +0.177 +0.166+0.226 +0.185 +0.157)/7
# cat(final_table, file = "path/to/your/table.tex") # Save to file if needed
# Assuming df is your original table loaded in R
 
df_long <- summary_table %>%
  pivot_longer(cols = -Variable, names_to = "Year", values_to = "Value") %>%
  separate(Value, into = c("Mean", "SD"), sep = " ", convert = TRUE) %>%
  mutate(
    Mean = as.numeric(Mean),
    SD = as.numeric(gsub("[()]", "", SD)),  # Remove parentheses and convert to numeric
    Year = as.integer(Year)
  )

# Calculate confidence intervals (assuming 95% CI, z = 1.96)
df_long <- df_long %>%
  mutate(
    CI_Lower = Mean - 1.96 * (SD / sqrt(n())),  # Replace n() with the actual sample size
    CI_Upper = Mean + 1.96 * (SD / sqrt(n()))
  )

for (i  in unique(df_long$Variable)) {
  # Plotting with ggplot2
  plot_ = ggplot(df_long[ df_long['Variable']==i ,], aes(x = Year, y = Mean)) +
    geom_line(size = 1) +                            # Line plot for the mean values
    geom_ribbon(aes(ymin = CI_Lower, ymax = CI_Upper), fill = "blue", alpha = 0.2) +  # Shaded confidence interval
    labs(
      # title = "Trends with Confidence Intervals by Variable",
      x = "Year",
      y = "Mean Value"
    ) +
    facet_wrap(~ Variable, scales = "free_y") +      # Facet by Variable
    theme_minimal() +                                # Clean theme
    theme(
      plot.title = element_text(size = 16, face = "bold"),
      axis.title = element_text(size = 14),
      strip.text = element_text(size = 12),
      legend.position = "none"                       # Hide legend since each plot is for one variable
    )
  print(plot_)
}



####################
# Plot by Cohorts

# Load necessary libraries
library(ggplot2)
library(dplyr)

# Ensure that transition_year is a factor for coloring purposes
merged_data_ =  (merged_data[!merged_data$transition_year=='2020', ])

merged_data_ <- merged_data_ %>%
  mutate(transition_year = as.factor(transition_year),
         YEAR_INFO = as.numeric(YEAR_INFO))
merged_data_ =  (merged_data_[merged_data_$YEAR_INFO<=2019, ])
unique(merged_data_$transition_year)
# Calculate mean and confidence interval for TOT_STEM_ by transition_year and YEAR_INFO
summary_data <- merged_data_ %>%
  group_by(transition_year, YEAR_INFO) %>%
  summarise(
    mean_TOT_STEM = mean(TOT_STEM_, na.rm = TRUE),
    ci = qt(0.975, df=n()-1) * sd(TOT_STEM_, na.rm = TRUE) / sqrt(n()),
    .groups = 'drop'
  )

# Create the line plot with confidence intervals
ggplot(na.omit(summary_data), aes(x = YEAR_INFO, y = mean_TOT_STEM, color = transition_year, group = transition_year)) +
  # Line plot for each transition_year
  geom_line(size = 1) +
  # Confidence interval ribbons
  geom_ribbon(aes(ymin = mean_TOT_STEM - ci, ymax = mean_TOT_STEM + ci, fill = transition_year), alpha = 0.05) +
  # Vertical lines for each transition_year
  geom_vline(data = summary_data %>% distinct(transition_year), 
             aes(xintercept = as.numeric(as.character(transition_year)), color = transition_year),
             linetype = "dashed", size = 1) +
  # Customizing the plot appearance
  labs(
    x = "Year",
    y = "Average TOT_STEM",
    title = "Average TOT_STEM by Year for Different Transition Periods"
  ) +
  theme_minimal() +
  theme(legend.position = "right") +
  scale_color_brewer(palette = "Set1") +  # You can choose another palette here
  scale_fill_brewer(palette = "Set1")     # Ensuring fill colors match line colors

cohort_data = na.omit(summary_data[summary_data$YEAR_INFO!=2020,])
 
# Plot with facets for each transition_year
plot = ggplot(cohort_data, aes(x = YEAR_INFO, y = mean_TOT_STEM, color = transition_year, group = transition_year)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = mean_TOT_STEM - ci, ymax = mean_TOT_STEM + ci, fill = transition_year), alpha = 0.05) +
  geom_vline(data = summary_data %>% distinct(transition_year), 
             aes(xintercept = as.numeric(as.character(transition_year)), color = transition_year),
             linetype = "dashed", size = 1) +
  labs(
    x = "Year",
    y = "Average Female Enrollment in STEM",
    title = "Average Female Enrollment in STEM by Year for Different Transition Periods"
  ) +
  theme_minimal() +
  theme(legend.position = "right") +
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1") +
  facet_wrap(~ transition_year, scales = "free_y")  # Separate panel for each transition_year

save_plot_png(plot, 'cohorts_plot_stem')
