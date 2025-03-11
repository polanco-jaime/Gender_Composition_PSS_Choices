



source("Scripts/R/genereal_settings.R", echo=TRUE)
source("./Scripts/R/functions.R", echo=TRUE)
source("./Scripts/R/read_data.R", echo=TRUE)


outcomes = c(
  'ECONOMICS_BUSINESS_RELATED' ,
  'ENG_ARCH_RELATED',
  'FINE_ARTS',
  'MATHEMATICS_NATURAL_SCIENCES',
  'SOCIAL_SCIENCES_HUMANITIES',
  'AGRONOMY_VETERINARY_RELATED',
  'EDUCATION_SCIENCES',
  'HEALTH_SCIENCES',
  'NO_STUDIES',
  'MEDICINE',
  'LAW' ,
  'STEM',
  'NO_STEM'
)
gc()
# data = sqldf::sqldf("SELECT * ,
# codigo_dane_sede ||' - ' || YEAR_INFO fe_group 
# FROM data")
data$fe_group = paste0(data$codigo_dane_sede, ' - ' , data$YEAR_INFO)

gc()
colnames(data)
table(data$SCHOOL_STATUS)
###################################################################
#
####################################################################
# Convert your data to a data.table
library(data.table)


# Convert your data to a data.table
data <- as.data.table(data)
result <- data[, .(
  TOT_MALE = sum(genero == 'M', na.rm = TRUE),
  TOT_FEMALE = sum(genero == 'F', na.rm = TRUE),
  TOT_STU = uniqueN(.SD),
  AVG_AGE = mean(EDAD, na.rm = TRUE),
  TOT_STEM = 100*sum(  genero == 'F' & STEM==1, na.rm = TRUE)/sum(genero == 'F', na.rm = TRUE),
  TOT_NO_STEM = 100*sum(  genero == 'F' & NO_STEM==1, na.rm = TRUE)/sum(genero == 'F', na.rm = TRUE),
  TOT_ECONOMICS_BUSINESS_RELATED = 100*sum(  genero == 'F' & EDUCATION_SCIENCES==1, na.rm = TRUE)/sum(genero == 'F', na.rm = TRUE),
  TOT_ENG_ARCH_RELATED= 100*sum(  genero == 'F' & ENG_ARCH_RELATED==1, na.rm=T)/sum(genero == 'F', na.rm = TRUE),
  TOT_FINE_ARTS= 100*sum(  genero == 'F' & FINE_ARTS==1, na.rm=T)/sum(genero == 'F', na.rm = TRUE),
  TOT_MATHEMATICS_NATURAL_SCIENCES= 100*sum(  genero == 'F' & MATHEMATICS_NATURAL_SCIENCES==1, na.rm=T)/sum(genero == 'F', na.rm = TRUE),
  TOT_SOCIAL_SCIENCES_HUMANITIES= 100*sum(  genero == 'F' & SOCIAL_SCIENCES_HUMANITIES==1, na.rm=T)/sum(genero == 'F', na.rm = TRUE),
  TOT_AGRONOMY_VETERINARY_RELATED= 100*sum(  genero == 'F' & AGRONOMY_VETERINARY_RELATED==1, na.rm=T)/sum(genero == 'F', na.rm = TRUE),
  TOT_EDUCATION_SCIENCES= 100*sum(  genero == 'F' & EDUCATION_SCIENCES==1, na.rm=T)/sum(genero == 'F', na.rm = TRUE),
  TOT_HEALTH_SCIENCES= 100*sum(  genero == 'F' & HEALTH_SCIENCES==1, na.rm=T)/sum(genero == 'F', na.rm = TRUE),
  TOT_MEDICINE= 100*sum(  genero == 'F' & MEDICINE==1, na.rm=T)/sum(genero == 'F', na.rm = TRUE),
  TOT_LAW= 100*sum(  genero == 'F' & LAW==1, na.rm=T)/sum(genero == 'F', na.rm = TRUE),
  TOT_NO_STUDIES= 100*sum(  genero == 'F' & NO_STUDIES==1, na.rm=T)/sum(genero == 'F', na.rm = TRUE)
), by = .(codigo_dane_sede, grupo, YEAR_INFO)]
gc()
rm(data)
gc()
# Function to check if a school transitioned to co-ed in a given year
# check_transition <- function(school_data, year) {
#   # Exclude the first reported year for the school
#   first_year <- min(school_data$YEAR_INFO)
#   school_data <- school_data[school_data$YEAR_INFO > first_year, ]
#   
#   # Filter classrooms with more than 6 students
#   school_data <- school_data[school_data$TOT_STU > 6, ]
#   
#   # Check if the school had only female students before the given year
#   before_year <- school_data[school_data$YEAR_INFO < year, ]
#   all_female_before <- all(before_year$TOT_MALE == 0)
#   
#   # Check if the school had at least one male student in the given year
#   in_year <- school_data[school_data$YEAR_INFO == year, ]
#   at_least_one_male <- any(in_year$TOT_MALE > 0)
#   
#   # Return TRUE if the school transitioned, FALSE otherwise
#   return(all_female_before & at_least_one_male & nrow(before_year) > 0)
# }
# 
# # Find schools that transitioned to co-ed (with the new conditions)
# transition_schools <- unique(result$codigo_dane_sede)
# 
# transition_years <- c()
# 
# for (school in transition_schools) {
#   school_data <- result[result$codigo_dane_sede == school, ]
#   years <- unique(school_data$YEAR_INFO)
#   
#   for (year in years) {
#     if (check_transition(school_data, year)) {
#       transition_years <- c(transition_years, 
#                             paste(school, year, sep = "_"))
#       break # Stop checking after the first transition year
#     }
#   }
# }

data$grupo_numeric = as.numeric(factor(data$grupo, levels = sort(unique(data$grupo)) ))
data$school_id = paste0(data$grupo_numeric ,"-",  data$codigo_dane_sede)

data = data[, c(1:3,62,4:61,63)]

classroom_summary <- data %>%
  group_by(codigo_dane_sede, grupo_numeric, YEAR_INFO) %>%
  summarize(
    TOT_MALE = sum(genero == 'M', na.rm = TRUE),
    TOT_FEMALE = sum(genero == 'F', na.rm = TRUE),
    TOT_STU = n(),  # More efficient way to count students
    AVG_AGE = mean(EDAD, na.rm = TRUE),
    TOT_STEM = sum(STEM == 1, na.rm = TRUE), # No need to multiply by 100 or filter by gender here
    TOT_NO_STEM = sum(NO_STEM == 1, na.rm = TRUE),
    TOT_ECONOMICS_BUSINESS_RELATED = sum(ECONOMICS_BUSINESS_RELATED == 1, na.rm = TRUE),
    TOT_ENG_ARCH_RELATED = sum(ENG_ARCH_RELATED == 1, na.rm = TRUE),
    TOT_FINE_ARTS = sum(FINE_ARTS == 1, na.rm = TRUE),
    TOT_MATHEMATICS_NATURAL_SCIENCES = sum(MATHEMATICS_NATURAL_SCIENCES == 1, na.rm = TRUE),
    TOT_SOCIAL_SCIENCES_HUMANITIES = sum(SOCIAL_SCIENCES_HUMANITIES == 1, na.rm = TRUE),
    TOT_AGRONOMY_VETERINARY_RELATED = sum(AGRONOMY_VETERINARY_RELATED == 1, na.rm = TRUE),
    TOT_EDUCATION_SCIENCES = sum(EDUCATION_SCIENCES == 1, na.rm = TRUE),
    TOT_HEALTH_SCIENCES = sum(HEALTH_SCIENCES == 1, na.rm = TRUE),
    TOT_MEDICINE = sum(MEDICINE == 1, na.rm = TRUE),
    TOT_LAW = sum(LAW == 1, na.rm = TRUE),
    TOT_NO_STUDIES = sum(NO_STUDIES == 1, na.rm = TRUE)
    
  ) %>%
  ungroup()



classroom_summary <- classroom_summary %>%
  group_by(codigo_dane_sede, YEAR_INFO) %>%
  mutate(grupo_numeric = as.numeric(factor(grupo, levels = sort(unique(grupo)))) # ,
         # school_id = paste0(grupo_numeric , codigo_dane_sede),
         # school_id = paste0('School-',as.character(as.numeric(factor(school_id, levels = sort(unique(school_id)))) ) )
  )%>%
  ungroup()

classroom_summary$school_id = paste0(classroom_summary$grupo_numeric ,"-",  classroom_summary$codigo_dane_sede)

library(dplyr)
transition_years <- classroom_summary %>%
  group_by( codigo_dane_sede) %>%
  arrange(YEAR_INFO) %>%
  mutate(
    transition_condition = TOT_MALE > 0 &  # Current year has males
      lead(TOT_MALE, n = 2, default = NA) > 0 & # Two year after
      lead(TOT_MALE, n = 1, default = NA) > 0 &
      lag(TOT_MALE, n = 1, default = NA) == 0 & # Previous year had no males
      # lag(TOT_MALE, n = 2, default = NA) == 0 & # Two years prior had no males
      TOT_STU > 6, # At least 7 students
    
    transition_year = ifelse(transition_condition == TRUE, YEAR_INFO, NA)
  ) %>%
  filter(!is.na(transition_year))  %>%
  group_by(codigo_dane_sede) %>% # Group by school
  filter(transition_year != min(YEAR_INFO))  %>%  # Exclude transitions in the first year observed for the school
  ungroup() %>% #Ungroup school level.
  select( codigo_dane_sede, grupo_numeric, transition_year, school_id)%>%
  distinct()



# Print the schools and their transition years
print(transition_years)

# # Extract the school codes and years
# transition_df <- data.frame(
#   codigo_dane_sede = as.numeric(sapply(strsplit(transition_years, "_"), `[`, 1)),
#   transition_year = as.numeric(sapply(strsplit(transition_years, "_"), `[`, 2))
# )

# sqldf(" SELECT * FROM transition_df")
# #Checking
# sqldf( "SELECT * FROM  result WHERE codigo_dane_sede = 268318000215 ORDER BY 3")

merged_data = sqldf( "SELECT * FROM  result A
        INNER JOIN transition_df B
        ON A.codigo_dane_sede = B.codigo_dane_sede ")






# Create a treatment dummy variable
merged_data$treated <- ifelse(merged_data$YEAR_INFO >= merged_data$transition_year, 1, 0)
merged_data$YEAR_INFO = as.numeric(merged_data$YEAR_INFO )
# Create a time-to-treatment variable (relative years since treatment)
merged_data$rel_year <- merged_data$YEAR_INFO - merged_data$transition_year
 
colnames(merged_data)[1] = "codigo_dane_sede_"
c("codigo_dane_sede","grupo","YEAR_INFO", 'transition_year'   )
sqldf::sqldf("SELECT  transition_year , COUNT(DISTINCT codigo_dane_sede_) /137 TOT FROM  merged_data
             GROUP BY 1
             ")
merged_data %>%
  group_by(transition_year) %>%
  summarise(TOT = n_distinct(codigo_dane_sede_) / 137) %>%
  mutate(Cumulative_TOT = cumsum(TOT))

# Assuming 'merged_data' contains the raw data
plot_data <- merged_data %>%
  group_by(transition_year) %>%
  summarise(TOT = n_distinct(codigo_dane_sede_) / 137) %>%
  mutate(Cumulative_TOT = cumsum(TOT))

# Creating the stacked bar plot
ggplot(plot_data, aes(x = factor(transition_year), y = Cumulative_TOT)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = "Transition Year", y = "Cumulative Treated Schools",
       title = "Cumulative Number of Treated Schools by Transition Year") +
  theme_minimal()


plot_data <- merged_data %>%
  group_by(transition_year) %>%
  summarise(TOT = n_distinct(codigo_dane_sede_) / 137) %>%
  mutate(Cumulative_TOT = cumsum(TOT),
         no_treated_yet = 1 - Cumulative_TOT)
library(tidyr)
# Reshape the data to long format for stacked plotting
plot_data_long <- plot_data %>%
  pivot_longer(cols = c(Cumulative_TOT, no_treated_yet),
               names_to = "Status",
               values_to = "Proportion")

# Creating the stacked bar plot
plot = ggplot(plot_data_long, aes(x = factor(transition_year), y = Proportion, fill = Status)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("Cumulative_TOT" = "steelblue", "no_treated_yet" = "lightgray"),
                    labels = c("Treated", "Not Treated Yet")) +
  labs(x = "Transition Year", y = "Proportion of Classrooms",
       # title = "Proportion of Treated and Not-Yet-Treated Schools by Transition Year"
       ) +
  theme_minimal()
print(plot)
save_plot_png(plot, 'stagered_progression')
# %%%%
##################################################
#
##################################################
# merged_data
# The columns that identify my classrooom are: 'codigo_dane_sede_', 'grupo', where codigo_dane_sede_ refeers to the school and grupo refers to the classroom in the school
# YEAR_INFO refers the year of observation, while transition_year refers the year in which this classrrom transitioned from female school to coeducational.
# TOT_FEMALE Provide me the numer of female students by group and AVG_AGE refers to the average age of each classroom
# TOT_STEM, TOT_NO_STEM, TOT_ECONOMICS_BUSINESS_RELATED, TOT_ENG_ARCH_RELATED, TOT_FINE_ARTS, TOT_MATHEMATICS_NATURAL_SCIENCES, TOT_SOCIAL_SCIENCES_HUMANI, TOT_AGRONOMY_VETERINARY_RELATED TOT_EDUCATION_SCIENCES TOT_HEALTH_SCIENCES TOT_MEDICINE   TOT_LAW TOT_NO_STUDIES  are the variables related to the post secondary major choice



library(dplyr)
library(tidyr)
library(knitr)

# Filter data for one year prior to transition

pre_transition_data <- merged_data %>%
  filter(YEAR_INFO == transition_year - 1)

pre_transition_data <- pre_transition_data %>%
  mutate(classroom_id = paste0(codigo_dane_sede_, "_", grupo))

colnames(pre_transition_data)
# Function to calculate summary statistics and proportions
summarize_data <- function(data) {
  data %>%
    summarize( 
      `Female Students (N)` = mean(TOT_FEMALE, na.rm = TRUE),
      `Average Age (Years)` = mean(AVG_AGE, na.rm = TRUE),
      `Non-STEM Fields` = mean(TOT_NO_STEM / (TOT_STEM + TOT_NO_STEM ), na.rm = TRUE) ,
      `STEM Fields` = mean(TOT_STEM / (TOT_STEM + TOT_NO_STEM ), na.rm = TRUE) ,
      `Health Sciences` = mean(TOT_HEALTH_SCIENCES / (TOT_STEM + TOT_NO_STEM ), na.rm = TRUE) ,
      `Social Sciences/Humanities` = mean(TOT_SOCIAL_SCIENCES_HUMANITIES / (TOT_STEM + TOT_NO_STEM ), na.rm = TRUE) ,
      `Law` = mean(TOT_LAW/ TOT_STU, na.rm = TRUE) ,
      `Education Sciences` = mean(TOT_EDUCATION_SCIENCES/ TOT_STU, na.rm = TRUE) ,
      `Economics/Business` = mean(TOT_ECONOMICS_BUSINESS_RELATED/ TOT_STU, na.rm = TRUE) ,
      `Engineering/Architecture` = mean(TOT_ENG_ARCH_RELATED/ TOT_STU, na.rm = TRUE) ,
      `Fine Arts` = mean(TOT_FINE_ARTS/ TOT_STU, na.rm = TRUE) ,
      `Mathematics/Natural Sciences` = mean(TOT_MATHEMATICS_NATURAL_SCIENCES/ TOT_STU, na.rm = TRUE) ,
      `Agronomy/Veterinary` = mean(TOT_AGRONOMY_VETERINARY_RELATED/ TOT_STU, na.rm = TRUE) ,
      `Not Continuing Education` = mean(TOT_NO_STUDIES/ TOT_STU, na.rm = TRUE) 
     
    )
}


# Group data by year and classroom and calculate summary stats and counts
summary_by_year <- pre_transition_data %>%
  group_by(YEAR_INFO, classroom_id) %>%  
  summarize_data() %>%
  ungroup() %>%
  group_by(YEAR_INFO) %>%
  summarize(across(where(is.numeric), ~mean(., na.rm = TRUE))) %>% # Calculate mean only for numeric columns
  ungroup()

n_classroom <- pre_transition_data %>%
  group_by(YEAR_INFO) %>%
  summarize(
    `N (Classrooms)` = n_distinct(classroom_id)
  ) %>%
  ungroup()

summary_by_year = cbind(summary_by_year, n_classroom)
summary_by_year = summary_by_year[ , c(1:15,17) ]
# Reshape data for table
summary_table <- summary_by_year %>%
  pivot_longer(cols = -YEAR_INFO, names_to = "Variable", values_to = "Mean") %>%
  pivot_wider(names_from = YEAR_INFO, values_from = Mean)


# Number of schools transitioned by year
schools_transitioned <- pre_transition_data %>%
  group_by(YEAR_INFO) %>%
  summarize(`Number of Schools Transitioned` = n_distinct(codigo_dane_sede_)) %>%
  pivot_wider(names_from = YEAR_INFO, values_from = `Number of Schools Transitioned`)


# Combine school counts and summary statistics
final_table_data <- bind_rows(schools_transitioned, summary_table)

final_table_data
# Create the kable table
final_table <- kable(final_table_data, format = "latex", booktabs = TRUE, digits = 2,
                     caption = "Descriptive Statistics of Ex-Female Schools One Year Prior to Their Transition to Coeducational School (Cohorts from 2012-2020)",
                     label = "baseline_desc") %>%
  kable_styling(latex_options = "scale_down") # Adjust scaling as needed



# Print the table (or save to file)
print(final_table)


#Example to save in a specific path
# save_kable(final_table, file = "path/to/your/table.tex")


