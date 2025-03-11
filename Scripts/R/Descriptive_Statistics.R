# Convert your data to a data.table
library(data.table)


source("Scripts/R/genereal_settings.R", echo=TRUE)
source("./Scripts/R/functions.R", echo=TRUE)
source("./Scripts/R/read_data.R", echo=TRUE)

 
# Convert your data to a data.table
data <- as.data.table(data)
colnames(data)
# data_m <- data[data$genero =='M', ] 
# data_F <- data[data$genero =='F', ] 
# Perform the aggregation using data.table syntax
result <- data[, .(
  TOT_MALE = sum(genero == 'M', na.rm = TRUE),
  TOT_FEMALE = sum(genero == 'F', na.rm = TRUE),
  TOT_STU = uniqueN(.SD),
  AVG_AGE = mean(EDAD, na.rm = TRUE),
  TOT_STEM = sum(STEM==1, na.rm = TRUE)/uniqueN(.SD),
  TOT_NO_STEM = sum(NO_STEM==1, na.rm = TRUE)/uniqueN(.SD),
  TOT_ECONOMICS_BUSINESS_RELATED = sum(EDUCATION_SCIENCES==1, na.rm = TRUE)/uniqueN(.SD),
  TOT_ENG_ARCH_RELATED= sum(ENG_ARCH_RELATED==1, na.rm=T)/uniqueN(.SD),
  TOT_FINE_ARTS= sum(FINE_ARTS==1, na.rm=T)/uniqueN(.SD),
  TOT_MATHEMATICS_NATURAL_SCIENCES= sum(MATHEMATICS_NATURAL_SCIENCES==1, na.rm=T)/uniqueN(.SD),
  TOT_SOCIAL_SCIENCES_HUMANITIES= sum(SOCIAL_SCIENCES_HUMANITIES==1, na.rm=T)/uniqueN(.SD),
  TOT_AGRONOMY_VETERINARY_RELATED= sum(AGRONOMY_VETERINARY_RELATED==1, na.rm=T)/uniqueN(.SD),
  TOT_EDUCATION_SCIENCES= sum(EDUCATION_SCIENCES==1, na.rm=T)/uniqueN(.SD),
  TOT_HEALTH_SCIENCES= sum(HEALTH_SCIENCES==1, na.rm=T)/uniqueN(.SD),
  TOT_MEDICINE= sum(MEDICINE==1, na.rm=T)/uniqueN(.SD),
  TOT_LAW= sum(LAW==1, na.rm=T)/uniqueN(.SD),
  TOT_NO_STUDIES= sum(NO_STUDIES==1, na.rm=T)/uniqueN(.SD)
), by = .(codigo_dane_sede, grupo, YEAR_INFO, TREATMENT_TIME)]

 

result$MALE_PROPORTION <-  result$TOT_MALE / result$TOT_STU
result$FEMALE_PROPORTION <- result$TOT_FEMALE / result$TOT_STU

rm(data)

summary(result$FEMALE_PROPORTION)
result$year_ref <- as.numeric(result$YEAR_INFO )
result$TREATMENT_TIME  = as.numeric(result$TREATMENT_TIME)

############################
# Barplot Gender Composition
###########################
result_ = result[ !is.na(result$TREATMENT_TIME),]
result_$TREATMENT_TIME
result_= result_[result_$year_ref == result_$TREATMENT_TIME - 1 ,]
table(result_$year_ref)
# Calculate summary statistics only for year_ref = TREATMENT_TIME - 1
summary_data <- result_ %>%   
  group_by(year_ref) %>%
  summarize(
    mean_proportion = mean(FEMALE_PROPORTION, na.rm = TRUE),
    sd_proportion = sd(FEMALE_PROPORTION, na.rm = TRUE),
    n = n(),
    se_proportion = sd_proportion / sqrt(n),
    lower_ci = mean_proportion - 1.96 * se_proportion, # 95% CI
    upper_ci = mean_proportion + 1.96 * se_proportion
  )

# Calculate summary statistics
summary_data <- result_ %>%
  group_by(year_ref) %>%
  summarize(
    mean_proportion = mean(FEMALE_PROPORTION, na.rm = T),
    sd_proportion = sd(FEMALE_PROPORTION, na.rm = T),
    n = n(),
    se_proportion = sd_proportion / sqrt(n),
    lower_ci = mean_proportion - 1.96 * se_proportion, # 95% CI
    upper_ci = mean_proportion + 1.96 * se_proportion
  )

ggplot(summary_data, aes(x = year_ref, y = mean_proportion)) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2, color = "#FF8080") +  # Red error bars
  labs(x = "Year",
       y = "Mean Female Proportion",
       title = paste0("Mean Female Proportion by Year\n")) + # Title like other plots
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14)) +
  scale_x_continuous(breaks = unique(summary_data %>% pull(year_ref))) +  # Integer breaks for year
  coord_cartesian(ylim = c(0.50, 0.55))


############################
# Histogram Gender Composition
###########################
 
mynamestheme <- theme(
  plot.title = element_text(family = "Helvetica", face = "bold", size = 15, hjust = 0.5, vjust = 0.5),
  axis.title = element_text(family = "Helvetica", face = "bold", size = 12, colour = "steelblue4"),
  axis.text = element_text(family = "Courier", face = "bold", colour = "cornflowerblue", size = 12),
  panel.background = element_rect(fill = "white"),
  panel.grid.major = element_blank(),  # Remove major grid lines
  panel.grid.minor = element_blank(),  # Remove minor grid lines
  axis.line = element_line(color = "steelblue4", size = 0.5),
  legend.position = "bottom",
  strip.background = element_blank(),  # Remove gray background from facet labels
  strip.text = element_text(color = "steelblue4", face = "bold")  # Set facet text color to match axis title
)
# Histogram plot
png(paste0(graphs_dir,'histogram_freq_plot.png'),  width = 1030, height = 598)
ggplot(result, aes(x = FEMALE_PROPORTION)) +
  geom_histogram(aes(y = ..count.. / sum(..count..)), 
                 binwidth = 0.1, fill = "#FF8080", color = "black") +  # Matching fill color
  facet_wrap(~ YEAR_INFO, ncol = 2) +
  labs(#title = "Distribution of Female Student Proportion by Transition Year (Frequency)",
       x = "Proportion of Female Students",
       y = "Relative Frequency") +  # Updated y-axis label
  theme_light() + 
  mynamestheme  # Applying the custom theme
dev.off() 
 
ggplot(result, aes(x = FEMALE_PROPORTION)) +
  geom_histogram(aes(y = ..count.. / sum(..count..)), 
                 binwidth = 0.1, fill = "#FF8080", color = "black")  + # Frequency polygon
  geom_density(aes(y = ..density../100 ), fill = "#FF8080", alpha = 0.2) + # Scaled density
  facet_wrap(~ YEAR_INFO, ncol = 4) +
  labs(#title = "Distribution of Female Student Proportion by Transition Year (Frequency)",
       x = "Proportion of Female Students",
       y = "Frequency") +
  theme_bw()+ 
  mynamestheme 


ggplot(result, aes(x = FEMALE_PROPORTION)) +
  geom_freqpoly(aes(y = ..count../sum(..count..)), binwidth = 0.1, color = "skyblue") + # Frequency polygon
  geom_density(aes(y = ..density.. * sum(..count..) * 0.1), fill = "red", alpha = 0.2) + # Scaled density
  facet_wrap(~ YEAR_INFO, ncol = 3) +
  labs(title = "Distribution of Female Student Proportion by Transition Year (Frequency)",
       x = "Proportion of Female Students",
       y = "Frequency") +
  theme_bw()
# choose school with more than 6 students
colnames(data)
#####
result = result[result$TOT_STU>=6]
summary(result$TOT_STEM)
mean(result$TOT_STEM, na.rm = T)
sd(result$TOT_STEM, na.rm = T)
sum(result$TOT_STU)
length(unique(result$codigo_dane_sede))
summary(result[result$TOT_STU>=6]$MALE_PROPORTION)
result <- result %>% mutate(DISTRIBUTION = case_when(
    MALE_PROPORTION < 36.36  ~ '1st Qu.',
    MALE_PROPORTION >= 36.36 & MALE_PROPORTION < 47.06 ~ '2nd Qu.',
    MALE_PROPORTION >= 47.06 & MALE_PROPORTION < 57.58 ~ '3rd Qu.',
    TRUE ~ '4th Qu.'
  ))
summary(result)
table(result$DISTRIBUTION)
summary(as.numeric(data$YEAR_INFO))



data_se$ESTRATO_1 =100*data_se$ESTRATO_1 / data_se$TOT_STU
data_se$ESTRATO_2 =100*data_se$ESTRATO_2 / data_se$TOT_STU
data_se$ESTRATO_3 =100*data_se$ESTRATO_3 / data_se$TOT_STU
data_se$ESTRATO_4 =100*data_se$ESTRATO_4 / data_se$TOT_STU
data_se$ESTRATO_5 =100*data_se$ESTRATO_5 / data_se$TOT_STU
data_se$ESTRATO_6 =100*data_se$ESTRATO_6 / data_se$TOT_STU

data_se$zon_alu_urban =100*data_se$zon_alu_urban / data_se$TOT_STU
data_se$zon_alu_rural =100*data_se$zon_alu_rural / data_se$TOT_STU

data_se$Passed_Last_year  =100*data_se$Passed_Last_year / data_se$TOT_STU
data_se$Failed_Last_year   =100*data_se$Failed_Last_year / data_se$TOT_STU
data_se$Retired_Last_year  =100*data_se$Retired_Last_year / data_se$TOT_STU

colnames(result)
joined_data <-  sqldf("
      SELECT * FROM  result
      LEFT JOIN data_se 
      on data_se.codigo_dane_sede = result.codigo_dane_sede and result.YEAR_INFO = data_se.YEAR_INFO and result.grupo=data_se.grupo
            ")
colnames(joined_data)
# Example usage
 
joined_data = joined_data[,c(1:22,29:39)]
 

library(dplyr)
library(dplyr)

# Example usage
vars <- c("TOT_MALE", "TOT_FEMALE", "TOT_STU", "AVG_AGE", "TOT_STEM", "TOT_NO_STEM", 
          "TOT_ECONOMICS_BUSINESS_RELATED", "TOT_ENG_ARCH_RELATED", "TOT_FINE_ARTS", 
          "TOT_MATHEMATICS_NATURAL_SCIENCES", "TOT_SOCIAL_SCIENCES_HUMANITIES", 
          "TOT_AGRONOMY_VETERINARY_RELATED", "TOT_EDUCATION_SCIENCES", 
          "TOT_HEALTH_SCIENCES", "TOT_MEDICINE", "TOT_LAW","TOT_NO_STUDIES",
          "Failed_Last_year", "Passed_Last_year", "Retired_Last_year", "zon_alu_rural", "zon_alu_urban", "ESTRATO_6", 
          "ESTRATO_5", "ESTRATO_4", "ESTRATO_3", 
          "ESTRATO_2", "ESTRATO_1",
          "MALE_PROPORTION")

# Generate the LaTeX table
latex_table_output <- create_desc_stats_table(joined_data, vars)


# Print the LaTeX table
cat(latex_table_output)
latex_table_output <- create_desc_stats_table_single_sex(joined_data, vars)
cat(latex_table_output)


sqldf("SELECT DISTRIBUTION ,  SUM(TOT_STU) FROM result GROUP BY 1")



###ANOVA TEXT
# Load necessary library (if not already loaded)
library(dplyr)

# Assuming your data is stored in a data frame called 'result'

# 1. Create a factor variable for male proportion quartiles
result <- joined_data %>%
  mutate(MALE_PROPORTION_QUARTILE = cut(MALE_PROPORTION, 
                                        breaks = quantile(MALE_PROPORTION, probs = seq(0, 1, 0.25)), 
                                        labels = c("1st Qu.", "2nd Qu.", "3rd Qu.", "4th Qu."), 
                                        include.lowest = TRUE))

# 2. Perform ANOVA test
str(result$MALE_PROPORTION_QUARTILE)
# "TOT_STEM" = "Students in STEM Fields (\\%)"

summary(aov(TOT_STEM ~ MALE_PROPORTION_QUARTILE, data = result) )

# "TOT_NO_STEM" = "Students in Non-STEM Fields (\\%)"
summary(aov(TOT_NO_STEM ~ MALE_PROPORTION_QUARTILE, data = result) )

# "TOT_NO_STUDIES" = "Stu. Not Continuing Education (\\%)",
summary(aov(TOT_NO_STUDIES ~ MALE_PROPORTION_QUARTILE, data = result) )
# "TOT_ECONOMICS_BUSINESS_RELATED" = "Students in Economics/Business (\\%)",
summary(aov(TOT_ECONOMICS_BUSINESS_RELATED ~ MALE_PROPORTION_QUARTILE, data = result) )
# "TOT_ENG_ARCH_RELATED" = "Students in Engineering/Architecture (\\%)",
summary(aov(TOT_ENG_ARCH_RELATED ~ MALE_PROPORTION_QUARTILE, data = result) )

# "TOT_FINE_ARTS" = "Students in Fine Arts (\\%)",
summary(aov(TOT_FINE_ARTS ~ MALE_PROPORTION_QUARTILE, data = result) )
# "TOT_MATHEMATICS_NATURAL_SCIENCES" = "Students in Mathematics/Natural Sciences (\\%)",
summary(aov(TOT_MATHEMATICS_NATURAL_SCIENCES ~ MALE_PROPORTION_QUARTILE, data = result) )
# "TOT_SOCIAL_SCIENCES_HUMANITIES" = "Students in Social Sciences/Humanities (\\%)",
summary(aov(TOT_SOCIAL_SCIENCES_HUMANITIES ~ MALE_PROPORTION_QUARTILE, data = result) )
# "TOT_AGRONOMY_VETERINARY_RELATED" = "Students in Agronomy/Veterinary (\\%)",
summary(aov(TOT_AGRONOMY_VETERINARY_RELATED ~ MALE_PROPORTION_QUARTILE, data = result) )
# "TOT_EDUCATION_SCIENCES" = "Students in Education Sciences (\\%)",
summary(aov(TOT_EDUCATION_SCIENCES ~ MALE_PROPORTION_QUARTILE, data = result) )
# "TOT_HEALTH_SCIENCES" = "Students in Health Sciences (\\%)",
summary(aov(TOT_HEALTH_SCIENCES ~ MALE_PROPORTION_QUARTILE, data = result) )
# "TOT_MEDICINE" = "Students in Medicine (\\%)",
summary(aov(TOT_MEDICINE ~ MALE_PROPORTION_QUARTILE, data = result) )
# "TOT_LAW" = "Students in Law (\\%) ",
summary(aov(TOT_LAW ~ MALE_PROPORTION_QUARTILE, data = result) )


summary(aov(ESTRATO_1 ~ MALE_PROPORTION_QUARTILE, data = result) )
summary(aov(ESTRATO_2 ~ MALE_PROPORTION_QUARTILE, data = result) )
summary(aov(ESTRATO_3 ~ MALE_PROPORTION_QUARTILE, data = result) )
summary(aov(ESTRATO_4 ~ MALE_PROPORTION_QUARTILE, data = result) )
summary(aov(ESTRATO_5 ~ MALE_PROPORTION_QUARTILE, data = result) )
summary(aov(ESTRATO_6 ~ MALE_PROPORTION_QUARTILE, data = result) )

summary(aov(Failed_Last_year ~ MALE_PROPORTION, data = result) )

summary(aov(Passed_Last_year ~ MALE_PROPORTION_QUARTILE, data = result) )

summary(aov(zon_alu_urban ~ MALE_PROPORTION_QUARTILE, data = result) )
summary(aov(zon_alu_rural ~ MALE_PROPORTION_QUARTILE, data = result) )

############################################################################
# Data descriptive at baseline, i.e.  Schools transition from single sex school to coeducaitonal
############################################################################

table(data$SCHOOL_STATUS)

result= data[data$SCHOOL_STATUS == 'FEMALE', ]
result$time_to_treatment = as.numeric(result$YEAR_INFO) -as.numeric(result$TREATMENT_TIME) 
result = result[result$time_to_treatment>=-5 & result$time_to_treatment<=-1  , ]

result <- result[, .(
  TOT_MALE = sum(genero == 'M', na.rm = TRUE),
  TOT_FEMALE = sum(genero == 'F', na.rm = TRUE),
  TOT_STU = uniqueN(.SD),
  AVG_AGE = mean(EDAD, na.rm = TRUE),
  TOT_STEM = 100*sum(STEM==1, na.rm = TRUE)/uniqueN(.SD),
  TOT_NO_STEM = 100*sum(NO_STEM==1, na.rm = TRUE)/uniqueN(.SD),
  TOT_ECONOMICS_BUSINESS_RELATED = 100*sum(EDUCATION_SCIENCES==1, na.rm = TRUE)/uniqueN(.SD),
  TOT_ENG_ARCH_RELATED= 100*sum(ENG_ARCH_RELATED==1, na.rm=T)/uniqueN(.SD),
  TOT_FINE_ARTS= 100*sum(FINE_ARTS==1, na.rm=T)/uniqueN(.SD),
  TOT_MATHEMATICS_NATURAL_SCIENCES= 100*sum(MATHEMATICS_NATURAL_SCIENCES==1, na.rm=T)/uniqueN(.SD),
  TOT_SOCIAL_SCIENCES_HUMANITIES= 100*sum(SOCIAL_SCIENCES_HUMANITIES==1, na.rm=T)/uniqueN(.SD),
  TOT_AGRONOMY_VETERINARY_RELATED= 100*sum(AGRONOMY_VETERINARY_RELATED==1, na.rm=T)/uniqueN(.SD),
  TOT_EDUCATION_SCIENCES= 100*sum(EDUCATION_SCIENCES==1, na.rm=T)/uniqueN(.SD),
  TOT_HEALTH_SCIENCES= 100*sum(HEALTH_SCIENCES==1, na.rm=T)/uniqueN(.SD),
  TOT_MEDICINE= 100*sum(MEDICINE==1, na.rm=T)/uniqueN(.SD),
  TOT_LAW= 100*sum(LAW==1, na.rm=T)/uniqueN(.SD),
  schools = uniqueN(codigo_dane_sede)
), by = .(codigo_dane_sede, grupo, YEAR_INFO)]
result = result[result$TOT_STU>=6 & result$TOT_MALE==0, ]
summary(result)
sum(result$schools)
#########
vars <- c("TOT_MALE", "TOT_FEMALE", "TOT_STU", "AVG_AGE", "TOT_STEM", "TOT_NO_STEM", 
          "TOT_ECONOMICS_BUSINESS_RELATED", "TOT_ENG_ARCH_RELATED", "TOT_FINE_ARTS", 
          "TOT_MATHEMATICS_NATURAL_SCIENCES", "TOT_SOCIAL_SCIENCES_HUMANITIES", 
          "TOT_AGRONOMY_VETERINARY_RELATED", "TOT_EDUCATION_SCIENCES", 
          "TOT_HEALTH_SCIENCES", "TOT_MEDICINE", "TOT_LAW" )  
# Generate the LaTeX table

latex_table_output <- create_desc_stats_table_single_sex(result, vars)


# Print the LaTeX table
cat(latex_table_output)



##############

table(data$SCHOOL_STATUS)

 nrow(data)

result <- data[, .(
  TOT_MALE = sum(genero == 'M', na.rm = TRUE),
  TOT_FEMALE = sum(genero == 'F', na.rm = TRUE),
  TOT_STU = uniqueN(.SD),
  AVG_AGE = mean(EDAD, na.rm = TRUE),
  TOT_STEM = 100*sum(STEM==1, na.rm = TRUE)/uniqueN(.SD),
  TOT_NO_STEM = 100*sum(NO_STEM==1, na.rm = TRUE)/uniqueN(.SD),
  TOT_ECONOMICS_BUSINESS_RELATED = 100*sum(EDUCATION_SCIENCES==1, na.rm = TRUE)/uniqueN(.SD),
  TOT_ENG_ARCH_RELATED= 100*sum(ENG_ARCH_RELATED==1, na.rm=T)/uniqueN(.SD),
  TOT_FINE_ARTS= 100*sum(FINE_ARTS==1, na.rm=T)/uniqueN(.SD),
  TOT_MATHEMATICS_NATURAL_SCIENCES= 100*sum(MATHEMATICS_NATURAL_SCIENCES==1, na.rm=T)/uniqueN(.SD),
  TOT_SOCIAL_SCIENCES_HUMANITIES= 100*sum(SOCIAL_SCIENCES_HUMANITIES==1, na.rm=T)/uniqueN(.SD),
  TOT_AGRONOMY_VETERINARY_RELATED= 100*sum(AGRONOMY_VETERINARY_RELATED==1, na.rm=T)/uniqueN(.SD),
  TOT_EDUCATION_SCIENCES= 100*sum(EDUCATION_SCIENCES==1, na.rm=T)/uniqueN(.SD),
  TOT_HEALTH_SCIENCES= 100*sum(HEALTH_SCIENCES==1, na.rm=T)/uniqueN(.SD),
  TOT_MEDICINE= 100*sum(MEDICINE==1, na.rm=T)/uniqueN(.SD),
  TOT_LAW= 100*sum(LAW==1, na.rm=T)/uniqueN(.SD),
  schools = uniqueN(codigo_dane_sede)
), by = .(codigo_dane_sede, grupo, YEAR_INFO)]
result = result[result$TOT_STU>=6 
                ]
summary(result)
sum(result$schools)
result$MALE_PROPORTION <- 100*result$TOT_MALE / result$TOT_STU
#########
vars <- c("TOT_MALE", "TOT_FEMALE", "TOT_STU", "AVG_AGE", "TOT_STEM", "TOT_NO_STEM", 
          "TOT_ECONOMICS_BUSINESS_RELATED", "TOT_ENG_ARCH_RELATED", "TOT_FINE_ARTS", 
          "TOT_MATHEMATICS_NATURAL_SCIENCES", "TOT_SOCIAL_SCIENCES_HUMANITIES", 
          "TOT_AGRONOMY_VETERINARY_RELATED", "TOT_EDUCATION_SCIENCES", 
          "TOT_HEALTH_SCIENCES", "TOT_MEDICINE", "TOT_LAW" ,"TOT_NO_STUDIES", "MALE_PROPORTION")
# Generate the LaTeX table

latex_table_output <- create_desc_stats_table_single_sex(result, vars)
for (var in vars) {
  result_summary <- result %>%
    summarize(
      Mean = mean(!!sym(var), na.rm = TRUE),
      SD = sd(!!sym(var), na.rm = TRUE)
    ) %>%
    mutate(Mean_SD = paste0(round(Mean, 3), " (", round(SD, 3), ")")) %>%
    select(Mean_SD)
  
  # Print the resulting Mean_SD
  print(paste0(var, ' ', result_summary$Mean_SD))
}

# Print the LaTeX table
cat(latex_table_output)



