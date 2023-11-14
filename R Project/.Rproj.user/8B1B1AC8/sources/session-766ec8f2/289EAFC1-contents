source("C:/Users/USER/Desktop/01-with-the-boys/Scripts/R/genereal_settings.R", echo=TRUE)
source("C:/Users/USER/Desktop/01-with-the-boys/Scripts/R/read_data.R", echo=TRUE)
source("C:/Users/USER/Desktop/01-with-the-boys/Scripts/R/functions.R", echo=TRUE)
outcomes = c('ECONOMICS_BUSINESS_RELATED' ,
             'ENG_ARCH_RELATED',
             'FINE_ARTS',
             'MATHEMATICS_NATURAL_SCIENCES',
             'SOCIAL_SCIENCES_HUMANITIES',
             'AGRONOMY_VETERINARY_RELATED',
             'EDUCATION_SCIENCES',
             'HEALTH_SCIENCES',
             'NO_STUDIES'
)
# FROM data")
data$fe_group = paste0(data$codigo_dane_sede, ' - ' , data$YEAR_INFO)

variables = c(outcomes, 'genero' , 'EDAD' , 'frac_males_in_the_group',  'frac_females_in_the_group','tot_students_school_group', 'fe_group')

data  = data[variables]

gc()
################################
# sqldf::sqldf("SELECT   
# SUM(CASE WHEN genero = 'M' THEN 1 ELSE 0 END),
# SUM(CASE WHEN genero = 'F' THEN 1 ELSE 0 END),
# 
# AVG(tot_females_school_group),
# AVG(tot_males_school_group),
# 
# AVG(frac_males_in_the_group),
# AVG(frac_females_in_the_group),
# 
# count(*)
# FROM data")
# summary(data$frac_males_in_the_group) 
# hist(data$frac_males_in_the_group) 
# # model_list <- c()
# summary(data$frac_males_in_the_group)
# 
# # # Create a histogram with ggplot2
# png(paste0(graphs_dir ,"his_frac_males" ,".png"),  width = 1030, height = 598)
# ggplot(data , aes(x = frac_males_in_the_group)) +
#   geom_histogram(fill = "blue", color = "black", bins = 20) +  # Customize fill and color
#   geom_vline(xintercept = 0.025, color = "red", linetype = "dashed") +  # Vertical line at x = 0.025
#   geom_vline(xintercept = 0.975, color = "green", linetype = "dashed") +  # Vertical line at x = 0.975
#   annotate("text", x = 0.025, y = 10, label = "Female Schools", color = "black" , angle = 90, vjust = 1, hjust = -2.5) +  # Add label for the red line
#   annotate("text", x = 0.975, y = 10, label = "Male Schools", color = "black", angle = 90, vjust = 1, hjust = -3) +  # Add label for the green line
#   labs(title = "Distribution of Proportion of Males in Class Groups",   # Add a title
#        x = "Proportion of Males",               # Label x-axis
#        y = "Frequency") +                       # Label y-axis
#   theme_minimal() +                             # Use a minimal theme
#   theme(plot.title = element_text(hjust = 0.5))  # Center the title
# dev.off() 
#  

################ ## ################
gc()
data= subset(data, data$frac_females_in_the_group >= 0.025  & data$frac_females_in_the_group <= 0.975 ) 
gc()
################ ## #################
# Y ~ frac_male
################ ## #################

model_list <- list()
for (outcome in outcomes ) {
  formuala_ = paste0( 'factor(' , outcome  , ') ~ ' ,  'frac_males_in_the_group ' )   
  print(formuala_)
  model <- glm( as.formula(formuala_), data = data, family = binomial)
  model_list  <- append(model_list, list(model))
}

gc()

result = stargazer::stargazer(model_list,  
                     dep.var.labels=outcomes ,
                     covariate.labels=c('Proportion of males within a class group'  ), 
                     align=TRUE   
)
gc()
caption = 'Correlation between the gender composition in a class and the likelihood of a student choosing a career'
file_name = 'correlation_outcome_frac_males'

table_save(result,caption, file_name ) 


 
rm(model_list)
gc()
 
 
################ ## #################
# Y ~ frac_male + Gender=f
################ ## #################
data$genero <- factor(data$genero, levels = c("M","F"))

model_list <- list()
for (outcome in outcomes ) {
  formuala_ = paste0( 'factor(' , outcome  , ') ~ ' ,  'frac_males_in_the_group  + genero ' )   
  print(formuala_)
  model <- glm( as.formula(formuala_), data = data, family = binomial)
  model_list  <- append(model_list, list(model))
}

gc()

result = stargazer::stargazer(model_list,  
                     # dep.var.labels=outcomes ,
                     covariate.labels=c('Proportion of males within a class group' , 'Gender (Female)'   ), 
                     align=TRUE 
)

gc()
caption = 'Correlation between the gender composition in a class and the likelihood of a male student choose a career'
file_name = 'correlation_outcome_frac_males_plus_gender_no_singlesex'
summary(model)
table_save(result,caption, file_name ) 



rm(model_list)
gc()
 
################ ## #################
# Y ~ numer_males * Gender=f
################ ## #################

data$genero <- factor(data$genero, levels = c("M","F"))

model_list <- list()
for (outcome in outcomes ) {
  formuala_ = paste0( 'factor(' , outcome  , ') ~ ' ,  'frac_males_in_the_group  * genero ' )   
  print(formuala_)
  model <- glm( as.formula(formuala_), data = data, family = binomial)
  model_list  <- append(model_list, list(model))
}

gc()

result = stargazer::stargazer(model_list,  
                              # dep.var.labels=outcomes ,
                              covariate.labels=c('Proportion of males within a class group' , 'Gender (Female)'   ), 
                              align=TRUE 
)

gc()
caption = 'Correlation and interaction between the gender composition in a class and the likelihood of a female student choose a career'
file_name = 'correlation_outcome_frac_males_cdot_gender'

table_save(result,caption, file_name ) 



rm(model_list)
gc()

################ ## #################
# Setting by percentils excluiding only sex schools
################ ## ################# 
data= subset(data, data$frac_females_in_the_group >= 0.025  & data$frac_females_in_the_group <= 0.975 ) 


data$genero <- factor(data$genero, levels = c("M","F"))
gc()

breakpoints <- seq(0, 1, by = 0.1)
 
estimated_points <- data.frame()
# summary(glm(factor(ECONOMICS_BUSINESS_RELATED) ~ genero , data, family = binomial ) )
for (outcome in outcomes ) {
  print(outcome)
  for (i in 1:(length(breakpoints) - 1)) {
    # Define the range for the current subsample
    range_start <- breakpoints[i]
    range_end <- breakpoints[i + 1]
    subsample <- data[data$frac_males_in_the_group >= range_start & data$frac_males_in_the_group < range_end, ]
    if (outcome != "NO_STUDIES" ) {
      subsample <- subset(subsample, subsample[["NO_STUDIES"]] != 1 )
    }
    formuala_ = paste0( 'factor(' , outcome  , ') ~ ' ,  'genero ' )   
    print( paste0('the current estimation is from: ',range_start , ', to: ',range_end ) )
    model <- glm( as.formula(formuala_), data = subsample , family = binomial)
    
    # Obtain the summary of the model
    model_summary <- summary(model)
    
    # Extract the standard errors
    standard_errors <- model_summary$coef[, "Std. Error"]
    intercept_std_error =  standard_errors[1]
    estimated_point_std_error =  standard_errors[2]
    estimated_point <- coef(model)[2]
    temp = data.frame(Breakpoint = (range_start + range_end) / 2, Estimated_Point = estimated_point)
    temp$estimated_point_std_error = estimated_point_std_error
    temp$intercept = coef(model)[1]
    temp$intercept_std_error = intercept_std_error
    temp$outcome = outcome
    estimated_points <- rbind(estimated_points,temp )
    gc()
    
  }

}
gc()


write.csv(estimated_points, 'Tables/optimal_gender_composition_classroom_10p_v2.csv', row.names = F)


library(readr)
# estimated_points <- read_csv("C:/Users/USER/Desktop/01-with-the-boys/Tables/optimal_gender_composition_classroom.csv", 
                                                 # col_types = cols(...1 = col_skip()))

for (career in unique(estimated_points$outcome)) {
  print(career)
  subsample = subset(estimated_points, estimated_points$outcome ==  career)
  # Create a bar plot using ggplot2
  x_continuous = 'Breakpoint'
  sd_error = 'estimated_point_std_error'
  estimate_point = 'Estimated_Point'
  TITULO = convert_outcome(career) 
  # png(paste0(graphs_dir , 'gender_composition_wome_in_', career ,"_10p.png"),  width = 1030, height = 598)
  plot_coefficients(subsample, estimate_point = estimate_point ,sd_error = sd_error, x_continuous = x_continuous , TITULO= TITULO)
  # dev.off()
}
 
# Relative Effect 
estimated_points$relative_effect = log(estimated_points$Estimated_Point) #estimated_points$Estimated_Point / estimated_points$intercept

for (career in unique(estimated_points$outcome)) {
  print(career)
  subsample = subset(estimated_points, estimated_points$outcome ==  career)
  # Create a bar plot using ggplot2
  x_continuous = 'Breakpoint'
  sd_error = 'estimated_point_std_error'
  estimate_point = 'relative_effect'
  TITULO = convert_outcome(career) 
  # png(paste0(graphs_dir , 'gender_composition_wome_in_', career ,"_10p.png"),  width = 1030, height = 598)
  plot_coefficients(subsample, estimate_point = estimate_point ,sd_error = sd_error, x_continuous = x_continuous , TITULO= TITULO)
  # dev.off()
} 


