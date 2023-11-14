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
gc()
# data = sqldf::sqldf("SELECT * ,
                  # codigo_dane_sede ||' - ' || YEAR_INFO fe_group 
             # FROM data")
data$fe_group = paste0(data$codigo_dane_sede, ' - ' , data$YEAR_INFO)
gc()
###############################################
# Fixed effect by schools level
###############################################



# First. Panel construction of relation by school

data_panel_by_school  = sqldf::sqldf("
                           SELECT  
                           tot_students_school_group, codigo_dane_sede,
                           frac_males_in_the_group, YEAR_INFO year,
                           codigo_dane_sede ||' - ' || YEAR_INFO fe_group ,
                           SUM( CASE WHEN genero = 'M' THEN 1 ELSE 0 END) MALES,
                           SUM( CASE WHEN genero = 'F' THEN 1 ELSE 0 END) FEMALES,
                           sum(ECONOMICS_BUSINESS_RELATED) ECONOMICS_BUSINESS_RELATED, 
                           
                           sum(ECONOMICS_BUSINESS_RELATED) ECONOMICS_BUSINESS_RELATED, 
                            sum(ENG_ARCH_RELATED) ENG_ARCH_RELATED, 
                             sum(FINE_ARTS) FINE_ARTS, 
                              sum(MATHEMATICS_NATURAL_SCIENCES) MATHEMATICS_NATURAL_SCIENCES, 
                               sum(SOCIAL_SCIENCES_HUMANITIES) SOCIAL_SCIENCES_HUMANITIES, 
                                sum(AGRONOMY_VETERINARY_RELATED) AGRONOMY_VETERINARY_RELATED, 
                                 sum(EDUCATION_SCIENCES) EDUCATION_SCIENCES, 
                                  sum(HEALTH_SCIENCES) HEALTH_SCIENCES, 
                                    sum(NO_STUDIES) NO_STUDIES, 
                                 
                           avg(EDAD) age
                            
                           FROM data
                           GROUP BY 1,2,3,4
                           ")


gc()
gc()
rm(data)
gc()

data_panel_by_school$frac_males = data_panel_by_school$MALES / data_panel_by_school$tot_students_school_group
data_panel_by_school$frac_females = data_panel_by_school$FEMALES / data_panel_by_school$tot_students_school_group


data_panel_by_school$frac_ECONOMICS_BUSINESS_RELATED = data_panel_by_school$ECONOMICS_BUSINESS_RELATED / data_panel_by_school$tot_students_school_group
data_panel_by_school$frac_ENG_ARCH_RELATED = data_panel_by_school$ENG_ARCH_RELATED / data_panel_by_school$tot_students_school_group
data_panel_by_school$frac_FINE_ARTS = data_panel_by_school$FINE_ARTS / data_panel_by_school$tot_students_school_group
data_panel_by_school$frac_MATHEMATICS_NATURAL_SCIENCES = data_panel_by_school$MATHEMATICS_NATURAL_SCIENCES / data_panel_by_school$tot_students_school_group
data_panel_by_school$frac_SOCIAL_SCIENCES_HUMANITIES = data_panel_by_school$SOCIAL_SCIENCES_HUMANITIES / data_panel_by_school$tot_students_school_group
data_panel_by_school$frac_AGRONOMY_VETERINARY_RELATED = data_panel_by_school$AGRONOMY_VETERINARY_RELATED / data_panel_by_school$tot_students_school_group
data_panel_by_school$frac_EDUCATION_SCIENCES = data_panel_by_school$EDUCATION_SCIENCES / data_panel_by_school$tot_students_school_group
data_panel_by_school$frac_HEALTH_SCIENCES = data_panel_by_school$HEALTH_SCIENCES / data_panel_by_school$tot_students_school_group
data_panel_by_school$frac_NO_STUDIES = data_panel_by_school$NO_STUDIES / data_panel_by_school$tot_students_school_group
data_panel_by_school = subset(data_panel_by_school, data_panel_by_school$tot_students_school_group >= 4)
gc()
# rm(data)
# 
# data_panel_by_school$year
# modelo = fixest::feols(data = data_panel_by_school, frac_ECONOMICS_BUSINESS_RELATED ~ frac_females | fe_group )
# etable(modelo)
# summary(modelo)

# name_in_enviroment = paste0(  "Score at ", m, " Meters")
# TWFE_reading_naturaleza_base_10p[[name_in_enviroment]] <- (MODEL) 

breakpoints <- seq(0, 1, by = 0.1)  
estimated_points <- data.frame()
data  = data_panel_by_school
# summary(glm(factor(ECONOMICS_BUSINESS_RELATED) ~ genero , data, family = binomial ) )
for (outcome in outcomes  ) {
  print(outcome)
  for (i in 1:(length(breakpoints) - 1)) {
    # Define the range for the current subsample
    range_start <- breakpoints[i]
    range_end <- breakpoints[i + 1]
    subsample <- data[data$frac_males_in_the_group >= range_start & data$frac_males_in_the_group < range_end, ]
    #
    if (outcome != "NO_STUDIES" ) {
      subsample <- subset(subsample, subsample[["NO_STUDIES"]] != 1 )
      
    }
    formuala_ = paste0( 'frac_' , outcome  , ' ~ ' ,  ' frac_females +age+tot_students_school_group   | fe_group ' )   
    print( paste0('the current estimation is from: ',range_start , ', to: ',range_end ) )
    model = fixest::feols(data = subsample, as.formula(formuala_)  )
    
    # model <- glm( as.formula(formuala_), data = subsample , family = binomial)
    
    # Obtain the summary of the model
    model_summary <- summary(model)
    model_summary
    # Extract the standard errors
    standard_errors <- model_summary$se
    standard_errors <- as.numeric(standard_errors[1] ) # model_summary$coefficients[, "Std. Error"]
    # intercept_std_error =  standard_errors[1]
    estimated_point_std_error =  standard_errors
    estimated_point <- as.numeric(model_summary$coefficients[1])
    temp = data.frame(Breakpoint = (range_start + range_end) / 2,
                      Estimated_Point = estimated_point, 
                      estimated_point_std_error = estimated_point_std_error, 
                      number_schools = length( unique(subsample$codigo_dane_sede) ) ,
                      numer_period = length( unique(subsample$year) )
                      
                      )
    
    temp$outcome = outcome
    estimated_points <- rbind(estimated_points,temp )
    gc()
    
  }
  
}

# Estimation fixed effect by school. 
# feglm(Sepal.Length ~ Sepal.Width + Petal.Length | Species, iris, "logit")


for (career in unique(estimated_points$outcome)) {
  print(career)
  subsample = subset(estimated_points, estimated_points$outcome ==  career)
  row.names(subsample) <- NULL
  glimpse(subsample)
  # Create a bar plot using ggplot2
  x_continuous = 'Breakpoint'
  sd_error = 'estimated_point_std_error'
  estimate_point = 'Estimated_Point'
  TITULO = convert_outcome(career) 
  png(paste0(graphs_dir , 'fe_panel_school_gender_composition_wome_in_', career ,"_10p.png"),  width = 1030, height = 598)
  plot_coefficients(subsample, estimate_point = estimate_point ,sd_error = sd_error, x_continuous = x_continuous , TITULO= TITULO)
  dev.off()
}
gc()
###############################################################################
# 
###############################################################################

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
gc()
data$fe_group = paste0(data$codigo_dane_sede, ' - ' , data$YEAR_INFO  ) # ,' - ', data$nro_documento
###############################################
# Fixed effect by student level
###############################################
gc()
 
data$genero <- factor(data$genero, levels = c("M","F"))

breakpoints <- seq(0, 1, by = 0.1)  
estimated_points <- data.frame()
 
gc()
 
for (outcome in outcomes[1]  ) {
  print(outcome)
  for (i in 1:(length(breakpoints) - 1)) {
    # Define the range for the current subsample
    range_start <- breakpoints[i]
    range_end <- breakpoints[i + 1]
    subsample <- data[data$frac_males_in_the_group >= range_start & data$frac_males_in_the_group < range_end, ]
    gc()
    if (outcome != "NO_STUDIES" ) {
      subsample <- subset(subsample, subsample[["NO_STUDIES"]] != 1 )
      
    }
    formuala_ = paste0( ' (',outcome     , ') ~ ' ,  ' genero +EDAD+tot_students_school_group   | fe_group ' )
    
    print( paste0('the current estimation is from: ',range_start , ', to: ',range_end ) )
     
    model = fixest::feglm( data = subsample, family = 'logit', as.formula(formuala_)  )  
    gc()
    # Obtain the summary of the model
    model_summary <- summary(model)
    model_summary
    # Extract the standard errors
    standard_errors <- model_summary$se
    standard_errors <- as.numeric(standard_errors[1] ) # model_summary$coefficients[, "Std. Error"]
    # intercept_std_error =  standard_errors[1]
    estimated_point_std_error =  standard_errors
    estimated_point <- as.numeric(model_summary$coefficients[1])
    temp = data.frame(Breakpoint = (range_start + range_end) / 2,
                      Estimated_Point = estimated_point, 
                      estimated_point_std_error = estimated_point_std_error, 
                      number_schools = length( unique(subsample$codigo_dane_sede) ) ,
                      numer_period = length( unique(subsample$year) )
                      
    )
    
    temp$outcome = outcome
    estimated_points <- rbind(estimated_points,temp )
    gc()
    
  }
  
}
 
 
 
###

gc()





for (career in unique(estimated_points$outcome)) {
  print(career)
  subsample = subset(estimated_points, estimated_points$outcome ==  career)
  row.names(subsample) <- NULL
  glimpse(subsample)
  # Create a bar plot using ggplot2
  x_continuous = 'Breakpoint'
  sd_error = 'estimated_point_std_error'
  estimate_point = 'Estimated_Point'
  TITULO = paste0('Odds Ratio of\n' , convert_outcome(career) )
  png(paste0(graphs_dir , 'fe_panel_student_gender_composition_wome_in_', career ,"_10p.png"),  width = 1030, height = 598)
  plot_coefficients(subsample, estimate_point = estimate_point ,sd_error = sd_error, x_continuous = x_continuous , TITULO= TITULO)
  dev.off()
}
gc()


write.csv(estimated_points, 'Data/estimated_points_fe_panel_student_gender_composition_wome_10P.csv')
###############################################    ###############################################
# Marginal effect => ME = \beta1 * P*(1-p) 
###############################################    ###############################################

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
data= subset(data, data$frac_females_in_the_group >= 0.025  & data$frac_females_in_the_group <= 0.975 ) 
gc()


data$genero <- factor(data$genero, levels = c("M","F"))

breakpoints <- seq(0, 1, by = 0.1)  
estimated_points <- data.frame()

gc()

for (outcome in outcomes   ) {
  print(outcome)
  for (i in 1:(length(breakpoints) - 1)) {
    # Define the range for the current subsample
    range_start <- breakpoints[i]
    range_end <- breakpoints[i + 1]
    subsample <- data[data$frac_males_in_the_group >= range_start & data$frac_males_in_the_group < range_end, ]
    gc()
    if (outcome != "NO_STUDIES" ) {
      subsample <- subset(subsample, subsample[["NO_STUDIES"]] != 1 )
      
    }
    formuala_ = paste0( ' (',outcome     , ') ~ ' ,  ' genero +EDAD+tot_students_school_group   | fe_group ' )
    
    print( paste0('the current estimation is from: ',range_start , ', to: ',range_end ) )
    
    model = fixest::feglm( data = subsample, family = 'logit', as.formula(formuala_)  )  
    gc()
    # Obtain the summary of the model
    model_summary <- summary(model)
    model_summary
    # Extract the standard errors
    ame = compute_marginal_effect(model, subsample, 'genero', num_bootstraps = 100) 
    
    
    standard_errors <- model_summary$se
    standard_errors <- as.numeric(standard_errors[1] ) # model_summary$coefficients[, "Std. Error"]
    # intercept_std_error =  standard_errors[1]
    estimated_point_std_error =  standard_errors
    estimated_point <- as.numeric(model_summary$coefficients[1])
    temp = data.frame(Breakpoint = (range_start + range_end) / 2,
                      Estimated_Point = estimated_point, 
                      estimated_point_std_error = estimated_point_std_error, 
                      number_schools = length( unique(subsample$codigo_dane_sede) ) ,
                      numer_period = length( unique(subsample$year)),
                      ame = ame$marginal_effect,
                      sd_ame = ame$std_error, 
                      comparison_base = ame$comparasion_level
                      
    )
    
    temp$outcome = outcome
    estimated_points <- rbind(estimated_points,temp )
    gc()
    
  }
  
}


write.csv(estimated_points, 'Data/estimated_points_fe_panel_student_gender_composition_wome_10P_ame.csv')




for (career  in unique(estimated_points$outcome)) {
  print(career)
  subsample = subset(estimated_points, estimated_points$outcome ==  career)
  row.names(subsample) <- NULL
  glimpse(subsample)
  # Create a bar plot using ggplot2
  x_continuous = 'Breakpoint'
  sd_error = 'estimated_point_std_error'
  estimate_point = 'Estimated_Point'
  
  TITULO = paste0('Odds-Ratio for choosing:\n' , convert_outcome(career) )
  png(paste0(graphs_dir , 'fe_panel_student_gender_composition_wome_in_', career ,"_10p.png"),  width = 1030, height = 598)
  plot_coefficients(subsample, estimate_point = estimate_point ,sd_error = sd_error, x_continuous = x_continuous , TITULO= TITULO)
  dev.off()
  
  
  TITULO = paste0('Average Marginal Effect for choosing:\n' , convert_outcome(career) )
  png(paste0(graphs_dir , 'ame_fe_panel_student_gender_composition_wome_in_', career ,"_10p.png"),  width = 1030, height = 598)
  plot_coefficients(subsample, estimate_point = 'ame' ,
                               sd_error = 'sd_ame', 
                                x_continuous = x_continuous , TITULO= TITULO)
  dev.off()
}
gc()










###############################################    ###############################################
#
###############################################    ###############################################


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
data= subset(data, data$frac_females_in_the_group >= 0.025  & data$frac_females_in_the_group <= 0.975 ) 
gc()


data$genero <- factor(data$genero, levels = c("M","F"))

breakpoints <- seq(0, 1, by = 0.1)  
estimated_points <- data.frame()

gc()

for (outcome in outcomes[1]  ) {
  print(outcome)
  # for (i in 1:(length(breakpoints) - 1)) {
  #   # Define the range for the current subsample
  #   range_start <- breakpoints[i]
  #   range_end <- breakpoints[i + 1]
    subsample <- data #[data$frac_males_in_the_group >= range_start & data$frac_males_in_the_group < range_end, ]
    gc()
    if (outcome != "NO_STUDIES" ) {
      subsample <- subset(subsample, subsample[["NO_STUDIES"]] != 1 )
      
    }
    formuala_ = paste0( ' (',outcome     , ') ~ ' ,  ' genero * frac_males_in_the_group   | fe_group ' )
    
    
    
    model = fixest::feglm( data = subsample, family = 'logit', as.formula(formuala_)  )  
    gc()
    # Obtain the summary of the model
    model_summary <- summary(model)
    model_summary
    # Extract the standard errors
    standard_errors <- model_summary$se
    standard_errors <- as.numeric(standard_errors[1] ) # model_summary$coefficients[, "Std. Error"]
    # intercept_std_error =  standard_errors[1]
    estimated_point_std_error =  standard_errors
    estimated_point <- as.numeric(model_summary$coefficients[1])
    temp = data.frame( 
                      Estimated_Point = estimated_point, 
                      estimated_point_std_error = estimated_point_std_error, 
                      number_schools = length( unique(subsample$codigo_dane_sede) ) ,
                      numer_period = length( unique(subsample$year) )
                      
    )
    
    temp$outcome = outcome
    estimated_points <- rbind(estimated_points,temp )
    gc()
    
}





 
 