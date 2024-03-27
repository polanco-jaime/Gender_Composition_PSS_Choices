

###############################################    ###############################################
###############################################    ###############################################
# Marginal effect => ME = \beta1 * P*(1-p) 
###############################################    ###############################################
###############################################    ###############################################

source("Scripts/R/genereal_settings.R", echo=TRUE)
source("./Scripts/R/read_data.R", echo=TRUE)
source("./Scripts/R/functions.R", echo=TRUE)
outcomes = c('ECONOMICS_BUSINESS_RELATED' ,
             'ENG_ARCH_RELATED',
             'FINE_ARTS',
             'MATHEMATICS_NATURAL_SCIENCES',
             'SOCIAL_SCIENCES_HUMANITIES',
             'AGRONOMY_VETERINARY_RELATED',
             'EDUCATION_SCIENCES',
             'HEALTH_SCIENCES',
             'NO_STUDIES',
             'MEDICINE',
             'LAW'
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
