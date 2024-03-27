##############################################################################
# 
###############################################################################

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
gc()
table(data$NO_STUDIES)


data$genero <- factor(data$genero, levels = c("M","F"))





gc()
gc()
gc()
final_variable = c(outcomes, 'genero', 'EDAD' , 'tot_students_school_group' , "codigo_dane_sede",'fe_group', "year" , 'School_with_more_6_students',
                   'frac_males_in_the_group')
colnames(data)
table(data$YEAR_INFO)
data = data[final_variable]
gc()
gc()
gc()
###########################
# breakpoints <- seq(0, 1, by = 0.065)

estimated_points <- data.frame()

covariates <- c("genero", "EDAD", "tot_students_school_group", "frac_males_in_the_group")
group_var <- "fe_group"

for (outcome in outcomes  ) {
  print(outcome)
  gen_estimate = estimate_point_logit(outcome,  data, covariates, group_var)
  estimated_points <- rbind(estimated_points,gen_estimate )
  json_data <- fromJSON("Tables/obtimal_distance.json")
  optimal_distance <- json_data[json_data$outcome == outcome,  "Optimal_Distance"]
  breakpoints <-seq(0.001, 0.999, by = optimal_distance)  
   
  if (tail(breakpoints, 1) > 0.93) {
    print("The last value is greater than 0.93")
  }
  else {
    breakpoints <- c( breakpoints, 0.999)
  }
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
gc()
gc()





for (career in unique(estimated_points$outcome) ) {
  print(career)
  subsample = subset(estimated_points, estimated_points$outcome ==  career)
  row.names(subsample) <- NULL
  glimpse(subsample)
  # Create a bar plot using ggplot2
  x_continuous = 'Breakpoint'
  sd_error = 'estimated_point_std_error'
  estimate_point = 'Estimated_Point'
  TITULO = paste0('Odds Ratio of\n' , convert_outcome(career) )
  Plot =   plot_coefficients(subsample, estimate_point = estimate_point ,
                             sd_error = sd_error, x_continuous = x_continuous ,
                             TITULO= TITULO, 5)
  Plot2 =   plot_coefficients_smooth(subsample, estimate_point = estimate_point ,
                             sd_error = sd_error, x_continuous = x_continuous ,
                             TITULO= TITULO, 3)
  
  png(paste0(graphs_dir , 'fe_panel_student_gender_composition_wome_in_', career ,"_bce.png"),  width = 1030, height = 598)
  print(Plot)
  dev.off()
  
  png(paste0(graphs_dir , 'fe_panel_student_gender_composition_wome_in_', career ,"_smooth_bce.png"),  width = 1030, height = 598)
  print(Plot2)
  dev.off()
  
  gc()
}


 
write.csv(estimated_points, 'Data/estimated_points_fe_panel_student_gender_composition_wome_bce.csv')

# 
# ########################################
# if (Sys.info()["nodename"] == "CLOUD37") {
#   General_path = "C:/Users/USER/Desktop/01-with-the-boys/"
#   data_dir <- paste0(General_path , "Data/")
#   graphs_dir <-  paste0(General_path , "Graph/") 
#   tables_dir <- paste0(General_path , "Tables/")  
# }  else if (Sys.info()["nodename"] == "JAIME") {
#   General_path = "C:/Users/USER/Desktop/01-with-the-boys/"
#   data_dir <- paste0(General_path , "Data/")
#   graphs_dir <-  paste0(General_path , "Graph/") 
#   tables_dir <- paste0(General_path , "Tables/")  
# }
# 
# LISTA = unique(estimated_points$outcome) 
# 
# for (career  in unique(estimated_points$outcome)  ) {
#   career   = LISTA[11]
#   print(career)
#   subsample = subset(estimated_points, estimated_points$outcome ==  career)
#   row.names(subsample) <- NULL
#   glimpse(subsample)
#   # Create a bar plot using ggplot2
#   x_continuous = 'Breakpoint'
#   sd_error = 'estimated_point_std_error'
#   estimate_point = 'Estimated_Point'
#   
#   # TITULO = paste0('Odds-Ratio for choosing:\n' , convert_outcome(career) )
#   # png(paste0(graphs_dir , 'fe_panel_student_gender_composition_wome_in_', career ,"_10p.png"),  width = 1030, height = 598)
#   # plot_coefficients(subsample, estimate_point = estimate_point ,sd_error = sd_error, x_continuous = x_continuous , TITULO= TITULO)
#   # dev.off()
#   # 
#   # 
#   # TITULO = paste0('Average Marginal Effect for choosing:\n' , convert_outcome(career) )
#   # png(paste0(graphs_dir , 'ame_fe_panel_student_gender_composition_wome_in_', career ,"_10p.png"),  width = 1030, height = 598)
#   # plot_coefficients(subsample, estimate_point = 'ame' ,
#   #                   sd_error = 'sd_ame', 
#   #                   x_continuous = x_continuous , TITULO= TITULO)
#   # dev.off()
#   # 
#   TITULO = paste0('AME for choosing:\n' , convert_outcome(career) )
#   
#   Plot =   plot_coefficients(subsample, estimate_point = 'ame' ,
#                              sd_error = 'sd_ame', x_continuous = x_continuous ,
#                              TITULO= TITULO, 5)
#   Plot2 =   plot_coefficients_smooth(subsample, estimate_point = 'ame' ,
#                                      sd_error = 'sd_ame', x_continuous = x_continuous ,
#                                      TITULO= TITULO, 3)
#   
#   # Sys.sleep(3)
#   png(paste0(graphs_dir ,  'ame_fe_panel_student_gender_composition_wome_in_',career,"_10p.png") ,  width = 1030, height = 598 )
#   (Plot)
#   dev.off()
#   # Sys.sleep(3)
#   png(paste0(graphs_dir , 'ame_fe_panel_student_gender_composition_wome_in_', career ,"_smooth_10p.png"),  width = 1030, height = 598)
# (Plot2)
#   dev.off()
#    
# }
# gc()
# 
# 
# 
# 
# 
# 
# 



###############################################    ###############################################
#
###############################################    ###############################################
# 
# source("Scripts/R/genereal_settings.R", echo=TRUE)
# source("./Scripts/R/read_data.R", echo=TRUE)
# source("./Scripts/R/functions.R", echo=TRUE)
# outcomes = c('ECONOMICS_BUSINESS_RELATED' ,
#              'ENG_ARCH_RELATED',
#              'FINE_ARTS',
#              'MATHEMATICS_NATURAL_SCIENCES',
#              'SOCIAL_SCIENCES_HUMANITIES',
#              'AGRONOMY_VETERINARY_RELATED',
#              'EDUCATION_SCIENCES',
#              'HEALTH_SCIENCES',
#              'NO_STUDIES'
# )
# # FROM data")
# data$fe_group = paste0(data$codigo_dane_sede, ' - ' , data$YEAR_INFO)
# 
# variables = c(outcomes, 'genero' , 'EDAD' , 'frac_males_in_the_group',  'frac_females_in_the_group','tot_students_school_group', 'fe_group')
# 
# data  = data[variables]
# 
# gc()
# data= subset(data, data$frac_females_in_the_group >= 0.025  & data$frac_females_in_the_group <= 0.975 ) 
# gc()
# 
# 
# data$genero <- factor(data$genero, levels = c("M","F"))
# 
# breakpoints <- seq(0, 1, by = 0.1)  
# estimated_points <- data.frame()
# 
# gc()
# 
# for (outcome in outcomes[1]  ) {
#   print(outcome)
#   # for (i in 1:(length(breakpoints) - 1)) {
#   #   # Define the range for the current subsample
#   #   range_start <- breakpoints[i]
#   #   range_end <- breakpoints[i + 1]
#   subsample <- data #[data$frac_males_in_the_group >= range_start & data$frac_males_in_the_group < range_end, ]
#   gc()
#   if (outcome != "NO_STUDIES" ) {
#     subsample <- subset(subsample, subsample[["NO_STUDIES"]] != 1 )
#     
#   }
#   formuala_ = paste0( ' (',outcome     , ') ~ ' ,  ' genero * frac_males_in_the_group   | fe_group ' )
#   
#   
#   
#   model = fixest::feglm( data = subsample, family = 'logit', as.formula(formuala_)  )  
#   gc()
#   # Obtain the summary of the model
#   model_summary <- summary(model)
#   model_summary
#   # Extract the standard errors
#   standard_errors <- model_summary$se
#   standard_errors <- as.numeric(standard_errors[1] ) # model_summary$coefficients[, "Std. Error"]
#   # intercept_std_error =  standard_errors[1]
#   estimated_point_std_error =  standard_errors
#   estimated_point <- as.numeric(model_summary$coefficients[1])
#   temp = data.frame( 
#     Estimated_Point = estimated_point, 
#     estimated_point_std_error = estimated_point_std_error, 
#     number_schools = length( unique(subsample$codigo_dane_sede) ) ,
#     numer_period = length( unique(subsample$year) )
#     
#   )
#   
#   temp$outcome = outcome
#   estimated_points <- rbind(estimated_points,temp )
#   gc()
#   
# }
# 
# 
# 
# ###############################################
# # Fixed effect by schools level
# ###############################################
# 
# 
# 
# ###############################################################################
# # 
# ###############################################################################
# 
# source("Scripts/R/genereal_settings.R", echo=TRUE)
# source("./Scripts/R/read_data.R", echo=TRUE)
# source("./Scripts/R/functions.R", echo=TRUE)
# outcomes = c('ECONOMICS_BUSINESS_RELATED' ,
#              'ENG_ARCH_RELATED',
#              'FINE_ARTS',
#              'MATHEMATICS_NATURAL_SCIENCES',
#              'SOCIAL_SCIENCES_HUMANITIES',
#              'AGRONOMY_VETERINARY_RELATED',
#              'EDUCATION_SCIENCES',
#              'HEALTH_SCIENCES',
#              'NO_STUDIES'
# )
# gc()
# data$fe_group = paste0(data$codigo_dane_sede, ' - ' , data$YEAR_INFO  ) # ,' - ', data$nro_documento
# ###############################################
# # Fixed effect by student level
# ###############################################
# gc()
#  
# data$genero <- factor(data$genero, levels = c("M","F"))
# 
# breakpoints <- seq(0, 1, by = 0.1)  
# estimated_points <- data.frame()
#  
# gc()
#  
# for (outcome in outcomes[1]  ) {
#   print(outcome)
#   for (i in 1:(length(breakpoints) - 1)) {
#     # Define the range for the current subsample
#     range_start <- breakpoints[i]
#     range_end <- breakpoints[i + 1]
#     subsample <- data[data$frac_males_in_the_group >= range_start & data$frac_males_in_the_group < range_end, ]
#     gc()
#     if (outcome != "NO_STUDIES" ) {
#       subsample <- subset(subsample, subsample[["NO_STUDIES"]] != 1 )
#       
#     }
#     formuala_ = paste0( ' (',outcome     , ') ~ ' ,  ' genero +EDAD+tot_students_school_group   | fe_group ' )
#     
#     print( paste0('the current estimation is from: ',range_start , ', to: ',range_end ) )
#      
#     model = fixest::feglm( data = subsample, family = 'logit', as.formula(formuala_)  )  
#     gc()
#     # Obtain the summary of the model
#     model_summary <- summary(model)
#     model_summary
#     # Extract the standard errors
#     standard_errors <- model_summary$se
#     standard_errors <- as.numeric(standard_errors[1] ) # model_summary$coefficients[, "Std. Error"]
#     # intercept_std_error =  standard_errors[1]
#     estimated_point_std_error =  standard_errors
#     estimated_point <- as.numeric(model_summary$coefficients[1])
#     temp = data.frame(Breakpoint = (range_start + range_end) / 2,
#                       Estimated_Point = estimated_point, 
#                       estimated_point_std_error = estimated_point_std_error, 
#                       number_schools = length( unique(subsample$codigo_dane_sede) ) ,
#                       numer_period = length( unique(subsample$year) )
#                       
#     )
#     
#     temp$outcome = outcome
#     estimated_points <- rbind(estimated_points,temp )
#     gc()
#     
#   }
#   
# }
#  
#  
#  
# ###
# 
# gc()
# 
# 
# 
# 
# 
# for (career in unique(estimated_points$outcome)) {
#   print(career)
#   subsample = subset(estimated_points, estimated_points$outcome ==  career)
#   row.names(subsample) <- NULL
#   glimpse(subsample)
#   # Create a bar plot using ggplot2
#   x_continuous = 'Breakpoint'
#   sd_error = 'estimated_point_std_error'
#   estimate_point = 'Estimated_Point'
#   TITULO = paste0('Odds Ratio of\n' , convert_outcome(career) )
#   png(paste0(graphs_dir , 'fe_panel_student_gender_composition_wome_in_', career ,"_10p.png"),  width = 1030, height = 598)
#   plot_coefficients(subsample, estimate_point = estimate_point ,sd_error = sd_error, x_continuous = x_continuous , TITULO= TITULO)
#   dev.off()
# }
# gc()
# 
# 
# write.csv(estimated_points, 'Data/estimated_points_fe_panel_student_gender_composition_wome_10P.csv')
# ###############################################    ###############################################
# # Marginal effect => ME = \beta1 * P*(1-p) 
# ###############################################    ###############################################
# source("Scripts/R/genereal_settings.R", echo=TRUE)
# source("./Scripts/R/read_data.R", echo=TRUE)
# source("./Scripts/R/functions.R", echo=TRUE)
# outcomes = c('ECONOMICS_BUSINESS_RELATED' ,
#              'ENG_ARCH_RELATED',
#              'FINE_ARTS',
#              'MATHEMATICS_NATURAL_SCIENCES',
#              'SOCIAL_SCIENCES_HUMANITIES',
#              'AGRONOMY_VETERINARY_RELATED',
#              'EDUCATION_SCIENCES',
#              'HEALTH_SCIENCES',
#              'NO_STUDIES'
# )
# # FROM data")
# data$fe_group = paste0(data$codigo_dane_sede, ' - ' , data$YEAR_INFO)
# 
# variables = c(outcomes, 'genero' , 'EDAD' , 'frac_males_in_the_group',  'frac_females_in_the_group','tot_students_school_group', 'fe_group')
# 
# data  = data[variables]
# 
# gc()
# data= subset(data, data$frac_females_in_the_group >= 0.025  & data$frac_females_in_the_group <= 0.975 ) 
# gc()
# 
# 
# data$genero <- factor(data$genero, levels = c("M","F"))
# 
# breakpoints <- seq(0, 1, by = 0.1)  
# estimated_points <- data.frame()
# 
# gc()
# 
# for (outcome in outcomes   ) {
#   print(outcome)
#   for (i in 1:(length(breakpoints) - 1)) {
#     # Define the range for the current subsample
#     range_start <- breakpoints[i]
#     range_end <- breakpoints[i + 1]
#     subsample <- data[data$frac_males_in_the_group >= range_start & data$frac_males_in_the_group < range_end, ]
#     gc()
#     if (outcome != "NO_STUDIES" ) {
#       subsample <- subset(subsample, subsample[["NO_STUDIES"]] != 1 )
#       
#     }
#     formuala_ = paste0( ' (',outcome     , ') ~ ' ,  ' genero +EDAD+tot_students_school_group   | fe_group ' )
#     
#     print( paste0('the current estimation is from: ',range_start , ', to: ',range_end ) )
#     
#     model = fixest::feglm( data = subsample, family = 'logit', as.formula(formuala_)  )  
#     gc()
#     # Obtain the summary of the model
#     model_summary <- summary(model)
#     model_summary
#     # Extract the standard errors
#     ame = compute_marginal_effect(model, subsample, 'genero', num_bootstraps = 100) 
#     
#     
#     standard_errors <- model_summary$se
#     standard_errors <- as.numeric(standard_errors[1] ) # model_summary$coefficients[, "Std. Error"]
#     # intercept_std_error =  standard_errors[1]
#     estimated_point_std_error =  standard_errors
#     estimated_point <- as.numeric(model_summary$coefficients[1])
#     temp = data.frame(Breakpoint = (range_start + range_end) / 2,
#                       Estimated_Point = estimated_point, 
#                       estimated_point_std_error = estimated_point_std_error, 
#                       number_schools = length( unique(subsample$codigo_dane_sede) ) ,
#                       numer_period = length( unique(subsample$year)),
#                       ame = ame$marginal_effect,
#                       sd_ame = ame$std_error, 
#                       comparison_base = ame$comparasion_level
#                       
#     )
#     
#     temp$outcome = outcome
#     estimated_points <- rbind(estimated_points,temp )
#     gc()
#     
#   }
#   
# }
# 
# 
# write.csv(estimated_points, 'Data/estimated_points_fe_panel_student_gender_composition_wome_10P_ame.csv')
# 
# 
# 
# 
# for (career  in unique(estimated_points$outcome)) {
#   print(career)
#   subsample = subset(estimated_points, estimated_points$outcome ==  career)
#   row.names(subsample) <- NULL
#   glimpse(subsample)
#   # Create a bar plot using ggplot2
#   x_continuous = 'Breakpoint'
#   sd_error = 'estimated_point_std_error'
#   estimate_point = 'Estimated_Point'
#   
#   TITULO = paste0('Odds-Ratio for choosing:\n' , convert_outcome(career) )
#   png(paste0(graphs_dir , 'fe_panel_student_gender_composition_wome_in_', career ,"_10p.png"),  width = 1030, height = 598)
#   plot_coefficients(subsample, estimate_point = estimate_point ,sd_error = sd_error, x_continuous = x_continuous , TITULO= TITULO)
#   dev.off()
#   
#   
#   TITULO = paste0('Average Marginal Effect for choosing:\n' , convert_outcome(career) )
#   png(paste0(graphs_dir , 'ame_fe_panel_student_gender_composition_wome_in_', career ,"_10p.png"),  width = 1030, height = 598)
#   plot_coefficients(subsample, estimate_point = 'ame' ,
#                                sd_error = 'sd_ame', 
#                                 x_continuous = x_continuous , TITULO= TITULO)
#   dev.off()
# }
# gc()
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# ###############################################    ###############################################
# #
# ###############################################    ###############################################
# 
# 
# source("Scripts/R/genereal_settings.R", echo=TRUE)
# source("./Scripts/R/read_data.R", echo=TRUE)
# source("./Scripts/R/functions.R", echo=TRUE)
# outcomes = c('ECONOMICS_BUSINESS_RELATED' ,
#              'ENG_ARCH_RELATED',
#              'FINE_ARTS',
#              'MATHEMATICS_NATURAL_SCIENCES',
#              'SOCIAL_SCIENCES_HUMANITIES',
#              'AGRONOMY_VETERINARY_RELATED',
#              'EDUCATION_SCIENCES',
#              'HEALTH_SCIENCES',
#              'NO_STUDIES'
# )
# # FROM data")
# data$fe_group = paste0(data$codigo_dane_sede, ' - ' , data$YEAR_INFO)
# 
# variables = c(outcomes, 'genero' , 'EDAD' , 'frac_males_in_the_group',  'frac_females_in_the_group','tot_students_school_group', 'fe_group')
# 
# data  = data[variables]
# 
# gc()
# data= subset(data, data$frac_females_in_the_group >= 0.025  & data$frac_females_in_the_group <= 0.975 ) 
# gc()
# 
# 
# data$genero <- factor(data$genero, levels = c("M","F"))
# 
# breakpoints <- seq(0, 1, by = 0.1)  
# estimated_points <- data.frame()
# 
# gc()
# 
# for (outcome in outcomes[1]  ) {
#   print(outcome)
#   # for (i in 1:(length(breakpoints) - 1)) {
#   #   # Define the range for the current subsample
#   #   range_start <- breakpoints[i]
#   #   range_end <- breakpoints[i + 1]
#     subsample <- data #[data$frac_males_in_the_group >= range_start & data$frac_males_in_the_group < range_end, ]
#     gc()
#     if (outcome != "NO_STUDIES" ) {
#       subsample <- subset(subsample, subsample[["NO_STUDIES"]] != 1 )
#       
#     }
#     formuala_ = paste0( ' (',outcome     , ') ~ ' ,  ' genero * frac_males_in_the_group   | fe_group ' )
#     
#     
#     
#     model = fixest::feglm( data = subsample, family = 'logit', as.formula(formuala_)  )  
#     gc()
#     # Obtain the summary of the model
#     model_summary <- summary(model)
#     model_summary
#     # Extract the standard errors
#     standard_errors <- model_summary$se
#     standard_errors <- as.numeric(standard_errors[1] ) # model_summary$coefficients[, "Std. Error"]
#     # intercept_std_error =  standard_errors[1]
#     estimated_point_std_error =  standard_errors
#     estimated_point <- as.numeric(model_summary$coefficients[1])
#     temp = data.frame( 
#                       Estimated_Point = estimated_point, 
#                       estimated_point_std_error = estimated_point_std_error, 
#                       number_schools = length( unique(subsample$codigo_dane_sede) ) ,
#                       numer_period = length( unique(subsample$year) )
#                       
#     )
#     
#     temp$outcome = outcome
#     estimated_points <- rbind(estimated_points,temp )
#     gc()
#     
# }
# 
# 



 
 