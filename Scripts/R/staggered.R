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
# data = sqldf::sqldf("SELECT * ,
# codigo_dane_sede ||' - ' || YEAR_INFO fe_group 
# FROM data")
data$fe_group = paste0(data$codigo_dane_sede, ' - ' , data$YEAR_INFO)

gc()
colnames(data)
table(data$SCHOOL_STATUS)
################################# #################################
# DiD Staggered Sunab - Ex males schools 
################################# #################################

data_panel_by_school  = sqldf::sqldf("
                           SELECT  
                            codigo_dane_sede, 
                            YEAR_INFO year,
                           fe_group fe_group ,TREATMENT_TIME,SCHOOL_STATUS,
                           AVG(tot_students_school_group) tot_students_school_group,
                           AVG(frac_males_in_the_group) frac_males_in_the_group,
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
                                    sum(MEDICINE) MEDICINE, 
                                    sum(LAW) LAW, 
                                 
                           avg(EDAD) age
                            
                           FROM data
                            
                           GROUP BY 1,2,3,4,5 
                           ")


gc()
gc()
# rm(data)
gc()
if(1==1){
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
data_panel_by_school$frac_MEDICINE = data_panel_by_school$MEDICINE / data_panel_by_school$tot_students_school_group
data_panel_by_school$frac_LAW = data_panel_by_school$LAW / data_panel_by_school$tot_students_school_group

data_panel_by_school = subset(data_panel_by_school, data_panel_by_school$tot_students_school_group >= 4)
gc()
# rm(data)
# 
}
# data_panel_by_school = sqldf::sqldf("
#              SELECT * FROM data_panel_by_school
#              WHERE TREATMENT_TIME -  year BETWEEN -7 AND 7
#              ")
colnames(data_panel_by_school)
# data_panel_by_school$year
modelo = feols(frac_ECONOMICS_BUSINESS_RELATED ~ frac_males +age+ tot_students_school_group+
                 sunab(TREATMENT_TIME, year #, ref.p = c(.F + 0:2, -1)
                       ) |
                 codigo_dane_sede + year, 
               subset(data_panel_by_school,data_panel_by_school$SCHOOL_STATUS=='FEMALE') 
               )

# modelo = fixest::feols(data = data_panel_by_school, frac_ECONOMICS_BUSINESS_RELATED ~ frac_females | fe_group )
etable(modelo)
summary(modelo)
fixest::coefplot(modelo)
fixest::iplot(modelo)
####################################################################################
# Ex single FEMALE schools to coeducationl
####################################################################################

data_  = subset(data_panel_by_school,data_panel_by_school$SCHOOL_STATUS=='MALE')   

for (outcome in outcomes  ) {
  print(outcome)
 
 
    subsample <- data_ #[data$frac_males_in_the_group >= range_start & data$frac_males_in_the_group < range_end, ]
    #
    if (outcome != "NO_STUDIES" ) {
      subsample <- subset(subsample, subsample[["NO_STUDIES"]] != 1 )
      
    }
    formuala_ = paste0( 'frac_' , outcome  , ' ~ ' ,  ' frac_males +age+tot_students_school_group+grupo   ', "+
                 sunab(TREATMENT_TIME, year, ref.p = -1 ) |
                 codigo_dane_sede+  year
                        " )   
 
    model = fixest::feols(data = subsample, as.formula(formuala_)  )
    tabla = (SA_table(model) )
    
    png(paste0(graphs_dir , 'stagered_ex_males_', outcome ,".png"),  width = 1030, height = 598)
    
    plot_es = ( event_study_plot(tabla, 
                                 TITULO = convert_outcome( gsub(outcome, pattern = "frac_", replacement = "" ) ),
                                 ref_p=-1 ) )
    print(plot_es)
    
    dev.off()
    
    gc()
}
####################################################################################
# Ex single MALE schools to coeducationl
####################################################################################

data_  = subset(data_panel_by_school,data_panel_by_school$SCHOOL_STATUS=='FEMALE')   

for (outcome in outcomes  ) {
  print(outcome)
  
  
  subsample <- data_ #[data$frac_males_in_the_group >= range_start & data$frac_males_in_the_group < range_end, ]
  #
  if (outcome != "NO_STUDIES" ) {
    subsample <- subset(subsample, subsample[["NO_STUDIES"]] != 1 )
    
  }
  formuala_ = paste0( 'frac_' , outcome  , ' ~ ' ,  ' frac_males +age+tot_students_school_group+grupo   ', "+
                 sunab(TREATMENT_TIME, year, ref.p = -1 ) |
                 codigo_dane_sede + year
                        " )   
  
  model = fixest::feols(data = subsample, as.formula(formuala_)  )
  tabla = (SA_table(model) )
  
  png(paste0(graphs_dir , 'stagered_ex_females_', outcome ,".png"),  width = 1030, height = 598)
  
  plot_es = ( event_study_plot(tabla, 
                               TITULO = convert_outcome( gsub(outcome, pattern = "frac_", replacement = "" ) ),
                               ref_p=-1 ) )
  print(plot_es)
  
  dev.off()
  
  gc()
}
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################


# Estimation fixed effect by school. 
# feglm(Sepal.Length ~ Sepal.Width + Petal.Length | Species, iris, "logit")

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
#   TITULO = convert_outcome(career) 
#   png(paste0(graphs_dir , 'fe_panel_school_gender_composition_wome_in_', career ,"_10p.png"),  width = 1030, height = 598)
#   plot_coefficients(subsample, estimate_point = estimate_point ,sd_error = sd_error, x_continuous = x_continuous , TITULO= TITULO)
#   dev.off()
# }
# gc()