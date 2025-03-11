
source("Scripts/R/genereal_settings.R", echo=TRUE)
source("./Scripts/R/functions.R", echo=TRUE)
source("./Scripts/R/read_data.R", echo=TRUE)


outcomes = c(
  'STEM',
  'NO_STEM',
  'NO_STUDIES',
  'ECONOMICS_BUSINESS_RELATED' ,
  'ENG_ARCH_RELATED',
  'FINE_ARTS',
  'MATHEMATICS_NATURAL_SCIENCES',
  'SOCIAL_SCIENCES_HUMANITIES',
  'AGRONOMY_VETERINARY_RELATED',
  'EDUCATION_SCIENCES',
  'HEALTH_SCIENCES',
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
###################################################################
#
####################################################################
# Convert your data to a data.table
library(data.table)


# Convert your data to a data.table
data <- as.data.table(data)
# result <- data[, .(
#   TOT_MALE = sum(genero == 'M', na.rm = TRUE),
#   TOT_FEMALE = sum(genero == 'F', na.rm = TRUE),
#   TOT_STU = uniqueN(.SD),
#   AVG_AGE = mean(EDAD, na.rm = TRUE),
#   TOT_STEM = 100*sum(  genero == 'F' & STEM==1, na.rm = TRUE)/sum(genero == 'F', na.rm = TRUE),
#   TOT_NO_STEM = 100*sum(  genero == 'F' & NO_STEM==1, na.rm = TRUE)/sum(genero == 'F', na.rm = TRUE),
#   TOT_ECONOMICS_BUSINESS_RELATED = 100*sum(  genero == 'F' & EDUCATION_SCIENCES==1, na.rm = TRUE)/sum(genero == 'F', na.rm = TRUE),
#   TOT_ENG_ARCH_RELATED= 100*sum(  genero == 'F' & ENG_ARCH_RELATED==1, na.rm=T)/sum(genero == 'F', na.rm = TRUE),
#   TOT_FINE_ARTS= 100*sum(  genero == 'F' & FINE_ARTS==1, na.rm=T)/sum(genero == 'F', na.rm = TRUE),
#   TOT_MATHEMATICS_NATURAL_SCIENCES= 100*sum(  genero == 'F' & MATHEMATICS_NATURAL_SCIENCES==1, na.rm=T)/sum(genero == 'F', na.rm = TRUE),
#   TOT_SOCIAL_SCIENCES_HUMANITIES= 100*sum(  genero == 'F' & SOCIAL_SCIENCES_HUMANITIES==1, na.rm=T)/sum(genero == 'F', na.rm = TRUE),
#   TOT_AGRONOMY_VETERINARY_RELATED= 100*sum(  genero == 'F' & AGRONOMY_VETERINARY_RELATED==1, na.rm=T)/sum(genero == 'F', na.rm = TRUE),
#   TOT_EDUCATION_SCIENCES= 100*sum(  genero == 'F' & EDUCATION_SCIENCES==1, na.rm=T)/sum(genero == 'F', na.rm = TRUE),
#   TOT_HEALTH_SCIENCES= 100*sum(  genero == 'F' & HEALTH_SCIENCES==1, na.rm=T)/sum(genero == 'F', na.rm = TRUE),
#   TOT_MEDICINE= 100*sum(  genero == 'F' & MEDICINE==1, na.rm=T)/sum(genero == 'F', na.rm = TRUE),
#   TOT_LAW= 100*sum(  genero == 'F' & LAW==1, na.rm=T)/sum(genero == 'F', na.rm = TRUE),
#   TOT_NO_STUDIES= 100*sum(  genero == 'F' & NO_STUDIES==1, na.rm=T)/sum(genero == 'F', na.rm = TRUE)
# ), by = .(codigo_dane_sede, grupo, YEAR_INFO)]
# gc()
result <- data[, .(
  TOT_MALE = sum(genero == 'M', na.rm = TRUE),
  TOT_FEMALE = sum(genero == 'F', na.rm = TRUE),
  TOT_STU = uniqueN(.SD),
  AVG_AGE = mean(EDAD, na.rm = TRUE),
  TOT_STEM = sum(  genero == 'F' & STEM==1, na.rm = TRUE)/sum(genero == 'F', na.rm = TRUE),
  TOT_NO_STEM = sum(  genero == 'F' & NO_STEM==1, na.rm = TRUE)/sum(genero == 'F', na.rm = TRUE),
  TOT_NO_STUDIES= sum(  genero == 'F' & NO_STUDIES==1, na.rm=T)/sum(genero == 'F', na.rm = TRUE),
  TOT_ECONOMICS_BUSINESS_RELATED = sum(  genero == 'F' & EDUCATION_SCIENCES==1, na.rm = TRUE)/sum(genero == 'F', na.rm = TRUE),
  TOT_ENG_ARCH_RELATED= sum(  genero == 'F' & ENG_ARCH_RELATED==1, na.rm=T)/sum(genero == 'F', na.rm = TRUE),
  TOT_FINE_ARTS= sum(  genero == 'F' & FINE_ARTS==1, na.rm=T)/sum(genero == 'F', na.rm = TRUE),
  TOT_MATHEMATICS_NATURAL_SCIENCES= sum(  genero == 'F' & MATHEMATICS_NATURAL_SCIENCES==1, na.rm=T)/sum(genero == 'F', na.rm = TRUE),
  TOT_SOCIAL_SCIENCES_HUMANITIES= sum(  genero == 'F' & SOCIAL_SCIENCES_HUMANITIES==1, na.rm=T)/sum(genero == 'F', na.rm = TRUE),
  TOT_AGRONOMY_VETERINARY_RELATED= sum(  genero == 'F' & AGRONOMY_VETERINARY_RELATED==1, na.rm=T)/sum(genero == 'F', na.rm = TRUE),
  TOT_EDUCATION_SCIENCES= sum(  genero == 'F' & EDUCATION_SCIENCES==1, na.rm=T)/sum(genero == 'F', na.rm = TRUE),
  TOT_HEALTH_SCIENCES= sum(  genero == 'F' & HEALTH_SCIENCES==1, na.rm=T)/sum(genero == 'F', na.rm = TRUE),
  TOT_MEDICINE= sum(  genero == 'F' & MEDICINE==1, na.rm=T)/sum(genero == 'F', na.rm = TRUE),
  TOT_LAW= sum(  genero == 'F' & LAW==1, na.rm=T)/sum(genero == 'F', na.rm = TRUE)
), by = .(codigo_dane_sede, grupo, YEAR_INFO)]
gc()

rm(data)
gc()
# Function to check if a school transitioned to co-ed in a given year
check_transition <- function(school_data, year) {
  # Exclude the first reported year for the school
  first_year <- min(school_data$YEAR_INFO)
  school_data <- school_data[school_data$YEAR_INFO > first_year, ]
  
  # Filter classrooms with more than 6 students
  school_data <- school_data[school_data$TOT_STU > 6, ]
  
  # Check if the school had only female students before the given year
  before_year <- school_data[school_data$YEAR_INFO < year, ]
  all_female_before <- all(before_year$TOT_MALE == 0)
  
  # Check if the school had at least one male student in the given year
  in_year <- school_data[school_data$YEAR_INFO == year, ]
  at_least_one_male <- any(in_year$TOT_MALE > 0)
  
  # Return TRUE if the school transitioned, FALSE otherwise
  return(all_female_before & at_least_one_male & nrow(before_year) > 0)
}

# Find schools that transitioned to co-ed (with the new conditions)
transition_schools <- unique(result$codigo_dane_sede)

transition_years <- c()

for (school in transition_schools) {
  school_data <- result[result$codigo_dane_sede == school, ]
  years <- unique(school_data$YEAR_INFO)
  
  for (year in years) {
    if (check_transition(school_data, year)) {
      transition_years <- c(transition_years, 
                            paste(school, year, sep = "_"))
      break # Stop checking after the first transition year
    }
  }
}

# Print the schools and their transition years
print(transition_years)

# Extract the school codes and years
transition_df <- data.frame(
  codigo_dane_sede = as.numeric(sapply(strsplit(transition_years, "_"), `[`, 1)),
  transition_year = as.numeric(sapply(strsplit(transition_years, "_"), `[`, 2))
)

sqldf(" SELECT * FROM transition_df")
#Checking
 sqldf( "SELECT * FROM  result WHERE codigo_dane_sede = 268318000215 ORDER BY 3")
 
 merged_data = sqldf( "SELECT * FROM  result A
        INNER JOIN transition_df B
        ON A.codigo_dane_sede = B.codigo_dane_sede ")






# Create a treatment dummy variable
merged_data$treated <- ifelse(merged_data$YEAR_INFO >= merged_data$transition_year, 1, 0)
merged_data$YEAR_INFO = as.numeric(merged_data$YEAR_INFO )
merged_data$transition_year = as.numeric(merged_data$transition_year )
# Create a time-to-treatment variable (relative years since treatment)
merged_data$rel_year <- merged_data$YEAR_INFO - merged_data$transition_year

merged_data[merged_data$rel_year==-1,]$YEAR_INFO

merged_data$male_proportion = (merged_data$TOT_MALE/merged_data$TOT_STU)
merged_data$male_proportion <- as.numeric( scale(merged_data$male_proportion, center = TRUE, scale = TRUE) )
merged_data$intensity_event_time <- merged_data$male_proportion * as.numeric(merged_data$YEAR_INFO - merged_data$transition_year >= 0)
merged_data$intensity_event_time <- as.numeric(scale(merged_data$intensity_event_time))

summary(merged_data$male_proportion )


################################################
#########
vars <- c("TOT_MALE", "TOT_FEMALE", "TOT_STU", "AVG_AGE", "TOT_STEM", "TOT_NO_STEM","TOT_NO_STUDIES" , 
          "TOT_ECONOMICS_BUSINESS_RELATED", "TOT_ENG_ARCH_RELATED", "TOT_FINE_ARTS", 
          "TOT_MATHEMATICS_NATURAL_SCIENCES", "TOT_SOCIAL_SCIENCES_HUMANITIES", 
          "TOT_AGRONOMY_VETERINARY_RELATED", "TOT_EDUCATION_SCIENCES", 
          "TOT_HEALTH_SCIENCES", "TOT_MEDICINE", "TOT_LAW" )
# Generate the LaTeX table
 colnames(merged_data)
 # vars <- c("TOT_STEM","TOT_STEM_F","TOT_NO_STEM"   )
latex_table_output <- create_desc_stats_table_single_sex(merged_data[merged_data$rel_year==-2, c(1:20,22:24)] , vars)
cat(latex_table_output)
for (var in vars) {
  result_summary <- merged_data[merged_data$rel_year==-2, c(1:20,22:24)] %>%
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
###############################################
# ATT_sim_dyn_CS = function(outcome){
#   outcome = paste0('TOT_', outcome)
#   # Estimate the Callaway & Sant'Anna staggered DiD
#   did_result <- att_gt(yname = outcome, # Outcome variable
#                        tname = "YEAR_INFO", # Time variable
#                        idname = "codigo_dane_sede", # School ID
#                        gname = "transition_year", # Treatment group (year of transition)
#                        control_group =  "notyettreated",
#                        data = merged_data,
#                        base_period = "universal",
#                        panel = F,
#                        xformla = ~1, # No covariates in this example (you can add them)
#                        est_method = "dr",  # "dr" for doubly robust estimation
#                        bstrap = TRUE,
#                        biters = 1000, # Recommended: increase for more reliable p-values
#                        cband = TRUE )   
#   
#   # print(summary(did_result))
#   # print(ggdid(did_result))
#   # ?aggte
#   agg.simple <- aggte(did_result, type = "simple", na.rm = TRUE)
#   # print(summary(agg.simple))
#   agg.dynamic <- aggte(did_result, type = "dynamic"  ,min_e = -5, max_e = 5, na.rm = TRUE )
#   # print(summary(agg.dynamic))
#   # ggdid(agg.dynamic)
#   
#   agg.calendar <- aggte(did_result, type = "calendar", na.rm = TRUE)
#   # print(summary(agg.calendar))
#   # ggdid(agg.calendar)
#   
#   return(list(simple = agg.simple, 
#               dynamic = agg.dynamic, 
#               calendar = agg.calendar))
# }

 for (i in outcomes) {
   print(get_var_label(i ) )
   out =  ATT_sim_dyn_CS( i ) 
   summary(out[["dynamic"]])
 }
########      ########

########      ########
print(get_var_label(outcomes[1]) )
stem = ATT_sim_dyn_CS( outcomes[1] )
merged_data$TOT_STEM_sd = as.numeric(scale(merged_data$TOT_STEM)) 
stem_sd = ATT_sim_dyn_CS( "STEM_sd" )

ggdid(stem$conditional_pretest)

paste0("the p-value of the Wald statistic for pre-testing the common trends assumption: ", 
       stem[["conditional_pretest"]][["Wpval"]])

paste0("the p-value of the Wald statistic for pre-testing the common trends assumption: ", 
       stem_sd[["conditional_pretest"]][["Wpval"]])

ggdid(stem[["dynamic"]])
summary(stem[["simple"]])
summary(stem[["dynamic"]])
png(paste0(graphs_dir,'CS_',outcomes[1],".png"),  width = 1030, height = 598)
plot_ev_CS(stem_sd, field_study=get_var_label(outcomes[1]))
dev.off() 
########      ########
print(get_var_label(outcomes[2]) )
no_stem = ATT_sim_dyn_CS( outcomes[2] )
paste0("the p-value of the Wald statistic for pre-testing the common trends assumption: ", 
       no_stem[["conditional_pretest"]][["Wpval"]])
ggdid(no_stem[["dynamic"]])
summary(no_stem[["dynamic"]])

png(paste0(graphs_dir,'CS_',outcomes[2],".png"),  width = 1030, height = 598)
plot_ev_CS(no_stem, field_study=get_var_label(outcomes[2]))
dev.off() 
########      ########
print(get_var_label(outcomes[3]) )
dropout = ATT_sim_dyn_CS( outcomes[3] )
# ggdid(dropout[["dynamic"]])
# summary(dropout[["simple"]])
summary(dropout[["dynamic"]])

png(paste0(graphs_dir,'CS_',outcomes[3],".png"),  width = 1030, height = 598)
plot_ev_CS(dropout, field_study=get_var_label(outcomes[3]))
dev.off() 
########      ########
# Other results
########      ########
### ENG Results
print(get_var_label(outcomes[1]) )
eco = ATT_sim_dyn_CS( outcomes[1] )
ggdid(eco[["dynamic"]])
summary(eco[["dynamic"]])

png(paste0(graphs_dir,'CS_',outcomes[1],".png"),  width = 1030, height = 598)
plot_ev_CS(eco, field_study=get_var_label(outcomes[1]))
dev.off() 
########      ########
print(get_var_label(outcomes[2]) )
eng = ATT_sim_dyn_CS( outcomes[2] )
ggdid(eng[["dynamic"]])
summary(eng[["dynamic"]])

png(paste0(graphs_dir,'CS_',outcomes[2],".png"),  width = 1030, height = 598)
plot_ev_CS(eng, field_study=get_var_label(outcomes[2]) )
dev.off() 


########      ########
print(get_var_label(outcomes[3]) )
art = ATT_sim_dyn_CS( outcomes[3] )
ggdid(art[["dynamic"]])
summary(art[["dynamic"]])

png(paste0(graphs_dir,'CS_',outcomes[3],".png"),  width = 1030, height = 598)
plot_ev_CS(art, field_study=get_var_label(outcomes[3]))
dev.off() 

########      ########
print(get_var_label(outcomes[4]) )
math = ATT_sim_dyn_CS( outcomes[4] )
ggdid(math[["dynamic"]])
summary(math[["dynamic"]])

png(paste0(graphs_dir,'CS_',outcomes[4],".png"),  width = 1030, height = 598)
plot_ev_CS(math, field_study=get_var_label(outcomes[4]))
dev.off() 

########      ########
print(get_var_label(outcomes[5]) )
humani = ATT_sim_dyn_CS( outcomes[5] )
ggdid(humani[["dynamic"]])
summary(humani[["dynamic"]])

png(paste0(graphs_dir,'CS_',outcomes[5],".png"),  width = 1030, height = 598)
plot_ev_CS(humani, field_study=get_var_label(outcomes[5]))
dev.off() 

########      ########
print(get_var_label(outcomes[6]) )
agro = ATT_sim_dyn_CS( outcomes[6] )
ggdid(agro[["dynamic"]])
summary(agro[["dynamic"]])

png(paste0(graphs_dir,'CS_',outcomes[6],".png"),  width = 1030, height = 598)
plot_ev_CS(agro, field_study=get_var_label(outcomes[6]) )
dev.off() 

########      ########
print(get_var_label(outcomes[7]) )
educ = ATT_sim_dyn_CS( outcomes[7] )
ggdid(educ[["dynamic"]])
summary(educ[["dynamic"]])

png(paste0(graphs_dir,'CS_',outcomes[7],".png"),  width = 1030, height = 598)
plot_ev_CS(educ, field_study=get_var_label(outcomes[7]))
dev.off() 

########      ########
print(get_var_label(outcomes[8]) )
health = ATT_sim_dyn_CS( outcomes[8] )
ggdid(health[["dynamic"]])
summary(health[["dynamic"]])

png(paste0(graphs_dir,'CS_',outcomes[8],".png"),  width = 1030, height = 598)
plot_ev_CS(health, field_study=get_var_label(outcomes[8]))
dev.off() 




########      ########
print(get_var_label(outcomes[10]) )
medicine = ATT_sim_dyn_CS( outcomes[10] )
ggdid(medicine[["dynamic"]])
summary(medicine[["dynamic"]])

png(paste0(graphs_dir,'CS_',outcomes[10],".png"),  width = 1030, height = 598)
plot_ev_CS(medicine, field_study=get_var_label(outcomes[10]))
dev.off() 


########      ########
print(get_var_label(outcomes[11]) )
law = ATT_sim_dyn_CS( outcomes[11] )
ggdid(law[["dynamic"]])
summary(law[["dynamic"]])

png(paste0(graphs_dir,'CS_',outcomes[11],".png"),  width = 1030, height = 598)
plot_ev_CS(law, field_study=get_var_label(outcomes[11]))
dev.off() 

################################# #################################
# DiD Staggered Sunab - Ex males schools 
################################# #################################
# 
# data_panel_by_school  = sqldf::sqldf("
#                            SELECT  
#                             codigo_dane_sede, 
#                             YEAR_INFO year,
#                            fe_group fe_group ,
#                            TREATMENT_TIME,
#                            SCHOOL_STATUS,
#                            grupo,
#                            
#                             AVG(tot_students_school_group) tot_students_school_group,
#                             AVG(frac_males_in_the_group) frac_males_in_the_group,
#                             SUM( CASE WHEN genero = 'M' THEN 1 ELSE 0 END) MALES,
#                             SUM( CASE WHEN genero = 'F' THEN 1 ELSE 0 END) FEMALES,
#                             sum(ECONOMICS_BUSINESS_RELATED) ECONOMICS_BUSINESS_RELATED, 
#                            
#                             sum(ECONOMICS_BUSINESS_RELATED) ECONOMICS_BUSINESS_RELATED, 
#                             sum(ENG_ARCH_RELATED) ENG_ARCH_RELATED, 
#                             sum(FINE_ARTS) FINE_ARTS, 
#                             sum(MATHEMATICS_NATURAL_SCIENCES) MATHEMATICS_NATURAL_SCIENCES, 
#                             sum(SOCIAL_SCIENCES_HUMANITIES) SOCIAL_SCIENCES_HUMANITIES, 
#                             sum(AGRONOMY_VETERINARY_RELATED) AGRONOMY_VETERINARY_RELATED, 
#                             sum(EDUCATION_SCIENCES) EDUCATION_SCIENCES, 
#                             sum(HEALTH_SCIENCES) HEALTH_SCIENCES, 
#                             sum(NO_STUDIES) NO_STUDIES, 
#                             sum(MEDICINE) MEDICINE, 
#                             sum(LAW) LAW, 
#                             sum(STEM) STEM, 
#                             sum(NO_STEM) NO_STEM, 
#                                  
#                            avg(EDAD) age
#                             
#                            FROM data
#                             
#                            GROUP BY 1,2,3,4,5 ,6
#                            ")
# 
# 
# gc()
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
data_panel_by_school$frac_STEM = data_panel_by_school$STEM / data_panel_by_school$tot_students_school_group
data_panel_by_school$frac_NO_STEM = data_panel_by_school$NO_STEM / data_panel_by_school$tot_students_school_group

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
modelo = feols(frac_ECONOMICS_BUSINESS_RELATED ~ frac_males + 
                 sunab(TREATMENT_TIME, year , ref.p = c(.F + .L, -1)
                       ) |
                 codigo_dane_sede+grupo + year, 
               subset(data_panel_by_school,data_panel_by_school$SCHOOL_STATUS=='FEMALE') 
               )
# modelo = lm(data=subset(data_panel_by_school, data_panel_by_school$frac_ECONOMICS_BUSINESS_RELATED!=0 & data_panel_by_school$frac_males!=0),
            # log(frac_ECONOMICS_BUSINESS_RELATED) ~ log(frac_males))
summary(modelo)
# modelo = fixest::feols(data = data_panel_by_school, frac_ECONOMICS_BUSINESS_RELATED ~ frac_females | fe_group )
etable(modelo)
summary(modelo)
# fixest::coefplot(modelo)
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
                 sunab(TREATMENT_TIME, year, ref.p = -1  ) |
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
colnames(merged_data)
 

mean_sd_function = function(Variable){
  return(paste0( round(mean(Variable   , na.rm = TRUE), 3),' (', round(sd(Variable   , na.rm = TRUE), 3),')')) 
}


summary_descriptive = merged_data[merged_data$rel_year == -1, c(1:20, 22:24)] %>%
  dplyr::group_by(YEAR_INFO) %>%
  dplyr::reframe(
    `Total students (N)` = sum(TOT_STU, na.rm = TRUE),  # First, calculate total students per group
    `Male Students (N)` = round(sum(TOT_MALE, na.rm = TRUE) / TOT_STU,1),
    `Female Students (N)` = round(sum(TOT_FEMALE, na.rm = TRUE) / TOT_STU, 1),
    `Average Age (Years)` = mean(AVG_AGE, na.rm = TRUE),
    `STEM Fields` = mean_sd_function(TOT_STEM), # paste0(mean(TOT_STEM   , na.rm = TRUE),' (', sd(TOT_STEM   , na.rm = TRUE),')'), # No need to multiply by 100 or filter by gender here
    `Non-STEM Fields` = mean_sd_function(TOT_NO_STEM),
    `Economics/Business` = mean_sd_function(TOT_ECONOMICS_BUSINESS_RELATED  ),
    `Not Continuing Education` = mean_sd_function(TOT_NO_STUDIES )
  )
summary_by_year
colnames(summary_by_year)
