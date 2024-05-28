options(scipen=999)

# ###############################################
# #  
# ###############################################
# bq_auth(token = STEP1)
# project_id <- "ph-jabri"
# dataset_id <- "04_gender_career_choices"
# table_id <- "schooling_decision_grade_11"

# data = bigrquery::bq_table_download(
#   as_bq_table(
#   paste0(  project_id, '.' ,  dataset_id   , '.' ,table_id  )
#    ),
#   n_max  = Inf )
# #
# data$NO_STUDIES = ifelse( is.na(data$IES_NOMBRE) ==T , 1, 0 )
# 
# data$fe_group = paste0(data$codigo_dane_sede, ' - ' , data$YEAR_INFO  ) # ,' - ', data$nro_documento
# 
# gc()
# 
# data <- data %>%
#   mutate(MEDICINE = ifelse(AREA_CONOCIMIENTO == 'HEALTH SCIENCES' & grepl('MED', NOMBRE_PROGRAMA), 1, 0))
# 
# data$AREA_CONOCIMIENTO = ifelse(data$MEDICINE == 1, 'MEDICINE', data$AREA_CONOCIMIENTO)
# 
# data$AREA_CONOCIMIENTO = ifelse( is.na(data$AREA_CONOCIMIENTO)==T, 'NO STUDIES', data$AREA_CONOCIMIENTO)
# 
# data$HEALTH_SCIENCES = ifelse(data$MEDICINE == 1, 0, data$HEALTH_SCIENCES)
# 
# outcomes = c(outcomes, 'MEDICINE')
# 
# data <- data %>%
#   mutate(LAW = ifelse(AREA_CONOCIMIENTO == 'SOCIAL SCIENCES AND HUMANITIES' & grepl('DERE', NOMBRE_PROGRAMA), 1, 0))
# 
# 
# outcomes = c(outcomes, 'LAW')
# data$SOCIAL_SCIENCES_HUMANITIES = ifelse(data$LAW == 1, 0, data$SOCIAL_SCIENCES_HUMANITIES)
# 
# arrow::write_parquet(data , 'Data/schooling_decision_grade_11.parquet')
###############################################
#  
###############################################


gc()
data = arrow::read_parquet('Data/schooling_decision_grade_11.parquet')

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
             'LAW',
             'STEM', 'NO_STEM'
)


 
table(data$MEDICINE)

data <- data %>%
  mutate(MEDICINE = ifelse(AREA_CONOCIMIENTO == 'MEDICINE' & grepl('MED', NOMBRE_PROGRAMA), 1, 0))

data$AREA_CONOCIMIENTO = ifelse(data$MEDICINE == 1, 'MEDICINE', data$AREA_CONOCIMIENTO)

# data$AREA_CONOCIMIENTO = ifelse( is.na(data$AREA_CONOCIMIENTO)==T, 'NO STUDIES', data$AREA_CONOCIMIENTO)

data$HEALTH_SCIENCES = ifelse(data$MEDICINE == 1, 0, data$HEALTH_SCIENCES)

# outcomes = c(outcomes, 'MEDICINE')
# 
data$year = data$YEAR_INFO
# data <- data %>%
#   mutate(LAW = ifelse(AREA_CONOCIMIENTO == 'LAW' & grepl('DERE', NOMBRE_PROGRAMA), 1, 0))
# 
# 
# outcomes = c(outcomes, 'LAW')
data$SOCIAL_SCIENCES_HUMANITIES = ifelse(data$LAW == 1, 0, data$SOCIAL_SCIENCES_HUMANITIES)


# Eliminar tildes de la columna 'texto'
data$NOMBRE_PROGRAMA <- chartr("áéíóúÁÉÍÓÚ", "aeiouAEIOU", data$NOMBRE_PROGRAMA)  
data$STEM <- detectar_stem(data$NOMBRE_PROGRAMA)
# data$STEM = ifelse(data$NO_STUDIES!=1 & (data$MATHEMATICS_NATURAL_SCIENCES == 1 | data$ENG_ARCH_RELATED == 1) , 1, 0 )

data$NO_STEM = ifelse(data$NO_STUDIES!=1 & data$STEM != 1   , 1, 0 )
  # ifelse(data$NO_STUDIES==0 &  data$ECONOMICS_BUSINESS_RELATED == 0 & data$ENG_ARCH_RELATED == 0 , 1, 0 ) 
#  
# unique(data$NOMBRE_PROGRAMA)
# unique(data$LEVEL)
# 
# table(data$STEM)
  