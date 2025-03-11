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
# Load data from parquet file
data = arrow::read_parquet('Data/schooling_decision_grade_11.parquet')

# Define a vector of outcome variables
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


 
 
# Create MEDICINE variable: 1 if AREA_CONOCIMIENTO is 'MEDICINE' and NOMBRE_PROGRAMA contains 'MED', otherwise 0
data <- data %>%
  mutate(MEDICINE = ifelse(AREA_CONOCIMIENTO == 'MEDICINE' & grepl('MED', NOMBRE_PROGRAMA), 1, 0))
# Update AREA_CONOCIMIENTO: if MEDICINE is 1, set AREA_CONOCIMIENTO to 'MEDICINE'
data$AREA_CONOCIMIENTO = ifelse(data$MEDICINE == 1, 'MEDICINE', data$AREA_CONOCIMIENTO)

#Update HEALTH_SCIENCES: If MEDICINE is 1, set HEALTH_SCIENCES to 0 (avoid double counting)
data$HEALTH_SCIENCES = ifelse(data$MEDICINE == 1, 0, data$HEALTH_SCIENCES)

# Create year variable from YEAR_INFO
data$year = data$YEAR_INFO

#Update SOCIAL_SCIENCES_HUMANITIES: If LAW is 1, set SOCIAL_SCIENCES_HUMANITIES to 0 (avoid double counting)
data$SOCIAL_SCIENCES_HUMANITIES = ifelse(data$LAW == 1, 0, data$SOCIAL_SCIENCES_HUMANITIES)
 
# Replaces accented characters with their unaccented equivalents in the NOMBRE_PROGRAMA column
data$NOMBRE_PROGRAMA <- chartr("áéíóúÁÉÍÓÚ", "aeiouAEIOU", data$NOMBRE_PROGRAMA)  

gc()
# Classifies academic programs as STEM or non-STEM using the detectar_stem function
data$STEM <- detectar_stem(data$NOMBRE_PROGRAMA)
gc()
# Filters STEM programs: only considers as STEM those programs classified as STEM by the function and whose level is 'University Program' or 'Technological Program'
# data$STEM <-ifelse(data$STEM==1 & (data$LEVEL=='University Program'  ),1,0 ) #| data$LEVEL=='Technological Program'
# Displays the unique names of the programs classified as STEM

unique(data[data$STEM==1 ,]$NOMBRE_PROGRAMA)

# Classifies academic programs as NO_STEM: if not in studies (NO_STUDIES != 1) and not STEM (STEM != 1), they are classified as NO_STEM
data$NO_STEM = ifelse(data$NO_STUDIES!=1 & data$STEM != 1   , 1, 0 )

data$ENGINEER <- detector_engineering(data$NOMBRE_PROGRAMA)
data$ENGINEER <- ifelse(data$ENGINEER==1 & data$STEM == 1   , 1, 0 )
table(data[data$ENGINEER==1, ]$STEM )


 
data$Ciencia <- detectar_ciencia(data$NOMBRE_PROGRAMA)
data$Ciencia <- ifelse(data$Ciencia==1 & data$STEM == 1   , 1, 0 )

data$Tecnologia <- detectar_tecnologia(data$NOMBRE_PROGRAMA)
data$Tecnologia <- ifelse(data$Tecnologia==1 & data$STEM == 1   , 1, 0 )

data$Ingenieria <- detectar_ingenieria(data$NOMBRE_PROGRAMA)
data$Ingenieria <- ifelse(data$Ingenieria==1 & data$STEM == 1   , 1, 0 )

data$Matematicas <- detectar_matematicas(data$NOMBRE_PROGRAMA)
data$Matematicas <- ifelse(data$Matematicas==1 & data$STEM == 1   , 1, 0 )


unique(data$LEVEL)

data$University <-ifelse(  data$LEVEL=='University Program'   ,1,0 )
data$Technical <-ifelse(  data$LEVEL=="Technical Program"    ,1,0 )
data$Technological <-ifelse(  data$LEVEL=="Technological Program"   ,1,0 )

