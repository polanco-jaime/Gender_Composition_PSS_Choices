options(scipen=999)
# bq_auth(token = STEP1)
# project_id <- "ph-jabri"
# dataset_id <- "04_gender_career_choices"
# table_id <- "correlations"
# data = bigrquery::bq_table_download(
#   as_bq_table(
#   paste0(  project_id, '.' ,  dataset_id   , '.' ,table_id  )
#    ),
#   n_max  = Inf )
# 
# data$NO_STUDIES = ifelse( is.na(data$SEXO_BIOLOGICO) ==T , 1, 0 )
# arrow::write_parquet(data , 'Data/correlations.parquet')
# gc()
data = arrow::read_parquet('Data/correlations.parquet')

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
