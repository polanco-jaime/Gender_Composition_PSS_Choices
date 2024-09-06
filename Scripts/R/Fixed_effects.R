################
# We calculate the effect of shocks on DUM_NUCLEO_PETROLEOS and DUM_PRGM_PETROLEOS with fixed effect of time, school and oilfield
#################
library(utils)

colnames(data)
table(data$ID_SCHOOL)
# summary(lm(data=data, DUM_NUCLEO_PETROLEOS_SBPRO~TREAT_P1))
# 
# summary(lm(data=data, DUM_NUCLEO_PETROLEOS~TREAT_P10))
# 
# summary(lm(data=data, DUM_PRGM_PETROLEOS~TREAT_P1))
# 
# summary(lm(data=data, DUM_PRGM_PETROLEOS~TREAT_P10))

# data_ = data[c('DUM_NUCLEO_PETROLEOS', 'TREAT_P1', 'cole_cod_dane_institucion', 'AREA_NOMBR', 'YEAR_SB11', 'TREAT_P10', 'DUM_PRGM_PETROLEOS' )] 
#######
#GENERAL
fixest::feols( TOT_TERC/TOT_STU ~Production_sd | ID_SCHOOL+  AREA_NOMBR +  ANIO   , data )
 

########
# Effect on Tertiary Study Enrollment
########
data_ = subset(data, as.numeric(data$ANIO)<= 2016 )
model_ts = fixest::feols( TOT_TERC/TOT_STU ~TREAT_P1 | ID_SCHOOL+  AREA_NOMBR +  ANIO   , data_ )
summary(model_ts)
 

#-
model_ts_cv = fixest::feols( TOT_TERC/TOT_STU ~TREAT_P1 + AVG_AGE + TOT_MALES/TOT_STU +TOT_CHILD_LABOR/TOT_STU +math_sd_i+ reading_sd_i | ID_SCHOOL+  AREA_NOMBR +  ANIO   , data_ )
summary(model_ts_cv)

 fixest::etable(model_ts, model_ts_cv ,tex = T,  cluster = c( 'ID_SCHOOL') ) 


 model_ts_pro =  ( fixest::feols( TOT_PRO/TOT_STU ~TREAT_P1 | ID_SCHOOL+  AREA_NOMBR +  ANIO   , data_))
 model_ts_tyt =  ( fixest::feols( TOT_TYT/TOT_STU ~TREAT_P1 | ID_SCHOOL+  AREA_NOMBR +  ANIO   , data_ ))

output <- capture.output(fixest::etable(model_ts, model_ts_cv ,model_ts_pro,model_ts_tyt,  tex = T,cluster = c( 'ID_SCHOOL') ))
write(output, "Tables/Effect_on_Tertiary_Study_Enrollment.tex")

########
# Effect on Enrollment of students in Environmental Path
########
model_env = fixest::feols( DUM_PRGM_AMBIENTE ~TREAT_P1 | ID_SCHOOL+  AREA_NOMBR +  ANIO   , data_ )
model_env_pro = fixest::feols( DUM_PRGM_AMBIE_SBPRO ~TREAT_P1 | ID_SCHOOL+  AREA_NOMBR +  ANIO   , data_ )
model_env_tyt = fixest::feols( DUM_PRGM_AMBIENTE_TYT ~TREAT_P1 | ID_SCHOOL+  AREA_NOMBR +  ANIO   , data_ )
 

output <- capture.output(fixest::etable(model_env, model_env_pro, model_env_tyt,tex = T,cluster = c( 'ID_SCHOOL') ))
write(output, "Tables/Effect_on_Environmental_Path_Enrollment.tex")

 
 
########
# Effect on Oil and Gas desision 
########

model_oil = fixest::feols( DUM_PRGM_PETROLEOS/TOT_STU ~TREAT_P1 | ID_SCHOOL+  AREA_NOMBR +  ANIO   , data_ )
model_oil_pro = fixest::feols( DUM_PRGM_PETROLEOS_SBPRO/TOT_STU ~TREAT_P1 | ID_SCHOOL+  AREA_NOMBR +  ANIO   , data_ )
model_oil_tyt = fixest::feols( DUM_PRGM_PETROLEOS_TYT/TOT_STU ~TREAT_P1 | ID_SCHOOL+  AREA_NOMBR +  ANIO   , data_ )
model_oil_cv = fixest::feols( DUM_PRGM_PETROLEOS/TOT_STU ~TREAT_P1 + AVG_AGE + TOT_MALES/TOT_STU +TOT_CHILD_LABOR/TOT_STU +math_sd_i+ reading_sd_i | ID_SCHOOL+  AREA_NOMBR +  ANIO   , data_ )


output <- capture.output(fixest::etable(model_oil,model_oil_cv, model_oil_pro, model_oil_tyt,tex = T,cluster = c( 'ID_SCHOOL') ))
write(output, "Tables/Effect_on_Oil_Path_Enrollment.tex")

########

model1 =  fixest::feols( DUM_NUCLEO_PETROLEOS/TOT_STU ~TREAT_P1 | ID_SCHOOL+  AREA_NOMBR +  ANIO   , data )

model2 =  fixest::feols( DUM_NUCLEO_PETROLEOS/TOT_STU~TREAT_P10 | ID_SCHOOL+   AREA_NOMBR + ANIO    , data )

model3 =  fixest::feols( DUM_PRGM_PETROLEOS/TOT_STU ~TREAT_P1 | ID_SCHOOL+   AREA_NOMBR + ANIO    , data )

model4 =  fixest::feols( DUM_PRGM_PETROLEOS/TOT_STU ~TREAT_P10 |  ID_SCHOOL+   AREA_NOMBR + ANIO    , data )


model5 =  fixest::feols( DUM_PRGM_AMBIENTE/TOT_STU ~TREAT_P1 |   ID_SCHOOL+   AREA_NOMBR + ANIO    , data )


summary(model1)
data$cole_cod_dane_institucion
fixest::etable(model1, model2, model3, model4, model5 ) 

?etable
