library(tibble)
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
             'NO_STUDIES'
)
######################
# outcome <- "NO_STUDIES"
covariates <- c("genero", "EDAD", "tot_students_school_group")
group_var <- "fe_group"
Resultado = data.frame()
for (outcome in outcomes) {
  gc()
  
  print(paste("The outcome running is" , outcome ))
  for (i in seq(5,15) ) {
    print(paste("The sequence is" , i ) )
    tryCatch({
    resultado = calculate_bootstrap_summary(data = data ,
                                            outcome = outcome,
                                            covariates = c("genero", "EDAD", "tot_students_school_group"), 
                                            group_var = "fe_group", 
                                            start_point = 0.001,
                                            end_point = 0.999, 
                                            bw_by = i , 
                                            num_bootstrap_samples = 100)
    resultado$outcome = outcome
    Resultado = rbind(resultado, Resultado)
    cat("Process at outcome: ", outcome, "\n")
    }, error = function(e) {
      cat("Error in calculating bootstrap summary for outcome:", outcome, "and sequence:", i, "\n")
      cat("Error message:", conditionMessage(e), "\n")
    })
  }
  gc()
  
}

arrow::write_parquet(Resultado, "Tables/avg_BCE_bt_100.parquet")
# end_point = 0.999
# start_point = 0.001
# bw_by= 1
# bw_distance <- (end_point - start_point) / bw_by
# sequence <- seq(start_point, end_point, by = bw_distance)
#  
