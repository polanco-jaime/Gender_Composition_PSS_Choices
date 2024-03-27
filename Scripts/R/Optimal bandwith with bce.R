library(tibble)
source("Scripts/R/genereal_settings.R", echo=TRUE)
source("./Scripts/R/read_data.R", echo=TRUE)
source("./Scripts/R/functions.R", echo=TRUE)

outcomes = c(
  'ECONOMICS_BUSINESS_RELATED' ,
  'ENG_ARCH_RELATED',
  'FINE_ARTS',
  'MATHEMATICS_NATURAL_SCIENCES',
  'SOCIAL_SCIENCES_HUMANITIES',
  'AGRONOMY_VETERINARY_RELATED', # 9 at 1024pm
  'EDUCATION_SCIENCES',
  'HEALTH_SCIENCES',
  'NO_STUDIES', 
  'MEDICINE',
  'LAW'
)

 
table(data$MEDICINE)
######################
# outcome <- "NO_STUDIES"
covariates <- c("genero", "EDAD", "tot_students_school_group")
group_var <- "fe_group"
Resultado = data.frame()


 
for (outcome in outcomes) {
  gc()
  
  print(paste("The outcome running is" , outcome ))
  for (i in seq(9,15) ) {
    print(paste("The sequence is" , i ) )
    tryCatch({
    resultado = calculate_bootstrap_summary(data = data ,
                                            outcome = outcome,
                                            covariates = c("genero", "EDAD", "tot_students_school_group"), 
                                            group_var = "fe_group", 
                                            start_point = 0.001,
                                            end_point = 0.999, 
                                            bw_by = i , 
                                            num_bootstrap_samples = 5)
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

Resultado = rbind(Resultado, Resultado_filtrado)
# arrow::write_parquet(Resultado, "Tables/avg_BCE_bt_30_.parquet")
Resultado = arrow::read_parquet(( "Tables/avg_BCE_bt_30_.parquet"))

Resultado_ = arrow::read_parquet(( "Tables/avg_BCE_bt_30.parquet"))
Resultado_filtrado <- Resultado_ %>%   filter(!outcome %in% outcomes)
# end_point = 0.999
# start_point = 0.001
# bw_by= 1
# bw_distance <- (end_point - start_point) / bw_by
# sequence <- seq(start_point, end_point, by = bw_distance)
#  
  


# Example usage:
# plot_with_errorbars(Resultado, "Optimal_Distance", "Average_BCE_mean", "Confidence_Interval_Lower", "Confidence_Interval_Upper")
table(Resultado$outcome)
for (i in unique(Resultado$outcome)) {
  print(convert_outcome(i))
  png(paste0(graphs_dir , '/optimal_distance_bs30_', i ,".png"),  width = 1030, height = 598)
  plot = plot_with_errorbars(Resultado[Resultado$outcome==i,], 
                      'Optimal_Distance',  "Average_BCE_mean", 
                      "Confidence_Interval_Lower", "Confidence_Interval_Upper", (convert_outcome(i))) 
  print(plot)
  dev.off()
  
}


obtimal_distance =  data.frame()
 
for (y_col in outcomes) {
  min_row <- Resultado %>% subset( Resultado[['outcome']] == y_col )
  min_row <-  min_row[which.min( min_row[['Average_BCE_mean' ]]) ,  ] 
  print(min_row)
  obtimal_distance = rbind(min_row,obtimal_distance )
  
}

library(jsonlite)

# Assuming your dataframe is named 'df'
json_data <- toJSON(obtimal_distance, pretty = TRUE)

# Save JSON data to a file
write(json_data, file = "Tables/obtimal_distance.json")

# Print JSON data
cat(json_data)
 
