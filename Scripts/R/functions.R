options(scipen=999)
table_save = function(result,caption, file_name ) {
  position_start = str_locate(result, "begin\\{tabular\\}")
  position_start  = as.data.frame(subset(is.na(position_start)   ))
  position_start= which(!position_start[[1]])
  
  position_end = as.data.frame(subset(is.na( str_locate(result, "end\\{tabular\\}") ) ) )
  position_end= which(!position_end[[1]])
  
  
  starting = paste0( "
\\begin{table}[!htbp] 
    \\centering
\\caption{", caption ,"}
\\begin{adjustbox}{width=\\textwidth}  
")
  
  result = result[position_start: position_end]
  result <- paste(result, collapse = "\n")
  
  ending = "
\\end{adjustbox}  
\\label{tab:tab1}
\\end{table} "
  
  result <-paste(starting, result, ending, collapse = "\n")
  cat(result)
  
  file_path = paste0('Tables/', file_name, '.tex')
  # Write the text to the file
  writeLines(result, file_path)
  
}

 



plot_coefficients = function (data, estimate_point = '' ,sd_error = '', x_continuous = '' , TITULO= '', polynom = 3 ) {
  out = subset(data,  (data$Breakpoint)<=2 )
  horizon_value =  subset(data,  (data$Breakpoint)>=2 )
  sd_error_ = as.numeric(horizon_value[[ 'estimated_point_std_error']])
  horizon_value = as.numeric(horizon_value[[ 'Estimated_Point']])
  horizon = NULL
  color_scale = "#00A1D5"
  out$term = out[[x_continuous]]
  out$estimate = out[[estimate_point]] 
  mynamestheme <- ggplot2::theme(
    plot.title = element_text(family = "Helvetica", face = "bold", size = (15), hjust = 0.5, vjust = 0.5),
    legend.title = element_text(colour = "steelblue", face = "bold.italic", family = "Helvetica"),
    legend.text = element_text(face = "italic", colour = "steelblue4", family = "Helvetica"),
    axis.title = element_text(family = "Helvetica", face = "bold", size = (12), colour = "steelblue4"),
    axis.text = element_text(family = "Courier", face = "bold", colour = "cornflowerblue", size = (12)),
    legend.position = "bottom" )
  
  
  out$ci_lower = out[[estimate_point]] - 1.96 * out[[sd_error]]
  out$ci_upper = out[[estimate_point]] + 1.96 * out[[sd_error]]
  out$ci_lower_90 = out[[estimate_point]] - 1.645 * out[[sd_error]]
  out$ci_upper_90 = out[[estimate_point]] + 1.645 * out[[sd_error]]
  
  # Intervalo de confianza del 99%
  out$ci_lower_99 = out[[estimate_point]] - 2.576 * out[[sd_error]]
  out$ci_upper_99 = out[[estimate_point]] + 2.576 * out[[sd_error]]
  
  position = ggplot2::position_dodge(width = 0.05 )
  
  
  y_lims = c(min(out$ci_lower), max(out$ci_upper)) * 1.05
  x_lims = c(0, 1)
  y_label_ = ("Point Estimate and 95% Confidence Interval ") # \n  Likelihood of a female student choosing a career 
  x_label_ = "Proportion of males within a class group"
  Plot = ggplot2::ggplot(data = out, ggplot2::aes(x = .data$term, 
                                                  y = .data$estimate,  
                                                  ymin = .data$ci_lower , 
                                                  ymax = .data$ci_upper)) +  theme_light()  +
    ggplot2::geom_point(position = position, size = 0.5) + 
    # ggplot2::geom_errorbar(position = position,   width = 0.01) + 
    ggplot2::geom_vline(xintercept = -1, linetype = "dashed") + 
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") + 
    ggplot2::ggtitle(TITULO)+ ggplot2::scale_x_continuous(limits = x_lims, 
                                                          breaks = 
                                                            seq( round(min(out$term)*0.95, 2) , 
                                                                 round(max(out$term)*1.05, 2 ), 
                                                                 by = round(max(out$term)/10  ,  2)) ) +
    # ggplot2::geom_tile(colour="white" ) +
    ggplot2::theme(  plot.title = element_text(hjust = 0.5, vjust = 0.5),
                     axis.line.x = element_line(color="steelblue4", size = 0.05),
                     axis.line.y = element_line(color="steelblue4", size = 0.5)) +
      ggplot2::labs(y = y_label_, 
                  x = x_label_ , color = "Estimator" ) + 
      ggplot2::scale_color_manual(values = color_scale) 
    P =  Plot +   mynamestheme +
      geom_ribbon(
        aes(ymin = .data$ci_lower_90, ymax = .data$ci_upper_90),
        position = position,
        fill = "lightgray",
        alpha = 0.45,
        show.legend = FALSE 
      )    +
      geom_ribbon(
        aes(ymin = .data$ci_lower, ymax = .data$ci_upper),
        position = position,
        fill = "gray",
        alpha = 0.45,
        show.legend = FALSE 
      )    +
      geom_ribbon(
        aes(ymin = .data$ci_upper_99, ymax = .data$ci_upper_99),
        position = position,
        fill = "darkgray",
        alpha = 0.45,
        show.legend = FALSE 
      )  +  
      geom_line(position = position, color = "steelblue4", size = 1, alpha = 0.7,
                show.legend = FALSE)+
      geom_hline(yintercept = horizon_value, linetype = "dashed", color = "red") +
      geom_ribbon(
        aes(ymin = horizon_value - 1.96 * sd_error_, ymax = horizon_value + 1.96 * sd_error_),
        fill = "red", alpha = 0.2
      )+
      geom_text(aes(x = max(out$term) + 0.01, y = horizon_value, label = round(horizon_value, 3)), 
                vjust = -0.5, hjust = 0, color = "red", size = 4, alpha = 0.2) +
      geom_point(position = position, size = 0.5, aes(color = "Overall")) + 
      geom_point(position = position, size = 0.5, aes(color = "By Gender Composition")) +
      scale_color_manual(values = c("Overall" = "red", "By Gender Composition" = "blue"))  +
      theme(
        legend.position = "top",
        legend.box.background = element_rect(color = "black", size = 0.3),
        # legend.box.background = element_blank(),  # No box background
        legend.margin = margin(0),  # No margin
        legend.key.size = unit(1, "lines"),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10),
        plot.title = element_text(hjust = 0.5, vjust = 1.5)
      )
       
  
   
  # 
  return(P)
}


plot_coefficients_smooth = function (data, estimate_point = '' ,sd_error = '', x_continuous = '' , TITULO= '', polynom = 3) {
  out = subset(data,  (data$Breakpoint)<=2 )
  horizon_value =  subset(data,  (data$Breakpoint)>=2 )
  sd_error_ = as.numeric(horizon_value[[ 'estimated_point_std_error']])
  horizon_value = as.numeric(horizon_value[[ 'Estimated_Point']])
  
  horizon = NULL
  color_scale = "#00A1D5"
  out$term = out[[x_continuous]]
  out$estimate = out[[estimate_point]] 
  mynamestheme <- ggplot2::theme(
    plot.title = element_text(family = "Helvetica", face = "bold", size = (15), hjust = 0.5, vjust = 0.5),
    legend.title = element_text(colour = "steelblue", face = "bold.italic", family = "Helvetica"),
    legend.text = element_text(face = "italic", colour = "steelblue4", family = "Helvetica"),
    axis.title = element_text(family = "Helvetica", face = "bold", size = (12), colour = "steelblue4"),
    axis.text = element_text(family = "Courier", face = "bold", colour = "cornflowerblue", size = (12)),
    legend.position = "bottom" )
  
  
  out$ci_lower = out[[estimate_point]] - 1.96 * out[[sd_error]]
  out$ci_upper = out[[estimate_point]] + 1.96 * out[[sd_error]]
  
  position = ggplot2::position_dodge(width = 0.05 )
  
  
  y_lims = c(min(out$ci_lower), max(out$ci_upper)) * 1.05
  x_lims = c(0, 1.05)
  y_label_ = ("Point Estimate and 95% Confidence Interval ") # \n  Likelihood of a female student choosing a career 
  x_label_ = "Proportion of males within a class group"
  Plot = ggplot2::ggplot(data = out, ggplot2::aes(x = .data$term, 
                                                  y = .data$estimate,  
                                                  ymin = .data$ci_lower , 
                                                  ymax = .data$ci_upper)) +  theme_light()  +
    ggplot2::geom_point(position = position, size = 0.5) + 
    ggplot2::geom_errorbar(position = position,   width = 0.01) + 
    ggplot2::geom_vline(xintercept = -1, linetype = "dashed") + 
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") + 
    ggplot2::ggtitle(TITULO)+ ggplot2::scale_x_continuous(limits = x_lims, 
                                                          breaks = 
                                                            seq( round(min(out$term)*0.95, 2) , 
                                                                 round(max(out$term)*1.05, 2 ), 
                                                                 by = round(max(out$term)/10  ,  2)) ) +
    # ggplot2::geom_tile(colour="white" ) +
    ggplot2::theme(  plot.title = element_text(hjust = 0.5, vjust = 0.5),
                     axis.line.x = element_line(color="steelblue4", size = 0.05),
                     axis.line.y = element_line(color="steelblue4", size = 0.5)) +
    ggplot2::labs(y = y_label_, 
                  x = x_label_ , color = "Estimator" ) + 
    ggplot2::scale_color_manual(values = color_scale) +
    geom_hline(yintercept = horizon_value, linetype = "dashed", color = "red") +
    geom_ribbon(
      aes(ymin = horizon_value - 1.96 * sd_error_, ymax = horizon_value + 1.96 * sd_error_),
      fill = "red", alpha = 0.2
    ) +
    geom_text(aes(x = max(out$term) + 0.01, y = horizon_value, label = round(horizon_value, 3)), 
              vjust = -0.5, hjust = 0, color = "red", size = 4, alpha = 0.2) +
    geom_point(position = position, size = 0.5, aes(color = "Overall")) + 
    geom_point(position = position, size = 0.5, aes(color = "By Gender Composition")) +
    scale_color_manual(values = c("Overall" = "red", "By Gender Composition" = "blue")) +
    # theme(
    #   legend.position = "top",
    #   legend.box.background = element_rect(color = "black", size = 1),
    #   legend.margin = margin(5, 5, 5, 5),
    #   legend.key.size = unit(1, "lines"),
    #   legend.title = element_text(size = 8),
    #   legend.text = element_text(size = 6),
    #   plot.title = element_text(hjust = 0.1, vjust = 1.5)
    # )
  
    theme(
      legend.position = "top",
      legend.box.background = element_rect(color = "black", size = 0.3),
      # legend.box.background = element_blank(),  # No box background
      legend.margin = margin(0),  # No margin
      legend.key.size = unit(1, "lines"),
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 10),
      plot.title = element_text(hjust = 0.5, vjust = 1.5)
    )
  P =  Plot +  geom_smooth(method = "lm", formula = y ~ poly(x, polynom), se = T, color = "steelblue4", fill = "gray") 
  # +
  # geom_ribbon(
  #   aes(ymin = .data$ci_lower, ymax = .data$ci_upper),
  #   position = position,
  #   fill = "gray",
  #   alpha = 0.5
  # ) 
  # 
  return(P)
}


convert_outcome <- function(outcome) {
  List <- c(
    'Economics, Business & related Careers',
    'Engineering, Architecture and related Careers',
    'Fine Arts',
    'Mathematics and Natural Sciences',
    'Social Sciences and Humanities',
    'Agronomy, Veterinary and related Careers',
    'Education Sciences',
    'Health Sciences (except medicine)',
    'No Studies',
    'Law',
    'Medicine'
  )
  outcomes <- c(
    'ECONOMICS_BUSINESS_RELATED',
    'ENG_ARCH_RELATED',
    'FINE_ARTS',
    'MATHEMATICS_NATURAL_SCIENCES',
    'SOCIAL_SCIENCES_HUMANITIES',
    'AGRONOMY_VETERINARY_RELATED',
    'EDUCATION_SCIENCES',
    'HEALTH_SCIENCES',
    'NO_STUDIES',
    'LAW',
    'MEDICINE'
  )
  
  if (outcome %in% outcomes) {
    return(List[outcomes == outcome])
  } else {
    return("Outcome not found")
  }
}






#######################################################################
# Compute of the Average Marginal Effect
########################################################################
compute_marginal_effect <- function(model, data, variable_of_interest, num_bootstraps = 100) {
  # data = subsample
  marginal_effects <- vector(mode = "numeric", length = num_bootstraps)
  sd_me <- vector(mode = "numeric", length = num_bootstraps)
  orig_prob <- predict(model, type = "response", newdata = data)
  
  for (i in 1:num_bootstraps) {
    boot_data <- data[sample(nrow(data), replace = TRUE), ]
    prob_base <- predict(model, type = "response",
                         newdata = subset(boot_data, boot_data[[variable_of_interest]] == levels(boot_data[[variable_of_interest]])[1] )  )
    # predict(model, type = "response", newdata = boot_data)
    
    # prob_base <- 1/(1+ exp(-(prob_base) ) ) 
    
    prob_base_mod <- predict(model, type = "response", 
                             newdata = subset(boot_data, boot_data[[variable_of_interest]] != levels(boot_data[[variable_of_interest]])[1] ) 
    )
    # prob_base_mod <- 1/(1+ exp(-(prob_base_mod) ) ) 
    marginal_effects[i] <- mean(prob_base_mod - prob_base , na.rm = T)
    sd_me[i] <- sd( marginal_effects[i] , na.rm = T) #- sd(prob_base, na.rm = T)
  }
  
  se <- round( sd(marginal_effects, na.rm = T) , 5 )
  mean_marginal_effect <- round(mean(marginal_effects) , 5)
  
  list("marginal_effect" = mean_marginal_effect, "std_error" = se ,  
       "comparasion_level" =  levels(data[[variable_of_interest]])[1]
       )
}



categorize_stem <- function(data) {
  # Create a new column 'STEM' with values 'STEM' or 'NO STEM'
  data <- data %>%
    mutate(STEM = ifelse(grepl("ENGINEERING|MATHEMATICS|HEALTH SCIENCES|MEDICINE", AREA_CONOCIMIENTO, ignore.case = TRUE), 'STEM', 'NO STEM'))
  
  return(data)
}
#################
#
#################
# Define a function to estimate the optimal bandwidth based on logistic regression

calculate_bootstrap_summary <- function(data, outcome, covariates, group_var, start_point, end_point, bw_by, num_bootstrap_samples) {
  bw_distance <- (end_point - start_point) / bw_by
  sequence <- seq(start_point, end_point, by = bw_distance)
  
  bootstrap_results <- data.frame(lower_bound = numeric(), upper_bound = numeric(), mean_bce = numeric())
  
  for (j in 1:num_bootstrap_samples) {
    bootstrap_data <- data[sample( as.integer(nrow(data)*0.8), replace = TRUE), ]
    
    for (i in 1:(length(sequence) - 1)) {
      lower_bound <- sequence[i]
      upper_bound <- sequence[i + 1]
      
      subsample <- bootstrap_data[bootstrap_data$frac_males_in_the_group >= lower_bound & 
                                    bootstrap_data$frac_males_in_the_group < upper_bound, ]
      schools <- length(unique(subsample$codigo_dane_sede))
      students <- nrow(subsample)
      
      formula <- paste0("(", outcome, ") ~ ", paste(covariates, collapse = "+"), 
                        " | ", group_var)
      model <- fixest::feglm(data = subsample, family = 'binomial', as.formula(formula))
      gc()
      predicted_probs <- predict(model, type = "response")
      rm(model)
      bce <- ifelse(subsample[[outcome]] == 1, -log(predicted_probs), -log(1 - predicted_probs))
      mean_bce <- mean(bce)
      
      result <- data.frame(lower_bound = lower_bound,
                           upper_bound = upper_bound,
                           mean_bce = mean_bce)
      
      bootstrap_results <- rbind(bootstrap_results, result)
      
    }
  }
  
  variance <- var(bootstrap_results$mean_bce)
  mean_ <- mean(bootstrap_results$mean_bce)
  confidence_interval <- t.test(bootstrap_results$mean_bce)$conf.int
  stability <- sd(bootstrap_results$mean_bce)
  
  results_table <- data.frame('Average_BCE_mean' = mean_,
                              "Standar_Desviation" = stability,
                              "Variance_BCE_mean" = variance,
                              "Confidence_Interval_Lower" = confidence_interval[1],
                              "Confidence_Interval_Upper" = confidence_interval[2],
                              "Optimal_Distance" = bw_distance,
                              'outcome'  =outcome)
  
  return(results_table)
}


plot_with_errorbars <- function(data, x_col, y_col, ci_lower_col, ci_upper_col,title="") {
  min_row <-  data[which.min(data[[y_col]]), ] 
  optimal_dista = round(as.numeric(data[which.min(data[[ y_col]]), ][x_col]) , 4)
  subtitle = paste0("The optimal distance in wich the grups have the minimum entropy is: ", optimal_dista)
  ggplot(data, aes_string(x = x_col, y = y_col)) +
    geom_point(color = ifelse(data[[y_col]] == min_row[[y_col]], "red", "blue")) +
    geom_errorbar(aes_string(ymin = ci_lower_col, ymax = ci_upper_col), color = ifelse(data[[y_col]] == min_row[[y_col]], "red", "blue"), width = 0.005) +
    labs(x = x_col, y = y_col, title = title, subtitle = subtitle) +
    
    # annotate("text", x = min_row[[x_col]], y = min_row[[y_col]], label = as.character(min_row), color = "red", size = 2, hjust = 0, vjust = -0.5) +
    theme_minimal()
  
  
}




estimate_point_logit <- function(outcome,  subsample, covariates, group_var) {
  covariates <- paste(covariates, collapse = " + ")
  formula_ <- paste0(' ', outcome, ' ~ ', covariates  , ' | ',  group_var)
  
 
  
  # Fit the model
  model <- fixest::feglm(data = subsample, family = 'logit', as.formula(formula_))
  
  # Extract standard errors
  standard_errors <- summary(model)$se
  estimated_point_std_error <- as.numeric(standard_errors[1])
  estimated_point <- as.numeric(summary(model)$coefficients[1])
  
  # Create temporary dataframe
  temp <- data.frame(Breakpoint =999, 
                     Estimated_Point = estimated_point, 
                     estimated_point_std_error = estimated_point_std_error, 
                     number_schools = length(unique(subsample$codigo_dane_sede)),
                     numer_period = length(unique(subsample$year)))
  
  temp$outcome <- outcome
  
 return(temp)
}
#####################################

SA_table = function( MODELO ) {
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(stringi)
  
  estimator = 'Sun and Abraham (2020)'
  a = etable(MODELO)
  # a$term = rownames(a)
  # 
  # a = a[  (3:(nrow(a)-8)) , ]
  # row.names(a) <- NULL
  a = drop_character_graph_tab(a)
  
  a = cbind(a , data.frame(str_split_fixed(a[[2]], " ", 2))) 
  
  a[[3]]   = gsub("\\.*$","", a[[3]]   )
  
  #a[[3]] =   ifelse(substr(a[[3]], 1,1) == '-', substr(a[[3]], 1,7), substr(a[[3]], 1,6) )
  a = a[,c(1,3,4)]
  colnames(a) = c('term', 'estimate', 'std.error')
  a$std.error = as.numeric( gsub(a$std.error, pattern = '[()]', replacement = '') )  
  a$estimator = estimator
  a$term = as.numeric(a$term)
  a$estimate = as.numeric(a$estimate)
  a$std.error = as.numeric(a$std.error)
  a = a[,c('estimator','term','estimate','std.error')]
  a = subset(a, a$term <= 5 )
  a = subset(a, a$term >= -5 )
  return(a)
}

drop_character_graph_tab = function(tabla){
  tabla[] <- lapply(tabla, gsub, pattern='time_to_treat::', replacement='')
  tabla[] <- lapply(tabla, gsub, pattern=':treat', replacement='')
  tabla[] <- lapply(tabla, gsub, pattern='year::', replacement='')
  tabla[] <- lapply(tabla, gsub, pattern='treat x time_to_treat =', replacement='')
  tabla[] <- lapply(tabla, gsub, pattern='year = ', replacement='')
  tabla[] <- lapply(tabla, gsub, pattern='[*]', replacement='')
  row.names(tabla) <- NULL
  
  return(tabla)
}


event_study_plot = function (out, seperate = TRUE, horizon = NULL, TITULO= '',ref_p=0) {
  library(ggplot2)
  library("scales")
  
  mynamestheme <- ggplot2::theme(
    plot.title = element_text(family = "Helvetica", face = "bold", size = (15), hjust = 0.5, vjust = 0.5),
    legend.title = element_text(colour = "steelblue", face = "bold.italic", family = "Helvetica"),
    legend.text = element_text(face = "italic", colour = "steelblue4", family = "Helvetica"),
    axis.title = element_text(family = "Helvetica", face = "bold", size = (12), colour = "steelblue4"),
    axis.text = element_text(family = "Courier", face = "bold", colour = "cornflowerblue", size = (12)),
    legend.position = "bottom"
 
  )
  estimators = unique(out$estimator)
  levels = c("TWFE", "Borusyak, Jaravel, Spiess (2021)", "Callaway and Sant'Anna (2020)", 
             "Gardner (2021)", "Roth and Sant'Anna (2021)", "Sun and Abraham (2020)",
             "Score at 1000 Meters" ,"Score at 1500 Meters" ,"Score at 2000 Meters",
             "Score at 2500 Meters" ,"Score at 3000 Meters" ,"Score at 3500 Meters",
             "Score at 4000 Meters" ,"Score at 4500 Meters", 'Private schools' , 
             'Public schools' ,  'All sample schools' 
  )
  
  levels = levels[levels %in% estimators]
  out$estimator = factor(out$estimator, levels = levels)
  
  color_scale = c(TWFE = "#374E55", `Gardner (2021)` = "#DF8F44", 
                  `Callaway and Sant'Anna (2020)` = "#00A1D5", `Sun and Abraham (2020)` = "#B24745",
                  `Roth and Sant'Anna (2021)` = "#79AF97", `Borusyak, Jaravel, Spiess (2021)` = "#6A6599",
                  `Score at 1000 Meters`  = "#374E55" , `Score at 1500 Meters` = "#DF8F44" ,`Score at 2000 Meters`  = "#00A1D5",
                  `Score at 2500 Meters`= "#B24745" , `Score at 3000 Meters` = "#79AF97",`Score at 3500 Meters` = "#6A6599",
                  `Score at 4000 Meters` = '#ED8975' , `Score at 4500 Meters` = '#EAAC8B' , 
                  `Private schools` = "#374E55", `Public schools` = "#DF8F44", 
                  `All sample schools` = "#00A1D5"   )
  
  color_scale = color_scale[names(color_scale) %in% estimators]
  out$ci_lower = out$estimate - 1.96 * out$std.error
  out$ci_upper = out$estimate + 1.96 * out$std.error
  if (seperate){ 
    position = "identity"}
  else {position = ggplot2::position_dodge(width = 0.5)}
  if (!is.null(horizon)) {
    out = out[out$term >= horizon[1] & out$term <= horizon[2], 
    ] 
  }
  y_lims = c(min(out$ci_lower), max(out$ci_upper)) * 1.05
  x_lims = c(min(out$term) - 0.5, max(out$term) + 0.5)
  Plot = ggplot2::ggplot(data = out, ggplot2::aes(x = .data$term, 
                                                  y = .data$estimate, color = .data$estimator,
                                                  ymin = .data$ci_lower , 
                                                  ymax = .data$ci_upper)) + {
                                                    if (seperate) 
                                                      ggplot2::facet_wrap(~estimator, scales = "free")
                                                  } +  theme_light()  +
    ggplot2::geom_point(position = position, size = 1.8) + 
    ggplot2::geom_errorbar(position = position, width = 0.01) + 
    ggplot2::geom_vline(xintercept = ref_p, linetype = "dashed") + 
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") + 
    ggplot2::ggtitle(TITULO)+ ggplot2::scale_x_continuous(limits = x_lims, breaks = c(  (min(out$term)  ) :  (max(out$term)   ) ) ) +
    # ggplot2::geom_tile(colour="white" ) +
    ggplot2::theme(  plot.title = element_text(hjust = 0.5, vjust = 0.5),
                     axis.line.x = element_line(color="steelblue4", size = 0.5),
                     axis.line.y = element_line(color="steelblue4", size = 0.5)) +
    
    ggplot2::labs(y = "Point Estimate and 95% Confidence Interval", 
                  x = "Event Time", color = "Estimator") + {
                    if (seperate) 
                      ggplot2::scale_y_continuous(limits = y_lims)
                  } + {
                    if (seperate) 
                      ggplot2::scale_x_continuous(limits = x_lims, breaks =  c(  (min(out$term)  ) :  (max(out$term)   ) ) )
                  }  + ggplot2::scale_color_manual(values = color_scale) 
  P =  Plot +
    theme_w_did( )     + mynamestheme  
  return(P  )
}

theme_w_did <- function (base_size = 16, base_family = "", base_line_size = base_size/22, 
                         base_rect_size = base_size/22) {
  half_line <- base_size/2
  theme_grey(base_size = base_size, base_family = base_family, 
             base_line_size = base_line_size, base_rect_size = base_rect_size) %+replace% 
    theme(panel.background = element_rect(fill = "white", 
                                          colour = NA), panel.border = element_rect(fill = NA, 
                                                                                    colour = "white", size = rel(1)), panel.grid = element_line(colour = "grey87"), 
          panel.grid.major = element_line(size = rel(0.5)), 
          panel.grid.minor = element_line(size = rel(0.25)), 
          axis.ticks = element_line(colour = "grey70", size = rel(0.5)), 
          legend.key = element_rect(fill = "white", colour = NA), 
          strip.background = element_rect(fill = "white", 
                                          colour = NA), strip.text = element_text(colour = "steelblue4", 
                                                                                  size = rel(0.8), margin = margin(0.8 * half_line, 
                                                                                                                   0.8 * half_line, 0.8 * half_line, 0.8 * half_line)), 
          complete = TRUE)
}


#########################################

