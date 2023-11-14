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





plot_coefficients = function (data, estimate_point = '' ,sd_error = '', x_continuous = '' , TITULO= '') {
  out = data
  
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
  x_lims = c(0, 1)
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
                  x = x_label_ , color = "Estimator" ) +  ggplot2::scale_color_manual(values = color_scale) 
  P =  Plot +   geom_smooth(method = "lm",formula = y ~ x + poly(x, 2), se = F, color = "blue") +
    mynamestheme  
  print(P  )
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
    'Health Sciences',
    'No Studies'
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
    'NO_STUDIES'
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

