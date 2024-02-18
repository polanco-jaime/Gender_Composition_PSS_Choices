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

 



plot_coefficients = function (data, estimate_point = '' ,sd_error = '', x_continuous = '' , TITULO= '', polynom = 3) {
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
                show.legend = FALSE)
  
   
  # 
  return(P)
}


plot_coefficients_smooth = function (data, estimate_point = '' ,sd_error = '', x_continuous = '' , TITULO= '', polynom = 3) {
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
                  x = x_label_ , color = "Estimator" ) + 
    ggplot2::scale_color_manual(values = color_scale) 
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
estimate_optimal_bandwidth_logit <- function(data, outcome, covariates, group_var, range_start, range_end) {
  
  # Define the logistic regression formula
  formula <- paste0("(", outcome, ") ~ ", paste(covariates, collapse = "+"), 
                    " | ", group_var)
  
  # Print the current estimation range
  print(paste0("The current estimation is from: ", range_start, ", to: ", range_end))
  
  # Fit the logistic regression model
  model <- fixest::feglm(data = data, family = 'binomial', as.formula(formula))
  
  # Implement your method to estimate the optimal bandwidth
  # Here, we assume a simple approach of choosing a default value
  # Modify this section to include your estimation method
  
  # Default bandwidth value for demonstration
  h_default <- 0.1
  
  return(h_default)
}

# Example usage
# Set your data, outcome variable, covariates, group variable, and range
# Replace these placeholders with your actual data and variables
# data <- subsample
# Define a function to estimate the optimal bandwidth based on Binary Cross-Entropy (BCE)
estimate_optimal_bandwidth <- function(Y, X, bandwidth_type = "BCE") {
  
  # Function to calculate the BCE loss
  bce_loss <- function(m, Y) {
    -mean(Y * log(m) + (1 - Y) * log(1 - m))
  }
  
  # Function to estimate the regression function m(X)
  estimate_m <- function(X, bandwidth) {
    # Implement your regression model here
    # This function should return estimated probabilities m(X) for each X_i
    # based on the bandwidth specified
    # Example: Use local polynomial regression, kernel density estimation, etc.
    return(m_estimate)
  }
  
  # Estimate the regression function m(X)
  m_estimate <- estimate_m(X, bandwidth_type)
  
  # Calculate the BCE loss
  bce <- bce_loss(m_estimate, Y)
  
  # Display the BCE loss
  cat("BCE Loss:", bce, "\n")
  
  # Implement your method to estimate the optimal bandwidth
  # Here, we assume a simple approach of choosing a default value
  # Modify this section to include your estimation method
  
  # Default bandwidth value for demonstration
  h_default <- 0.1
  
  return(h_default)
}
 

# Define a function to estimate binary cross-entropy
estimate_binary_cross_entropy <- function(data, outcome, covariates, group_var) {
  
  # Define the logistic regression formula
  formula <- paste0("(", outcome, ") ~ ", paste(covariates, collapse = "+"), 
                    " | ", group_var)
  
  # Fit the logistic regression model
  model <- fixest::feglm(data = data, family = 'binomial', as.formula(formula))
  
  # Extract predicted probabilities
  predicted_probs <- predict(model, type = "response")
  
  # Calculate binary cross-entropy
  bce <- ifelse(data[[outcome]] == 1, -log(predicted_probs), -log(1 - predicted_probs))
  
  # Compute mean binary cross-entropy
  mean_bce <- mean(bce)
  
  return(mean_bce)
}

# Example usage
# Set your data, outcome variable, covariates, and group variable
# Replace these placeholders with your actual data and variables
data <- subsample
outcome <- "NO_STUDIES" 
covariates <- c("genero", "EDAD", "tot_students_school_group")
group_var <- "fe_group"

# Estimate binary cross-entropy
binary_cross_entropy <- estimate_binary_cross_entropy(data, outcome, covariates, group_var)
cat("Estimated Binary Cross-Entropy:", binary_cross_entropy, "\n")

################
start_point = 0.001
end_point = 0.999
bw_by  =3
sequence <- seq(start_point, end_point, by = (end_point-start_point)/bw_by )
# Loop over the sequence
for (i in 1:(length(sequence)-1)) {
  # Define the lower and upper bounds for the subsample
  lower_bound <- sequence[i]
  upper_bound <- sequence[i+1]
  
  # Subset the data based on the bounds
  subsample <- data[data$frac_males_in_the_group >= lower_bound & 
                      data$frac_males_in_the_group < upper_bound, ]
  
  # Print some information about the current subsample
  cat("Subsample", i, ":", nrow(subsample), "observations\n") 
}
length( unique(subsample$codigo_dane_sede) )
# Define the logistic regression formula
formula <- paste0("(", outcome, ") ~ ", paste(covariates, collapse = "+"), 
                  " | ", group_var)

# Fit the logistic regression model
model <- fixest::feglm(data = data, family = 'binomial', as.formula(formula))

# Extract predicted probabilities
predicted_probs <- predict(model, type = "response")

# Calculate binary cross-entropy
bce <- ifelse(data[[outcome]] == 1, -log(predicted_probs), -log(1 - predicted_probs))

# Compute mean binary cross-entropy
mean_bce <- mean(bce)
mean_bce
