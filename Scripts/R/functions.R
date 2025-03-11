library(pretrends)
# library(here)
library(dplyr)
library(did)
library(haven)
library(ggplot2)
library(fixest)
library(HonestDiD)
?HonestDiD
options(scipen=999)

plot_ev_CS <- function(result_cs, field_study=''){
  plot_data <- data.frame(
    time = result_cs[["dynamic"]]$egt,
    estimate = result_cs[["dynamic"]]$att.egt,
    ci.lb = result_cs[["dynamic"]]$att.egt - 1.9 * result_cs[["dynamic"]]$se.egt,
    ci.ub = result_cs[["dynamic"]]$att.egt + 1.9 * result_cs[["dynamic"]]$se.egt
  )
  
  plot = ggplot(plot_data, aes(x = time, y = estimate, ymin = ci.lb, ymax = ci.ub)) +
    geom_hline(yintercept = 0, color = "gray", linetype = "dashed") +
    geom_vline(xintercept = -1, color = "gray", linetype = "dashed") +
    geom_pointrange(aes(color = time >= -1), size = 0.2) +  # Color based on time
    # geom_line(aes(color = time >= 0)) + # Color based on time
    labs(x = "Time Relative to Transition", 
         y = "Average Treatment Effect (ATT)",
         title = paste0("Callaway & Sant'Anna (2021)\n", field_study) ) +
    scale_color_manual(values = c("#FF8080", "#59EEFC"), guide = "none") +  # Set colors
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5), 
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 14)) +
    scale_x_continuous(breaks = unique(plot_data$time))
  print(plot)
}
# Funciones separadas para cada área STEM

detectar_ciencia <- function(career_name) {
  career_name <- tolower(career_name)
  ciencia_terms <- "(?i)biologia|quimica|fisica|ciencias|oceanografia|astronomia|ambiental|forestal|geologia"
  return(str_detect(career_name, ciencia_terms))
}

detectar_tecnologia <- function(career_name) {
  career_name <- tolower(career_name)
  tecnologia_terms <- "(?i)tecnologia|informatica|electronica|telematica|mecatronica|sistemas|computaciona|computer"  # Incluí computación aquí
  return(str_detect(career_name, tecnologia_terms))
}

detectar_ingenieria <- function(career_name) {
  career_name <- tolower(career_name)
  ingenieria_terms <- "(?i)ingenieria|engineering|arquitectura|minas|metalurgia"
  return(str_detect(career_name, ingenieria_terms))
}

detectar_matematicas <- function(career_name) {
  career_name <- tolower(career_name)
  matematicas_terms <- "(?i)matematicas|matematica|estadistica|logistica"
  return(str_detect(career_name, matematicas_terms))
}

# Define your function
detector_engineering <- function(career_name) {
  career_name <- tolower(career_name)
  stem_terms <-  "(?i)ingenier\\w|engineering"
  is_stem <- str_detect(career_name, stem_terms)
  result <- ifelse(is_stem , 1, 0) 
  return(result)
}
detector_engineering <- function(career_name) {
  career_name <- tolower(career_name)
  stem_terms <-  "(?i)ingenier\\w|engineering"
  is_stem <- str_detect(career_name, stem_terms)
  result <- ifelse(is_stem , 1, 0) 
  return(result)
}
detectar_stem <- function(career_name) {
  career_name <- tolower(career_name)
  stem_terms <-  "(?i)ingenieria|engineering|computaciona|biologia|computer|matematicas|fisica|quimica|biologia|informatica|electronica|telematica|mecatronica|ciencias|tecnologia|arquitectura|oceanografia|astronomia|ambiental|forestal|minas|metalurgia|geologia|matematica|estadistica|logistica|sistemas"
  is_stem <- str_detect(career_name, stem_terms)
  # Create another logical vector to detect non-STEM terms (not_stem)  
  non_stem_terms <- "(?i)derech\\w|juridic\\w"
  not_stem <- str_detect(career_name, non_stem_terms)
  # ifelse(is.na(career_name) | nchar(career_name) <= 1, NA,
  #        ifelse(str_detect(tolower(career_name),
  #                          "(?i)ingenieria|engineering|computaciona|biologia|computer|matematicas|fisica|quimica|biologia|informatica|electronica|telematica|mecatronica|ciencias|tecnologia|arquitectura|oceanografia|astronomia|ambiental|forestal|minas|metalurgia|geologia|matematica|estadistica|logistica|sistemas"),
  #               1, 0))
  # Combine the conditions: mark as 1 if STEM-related and not "licenciatura"
  result <- ifelse(is_stem & !not_stem, 1, 0) #ifelse(is.na(career_name) | nchar(career_name) <= 1, NA, ifelse(is_stem & !has_licenciatura, 1, 0))
  
  return(result)
}
# 
# detectar_stem <- function(career_name) {
#   # Convert to lowercase for consistent matching
#   career_name <- tolower(career_name)
#   
#   # Create a logical vector for detecting STEM-related terms
#   # is_stem <- str_detect(career_name, "(?i)ingenieria|engineering|mecanica|informatico|computaciona|biologia|datos|computer|matematicas|fisica|quimica|informatica|electronica|telematica|mecatronica|ciencias|tecnologia|arquitectura|oceanografia|astronomia|ambiental|forestal|minas|metalurgia|geologia|matematica|estadistica|logistica|sistemas")
#   stem_terms <- "(?i)ingenieria|engineering|mecanica|marina|transformacion de productos|nuevas tecn|softw|ingen|civil|instrumentacion|matematicas|informatico|computaciona|biologia|datos|computer|matematicas|fisica|quimica|informatica|electronica|telematica|mecatronica|ciencias|tecnologia|arquitectura|oceanografia|astronomia|ambiental|forestal|minas|metalurgia|geologia|matematica|estadistica|logistica|sistemas|agro\\w+|bio\\w+|quimic\\w+|multimedia|software|\\w+matica" # Added more terms and word variations with \w+
#   is_stem <- str_detect(career_name, stem_terms)
#   
#   # Create another logical vector to detect non-STEM terms (not_stem)  
#   non_stem_terms <- "(?i)deportiv|transporte|mercadeo|deporte|cultura|publicita|comercio|administracion|culinarias|derecho|juridic|archivist|trabajo|cocina|actuacion|contabilidad|ventas|hoteler\\w+|turis\\w+|moda|negocios|calidad|gestion|criminalistic\\w+|portuari\\w+|idiomas|cartografia|grafico|dental|contable|electricidad|construccion|animacion 3d|militar|produccion|television|judicial|evento\\w+|gerencial|prehospitalari\\w+|proyect\\w+|internacional\\w+|sommelier|periodism|laborator\\w+|televisi\\w+|mercadotenia|publicidad|social|alta cocina|bibliotecari\\w+|ganader\\w+|metalmecanica|agropecuari\\w+|moviliar\\w+|joyer\\w+|laboral|metalicos|maquinas herramienta\\w+|geotecnia|seguridad aeroportuari\\w+|acuicola|nutricion|terapeuti\\w+|gastronom\\w+|mercantes|levantamiento\\w+|agrimensur\\w+|labor\\w+|costos|auditoria|promocion|empresarial|maestro|bananer\\w+|bovina|aeroportuari\\w+|produccion pecuaria|logistica militar|turismo sostenible|asistencia|geografica|maquinaria y herramientas|recursos humanos|contadur\\w+|administrativa\\w+|audiovisual\\w+|horticultura|procesamiento de alimentos|abastecimiento\\w+|joveria|recreativ\\w+|recreaci\\w+|salud ocupacional|agroforester\\w+|navier\\w+|o\\w+ales" # Significantly expanded this list
#   not_stem <- str_detect(career_name, non_stem_terms)
#   
#    
#   # Combine the conditions: mark as 1 if STEM-related and not "licenciatura"
#   result <- ifelse(is_stem & !not_stem, 1, 0) #ifelse(is.na(career_name) | nchar(career_name) <= 1, NA, ifelse(is_stem & !has_licenciatura, 1, 0))
#   
#   return(result)
# }
# Function to generate the descriptive stats table
create_desc_stats_table_single_sex <- function(data, vars ) {
 
  
  # Function to calculate mean and sd for a variable
  calc_mean_sd <- function(var) {
    data %>% 
      summarize(
        Mean = mean(!!sym(var), na.rm = TRUE),
        SD = sd(!!sym(var), na.rm = TRUE)
      ) %>%
      mutate(Mean_SD = paste0(round(Mean, 3), " (", round(SD, 3), ")")) %>%
      select( Mean_SD)
  }
  
  # Create the LaTeX table
  latex_table <- "\\begin{table}[H]\n\\centering\n\\caption{Descriptive Statistics}\n\\label{tab:desc_stats}\n\\begin{threeparttable}\n"
  latex_table <- paste0(latex_table, "\\begin{tabular}{lcccc}\n\\toprule\n& \\multicolumn{4}{c}{\\textbf{Proportion of Male Students}} \\\\\n\\cmidrule(lr){2-5}\n")
  
  for (var in vars) {
    # Get the mean and sd for each variable
    stats <- calc_mean_sd(var)
    
    # Add descriptive variable names
    var_label <- get_var_label(var)
    
    # Add variable name and its stats to the LaTeX table
    latex_table <- paste0(latex_table, var_label, " & ",
                          paste(stats$Mean_SD, collapse = " & "), " \\\\\n")
  }
  
  latex_table <- paste0(latex_table, "\\bottomrule\n\\end{tabular}\n\\end{threeparttable}\n\\end{table}")
  
  return(latex_table)
}

# Function to generate the descriptive stats table
create_desc_stats_table <- function(data, vars, proportion_var = "MALE_PROPORTION") {
  # Define the quartiles
  data$proportion_var = data[[proportion_var]]
  data <- data %>%
    mutate(Quartile = case_when(
      MALE_PROPORTION < 36.36  ~ '1st Qu.',
      MALE_PROPORTION >= 36.36 & MALE_PROPORTION < 47.06 ~ '2nd Qu.',
      MALE_PROPORTION >= 47.06 & MALE_PROPORTION < 57.58 ~ '3rd Qu.',
      TRUE ~ '4th Qu.' 
    ))
  
  # Function to calculate mean and sd for a variable
  calc_mean_sd <- function(var) {
    data %>%
      group_by(Quartile) %>%
      summarize(
        Mean = mean(!!sym(var), na.rm = TRUE),
        SD = sd(!!sym(var), na.rm = TRUE)
      ) %>%
      mutate(Mean_SD = paste0(round(Mean, 3), " (", round(SD, 3), ")")) %>%
      select(Quartile, Mean_SD)
  }
  
  # Create the LaTeX table
  latex_table <- "\\begin{table}[H]\n\\centering\n\\caption{Descriptive Statistics}\n\\label{tab:desc_stats}\n\\begin{threeparttable}\n"
  latex_table <- paste0(latex_table, "\\begin{tabular}{lcccc}\n\\toprule\n& \\multicolumn{4}{c}{\\textbf{Proportion of Male Students}} \\\\\n\\cmidrule(lr){2-5}\n")
  
  for (var in vars) {
    # Get the mean and sd for each variable
    stats <- calc_mean_sd(var)
    
    # Add descriptive variable names
    var_label <- get_var_label(var)
    
    # Add variable name and its stats to the LaTeX table
    latex_table <- paste0(latex_table, var_label, " & ",
                          paste(stats$Mean_SD, collapse = " & "), " \\\\\n")
  }
  
  latex_table <- paste0(latex_table, "\\bottomrule\n\\end{tabular}\n\\end{threeparttable}\n\\end{table}")
  
  return(latex_table)
}

  
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
    'Medicine',
    'STEM',
    'No STEM'
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
    'MEDICINE',
    'STEM',
    'NO_STEM'
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
      tryCatch({
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
      }, error = function(e) {
        cat("Error occurred in iteration", i, ": ", conditionMessage(e), "\n")
      })
    }
      
    }
 
  bootstrap_results = na.omit(bootstrap_results)
  variance <- var(bootstrap_results$mean_bce)
  mean_ <- mean(bootstrap_results$mean_bce )
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
             "Gardner (2021)", "Roth and Sant'Anna (2021)", "Sun and Abraham (2021)",
             "Score at 1000 Meters" ,"Score at 1500 Meters" ,"Score at 2000 Meters",
             "Score at 2500 Meters" ,"Score at 3000 Meters" ,"Score at 3500 Meters",
             "Score at 4000 Meters" ,"Score at 4500 Meters", 'Private schools' , 
             'Public schools' ,  'All sample schools' 
  )
  
  levels = levels[levels %in% estimators]
  out$estimator = factor(out$estimator, levels = levels)
  
  color_scale = c(TWFE = "#374E55", `Gardner (2021)` = "#DF8F44", 
                  `Callaway and Sant'Anna (2020)` = "#00A1D5", `Sun and Abraham (2021)` = "#B24745",
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

# Function to create a mapping for more descriptive variable names
get_var_label <- function(var) {
  labels <- list(
    "TOT_MALE" = "Total Male Students (N)",
    "TOT_FEMALE" = "Total Female Students (N)",
    "TOT_STU" = "Total Students (N)",
    "AVG_AGE" = "Average Age (Years)",
    "TOT_STEM" = "Students in STEM Fields (\\%)",
    "TOT_NO_STEM" = "Students in Non-STEM Fields (\\%)",
    "TOT_ECONOMICS_BUSINESS_RELATED" = "Students in Economics/Business (\\%)",
    "TOT_ENG_ARCH_RELATED" = "Students in Engineering/Architecture (\\%)",
    "TOT_FINE_ARTS" = "Students in Fine Arts (\\%)",
    "TOT_MATHEMATICS_NATURAL_SCIENCES" = "Students in Mathematics/Natural Sciences (\\%)",
    "TOT_SOCIAL_SCIENCES_HUMANITIES" = "Students in Social Sciences/Humanities (\\%)",
    "TOT_AGRONOMY_VETERINARY_RELATED" = "Students in Agronomy/Veterinary (\\%)",
    "TOT_EDUCATION_SCIENCES" = "Students in Education Sciences (\\%)",
    "TOT_HEALTH_SCIENCES" = "Students in Health Sciences (\\%)",
    "TOT_MEDICINE" = "Students in Medicine (\\%)",
    "TOT_LAW" = "Students in Law (\\%) ",
    "TOT_NO_STUDIES" = "Stu. Not Continuing Education (\\%)",  
    "STEM" = "Students in STEM Fields (%)",
    "NO_STEM" = "Students in Non-STEM Fields (%)",
    "ECONOMICS_BUSINESS_RELATED" = "Students in Economics/Business (%)",
    "ENG_ARCH_RELATED" = "Students in Engineering/Architecture (%)",
    "FINE_ARTS" = "Students in Fine Arts (%)",
    "MATHEMATICS_NATURAL_SCIENCES" = "Students in Mathematics/Natural Sciences (%)",
    "SOCIAL_SCIENCES_HUMANITIES" = "Students in Social Sciences/Humanities (%)",
    "AGRONOMY_VETERINARY_RELATED" = "Students in Agronomy/Veterinary (%)",
    "EDUCATION_SCIENCES" = "Students in Education Sciences (%)",
    "HEALTH_SCIENCES" = "Students in Health Sciences (%)",
    "MEDICINE" = "Students in Medicine (%)",
    "LAW" = "Students in Law (%) ",
    "NO_STUDIES" = "Stu. Not Continuing Education (%)",  
    "MALE_PROPORTION" = "Proportion of Male Students (%)",
    # Additional variables
    "Failed_Last_year" = "Students Failed Last Year (\\%)",
    "Passed_Last_year" = "Students Passed Last Year (\\%)",
    "Retired_Last_year" = "Students Retired Last Year (\\%)",
    "zon_alu_rural" = "Students from Rural Area (\\%)",
    "zon_alu_urban" = "Students from Urban Area (\\%)",
    "ESTRATO_6" = "Students in Socioeconomic Stratum 6 (\\%)",
    "ESTRATO_5" = "Students in Socioeconomic Stratum 5 (\\%)",
    "ESTRATO_4" = "Students in Socioeconomic Stratum 4 (\\%)",
    "ESTRATO_3" = "Students in Socioeconomic Stratum 3 (\\%)",
    "ESTRATO_2" = "Students in Socioeconomic Stratum 2 (\\%)",
    "ESTRATO_1" = "Students in Socioeconomic Stratum 1 (\\%)"
  )
  
  return(labels[[var]])
}

# Function to generate the descriptive stats table
create_desc_stats_table <- function(data, vars, proportion_var = "MALE_PROPORTION") {
  # Define the quartiles
  data$proportion_var = data[[proportion_var]]
  data <- data %>%
    mutate(Quartile = case_when(
      MALE_PROPORTION < 36.36  ~ '1st Qu.',
      MALE_PROPORTION >= 36.36 & MALE_PROPORTION < 47.06 ~ '2nd Qu.',
      MALE_PROPORTION >= 47.06 & MALE_PROPORTION < 57.58 ~ '3rd Qu.',
      TRUE ~ '4th Qu.' 
    ))
  
  # Function to calculate mean and sd for a variable
  calc_mean_sd <- function(var) {
    data %>%
      group_by(Quartile) %>%
      summarize(
        Mean = mean(!!sym(var), na.rm = TRUE),
        SD = sd(!!sym(var), na.rm = TRUE)
      ) %>%
      mutate(Mean_SD = paste0(round(Mean, 3), " (", round(SD, 3), ")")) %>%
      select(Quartile, Mean_SD)
  }
  
  # Create the LaTeX table
  latex_table <- "\\begin{table}[H]\n\\centering\n\\caption{Descriptive Statistics}\n\\label{tab:desc_stats}\n\\begin{threeparttable}\n"
  latex_table <- paste0(latex_table, "\\begin{tabular}{lcccc}\n\\toprule\n& \\multicolumn{4}{c}{\\textbf{Proportion of Male Students}} \\\\\n\\cmidrule(lr){2-5}\n")
  
  for (var in vars) {
    # Get the mean and sd for each variable
    stats <- calc_mean_sd(var)
    
    # Add descriptive variable names
    var_label <- get_var_label(var)
    
    # Add variable name and its stats to the LaTeX table
    latex_table <- paste0(latex_table, var_label, " & ",
                          paste(stats$Mean_SD, collapse = " & "), " \\\\\n")
  }
  
  latex_table <- paste0(latex_table, "\\bottomrule\n\\end{tabular}\n\\end{threeparttable}\n\\end{table}")
  
  return(latex_table)
}

save_plot_png <- function(plot, plot_name ){
  png(paste0(graphs_dir,plot_name,'.png'),  width = 1030, height = 598)
  print(plot)
  dev.off() 
}


mean_sd_table = function(calculo_var){
  return(paste0( round(mean(calculo_var, na.rm = TRUE), 3), " (", round(sd(calculo_var, na.rm = TRUE), 3) ,")" ))
}
# Function to calculate summary statistics and proportions
summarize_data <- function(data) {
  data %>%
    summarize( 
      `Female Students (N)` = mean_sd_table(TOT_FEMALE), # paste0( round(mean(TOT_FEMALE, na.rm = TRUE), 3), "(", ,")" ) ,
      `Total students (N)` =mean_sd_table(TOT_STU), # mean(TOT_STU, na.rm = TRUE),
      `Average Age (Years)` = mean_sd_table(AVG_AGE), #  mean(AVG_AGE, na.rm = TRUE),
      `Non-STEM Fields` = mean_sd_table(TOT_NO_STEM / (TOT_STEM + TOT_NO_STEM+ TOT_NO_STUDIES )), #  mean(TOT_NO_STEM / (TOT_STEM + TOT_NO_STEM ), na.rm = TRUE) ,
      `STEM Fields` = mean_sd_table(TOT_STEM / (TOT_STEM + TOT_NO_STEM + TOT_NO_STUDIES)), # mean(TOT_STEM / (TOT_STEM + TOT_NO_STEM ), na.rm = TRUE) ,
      `Health Sciences` =mean_sd_table(TOT_HEALTH_SCIENCES / (TOT_STEM + TOT_NO_STEM )), #mean(TOT_HEALTH_SCIENCES / (TOT_STEM + TOT_NO_STEM ), na.rm = TRUE) ,
      `Social Sciences/Humanities` = mean_sd_table(TOT_SOCIAL_SCIENCES_HUMANITIES /  (TOT_STEM + TOT_NO_STEM ) ), #mean(TOT_SOCIAL_SCIENCES_HUMANITIES / (TOT_STEM + TOT_NO_STEM ), na.rm = TRUE) ,
      `Law` = mean_sd_table(TOT_LAW/ TOT_STU), #mean(TOT_LAW/ TOT_STU, na.rm = TRUE) ,
      `Education Sciences` = mean_sd_table(TOT_EDUCATION_SCIENCES/ TOT_STU), #mean(TOT_EDUCATION_SCIENCES/ TOT_STU, na.rm = TRUE) ,
      `Economics/Business` = mean_sd_table(TOT_ECONOMICS_BUSINESS_RELATED/ TOT_STU), #mean(TOT_ECONOMICS_BUSINESS_RELATED/ TOT_STU, na.rm = TRUE) ,
      `Engineering/Architecture` = mean_sd_table(TOT_ENG_ARCH_RELATED/ TOT_STU), #mean(TOT_ENG_ARCH_RELATED/ TOT_STU, na.rm = TRUE) ,
      `Fine Arts` = mean_sd_table(TOT_FINE_ARTS/ TOT_STU), #mean(TOT_FINE_ARTS/ TOT_STU, na.rm = TRUE) ,
      `Mathematics/Natural Sciences` = mean_sd_table(TOT_MATHEMATICS_NATURAL_SCIENCES/ TOT_STU), # mean(TOT_MATHEMATICS_NATURAL_SCIENCES/ TOT_STU, na.rm = TRUE) ,
      `Agronomy/Veterinary` = mean_sd_table(TOT_AGRONOMY_VETERINARY_RELATED/ TOT_STU), # mean(TOT_AGRONOMY_VETERINARY_RELATED/ TOT_STU, na.rm = TRUE) ,
      `Not Continuing Education` = mean_sd_table(TOT_NO_STUDIES/  (TOT_STEM + TOT_NO_STEM + TOT_NO_STUDIES) ), # mean(TOT_NO_STUDIES/ TOT_STU, na.rm = TRUE) ,
      `Socioeconomic Strata 1`=mean_sd_table(ESTRATO_1), # mean( ESTRATO_1, na.rm = TRUE),
      `Socioeconomic Strata 2`=mean_sd_table(ESTRATO_2), #mean( ESTRATO_2, na.rm = TRUE),
      `Socioeconomic Strata 3`=mean_sd_table(ESTRATO_3), #mean( ESTRATO_3, na.rm = TRUE),
      `Socioeconomic Strata 4`=mean_sd_table(ESTRATO_4), #mean( ESTRATO_4, na.rm = TRUE),
      `Socioeconomic Strata 5`=mean_sd_table(ESTRATO_5), #mean( ESTRATO_5, na.rm = TRUE),
      `Socioeconomic Strata 6`=mean_sd_table(ESTRATO_6), #mean( ESTRATO_6, na.rm = TRUE),
      
      `Low Socioeconomic Strata`=mean_sd_table(LOW_STRATA), #mean( LOW_STRATA, na.rm = TRUE),
      `Middle Socioeconomic Strata`=mean_sd_table(MID_STRATA), #mean( MID_STRATA, na.rm = TRUE),
      `High Socioeconomic Strata`=mean_sd_table(HIG_STRATA), #mean( HIG_STRATA, na.rm = TRUE),
      
      `Pass Rate for Last Year`= mean_sd_table(Passed_Last_year / TOT_STU_se), #mean( Passed_Last_year / TOT_STU_se, na.rm = TRUE),
      
      `Failure Rate for Last Year`=mean_sd_table(Failed_Last_year / TOT_STU_se), #mean( Failed_Last_year / TOT_STU_se, na.rm = TRUE)
      
    )
}


summarize_data_if_exist <- function(data) {
  data %>%
    summarize(
      `Female Students (N)` = if("TOT_FEMALE" %in% colnames(data)) mean_sd_table(TOT_FEMALE) else NA,
      `Total students (N)` = if("TOT_STU" %in% colnames(data)) mean_sd_table(TOT_STU) else NA,
      `Average Age (Years)` = if("AVG_AGE" %in% colnames(data)) mean_sd_table(AVG_AGE) else NA,
      `Non-STEM Fields` = if(all(c("TOT_NO_STEM", "TOT_STEM", "TOT_NO_STUDIES") %in% colnames(data))) mean_sd_table(TOT_NO_STEM / (TOT_STEM + TOT_NO_STEM + TOT_NO_STUDIES)) else NA,
      `STEM Fields` = if(all(c("TOT_STEM", "TOT_NO_STEM", "TOT_NO_STUDIES") %in% colnames(data))) mean_sd_table(TOT_STEM / (TOT_STEM + TOT_NO_STEM + TOT_NO_STUDIES)) else NA,
      `Health Sciences` = if(all(c("TOT_HEALTH_SCIENCES", "TOT_STEM", "TOT_NO_STEM") %in% colnames(data))) mean_sd_table(TOT_HEALTH_SCIENCES / (TOT_STEM + TOT_NO_STEM)) else NA,
      `Social Sciences/Humanities` = if(all(c("TOT_SOCIAL_SCIENCES_HUMANITIES", "TOT_STEM", "TOT_NO_STEM") %in% colnames(data))) mean_sd_table(TOT_SOCIAL_SCIENCES_HUMANITIES / (TOT_STEM + TOT_NO_STEM)) else NA,
      `Law` = if("TOT_LAW" %in% colnames(data)) mean_sd_table(TOT_LAW / TOT_STU) else NA,
      `Education Sciences` = if("TOT_EDUCATION_SCIENCES" %in% colnames(data)) mean_sd_table(TOT_EDUCATION_SCIENCES / TOT_STU) else NA,
      `Economics/Business` = if("TOT_ECONOMICS_BUSINESS_RELATED" %in% colnames(data)) mean_sd_table(TOT_ECONOMICS_BUSINESS_RELATED / TOT_STU) else NA,
      `Engineering/Architecture` = if("TOT_ENG_ARCH_RELATED" %in% colnames(data)) mean_sd_table(TOT_ENG_ARCH_RELATED / TOT_STU) else NA,
      `Fine Arts` = if("TOT_FINE_ARTS" %in% colnames(data)) mean_sd_table(TOT_FINE_ARTS / TOT_STU) else NA,
      `Mathematics/Natural Sciences` = if("TOT_MATHEMATICS_NATURAL_SCIENCES" %in% colnames(data)) mean_sd_table(TOT_MATHEMATICS_NATURAL_SCIENCES / TOT_STU) else NA,
      `Agronomy/Veterinary` = if("TOT_AGRONOMY_VETERINARY_RELATED" %in% colnames(data)) mean_sd_table(TOT_AGRONOMY_VETERINARY_RELATED / TOT_STU) else NA,
      `Not Continuing Education` = if("TOT_NO_STUDIES" %in% colnames(data)) mean_sd_table(TOT_NO_STUDIES / (TOT_STEM + TOT_NO_STEM + TOT_NO_STUDIES)) else NA,
      `Socioeconomic Strata 1` = if("ESTRATO_1" %in% colnames(data)) mean_sd_table(ESTRATO_1) else NA,
      `Socioeconomic Strata 2` = if("ESTRATO_2" %in% colnames(data)) mean_sd_table(ESTRATO_2) else NA,
      `Socioeconomic Strata 3` = if("ESTRATO_3" %in% colnames(data)) mean_sd_table(ESTRATO_3) else NA,
      `Socioeconomic Strata 4` = if("ESTRATO_4" %in% colnames(data)) mean_sd_table(ESTRATO_4) else NA,
      `Socioeconomic Strata 5` = if("ESTRATO_5" %in% colnames(data)) mean_sd_table(ESTRATO_5) else NA,
      `Socioeconomic Strata 6` = if("ESTRATO_6" %in% colnames(data)) mean_sd_table(ESTRATO_6) else NA,
      `Low Socioeconomic Strata` = if("LOW_STRATA" %in% colnames(data)) mean_sd_table(LOW_STRATA) else NA,
      `Middle Socioeconomic Strata` = if("MID_STRATA" %in% colnames(data)) mean_sd_table(MID_STRATA) else NA,
      `High Socioeconomic Strata` = if("HIG_STRATA" %in% colnames(data)) mean_sd_table(HIG_STRATA) else NA,
      `Pass Rate for Last Year` = if(all(c("Passed_Last_year", "TOT_STU_se") %in% colnames(data))) mean_sd_table(Passed_Last_year / TOT_STU_se) else NA,
      `Failure Rate for Last Year` = if(all(c("Failed_Last_year", "TOT_STU_se") %in% colnames(data))) mean_sd_table(Failed_Last_year / TOT_STU_se) else NA
    )
}


summarize_data_staggered <- function(data) {
  data %>%
    mutate(  
      `Female Students (N)` = (TOT_FEMALE), # paste0( round(mean(TOT_FEMALE, na.rm = TRUE), 3), "(", ,")" ) ,
      `Total students (N)` =(TOT_STU), # mean(TOT_STU, na.rm = TRUE),
      `Intensity Event Time` =(TOT_MALE/TOT_STU) , 
      `Average Age (Years)` = (AVG_AGE), #  mean(AVG_AGE, na.rm = TRUE),
      `Non-STEM Fields` = (TOT_NO_STEM / (TOT_STEM + TOT_NO_STEM+ TOT_NO_STUDIES )), #  mean(TOT_NO_STEM / (TOT_STEM + TOT_NO_STEM ), na.rm = TRUE) ,
      `STEM Fields` = (TOT_STEM / (TOT_STEM + TOT_NO_STEM + TOT_NO_STUDIES)), # mean(TOT_STEM / (TOT_STEM + TOT_NO_STEM ), na.rm = TRUE) ,
      `Health Sciences` =(TOT_HEALTH_SCIENCES / (TOT_STEM + TOT_NO_STEM )), #mean(TOT_HEALTH_SCIENCES / (TOT_STEM + TOT_NO_STEM ), na.rm = TRUE) ,
      `Social Sciences/Humanities` = (TOT_SOCIAL_SCIENCES_HUMANITIES /  (TOT_STEM + TOT_NO_STEM ) ), #mean(TOT_SOCIAL_SCIENCES_HUMANITIES / (TOT_STEM + TOT_NO_STEM ), na.rm = TRUE) ,
      `Law` = (TOT_LAW/ TOT_STU), #mean(TOT_LAW/ TOT_STU, na.rm = TRUE) ,
      `Education Sciences` = (TOT_EDUCATION_SCIENCES/ TOT_STU), #mean(TOT_EDUCATION_SCIENCES/ TOT_STU, na.rm = TRUE) ,
      `Economics/Business` = (TOT_ECONOMICS_BUSINESS_RELATED/ TOT_STU), #mean(TOT_ECONOMICS_BUSINESS_RELATED/ TOT_STU, na.rm = TRUE) ,
      `Engineering/Architecture` = (TOT_ENG_ARCH_RELATED/ TOT_STU), #mean(TOT_ENG_ARCH_RELATED/ TOT_STU, na.rm = TRUE) ,
      `Fine Arts` = (TOT_FINE_ARTS/ TOT_STU), #mean(TOT_FINE_ARTS/ TOT_STU, na.rm = TRUE) ,
      `Mathematics/Natural Sciences` = (TOT_MATHEMATICS_NATURAL_SCIENCES/ TOT_STU), # mean(TOT_MATHEMATICS_NATURAL_SCIENCES/ TOT_STU, na.rm = TRUE) ,
      `Agronomy/Veterinary` = (TOT_AGRONOMY_VETERINARY_RELATED/ TOT_STU), # mean(TOT_AGRONOMY_VETERINARY_RELATED/ TOT_STU, na.rm = TRUE) ,
      `Not Continuing Education` = (TOT_NO_STUDIES/  (TOT_STEM + TOT_NO_STEM + TOT_NO_STUDIES) ), # mean(TOT_NO_STUDIES/ TOT_STU, na.rm = TRUE) ,
      `Medicine` = TOT_MEDICINE/  (TOT_STEM + TOT_NO_STEM + TOT_NO_STUDIES) ,
      `STEM - Science` = TOT_SCIENCE /  (TOT_STEM + TOT_NO_STEM + TOT_NO_STUDIES) ,
      `STEM - Technology` = TOT_TECHNOLOGY /  (TOT_STEM + TOT_NO_STEM + TOT_NO_STUDIES) ,
      `STEM - Engineering` = TOT_ENGINEERING/  (TOT_STEM + TOT_NO_STEM + TOT_NO_STUDIES) ,
      `STEM - Mathematics` = TOT_MATHEMATICS/  (TOT_STEM + TOT_NO_STEM + TOT_NO_STUDIES) , 
      
      `Enrollment in University` = TOT_UNIVERSITY /  (TOT_STEM + TOT_NO_STEM + TOT_NO_STUDIES) , 
      `Enrollment in Technical` = TOT_TECHNICAL /  (TOT_STEM + TOT_NO_STEM + TOT_NO_STUDIES) , 
      `Enrollment in Technological` = TOT_TECHNOLOGY /  (TOT_STEM + TOT_NO_STEM + TOT_NO_STUDIES) , 
      
      `Socioeconomic Strata 1`=(ESTRATO_1), # mean( ESTRATO_1, na.rm = TRUE),
      `Socioeconomic Strata 2`=(ESTRATO_2), #mean( ESTRATO_2, na.rm = TRUE),
      `Socioeconomic Strata 3`=(ESTRATO_3), #mean( ESTRATO_3, na.rm = TRUE),
      `Socioeconomic Strata 4`=(ESTRATO_4), #mean( ESTRATO_4, na.rm = TRUE),
      `Socioeconomic Strata 5`=(ESTRATO_5), #mean( ESTRATO_5, na.rm = TRUE),
      `Socioeconomic Strata 6`=(ESTRATO_6), #mean( ESTRATO_6, na.rm = TRUE),
      
      `Low Socioeconomic Strata`=(LOW_STRATA), #mean( LOW_STRATA, na.rm = TRUE),
      `Middle Socioeconomic Strata`=(MID_STRATA), #mean( MID_STRATA, na.rm = TRUE),
      `High Socioeconomic Strata`=(HIG_STRATA), #mean( HIG_STRATA, na.rm = TRUE),
      
      `Pass Rate for Last Year`= (Passed_Last_year / TOT_STU_se), #mean( Passed_Last_year / TOT_STU_se, na.rm = TRUE),
      
      `Failure Rate for Last Year`=(Failed_Last_year / TOT_STU_se), #mean( Failed_Last_year / TOT_STU_se, na.rm = TRUE)
      
    )
}



####################### Pre Trends  fundtions
honest_did <- function(...) UseMethod("honest_did")

honest_did.AGGTEobj <- function(es,
                                e          = 0,
                                type       = c("smoothness", "relative_magnitude"),
                                gridPoints = 100,
                                ...) {
  
  type <- match.arg(type)
  
  # Make sure that user is passing in an event study
  if (es$type != "dynamic") {
    stop("need to pass in an event study")
  }
  
  # Check if used universal base period and warn otherwise
  if (es$DIDparams$base_period != "universal") {
    stop("Use a universal base period for honest_did")
  }
  
  # Recover influence function for event study estimates
  es_inf_func <- es$inf.function$dynamic.inf.func.e
  
  # Recover variance-covariance matrix
  n <- nrow(es_inf_func)
  V <- t(es_inf_func) %*% es_inf_func / n / n
  
  # Check time vector is consecutive with referencePeriod = -1
  referencePeriod <- -1
  consecutivePre  <- !all(diff(es$egt[es$egt <= referencePeriod]) == 1)
  consecutivePost <- !all(diff(es$egt[es$egt >= referencePeriod]) == 1)
  if ( consecutivePre | consecutivePost ) {
    msg <- "honest_did expects a time vector with consecutive time periods;"
    msg <- paste(msg, "please re-code your event study and interpret the results accordingly.", sep="\n")
    stop(msg)
  }
  
  # Remove the coefficient normalized to zero
  hasReference <- any(es$egt == referencePeriod)
  if ( hasReference ) {
    referencePeriodIndex <- which(es$egt == referencePeriod)
    V    <- V[-referencePeriodIndex,-referencePeriodIndex]
    beta <- es$att.egt[-referencePeriodIndex]
  } else {
    beta <- es$att.egt
  }
  
  nperiods <- nrow(V)
  npre     <- sum(1*(es$egt < referencePeriod))
  npost    <- nperiods - npre
  if ( !hasReference & (min(c(npost, npre)) <= 0) ) {
    if ( npost <= 0 ) {
      msg <- "not enough post-periods"
    } else {
      msg <- "not enough pre-periods"
    }
    msg <- paste0(msg, " (check your time vector; note honest_did takes -1 as the reference period)")
    stop(msg)
  }
  
  baseVec1 <- basisVector(index=(e+1),size=npost)
  orig_ci  <- constructOriginalCS(betahat        = beta,
                                  sigma          = V,
                                  numPrePeriods  = npre,
                                  numPostPeriods = npost,
                                  l_vec          = baseVec1)
  
  if (type=="relative_magnitude") {
    robust_ci <- createSensitivityResults_relativeMagnitudes(betahat        = beta,
                                                             sigma          = V,
                                                             numPrePeriods  = npre,
                                                             numPostPeriods = npost,
                                                             l_vec          = baseVec1,
                                                             gridPoints     = gridPoints,
                                                             ...)
    
  } else if (type == "smoothness") {
    robust_ci <- createSensitivityResults(betahat        = beta,
                                          sigma          = V,
                                          numPrePeriods  = npre,
                                          numPostPeriods = npost,
                                          l_vec          = baseVec1,
                                          ...)
  }
  
  return(list(robust_ci=robust_ci, orig_ci=orig_ci, type=type))
}

ATT_sim_dyn_CS = function(outcome   , data_function, never_treated = F){
  if (never_treated==F) {
    control_group = "notyettreated"
  } else{
    control_group = "nevertreated"
    data_function$transition_year = ifelse(is.na(data_function$transition_year)==T, 0,  data_function$transition_year)
    
  }
  # outcome = paste0('TOT_', outcome)
  # Estimate the Callaway & Sant'Anna staggered DiD
  data_function = data_function[data_function$rel_year<=7 | is.na(data_function$rel_year),  ]
  did_result <- att_gt(yname = outcome, # Outcome variable
                       tname = "YEAR_INFO", # Time variable
                       idname = "codigo_dane_sede", # School ID
                       gname = "transition_year", # Treatment group (year of transition)
                       control_group =  control_group,
                       data = data_function,
                       base_period = "universal",
                       # base_period = -2,
                       panel = F,
                       # xformla = ~intensity_event_time, #1, # No covariates in this example (you can add them)
                       est_method = "dr",  # "dr" for doubly robust estimation
                       bstrap = TRUE,
                       biters = 10000, # Recommended: increase for more reliable p-values
                       cband = TRUE
  ) 
  
  print(summary(did_result))
  print(ggdid(did_result))
  # ?aggte
  agg.simple <- aggte(did_result, type = "simple", na.rm = TRUE)
  # print(summary(agg.simple))
  agg.dynamic <- aggte(did_result, type = "dynamic"  , min_e = -5, max_e = 6, na.rm = TRUE ) #
  # print(summary(agg.dynamic))
  # ggdid(agg.dynamic)
  
  agg.calendar <- aggte(did_result, type = "calendar", na.rm = TRUE)
  # print(summary(agg.calendar))
  # ggdid(agg.calendar)
  
  return(list(simple = agg.simple, 
              dynamic = agg.dynamic, 
              calendar = agg.calendar, 
              conditional_pretest = did_result))
}

save_img_callaway_dynamic <- function(model, field_study){
  filename_ = gsub(gsub(field_study, pattern = "/", replace = " and or "), pattern = "\n", replace = "_")

  png(paste0(graphs_dir,'CS_',filename_,".png"),  width = 1030, height = 598)
  plot_ev_CS(model, field_study= field_study )
  dev.off() 
  return(print(plot_ev_CS(model, field_study= field_study )))
}

save_img_sunab_dynamic <- function(model, field_study){
  filename_ = gsub(gsub(field_study, pattern = "/", replace = " and or "), pattern = "\n", replace = "_")
  print(filename_)
  png(paste0(graphs_dir,'SA_',filename_,".png"),  width = 1030, height = 598)
  event_study_plot(model, TITULO = field_study)
  dev.off() 
  return(print( event_study_plot(model, TITULO = field_study) ))
}

wald_test_att <- function(model){
  p_value <- model[["conditional_pretest"]][["Wpval"]]
  
  # Create interpretation based on p-value
  trend_interpretation <- if(p_value < 0.05) {
    "This suggests a potential violation of the parallel trends assumption."
  } else {
    "This provides strong evidence that the parallel trends assumption holds."
  }
  
  model_answer <- paste0(
    model[["simple"]][["DIDparams"]][["yname"]], "\n",
    
    "The Overall ATT: ", 
    round(model[["dynamic"]][["overall.att"]], 4), 
    " (", round(model[["dynamic"]][["overall.se"]], 4),")", "\n",
    
    "The p-value of the Wald statistic for pre-testing the common trends assumption: ", 
    p_value, "\n",
    trend_interpretation 
  )
  
  cat(model_answer)
}
  

significance_stars <- function(string) {
  # Verifica si el input es un string.
  if (!is.character(string)) {
    warning("Input must be a string.")
    return(NA)
  }
  
  # Intenta extraer el estimate y el error estándar usando expresiones regulares.
  match <- regexpr("([-+]?[0-9.]+) \\(([-+]?[0-9.]+)\\)", string)
  
  # Verifica si el patrón fue encontrado. Si no, retorna NA.
  if (match == -1) {
    warning("Invalid input string format.  Expected 'estimate (standard error)'.")
    return(NA)
  }
  
  # Extrae los valores del estimate y error estándar del string.
  estimate_str <- regmatches(string, regexec("([-+]?[0-9.]+) \\(([-+]?[0-9.]+)\\)", string))[[1]][2]
  se_str <- regmatches(string, regexec("([-+]?[0-9.]+) \\(([-+]?[0-9.]+)\\)", string))[[1]][3]
  
  # Intenta convertir los valores extraídos a numérico.  Si falla, retorna NA.
  estimate <- tryCatch(as.numeric(estimate_str), warning = function(w) NA)
  se <- tryCatch(as.numeric(se_str), warning = function(w) NA)
  
  if (is.na(estimate) || is.na(se)) {
    warning("Could not convert estimate or standard error to numeric.")
    return(NA)
  }
  
  # Calcula el p-valor usando un test t de dos colas.
  t_value <- estimate / se
  p_value <- 2 * pt(abs(t_value), df = Inf, lower.tail = FALSE)  #Usamos df=Inf para que se comporte como un test Z
  
  # Asigna las estrellas basándose en el p-valor.
  stars <- case_when(
    p_value < 0.001 ~ "***",
    p_value < 0.01 ~ "**",
    p_value < 0.05 ~ "*",
    p_value < 0.1 ~ ".",
    TRUE ~ " "
  )
  
  return(stars)
}

