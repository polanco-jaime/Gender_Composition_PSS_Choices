#trash code


model_summary
################################################################
fixest_ame_gender <- function(modelo, tabla){ 
  "
  This function estimate the average marginal effect 
  "
  tabla$model_fit =  predict(modelo,tabla, type = "response" )
  
  summary()
  y.hat_F = mean( subset(tabla, tabla$genero == 'F'  & is.na(tabla$model_fit)==F )$model_fit , )
  sd.hat_F = sd( subset(tabla, tabla$genero == 'F'  & is.na(tabla$model_fit)==F )$model_fit ,na.rm = T )
  
  y.hat_M = mean( subset(tabla, tabla$genero == 'M'  & is.na(tabla$model_fit)==F )$model_fit , )
  sd.hat_M = sd( subset(tabla, tabla$genero == 'M'  & is.na(tabla$model_fit)==F )$model_fit ,na.rm = T )
  
  return(c(y.hat_F-y.hat_M , sd.hat_F- sd.hat_M)) 
  
}





####################################################################################


margins_summary(model)
compute_marginal_effect(model, subsample, 'genero')

variable_of_interest = 'genero'

summary(model)
##################################################################################
# data_ = data
data = subsample
prob_base <- predict(model, type = "response",
                     newdata = subset(data, data[[variable_of_interest]] == levels(data[[variable_of_interest]])[1] )  )

grepl(variable_of_interest, )

beta_0 = model$coefficients[1]
beta_1 = model$coefficients[2]
beta_2 = model$coefficients[3]



# Create a vector of the other strings
other_strings <- c(  names(beta_0) ,names(beta_1), names(beta_2))
contained_in <- other_strings[which(sapply(other_strings, function(x) grepl(variable_of_interest, x)))]

beta_interest = model$coefficients[contained_in]




# Creating a modified dataset for calculating the marginal effect
# data_mod[[variable_of_interest]] <- levels(data[[variable_of_interest]])[1]
prob_base_mod <- predict(model, type = "response",
                         newdata = subset(data, data[[variable_of_interest]] != levels(data[[variable_of_interest]])[1] )  )

prob_base_mod_ <- (beta_interest * exp(prob_base_mod)  ) / (1 + exp(-prob_base_mod) )^2 

prob_base_ <-(beta_interest * exp(prob_base)  ) / (1 + exp(-prob_base) )^2 
# prob_base_mod <- predict(model, type = "response", newdata = data_mod)

marginal_effect <- prob_base_mod - prob_base
marginal_effect_ <- prob_base_mod_ - prob_base_

summary(marginal_effect)
summary(marginal_effect_)


sd(marginal_effect, na.rm = T)
sd(marginal_effect_, na.rm = T)


sd_marginal_effect <- sd(marginal_effect , na.rm = T)  #sd(marginal_effect, na.rm = )

summary(predict(model, type = "response", newdata = data))




summary(predict(model, type = "response", newdata = subset(data, data[[variable_of_interest]] != levels(data[[variable_of_interest]])[1] ) ))

summary(predict(model, type = "response", newdata = subset(data, data[[variable_of_interest]] == levels(data[[variable_of_interest]])[1] ) ))




##################################################################################







data = tabla
variable_of_interest = 'genero'
compute_marginal_effect <- function(model, data, variable_of_interest) {
  prob_base <- predict(model, type = "response", newdata = data)
  data_mod <- data
  
  # Creating a modified dataset for calculating the marginal effect
  data_mod[[variable_of_interest]] <- levels(data[[variable_of_interest]])[1]
  prob_base_mod <- predict(model, type = "response", newdata = data_mod)
  
  marginal_effect <- prob_base - prob_base_mod
  
  return( mean( marginal_effect, na.rm = T) )
}



###
# fixest_ame_gender <- function(modelo, tabla){ 
"
  This function estimate the average marginal effect 
  "
modelo = model
summary(modelo)
tabla=subsample
tabla$model_fit =  predict(modelo,tabla, type = "response" )
tabla$Probability = 1 / (1+ exp( -(tabla$model_fit) ) )

y.hat_F = mean( subset(tabla, tabla$genero == 'F'  & is.na(tabla$Probability)==F )$Probability , )
y.hat_F = mean( subset(tabla, tabla$genero == 'F'  & is.na(tabla$model_fit)==F )$model_fit , )

sd.hat_F = sd( subset(tabla, tabla$genero == 'F'  & is.na(tabla$Probability)==F )$Probability ,na.rm = T )

y.hat_M = mean( subset(tabla, tabla$genero == 'M'  & is.na(tabla$Probability)==F )$Probability , )
y.hat_M = mean( subset(tabla, tabla$genero ==  'M'   & is.na(tabla$model_fit)==F )$model_fit , )

sd.hat_M = sd( subset(tabla, tabla$genero == 'M'  & is.na(tabla$Probability)==F )$Probability ,na.rm = T )

return(c(y.hat_F-y.hat_M , sd.hat_F- sd.hat_M)) 
summary(modelo)
# }

