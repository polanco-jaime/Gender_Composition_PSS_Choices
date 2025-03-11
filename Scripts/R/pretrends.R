source("Scripts/R/genereal_settings.R", echo=TRUE)
source("./Scripts/R/functions.R", echo=TRUE)

gc()
data_ = merged_data #arrow::read_parquet("Data/staggered_data.parquet")
gc()
"
Staggered timing
So far we have focused on a simple case without staggered timing. 
Fortunately, the HonestDiD approach works well with recently-introduced methods for 
DiD under staggered treatment timing. Below, we show how the package can be used with 
the fixest package implementing Sun and Abraham or with the did package implementing 
Callaway and Sant’Anna. (See, also, the example on the did package website.) 
We are hoping to more formally integrate the did and HonestDiD packages in the future – stay tuned!
"
# "Callaway and Sant’Anna"

###
data_ = summarize_data_staggered(data_[ data_$TOT_STU>=4,])
data_$id = as.numeric(factor(data_$school_id_, levels = sort(unique(data_$school_id_), decreasing = TRUE)))
data_$YEAR_INFO = as.numeric( data_$YEAR_INFO )
data_$transition_year = as.numeric( data_$transition_year )
 
gc()
 
data_$intensity_event_time <- data_$`Intensity Event Time` * as.numeric(data_$YEAR_INFO - data_$transition_year >= 0)
hist(data_$intensity_event_time)
data_$intensity_event_time <- (scale(data_$intensity_event_time))
hist(data_$intensity_event_time)
# Run the CS event-study with 'universal' base-period option
## Note that universal base period normalizes the event-time minus 1 coef to 0
data_$`STEM Fields` <- (scale(data_$`STEM Fields`))
data_$`Non-STEM Fields` <- (scale(data_$`Non-STEM Fields`))
# data_ = data_[data_$transition_year!= 2015,]
 
cs_results <- did::att_gt( 
      yname = outcomes[1] ,
       tname = "YEAR_INFO", # Time variable
       idname = "codigo_dane_sede", # School ID
       gname = "transition_year", # Treatment group (year of transition)
       control_group =  "notyettreated",
       base_period = "universal",
       data = temp,
       panel = F, 
       est_method = "dr")

gc()
 

es <- did::aggte(cs_results, type = "dynamic"
                 # ,min_e = -5, max_e = 5, na.rm = TRUE
                 )

summary(es)

gc()
#Run sensitivity analysis for relative magnitudes
sensitivity_results <-
  honest_did(es,
             e=0.2,
             type="relative_magnitude",
             Mbarvec=seq(from = 0.8, 
                         to = 2, 
                         by = 0.1))

sensitivity_results
gc()
HonestDiD::createSensitivityPlot_relativeMagnitudes(sensitivity_results$robust_ci,
                                                    sensitivity_results$orig_ci)

gc()





# 
# 
# 
# ##############
# "
# To use the pretrends package, we need the results of an event-study, 
# namely the vector of event-study coefficients (beta),
# their variance-covariance matrix (sigma), and the relative time periods they correspond to (t).
# We use the beta and sigma saved from Callaway and Sant’Anna (2021) estimator.
# 
# 
# "
# 
# # remotes::install_github("asheshrambachan/HonestDiD")
# 
# 
# 
# df <- read_dta("https://raw.githubusercontent.com/Mixtape-Sessions/Advanced-DID/main/Exercises/Data/ehec_data.dta")
# df
# head(df,5)
# 
# 
# #Keep years before 2016. Drop the 2016 cohort
# df_nonstaggered <- df %>% filter(year < 2016 &
#                                    (is.na(yexp2)| yexp2 != 2015) )
# 
# #Create a treatment dummy
# df_nonstaggered <- df_nonstaggered %>% mutate(D = case_when( yexp2 == 2014 ~ 1,
#                                                              T ~ 0))
# 
# #Run the TWFE spec
# twfe_results <- fixest::feols(dins ~ i(year, D, ref = 2013) | stfips + year,
#                               cluster = "stfips",
#                               data = df_nonstaggered)
# 
# 
# betahat <- summary(twfe_results)$coefficients #save the coefficients
# sigma <- summary(twfe_results)$cov.scaled #save the covariance matrix
# 
# 
# fixest::iplot(twfe_results)
# 
# delta_rm_results <-
#   HonestDiD::createSensitivityResults_relativeMagnitudes(
#     betahat = betahat, #coefficients
#     sigma = sigma, #covariance matrix
#     numPrePeriods = 5, #num. of pre-treatment coefs
#     numPostPeriods = 2, #num. of post-treatment coefs
#     Mbarvec = seq(0.5,2,by=0.5) #values of Mbar
#   )
# 
# delta_rm_results
# "
# The output of the previous command shows a robust confidence
# interval for different values of Mbar. We see that the “breakdown
# value” for a significant effect is Mbar = 2, meaning that the significant
# result is robust to allowing for violations of parallel trends up to twice as
# big as the max violation in the pre-treatment period.
# "
# originalResults <- HonestDiD::constructOriginalCS(betahat = betahat,
#                                                   sigma = sigma,
#                                                   numPrePeriods = 5,
#                                                   numPostPeriods = 2)
# 
# HonestDiD::createSensitivityPlot_relativeMagnitudes(delta_rm_results, originalResults)
# 
# # Sensitivity Analysis Using Smoothness Restrictions
# "
# We can also do a sensitivity analysis based on smoothness restrictions –
# i.e. imposing that the slope of the difference in trends changes by no more than M between periods.
# "
# delta_sd_results <-
#   HonestDiD::createSensitivityResults(betahat = betahat,
#                                       sigma = sigma,
#                                       numPrePeriods = 5,
#                                       numPostPeriods = 2,
#                                       Mvec = seq(from = 0, to = 0.05, by =0.01))
# 
# delta_sd_results
# 
# 
# createSensitivityPlot(delta_sd_results, originalResults)
# 
# "
# We see that the breakdown value for a significant effect is M ≈ 0.03,
# meaning that we can reject a null effect unless we are willing to allow
# for the linear extrapolation across consecutive periods
# to be off by more than 0.03 percentage points.
# 
# 
# "
# 
# 
# 
# delta_rm_results_avg <-
#   HonestDiD::createSensitivityResults_relativeMagnitudes(betahat = betahat,
#                                                          sigma = sigma,
#                                                          numPrePeriods = 5,
#                                                          numPostPeriods = 2, Mbarvec = seq(0,2,by=0.5),
#                                                          l_vec = c(0.5,0.5))
# 
# originalResults_avg <- HonestDiD::constructOriginalCS(betahat = betahat,
#                                                       sigma = sigma,
#                                                       numPrePeriods = 5,
#                                                       numPostPeriods = 2,
#                                                       l_vec = c(0.5,0.5))
# 
# HonestDiD::createSensitivityPlot_relativeMagnitudes(delta_rm_results_avg, originalResults_avg)
