source("Scripts/R/genereal_settings.R", echo=TRUE)
source("./Scripts/R/functions.R", echo=TRUE)


# 1.  Data Reading and Preparation ####
## 1.1 Data For Never Treated Estimation ####
merged_data_never_treated = arrow::read_parquet("Data/never_treated_data.parquet")
merged_data_never_treated$`Female Proportion` = (merged_data_never_treated$TOT_FEMALE/merged_data_never_treated$TOT_STU)
merged_data_never_treated$`Female Proportion`  <- as.numeric( scale(merged_data_never_treated$`Female Proportion` , center = TRUE, scale = TRUE) )

merged_data_never_treated = summarize_data_staggered(merged_data_never_treated)
merged_data_never_treated$YEAR_INFO = as.numeric(merged_data_never_treated$YEAR_INFO)
control_group = "nevertreated"

## 1.2 Data For Not-Yet-Treated Estimation ####
merged_data_non_treated_yet = arrow::read_parquet("Data/no_yet_treated_data.parquet")
merged_data_non_treated_yet=subset(merged_data_non_treated_yet, is.na(merged_data_non_treated_yet$transition_year)==F)
merged_data_non_treated_yet$`Female Proportion` = (merged_data_non_treated_yet$TOT_FEMALE/merged_data_non_treated_yet$TOT_STU)
merged_data_non_treated_yet$`Female Proportion`  <- as.numeric( scale(merged_data_non_treated_yet$`Female Proportion` , center = TRUE, scale = TRUE) )

merged_data_non_treated_yet = summarize_data_staggered(merged_data_non_treated_yet)
merged_data_non_treated_yet$YEAR_INFO = as.numeric(merged_data_non_treated_yet$YEAR_INFO)
summary(merged_data_non_treated_yet$`Female Proportion`)
control_group = "notyettreated"
 
merged_data_non_treated_yet$intensity_event_time <- merged_data_non_treated_yet$`Female Proportion`  * as.numeric(merged_data_non_treated_yet$YEAR_INFO - merged_data_non_treated_yet$transition_year >= 0)
merged_data_non_treated_yet$intensity_event_time <- as.numeric(scale(merged_data_non_treated_yet$intensity_event_time))

# merged_data_non_treated_yet = merged_data_non_treated_yet[merged_data_non_treated_yet$rel_year<6, ]
merged_data_never_treated
# 2. Staggered Estimation ####
outcomes = c(
  "STEM Fields", "Non-STEM Fields", "Not Continuing Education",  
  "Mathematics/Natural Sciences",   "Engineering/Architecture",
  "Law", "Social Sciences/Humanities","Economics/Business",
  "Education Sciences", "Fine Arts","Agronomy/Veterinary"  , 
  "Health Sciences", 'Medicine', 
  'STEM - Science', 'STEM - Technology','STEM - Engineering', 'STEM - Mathematics',
  'Enrollment in University', "Enrollment in Technical", "Enrollment in Technological"
)
# We exclude all the observations affected by COVID 19
temp = merged_data_non_treated_yet[merged_data_non_treated_yet$YEAR_INFO<=2019,]
mean(merged_data_non_treated_yet[merged_data_non_treated_yet$rel_year==-2 ,]$`STEM Fields`)

## 2.1 Main Results ####
### 2.1.0 Enrrollment ####
### 2.1.0 'Enrollment in University'   ########
print( (outcomes[18]) )
university  = ATT_sim_dyn_CS( outcomes[18] , temp )
wald_test_att(university)
save_img_callaway_dynamic(university, outcomes[18])

print( (outcomes[19]) )
technical  = ATT_sim_dyn_CS( outcomes[19] , temp )
wald_test_att(technical)
save_img_callaway_dynamic(technical, outcomes[19])

print( (outcomes[20]) )
technological  = ATT_sim_dyn_CS( outcomes[20] , temp )
wald_test_att(technological)
save_img_callaway_dynamic(technological, outcomes[20])


### 2.1.1  "STEM Fields"   ########
print( (outcomes[1]) )
stem = ATT_sim_dyn_CS( outcomes[1], temp )
save_img_callaway_dynamic(stem, outcomes[1])
wald_test_att(stem)


##### 2.1.1.1   "STEM - Science"   ########
print( (outcomes[14]) )
science = ATT_sim_dyn_CS( outcomes[14], temp )
save_img_callaway_dynamic(science, outcomes[14])
wald_test_att(science)

##### 2.1.1.2  "STEM - Technology"  ########
print( (outcomes[15]) )
technology = ATT_sim_dyn_CS( outcomes[15], temp )
save_img_callaway_dynamic(technology, outcomes[15])
wald_test_att(technology)
mean(temp[temp$rel_year==-2, ]$`STEM - Technology`)*100
##### 2.2.1.3  "STEM - Engineering"  ########
print( (outcomes[16]) )
engineering = ATT_sim_dyn_CS( outcomes[16], temp )
save_img_callaway_dynamic(engineering, outcomes[16])
wald_test_att(engineering)

##### 2.2.1.4 "STEM - Mathematics"  ########
print( (outcomes[17]) )
mathematics = ATT_sim_dyn_CS( outcomes[17], temp )
save_img_callaway_dynamic(mathematics, outcomes[17])
wald_test_att(mathematics)
mean(temp[temp$rel_year==-2, ]$`STEM - Mathematics`)*100

outcomes[17]
### 2.1.2  "Non-STEM Fields"    ########
print((outcomes[2]) )
no_stem = ATT_sim_dyn_CS( outcomes[2], temp )
save_img_callaway_dynamic(no_stem, outcomes[2])
wald_test_att(no_stem)


### 2.1.3  "Not Continuing Education"    ########
print( (outcomes[3]) )
dropout = ATT_sim_dyn_CS( outcomes[3], temp )
save_img_callaway_dynamic(dropout, outcomes[3])
wald_test_att(dropout)

## 2.2 Results by Fields of Study ####

### 2.2.1  "Mathematics/Natural Sciences"    ######## 
 
print((outcomes[4]) )
math_ns = ATT_sim_dyn_CS( outcomes[4] , temp )

save_img_callaway_dynamic(math_ns, gsub(outcomes[4], pattern= "/", replace= " and or "))
wald_test_att(math_ns)
 
### 2.2.2  "Engineering/Architecture"    ######## 
print((outcomes[5]) )
eng = ATT_sim_dyn_CS( outcomes[5]  , temp )

save_img_callaway_dynamic(eng, outcomes[5])
wald_test_att(eng)

### 2.2.3  "Law"    ######## 
print((outcomes[6]) )
law = ATT_sim_dyn_CS( outcomes[6]  , temp )
save_img_callaway_dynamic(law,outcomes[6])
wald_test_att(law)

### 2.2.4  "Social Sciences/Humanities"   ######## 
print((outcomes[7]) )
humanities = ATT_sim_dyn_CS( outcomes[7]  , temp )
save_img_callaway_dynamic(humanities,outcomes[7])
wald_test_att(humanities)

### 2.2.5  "Economics/Business"    ######## 
print((outcomes[8]) )
econ = ATT_sim_dyn_CS( outcomes[8]  , temp )
save_img_callaway_dynamic(econ,outcomes[8])
wald_test_att(econ)

### 2.2.6  "Education Sciences"    ######## 
print((outcomes[9]) )
educ = ATT_sim_dyn_CS( outcomes[9]  , temp )
educ$dynamic
save_img_callaway_dynamic(educ,outcomes[9])
wald_test_att(educ)

### 2.2.7  "Fine Arts"    ######## 
print((outcomes[10]) )
arts = ATT_sim_dyn_CS( outcomes[10]  , temp )
arts$dynamic
save_img_callaway_dynamic(arts,outcomes[10])
wald_test_att(arts)


### 2.2.8  "Agronomy/Veterinary"    ######## 
print((outcomes[11]) )
agro = ATT_sim_dyn_CS( outcomes[11]  , temp )
agro$dynamic
save_img_callaway_dynamic(agro,outcomes[11])
wald_test_att(agro)

### 2.2.9  "Health Sciences"    ######## 
print((outcomes[12]) )
health = ATT_sim_dyn_CS( outcomes[12]  , merged_data_non_treated_yet )
health$dynamic
save_img_callaway_dynamic(health,outcomes[12])
wald_test_att(health)

### 2.2.9  "Medicine"    ######## 
print((outcomes[13]) )
medicine = ATT_sim_dyn_CS( outcomes[13]  , temp )
medicine$dynamic
save_img_callaway_dynamic(medicine,outcomes[13])
wald_test_att(medicine)



# 3. Staggered Estimation - Never Treated ####
## 3.1 Main Results ####
### 3.1.1  "STEM Fields"   ########
print( (outcomes[1]) )
merged_data_never_treated = merged_data_never_treated[merged_data_never_treated$YEAR_INFO<=2019,]
stem_nt = ATT_sim_dyn_CS( outcomes[1], merged_data_never_treated, T )
save_img_callaway_dynamic(stem_nt, paste0(outcomes[1],"\n", "Never Treated") )
wald_test_att(stem_nt)


#### 3.1.1.1  "STEM - Science"   ########
print( (outcomes[14]) )
science_nt = ATT_sim_dyn_CS( outcomes[14], merged_data_never_treated, T )
save_img_callaway_dynamic(science_nt, paste0(outcomes[14],"\n", "Never Treated") )
wald_test_att(science_nt)

#### 3.1.1.2  "STEM - Technology"  ########
print( (outcomes[15]) )
technology_nt = ATT_sim_dyn_CS( outcomes[15], merged_data_never_treated, T )
save_img_callaway_dynamic(technology_nt, paste0(outcomes[15],"\n", "Never Treated") )
wald_test_att(technology_nt)

#### 3.1.1.3  "STEM - Engineering" ########
print( (outcomes[16]) )
engineering_nt = ATT_sim_dyn_CS( outcomes[16], merged_data_never_treated, T )
save_img_callaway_dynamic(engineering_nt, paste0(outcomes[16],"\n", "Never Treated") )
wald_test_att(engineering_nt) 

#### 3.1.1.4  "STEM - Mathematics" ########
print( (outcomes[17]) )
mathematics_nt = ATT_sim_dyn_CS( outcomes[17], merged_data_never_treated, T )
save_img_callaway_dynamic(mathematics_nt, paste0(outcomes[17],"\n", "Never Treated") )
wald_test_att(mathematics_nt) 

 


## 