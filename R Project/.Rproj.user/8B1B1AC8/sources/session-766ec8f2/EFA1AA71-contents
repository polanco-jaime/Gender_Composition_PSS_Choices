############### General Settings ############### 
source("C:/Users/USER/Desktop/01-with-the-boys/Scripts/R/genereal_settings.R", echo=TRUE)
source("C:/Users/USER/Desktop/01-with-the-boys/Scripts/R/read_data.R", echo=TRUE)
source("C:/Users/USER/Desktop/01-with-the-boys/Scripts/R/functions.R", echo=TRUE)
############### General Settings ###############

data = subset(data, data$YEAR_INFO == '2016')
data$NOMBRE_PROGRAMA = gsub(data$NOMBRE_PROGRAMA, pattern = "Í",replacement = "I")
unique(data$AREA_CONOCIMIENTO)
gc()
gc()
############### General Statistics ###############

data = sqldf("SELECT *,
      CASE WHEN AREA_CONOCIMIENTO = 'HEALTH SCIENCES' 
          AND NOMBRE_PROGRAMA  like '%MED%' THEN 1 ELSE 0 END AS MEDICINE
      FROM data")

data$AREA_CONOCIMIENTO = ifelse(data$MEDICINE == 1, 'MEDICINE', data$AREA_CONOCIMIENTO)

data$AREA_CONOCIMIENTO = ifelse( is.na(data$AREA_CONOCIMIENTO)==T, 'NO STUDIES', data$AREA_CONOCIMIENTO)

gc()
gc()

data_ = sqldf::sqldf("SELECT genero,
             AREA_CONOCIMIENTO, count(*) TOTAL
             FROM data WHERE AREA_CONOCIMIENTO != 'SIN INFORMACION'
             GROUP BY 1,2 ") 
str(data)
ggplot(data_, aes(x = AREA_CONOCIMIENTO, y = TOTAL, fill = genero)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Proportion by Gender in Each Area of Knowledge", y = "Total", x = "Area of Knowledge") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


data_ <- transform(data_, proportion = TOTAL / ave(TOTAL, AREA_CONOCIMIENTO, FUN = sum))

ggplot(data_, aes(x = AREA_CONOCIMIENTO, y = proportion, fill = genero)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Proportion by Gender in Each Area of Knowledge", y = "Proportion", x = "Area of Knowledge") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Create a new variable with wrapped text
data_$wrapped_area <- str_wrap(data_$AREA_CONOCIMIENTO, width = 15) 


# Create the bar plot with wrapped text
ggplot(data_, aes(x = wrapped_area, y = proportion, fill = genero)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(title = "Proportion of Individuals by Gender in Each Area of Knowledge", y = "Proportion", x = "Area of Knowledge") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5) )

##############
# Define custom colors for each gender
blue_palette <- c("#0073C2", "#0096D6")# You can add or remove colors as needed



png(paste0(graphs_dir , 'gender_distr_by_knowlledge' ,".png"),  width = 1030, height = 598)

# Create the bar plot with wrapped text and custom colors
# Create the bar plot with wrapped text and the blue color palette
ggplot(data_, aes(x = wrapped_area, y = proportion, fill = genero)) +
  geom_bar(stat = "identity", position = "fill") +  # Use position = "fill"
  labs(title = "Proportion of Individuals by Gender in Each Area of Knowledge", y = "Proportion", x = "Area of Knowledge") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5),  # Adjust vjust to center text
        axis.text.y = element_text()) +  # Remove the unused argument
  scale_fill_manual(values = blue_palette)



dev.off()



####################

data_ = categorize_stem(data_)
sqldf("SELECT STEM,  GENERO, SUM(TOTAL) TOTAL FROM data_
      GROUP BY 1,2 ")


35464 / ( 181694 + 35464 ) # F
43844 / (43844 + 134594 ) # m

(35464 + 35464) /   (43844 + 134594 + 181694 + 35464 )

