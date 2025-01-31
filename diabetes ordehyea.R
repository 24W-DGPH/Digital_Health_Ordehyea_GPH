#Introduction------------------
#Digital Health Exam (Agyeman Serebouh Ordehyea) 
#GPH 01
#2025-01-31

# Install and Load Packages into R-----------
install.packages("pacman")
install.packages('rsconnect')

# load packages
pacman::p_load(
  tidyverse,         # data management
  stringr,           # work with strings/characters
  lubridate,         # work with dates
  rio,               # import / export data
  here,              # relative file paths
  fs)                # directory interactions
#
skimr # preview tibbles(aka data frames)
install.packages('styler')
library(dplyr)
library(rio)
library(ggplot2)

#KINDLY LOAD THE ABOVE PACKAGES IN THE PACKAGE SECTION INCASE OF ERROR......


# Import Data -------------------------
diabetes_unclean <- import('diabetes_unclean.csv')

skimr::skim(diabetes_unclean)

install.packages("RColorBrewer")  # Predefined palettes
install.packages("viridis")       # Colorblind-friendly palettes
install.packages("colorspace")    # Advanced palettes

#Cleaning Data ------------------

diabetes_clean <- diabetes_unclean %>%
# NEW name             # OLD name
rename(Patient_Number = No_Pation , 
       Cholesterol  =     Chol ,
       Creatinine     = Cr,
       Triglycerides = TG ,
       Age_years = AGE ,
       Diabetes_status = CLASS)   %>%
  
  #create Age_groups Column
  mutate(Age_years = as.integer(Age_years)) %>%
  
  mutate(Age_group = cut(
    Age_years, 
    breaks = c(0, 25, 35, 45, 55, 65, 100), 
    labels = c("0-25", "26-35", "36-45", "46-55", "56-65", "66-80"),
    right = TRUE  # Include the upper bound in each interval
  )) %>%

  # Create BMI_group column
  mutate(BMI_group = cut(
    BMI , 
    breaks = c(-Inf, 18.5, 24.9, 29.9, 39.9, Inf), 
    labels = c("Underweight", "Normal Range", "Overweight", "Obesity", "Severe Obesity"),
    right = FALSE  # Exclude the upper bound from each interval except the last
  ))  %>%

  #Create Triglyceride_group column
  mutate(Triglyceride_group = cut(
    Triglycerides, 
    breaks = c(-Inf, 1.7, 5.6 , 10.0,  Inf), 
    labels = c("Normal", "Mildly Increased", "Moderately Increased", "Very High"),
    right = TRUE  # Include the upper bound of each interval
  )) %>%
  
  # Create Cholesterol_group column
  
  mutate(Cholesterol_group = cut(
    Cholesterol, 
    breaks = c(-Inf, 5.17, 6.18, Inf), 
    labels = c("Normal", "Borderline High", "High"),
    right = TRUE  # Include the upper bound of each interval
  ))%>%
  
#Create LDL_group column

  mutate(LDL_group = cut(
    LDL,
    breaks = c(-Inf, 1.8, 2.6, 3.3, 4.1, 4.9, Inf),
    labels = c("Optimal (High Risk)", "Optimal", "Near Optimal", 
               "Borderline High", "High", "Very High"),
    right = TRUE  # Include the upper bound of each interval
  )) %>%
  
 # HDL Gender Specification 
  mutate(HDL_group = case_when(
    # For men (Gender == "M")
    Gender == "M" & HDL < 1 ~ "Poor",
    Gender == "M" & HDL >= 1 & HDL <= 1.5 ~ "Better",
    Gender == "M" & HDL > 1.5 ~ "Best",
    # For women (Gender == "F")
    Gender == "F" & HDL < 1.3 ~ "Poor",
    Gender == "F" & HDL >= 1.3 & HDL <= 1.5 ~ "Better",
    Gender == "F" & HDL > 1.5 ~ "Best",
    # Default case (if no condition matches)
    TRUE ~ NA_character_
  )) %>%
  
  
# Remove duplicated rows
distinct()%>% 
  
 # Remove duplicate ID numbers
  distinct(ID, .keep_all = TRUE) %>%
  
 # Remove the Patient_number column
  select(-Patient_Number) %>%
  
# Rearrange columns
 select(ID, Gender, Age_years, Age_group, HbA1c, Diabetes_status, BMI, 
        BMI_group,Cholesterol, 
        Cholesterol_group,Triglycerides, Triglyceride_group,
        LDL,LDL_group,HDL,HDL_group, VLDL,everything()) %>% 
  
# Replace Diabetes_Status values
  mutate(Diabetes_status = recode(Diabetes_status, 
                        "N" = "Non-Diabetic", 
                        "P" = "Pre-Diabetic", 
                        "Y" = "Diabetic")) 


#Grouping Data --------------

# Count the number of diabetic patients
count_diabetic <- diabetes_clean %>% 
filter(Diabetes_status == "Diabetic") %>% 
nrow()

#Count the number of Non-diabetic patients
count_ndiabetic <- diabetes_clean %>%
filter(Diabetes_status == "Non-Diabetic") %>%
nrow()

#Count the number of Pre-diabetic patients
count_pdiabetic <- diabetes_clean  %>%
  filter(Diabetes_status == "Pre-Diabetic")  %>%
  nrow()
 
# Count Patients In a Specific Age group 
age_group_counts <- diabetes_clean %>%
  group_by(Age_group) %>%
  summarise(Patient_Count = n())

## Count patients in each BMI group
bmi_group_counts <- diabetes_clean %>%
  group_by(BMI_group) %>%
  summarise(Patient_Count = n())

# Count patients in each Cholesterol group
cholesterol_group_counts <- diabetes_clean %>%
  group_by(Cholesterol_group) %>%
  summarise(Patient_Count = n())

# Count patients in each Triglyceride group
triglyceride_group_counts <- diabetes_clean %>%
  group_by(Triglyceride_group) %>%
  summarise(Patient_Count = n()) 

# Count patients in each LDL group
ldl_group_counts <- diabetes_clean %>%
  group_by(LDL_group) %>%
  summarise(Patient_Count = n())

# Count patients in each HDL group
hdl_group_counts <- diabetes_clean %>%
  group_by(HDL_group) %>%
  summarise(Patient_Count = n())
 

#Visualize the Data (ggplot2) ------------------------------
library(ggplot2)
library(colorspace)
library(RColorBrewer)

# N- (Non- Diabetic) , P- (Pre-Diabetic), Y,(Diabetic)

# Stacked bar chart: Diabetes_Status by BMI_group
ggplot(data = diabetes_clean, aes(x = BMI_group, fill = Diabetes_status)) +
  geom_bar(position = "fill") +
  labs(
    title = "Proportion of Diabetes status Across BMI Categories",
    x = "BMI Group",
    y = "Proportion",
    fill = "Diabetes status"
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal() + 
  scale_fill_manual(values = c("Non-Diabetic" = "blue", "Pre-Diabetic" = "orange", "Diabetic" = "red"))

  

# Bar chart of BMI_group by Gender
ggplot(data = diabetes_clean, aes(x = BMI_group, fill = Gender)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Counts of BMI Groups by Gender",
    x = "BMI Group",
    y = "Count",
    fill = "Gender"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("M" = "blue", "F" = "yellow"))


# Bar chart of LDL_group by Diabetes_status
ggplot(data = diabetes_clean, aes(x = LDL_group, fill = Diabetes_status)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Counts of LDL Categories by Diabetes status",
    x = "LDL Group",
    y = "Count",
    fill = "Diabetes status"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("Non-Diabetic" = "blue", "Pre-Diabetic" = "orange", "Diabetic" = "red"))

#Scatter Plot of Age by Diabetes status
ggplot(data = diabetes_clean, aes(x = Diabetes_status, y = Age_years, color 
                                  = Diabetes_status)) +
  geom_jitter(width = 0.2, alpha = 0.7) +
  labs(
    title = "Scatter Plot of Age by Diabetes status",
    x = "Diabetes Status",
    y = "Age (Years)",
    color = "Diabetes status"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("Non-Diabetic" = "blue", "Pre-Diabetic" = "yellow", "Diabetic" = "red"))


# Bar plot of Cholesterol Categories by Diabetes status
ggplot(data = diabetes_clean, aes(x = Cholesterol_group, fill = Diabetes_status)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Counts of Cholesterol Categories by Diabetes status",
    x = "Cholesterol Group",
    y = "Count",
    fill = "Diabetes status"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("Non-Diabetic" = "blue", "Pre-Diabetic" = "orange", "Diabetic" = "red"))


# Stacked Bar Plot: Diabetes status by Gender
ggplot(data = diabetes_clean, aes(x = Gender, fill = Diabetes_status)) +
  geom_bar(position = "fill") +  # Proportions (stacked)
  labs(
    title = "Proportion of Diabetes Status by Gender",
    x = "Gender",
    y = "Proportion",
    fill = "Diabetes status"
  ) +
  scale_y_continuous(labels = scales::percent_format()) +  # Convert y-axis to percentages
  scale_fill_manual(values = c("Non-Diabetic" = "blue", "Pre-Diabetic" = "orange", "Diabetic" = "red")) +
  theme_minimal()





                     