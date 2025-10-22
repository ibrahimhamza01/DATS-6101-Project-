# Step 1: Load libraries and read data
library(dplyr)
library(survey)
library(ggplot2)
library(tidyr)

setwd("C:/Users/dhowell07/Documents/DATS6101/R/Project1")

# Step 2: Read yearly BRFSS datasets
brfss = read.csv("brfss_2018_2023.csv", header=T)

## Question 3 (Demographic Differences): Did trends in obesity differ significantly across demographic groups such as sex, income, and race/ethnicity during 2019–2022?
## Demographic data for this dataset will include sex, age group, ethnicity, education level, income, and state.
## Created new dataset to include Body Mass Index (bmi) and the demographic variables.

brfss_demo = brfss[,c(1:4,6,7,12,14:18)]

##Check class of variables
t(t(sapply(brfss_demo,class))) 

##Change varaible class to factor and order variables to need to be ordered
brfss_demo$psu = as.factor(brfss_demo$psu)
brfss_demo$psu <- factor(brfss_demo$psu)
brfss_demo$interview_year = as.factor(brfss_demo$interview_year)
brfss_demo$interview_year <- factor(brfss_demo$interview_year, order=T,levels = c("2018","2019","2020","2021","2022","2023"))
brfss_demo$bmi_category = as.factor(brfss_demo$bmi_category)
brfss_demo$bmi_category <- factor(brfss_demo$bmi_category, order=T,levels = c("Underweight","Normal weight","Overweight","Obese"))
brfss_demo$overweight_or_obese = as.factor(brfss_demo$overweight_or_obese)
brfss_demo$overweight_or_obese <- factor(brfss_demo$overweight_or_obese, order=T,levels = c("Yes","No"))
brfss_demo$sex = as.factor(brfss_demo$sex)
brfss_demo$sex <- factor(brfss_demo$sex, order=F)
brfss_demo$age_group = as.factor(brfss_demo$age_group)
brfss_demo$age_group <- factor(brfss_demo$age_group, order=T,levels = c("18-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80 or older"))
brfss_demo$race_ethnicity = as.factor(brfss_demo$race_ethnicity)
brfss_demo$race_ethnicity <- factor(brfss_demo$race_ethnicity, order=F)
brfss_demo$education_level = as.factor(brfss_demo$education_level)
brfss_demo$education_level <- factor(brfss_demo$education_level, order=T,levels = c("No school/K","Grades 1-8","Grades 9-11","Grade 12/GED","College 1-3 years","College 4+ years"))


### Post-Covid Chi-squared Test ###
post_covid = subset(brfss_demo,interview_year %in% c('2021','2022','2023'))
summary(post_covid)

# Create Post-Covid Survey Design
post_covid_design <- svydesign(
  id = ~psu,
  weights = ~weight_final,
  strata = ~strata,
  data = post_covid, 
  nest = TRUE
)

##BMI Category vs Gender##
sex_post_table = svytable(~bmi_category + sex,design = post_covid_design)
sex_post_table
sex_post_chisq = svychisq(~bmi_category + sex, design = post_covid_design, statistic = "Chisq")
sex_post_chisq

sex_post_table <- as.data.frame(sex_post_table)
ggplot(data = sex_post_table, mapping = aes (x = sex, y = Freq, fill = bmi_category)) +
  geom_col(position="fill")+
  coord_flip() +
  labs(x="Gender", y="Frequency") +
  labs(fill = "BMI Category")+
  ggtitle("Post-COVID BMI Category Frequency by Gender")+
  theme(plot.title = element_text(hjust=0.5))

##BMI Category vs Age Group##
age_post_table = svytable(~bmi_category + age_group,design = post_covid_design)
age_post_chisq = svychisq(~bmi_category + age_group, design = post_covid_design, statistic = "Chisq")
age_post_chisq

age_post_table <- as.data.frame(age_post_table)
ggplot(data = age_post_table, mapping = aes (x = age_group, y = Freq, fill = bmi_category)) +
  geom_col(position="fill")+
  coord_flip()+  
  labs(x="Age Group", y="Frequency") +
  labs(fill = "BMI Category")+
  ggtitle("Post-COVID BMI Category Frequency by Age Group")+
  theme(plot.title = element_text(hjust=0.5))
  
##BMI Category vs Ethnicity##
ethnicity_post_table = svytable(~bmi_category + race_ethnicity,design = post_covid_design)
ethnicity_post_chisq = svychisq(~bmi_category + race_ethnicity, design = post_covid_design, statistic = "Chisq")
ethnicity_post_chisq

ethnicity_post_table <- as.data.frame(ethnicity_post_table)
ggplot(data = ethnicity_post_table, mapping = aes (x = race_ethnicity, y = Freq, fill = bmi_category)) +
  geom_col(position="fill")+
  coord_flip()+
  labs(x="Ethnicity", y="Frequency") +
  labs(fill = "BMI Category")+
  ggtitle("Post-COVID BMI Category Frequency by Ethnicity")+
  theme(plot.title = element_text(hjust=0.5))

##BMI Category vs Education Level##
education_post_table = svytable(~bmi_category + education_level,design = post_covid_design)
education_post_chisq = svychisq(~bmi_category + education_level, design = post_covid_design, statistic = "Chisq")
education_post_chisq

education_post_table <- as.data.frame(education_post_table)
ggplot(data = education_post_table, mapping = aes (x = education_level, y = Freq, fill = bmi_category)) +
  geom_col(position="fill")+
  coord_flip()+
  labs(x="Education Level", y="Frequency") +
  labs(fill = "BMI Category")+
  ggtitle("Post-COVID BMI Category Frequency by Education Level")+
  theme(plot.title = element_text(hjust=0.5))

### Pre-Covid Chi-squared Test ###
pre_covid = subset(brfss_demo,interview_year %in% c('2018','2019','2020'))
summary(pre_covid)


# Create Pre-Covid Survey Design
pre_covid_design <- svydesign(
  id = ~psu,
  weights = ~weight_final,
  strata = ~strata,
  data = pre_covid,
  nest = TRUE
)

##BMI Category vs Gender##
sex_pre_table = svytable(~bmi_category + sex,design = pre_covid_design)
sex_pre_chisq = svychisq(~bmi_category + sex, design = pre_covid_design, statistic = "Chisq")
sex_pre_chisq

sex_pre_table <- as.data.frame(sex_pre_table)
ggplot(data = sex_pre_table, mapping = aes (x = sex, y = Freq, fill = bmi_category)) +
  geom_col(position="fill")+
  coord_flip()+
  labs(x="Gender", y="Frequency") +
  labs(fill = "BMI Category")+
  ggtitle("Pre-COVID BMI Category Frequency by Gender")+
  theme(plot.title = element_text(hjust=0.5))

##BMI Category vs Age Group##
age_pre_table = svytable(~bmi_category + age_group,design = pre_covid_design)
age_pre_chisq = svychisq(~bmi_category + age_group, design = pre_covid_design, statistic = "Chisq")
age_pre_chisq

age_pre_table <- as.data.frame(age_pre_table)
ggplot(data = age_pre_table, mapping = aes (x = age_group, y = Freq, fill = bmi_category)) +
  geom_col(position="fill")+
  coord_flip()+
  labs(x="Age Group", y="Frequency") +
  labs(fill = "BMI Category")+
  ggtitle("Pre-COVID BMI Category Frequency by Age Group")+
  theme(plot.title = element_text(hjust=0.5))

##BMI Category vs Ethnicity##
ethnicity_pre_table = svytable(~bmi_category + race_ethnicity,design = pre_covid_design)
ethnicity_pre_chisq = svychisq(~bmi_category + race_ethnicity, design = pre_covid_design, statistic = "Chisq")
ethnicity_pre_chisq

ethnicity_pre_table <- as.data.frame(ethnicity_pre_table)
ggplot(data = ethnicity_pre_table, mapping = aes (x = race_ethnicity, y = Freq, fill = bmi_category)) +
  geom_col(position="fill")+
  coord_flip()+
  labs(x="Ethnicity", y="Frequency") +
  labs(fill = "BMI Category")+
  ggtitle("Pre-COVID BMI Category Frequency by Ethnicity")+
  theme(plot.title = element_text(hjust=0.5))

##BMI Category vs Education Level##
education_pre_table = svytable(~bmi_category + education_level,design = pre_covid_design)
education_pre_chisq = svychisq(~bmi_category + education_level, design = pre_covid_design, statistic = "Chisq")
education_pre_chisq

education_pre_table <- as.data.frame(education_pre_table)
ggplot(data = education_pre_table, mapping = aes (x = education_level, y = Freq, fill = bmi_category)) +
  geom_col(position="fill")+
  coord_flip()+
  labs(x="Education Level", y="Frequency") +
  labs(fill = "BMI Category")+
  ggtitle("Pre-COVID BMI Category Frequency by Education Level")+
  theme(plot.title = element_text(hjust=0.5))
