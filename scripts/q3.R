install.packages("readr")
library(readr)
readr::write_csv(x = brfss_2018_2023,file = "brfss_2018_2023.csv", na ="")
getwd()

##brfss = read.csv("C:/Users/dhowell07/Documents/DATS6101/R/Project1/brfss_2018_2023.csv", header=T)
brfss_sample = read.csv("C:/Users/dhowell07/Documents/DATS6101/R/Project1/brfss_2018-2023_full_clean_sample.csv", header=T)

##t(t(sapply(brfss,class))) ##To view the classes of each variable. Interview year is a numeric variable and needs to be changed to a factor variable.
##brfss$interview_year = as.factor(brfss$interview_year)

t(t(sapply(brfss_sample,class))) ##To view the classes of each variable. Interview year is a numeric variable and needs to be changed to a factor variable.
brfss_sample$interview_year = as.factor(brfss_sample$interview_year)



## Question 3 (Demographic Differences): Did trends in obesity differ significantly across demographic groups such as sex, income, and race/ethnicity during 2019–2022?
## Demographic data for this dataset will include sex, age group, ethnicity, education level, income, and state.
## Created new dataset to include Body Mass Index (bmi) and the demographic variables.

brfss_demo = brfss_sample[,c(4:6,12,14:18)]

t(t(sapply(brfss_demo,class)))

brfss_demo$interview_year = as.factor(brfss_demo$interview_year)
brfss_demo$interview_year <- factor(brfss_demo$interview_year, order=T,levels = c("2018","2019","2020","2021","2022","2023"))
brfss_demo$bmi_category = as.factor(brfss_demo$bmi_category)
brfss_demo$bmi_category <- factor(brfss_demo$bmi_category, order=T,levels = c("Underweight","Normal weight","Overweight","Obese"))
brfss_demo$sex = as.factor(brfss_demo$sex)
brfss_demo$sex <- factor(brfss_demo$sex, order=F)
brfss_demo$age_group = as.factor(brfss_demo$age_group)
brfss_demo$age_group <- factor(brfss_demo$age_group, order=T,levels = c("18-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80 or older"))
brfss_demo$race_ethnicity = as.factor(brfss_demo$race_ethnicity)
brfss_demo$race_ethnicity <- factor(brfss_demo$race_ethnicity, order=F)
brfss_demo$education_level = as.factor(brfss_demo$education_level)
brfss_demo$education_level <- factor(brfss_demo$education_level, order=T,levels = c("No school/K","Grades 1-8","Grades 9-11","Grade 12/GED","College 1-3 years","College 4+ years"))

class(brfss_demo$interview_year)
class(brfss_demo$bmi_category)
class(brfss_demo$sex)
class(brfss_demo$age_group)
class(brfss_demo$race_ethnicity)
class(brfss_demo$education_level)

pre_covid = subset(brfss_demo,interview_year %in% c('2018','2019'))
summary(pre_covid)

covid = subset(brfss_demo,interview_year %in% c('2020','2021'))
summary(covid)

post_covid = subset(brfss_demo,interview_year %in% c('2022','2023'))
summary(post_covid)

loadPkg("MASS")
####Pre-Covid Comparison####

###BMI Category vs Gender 
##Contingency table
contable_sex_precovid = table( pre_covid$sex,pre_covid$bmi_category)
# contable # 
xkabledply(contable_sex_precovid, title="Contingency table for Pre-Covid BMI Category vs Gender")

##Chi Test
chitest_sex_precovid = chisq.test(contable_sex_precovid)
chitest_sex_precovid

###BMI Category vs Age Group 
##Contingency table
contable_age_group_precovid = table(pre_covid$age_group,pre_covid$bmi_category)
# contable # 
xkabledply(contable_age_group_precovid, title="Contingency table for Pre-Covid BMI Category vs Age Group")

chitest_age_group_precovid = chisq.test(contable_age_group_precovid)
chitest_age_group_precovid

##BMI Category vs Ethnicity
##Contingency table
summary(pre_covid$bmi_category)
contable_race_ethnicity_precovid = table(pre_covid$race_ethnicity,pre_covid$bmi_category)
# contable # 
xkabledply(contable_race_ethnicity_precovid, title="Contingency table for Pre-Covid BMI Category vs Ethnicity")

chitest_race_ethnicity_precovid = chisq.test(contable_race_ethnicity_precovid)
chitest_race_ethnicity_precovid

##BMI Category vs Education Level)
##Contingency table
contable_education_level_precovid = table(pre_covid$education_level,pre_covid$bmi_category)
# contable # 
xkabledply(contable_education_level_precovid, title="Contingency table for Pre-Covid BMI Category vs Education Level")

chitest_education_level_precovid = chisq.test(contable_education_level_precovid)
chitest_education_level_precovid

####Covid Comparison####

###BMI Category vs Sex 
##Contingency table
contable_sex_covid = table(covid$bmi_category, covid$sex)
# contable # 
xkabledply(contable_sex_covid, title="Contingency table for Covid BMI Category vs Gender")

##Chi Test
chitest_sex_covid = chisq.test(contable_sex_covid)
chitest_sex_covid

###BMI Category vs Age Group 
##Contingency table
contable_age_group_covid = table(covid$bmi_category, covid$age_group)
# contable # 
xkabledply(contable_age_group_covid, title="Contingency table for Covid BMI Category vs Age Group")

chitest_age_group_covid = chisq.test(contable_age_group_covid)
chitest_age_group_covid

##BMI Category vs Ethnicity
##Contingency table
contable_race_ethnicity_covid = table(covid$bmi_category, covid$race_ethnicity)
# contable # 
xkabledply(contable_race_ethnicity_covid, title="Contingency table for Covd BMI Category vs Ethnicity")

chitest_race_ethnicity_covid = chisq.test(contable_race_ethnicity_covid)
chitest_race_ethnicity_covid

##BMI Category vs Education Level)
##Contingency table
contable_education_level_covid = table(covid$bmi_category, covid$education_level)
# contable # 
xkabledply(contable_education_level_covid, title="Contingency table for Covid BMI Category vs Education Level")

chitest_education_level_covid = chisq.test(contable_education_level_precovid)
chitest_education_level_covid

####Post-Covid Comparison####

###BMI Category vs Sex 
##Contingency table
contable_sex_postcovid = table(post_covid$bmi_category, post_covid$sex)
# contable # 
xkabledply(contable_sex_postcovid, title="Contingency table for Post-Covid BMI Category vs Gender")

##Chi Test
chitest_sex_postcovid = chisq.test(contable_sex_postcovid)
chitest_sex_postcovid

###BMI Category vs Age Group 
##Contingency table
contable_age_group_postcovid = table(post_covid$bmi_category, post_covid$age_group)
# contable # 
xkabledply(contable_age_group_postcovid, title="Contingency table for Post-Covid BMI Category vs Age Group")

chitest_age_group_postcovid = chisq.test(contable_age_group_postcovid)
chitest_age_group_postcovid

##BMI Category vs Ethnicity
##Contingency table
contable_race_ethnicity_postcovid = table(post_covid$bmi_category, post_covid$race_ethnicity)
# contable # 
xkabledply(contable_race_ethnicity_postcovid, title="Contingency table for Post-Covid BMI Category vs Ethnicity")

chitest_race_ethnicity_postcovid = chisq.test(contable_race_ethnicity_postcovid)
chitest_race_ethnicity_postcovid

##BMI Category vs Education Level)
##Contingency table
contable_education_level_postcovid = table(post_covid$bmi_category, post_covid$education_level)
# contable # 
xkabledply(contable_education_level_postcovid, title="Contingency table for Post-Covid BMI Category vs Education Level")

chitest_education_level_postcovid = chisq.test(contable_education_level_postcovid)
chitest_education_level_postcovid