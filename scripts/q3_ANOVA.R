# Step 1: Load libraries and read data
library(dplyr)
library(survey)
library(ggplot2)
library(tidyr)
library(ezids)

setwd("C:/Users/dhowell07/Documents/DATS6101/R/Project1")

brfss = read.csv("brfss_2018_2023.csv", header=T)
brfss_demo = brfss[,c(1:6,7,12,14:18)]

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

## BMI By Gender ##

##Male##
male = subset(brfss_demo,sex %in% 'Male')
summary(male)

library("ggplot2")
ggplot(male, aes(x=interview_year, y=bmi)) + 
  geom_boxplot( colour=c("#ff0000","#11cc11","#0000ff","#ff00ff","#ffa500","#551a8b"), outlier.shape=8, outlier.size=4) +
  labs(x="Year", y = "Body Mass Index (BMI)")+
  ggtitle("Body Mass Index (BMI) for Males from 2018 - 2023")+
  theme(plot.title = element_text(hjust=0.5))

male_anova = aov(bmi ~ interview_year, data=male)
xkabledply(male_anova)
summary(male_anova)

tukeymale<- TukeyHSD(male_anova) 
tukeymale

##Female##
female = subset(brfss_demo,sex %in% 'Female')
summary(female)

library("ggplot2")
ggplot(female, aes(x=interview_year, y=bmi)) + 
  geom_boxplot( colour=c("#ff0000","#11cc11","#0000ff","#ff00ff","#ffa500","#551a8b"), outlier.shape=8, outlier.size=4) +
  labs(x="Year", y = "Body Mass Index (BMI)")+
  ggtitle("Body Mass Index (BMI) for Females from 2018 - 2023")+
  theme(plot.title = element_text(hjust=0.5))

female_anova = aov(bmi ~ interview_year, data=female)
xkabledply(female_anova)
summary(female_anova)

tukeyfemale<- TukeyHSD(male_anova) 
tukeyfemale

## BMI By Ethnicity ##

##White##
white = subset(brfss_demo,race_ethnicity %in% 'White NH')
summary(white)

library("ggplot2")
ggplot(white, aes(x=interview_year, y=bmi)) + 
  geom_boxplot( colour=c("#ff0000","#11cc11","#0000ff","#ff00ff","#ffa500","#551a8b"), outlier.shape=8, outlier.size=4) +
  labs(x="Year", y = "Body Mass Index (BMI)")+
  ggtitle("Body Mass Index (BMI) for Whites from 2018 - 2023")+
  theme(plot.title = element_text(hjust=0.5))

white_anova = aov(bmi ~ interview_year, data=white)
xkabledply(white_anova)
summary(white_anova)

tukeywhite<- TukeyHSD(white_anova) 
tukeywhite

##Hispanic##
hispanic = subset(brfss_demo,race_ethnicity %in% 'Hispanic')
summary(hispanic)

library("ggplot2")
ggplot(hispanic, aes(x=interview_year, y=bmi)) + 
  geom_boxplot( colour=c("#ff0000","#11cc11","#0000ff","#ff00ff","#ffa500","#551a8b"), outlier.shape=8, outlier.size=4) +
  labs(x="Year", y = "Body Mass Index (BMI)")+
  ggtitle("Body Mass Index (BMI) for Hispanics from 2018 - 2023")+
  theme(plot.title = element_text(hjust=0.5))

hispanic_anova = aov(bmi ~ interview_year, data=hispanic)
xkabledply(hispanic_anova)
summary(hispanic_anova)

tukeyhispanic<- TukeyHSD(hispanic_anova) 
tukeyhispanic

##Black##
black = subset(brfss_demo,race_ethnicity %in% 'Black NH')
summary(black)

library("ggplot2")
ggplot(black, aes(x=interview_year, y=bmi)) + 
  geom_boxplot( colour=c("#ff0000","#11cc11","#0000ff","#ff00ff","#ffa500","#551a8b"), outlier.shape=8, outlier.size=4) +
  labs(x="Year", y = "Body Mass Index (BMI)")+
  ggtitle("Body Mass Index (BMI) for Blacks from 2018 - 2023")+
  theme(plot.title = element_text(hjust=0.5))

black_anova = aov(bmi ~ interview_year, data=black)
xkabledply(black_anova)
summary(black_anova)

tukeyblack<- TukeyHSD(black_anova) 
tukeyblack

##Multiracial##
multiracial = subset(brfss_demo,race_ethnicity %in% 'Multiracial NH')
summary(multiracial)

library("ggplot2")
ggplot(multiracial, aes(x=interview_year, y=bmi)) + 
  geom_boxplot( colour=c("#ff0000","#11cc11","#0000ff","#ff00ff","#ffa500","#551a8b"), outlier.shape=8, outlier.size=4) +
  labs(x="Year", y = "Body Mass Index (BMI)")+
  ggtitle("Body Mass Index (BMI) for Multiracial from 2018 - 2023")+
  theme(plot.title = element_text(hjust=0.5))

multiracial_anova = aov(bmi ~ interview_year, data=multiracial)
xkabledply(multiracial_anova)
summary(multiracial_anova)

tukeymultiracial<- TukeyHSD(multiracial_anova) 
tukeymultiracial

##Other##
other = subset(brfss_demo,race_ethnicity %in% 'Other NH')
summary(other)

library("ggplot2")
ggplot(other, aes(x=interview_year, y=bmi)) + 
  geom_boxplot( colour=c("#ff0000","#11cc11","#0000ff","#ff00ff","#ffa500","#551a8b"), outlier.shape=8, outlier.size=4) +
  labs(x="Year", y = "Body Mass Index (BMI)")+
  ggtitle("Body Mass Index (BMI) for Other from 2018 - 2023")+
  theme(plot.title = element_text(hjust=0.5))

other_anova = aov(bmi ~ interview_year, data=other)
xkabledply(other_anova)
summary(other_anova)

tukeyother<- TukeyHSD(other_anova) 
tukeyother
