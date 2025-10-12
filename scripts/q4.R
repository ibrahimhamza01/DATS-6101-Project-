# Step 1: Load libraries and read data
library(dplyr)
library(survey)
library(ggplot2)
library(tidyr)

setwd("C:/Users/namitha/Documents/DATS-6101-Project-/data/processed")

# Step 2: Read yearly BRFSS datasets
d1 <- read.csv("brfss_2018_clean_sample.csv")
d2 <- read.csv("brfss_2019_clean_sample.csv")
d3 <- read.csv("brfss_2020_clean_sample.csv")
d4 <- read.csv("brfss_2021_clean_sample.csv")
d5 <- read.csv("brfss_2022_clean_sample.csv")
d6 <- read.csv("brfss_2023_clean_sample.csv")

# Combine datasets into one dataframe
brfss_data <- bind_rows(d1, d2, d3, d4,d5,d6)

# Quick check
head(brfss_data)

# Step 3: Create obesity variable and filter post-COVID period
brfss_postcovid <- brfss_data %>%
  mutate(obese = ifelse(bmi_category == "Obese", 1, 0)) %>%  # 1=Obese, 0=Not Obese
  filter(interview_year >= 2019)

# Step 4: Create chronic conditions variables with correct names
brfss_postcovid <- brfss_postcovid %>%
  mutate(
    diabetes = ifelse(!is.na(diabetes_status) & diabetes_status == 1, 1, 0),
    cardio = ifelse(!is.na(heart_attack_history) & heart_attack_history == 1 |
                      !is.na(coronary_hd_history) & coronary_hd_history == 1 |
                      !is.na(stroke_history) & stroke_history == 1, 1, 0),
    depression = ifelse(!is.na(depression_history) & depression_history == 1, 1, 0)
  )

# Quick check
head(brfss_postcovid)

# Step 5: Create survey design for weighted analysis
# Using BRFSS weights to account for complex survey design
brfss_design <- svydesign(
  id = ~psu,
  strata = ~strata,
  weights = ~weight_final,
  data = brfss_postcovid,
  nest = TRUE
)



