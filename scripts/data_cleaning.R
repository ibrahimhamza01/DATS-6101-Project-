library(haven)
library(readr)
library(tidyr)
library(dplyr)
library(purrr)

# importing data (mandatory)
brfss_2018 <- read_xpt("data/raw/LLCP2018.XPT")
brfss_2019 <- read_xpt("data/raw/LLCP2019.XPT")
brfss_2020 <- read_xpt("data/raw/LLCP2020.XPT")
brfss_2021 <- read_xpt("data/raw/LLCP2021.XPT")
brfss_2022 <- read_xpt("data/raw/LLCP2022.XPT")
brfss_2023 <- read_xpt("data/raw/LLCP2023.XPT")

# creating smaller view of raw data set (no need to run these)
brfss_2018_sample <- brfss_2018 %>% slice(1:100)
write_csv(brfss_2018_sample, "data/raw/brfss_2018_sample.csv")
brfss_2019_sample <- brfss_2019 %>% slice(1:100)
write_csv(brfss_2019_sample, "data/raw/brfss_2019_sample.csv")
brfss_2020_sample <- brfss_2020 %>% slice(1:100)
write_csv(brfss_2020_sample, "data/raw/brfss_2020_sample.csv")
brfss_2021_sample <- brfss_2021 %>% slice(1:100)
write_csv(brfss_2021_sample, "data/raw/brfss_2021_sample.csv")
brfss_2022_sample <- brfss_2022 %>% slice(1:100)
write_csv(brfss_2022_sample, "data/raw/brfss_2022_sample.csv")
brfss_2023_sample <- brfss_2023 %>% slice(1:100)
write_csv(brfss_2023_sample, "data/raw/brfss_2023_sample.csv")


# 2018 data cleaning (mandatory)

  #renaming columns
  brfss_2018 <- brfss_2018 %>%
    select(
      '_LLCPWT', '_STSTR', '_PSU', 'IYEAR',
      '_BMI5', '_BMI5CAT', '_RFBMI5',
      'EXERANY2', '_TOTINDA', 'DRNK3GE5', 'MAXDRNKS',
      'SEX1', '_AGE80', '_AGEG5YR', '_RACEGR3', 'EDUCA', '_INCOMG', '_STATE',
      'DIABETE3', 'CVDINFR4', 'CVDCRHD4', 'CVDSTRK3', 'ADDEPEV2',
      '_RFHLTH'
    ) %>%
    rename(
      # Survey design
      weight_final       = '_LLCPWT',
      strata             = '_STSTR',
      psu                = '_PSU',
      interview_year     = 'IYEAR',
      
      # BMI / Obesity
      bmi                = '_BMI5',
      bmi_category       = '_BMI5CAT',
      overweight_or_obese = '_RFBMI5',
      
      # Lifestyle behaviors
      any_exercise_last_month = 'EXERANY2',
      exercise_30day         = '_TOTINDA',
      binge_drinking          = 'DRNK3GE5',
      max_drinks_30day        = 'MAXDRNKS',
      
      # Demographics
      sex               = 'SEX1',
      age               = '_AGE80',
      age_group         = '_AGEG5YR',
      race_ethnicity    = '_RACEGR3',
      education_level   = 'EDUCA',
      income_category   = '_INCOMG',
      state_fips        = '_STATE',
      
      # Health conditions
      diabetes_status      = 'DIABETE3',
      heart_attack_history = 'CVDINFR4',
      coronary_hd_history  = 'CVDCRHD4',
      stroke_history       = 'CVDSTRK3',
      depression_history   = 'ADDEPEV2',
      
      # Self-reported health
      self_reported_health = '_RFHLTH'
    )

  #mapping
  brfss_2018 <- brfss_2018 %>%
    mutate(
      # Convert all special codes to NA first
      overweight_or_obese       = ifelse(overweight_or_obese %in% c(9), NA, overweight_or_obese),
      any_exercise_last_month   = ifelse(any_exercise_last_month %in% c(7,9), NA, any_exercise_last_month),
      exercise_30day            = ifelse(exercise_30day %in% c(9), NA, exercise_30day),
      binge_drinking            = ifelse(binge_drinking %in% c(9), NA, binge_drinking),
      sex                       = ifelse(sex %in% c(3,4), NA, sex),
      age_group                 = ifelse(age_group %in% c(14), NA, age_group),
      race_ethnicity            = ifelse(race_ethnicity %in% c(9), NA, race_ethnicity),
      education_level           = ifelse(education_level %in% c(9), NA, education_level),
      income_category           = ifelse(income_category %in% c(9), NA, income_category),
      diabetes_status           = ifelse(diabetes_status %in% c(7,9), NA, diabetes_status),
      heart_attack_history      = ifelse(heart_attack_history %in% c(7,9), NA, heart_attack_history),
      coronary_hd_history       = ifelse(coronary_hd_history %in% c(7,9), NA, coronary_hd_history),
      stroke_history            = ifelse(stroke_history %in% c(7,9), NA, stroke_history),
      depression_history        = ifelse(depression_history %in% c(7,9), NA, depression_history),
      self_reported_health      = ifelse(self_reported_health %in% c(9), NA, self_reported_health),
      max_drinks_30day          = ifelse(max_drinks_30day %in% c(77,99), NA, max_drinks_30day)
    ) %>%
    mutate(
      # Then factorize
      bmi_category          = factor(bmi_category, levels = c(1,2,3,4),
                                     labels = c('Underweight','Normal weight','Overweight','Obese')),
      overweight_or_obese   = factor(overweight_or_obese, levels = c(1,2),
                                     labels = c('No','Yes')),
      any_exercise_last_month = factor(any_exercise_last_month, levels = c(1,2),
                                       labels = c('Yes','No')),
      exercise_30day        = factor(exercise_30day, levels = c(1,2),
                                     labels = c('Had physical activity','No physical activity')),
      binge_drinking        = factor(binge_drinking, levels = c(1,2),
                                     labels = c('No','Yes')),
      sex                   = factor(sex, levels = c(1,2),
                                     labels = c('Male','Female')),
      age_group             = factor(age_group, levels = 1:13,
                                     labels = c('18-24','25-29','30-34','35-39','40-44',
                                                '45-49','50-54','55-59','60-64','65-69',
                                                '70-74','75-79','80 or older')),
      race_ethnicity        = factor(race_ethnicity, levels = 1:5,
                                     labels = c('White NH','Black NH','Other NH','Multiracial NH','Hispanic')),
      education_level       = factor(education_level, levels = 1:6,
                                     labels = c('No school/K','Grades 1-8','Grades 9-11','Grade 12/GED',
                                                'College 1-3 years','College 4+ years')),
      income_category       = factor(income_category, levels = 1:5,
                                     labels = c('<15k','15-25k','25-35k','35-50k','>50k')),
      diabetes_status       = factor(diabetes_status, levels = 1:4,
                                     labels = c('Yes','Yes, only pregnancy','No','Pre-diabetes')),
      heart_attack_history  = factor(heart_attack_history, levels = c(1,2),
                                     labels = c('Yes','No')),
      coronary_hd_history   = factor(coronary_hd_history, levels = c(1,2),
                                     labels = c('Yes','No')),
      stroke_history        = factor(stroke_history, levels = c(1,2),
                                     labels = c('Yes','No')),
      depression_history    = factor(depression_history, levels = c(1,2),
                                     labels = c('Yes','No')),
      self_reported_health  = factor(self_reported_health, levels = c(1,2),
                                     labels = c('Good or Better','Fair or Poor')),
      state_fips = factor(state_fips,
                          levels = c(1,2,4,5,6,8,9,10,11,12,13,15,16,17,18,19,20,21,22,23,24,
                                     25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,44,
                                     45,46,47,48,49,50,51,53,54,55,56,66,72),
                          labels = c("Alabama","Alaska","Arizona","Arkansas","California","Colorado",
                                     "Connecticut","Delaware","District of Columbia","Florida","Georgia",
                                     "Hawaii","Idaho","Illinois","Indiana","Iowa","Kansas","Kentucky",
                                     "Louisiana","Maine","Maryland","Massachusetts","Michigan","Minnesota",
                                     "Mississippi","Missouri","Montana","Nebraska","Nevada","New Hampshire",
                                     "New Jersey","New Mexico","New York","North Carolina","North Dakota",
                                     "Ohio","Oklahoma","Oregon","Pennsylvania","Rhode Island","South Carolina",
                                     "South Dakota","Tennessee","Texas","Utah","Vermont","Virginia","Washington",
                                     "West Virginia","Wisconsin","Wyoming","Guam","Puerto Rico"))
    )

  # Keep only 2018 respondents
  brfss_2018 <- brfss_2018 %>% filter(interview_year == 2018)

  #drop null values
  brfss_2018_clean <- brfss_2018 %>% drop_na()
  summary(brfss_2018_clean)


# 2019 data cleaning (mandatory)

  brfss_2019 <- brfss_2019 %>%
    select(
      '_LLCPWT', '_STSTR', '_PSU', 'IYEAR',
      '_BMI5', '_BMI5CAT', '_RFBMI5',
      'EXERANY2', '_TOTINDA', 'DRNK3GE5', 'MAXDRNKS',
      '_SEX', '_AGE80', '_AGEG5YR', '_RACEGR3', 'EDUCA', '_INCOMG', '_STATE',
      'DIABETE4', 'CVDINFR4', 'CVDCRHD4', 'CVDSTRK3', 'ADDEPEV3',
      '_RFHLTH'
    ) %>%
    rename(
      # Survey design
      weight_final       = '_LLCPWT',
      strata             = '_STSTR',
      psu                = '_PSU',
      interview_year     = 'IYEAR',
      
      # BMI / Obesity
      bmi                = '_BMI5',
      bmi_category       = '_BMI5CAT',
      overweight_or_obese = '_RFBMI5',
      
      # Lifestyle behaviors
      any_exercise_last_month = 'EXERANY2',
      exercise_30day         = '_TOTINDA',
      binge_drinking          = 'DRNK3GE5',
      max_drinks_30day        = 'MAXDRNKS',
      
      # Demographics
      sex               = '_SEX',
      age               = '_AGE80',
      age_group         = '_AGEG5YR',
      race_ethnicity    = '_RACEGR3',
      education_level   = 'EDUCA',
      income_category   = '_INCOMG',
      state_fips        = '_STATE',
      
      # Health conditions
      diabetes_status      = 'DIABETE4',
      heart_attack_history = 'CVDINFR4',
      coronary_hd_history  = 'CVDCRHD4',
      stroke_history       = 'CVDSTRK3',
      depression_history   = 'ADDEPEV3',
      
      # Self-reported health
      self_reported_health = '_RFHLTH'
    ) 

  brfss_2019 <- brfss_2019 %>% 
    mutate(
      # Convert all special codes to NA first
      overweight_or_obese       = ifelse(overweight_or_obese %in% c(9), NA, overweight_or_obese),
      any_exercise_last_month   = ifelse(any_exercise_last_month %in% c(7,9), NA, any_exercise_last_month),
      exercise_30day            = ifelse(exercise_30day %in% c(9), NA, exercise_30day),
      binge_drinking            = ifelse(binge_drinking %in% c(9), NA, binge_drinking),
      sex                       = ifelse(sex %in% c(3,4), NA, sex),
      age_group                 = ifelse(age_group %in% c(14), NA, age_group),
      race_ethnicity            = ifelse(race_ethnicity %in% c(9), NA, race_ethnicity),
      education_level           = ifelse(education_level %in% c(9), NA, education_level),
      income_category           = ifelse(income_category %in% c(9), NA, income_category),
      diabetes_status           = ifelse(diabetes_status %in% c(7,9), NA, diabetes_status),
      heart_attack_history      = ifelse(heart_attack_history %in% c(7,9), NA, heart_attack_history),
      coronary_hd_history       = ifelse(coronary_hd_history %in% c(7,9), NA, coronary_hd_history),
      stroke_history            = ifelse(stroke_history %in% c(7,9), NA, stroke_history),
      depression_history        = ifelse(depression_history %in% c(7,9), NA, depression_history),
      self_reported_health      = ifelse(self_reported_health %in% c(9), NA, self_reported_health),
      max_drinks_30day          = ifelse(max_drinks_30day %in% c(77,99), NA, max_drinks_30day)
    ) %>%
    mutate(
      # Then factorize
      bmi_category          = factor(bmi_category, levels = c(1,2,3,4),
                                     labels = c('Underweight','Normal weight','Overweight','Obese')),
      overweight_or_obese   = factor(overweight_or_obese, levels = c(1,2),
                                     labels = c('No','Yes')),
      any_exercise_last_month = factor(any_exercise_last_month, levels = c(1,2),
                                       labels = c('Yes','No')),
      exercise_30day        = factor(exercise_30day, levels = c(1,2),
                                     labels = c('Had physical activity','No physical activity')),
      binge_drinking        = factor(binge_drinking, levels = c(1,2),
                                     labels = c('No','Yes')),
      sex                   = factor(sex, levels = c(1,2),
                                     labels = c('Male','Female')),
      age_group             = factor(age_group, levels = 1:13,
                                     labels = c('18-24','25-29','30-34','35-39','40-44',
                                                '45-49','50-54','55-59','60-64','65-69',
                                                '70-74','75-79','80 or older')),
      race_ethnicity        = factor(race_ethnicity, levels = 1:5,
                                     labels = c('White NH','Black NH','Other NH','Multiracial NH','Hispanic')),
      education_level       = factor(education_level, levels = 1:6,
                                     labels = c('No school/K','Grades 1-8','Grades 9-11','Grade 12/GED',
                                                'College 1-3 years','College 4+ years')),
      income_category       = factor(income_category, levels = 1:5,
                                     labels = c('<15k','15-25k','25-35k','35-50k','>50k')),
      diabetes_status       = factor(diabetes_status, levels = 1:4,
                                     labels = c('Yes','Yes, only pregnancy','No','Pre-diabetes')),
      heart_attack_history  = factor(heart_attack_history, levels = c(1,2),
                                     labels = c('Yes','No')),
      coronary_hd_history   = factor(coronary_hd_history, levels = c(1,2),
                                     labels = c('Yes','No')),
      stroke_history        = factor(stroke_history, levels = c(1,2),
                                     labels = c('Yes','No')),
      depression_history    = factor(depression_history, levels = c(1,2),
                                     labels = c('Yes','No')),
      self_reported_health  = factor(self_reported_health, levels = c(1,2),
                                     labels = c('Good or Better','Fair or Poor')),
      state_fips = factor(state_fips,
                          levels = c(1,2,4,5,6,8,9,10,11,12,13,15,16,17,18,19,20,21,22,23,24,
                                     25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,44,
                                     45,46,47,48,49,50,51,53,54,55,56,66,72),
                          labels = c("Alabama","Alaska","Arizona","Arkansas","California","Colorado",
                                     "Connecticut","Delaware","District of Columbia","Florida","Georgia",
                                     "Hawaii","Idaho","Illinois","Indiana","Iowa","Kansas","Kentucky",
                                     "Louisiana","Maine","Maryland","Massachusetts","Michigan","Minnesota",
                                     "Mississippi","Missouri","Montana","Nebraska","Nevada","New Hampshire",
                                     "New Jersey","New Mexico","New York","North Carolina","North Dakota",
                                     "Ohio","Oklahoma","Oregon","Pennsylvania","Rhode Island","South Carolina",
                                     "South Dakota","Tennessee","Texas","Utah","Vermont","Virginia","Washington",
                                     "West Virginia","Wisconsin","Wyoming","Guam","Puerto Rico"))
    ) 


  # Keep only 2019 respondents
  brfss_2019 <- brfss_2019 %>% filter(interview_year == 2019)

  #drop null values
  brfss_2019_clean <- brfss_2019 %>% drop_na()
  summary(brfss_2019_clean)



# 2020 data cleaning (mandatory)

  brfss_2020 <- brfss_2020 %>%
    select(
      '_LLCPWT', '_STSTR', '_PSU', 'IYEAR',
      '_BMI5', '_BMI5CAT', '_RFBMI5',
      'EXERANY2', '_TOTINDA', 'DRNK3GE5', 'MAXDRNKS',
      '_SEX', '_AGE80', '_AGEG5YR', '_RACEGR3', 'EDUCA', '_INCOMG', '_STATE',
      'DIABETE4', 'CVDINFR4', 'CVDCRHD4', 'CVDSTRK3', 'ADDEPEV3',
      '_RFHLTH'
    ) %>%
    rename(
      # Survey design
      weight_final       = '_LLCPWT',
      strata             = '_STSTR',
      psu                = '_PSU',
      interview_year     = 'IYEAR',
      
      # BMI / Obesity
      bmi                = '_BMI5',
      bmi_category       = '_BMI5CAT',
      overweight_or_obese = '_RFBMI5',
      
      # Lifestyle behaviors
      any_exercise_last_month = 'EXERANY2',
      exercise_30day         = '_TOTINDA',
      binge_drinking          = 'DRNK3GE5',
      max_drinks_30day        = 'MAXDRNKS',
      
      # Demographics
      sex               = '_SEX',
      age               = '_AGE80',
      age_group         = '_AGEG5YR',
      race_ethnicity    = '_RACEGR3',
      education_level   = 'EDUCA',
      income_category   = '_INCOMG',
      state_fips        = '_STATE',
      
      # Health conditions
      diabetes_status      = 'DIABETE4',
      heart_attack_history = 'CVDINFR4',
      coronary_hd_history  = 'CVDCRHD4',
      stroke_history       = 'CVDSTRK3',
      depression_history   = 'ADDEPEV3',
      
      # Self-reported health
      self_reported_health = '_RFHLTH'
    ) 

  brfss_2020 <- brfss_2020 %>% 
    mutate(
      overweight_or_obese       = ifelse(overweight_or_obese %in% c(9), NA, overweight_or_obese),
      any_exercise_last_month   = ifelse(any_exercise_last_month %in% c(7,9), NA, any_exercise_last_month),
      exercise_30day            = ifelse(exercise_30day %in% c(9), NA, exercise_30day),
      binge_drinking            = ifelse(binge_drinking %in% c(9), NA, binge_drinking),
      sex                       = ifelse(sex %in% c(3,4), NA, sex),           # _SEX should be 1/2, but guard anyway
      age_group                 = ifelse(age_group %in% c(14), NA, age_group),
      race_ethnicity            = ifelse(race_ethnicity %in% c(9), NA, race_ethnicity),
      education_level           = ifelse(education_level %in% c(9), NA, education_level),
      income_category           = ifelse(income_category %in% c(9), NA, income_category),
      diabetes_status           = ifelse(diabetes_status %in% c(7,9), NA, diabetes_status),
      heart_attack_history      = ifelse(heart_attack_history %in% c(7,9), NA, heart_attack_history),
      coronary_hd_history       = ifelse(coronary_hd_history %in% c(7,9), NA, coronary_hd_history),
      stroke_history            = ifelse(stroke_history %in% c(7,9), NA, stroke_history),
      depression_history        = ifelse(depression_history %in% c(7,9), NA, depression_history),
      self_reported_health      = ifelse(self_reported_health %in% c(9), NA, self_reported_health),
      max_drinks_30day          = ifelse(max_drinks_30day %in% c(77,99), NA, max_drinks_30day)
    ) %>%
    # 2) then convert to descriptive factors
    mutate(
      bmi_category            = factor(bmi_category, levels = c(1,2,3,4),
                                       labels = c('Underweight','Normal weight','Overweight','Obese')),
      overweight_or_obese     = factor(overweight_or_obese, levels = c(1,2),
                                       labels = c('No','Yes')),
      any_exercise_last_month = factor(any_exercise_last_month, levels = c(1,2),
                                       labels = c('Yes','No')),
      exercise_30day          = factor(exercise_30day, levels = c(1,2),
                                       labels = c('Had physical activity','No physical activity')),
      binge_drinking          = factor(binge_drinking, levels = c(1,2),
                                       labels = c('No','Yes')),
      sex                     = factor(sex, levels = c(1,2),
                                       labels = c('Male','Female')),
      age_group               = factor(age_group, levels = 1:13,
                                       labels = c('18-24','25-29','30-34','35-39','40-44',
                                                  '45-49','50-54','55-59','60-64','65-69',
                                                  '70-74','75-79','80 or older')),
      race_ethnicity          = factor(race_ethnicity, levels = 1:5,
                                       labels = c('White NH','Black NH','Other NH','Multiracial NH','Hispanic')),
      education_level         = factor(education_level, levels = 1:6,
                                       labels = c('No school/K','Grades 1-8','Grades 9-11','Grade 12/GED',
                                                  'College 1-3 years','College 4+ years')),
      income_category         = factor(income_category, levels = 1:5,
                                       labels = c('<15k','15-25k','25-35k','35-50k','>50k')),
      diabetes_status         = factor(diabetes_status, levels = 1:4,
                                       labels = c('Yes','Yes, only pregnancy','No','Pre-diabetes')),
      heart_attack_history    = factor(heart_attack_history, levels = c(1,2),
                                       labels = c('Yes','No')),
      coronary_hd_history     = factor(coronary_hd_history, levels = c(1,2),
                                       labels = c('Yes','No')),
      stroke_history          = factor(stroke_history, levels = c(1,2),
                                       labels = c('Yes','No')),
      depression_history      = factor(depression_history, levels = c(1,2),
                                       labels = c('Yes','No')),
      self_reported_health    = factor(self_reported_health, levels = c(1,2),
                                       labels = c('Good or Better','Fair or Poor')),
      state_fips = factor(state_fips,
                          levels = c(1,2,4,5,6,8,9,10,11,12,13,15,16,17,18,19,20,21,22,23,24,
                                     25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,44,
                                     45,46,47,48,49,50,51,53,54,55,56,66,72),
                          labels = c("Alabama","Alaska","Arizona","Arkansas","California","Colorado",
                                     "Connecticut","Delaware","District of Columbia","Florida","Georgia",
                                     "Hawaii","Idaho","Illinois","Indiana","Iowa","Kansas","Kentucky",
                                     "Louisiana","Maine","Maryland","Massachusetts","Michigan","Minnesota",
                                     "Mississippi","Missouri","Montana","Nebraska","Nevada","New Hampshire",
                                     "New Jersey","New Mexico","New York","North Carolina","North Dakota",
                                     "Ohio","Oklahoma","Oregon","Pennsylvania","Rhode Island","South Carolina",
                                     "South Dakota","Tennessee","Texas","Utah","Vermont","Virginia","Washington",
                                     "West Virginia","Wisconsin","Wyoming","Guam","Puerto Rico"))
    ) 

  # Keep only 2020 respondents
  brfss_2020 <- brfss_2020 %>% filter(interview_year == 2020)

  #drop null values
  brfss_2020_clean <- brfss_2020 %>% drop_na()
  summary(brfss_2020_clean)


# 2021 data cleaning (mandatory)

  brfss_2021 <- brfss_2021 %>%
    select(
      '_LLCPWT', '_STSTR', '_PSU', 'IYEAR',
      '_BMI5', '_BMI5CAT', '_RFBMI5',
      'EXERANY2', '_TOTINDA', 'DRNK3GE5', 'MAXDRNKS',
      '_SEX', '_AGE80', '_AGEG5YR', '_RACEGR3', 'EDUCA', '_INCOMG1', '_STATE',
      'DIABETE4', 'CVDINFR4', 'CVDCRHD4', 'CVDSTRK3', 'ADDEPEV3',
      '_RFHLTH'
    ) %>%
    rename(
      # Survey design
      weight_final       = '_LLCPWT',
      strata             = '_STSTR',
      psu                = '_PSU',
      interview_year     = 'IYEAR',
      
      # BMI / Obesity
      bmi                = '_BMI5',
      bmi_category       = '_BMI5CAT',
      overweight_or_obese = '_RFBMI5',
      
      # Lifestyle behaviors
      any_exercise_last_month = 'EXERANY2',
      exercise_30day         = '_TOTINDA',
      binge_drinking          = 'DRNK3GE5',
      max_drinks_30day        = 'MAXDRNKS',
      
      # Demographics
      sex               = '_SEX',
      age               = '_AGE80',
      age_group         = '_AGEG5YR',
      race_ethnicity    = '_RACEGR3',
      education_level   = 'EDUCA',
      income_category   = '_INCOMG1',
      state_fips        = '_STATE',
      
      # Health conditions
      diabetes_status      = 'DIABETE4',
      heart_attack_history = 'CVDINFR4',
      coronary_hd_history  = 'CVDCRHD4',
      stroke_history       = 'CVDSTRK3',
      depression_history   = 'ADDEPEV3',
      
      # Self-reported health
      self_reported_health = '_RFHLTH'
    ) 

  brfss_2021 <- brfss_2021 %>%
    mutate(
      overweight_or_obese       = ifelse(overweight_or_obese %in% c(9), NA, overweight_or_obese),
      any_exercise_last_month   = ifelse(any_exercise_last_month %in% c(7,9), NA, any_exercise_last_month),
      exercise_30day            = ifelse(exercise_30day %in% c(9), NA, exercise_30day),
      binge_drinking            = ifelse(binge_drinking %in% c(9), NA, binge_drinking),
      sex                       = ifelse(sex %in% c(3,4), NA, sex),              # guard though _SEX usually 1/2
      age_group                 = ifelse(age_group %in% c(14), NA, age_group),
      race_ethnicity            = ifelse(race_ethnicity %in% c(9), NA, race_ethnicity),
      education_level           = ifelse(education_level %in% c(9), NA, education_level),
      income_category           = ifelse(income_category %in% c(9), NA,
                                         ifelse(income_category %in% c(5,6,7), 5, income_category)),
      diabetes_status           = ifelse(diabetes_status %in% c(7,9), NA, diabetes_status),
      heart_attack_history      = ifelse(heart_attack_history %in% c(7,9), NA, heart_attack_history),
      coronary_hd_history       = ifelse(coronary_hd_history %in% c(7,9), NA, coronary_hd_history),
      stroke_history            = ifelse(stroke_history %in% c(7,9), NA, stroke_history),
      depression_history        = ifelse(depression_history %in% c(7,9), NA, depression_history),
      self_reported_health      = ifelse(self_reported_health %in% c(9), NA, self_reported_health),
      max_drinks_30day          = ifelse(max_drinks_30day %in% c(77,99), NA, max_drinks_30day)
    ) %>%
    # 2) then convert to descriptive factors (do factorization AFTER numeric NA recoding)
    mutate(
      bmi_category            = factor(bmi_category, levels = c(1,2,3,4),
                                       labels = c('Underweight','Normal weight','Overweight','Obese')),
      overweight_or_obese     = factor(overweight_or_obese, levels = c(1,2),
                                       labels = c('No','Yes')),
      any_exercise_last_month = factor(any_exercise_last_month, levels = c(1,2),
                                       labels = c('Yes','No')),
      exercise_30day          = factor(exercise_30day, levels = c(1,2),
                                       labels = c('Had physical activity','No physical activity')),
      binge_drinking          = factor(binge_drinking, levels = c(1,2),
                                       labels = c('No','Yes')),
      sex                     = factor(sex, levels = c(1,2),
                                       labels = c('Male','Female')),
      age_group               = factor(age_group, levels = 1:13,
                                       labels = c('18-24','25-29','30-34','35-39','40-44',
                                                  '45-49','50-54','55-59','60-64','65-69',
                                                  '70-74','75-79','80 or older')),
      race_ethnicity          = factor(race_ethnicity, levels = 1:5,
                                       labels = c('White NH','Black NH','Other NH','Multiracial NH','Hispanic')),
      education_level         = factor(education_level, levels = 1:6,
                                       labels = c('No school/K','Grades 1-8','Grades 9-11','Grade 12/GED',
                                                  'College 1-3 years','College 4+ years')),
      income_category         = factor(income_category, levels = 1:5,
                                       labels = c('<15k','15-25k','25-35k','35-50k','>50k')),
      diabetes_status         = factor(diabetes_status, levels = 1:4,
                                       labels = c('Yes','Yes, only pregnancy','No','Pre-diabetes')),
      heart_attack_history    = factor(heart_attack_history, levels = c(1,2),
                                       labels = c('Yes','No')),
      coronary_hd_history     = factor(coronary_hd_history, levels = c(1,2),
                                       labels = c('Yes','No')),
      stroke_history          = factor(stroke_history, levels = c(1,2),
                                       labels = c('Yes','No')),
      depression_history      = factor(depression_history, levels = c(1,2),
                                       labels = c('Yes','No')),
      self_reported_health    = factor(self_reported_health, levels = c(1,2),
                                       labels = c('Good or Better','Fair or Poor')),
      state_fips = factor(state_fips,
                          levels = c(1,2,4,5,6,8,9,10,11,12,13,15,16,17,18,19,20,21,22,23,24,
                                     25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,44,
                                     45,46,47,48,49,50,51,53,54,55,56,66,72),
                          labels = c("Alabama","Alaska","Arizona","Arkansas","California","Colorado",
                                     "Connecticut","Delaware","District of Columbia","Florida","Georgia",
                                     "Hawaii","Idaho","Illinois","Indiana","Iowa","Kansas","Kentucky",
                                     "Louisiana","Maine","Maryland","Massachusetts","Michigan","Minnesota",
                                     "Mississippi","Missouri","Montana","Nebraska","Nevada","New Hampshire",
                                     "New Jersey","New Mexico","New York","North Carolina","North Dakota",
                                     "Ohio","Oklahoma","Oregon","Pennsylvania","Rhode Island","South Carolina",
                                     "South Dakota","Tennessee","Texas","Utah","Vermont","Virginia","Washington",
                                     "West Virginia","Wisconsin","Wyoming","Guam","Puerto Rico"))
    ) 

  # Keep only 2021 respondents
  brfss_2021 <- brfss_2021 %>% filter(interview_year == 2021)

  #drop null values
  brfss_2021_clean <- brfss_2021 %>% drop_na()
  summary(brfss_2021_clean)


# 2022 data cleaning (mandatory)

  brfss_2022 <- brfss_2022 %>%
    select(
      '_LLCPWT', '_STSTR', '_PSU', 'IYEAR',
      '_BMI5', '_BMI5CAT', '_RFBMI5',
      'EXERANY2', '_TOTINDA', 'DRNK3GE5', 'MAXDRNKS',
      '_SEX', '_AGE80', '_AGEG5YR', '_RACEGR4', 'EDUCA', '_INCOMG1', '_STATE',
      'DIABETE4', 'CVDINFR4', 'CVDCRHD4', 'CVDSTRK3', 'ADDEPEV3',
      '_RFHLTH'
    ) %>%
    rename(
      # Survey design
      weight_final       = '_LLCPWT',
      strata             = '_STSTR',
      psu                = '_PSU',
      interview_year     = 'IYEAR',
      
      # BMI / Obesity
      bmi                = '_BMI5',
      bmi_category       = '_BMI5CAT',
      overweight_or_obese = '_RFBMI5',
      
      # Lifestyle behaviors
      any_exercise_last_month = 'EXERANY2',
      exercise_30day         = '_TOTINDA',
      binge_drinking          = 'DRNK3GE5',
      max_drinks_30day        = 'MAXDRNKS',
      
      # Demographics
      sex               = '_SEX',
      age               = '_AGE80',
      age_group         = '_AGEG5YR',
      race_ethnicity    = '_RACEGR4',
      education_level   = 'EDUCA',
      income_category   = '_INCOMG1',
      state_fips        = '_STATE',
      
      # Health conditions
      diabetes_status      = 'DIABETE4',
      heart_attack_history = 'CVDINFR4',
      coronary_hd_history  = 'CVDCRHD4',
      stroke_history       = 'CVDSTRK3',
      depression_history   = 'ADDEPEV3',
      
      # Self-reported health
      self_reported_health = '_RFHLTH'
    ) 

  brfss_2022 <- brfss_2022 %>%
    # 1) convert special codes to NA (numeric) and harmonize income (5/6/7 -> 5)
    mutate(
      overweight_or_obese       = ifelse(overweight_or_obese %in% c(9), NA, overweight_or_obese),
      any_exercise_last_month   = ifelse(any_exercise_last_month %in% c(7,9), NA, any_exercise_last_month),
      exercise_30day            = ifelse(exercise_30day %in% c(9), NA, exercise_30day),
      binge_drinking            = ifelse(binge_drinking %in% c(9), NA, binge_drinking),
      sex                       = ifelse(sex %in% c(3,4), NA, sex),                # guard though _SEX usually 1/2
      age_group                 = ifelse(age_group %in% c(14), NA, age_group),
      race_ethnicity            = ifelse(race_ethnicity %in% c(9), NA, race_ethnicity),
      education_level           = ifelse(education_level %in% c(9), NA, education_level),
      income_category           = ifelse(income_category %in% c(9), NA,
                                         ifelse(income_category %in% c(5,6,7), 5, income_category)),
      diabetes_status           = ifelse(diabetes_status %in% c(7,9), NA, diabetes_status),
      heart_attack_history      = ifelse(heart_attack_history %in% c(7,9), NA, heart_attack_history),
      coronary_hd_history       = ifelse(coronary_hd_history %in% c(7,9), NA, coronary_hd_history),
      stroke_history            = ifelse(stroke_history %in% c(7,9), NA, stroke_history),
      depression_history        = ifelse(depression_history %in% c(7,9), NA, depression_history),
      self_reported_health      = ifelse(self_reported_health %in% c(9), NA, self_reported_health),
      max_drinks_30day          = ifelse(max_drinks_30day %in% c(77,99), NA, max_drinks_30day)
    ) %>%
    # 2) then convert to descriptive factors (do factorization AFTER numeric NA recoding)
    mutate(
      bmi_category            = factor(bmi_category, levels = c(1,2,3,4),
                                       labels = c('Underweight','Normal weight','Overweight','Obese')),
      overweight_or_obese     = factor(overweight_or_obese, levels = c(1,2),
                                       labels = c('No','Yes')),
      any_exercise_last_month = factor(any_exercise_last_month, levels = c(1,2),
                                       labels = c('Yes','No')),
      exercise_30day          = factor(exercise_30day, levels = c(1,2),
                                       labels = c('Had physical activity','No physical activity')),
      binge_drinking          = factor(binge_drinking, levels = c(1,2),
                                       labels = c('No','Yes')),
      sex                     = factor(sex, levels = c(1,2),
                                       labels = c('Male','Female')),
      age_group               = factor(age_group, levels = 1:13,
                                       labels = c('18-24','25-29','30-34','35-39','40-44',
                                                  '45-49','50-54','55-59','60-64','65-69',
                                                  '70-74','75-79','80 or older')),
      race_ethnicity          = factor(race_ethnicity, levels = 1:5,
                                       labels = c('White NH','Black NH','Other NH','Multiracial NH','Hispanic')),
      education_level         = factor(education_level, levels = 1:6,
                                       labels = c('No school/K','Grades 1-8','Grades 9-11','Grade 12/GED',
                                                  'College 1-3 years','College 4+ years')),
      income_category         = factor(income_category, levels = 1:5,
                                       labels = c('<15k','15-25k','25-35k','35-50k','>50k')),
      diabetes_status         = factor(diabetes_status, levels = 1:4,
                                       labels = c('Yes','Yes, only pregnancy','No','Pre-diabetes')),
      heart_attack_history    = factor(heart_attack_history, levels = c(1,2),
                                       labels = c('Yes','No')),
      coronary_hd_history     = factor(coronary_hd_history, levels = c(1,2),
                                       labels = c('Yes','No')),
      stroke_history          = factor(stroke_history, levels = c(1,2),
                                       labels = c('Yes','No')),
      depression_history      = factor(depression_history, levels = c(1,2),
                                       labels = c('Yes','No')),
      self_reported_health    = factor(self_reported_health, levels = c(1,2),
                                       labels = c('Good or Better','Fair or Poor')),
      state_fips = factor(state_fips,
                          levels = c(1,2,4,5,6,8,9,10,11,12,13,15,16,17,18,19,20,21,22,23,24,
                                     25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,44,
                                     45,46,47,48,49,50,51,53,54,55,56,66,72),
                          labels = c("Alabama","Alaska","Arizona","Arkansas","California","Colorado",
                                     "Connecticut","Delaware","District of Columbia","Florida","Georgia",
                                     "Hawaii","Idaho","Illinois","Indiana","Iowa","Kansas","Kentucky",
                                     "Louisiana","Maine","Maryland","Massachusetts","Michigan","Minnesota",
                                     "Mississippi","Missouri","Montana","Nebraska","Nevada","New Hampshire",
                                     "New Jersey","New Mexico","New York","North Carolina","North Dakota",
                                     "Ohio","Oklahoma","Oregon","Pennsylvania","Rhode Island","South Carolina",
                                     "South Dakota","Tennessee","Texas","Utah","Vermont","Virginia","Washington",
                                     "West Virginia","Wisconsin","Wyoming","Guam","Puerto Rico"))
    ) 

  # Keep only 2022 respondents
  brfss_2022 <- brfss_2022 %>% filter(interview_year == 2022)

  #drop null values
  brfss_2022_clean <- brfss_2022 %>% drop_na()
  summary(brfss_2022_clean)


# 2023 data cleaning (mandatory)

  brfss_2023 <- brfss_2023 %>%
    select(
      '_LLCPWT', '_STSTR', '_PSU', 'IYEAR',
      '_BMI5', '_BMI5CAT', '_RFBMI5',
      'EXERANY2', '_TOTINDA', 'DRNK3GE5', 'MAXDRNKS',
      '_SEX', '_AGE80', '_AGEG5YR', '_RACEGR3', 'EDUCA', '_INCOMG1', '_STATE',
      'DIABETE4', 'CVDINFR4', 'CVDCRHD4', 'CVDSTRK3', 'ADDEPEV3',
      '_RFHLTH'
    ) %>%
    rename(
      # Survey design
      weight_final       = '_LLCPWT',
      strata             = '_STSTR',
      psu                = '_PSU',
      interview_year     = 'IYEAR',
      
      # BMI / Obesity
      bmi                = '_BMI5',
      bmi_category       = '_BMI5CAT',
      overweight_or_obese = '_RFBMI5',
      
      # Lifestyle behaviors
      any_exercise_last_month = 'EXERANY2',
      exercise_30day         = '_TOTINDA',
      binge_drinking          = 'DRNK3GE5',
      max_drinks_30day        = 'MAXDRNKS',
      
      # Demographics
      sex               = '_SEX',
      age               = '_AGE80',
      age_group         = '_AGEG5YR',
      race_ethnicity    = '_RACEGR3',
      education_level   = 'EDUCA',
      income_category   = '_INCOMG1',
      state_fips        = '_STATE',
      
      # Health conditions
      diabetes_status      = 'DIABETE4',
      heart_attack_history = 'CVDINFR4',
      coronary_hd_history  = 'CVDCRHD4',
      stroke_history       = 'CVDSTRK3',
      depression_history   = 'ADDEPEV3',
      
      # Self-reported health
      self_reported_health = '_RFHLTH'
    ) 
  
  brfss_2023 <- brfss_2023 %>%
    # 1) convert special codes to NA (numeric) and harmonize income (5/6/7 -> 5)
    mutate(
      overweight_or_obese       = ifelse(overweight_or_obese %in% c(9), NA, overweight_or_obese),
      any_exercise_last_month   = ifelse(any_exercise_last_month %in% c(7,9), NA, any_exercise_last_month),
      exercise_30day            = ifelse(exercise_30day %in% c(9), NA, exercise_30day),
      binge_drinking            = ifelse(binge_drinking %in% c(9), NA, binge_drinking),
      sex                       = ifelse(sex %in% c(3,4), NA, sex),                # guard though _SEX usually 1/2
      age_group                 = ifelse(age_group %in% c(14), NA, age_group),
      race_ethnicity            = ifelse(race_ethnicity %in% c(9), NA, race_ethnicity),
      education_level           = ifelse(education_level %in% c(9), NA, education_level),
      income_category           = ifelse(income_category %in% c(9), NA,
                                         ifelse(income_category %in% c(5,6,7), 5, income_category)),
      diabetes_status           = ifelse(diabetes_status %in% c(7,9), NA, diabetes_status),
      heart_attack_history      = ifelse(heart_attack_history %in% c(7,9), NA, heart_attack_history),
      coronary_hd_history       = ifelse(coronary_hd_history %in% c(7,9), NA, coronary_hd_history),
      stroke_history            = ifelse(stroke_history %in% c(7,9), NA, stroke_history),
      depression_history        = ifelse(depression_history %in% c(7,9), NA, depression_history),
      self_reported_health      = ifelse(self_reported_health %in% c(9), NA, self_reported_health),
      max_drinks_30day          = ifelse(max_drinks_30day %in% c(77,99), NA, max_drinks_30day)
    ) %>%
    # 2) then convert to descriptive factors (do factorization AFTER numeric NA recoding)
    mutate(
      bmi_category            = factor(bmi_category, levels = c(1,2,3,4),
                                       labels = c('Underweight','Normal weight','Overweight','Obese')),
      overweight_or_obese     = factor(overweight_or_obese, levels = c(1,2),
                                       labels = c('No','Yes')),
      any_exercise_last_month = factor(any_exercise_last_month, levels = c(1,2),
                                       labels = c('Yes','No')),
      exercise_30day          = factor(exercise_30day, levels = c(1,2),
                                       labels = c('Had physical activity','No physical activity')),
      binge_drinking          = factor(binge_drinking, levels = c(1,2),
                                       labels = c('No','Yes')),
      sex                     = factor(sex, levels = c(1,2),
                                       labels = c('Male','Female')),
      age_group               = factor(age_group, levels = 1:13,
                                       labels = c('18-24','25-29','30-34','35-39','40-44',
                                                  '45-49','50-54','55-59','60-64','65-69',
                                                  '70-74','75-79','80 or older')),
      race_ethnicity          = factor(race_ethnicity, levels = 1:5,
                                       labels = c('White NH','Black NH','Other NH','Multiracial NH','Hispanic')),
      education_level         = factor(education_level, levels = 1:6,
                                       labels = c('No school/K','Grades 1-8','Grades 9-11','Grade 12/GED',
                                                  'College 1-3 years','College 4+ years')),
      income_category         = factor(income_category, levels = 1:5,
                                       labels = c('<15k','15-25k','25-35k','35-50k','>50k')),
      diabetes_status         = factor(diabetes_status, levels = 1:4,
                                       labels = c('Yes','Yes, only pregnancy','No','Pre-diabetes')),
      heart_attack_history    = factor(heart_attack_history, levels = c(1,2),
                                       labels = c('Yes','No')),
      coronary_hd_history     = factor(coronary_hd_history, levels = c(1,2),
                                       labels = c('Yes','No')),
      stroke_history          = factor(stroke_history, levels = c(1,2),
                                       labels = c('Yes','No')),
      depression_history      = factor(depression_history, levels = c(1,2),
                                       labels = c('Yes','No')),
      self_reported_health    = factor(self_reported_health, levels = c(1,2),
                                       labels = c('Good or Better','Fair or Poor')),
      state_fips = factor(state_fips,
                          levels = c(1,2,4,5,6,8,9,10,11,12,13,15,16,17,18,19,20,21,22,23,24,
                                     25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,44,
                                     45,46,47,48,49,50,51,53,54,55,56,66,72),
                          labels = c("Alabama","Alaska","Arizona","Arkansas","California","Colorado",
                                     "Connecticut","Delaware","District of Columbia","Florida","Georgia",
                                     "Hawaii","Idaho","Illinois","Indiana","Iowa","Kansas","Kentucky",
                                     "Louisiana","Maine","Maryland","Massachusetts","Michigan","Minnesota",
                                     "Mississippi","Missouri","Montana","Nebraska","Nevada","New Hampshire",
                                     "New Jersey","New Mexico","New York","North Carolina","North Dakota",
                                     "Ohio","Oklahoma","Oregon","Pennsylvania","Rhode Island","South Carolina",
                                     "South Dakota","Tennessee","Texas","Utah","Vermont","Virginia","Washington",
                                     "West Virginia","Wisconsin","Wyoming","Guam","Puerto Rico"))
    ) 

  # Keep only 2023 respondents
  brfss_2023 <- brfss_2023 %>% filter(interview_year == 2023)
  
  #drop null values
  brfss_2023_clean <- brfss_2023 %>% drop_na()
  summary(brfss_2023_clean)

  
# save clean data set (mandatory)
write_csv(brfss_2018_clean, "data/processed/brfss_2018_clean.csv")
write_csv(brfss_2019_clean, "data/processed/brfss_2019_clean.csv")
write_csv(brfss_2020_clean, "data/processed/brfss_2020_clean.csv")
write_csv(brfss_2021_clean, "data/processed/brfss_2021_clean.csv")
write_csv(brfss_2022_clean, "data/processed/brfss_2022_clean.csv")
write_csv(brfss_2023_clean, "data/processed/brfss_2023_clean.csv")


# creating smaller random sample of clean data set (no need to run these)
set.seed(123)
brfss_2018_clean_sample <- brfss_2018_clean %>% slice_sample(n = 100)
write_csv(brfss_2018_clean_sample, "data/processed/brfss_2018_clean_sample.csv")
brfss_2019_clean_sample <- brfss_2019_clean %>% slice_sample(n = 100)
write_csv(brfss_2019_clean_sample, "data/processed/brfss_2019_clean_sample.csv")
brfss_2020_clean_sample <- brfss_2020_clean %>% slice_sample(n = 100)
write_csv(brfss_2020_clean_sample, "data/processed/brfss_2020_clean_sample.csv")
brfss_2021_clean_sample <- brfss_2021_clean %>% slice_sample(n = 100)
write_csv(brfss_2021_clean_sample, "data/processed/brfss_2021_clean_sample.csv")
brfss_2022_clean_sample <- brfss_2022_clean %>% slice_sample(n = 100)
write_csv(brfss_2022_clean_sample, "data/processed/brfss_2022_clean_sample.csv")
brfss_2023_clean_sample <- brfss_2023_clean %>% slice_sample(n = 100)
write_csv(brfss_2023_clean_sample, "data/processed/brfss_2023_clean_sample.csv")

# merging all data into one file (optional)
files <- list.files("data/processed/", pattern = "brfss_20.*_clean.csv", full.names = TRUE)
brfss_all <- purrr::map_dfr(files, read.csv)
brfss_all <- brfss_all %>%
  mutate(
    bmi = bmi / 100
  )
write.csv(brfss_all, "data/processed/brfss_2018_2023.csv", row.names = FALSE)
summary(brfss_all)