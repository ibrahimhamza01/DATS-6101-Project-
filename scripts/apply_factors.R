library(tidyverse)

# Helper: safely convert variable to factor when source may be numeric-coded or already labelled text
safe_factor <- function(x, code_levels = NULL, labels = NULL, ordered = FALSE) {
  # Try to coerce to numeric codes where appropriate
  x_num <- suppressWarnings(as.numeric(as.character(x)))
  use_numeric <- !all(is.na(x_num))
  if (!is.null(code_levels) && !is.null(labels)) {
    if (use_numeric) {
      # ensure code_levels numeric
      code_levels_num <- as.numeric(code_levels)
      return(forcats::fct_relevel(factor(x_num, levels = code_levels_num, labels = labels, ordered = ordered), labels))
    } else {
      # if values are already the label strings, coerce into factor with provided labels (keep order)
      return(forcats::fct_relevel(factor(as.character(x), levels = labels), labels))
    }
  } else {
    # fallback: just factor the variable preserving current unique values
    if (ordered) return(factor(x, ordered = TRUE))
    return(factor(x))
  }
}

# load CSV
brfss <- readr::read_csv("data/processed/brfss_2018_2023.csv", show_col_types = FALSE)

# Apply factor mappings (based on your data_cleaning.R)
brfss <- brfss %>%
  mutate(
    # Survey design variables left as-is (numeric)
    # weight_final, strata, psu, interview_year, bmi are kept numeric
    
    # BMI / Obesity
    bmi_category = safe_factor(bmi_category, code_levels = c(1,2,3,4),
                               labels = c('Underweight','Normal weight','Overweight','Obese')),
    overweight_or_obese = safe_factor(overweight_or_obese, code_levels = c(1,2),
                                      labels = c('No','Yes')),
    
    # Lifestyle behaviors
    any_exercise_last_month = safe_factor(any_exercise_last_month, code_levels = c(1,2),
                                          labels = c('Yes','No')),
    exercise_30day = safe_factor(exercise_30day, code_levels = c(1,2),
                                 labels = c('Had physical activity','No physical activity')),
    binge_drinking = safe_factor(binge_drinking, code_levels = c(1,2),
                                 labels = c('No','Yes')),
    # max_drinks_30day left numeric
    
    # Demographics
    sex = safe_factor(sex, code_levels = c(1,2), labels = c('Male','Female')),
    age_group = safe_factor(age_group, code_levels = 1:13,
                            labels = c('18-24','25-29','30-34','35-39','40-44',
                                       '45-49','50-54','55-59','60-64','65-69',
                                       '70-74','75-79','80 or older'),
                            ordered = TRUE),
    race_ethnicity = safe_factor(race_ethnicity, code_levels = 1:5,
                                 labels = c('White NH','Black NH','Other NH','Multiracial NH','Hispanic')),
    education_level = safe_factor(education_level, code_levels = 1:6,
                                  labels = c('No school/K','Grades 1-8','Grades 9-11','Grade 12/GED',
                                             'College 1-3 years','College 4+ years'),
                                  ordered = TRUE),
    income_category = safe_factor(income_category, code_levels = 1:5,
                                  labels = c('<15k','15-25k','25-35k','35-50k','>50k'),
                                  ordered = TRUE),
    state_fips = safe_factor(state_fips,
                             code_levels = c(1,2,4,5,6,8,9,10,11,12,13,15,16,17,18,19,20,21,22,23,24,
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
  ) %>%
  mutate(
    # Health conditions
    diabetes_status = safe_factor(diabetes_status, code_levels = c(1,2,3,4),
                                  labels = c('Yes','Yes, only pregnancy','No','Pre-diabetes')),
    heart_attack_history = safe_factor(heart_attack_history, code_levels = c(1,2),
                                       labels = c('Yes','No')),
    coronary_hd_history = safe_factor(coronary_hd_history, code_levels = c(1,2),
                                      labels = c('Yes','No')),
    stroke_history = safe_factor(stroke_history, code_levels = c(1,2),
                                 labels = c('Yes','No')),
    depression_history = safe_factor(depression_history, code_levels = c(1,2),
                                     labels = c('Yes','No')),
    self_reported_health = safe_factor(self_reported_health, code_levels = c(1,2),
                                       labels = c('Good or Better','Fair or Poor'))
  )

# Save canonical RDS so factor metadata are preserved
readr::write_rds(brfss, "data/processed/brfss_2018_2023.rds", compress = "gz")
message("Saved: data/processed/brfss_2018_2023.rds")
summary(brfss)

