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
brfss_data <- bind_rows(d1, d2, d3, d4, d5, d6)

# Quick check
head(brfss_data)

# Step 3: Create obesity variable and filter post-COVID period
brfss_postcovid <- brfss_data %>%
  mutate(obese = ifelse(bmi_category == "Obese", 1, 0)) %>%  # 1=Obese, 0=Not Obese
  filter(interview_year >= 2019)

# Step 4: Create chronic conditions variables with correct coding
brfss_postcovid <- brfss_postcovid %>%
  mutate(
    diabetes = ifelse(diabetes_status %in% c("Yes", "Yes, only pregnancy"), 1, 0),
    cardio = ifelse(
      heart_attack_history == "Yes" |
        coronary_hd_history == "Yes" |
        stroke_history == "Yes", 1, 0
    ),
    depression = ifelse(depression_history == "Yes", 1, 0)
  )

# Step 5: Convert chronic conditions to numeric
brfss_postcovid <- brfss_postcovid %>%
  mutate(
    diabetes = as.numeric(diabetes),
    cardio = as.numeric(cardio),
    depression = as.numeric(depression)
  )

# Step 6: Create survey design
brfss_design <- svydesign(
  id = ~psu,
  weights = ~weight_final,
  data = brfss_postcovid
)

# Step 6a: Function to calculate weighted prevalence by obesity status
weighted_prev <- function(var) {
  svyby(
    as.formula(paste0("~", var)),
    ~obese,
    brfss_design,
    svymean,
    na.rm = TRUE
  )
}

# Step 6b: Calculate weighted prevalence for each chronic condition
diabetes_prev <- weighted_prev("diabetes")
cardio_prev <- weighted_prev("cardio")
depression_prev <- weighted_prev("depression")

# Step 6c: Perform survey-weighted chi-square tests
diabetes_chisq <- svychisq(~diabetes + obese, design = brfss_design)
cardio_chisq <- svychisq(~cardio + obese, design = brfss_design)
depression_chisq <- svychisq(~depression + obese, design = brfss_design)

# Step 6d: Combine results into one table
chronic_by_obese <- data.frame(
  Condition = c("Diabetes", "Cardio", "Depression"),
  Non_Obese = c(diabetes_prev[1,2], cardio_prev[1,2], depression_prev[1,2]),
  Obese = c(diabetes_prev[2,2], cardio_prev[2,2], depression_prev[2,2]),
  P_Value = c(diabetes_chisq$p.value, cardio_chisq$p.value, depression_chisq$p.value)
)

# View weighted prevalence table with p-values
chronic_by_obese

# Step 7: Visualize weighted prevalence
chronic_long <- chronic_by_obese %>%
  pivot_longer(cols = c("Non_Obese", "Obese"), names_to = "Obesity_Status", values_to = "Prevalence")

ggplot(chronic_long, aes(x = Condition, y = Prevalence, fill = Obesity_Status)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Weighted Prevalence of Chronic Conditions by Obesity Status",
    x = "Chronic Condition",
    y = "Weighted Prevalence (%)",
    fill = "Obesity Status"
  ) +
  theme_minimal()

# Step 8: Save weighted prevalence table with p-values to CSV
if(!dir.exists("outputs")){
  dir.create("outputs")
}

write.csv(chronic_by_obese, "outputs/chronic_by_obese.csv", row.names = FALSE)
