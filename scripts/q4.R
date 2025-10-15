# Step 1: Load packages
library(dplyr)
library(survey)
library(ggplot2)
library(tidyr)
library(scales)

# Step 2: Read data
brfss_data <- readRDS("C:/Users/namitha/Downloads/brfss_2018_2023.rds")
brfss_data
# Step 3: Filter for post-COVID years (2021–2023)
brfss_postcovid <- brfss_data %>%
  filter(interview_year >= 2021)

# Step 4: Create derived variables
brfss_postcovid <- brfss_postcovid %>%
  mutate(
    obese = ifelse(bmi_category == "Obese", 1, 0),
    diabetes = ifelse(diabetes_status %in% c("Yes", "Yes, only pregnancy"), 1, 0),
    cardio = ifelse(heart_attack_history == "Yes" | coronary_hd_history == "Yes", 1, 0),
    stroke = ifelse(stroke_history == "Yes", 1, 0),
    depression = ifelse(depression_history == "Yes", 1, 0)
  )

# Step 5: Define survey design (account for strata, PSU, and weights)
options(survey.lonely.psu = "adjust")  # handle single-PSU strata
brfss_design <- svydesign(
  id = ~psu,
  strata = ~strata,
  weights = ~weight_final,
  data = brfss_postcovid,
  nest = TRUE
)

# Step 6: Helper function to calculate weighted prevalence
weighted_prev <- function(var) {
  svyby(
    as.formula(paste0("~", var)),
    ~obese,
    brfss_design,
    svymean,
    na.rm = TRUE
  )
}

# Step 7: Calculate weighted prevalence for each chronic condition
diabetes_prev <- weighted_prev("diabetes")
cardio_prev <- weighted_prev("cardio")
stroke_prev <- weighted_prev("stroke")
depression_prev <- weighted_prev("depression")

# Step 8: Chi-square tests
diabetes_chisq <- svychisq(~diabetes + obese, design = brfss_design)
cardio_chisq <- svychisq(~cardio + obese, design = brfss_design)
stroke_chisq <- svychisq(~stroke + obese, design = brfss_design)
depression_chisq <- svychisq(~depression + obese, design = brfss_design)

# Step 9: Combine results into one summary table
chronic_by_obese <- data.frame(
  Condition = c("Diabetes", "Cardiovascular Disease", "Stroke", "Depression"),
  Non_Obese = c(diabetes_prev[1,2], cardio_prev[1,2], stroke_prev[1,2], depression_prev[1,2]),
  Obese = c(diabetes_prev[2,2], cardio_prev[2,2], stroke_prev[2,2], depression_prev[2,2]),
  P_Value = c(diabetes_chisq$p.value, cardio_chisq$p.value, stroke_chisq$p.value, depression_chisq$p.value)
)

# Step 10: Convert results to long format for plotting
chronic_long <- chronic_by_obese %>%
  pivot_longer(cols = c("Non_Obese", "Obese"), names_to = "Obesity_Status", values_to = "Prevalence")

# Step 11: Plot weighted prevalence
ggplot(chronic_long, aes(x = Condition, y = Prevalence, fill = Obesity_Status)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Weighted Prevalence of Chronic Conditions by Obesity Status (Post-COVID: 2021–2023)",
    x = "Chronic Condition",
    y = "Weighted Prevalence (%)",
    fill = "Obesity Status"
  ) +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

# Step 12: Save results
if (!dir.exists("outputs")) dir.create("outputs")
write.csv(chronic_by_obese, "outputs/chronic_by_obese_postcovid.csv", row.names = FALSE)

# Step 13: View final summary
chronic_by_obese
