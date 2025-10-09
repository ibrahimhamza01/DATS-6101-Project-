# Step 1: Setup and Load Required Packages
library(dplyr)
library(survey)
library(ggplot2)

# Read individual yearly datasets
d1 <- read.csv("brfss_2018_clean_sample.csv")
d2 <- read.csv("brfss_2019_clean_sample.csv")
d3 <- read.csv("brfss_2020_clean_sample.csv")
d4 <- read.csv("brfss_2021_clean_sample.csv")
d5 <- read.csv("brfss_2022_clean_sample.csv")
d6 <- read.csv("brfss_2023_clean_sample.csv")

# Merge datasets into one dataframe
brfss_all <- bind_rows(d1, d2, d3, d4, d5, d6)

# Inspect merged dataset
glimpse(brfss_all)
table(brfss_all$interview_year)

# Quick check that merge worked
head(brfss_all)

#Step 3: Define survey design for weighted analysis
print(summary(brfss_design))
options(survey.lonely.psu = "adjust")  # Handle single-PSU strata
brfss_design <- svydesign(
  id = ~psu,
  strata = ~strata,
  weights = ~weight_final,
  data = brfss_all,
  nest = TRUE
)

#Step 4: Calculate weighted prevalence of overweight/obesity by year
print(prev_df)
prev_trends <- svyby(
  ~overweight_or_obese,
  ~interview_year,
  brfss_design,
  svymean,
  na.rm = TRUE
)

# Convert svyby results to dataframe and rename column
prev_df <- as.data.frame(prev_trends)
names(prev_df)[2] <- "prevalence"
prev_df

# Plot prevalence trends using ggplot2
ggplot(prev_df, aes(x = interview_year, y = prevalence)) +
  geom_line(size = 1.2, color = "red") +
  geom_point(size = 3, color = "red") +
  labs(
    title = "Prevalence of Overweight or Obesity (2018–2023)",
    y = "Weighted Prevalence",
    x = "Year"
  ) +
  theme_minimal()

