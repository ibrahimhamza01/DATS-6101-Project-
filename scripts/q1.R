# ================================
# Q1: Obesity Prevalence Analysis
# ================================

# Step 1: Load required libraries
library(dplyr)
library(survey)
library(ggplot2)

# Step 2: Read yearly BRFSS datasets
d1 <- read.csv("brfss_2018_clean_sample.csv")
d2 <- read.csv("brfss_2019_clean_sample.csv")
d3 <- read.csv("brfss_2020_clean_sample.csv")
d4 <- read.csv("brfss_2021_clean_sample.csv")
d5 <- read.csv("brfss_2022_clean_sample.csv")
d6 <- read.csv("brfss_2023_clean_sample.csv")

# Step 3: Merge datasets into one dataframe
brfss_all <- bind_rows(d1, d2, d3, d4, d5, d6)

# Step 4: Inspect merged dataset
glimpse(brfss_all)
table(brfss_all$interview_year)
head(brfss_all)

# Step 5: Define survey design
options(survey.lonely.psu = "adjust")  # handle single-PSU strata
brfss_design <- svydesign(
  id = ~psu,
  strata = ~strata,
  weights = ~weight_final,
  data = brfss_all,
  nest = TRUE
)

# Optional: check summary of survey design
print(summary(brfss_design))

# Step 6: Calculate weighted prevalence of overweight/obesity by year
prev_trends <- svyby(
  ~overweight_or_obese,
  ~interview_year,
  brfss_design,
  svymean,
  na.rm = TRUE
)

# Convert to dataframe and rename column
prev_df <- as.data.frame(prev_trends)
names(prev_df)[2] <- "prevalence"
print(prev_df)

# Step 7: Visualize prevalence trends
# Create outputs folder if it doesn't exist
if(!dir.exists("outputs")) dir.create("outputs")

# Assign plot to variable
prev_plot <- ggplot(prev_df, aes(x = interview_year, y = prevalence)) +
  geom_line(size = 1.2, color = "red") +
  geom_point(size = 3, color = "red") +
  labs(
    title = "Prevalence of Overweight or Obesity (2018–2023)",
    y = "Weighted Prevalence",
    x = "Year"
  ) +
  theme_minimal()

# Display the plot
print(prev_plot)

# Save the plot as PNG
ggsave("outputs/overweight_trend.png", plot = prev_plot, width = 7, height = 5)
