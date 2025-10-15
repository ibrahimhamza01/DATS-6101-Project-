# Step 1: Load required libraries
library(dplyr)
library(survey)
library(ggplot2)

# Step 2: Read the combined RDS dataset
brfss_all <- readRDS("C:/Users/namitha/Downloads/brfss_2018_2023.rds")

# Step 3: Inspect dataset
glimpse(brfss_all)
table(brfss_all$interview_year)
head(brfss_all)

# Step 4: Define survey design
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

# Step 5: Calculate weighted prevalence of overweight/obesity by year
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

# Step 6: Visualize prevalence trends
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
