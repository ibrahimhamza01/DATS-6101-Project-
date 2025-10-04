# Load packages
library(tidyverse)
library(janitor)
library(ggplot2)
library(scales)
library(gridExtra)
library(grid)
library(skimr)

# Load processed data set
brfss <- readr::read_rds("data/processed/brfss_2018_2023.rds")

# Create output directories
if(!dir.exists("outputs/plots")) dir.create("outputs/plots", recursive = TRUE)
if(!dir.exists("outputs/tables")) dir.create("outputs/tables", recursive = TRUE)

# BMI summary by year
bmi_summary_year <- brfss %>%
  group_by(interview_year) %>%
  summarize(
    mean_bmi = mean(bmi),
    median_bmi = median(bmi),
    sd_bmi = sd(bmi),
    min_bmi = min(bmi),
    max_bmi = max(bmi),
    .groups = "drop"
  )

bmi_table_grob <- tableGrob(bmi_summary_year)
png("outputs/tables/bmi_summary_year.png", width = 1200, height = 800)
grid.draw(bmi_table_grob)
dev.off()

# Overweight/Obesity proportion by year
overweight_year <- brfss %>%
  group_by(interview_year, overweight_or_obese) %>%
  summarize(n = n(), .groups = "drop") %>%
  group_by(interview_year) %>%
  mutate(prop = n / sum(n)) %>%
  select(interview_year, overweight_or_obese, prop)

overweight_table_grob <- tableGrob(overweight_year)
png("outputs/tables/overweight_year.png", width = 1200, height = 800)
grid.draw(overweight_table_grob)
dev.off()

# BMI category by sex
bmi_sex <- brfss %>%
  count(sex, bmi_category) %>%
  group_by(sex) %>%
  mutate(prop = n / sum(n)) %>%
  select(sex, bmi_category, prop)

bmi_sex_table_grob <- tableGrob(bmi_sex)
png("outputs/tables/bmi_sex.png", width = 1200, height = 800)
grid.draw(bmi_sex_table_grob)
dev.off()

# Histogram of BMI
p1 <- ggplot(brfss, aes(x = bmi)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "white") +
  labs(title = "Distribution of BMI (2018–2023)", x = "BMI", y = "Count") +
  theme_minimal()
ggsave("outputs/plots/bmi_histogram.png", p1, width = 7, height = 5)

# Mean BMI by year
p2 <- brfss %>%
  group_by(interview_year) %>%
  summarize(mean_bmi = mean(bmi)) %>%
  ggplot(aes(x = interview_year, y = mean_bmi)) +
  geom_line(linewidth = 1.2, color = "#0072B2") +
  geom_point(size = 3, color = "#0072B2") +
  labs(title = "Mean BMI by Year (2018–2023)", x = "Year", y = "Mean BMI") +
  theme_minimal()
ggsave("outputs/plots/mean_bmi_year.png", p2, width = 7, height = 5)

# Overweight/Obesity prevalence by year
p3 <- brfss %>%
  group_by(interview_year, overweight_or_obese) %>%
  summarize(n = n(), .groups = "drop") %>%
  group_by(interview_year) %>%
  mutate(prop = n / sum(n)) %>%
  ggplot(aes(x = interview_year, y = prop, fill = overweight_or_obese)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("No" = "#56B4E9", "Yes" = "#E69F00")) +
  labs(title = "Overweight/Obesity Prevalence by Year", x = "Year", y = "Proportion", fill = "Overweight or Obese") +
  theme_minimal()
ggsave("outputs/plots/overweight_year_plot.png", p3, width = 7, height = 5)

# BMI box plot by year
p4 <- ggplot(brfss, aes(x = factor(interview_year), y = bmi)) +
  geom_boxplot(fill = "lightgreen") +
  labs(title = "BMI Distribution by Year", x = "Year", y = "BMI") +
  theme_minimal()
ggsave("outputs/plots/bmi_box_year.png", p4, width = 7, height = 5)

# Histogram + density overlay for BMI
p5 <- ggplot(brfss, aes(x = bmi)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, fill = "skyblue", color = "white") +
  geom_density(color = "#E69F00", size = 1.2) +
  labs(title = "BMI Distribution (Histogram + Density)", x = "BMI", y = "Density") +
  theme_minimal()
ggsave("outputs/plots/bmi_hist_density.png", p5, width = 7, height = 5)

# Histogram for Age
p6 <- ggplot(brfss, aes(x = age)) +
  geom_histogram(binwidth = 5, fill = "#56B4E9", color = "white") +
  labs(title = "Age Distribution of Respondents", x = "Age", y = "Count") +
  theme_minimal()
ggsave("outputs/plots/age_histogram.png", p6, width = 7, height = 5)

# Box plot for Age by year
p7 <- ggplot(brfss, aes(x = factor(interview_year), y = age)) +
  geom_boxplot(fill = "#009E73") +
  labs(title = "Age Distribution by Year", x = "Year", y = "Age") +
  theme_minimal()
ggsave("outputs/plots/age_box_year.png", p7, width = 7, height = 5)

# Max drinks by BMI category
p8 <- ggplot(brfss, aes(x = bmi_category, y = max_drinks_30day)) +
  geom_boxplot(fill = "#D55E00") +
  labs(title = "Max Drinks by BMI Category", x = "BMI Category", y = "Max Drinks") +
  theme_minimal()
ggsave("outputs/plots/max_drinks_bmi.png", p8, width = 7, height = 5)
