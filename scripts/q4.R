# Step 1: Load libraries and read data
library(dplyr)
library(survey)
library(ggplot2)
library(tidyr)

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
