library(dplyr)
library(ggplot2)
library(scales)
library(gridExtra)
library(ezids)
library(survey)
library(kableExtra)
library(stats)

if (!dir.exists("outputs")) dir.create("outputs")

# Function to create a small stratified sample by year
create_small_brfss <- function(data, year_col = "interview_year", n_per_year = 2000, seed = 2025) {
  set.seed(seed)  # for reproducibility
  data %>%
    group_by(.data[[year_col]]) %>%
    slice_sample(n = n_per_year) %>%
    ungroup()
}

brfss <- create_small_brfss(readRDS('data/processed/brfss_2018_2023.rds'), year_col = "interview_year", n_per_year = 2000)
brfss$interview_year <- factor(brfss$interview_year)


# --------------------------
# Plots
# --------------------------

# Overall obesity trend
obesity_trend <- brfss %>%
  group_by(interview_year) %>%
  summarise(obesity_prev = mean(overweight_or_obese == "Yes", na.rm = TRUE), .groups = "drop")

p_trend <- ggplot(obesity_trend, aes(x = interview_year, y = obesity_prev)) +
  geom_line(color = "darkred", size = 1.1) +
  geom_point(size = 3) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(title = "Obesity Prevalence Trend (2018–2023)",
       x = "Year", y = "Proportion Overweight/Obese") +
  theme_minimal(base_size = 14)

ggsave("outputs/q2_obesity_trend.png", p_trend, width = 10, height = 6, dpi = 300)


# Obesity trend by exercise status
obesity_exercise <- brfss %>%
  group_by(interview_year, any_exercise_last_month) %>%
  summarise(obesity_prev = mean(overweight_or_obese == "Yes", na.rm = TRUE), .groups = "drop")

p_ex_tr <- ggplot(obesity_exercise, aes(x = interview_year, y = obesity_prev,
                                        color = any_exercise_last_month, group = any_exercise_last_month)) +
  geom_line(size = 1.1) +
  geom_point(size = 2.5) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(title = "Obesity Trend by Exercise Status (2018–2023)",
       x = "Year", y = "Obesity Prevalence", color = "Exercise") +
  theme_minimal(base_size = 14)

ggsave("outputs/q2_obesity_trend_by_exercise.png", p_ex_tr, width = 10, height = 6, dpi = 300)


# obesity prevalence by exercise
obesity_exercise <- brfss %>%
  group_by(interview_year, any_exercise_last_month) %>%
  summarise(obesity_prev = mean(overweight_or_obese == "Yes", na.rm = TRUE), .groups = "drop")

p_ex_ob <- ggplot(obesity_exercise, aes(x = factor(interview_year), y = obesity_prev, fill = any_exercise_last_month)) +
  geom_bar(stat="identity", position="dodge") +
  scale_y_continuous(labels = percent_format(accuracy=1)) +
  labs(title = "Obesity Prevalence by Exercise Status (2018–2023)", x = "Year", y = "Obesity %", fill = "Exercise") +
  theme_minimal(base_size=14)
ggsave("outputs/q2_obesity_by_exercise_bar.png", p_ex_ob, width = 8, height = 6, dpi = 300)


# Heat map: year × exercise obesity prevalence
p_heat_ex <- ggplot(obesity_exercise, aes(x = factor(interview_year), y = any_exercise_last_month, fill = obesity_prev)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "red", labels = percent_format(accuracy = 1)) +
  labs(title = "Obesity Prevalence Heatmap: Year × Exercise",
       x = "Year", y = "Exercise (Yes/No)", fill = "Obesity %") +
  theme_minimal(base_size = 12)

ggsave("outputs/q2_heatmap_year_exercise.png", p_heat_ex, width = 8, height = 4, dpi = 300)


# Boxplot of BMI by exercise status
summary_exercise <- brfss %>%
  group_by(any_exercise_last_month) %>%
  summarise(
    n = n(),
    mean_bmi = mean(bmi, na.rm = TRUE),
    sd_bmi = sd(bmi, na.rm = TRUE),
    .groups = "drop"
  )

summary_exercise_labels <- summary_exercise %>%
  mutate(label = paste0("n=", n, "\nmean=", round(mean_bmi, 2), "\nsd=", round(sd_bmi, 2)))

p_bmi_ex <- ggplot(brfss, aes(x = any_exercise_last_month, y = bmi, fill = any_exercise_last_month)) +
  geom_boxplot(outlier.size = 0.5, color = "black", alpha = 0.8) +  # colorful fill, smaller outliers
  labs(
    title = "BMI Distribution by Exercise Status",
    x = "Any Exercise Last Month",
    y = "BMI",
    fill = "Exercise"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",        # hide legend (optional)
    plot.title = element_text(face = "bold", hjust = 0.5)
  ) +
  scale_fill_brewer(palette = "Set2")   # choose a nice color palette

p_bmi_ex <- p_bmi_ex +
  geom_text(
    data = summary_exercise_labels,
    aes(
      x = any_exercise_last_month,
      y = max(brfss$bmi, na.rm = TRUE) + 1.5,
      label = label
    ),
    size = 3.0,
    hjust = 1.2
  )
ggsave("outputs/q2_bmi_by_exercise_boxplot.png", p_bmi_ex, width = 8, height = 6, dpi = 300)


# Box plot: BMI by binge drinking
p_bmi_binge <- ggplot(brfss, aes(x = binge_drinking, y = bmi)) +
  geom_boxplot() +
  labs(title = "BMI Distribution by Binge Drinking (Yes/No)", x = "Binge Drinking", y = "BMI") +
  theme_minimal(base_size = 14)

summary_binge <- brfss %>%
  group_by(binge_drinking) %>%
  summarise(n = n(), mean_bmi = mean(bmi, na.rm = TRUE), sd_bmi = sd(bmi, na.rm = TRUE), .groups = "drop")

summary_binge_labels <- summary_binge %>%
  mutate(label = paste0("n=", n, "\nmean=", round(mean_bmi, 2), "\nsd=", round(sd_bmi, 2)))

p_bmi_binge <- p_bmi_binge +
  geom_text(
    data = summary_binge_labels,
    aes(x = binge_drinking, y = max(brfss$bmi, na.rm = TRUE) + 1.5, label = label),
    size = 3.0,
    hjust = 1.2
  )
ggsave("outputs/q2_bmi_by_binge_boxplot.png", p_bmi_binge, width = 8, height = 6, dpi = 300)


options(survey.lonely.psu = "adjust")

brfss_design <- svydesign(
  ids = ~psu,     
  strata = ~strata,
  weights = ~weight_final,
  data = brfss,
  nest = TRUE
)

# --------------------------
# ANOVA tests
# --------------------------

survey_anova_table <- function(svy_model, factor_name) {
  library(survey)
  
  f_test <- regTermTest(svy_model, as.formula(paste0("~", factor_name)))
  
  Df_num <- f_test$df[1]
  F_val  <- f_test$Ftest
  P_val  <- f_test$p
  
  data.frame(
    Term = factor_name,
    Df_num = Df_num,
    F_value = F_val,
    Pr_F = P_val
  )
}

survey_tukey_table_fixed <- function(svy_model, factor_name, conf_level = 0.95, digits = NULL) {
  coefs <- coef(svy_model)
  vc <- vcov(svy_model)
  coef_names <- names(coefs)
  
  mf <- model.frame(svy_model)
  if (!factor_name %in% names(mf)) stop("factor_name not found in model frame")
  levels_factor <- levels(mf[[factor_name]])
  if (is.null(levels_factor) || length(levels_factor) < 2) stop("factor must have >= 2 levels")
  
  alpha <- 1 - conf_level
  zval <- qnorm(1 - alpha/2)
  
  mean_vector_for_level <- function(level) {
    vec <- rep(0, length(coefs))
    names(vec) <- coef_names
    if ("(Intercept)" %in% coef_names) vec["(Intercept)"] <- 1
    ref_level <- levels_factor[1]
    if (level == ref_level) return(vec)
    target_name <- paste0(factor_name, level)
    if (!target_name %in% coef_names) {
      matches <- grep(paste0(factor_name, ".*", level, "$"), coef_names, value = TRUE)
      if (length(matches) == 0) stop("Cannot find coefficient name for level: ", level)
      target_name <- matches[1]
    }
    vec[target_name] <- 1
    return(vec)
  }
  
  pairs <- combn(levels_factor, 2, simplify = FALSE)
  rows <- lapply(pairs, function(pr) {
    l1 <- pr[1]; l2 <- pr[2]
    v1 <- mean_vector_for_level(l1)
    v2 <- mean_vector_for_level(l2)
    contrast <- v2 - v1                 # mean(level2) - mean(level1)
    diff <- as.numeric(sum(coefs * contrast))
    var_diff <- as.numeric(t(contrast) %*% vc %*% contrast)
    se <- sqrt(var_diff)
    lwr <- diff - zval * se
    upr <- diff + zval * se
    pval <- 2 * (1 - pnorm(abs(diff / se)))
    tib <- data.frame(
      Comparison = paste0(l2, " - ", l1),
      diff = diff,
      lwr = lwr,
      upr = upr,
      p_value = pval,
      stringsAsFactors = FALSE
    )
    return(tib)
  })
  
  out <- bind_rows(rows)
  if (!is.null(digits) && is.numeric(digits)) {
    numcols <- setdiff(names(out), "Comparison")
    out[numcols] <- lapply(out[numcols], function(x) round(x, digits))
  }
  rownames(out) <- NULL
  return(out)
}

# Exercise
anova_exercise <- svyglm(bmi ~ any_exercise_last_month, design = brfss_design)
anova_table <- survey_anova_table(anova_exercise, "any_exercise_last_month")
xkabledply(anova_table, title = "Survey Anova Test: BMI vs Exercise")

tukey_fixed <- survey_tukey_table_fixed(anova_exercise, "any_exercise_last_month", conf_level = 0.95, digits = 3)
xkabledply(tukey_fixed, title = "Survey Tukey Test: BMI vs Exercise")

# Alcohol
anova_binge <- svyglm(bmi ~ binge_drinking, design = brfss_design)
anova_table <- survey_anova_table(anova_binge, "binge_drinking")
xkabledply(anova_table, title = "Survey Anova Test: BMI vs Binge drinking")

tukey_fixed <- survey_tukey_table_fixed(anova_binge, "binge_drinking", conf_level = 0.95, digits = 3)
xkabledply(tukey_fixed, title = "Survey Tukey Test: BMI vs Exercise")

# Over the year
anova_year <- svyglm(bmi ~ interview_year, design = brfss_design)
anova_table <- survey_anova_table(anova_year, "interview_year")
xkabledply(anova_table, title = "Survey Anova Test: BMI vs Interview Year")


# --------------------------
# Chi-square tests
# --------------------------

# Function for survey-weighted chi-square
svy_chi_test <- function(var1, var2, design) {
  fml <- as.formula(paste0("~", var1, " + ", var2))
  chisq_svy <- svychisq(fml, design = design, statistic = "Chisq")
  chi_summary <- data.frame(
    Statistic = chisq_svy$statistic,
    DF = chisq_svy$parameter,
    P_Value = chisq_svy$p.value
  )
  chi_summary
}

# bmi_category ~ any_exercise_last_month
result <- svy_chi_test("bmi_category", "any_exercise_last_month", brfss_design)
xkabledply(result, title = "Survey Chi-square Test: BMI vs Exercise")


# bmi_category ~ binge_drinking
result <- svy_chi_test("bmi_category", "binge_drinking", brfss_design)
xkabledply(result, title = "Survey Chi-square Test: BMI vs Binge Drinking")

# bmi_category ~ interview_year
result <- svy_chi_test("bmi_category", "interview_year", brfss_design)
xkabledply(result, title = "Survey Chi-square Test: BMI vs Interview Year")

# binge_drinking ~ interview_year
result <- svy_chi_test("binge_drinking", "interview_year", brfss_design)
xkabledply(result, title = "Survey Chi-square Test: binge_drinking vs Interview Year")

# overweight_or_obese vs binge_drinking
result <- svy_chi_test("overweight_or_obese", "binge_drinking", brfss_design)
xkabledply(result, title = "Survey Chi-square Test: overweight_or_obese vs binge_drinking")

colnames(brfss)

