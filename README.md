# Econ-103-Summer-Project

current version: 
---
title: "Adult Smoking & County Health Outcomes"
author: "Group 22. Maddie Thompson (UID 006460651) & Tommy Nguyen (UID 206756393)"
date: "September 7, 2026"
output:
  pdf_document:
    latex_engine: pdflatex
geometry: margin=1in
fontsize: 11pt
header-includes:
  - \usepackage{float}
  - \usepackage{booktabs}
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(
  echo = FALSE,
  warning = FALSE,
  message = FALSE,
  fig.width = 6,
  fig.height = 4
)
```

```{r packages}
library(readr)
library(dplyr)
library(ggplot2)
library(scales)
library(fixest)      # feols() + etable() for the two-specification table
library(car)         # linearHypothesis() for the joint F-test
library(knitr)
library(kableExtra)  # kbl() for the summary-statistics table

# One accent colour for every fitted line in the report.
fit_color <- "#C1440E"
```

```{r load-clean}
# Task 1: Load and clean ==================================================
# Read the county extract by a relative path. group22_data.csv sits next to
# this .Rmd so the report knits on a grader's machine without setwd().
# group22_build.R is the script that produced it from the raw.

counties_raw <- read_csv("group22_data.csv", show_col_types = FALSE)
n_raw <- nrow(counties_raw)
# 3,152 rows, one per US county

# Drop county rows missing any analysis variable, then build the constructed
# measures. County Health Rankings reports the four rate variables as
# proportions. Every table, axis, and coefficient in this report is
# in PERCENTAGE POINTS, so they are multiplied by 100 once.

# Population and income will enter in logs. Population runs from a few hundred
# people to nearly ten million, and income from under $30k to over $170k, so
# in levels a one-unit step means something completely different at the two
# ends of each range. Section 3 will make that argument with Figures 2 and 3.
counties <- counties_raw |>
  filter(
    !is.na(fair_poor_health), !is.na(adult_smoking), !is.na(median_income),
    !is.na(some_college), !is.na(uninsured), !is.na(population)
  ) |>
  mutate(
    fair_poor_health = fair_poor_health * 100,   # % of adults
    adult_smoking    = adult_smoking * 100,      # % of adults
    some_college     = some_college * 100,       # % of adults 25-44
    uninsured        = uninsured * 100,          # % of under-65 population
    log_income       = log(median_income),       # log dollars
    log_population   = log(population)           # log people
  )

n_clean   <- nrow(counties)
n_dropped <- n_raw - n_clean

# The handout requires at least 200 observations after dropping missing values.
stopifnot(n_clean >= 200)
```

# 1. Introduction 
Do counties with higher smoking rates have worse health outcomes? We answer that with the 2025 County Health Rankings analytic data file, which records one row per US county. After dropping counties missing any variable we use, 3,142 remain, each reporting its share of adults in fair or poor health alongside its smoking rate, median household income, education level, uninsured rate, and population.

Our dependent variable is the **percentage of adults who report that they are in fair or poor health**. Our main explanatory variable is the **county's adult smoking rate**, the percentage of adults who currently smoke. We include four further explanatory variables: **median household income**, the share of **adults aged 25 to 44 with some college**, the **share of under-65s without health insurance**, and **county population**. Each is plausibly associated with both smoking and health, so leaving them out would let the smoking coefficient absorb their effect. **Income and population enter in logs, for reasons set out in Section 3.**

We predict that counties with higher smoking rates have a higher share of adults in fair or poor health. Smoking has a direct and well-documented path to disease, including lung cancer, respiratory illness, and cardiovascular disease, and a county where smoking is common is also likely to differ in other health behaviours that track with worse outcomes. We therefore expect a positive coefficient of roughly 0.7 to 0.85 percentage points of fair or poor health per percentage point of smoking. Much more than that is hard to credit as fair or poor health spans only about 38 percentage points across all counties while smoking spans 32, so a coefficient near or above 1.0 would have smoking accounting for nearly the entire national spread in health on its own. We also expect the coefficient to shrink once income, education, insurance, and population enter, but not to collapse. Counties with similar median incomes smoke at very different rates depending on state tobacco taxes, local norms, and regional history, so income cannot be doing all the work. Additionally, cigarettes do physical damage to the lungs and other body parts in ways that poverty alone does not.

# 2. Data
We use the County Health Rankings & Roadmaps 2025 national analytic data file from countyhealthrankings.org, in which each observation represents a U.S. county in a single year. The raw file carries hundreds of measures; we keep the nine columns this analysis uses. We express the four rate variables in percentage points by multiplying the reported proportions by 100, and we construct the log of median household income and the log of population. After cleaning, 3,142 of 3,152 counties remain, with 10 dropped: two are very small counties (Kalawao, HI, with population 81, and Loving, TX, with population 43), and eight are Connecticut counties, which no longer have health data reported for them. Table 1 shows that fair or poor health averages 19.56% across counties (SD 4.80, range 8.8% to 46.5%), and adult smoking averages 17.96% (SD 3.89, range 5.9% to 38.3%). Population is the lopsided variable, where its mean of 106,593 sits against a standard deviation of 332,667, and it runs from 217 people to over 9.6 million.

```{r summary-table}
# Task 2: Summary-statistics table ========================================
# Mean, SD, min, max, and n for the outcome and every regressor, which is the
# set the rubric asks for. Built as a small data frame first so the numbers
# and their labels stay together in one place.
summary_vars <- tibble(
  Variable = c(
    "Fair or poor health (% of adults)",
    "Adult smoking (% of adults)",
    "Median household income ($)",
    "Some college (% of adults 25-44)",
    "Uninsured (% under 65)",
    "Population"
  ),
  values = list(
    counties$fair_poor_health,
    counties$adult_smoking,
    counties$median_income,
    counties$some_college,
    counties$uninsured,
    counties$population
  )
)

summary_table <- summary_vars |>
  mutate(
    Mean = sapply(values, mean),
    SD   = sapply(values, sd),
    Min  = sapply(values, min),
    Max  = sapply(values, max),
    N    = sapply(values, length)
  ) |>
  select(-values)

kbl(
  summary_table,
  digits = 2,
  format.args = list(big.mark = ","),
  booktabs = TRUE,
  caption = "Counties vary widely in both adult smoking and self-reported health, each spanning more than 30 percentage points, leaving substantial variation to explain.",
  linesep = ""
) |>
  kable_styling(latex_options = "HOLD_position", font_size = 9) |>
  footnote(
    general = paste0("One row per county (n = ", comma(n_clean),
                     "). Data: County Health Rankings & Roadmaps, 2025."),
    general_title = "",
    threeparttable = TRUE,
    footnote_as_chunk = TRUE
  )
```

```{r fig1-smoking, fig.width = 6.5, fig.height = 3.2, fig.align = "center"}
# Task 3: Figure 1, outcome against the main regressor
# Fair or poor health against adult smoking, both in percentage points, with
# the OLS line on top. This is the plot Section 2 reads, the raw
# relationship the whole report is about before any controls enter.
ggplot(counties, aes(x = adult_smoking, y = fair_poor_health)) +
  geom_point(alpha = 0.20, size = 0.8, color = "grey30") +
  geom_smooth(method = "lm", se = FALSE, color = fit_color, linewidth = 1) +
  scale_x_continuous(labels = label_number(suffix = "%")) +
  scale_y_continuous(labels = label_number(suffix = "%")) +
  labs(
    title = "Figure 1: Counties that smoke more tend to report worse health",
    subtitle = "Each extra percentage point of smoking goes with about one more point of adults in fair or poor health, \n                                                      summarizing the positive association",
    x = "Adult smoking rate (% of adults)",
    y = "Adults in fair or poor health (%)",
    caption = paste0("One point per county (n = ", comma(n_clean),
                     "). Line is OLS. Data: County Health Rankings, 2025.")
  ) +
  theme_minimal(base_size = 10) +
  theme(
    plot.title    = element_text(face = "bold"),
    plot.subtitle = element_text(size = 8.5),
    plot.caption  = element_text(color = "grey40", hjust = 0),
    panel.grid.minor = element_blank()
  )
```

# 3. Model and methods 
We estimate our OLS by 
$$\text{fair/poor}_i = \beta_0 + \beta_1\text{smoking}_i + \beta_2\log(\text{income}_i) + \beta_3\text{college}_i + \beta_4\text{uninsured}_i + \beta_5\log(\text{pop}_i) + u_i$$

Each control is plausibly correlated with smoking and health outcomes. Our first control, income, measured through its log, matters because poorer counties tend to have more people who smoke and also tend to report worse health, so we want to separate income's association with health from smoking's. Some college, in other words, education, is included because it can influence people's health choices and how well they understand information around health. Uninsured is important because people without health insurance may have less access to medical care, which can affect their health regardless of whether they smoke. Population describes the size of the county but also explains underlying factors such as demographics and potential smoking norms. Population also helps account for differences between rural and urban counties, since they can differ in both smoking rates and access to healthcare. Leaving these variables out would cause the smoking coefficient to be biased because the smoking coefficient would absorb their effect.

Our functional form has population logged because it ranges from just 217 people to 9.6 million, making a one-person increase meaningless for some counties but much more important for others. Income is also logged because it varies by about a factor of six, so proportional differences in income are a more useful comparison than treating a $1 increase as having the same meaning at every income level. Smoking is not transformed because it ranges from 5.9% to 38.3%, is fairly close to symmetric, and adding a quadratic term barely improves the fit.

Figures 2 and 3 also support this choice. Figure 2 shows the relationship using population in its original form, but the relationship is heavily compressed because a few very large counties pull the scale out, making it harder to see the pattern among most counties. Figure 3 uses log population, which spreads the observations out more evenly and makes the underlying relationship much clearer. That improvement is why the log transformation earns its place in the model.

```{r fig2-pop-levels, fig.width = 6.5, fig.height = 3.0, fig.align = "center"}
# Task 4: Figure 2, population in LEVELS ==================================
# Deliberately the untransformed version as its job is to show the
# skew that motivates the log in Section 3. A handful of very large counties
# take up the whole axis and every other county is crushed against the left
# edge, leaving it practically unreadable.
ggplot(counties, aes(x = population, y = fair_poor_health)) +
  geom_point(alpha = 0.20, size = 0.8, color = "grey30") +
  geom_smooth(method = "lm", se = FALSE, color = fit_color, linewidth = 1) +
  scale_x_continuous(labels = label_number(scale = 1e-6, suffix = "M")) +
  scale_y_continuous(labels = label_number(suffix = "%")) +
  labs(
    title = "Figure 2: County Size and Health Outcomes",
    subtitle = "Larger counties tend to show lower rates of poor/fair health outcomes",
    x = "County population (millions)",
    y = "Adults in fair or poor health (%)",
    caption = paste0("One point per county (n = ", comma(n_clean),
                     "). Line is OLS. Data: County Health Rankings, 2025.")
  ) +
  theme_minimal(base_size = 10) +
  theme(
    plot.title    = element_text(face = "bold"),
    plot.subtitle = element_text(size = 8.5),
    plot.caption  = element_text(color = "grey40", hjust = 0),
    panel.grid.minor = element_blank()
  )
```

```{r fig3-pop-logs, fig.width = 6.5, fig.height = 3.0, fig.align = "center"}
# Task 5: Figure 3, the same plot on the LOG scale ========================
# Same counties, same fitted line, log x-axis. Read side by side with Figure
# 2 as this is the evidence that the transformation earns its place. The points
# now fill the plotting region instead of piling up at the left edge.
ggplot(counties, aes(x = log_population, y = fair_poor_health)) +
  geom_point(alpha = 0.20, size = 0.8, color = "grey30") +
  geom_smooth(method = "lm", se = FALSE, color = fit_color, linewidth = 1) +
  scale_y_continuous(labels = label_number(suffix = "%")) +
  labs(
    title = "Figure 3: Log Population and Health Outcomes ",
    subtitle = "Larger counties show slightly lower rates of poor/fair health outcomes",
    x = "Log county population (log people)",
    y = "Adults in fair or poor health (%)",
    caption = paste0("One point per county (n = ", comma(n_clean),
                     "). Line is OLS. Data: County Health Rankings, 2025.")
  ) +
  theme_minimal(base_size = 10) +
  theme(
    plot.title    = element_text(face = "bold"),
    plot.subtitle = element_text(size = 8.5),
    plot.caption  = element_text(color = "grey40", hjust = 0),
    panel.grid.minor = element_blank()
  )
```

# 4 Results 

```{r regressiontable, results='asis', echo=FALSE, message=FALSE, warning=FALSE}
# Simple model: smoking alone
fit_simple <- feols(
  fair_poor_health ~ adult_smoking,
  data = counties
)

# Full model: smoking + 4 controls, two of them in logs. Both by OLS. 
fit_full <- feols(
  fair_poor_health ~ adult_smoking + log_income + some_college + uninsured + log_population,
  data = counties
)

# Two-specification regression table
results_table <- etable(
  fit_simple, fit_full,
  fitstat = ~ n + r2 + ar2,
  digits = 3,
  float     = TRUE,
  placement = "H",
  dict = c(
    fair_poor_health = "Fair or poor health (%)",
    adult_smoking    = "Adult smoking (%)",
    log_income       = "Log income",
    some_college     = "Some college (%)",
    uninsured        = "Uninsured (%)",
    log_population   = "Log population"
  ),
  caption = "Higher smoking rates are associated with worse health; the gradient shrinks once controls enter.",
  notes = "OLS estimates; standard errors in parentheses. Data: County Health Rankings 2025.",
  style.tex = style.tex("aer"),
  tex = TRUE
)

cat(results_table)
```

```{r f-test}

fit_full_lm <- lm(
  fair_poor_health ~ adult_smoking + log_income + some_college +
    uninsured + log_population,
  data = counties
)

f_controls <- linearHypothesis(
  fit_full_lm,
  c("log_income = 0", "some_college = 0", "uninsured = 0", "log_population = 0")
)

f_stat <- f_controls$F[2]
f_df1  <- abs(f_controls$Df[2])
f_df2  <- f_controls$Res.Df[2]
f_p    <- f_controls$`Pr(>F)`[2]
```

Table 2 shows the two specifications next to each other. The first column shows adult smoking on its own with a slope of 0.945, meaning that a one percentage point increase in smoking is associated with approximately 0.95 percentage points more adults reporting fair or poor health. In the second column we add our four controls (income, education, uninsured rate, and population). The coefficient falls to 0.542, a shrinkage of about 43%. Without those controls, part of what income, education, insurance, and county size explain is being attributed to smoking.

We interpret two of the full model's partial slopes. Holding smoking, income, uninsured rate, and population fixed, a county with one percentage point more of adults aged 25 to 44 with some college reports 0.081 percentage points fewer adults in fair or poor health. Holding smoking, education, uninsured rate, and population fixed, a county whose median household income is 10% higher reports about 0.56 percentage points fewer adults in fair or poor health.

The coefficient on log income is a semi-elasticity, read against proportional changes in income rather than dollar ones. The raw coefficient of −5.89 corresponds to a one-unit change in log income, which is a county roughly 172% richer, so the 10% comparison above is the readable version.

The estimated coefficient on smoking is 0.542, with a standard error of 0.016, giving a t-statistic of about 33 and a 95% confidence interval of roughly [0.51, 0.57]. This is statistically significant by any reasonable standard: zero is nowhere near the confidence interval, so we decisively reject the hypothesis of no association. Economically, the association is also substantial. A 10-percentage-point increase in a county's smoking rate is associated with about a 5.4-percentage-point increase in the share reporting fair or poor health, which is about a seventh of the 38-point national range. Statistical and economic significance point the same way here: the estimate is both large and precise, and even the low end of the confidence interval remains economically meaningful.

The $R^2$ is 0.780 and the adjusted $R^2$ is 0.779, slightly lower because it charges the model for the regressors it added. These five county characteristics account for most of what distinguishes healthier counties from less healthy ones, leaving about a fifth of the variation to everything we have not measured.

We tested the four controls jointly. Our F-test gives F(4, 3136) = 692.4 with a p-value below 0.001, so we reject the null that the four control coefficients are all zero. The controls belong in the model as a block, which is also what the movement between columns (1) and (2) was telling us.

\clearpage

# Appendix: all code

```{r appendix, ref.label = knitr::all_labels(label != "appendix"), echo = TRUE, eval = FALSE}
```
