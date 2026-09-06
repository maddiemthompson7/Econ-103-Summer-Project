# Econ-103-Summer-Project

maddie's version: 
---
title: "Adult Smoking & County Health Outcomes"
author: "Group 22. Maddie Thompson (UID 006460651) & Tommy Nguyen (UID 206756393)"
date: "September 7, 2026"
output:
  pdf_document:
    latex_engine: pdflatex
  word_document: default
header-includes:
- \usepackage{titling}
- \setlength{\droptitle}{-4em}
- \usepackage{float}
- \usepackage{booktabs}
- \fontsize{11pt}{13pt}\selectfont
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
# Read the county extract by a RELATIVE path. group22_data.csv sits next to
# this .Rmd, so the report knits on a grader's machine without setwd().
# group22_build.R is the script that produced it from the raw.

counties_raw <- read_csv("group22_data.csv", show_col_types = FALSE)
n_raw <- nrow(counties_raw)
# 3,152 rows, one per US county

# Drop county rows missing any analysis variable, then build the constructed
# measures. County Health Rankings reports the four rate variables as
# proportions (0.177); every table, axis, and coefficient in this report is
# in PERCENTAGE POINTS, so they are multiplied by 100 once, here.

# Population and income enter in logs. Population runs from a few hundred
# people to nearly ten million, and income from under $30k to over $170k, so
# in levels a one-unit step means something completely different at the two
# ends of each range. Section 3 makes that argument with Figures 2 and 3.
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

We predict that counties with higher smoking rates have a higher share of adults in fair or poor health. Smoking has a direct and well-documented path to disease, including lung cancer, respiratory illness, and cardiovascular disease, and a county where smoking is common is also likely to differ in other health behaviours that track with worse outcomes. We expect the coefficient to shrink once income, education, insurance, and population enter, but not to collapse. Counties with similar median incomes smoke at very different rates, depending on state tobacco taxes, local norms, and regional history, so income cannot be doing all the work. Additionally, cigarettes do physical damage to the lungs and other body parts in ways that poverty alone does not.

# 2. Data NEEDS EDIT
Each observation is from U.S County Health data in 2025. There are 3172 rows observed in 2025. No rows were removed but columns were removed due to the amount the data holds. There are hundreds of measures included which is not feasible or necessary for this regression. 

Table #1: Population showsis heavily right-skewed data, with a mean of 312,000 with a maximum of of 334 million.

```{r summary, echo=FALSE, message=FALSE, warning=FALSE, results='asis'}
summary_vars <- tibble(
  Variable = c(
    "Fair/Poor Health (%)",
    "Adult Smoking (%)",
    **"Median Household Income (ratio)",**
    "Some College (%)",
    "Uninsured (%)",
    "Population"
  ),
  values = list(
    gp_reg_data$outcome,
    gp_reg_data$smoking,
    gp_reg_data$income,
    gp_reg_data$education,
    gp_reg_data$uninsured,
    gp_reg_data$population
  )
)

summary_table <- summary_vars %>%
  mutate(
    Mean = sapply(values, mean),
    SD   = sapply(values, sd),
    Min  = sapply(values, min),
    Max  = sapply(values, max),
    N    = sapply(values, length)
  ) %>%
  select(-values)

kbl(summary_table, digits = 3, booktabs = TRUE,
    caption = "Summary Statistics")
```

The average county reports 19.6% of adults who are in poor/fair health with a standard deviation 0f 4.8%, showing that counties differed in health outcomes. The average individual smoking rate is 18% with a range from approximately 6% to 38%. County population's had a larger range from 217 residents to 334 million, averaging 312,000 which does not fully show the range. 


### Figure 1: Health Outome VS. Smoking
```{r fig1, fig.width=5, fig.height=3, out.width="60%"}
ggplot(gp_reg_data, aes(x = smoking, y = outcome)) +
  geom_point(alpha = 0.45, size = 1.1, color = "grey30") +
  geom_smooth(method = "lm", se = FALSE, color = "#1F7A5C", linewidth = 1) +
  labs(
    title = "Figure 1: Counties with higher smoking tend to 
                          report worse health",
    subtitle = "The fitted line slopes upward, summarizing the positive association",
    x = "Adult Smoking Rate",
    y = "Fair/Poor Health",
    caption = paste0("One point per county (n = ", n_obs,
                     "). Line is OLS. Data: County Health Rankings 2025.")
  ) +
  theme_minimal(base_size = 10) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.caption = element_text(color = "grey40", hjust = 0),
    panel.grid.minor = element_blank()
  )
```

### Figure 2: Transformed Data
```{r fig2, fig.width=5, fig.height=3, out.width="60%"}
ggplot(gp_reg_data, aes(x = smoking, y = outcome)) +
  geom_point(alpha = 0.45, size = 1.1, color = "grey30") +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE,
              color = "#1F7A5C", linewidth = 1) +
  labs(
    title = "Figure 2: Quadratic Smoking Term and Controls in 
                        2 OLS Specifications",
    subtitle = "The quadratic curve bends upward, indicating stronger association at higher smoking rates",
    x = "Adult Smoking Rate",
    y = "Fair/Poor Health",
    caption = paste0("One point per county (n = ", n_obs,
                     "). Line is quadratic OLS. Data: County Health Rankings 2025.")
  ) +
  theme_minimal(base_size = 10) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.caption = element_text(color = "grey40", hjust = 0),
    panel.grid.minor = element_blank()
  )
```
```{r regression-table, results='asis', echo=FALSE, message=FALSE, warning=FALSE}
fit_simple <- feols(
  outcome ~ smoking,
  data = gp_reg_data
)

fit_full <- feols(
  outcome ~ smoking + I(smoking^2) + income + education + uninsured + population,
  data = gp_reg_data
)

results_table <- etable(
  fit_simple, fit_full,
  fitstat = ~ n + r2 + ar2,
  digits = 3,
  dict = c(
    outcome        = "Fair/Poor Health (%)",
    smoking        = "Adult Smoking (%)",
    "I(smoking^2)" = "Smoking²",
    income         = "Income Inequality",
    education      = "Some College (%)",
    uninsured      = "Uninsured (%)",
    population     = "Population"
  ),
  caption = "Smoking is positively associated with poor health; the quadratic term strengthens the relationship.",
  notes = "OLS estimates; standard errors in parentheses. Data: County Health Rankings 2025.",
  style.tex = style.tex("aer"),
  tex = TRUE
)
cat(results_table)
```

```{r ftest, echo=FALSE}
fit_full_lm <- lm(
  outcome ~ smoking + I(smoking^2) + income + education + uninsured + population,
  data = gp_reg_data
)

f_controls <- linearHypothesis(
  fit_full_lm,
  c("income = 0", "education = 0", "uninsured = 0")
)

f_controls
```
# Results
Column (1) of Table X puts adult smoking on its own and returns a slope of 0.942.
Adding the controls in column (2) pulls it down to 0.213, a shrinkage of about 77%.

\clearpage
# Appendix: All Code
```{r appendix, echo=TRUE, eval=FALSE}
# Appendix: All code used in the report
# This chunk prints every other chunk in the document.
```{r appendix, echo=TRUE, eval=FALSE, ref.label=knitr::all_labels()}















**DRAFT**

gp_reg_data <- health_raw %>%
  select(
    state      = `State Abbreviation`,
    outcome    = `Poor or Fair Health raw value`,
    smoking    = `Adult Smoking raw value`,
    income     = `Income Inequality raw value`,
    education  = `Some College raw value`,
    uninsured  = `Uninsured raw value`,
    population = `Population raw value`
  ) %>%
  mutate(
    outcome    = as.numeric(outcome),
    smoking    = as.numeric(smoking),
    income     = as.numeric(income),
    education  = as.numeric(education),
    uninsured  = as.numeric(uninsured),
    population = as.numeric(population)
  ) %>%
  drop_na()
n_obs <- nrow(gp_reg_data)
```

# One accent colour for every fitted line in the report.
fit_color <- "#C1440E"
```
# Introduction
Do counties with higher adult smoking rates have a higher share of adults in fair or poor health?
We expect yes, with a positive coefficient of roughly [NUMBER] in the model, because smoking can cause disease
directly and a high county smoking rate also signifies a broader health culture locally. In other words, places
where smoking is common tend to have other habits that track with worse health. We expect the coefficient to shrink
when income, education, insurance, and county size enter, but not to collapse, because [REASON]

# Introduction-option 2-similar to example
Do counties with higher smoking rates have worse health outcomes? We dive into this question through data from the 2025 County Health Rankings analytical data set. This data shows each county in its own row, providing information regarding its poor/fair raw value, smoking rate, income inequality, educational level, uninsured rate, population size, and other factors. 

Our dependent variable is the percentage of adults who report that they have poor or fair health. Our explanatory variable is the county's adult smoking rate, where the number of adults who smoke is observed. We include some additional explanatory variables to thoroughly run tests on the data. We include income inequality, education level, uninsured, and population size of the county. Each of these variables are associated with smoking, showing their importance and why they should not be omitted.

By choosing this data, we predict that individuals who smoke have an association with worse health. This is something that is worldwide, showing more illness in smokers, such as lung cancer, respiratory diseases, cardiovascular diseases, poor dental health, etc. Having this in mind, we expect that a 1-point increase in smoking shall impact an individual's health. Through broader research, we find that even small increases in smoking can negatively impact one's health. We expect there to be a positive coefficient and smoking to be economically significant in an individual's health. 

# Data 
Each observation is from U.S County Health data in 2025. There are 3172 rows observed in 2025. No rows were removed but columns were removed due to the amount the data holds. There are hundreds of measures included which is not feasible or necessary for this regression. 

Table #1: Population showsis heavily right-skewed data, with a mean of 312,000 with a maximum of of 334 million.

## Data Source
## Cleaning


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

