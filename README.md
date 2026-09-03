# Econ-103-Summer-Project

---
title: "Adult Smoking & County Health Outcomes"
author: "Maddie Thompson (UID 006460651) & Tommy Nguyen (UID 206756393)"
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

library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(kableExtra)
library(fixest)
library(car)
```
```{r load-data, include = FALSE}
health_raw <- read_csv("countyhealth2025.csv")
# Try to detect columns by partial name
colnames(health_raw)
```

```{r clean}
# DO NOT convert all character columns to numeric
# Only convert numeric-looking columns

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

coding other option/similar to example:
```{r summary, echo=FALSE, message=FALSE, warning=FALSE, results='asis'}
summary_vars <- tibble(
  Variable = c(
    "Fair/Poor Health (%)",
    "Adult Smoking (%)",
    "Income Inequality (ratio)",
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
```{r fig1}
ggplot(gp_reg_data, aes(x = smoking, y = outcome)) +
  geom_point(alpha = 0.4, color = "grey40") +
  geom_smooth(method = "lm", se = FALSE, color = "#1F7A5C", linewidth = 1) +
  labs(
    title = "Counties with higher smoking tend to report worse health",
    x = "Adult Smoking Rate",
    y = "Fair/Poor Health",
    caption = paste("n =", n_obs)
  ) +
  theme_minimal()
```
### Figure 2: Transformed Data
```{r fig2}
ggplot(gp_reg_data, aes(x = smoking, y = outcome)) +
  geom_point(alpha = 0.4, color = "grey40") +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE,
              color = "#1F7A5C", linewidth = 1) +
  labs(
    title = "Quadratic Smoking Term and Controls in 2 OLS Specifications",
    x = "Adult Smoking Rate",
    y = "Fair/Poor Health",
    caption = paste("n =", n_obs)
  ) +
  theme_minimal()
```
#Regression
```{r regression}
fit_simple <- feols(outcome ~ smoking + I(smoking^2), data=gp_reg_data)

fit_full <- feols(outcome ~ smoking + I(smoking^2) + income + education + uninsured + population,
  data = gp_reg_data )
etable(
  fit_simple, fit_full,
  fitstat = ~ n + r2 + ar2,
  digits = 3,
  dict = c(
    outcome        = "Fair/Poor Health",
    smoking        = "Adult Smoking",
    "I(smoking^2)" = "Smoking²",
    income         = "Income Inequality",
    education      = "Some College",
    uninsured      = "Uninsured",
    population     = "Population"
  ),
  caption = "Quadratic smoking term and controls in two OLS specifications.",
  float = TRUE,
  placement = "H",
  style.tex = style.tex("aer"),
  tex = TRUE
)
```
# Interpretation 

#F Test 
```{r test}
fit_full_lm <- lm(outcome ~ smoking + I(smoking^2) + income + education + uninsured + population,
                  data = gp_reg_data)

f_controls <- linearHypothesis(
  fit_full_lm,
  c("income = 0", "education = 0", "uninsured = 0")
)
```





