# Econ-103-Summer-Project

current version: 
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
We use the County Health Rankings & Roadmaps 2025 national analytic data file from countyhealthrankings.org, in which each observation represents a U.S. county in a single year. The raw file carries hundreds of measures; we keep the nine columns this analysis uses. We express the four rate variables in percentage points by multiplying the reported proportions by 100, and we construct the log of median household income and the log of population. After cleaning, `r n_clean` of `r n_raw` counties remain, with `r n_dropped` dropped: two are very small counties (Kalawao, HI, population 81, and Loving, TX, population 43), and eight are Connecticut counties, which no longer have health data reported for them. Fair or poor health averages 19.56% across counties (SD 4.80, range 8.8% to 46.5%), and adult smoking averages 17.96% (SD 3.89, range 5.9% to 38.3%). Population is the lopsided variable: its mean of 106,593 sits against a standard deviation of 332,667, and it runs from 217 people to over 9.6 million.

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
# the OLS line on top. This is the plot Section 2 reads: it is the raw
# relationship the whole report is about, before any controls enter.
ggplot(counties, aes(x = adult_smoking, y = fair_poor_health)) +
  geom_point(alpha = 0.20, size = 0.8, color = "grey30") +
  geom_smooth(method = "lm", se = FALSE, color = fit_color, linewidth = 1) +
  scale_x_continuous(labels = label_number(suffix = "%")) +
  scale_y_continuous(labels = label_number(suffix = "%")) +
  labs(
    title = "Figure 1: Counties that smoke more report worse health",
    subtitle = "Each extra percentage point of smoking goes with about one more point of adults in fair or poor health",
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

