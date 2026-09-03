# Econ-103-Summer-Project

---
title: "Adult Smoking & County Health Outcomes"
author: "Group 22. Maddie Thompson (UID 006460651) and Tommy Nguyen (UID 206756393)
date: "September 7, 2026"
output:
  pdf_document:
    latex_engine: pdflatex
    geometry: margin=0.9in
    fontsize: 11pt
header-includes:
  - \usepackage{titling}
  - \setlength{\droptitle}{-4em}
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

```{r packages}
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(kableExtra)
library(fixest)
library(car)
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
```{r summary}
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
    caption = "Summary statistics for outcome and key explanatory variables") %>%
  kable_styling(latex_options = "HOLD_position")
```

###Figure 1: Health Outome VS. Smoking
```{r figure #1}
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




