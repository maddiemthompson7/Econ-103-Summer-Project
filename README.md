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
Do counties with higher adult smoking rates have a higher share of adults in fair or poor health? We expect yes, with a positive coefficient of roughly [NUMBER] in the model, because smoking can cause disease directly and a high county smoking rate also signifies a broader health culture locally. In other words, places where smoking is common tend to have other habits that track with worse health. We expect the coefficient to shrink when income, education, insurance, and county size enter, but not to collapse, because [REASON]

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






