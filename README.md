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

# 2. Data [REVIEW ME]
We use the County Health Rankings & Roadmaps 2025 national analytic data file from countyhealthrankings.org, in which each observation represents a U.S. county in a single year. The raw file carries hundreds of measures; we keep the nine columns this analysis uses. We express the four rate variables in percentage points by multiplying the reported proportions by 100, and we construct the log of median household income and the log of population. After cleaning, `r n_clean` of `r n_raw` counties remain, with `r n_dropped` dropped: two are very small counties (Kalawao, HI, with population 81, and Loving, TX, with population 43), and eight are Connecticut counties, which no longer have health data reported for them. Table 1 shows that fair or poor health averages 19.56% across counties (SD 4.80, range 8.8% to 46.5%), and adult smoking averages 17.96% (SD 3.89, range 5.9% to 38.3%). Population is the lopsided variable, where its mean of 106,593 sits against a standard deviation of 332,667, and it runs from 217 people to over 9.6 million.

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

# 3. Model and methods [EDIT ME]

 Section 3 needs the regression estimate and why that specification; why each control belongs; which functional form chosen and
why it suits that variable; and a reading of Figures 2 and 3.

We estimate our OLS by 
fair/poor health=  β₀ + β₁ smokingᵢ + β₂ log(incomeᵢ) + β₃ educationᵢ + β₄ uninsuredᵢ + β₅ log(populationᵢ) + ei 

Each control is plausibly correlated with smoking and health outcomes. Our first control, income, measured through its log, includes household differences such as living condidtions, equity. This is a control that both shapes influences on smoking and one's health outcomes. The next control, education, is a variable that describes one's education level, which ultimately can reflect in their knowledge around smoking and can influence their health. Uninsured rate directly affects the possibility of smoking and ultimately, health. This explains access to care. Population describes the size of the county but also explains underlying factors such as demographics and potential smoking norms. Leaving these variables out would cause the smoking coefficient to be biased because the smoking coefficient would absorb their effect.

Our functional form uses logs for median household d population. Population ranges from a couple hundred residents to roughly ten million. Income ranges from 3#0,000 to $170,000. 

Figure 2 plots fair/poor health against population levels. The plot shows a downward slope, showing data with most of the populations falling under 2.5 million. Figure 3 re-plots this data on a log scale. The slope changes and is supported by all counties instead of just one tail. The log scale creates more proportional data, which allows it to be meaningful for our hypothesis.






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
    subtitle = "Larger couties show slightly lower rates of poor/fair health outcomes",
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

# Full model: smoking + 4 controls
fit_full <- feols(
  fair_poor_health ~ adult_smoking + log_income + some_college + uninsured + log_population,
  data = counties
)

# Two-specification regression table
results_table <- etable(
  fit_simple, fit_full,
  fitstat = ~ n + r2 + ar2,
  digits = 3,
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

Table 2 shows the 2 specifications next to each other. The first column shows adult smoking on its own with a slope of 0.945, meaning that a 1% point increase in smoking is associated with approximately 94.5 percentage points more adults reporting fair or poor health. In the second column, we add in our 4 controls following our regression (income, education, uninsured rate, and population). The coefficient changes to approximately 0.542, showing a difference of ~43%. With the controls, counties with lower income, lower education, higher uninsured rate, and smaller populations report worse health outcomes. Like stated previously, without these controls, their effect to smoking is more accurately reported.

Two of the full model's partial slopes.  The uninsured rate as well as log-population coefficients are both positive, explaining that counties with more uninsured households or larger populations report roughly higher numbers of adults with fair/poor health. Holding smoking, income, uninsured rate, and population fixed, a county with individuals ages 25-44 with some college experience 1$ point higher shows 0.081 % points fewer adults in fair/poor health. Holding smoking, education, uninsured rate, and population fixed, a county whose median household income is 1% higher reports a ~5.89% point fewer adults in fair or poor health on average. 

The coefficient on adult smoking is **EDITTTTTTTTT**. A 1 % point increase in smoking is associated with 0.542 % point more adults reporting fair or poor health. Economically,  acountry whose smoking rate is 10% points higher on average reports a 5.42 % point increase in adults in fair/poor health.

The estimate R^2= 0.780 and the adjusted R^2= 0.779 since it accounts for the variables added to the regression. Although this is a small difference, the characteritsics chosen from the larger data set, explain a large share of thevariation in health outcomes.

We tested the significance of the variables addes. Our F test, F(4, 3136) is approziamtely 54 with a small p-value of ~0.001. Here, we reject the null hypothesis at the 0.05 level. Adding our controls and through our testing help us understant that they belong in the model. Coming back to our regression table, the differences and shfts between column (1) to column (2).

\clearpage

# Appendix: all code

```{r appendix, ref.label = knitr::all_labels(label != "appendix"), echo = TRUE, eval = FALSE}
```
