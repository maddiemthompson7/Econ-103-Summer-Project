# Econ-103-Summer-Project

---
title: "Do Smokers Face Higher Medical Charges?"
author: "Maddie Thompson (UID 006460651) and Tommy Ngyugen (UID 206756393)"
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
knitr::opts_chunk$set(echo = TRUE)
knitr::opts_chunk$set(
  echo = FALSE,
  warning = FALSE,
  message = FALSE,
  fig.width = 6,
  fig.height = 4
)

library(dplyr)
library(readr)
library(ggplot2)
library(kableExtra)
library(fixest)
library(car)
```

```
#Introduction 
## Research Question
Do smokers face higher medical insurance charges than non-smokers, after controlling for age, BMI, and number of children?
##Dependent & Explanatory Variables
Dependent Variable(Outcome): charges — individual medical costs billed by health insurance (USD).
Explanatory Variables: smoker — yes/no indicator (1 if smoker, 0 if non-smoker) 
##Hypothesis & Why?
Smokers in the United States have overall higher annual charges to their medical insurance through controlling for other factors. There is often a common understanding that smoker often have higher health needs compared to an individual who does not smoke.

#Data
##Data Source
The data observed and used comes from Medical Insurance Charges dataset identified through Kaggle. Through the listed data, there the data take account of indivudal characteristics such as BMI (body mass index), number of children, region, age, sex (female or male), and being a smoker.
##Cleaning
Since we will be examining dfferent values and want to be able to make conclusions based on the output, we created a new binary variable. In the original data, an individual being a smoker followed a 'yes" or "no". This now becomes 1=smoker and 0=non-smoker

```{r load-data}
insurance_data <- read_csv("insurance.csv")
```

```{r clean-data}
insurance_clean <- insurance_data |>
  mutate(smoker_binary = ifelse(smoker == "yes", 1, 0)) |>
  drop_na()

n_obs <- nrow(insurance_clean)
```
``{r summary-stats} 

summary_table <- insurance_clean |>
  summarise(
    n = n(),
    charges_mean = mean(charges),
    charges_sd   = sd(charges),
    charges_min  = min(charges),
    charges_max  = max(charges),
    
    smoker_mean  = mean(smoker_binary),
    smoker_sd    = sd(smoker_binary),
    
    age_mean     = mean(age),
    age_sd       = sd(age),
    age_min      = min(age),
    age_max      = max(age),
    
    bmi_mean     = mean(bmi),
    bmi_sd       = sd(bmi),
    bmi_min      = min(bmi),
    bmi_max      = max(bmi),
    
    children_mean = mean(children),
    children_sd   = sd(children),
    children_min  = min(children),
    children_max  = max(children)

kable(summary_table, caption = "Summary Statistics for Key Variables") |>
  kable_styling(full_width = FALSE)
```






















SECOND OPTION: 
---
title: "Coding Health Outcome"
author: "Maddie Thompson (UID 006460651) & Tommy Nguyen (UID 206756393)"
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

library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(kableExtra)
library(fixest)
library(car)
```
NEED TO FINISH: 
#Introduction 
##Research Question: 
##Dependent & explanatory Variables: 
##Hypothesis & Why: 

#Data 
##Data Source
##Cleaning

```{r load-data}
health_raw <- read_csv("healthdata.csv")
glimpse(health_raw)
```
