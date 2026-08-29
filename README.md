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


