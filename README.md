# Econ-103-Summer-Project

---
title: "Adult Smoking & County Health Outcomes"
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





