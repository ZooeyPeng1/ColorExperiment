library(datasets)  # Load base packages manually
library(kmed)
library(dplyr)
library(sjPlot)
library(gtsummary)
library(readxl)
library(car)
library(tidyverse)
library(ggpubr)
library(rstatix)
library(broom)
library(xtable)
library(flextable)
library(officer)
library(easystats)

# Installs pacman ("package manager") if needed
if (!require("pacman")) install.packages("pacman")

# Use pacman to load add-on packages as desired
pacman::p_load(pacman, rio) 

alldat <- import("D:/data/erp/grand-all-ica/finalVer/relate/relate.csv")
head(alldat)
View(alldat)
str(alldat)
summary(alldat)

alldat$id <- factor(alldat$id)
alldat$lag <- factor(alldat$lag)
alldat$category <- factor(alldat$lag)

library(correlation)

correlation::correlation(alldat,
                         include_factors = TRUE, method = "auto"
)
alldat %>%
  correlation(partial = TRUE, multilevel = TRUE) %>%
  summary()
