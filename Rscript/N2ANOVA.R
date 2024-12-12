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

# Installs pacman ("package manager") if needed
if (!require("pacman")) install.packages("pacman")

# Use pacman to load add-on packages as desired
pacman::p_load(pacman, rio) 

ERP_ANOVA <- import("D:/data/erp/grand-all-ica/finalVer/P1N2/N22018_meanAMP_0.3.csv")
head(ERP_ANOVA)
View(ERP_ANOVA)
str(ERP_ANOVA)
summary(ERP_ANOVA)

ERP_ANOVA <- ERP_ANOVA %>%
  gather(key = "category", value = "amplitude", Between, Within) %>%
  convert_as_factor(id, category,lag)
set.seed(123)
ERP_ANOVA %>% sample_n_by(category, lag, size = 1)

ERP_ANOVA %>%
  group_by(lag, category) %>%
  get_summary_stats(amplitude, type = "mean_sd")

ERP_ANOVA %>%
  group_by(lag, category) %>%
  shapiro_test(amplitude)

N2.aov <- anova_test(
  data = ERP_ANOVA, dv = amplitude, wid = id,
  within = c(lag, category)
)
get_anova_table(N2.aov)

one.way <- ERP_ANOVA %>%
  group_by(lag) %>%
  anova_test(dv = amplitude, wid = id, within = category) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way

library(ggpubr)

ggline(subset(ERP_ANOVA), 
       x = "lag",
       y = "amplitude",
       color = "category",
       add = c("mean_se") # add mean and standard error
) +
  labs(y = "N2 mean amplitude")



ggplot(ERP_ANOVA, aes(lag, amplitude, fill = category)) +
  stat_summary(geom = "bar", fun = mean, position = "dodge") +
  stat_summary(geom = "errorbar", fun.data = mean_se, position = "dodge")

m1=as_flextable(xtable(N2.aov))
doc = read_docx()
doc = body_add_flextable(doc,m1)
print(doc,"d:/data/erp/grand-all-ica/finalVer/P1N2/m1.docx")

m2=as_flextable(xtable(one.way))
doc = read_docx()
doc = body_add_flextable(doc,m2)
print(doc,"d:/data/erp/grand-all-ica/finalVer/P1N2/m2.docx")
