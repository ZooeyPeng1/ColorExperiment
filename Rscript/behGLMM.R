library(datasets)  # Load base packages manually
library(kmed)
library(dplyr)
library(sjPlot)
library(gtsummary)
library(readxl)
library(lme4)
library(car)
library(lmerTest)

# Installs pacman ("package manager") if needed
if (!require("pacman")) install.packages("pacman")

# Use pacman to load add-on packages as desired
pacman::p_load(pacman, rio) 

beh_reg <- import("D:/data/erp/beh-grand/grand_fixEEG/regession.csv")
head(beh_reg)
View(beh_reg)
str(beh_reg)

beh_reg <- beh_reg |>
  rename(
    lag = "Procedure[Trial]",
    t1Acc = t1res1.ACC,
    t2Acc = t2res1.ACC
  )

beh_reg$category <- factor(beh_reg$category)
beh_reg$lag <- factor(beh_reg$lag,
                      levels = c("lag3proc2", "lag7proc2"),
                      labels = c("lag3", "lag7")
)
beh_reg$relation <- factor(beh_reg$relation)
beh_reg$Subject <- factor(beh_reg$Subject)
beh_reg$t2Acc <- factor(beh_reg$t2Acc,
                        levels = c(0, 1),
                        labels = c("FALSE", "TRUE")
)
levels(beh_reg$t2Acc)

m1 <- glmer(t2Acc ~ lag + category + (1|Subject),
          data = beh_reg,
          family=binomial("logit")
)
summary(m1)

m1_inter <- glmer(t2Acc ~ lag * category + (1|Subject),
                data = beh_reg,
                family = "binomial"
)
tab_model(m1, m1_inter,
          show.ci = TRUE, # remove CI
          show.aic = TRUE, # display AIC
          p.style = "numeric_stars" # display p-values and stars
)

tbl_regression(m1, exponentiate = TRUE)

plot_model(m1,
           type = "pred",
           terms = c("lag", "category"),
) 


