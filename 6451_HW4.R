# PUBH6451; HW 4
# Kath Fillman


# Data Import & Set-up ----------------------------------------------------

setwd("C:/Users/Kath/Documents/Biostatistics_Homeworks/6451 Datasets")
first6 <- read.csv('first6.csv')
library(psych)
library(survival)
library(ggfortify)
library(survminer)
library(multcomp)

# Data Exploration --------------------------------------------------------

describeBy(first6$qoi, group = first6$twoclass, mat = TRUE, digits = 4)
describeBy(first6$t2qoi, group = first6$twoclass, mat = TRUE, digits = 4)
describeBy(first6$UNIT, group = first6$twoclass, mat = TRUE, digits = 4)
describeBy(first6$above200, group = first6$twoclass, mat = TRUE, digits = 4)

# Kaplan-Meier curve for two class data
unadjusted <- survfit(Surv(t2qoi, qoi) ~ twoclass, data=first6)
autoplot(unadjusted) + 
  xlab('Time to AIDS Event (Months)') + 
  ylab('Percent Without AIDS Event') +
  labs(title='Time to AIDS Event', 
       subtitle = 'Grouped by Two-Class or Three-Class Treatment Type',
       caption = 'Group 1 is the two-class threatment and group 2 is the three-class treatment') +
  theme_bw() +
  theme()


# Model Creation ----------------------------------------------------------

model <- coxph(Surv(t2qoi, qoi) ~ twoclass + above200 + factor(UNIT), data=first6)
summary(model)

# Check proportional hazards assumption
model_resid <- cox.zph(model)
ggcoxzph(model_resid)
