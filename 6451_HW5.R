# PUBH6451; HW 5
# Kath Fillman

# Import data & libraries -------------------------------------------------

setwd("C:/Users/Kath/Documents/Biostatistics_Homeworks/6451 Datasets")
tomhs <- read.csv('tomhs.csv')

library(psych)
library(jtools)
library(MASS)

# Data Exploration --------------------------------------------------------

describe(tomhs$ql12_4)
hist(tomhs$ql12_4, breaks=6, main = 'Participant Satisfaction at 12 Months',
     xlab = 'Satisfaction', names.arg = c('Completely Satisfied', 'Very Satisfied',
                                          'Somewhat Satisfied', 'Somewhat Dissatisfied',
                                          'Very Dissatisfied', 'Completely Dissatisfied'))

describe(tomhs$age)
hist(tomhs$age, main = 'Participant Age Distributions',
     xlab = 'Age', ylab = 'Frequency')

describe(tomhs$nowsmk)
table(tomhs$nowsmk, tomhs$ql12_4)
barplot((table(tomhs$nowsmk)), bins=2, main = 'Participant Current Smokers',
        xlab = 'Current Smoking Status', ylab = 'Frequency', 
        names.arg = c('Current Smoker','Not Current Smoker'))


# Model Creation ----------------------------------------------------------

model <- polr(factor(ql12_4) ~ age + nowsmk, data=tomhs, Hess = TRUE)
modelcoef <- coef(summary(model))
p.values <- pnorm(abs(modelcoef[, "t value"]), lower.tail = FALSE) * 2
modelcoef.p <- cbind(modelcoef, "p value" = p.values)
round(modelcoef.p, digits=4)

exp(cbind(-coef(model), -confint(model)))
