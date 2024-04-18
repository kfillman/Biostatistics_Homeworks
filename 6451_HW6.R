# PUBH6451; HW 6
# Kath Fillman


# Import data & libraries -------------------------------------------------

setwd("C:/Users/Kath/Documents/Biostatistics_Homeworks/6451 Datasets")
betterbirths <- read.csv('BetterBirth-DataSet.csv')
library(jtools)
library(stats)
library(ggfortify)
library(dplyr)
library(psych)

# Data cleaning & editing -------------------------------------------------

bb <- betterbirths %>% select(morbid, literacy , highincome , lucknow , age35plus , 
                              ageunder25 , scheduled , obc , meangravida , complicbefore , 
                              anemia) %>% na.omit()

# Exploratory Analysis ----------------------------------------------------

describe(bb[c("morbid", "literacy", "age35plus", "ageunder25", "scheduled", "obc",
              "meangravida", "complicbefore", "anemia")])
table(bb$lucknow)
table(bb$highincome)

hist(bb$morbid, )
hist(bb$morbid, main = 'Severe Maternal Morbidity',
     xlab = 'Proportion of Severe Maternal Morbidity')

cor(bb[ ,c("morbid", "literacy", "highincome","lucknow", "age35plus", "ageunder25",
           "scheduled", "obc", "meangravida", "complicbefore", "anemia")],
    use = "complete.obs")

# Model Selection ---------------------------------------------------------

# Fit the models
full_model <- glm(formula = morbid ~ literacy + highincome + lucknow + age35plus + 
                    ageunder25 + scheduled + obc + meangravida + complicbefore + 
                    anemia, data = bb)
null_model <- glm(formula = morbid ~ 1, data = bb)

# Forward Selection
forward_model <- step(null_model, direction = "forward",
                      scope = list(
                        upper = full_model,
                        lower = null_model))
summary(forward_model)

# Backward Selection
backward_model <- step(full_model, direction = "backward",
                       scope = list(
                         upper = full_model,
                         lower = null_model))
summary(backward_model)

# Stepwise Selection
stepwise_model <- step(full_model, direction = "both",
                       scope = list(
                         upper = full_model,
                         lower = null_model))
summary(stepwise_model)