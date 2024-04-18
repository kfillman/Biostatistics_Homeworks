# PUBH6451; HW 1 & 2
# Kath Fillman

##############################
# HW 1

# Setting up data for use
setwd("C:/Users/Kath/Documents/Biostatistics_Homeworks/6451 Datasets")
library(ggplot2)
library(psych)
library(jtools)
library(car)
tomhs <- read.csv('tomhs.csv')


# Calculate weight & chol changes
tomhs$wtchg <- tomhs$wt12-tomhs$wtbl
tomhs$cholchg <- tomhs$chol12-tomhs$cholbl

# Explore Weight Change
describe(tomhs$wtchg)
hist(tomhs$wtchg, col='lightblue',
     xlab='Change in Weight', main='Frequency of Weight Change')
qqnorm(tomhs$wtchg, main = "QQ Plot of total weight change")
qqline(tomhs$wtchg, col = 2)

# Explore Cholestorol Change
describe(tomhs$cholchg)
hist(tomhs$cholchg, col='lightblue',
     xlab='Change in Cholesterol', main='Frequency of Cholesterol Change')
qqnorm(tomhs$cholchg, main = "QQ Plot of total cholesterol change")
qqline(tomhs$cholchg, col = 2)

# Adjust for sex
sex_freq <- table(tomhs$sex)
sex_prop <- prop.table(sex_freq)
sex_summary <- cbind(Number = sex_freq, Proportion = sex_prop)
sum(is.na(tomhs$sex)) #0

# Create models
unadj_model<-glm(cholchg ~ wtchg, data=tomhs, family='gaussian')
adj_model<-glm(cholchg ~ wtchg + factor(sex), data=tomhs, family='gaussian')
summ(unadj_model, confint=TRUE, ci.width=0.95, digits=4)
summ(adj_model, confint=TRUE, ci.width=0.95, digits=4)

# Plot
plot(x=tomhs$wtchg, y=tomhs$cholchg,
     xlab= 'Weight Change (lbs)', ylab = 'Cholesterol Change (mg/dL)',
     main='Change in Weight vs Change in Cholesterol')
abline(unadj_model, col = 'red')

plot(x=tomhs$wtchg, y=tomhs$cholchg,
     xlab= 'Weight Change (lbs)', ylab = 'Cholesterol Change (mg/dL)',
     main='Change in Weight vs Change in Cholesterol Adjusted for Sex')
abline(a=-2.8319, b=0.5169, col='blue')
abline(a=-0.0370, b=0.5169, col='hotpink')
legend(x='bottomleft', title='Legend', legend=c('Males', 'Females'),
       fill=c('blue','hotpink'))


##############################
# HW 2

hist(adj_model$residuals)

# Standardized residuals plot- looks good
plot(adj_model, which=3, id.n=10)

# Cook's Distance- no influential points
influenceIndexPlot(adj_model, vars = "Cook")
abline(h = 1, lty = 2, col='red')
