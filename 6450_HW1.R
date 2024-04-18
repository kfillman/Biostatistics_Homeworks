# PUBH 6450; Homework 1
# Kath Fillman

# Importing datasets & libraries
SleepStudy <- read.csv("SleepStudy.csv")
library(ggplot2)
library(psych)


# Question 1: DAS Scores
# DASscore summary statistics
summary(SleepStudy$DASScore) # for IQR
var(SleepStudy$DASScore)
describe(SleepStudy$DASScore) #for trimmed mean

# DASScore histogram
hist(SleepStudy$DASScore, xlab = "DAS Score", main = 'Frequency of DAS Scores')
# DASScore Boxplot
boxplot(SleepStudy$DASScore, ylab = 'DAS Score', main = "Frequency of DAS Scores")


#Question 2: Average Sleep
# Average sleep summary statistics
summary(SleepStudy$AverageSleep) # for IQR
describe(SleepStudy$AverageSleep) #for trimmed mean
var(SleepStudy$AverageSleep)

# Average sleep histogram
hist(SleepStudy$AverageSleep, xlab = "Number of Hours Asleep", 
     main = 'Frequency of Average Number of Hours Slept')
# Average Sleep boxplot
boxplot(SleepStudy$AverageSleep, ylab = 'Number of Hours Asleep',
        main = "Frequency of Average Number of Hours Slept")
