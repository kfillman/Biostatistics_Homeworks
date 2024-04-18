# PUBH 6450; Week 4, HW 2
# Kath Fillman

# Load in data
setwd("C:/Users/Kath/Documents/Biostatistics_Homeworks/6450 Datasets")
SleepStudy <- read.csv("SleepStudy.csv")

# Exploratory data analysis
summary(SleepStudy$WeekdaySleep)
hist(SleepStudy$WeekdaySleep, xlab = "Weekday Hours of Sleep", main = "Frequency of Weekday Hours of Sleep") #skewed left

# Results
t.test(SleepStudy$WeekdaySleep, alternative = 'less', mu=8, conf.level = 0.95)