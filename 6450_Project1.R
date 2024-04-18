# PUBH6450; Project 1: Demographic Testing
# Kath Fillman


# SWD & Load in data
setwd("C:/Users/Kath/Documents/Biostatistics_Homeworks/6450 Datasets")
SHAPE_survey <- read.csv('SHAPEMHdata.csv')
library(psych)
library(epibasix)



# Table 1: demographics

# Gender3
# Visual summaries
hist(SHAPE_survey$Gender3) # not balanced
boxplot(SHAPE_survey$Insecurity2~SHAPE_survey$Gender3, 
        xlab='Gender Identity', ylab='Average Isecurity Score',
        main='Gender Identity in Relation to Average Insecurity Score')
# Get n, mean, & SD
describeBy(SHAPE_survey$Insecurity2, SHAPE_survey$Gender3)
# Find p-value using ANOVA
summary(aov(SHAPE_survey$Insecurity2~SHAPE_survey$Gender3))

# Trans Status
# Visual summaries
hist(SHAPE_survey$TransGender) # not balanced
boxplot(SHAPE_survey$Insecurity2~SHAPE_survey$TransGender, 
        xlab='Transgender Status', ylab='Average Isecurity Score',
        main='Transgender Status in Relation to Average Insecurity Score')
# Get n, mean, & SD
describeBy(SHAPE_survey$Insecurity2, SHAPE_survey$TransGender)
# Find p-value using ANOVA
summary(aov(SHAPE_survey$Insecurity2~SHAPE_survey$TransGender))

# Race/Ethnicity
# Visual summaries
hist(SHAPE_survey$EthRace5_2022) # not balanced
boxplot(SHAPE_survey$Insecurity2~SHAPE_survey$EthRace5_2022, 
        xlab='Ethnicity/Race', ylab='Average Isecurity Score',
        main='Ethnicty/Race in Relation to Average Insecurity Score')
# Get n, mean, & SD
describeBy(SHAPE_survey$Insecurity2, SHAPE_survey$EthRace5_2022)
# Find p-value using Kruskal-Wallis
kruskal.test(SHAPE_survey$Insecurity2, SHAPE_survey$EthRace5_2022)
# Post-hoc with multiple comparisons
pairwise.wilcox.test(SHAPE_survey$Insecurity2, SHAPE_survey$EthRace5_2022,
                     p.adjust.method = "BH")




# Table 2: Demographic risk of discrimination in health care

# Gender 2
# visualization
hist(SHAPE_survey$Discrim2) # Not balanced
barplot(table(SHAPE_survey$Gender2, SHAPE_survey$Discrim2), beside = TRUE,
        main='Gender Identity vs Instances of Medical Discrimination',
        xlab='Medically Discriminated Against', col=c('black', 'grey'))
legend('topright', legend=c('Agender/Genderqueer/Nonbinary', 'Male/Female'), fill=c('black','grey'))

# Create table & find expected values
table(SHAPE_survey$Gender2, SHAPE_survey$Discrim2)
table2_gender <- data.frame(discriminated=c(11,298), not_discriminated=c(60,2148))
rownames(table2_gender) <- c('AgenderNonbinaryGenderQueer', 'MaleFemale')
(sum(table2_gender$discriminated) * sum(table2_gender[,1]))/sum(table2_gender) #expected value over 5
# Relative Risk
summary(epi2x2(table2_gender))

# Trans status
# visualization
barplot(table(SHAPE_survey$TransGender, SHAPE_survey$Discrim2), beside = TRUE,
        main='Transgender status vs Instances of Medical Discrimination',
        xlab='Medically Discriminated Against', col=c('grey', 'black'))
legend('topright', legend=c('Cisgender', 'Transgender'), fill=c('grey', 'black'))
# Create table & find expected values
table(SHAPE_survey$TransGender, SHAPE_survey$Discrim2)
table2_trans <- data.frame(discriminated=c(11,298), not_discriminated=c(42,2166))
rownames(table2_trans) <- c('Transgender', 'Cisgender')
(sum(table2_trans$discriminated) * sum(table2_trans[,1]))/sum(table2_trans) #expected value over 5
# Relative Risk
summary(epi2x2(table2_trans))

# BIPOC/AI
# visualization
barplot(table(SHAPE_survey$BIPOCAI, SHAPE_survey$Discrim2), beside = TRUE,
        main='BIPOC/AI status vs Instances of Medical Discrimination',
        xlab='Medically Discriminated Against', col=c('black', 'grey'))
legend('topright', legend=c('BIPOC/AI', 'White'), fill=c('black','grey'))
# Create table & find expected values
table(SHAPE_survey$BIPOCAI, SHAPE_survey$Discrim2)
table2_bipocAI <- data.frame(discriminated=c(178,131), not_discriminated=c(399,1809))
rownames(table2_bipocAI) <- c('BIPOCAI', 'White')
(sum(table2_bipocAI$discriminated) * sum(table2_bipocAI[,1]))/sum(table2_bipocAI) #expected value over 5
# Relative Risk
summary(epi2x2(table2_bipocAI))




# Table 3

# Gender3
# Visual summaries
hist(SHAPE_survey$MHdays) # not balanced
boxplot(SHAPE_survey$MHdays~SHAPE_survey$Gender3, 
        xlab='Gender Identity', ylab='Number of Bad Mental Health Days',
        main='Gender in Relation to Bad Mental Health Days')
# Get n, mean, & SD
describeBy(SHAPE_survey$MHdays, SHAPE_survey$Gender3)
# Find p-value using ANOVA
summary(aov(SHAPE_survey$MHdays~SHAPE_survey$Gender3))
TukeyHSD(aov(SHAPE_survey$MHdays~as.factor(SHAPE_survey$Gender3)))

# Trans Status
# Visual summaries
boxplot(SHAPE_survey$MHdays~SHAPE_survey$TransGender, 
        xlab='Trans Status', ylab='Number of Bad Mental Health Days',
        main='Transgender Status in Relation to Bad Mental Health Days')
# Get n, mean, & SD
describeBy(SHAPE_survey$MHdays, SHAPE_survey$TransGender)
# Find p-value using ANOVA
summary(aov(SHAPE_survey$MHdays~SHAPE_survey$TransGender))

# EnthRace
# Visual summaries
boxplot(SHAPE_survey$MHdays~SHAPE_survey$EthRace5_2022, 
        xlab='Ethnicity/Race', ylab='Number of Bad Mental Health Days',
        main='Ethnicity/Race in Relation to Bad Mental Health Days')
# Get n, mean, & SD
describeBy(SHAPE_survey$MHdays, SHAPE_survey$EthRace5_2022)
# Find p-value using ANOVA
summary(aov(SHAPE_survey$MHdays~SHAPE_survey$EthRace5_2022))
TukeyHSD(aov(SHAPE_survey$MHdays~as.factor(SHAPE_survey$EthRace5_2022)))
