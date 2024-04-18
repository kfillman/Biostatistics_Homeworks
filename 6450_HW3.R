# PUBH 6450; HW 3
#Kath Fillman

# Get data in & libraries loaded
library(ggplot2)
library(psych)
setwd("~/Biostats/Biostats HW3")
pph_study <- read.csv('PPHstudyUpdated.csv')

# Find number of variables
ncol(pph_study)

#1: Prior Illness
describeBy(pph_study$TotalBloodLoss, pph_study$NoPriorIllness)
boxplot(pph_study$TotalBloodLoss~pph_study$NoPriorIllness,
        xlab = "Presenece of Prior Illness", ylab = "Total Blood Loss (mL)",
        main = "Total Blood Loss (mL) by Presence of Prior Illness")
bloodloss_priorillness <- aov(pph_study$TotalBloodLoss~pph_study$NoPriorIllness)
summary(bloodloss_priorillness)
# F value: 0.443
# Pr(>F): 0.506

#2: BBO
describeBy(pph_study$TotalBloodLoss, pph_study$BBO)
boxplot(pph_study$TotalBloodLoss~pph_study$BBO,
        xlab = "Baby's Birth Order", ylab = "Total Blood Loss (mL)",
        main = "Total Blood Loss (mL) by Baby's Birth Order")
bloodloss_birthorder <- aov(pph_study$TotalBloodLoss~pph_study$BBO)
summary(bloodloss_birthorder)
# F value: 15.61
# Pr(>F): 2.72e-07
pairwise.t.test(pph_study$TotalBloodLoss, pph_study$BBO, p.adj = "bonferroni")
