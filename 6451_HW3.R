# 6451; HW 3
# Kath Fillman

# Data import & set-up
setwd("C:/Users/Kath/Documents/Biostatistics_Homeworks/6451 Datasets")
dekaf <- read.csv('dekaf_dgf.csv')
library(jtools) # for glm()
library(vcd) # for mosaic()
library(ResourceSelection) # for hoslem.test()
library(dplyr)
library(psych)

# Data exploration
# DGF & dcd
mosaic(~dgf + dcd_yn, data=dekaf, highlighting='dgf') # plot dgf and dcd_yn
table<-table(dekaf$dgf, dekaf$dcd_yn)
addmargins(table)
prop.table(table)
# Donor age
hist(dekaf$agedonor16, main = 'Donor Age Distirbution', xlab='Donor age')
dekaf %>% group_by(dgf) %>% summarize(meanage=mean(agedonor16), 
                                      sd=sd(agedonor16))
describe(dekaf$agedonor16)



# test for relationship between dgf and dcd
chisq.test(table) # significant


# test for assocaitation between age and dgf/dcd_yn
t.test(dekaf$agedonor16~dekaf$dgf, conf.level=0.95, mu=0,
       alternative='two.sided', var.equal=FALSE) # significant
t.test(dekaf$agedonor16~dekaf$dcd_yn, conf.level=0.95, mu=0,
       alternative='two.sided', var.equal=FALSE) # significant

# fit model
model <- glm(factor(dgf)~factor(dcd_yn, ref=0)+agedonor16, data=dekaf, family = binomial(link="logit"))
summ(model, confint = TRUE, ci.width = 0.95, digits = 4, exp = TRUE) # in terms of odd ratio
# test model fit
hoslem.test(dekaf$dgf, fitted(model))
