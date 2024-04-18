# PUBH 6450; HW 4
# Kath Fillman

library(epibasix)

# Make data table from Background Info in Assignment Document
Vet_Temper <- matrix(c(176, 144, 595, 868), ncol=2, byrow=FALSE, 
                     dimnames = list(c('Vietnam Vet', 'Not Vietnam Vet'),
                                     (c('Temper Control Issues', 'No Temper Control Issues'))))
Vet_Temper
# n= 1783
# 1012 not serve, 144 temper issue
# 771 serve., 176 temper

# Tasks
summary(epi2x2(Vet_Temper))

# Graphical Summary
barplot(Vet_Temper, main = "Barplot of Temper Control Issues by 
Vietnam Veteran Status", xlab = "Vietnam Veteran Status",
        col = c("pink", "violet"), beside = TRUE)
legend("topleft", legend = c("Vietnam Veteran", "Non-Vietnam Veteran"),
       fill = c("pink", "violet"))
