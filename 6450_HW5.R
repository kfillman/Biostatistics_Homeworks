# PUBH6450; HW 5
# Kath Fillman

# setwd & load data
setwd("C:/Users/Kath/Documents/Biostatistics_Homeworks/6450 Datasets")
pph <- read.csv('PPHstudy.csv')

# SI as a predictor of blood loss during delivery
SI15 <- pph$SBP15/pph$HR15

# check the dimensions match
length(SI15)
length(pph$TotalBloodLoss)

# Create model & plot
model <- lm(pph$TotalBloodLoss~SI15)
plot(SI15, pph$TotalBloodLoss,
     xlab="Shock Index 15 min After Delivery",
     ylab="Total Blood Loss During Delivery (mL)",
     main="Shock Index 15 min After Delivery vs Total Blood Loss")
abline(model, col='red')

# Calculate cook's distance
cookd<-cooks.distance(model)
which(cookd > 1) # no influential points

# Model diagnostics
par(mfrow=c(2,2))
plot(model)
par(mfrow=c(1,1))



# Not equal variance, log transform total blood loss
SI152 <- pph$SBP15/pph$HR15
model2 <- lm(log(pph$TotalBloodLoss)~SI15)
plot(SI15, log(pph$TotalBloodLoss),
     xlab="Shock Index 15 min After Delivery",
     ylab="Total Blood Loss During Delivery (mL)",
     main="Shock Index 15 min After Delivery vs Total Blood Loss")
abline(model2, col='red')

# Calculate cook's distance round 2
cookd2<-cooks.distance(model2)
which(cookd2 > 1) # no influential points

# Model diagnostics round 2
par(mfrow=c(2,2))
plot(model2)
par(mfrow=c(1,1))

# Inference
summary(model2)
confint(model2, level=0.95)
