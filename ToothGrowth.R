library(reshape2)
library(ggplot2)

# Loading Data in R and doing preliminary check on dataset
data(ToothGrowth)
str(ToothGrowth)

# Renaming dose and making it as factor and checking the summary statistics
ToothGrowth$dose = factor(ToothGrowth$dose, levels=c(0.5,1.0,2.0), labels=c("low","med","high"))
summary(ToothGrowth)

# Generating plots on Data

par(mfrow = c(2,1))
ggplot (aes( x = supp, y = len), data = ToothGrowth) + geom_boxplot(aes(fill =supp))
ggplot (aes( x = dose, y = len), data = ToothGrowth) + geom_boxplot(aes(fill =dose))

coplot(len ~ dose | supp, data = ToothGrowth, panel = panel.smooth, xlab = "Tooth Growth data: length versus dose, given type of supplement")

#toothMelt <- melt(ToothGrowth, id = c("supp", "dose"), measure.vars = c("len"))
#round(acast(toothMelt, supp ~ variable, mean),2)
#round(acast(toothMelt, dose ~ variable, mean),2)
#round(acast(toothMelt, supp ~ dose, mean),2)

# Generating the mean and standard deviation per type of supplement
aggregate(ToothGrowth$len, by = list(ToothGrowth$supp), FUN = function(x) x= c(mean = round(mean(x),2), sd = round(sd(x),2)))

# Generating the mean and standard deviation per dose
aggregate(ToothGrowth$len, by = list(ToothGrowth$dose), FUN = function(x) x= c(mean = round(mean(x),2), sd = round(sd(x),2)))

# Generating the mean and standard deviation per type of supplement in conjuction with dose
aggregate(ToothGrowth$len, by = list(ToothGrowth$supp, ToothGrowth$dose), FUN = function(x) x= c(mean = round(mean(x),2), sd = round(sd(x),2)))

# Testing for mean difference by supplement type
t.test( len ~ supp, alternative = "greater", data = ToothGrowth)

# Splitting the data for dosage pairs
dLowMed <- subset(ToothGrowth, dose %in% c("low", "med"))
dLowHigh <- subset(ToothGrowth, dose %in% c("low", "high"))
dMedHigh <- subset(ToothGrowth, dose %in% c("med", "high"))

# Testing for mean difference by dose
t.test( len ~ dose, alternative = "less", data = dLowMed)
t.test( len ~ dose, alternative = "less", data = dLowHigh)
t.test( len ~ dose, alternative = "less", data = dMedHigh)

#Splitting the data by dose
dLow <- subset(ToothGrowth, dose == "low")
dMed <- subset(ToothGrowth, dose == "med")
dHigh <- subset(ToothGrowth, dose == "high")

# Testing for mean difference of type of supplement by dose
t.test( len ~ supp, alternative = "greater", data = dLow)
t.test( len ~ supp, alternative = "greater", data = dMed)
t.test( len ~ supp, data = dHigh)
