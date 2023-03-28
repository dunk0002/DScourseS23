#!/usr/bin/env Rscript

install.packages('mice')
install.packages('modelsummary')
library(mice)
library(modelsummary)

rawWages <- read.csv("/Users/davisdunk/Desktop/Scholastic/DScourseS23/ProblemSets/PS7/wages.csv")
head(rawWages)

Wages <- rawWages[complete.cases(rawWages[, c("hgc", "tenure")]),]

WagesTable <- datasummary_skim(Wages)

baseFormula <- logwage ~ hgc + college + tenure + I(tenure^2) + age + married

#Complete Cases
completeCases <- lm(baseFormula, data = Wages, na.action = na.exclude)

#Mean imputation 
wagesMean <- Wages
logwage_mean <- logwage_mean<-mean(wagesMean$logwage,na.rm=TRUE)
wagesMean$logwage[is.na(wagesMean$logwage)] <- logwage_mean
meanImputation<-lm(baseFormula,data=wagesMean)

#Predicted
wagesPredicted <- Wages
wagesPredicted$logwage[is.na(wagesPredicted$logwage)] <- predict(completeCases, newdata = wagesPredicted)[is.na(wagesPredicted$logwage)]
predictedWages <- lm(baseFormula, data = wagesPredicted)

#Multiple Imputation
wagesImputed <- mice(Wages, m = 5, method = "norm", seed = 123)
wageMice <- with(wagesImputed, lm(logwage ~ hgc + college + tenure + I(tenure^2) + age + married))

models <- list("Complete Cases"=completeCases, "Mean Imputation"=meanImputation, "Predicted Values"=predictedWages, "Multiple Imputation"=wageMice)
modelsummary(models,stars=TRUE)

