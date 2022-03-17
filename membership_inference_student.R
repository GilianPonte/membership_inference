rm(list = ls())
setwd("")
library(dplyr)
library(rpart)
library(rpart.plot)
library(data.table)

## load all required objects
load("C:/Users/Gilia/Desktop/membership_inference_data.RData")

## churn2 is the data set. 1666 observations are used for training of tree. But which ones?
summary(churn2)

## tree is a decision tree that is trained to predict churn. The object "BaseFormula" is used to predict churn.
independent_variables <- ("AccountLength + IntlPlan + VMailPlan + DayMins + DayCalls + EveMins + EveCalls + NightMins + NightCalls + IntlMins + IntlCalls + CustServCalls")
BaseFormula <- as.formula(paste0("Churn ~ ", independent))
summary(tree) ## summary of decision tree
rpart.plot(tree) ## pretty plot

## attack begins! think about the differences of the training set and out of sample set.
predictions <- predict(tree, newdata = churn2, type = "prob")[,2] # churn predictions of the model over entire data set.
