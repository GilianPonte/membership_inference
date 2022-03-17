rm(list = ls())
setwd("C:/Users/Gilia/Dropbox/PhD/Projects/1st project - Privacy-preserving Generative Adversarial Networks to Derive Marketing Insights/CHURN/")
library(dplyr)
library(rpart)
library(data.table)


# marketer ----------------------------------------------------------------

# read data
churn2 <- read.csv2("churn.csv", stringsAsFactors = F) # Real data
churn2$AreaCode <- NULL 
churn2$Phone <- NULL
churn2 <- mutate_all(churn2, .funs = as.numeric)
churn2$Churn = as.factor(churn2$Churn)
train = churn2[1:1666,]
test = churn2[1667:3333,]

# train model
independent_variables <- ("AccountLength + IntlPlan + VMailPlan + DayMins + DayCalls + EveMins + EveCalls + NightMins + NightCalls + IntlMins + IntlCalls + CustServCalls")
BaseFormula <- as.formula(paste0("Churn ~ ", independent_variables))
tree = rpart(BaseFormula, train)
rm(test) # attacker has no access to train + test set.
rm(train)

# attacker ----------------------------------------------------------------
## membership inference attack begins
independent <- ("AccountLength + IntlPlan + VMailPlan + DayMins + DayCalls + EveMins + EveCalls + NightMins + NightCalls + IntlMins + IntlCalls + CustServCalls")
BaseFormula <- as.formula(paste0("Churn ~ ", independent))

predictions <- predict(tree, newdata = churn2, type = "prob") # obtain predictions
difference = abs((as.numeric(churn2$Churn)-1) - predictions[,2]) # calculate the difference between real churn and predictions

sorted = data.frame(sort(difference, decreasing =T)) # sort descending.
sorted = as.data.frame(setDT(sorted, keep.rownames = TRUE)[]) # row numbers to a column in data frame.
in_training = sorted[1:1666,] # get the first 1,666 observations that have the highest loss.
in_training$rn = as.numeric(in_training$rn) # make row number numeric
in_training$train_prediction = 1 # assign label that the data point is in training set.


# evaluate the effectiveness. the first 1666 were in the training set.
churn2$rn = as.numeric(row.names(churn2)) # store row number.
churn2$train = c(rep(1, 1666), rep(0,1667)) # assign true labels.
accuracy = left_join(churn2, in_training) # join predictions/error with the original data set, based on row number. 
accuracy[is.na(accuracy$train_prediction),22] = 0 # the values that are missing = 0. In other words, they are not in the training set.

sum(accuracy$train_prediction == churn2$train)/3333 * 100 ## 78% accuracy!
