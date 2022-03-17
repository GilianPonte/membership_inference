rm(list = ls())
setwd("C:/Users/Gilia/Dropbox/PhD/Projects/1st project - Privacy-preserving Generative Adversarial Networks to Derive Marketing Insights/CHURN/")
library(dplyr)
library(rpart)
library(data.table)

# read data from GAN and real data.
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
rm(test)
rm(train)

## membership inference
independent <- ("AccountLength + IntlPlan + VMailPlan + DayMins + DayCalls + EveMins + EveCalls + NightMins + NightCalls + IntlMins + IntlCalls + CustServCalls")
BaseFormula <- as.formula(paste0("Churn ~ ", independent))

predictions <- predict(tree, newdata = churn2, type = "prob")
difference = abs((as.numeric(churn2$Churn)-1) - predictions[,2])
#difference = (as.numeric(churn2$Churn)-1) - predictions[,2]

sorted = data.frame(sort(difference, decreasing =T))
sorted = as.data.frame(setDT(sorted, keep.rownames = TRUE)[])
in_training = sorted[1:1666,]
in_training$rn = as.numeric(in_training$rn)
in_training$train_prediction = 1

churn2$rn = as.numeric(row.names(churn2))
churn2$train = c(rep(1, 1666), rep(0,1667))
accuracy = left_join(churn2, in_training)
accuracy[is.na(accuracy$train_prediction),22] = 0

sum(accuracy$train_prediction == churn2$train)/3333 * 100 ## 78% accuracy!
