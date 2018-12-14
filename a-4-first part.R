bank.data <- read.csv("C://Users//abarn_000//Desktop//DM 1//bankruptcy.csv", header = T)
#Part(i) starts

#Exploratory Data Analysis
names(bank.data)
dim(bank.data)
str(bank.data)
summary(bank.data)

#Training/Testing Dataset
subset <- sample(nrow(bank.data), nrow(bank.data) * 0.8)
bank.train = bank.data[subset, ]
bank.test = bank.data[-subset, ]

table(bank.data$DLRSN)
776/5436

table(bank.train$DLRSN)
622/4348

table(bank.test$DLRSN)
154/1088

#Running a generalized linear model
bank.glm <- glm(DLRSN ~ . - FYEAR -CUSIP, family = binomial, bank.train)
summary(bank.glm)
BIC(bank.glm)

#Running GLM with significant variables alone
bank.glm1 <- glm(DLRSN ~ R1 + R2 + R3 + R6+ R7 + R8 + R9 + R10 , family = binomial, bank.train)
summary(bank.glm1)
BIC(bank.glm1)

#Running logit linK
mylogit <- glm(DLRSN ~ . - FYEAR -CUSIP , data = bank.train, family = "binomial"(link = "logit"))
summary(mylogit)
BIC(mylogit) 

#Running probit link
myprobit <- glm(DLRSN ~ . - FYEAR -CUSIP , data = bank.train, family = "binomial"(link = "probit"))
summary(myprobit)
BIC(myprobit)


#Running complementary log-log link
mycloglog <- glm(DLRSN ~ . - FYEAR -CUSIP , data = bank.train, family = "binomial"(link = "cloglog"))
summary(mycloglog)
BIC(mycloglog)

#part(i) ends part (ii) starts

#Using stepwise method
bank.AIC <- step(mylogit)
summary(bank.AIC)
BIC(bank.AIC)

#Using BIC method
bank.BIC <- step(mylogit, k = log(nrow(bank.train)))
summary(bank.BIC)
BIC(bank.BIC)


#Histogram
hist(predict(bank.AIC, type = "response"))

#ROC curve for AIC -LoGIT MODEL

install.packages("verification")
library("verification")
prob.glm.insample <- predict(bank.AIC, type = "response")
roc.plot(bank.train$DLRSN == "1", prob.glm.insample)

#AREA UNDER CURVE
roc.plot(bank.train$DLRSN == "1", prob.glm.insample)$roc.vol


#Misclassification IN SAMPLE data.

predicted.glm.insample <- prob.glm.insample > 0.20
predicted.glm.insample <- as.numeric(predicted.glm.insample)
predicted.glm.insample
table(bank.train$DLRSN, predicted.glm.insample, dnn = c("Truth", "Predicted"))
mean(ifelse(bank.train$DLRSN != predicted.glm.insample, 1, 0))

#Part(ii) ends and part(iii) starts

#Misclassification OUT OF SAMPLE data.
pcut=0.09

prob.glm0.outsample <- predict(bank.AIC, bank.test, type = "response")
predicted.glm0.outsample <- prob.glm0.outsample > 0.09
predicted.glm0.outsample <- as.numeric(predicted.glm0.outsample)
predicted.glm0.outsample
table(bank.test$DLRSN, predicted.glm0.outsample, dnn = c("Truth", "Predicted"))
mean(ifelse(bank.test$DLRSN != predicted.glm0.outsample, 1, 0))


#Roc curve for OUT OF SAMPLE data

roc.plot(bank.test$DLRSN == "1", prob.glm0.outsample)

#AREA UNDER CURVE
roc.plot(bank.test$DLRSN == "1", prob.glm0.outsample)$roc.vol

#Part(iii)ends Part(iv) starts

#For INSAMPLE DATA
# Find optimal Cut-off probability for INSAMPLE DATA from 0.01 to 0.99
searchgrid = seq(0.01, 0.99, 0.01)
result = cbind(searchgrid, NA)
cost1 <- function(r, pi) {
  weight1 = 10
  weight0 = 1
  c1 = (r == 1) & (pi < pcut)  #logical vector - true if actual 1 but predict 0
  c0 = (r == 0) & (pi > pcut)  #logical vecotr - true if actual 0 but predict 1
  return(mean(weight1 * c1 + weight0 * c0))
}

for (i in 1:length(searchgrid)) {
  pcut <- result[i, 1]
  result[i, 2] <- cost1(bank.train$DLRSN, predict(bank.AIC, type = "response"))
}
plot(result, ylab = "Cost in Testing Set")
result
#part(iv) ends part(v) starts

#Cross Validation

library(boot)
bank.glm3 <- glm(DLRSN ~ R1+R2+R3+R4+R5+R6+R7+R8+R9+R10, family = binomial, bank.data)
bank.st=step(bank.glm3)
bank.cross=cv.glm(bank.data, bank.st, cost1, 5)
bank.cross$delta

#Misclassification 
pcut=0.10

prob.glm0.fullsample <- predict(bank.st, bank.data, type = "response")
predicted.glm0.fullsample <- prob.glm0.fullsample > 0.10
predicted.glm0.fullsample <- as.numeric(predicted.glm0.fullsample)
predicted.glm0.fullsample
table(bank.data$DLRSN, predicted.glm0.fullsample, dnn = c("Truth", "Predicted"))
mean(ifelse(bank.data$DLRSN != predicted.glm0.fullsample, 1, 0))

#Roc curve
roc.plot(bank.data$DLRSN == "1", prob.glm0.fullsample)

#AREA UNDER CURVE
roc.plot(bank.data$DLRSN == "1", prob.glm0.fullsample)$roc.vol

#part(v)ends part (vi)starts
install.packages("rpart")
library(rpart)

#Classification tree on training set
bank.rpart <- rpart(formula = DLRSN ~ R1+R2+R3+R4+R5+R6+R7+R8+R9+R10, data = bank.train, method = "class",parms = list(loss = matrix(c(0, 10, 1, 0), nrow = 2)))
bank.rpart
plot(bank.rpart)
text(bank.rpart)

#Prediction using classification tree

#In-sample prediction
bank.train.pred.tree1 = predict(bank.rpart, bank.train, type = "class")
table(bank.train$DLRSN, bank.train.pred.tree1, dnn = c("Truth", "Predicted"))

  #Out-Sample prediction
bank.test.pred.tree1 = predict(bank.rpart, bank.test, type = "class")
table(bank.test$DLRSN, bank.test.pred.tree1, dnn = c("Truth", "Predicted"))


cost <- function(r, pi) {
  weight1 = 10
  weight0 = 1
  c1 = (r == 1) & (pi == 0)  #logical vector - true if actual 1 but predict 0
  c0 = (r == 0) & (pi == 1)  #logical vecotr - true if actual 0 but predict 1
  return(mean(weight1 * c1 + weight0 * c0))
}
cost(bank.test$DLRSN, bank.test.pred.tree1)

#compare this model's out-of-sample performance with the logistic regression model with all variables in it

bank.glm.tree <- glm(DLRSN ~ R1+R2+R3+R4+R5+R6+R7+R8+R9+R10, family = binomial, bank.data)
bank.test.pred.glm = as.numeric(predict(bank.glm.tree, bank.test, type = "response") >0.09)
cost(bank.test$DLRSN, bank.test.pred.glm)


# Confusion matrix
table(bank.test$DLRSN, bank.test.pred.glm, dnn = c("Truth", "Predicted"))

#ROC of classifiction tree

bank.rpart2 <- rpart(formula = DLRSN ~ R1+R2+R3+R4+R5+R6+R7+R8+R9+R10, data = bank.train, method = "class", cp = 5e-04)
bank.test.prob.rpart2 = predict(bank.rpart2, bank.test, type = "prob")

install.packages("ROCR")
library(ROCR)
pred = prediction(bank.test.prob.rpart2[, 2], bank.test$DLRSN)
perf = performance(pred, "tpr", "fpr")
plot(perf, colorize = TRUE)

#AUC of classification tree

slot(performance(pred, "auc"), "y.values")[[1]]



redit.test.pred.rpart2 = as.numeric(credit.test.prob.rpart2[, 2] > 0.1)
table(credit.test$Y, credit.test.pred.rpart2, dnn = c("Truth", "Predicted"))

#Cross Validation of classification tree

library(boot)
install.packages("tree")
library(tree)
bank.glm3.tree <- glm(DLRSN ~ R1+R2+R3+R4+R5+R6+R7+R8+R9+R10, family = binomial, bank.data)
bank.step.tree=step(bank.glm.tree)
bank.cross.tree=cv.tree(bank.data, bank.step.tree, cost1, 5)
bank.cross.tree=cv.tree(bank.data)
bank.cross.tree$delta


