# Read data
bank<- read.csv("C://Users//PCS//Desktop//bank-full.csv", header = T)
str(bank)
bank$job=factor(bank$job)
bank$marital=factor(bank$marital)
bank$education=factor(bank$education)
bank$default=factor(bank$default)
bank$housing=factor(bank$housing)
bank$loan=factor(bank$loan)
bank$contact=factor(bank$contact)
bank$month=factor(bank$month)
bank$poutcome=factor(bank$poutcome)
bank$y=factor(bank$y)
str(bank)

# Sampling data
set.seed(121200)
bank_sample = sample(nrow(bank), nrow(bank) * 0.80)
bank_train=bank[bank_sample, ]
bank_test=bank[-bank_sample, ]

#To find response rates (# of No and Yes )
table(bank$y)
table(bank_train$y)
table(bank_test$y)

#LOGISTIC REGRESSION METHOD
    # 1.Full Variable selection

bank_glm=glm(y~ . , family=binomial, data=bank_train)
summary(bank_glm)
AIC(bank_glm)
BIC(bank_glm)
deviance_glm=bank_glm$deviance/bank_glm$df.residual
deviance_glm

    #2.Stepwise variable selection
bank_step <- step(bank_glm, direction = c("both"))
summary(bank_step)
AIC(bank_step)
BIC(bank_step)
deviance_glm=bank_step$deviance/bank_step$df.residual
deviance_glm

    #3. histogram
hist(predict(bank_step, type = "response"))

    #4. Cost Vs Cutoff using search grid

searchgrid = seq(0.01, 0.99, 0.01)
#define the searc grid from 0.01 to 0.99 result is a 99x2 matrix, 
#the 1st col stores the cut-off p, the 2nd column
# stores the cost
result = cbind(searchgrid, NA)
# in the cost function, both r and pi are vectors, r=truth, pi=predicted probability

cost1 <- function(r, pi) {
  weight1 = 10
  weight0 = 1
  c1 = (r == 1) & (pi < pcut)  #logical vector - true if actual 1 but predict 0
  c0 = (r == 0) & (pi > pcut)  #logical vecotr - true if actual 0 but predict 1
  return(mean(weight1 * c1 + weight0 * c0))
}
bank_prob <- glm(y~., family = binomial, bank_train)
for (i in 1:length(searchgrid)) {
  pcut <- result[i, 1]
  # assign the cost to the 2nd col
  result[i, 2] <- cost1(bank_train$y, predict(bank_prob, type = "response"))
}
plot(result, ylab = "Cost in Training Set")
result


  #5.In-sample (performance on training set)

prob.insample <- predict(bank_step, bank_train, type = "response")
predicted.insample <- prob.insample > 0.1
predicted.insample <- as.numeric(predicted.insample)

  #confusion matrix
table(bank_train$y, predicted.insample, dnn = c("Truth", "Predicted"))

  #Misclassification Rate
mean(ifelse(bank_train$y != predicted.insample, 1, 0))
  #Cost
cost1(bank_train$y,predicted.insample)

#ROC AND AUC
install.packages("verifictaion")
library(verification)
roc.plot(bank_train$y =="1", prob.insample)
roc.plot(bank_train$y == "1", prob.insample)$roc.vol
roc.plot(bank_train$y == "1", prob.insample)


  #6. Out-of-sample (performance on testing set)
prob.outsample <- predict(bank_step, bank_test, type = "response")
predicted.outsample <- prob.outsample > 0.1
predicted.outsample <- as.numeric(predicted.outsample)
  #Confusin Matrix
table(bank_test$y, predicted.outsample, dnn = c("Truth", "Predicted"))
  #Misclassification Rate
mean(ifelse(bank_test$y != predicted.outsample, 1, 0))
  #cost
cost1(bank_test$y,predicted.outsample)

  #ROC AND AUC
roc.plot(bank_test$y == "1", prob.outsample)
roc.plot(bank_test$y == "1", prob.outsample)$roc.vol


  #7. #Cross Validation Logistic regression -  5 fold
pcut=0.10
cost1 <- function(r, pi) {
  weight1 = 10
  weight0 = 1
  c1 = (r == 1) & (pi < pcut)  #logical vector - true if actual 1 but predict 0
  c0 = (r == 0) & (pi > pcut)  #logical vecotr - true if actual 0 but predict 1
  return(mean(weight1 * c1 + weight0 * c0))
}
library(boot)
bank.glm <- glm(y ~ job + marital + education + balance + housing + loan + contact + day + month + duration + campaign + previous + poutcome, family = binomial, data=bank)
bank.st=step(bank.glm,direction = c("both"))
bank.cross=cv.glm(bank, bank.glm, cost1, 10)
bank.cross$delta

  #Misclassification 

prob.glm0.fullsample <- predict(bank.st, type = "response")
predicted.glm0.fullsample <- prob.glm0.fullsample > 0.10
predicted.glm0.fullsample <- as.numeric(predicted.glm0.fullsample)

table(bank$y, predicted.glm0.fullsample, dnn = c("Truth", "Predicted"))
mean(ifelse(bank$y != predicted.glm0.fullsample, 1, 0))

#Roc curve
roc.plot(bank$y == "1", prob.glm0.fullsample)

#AREA UNDER CURVE
roc.plot(bank$y == "1", prob.glm0.fullsample)$roc.vol


# CLASSIFICATION TREE METHOD

install.packages("maptree")
library(maptree)
install.packages("rpart")
library(rpart)
bank.rpart <- rpart(formula = y ~ ., data = bank_train, method = "class",parms = list(loss = matrix(c(0, 10, 1, 0), nrow = 2)))
bank.rpart

plot(bank.rpart, uniform=TRUE,  margin=0.05)
text(bank.rpart, cex=0.5, digits=2, fheight=0.8)
#text(bank.rpart, use.n=TRUE, all=TRUE, cex=.5)
printcp(bank.rpart)
plotcp(bank.rpart)

  #Prediction using classification tree

  #In-sample prediction
bank.train.pred.tree1 = predict(bank.rpart, bank_train, type = "class")
table(bank_train$y, bank.train.pred.tree1, dnn = c("Truth", "Predicted"))
mean(ifelse(bank_train$y != bank.train.pred.tree1, 1, 0))

  #ROC of classifiction tree - Insample

bank.train.prob.rpart = predict(bank.rpart, bank_train, type = "prob")
predicted.tree.insample <- bank.train.prob.rpart > 0.1
predicted.tree.insample <- as.numeric(predicted.tree.insample)
install.packages("ROCR")
library(ROCR)
train.pred = prediction(bank.train.prob.rpart[, 2], bank_train$y)
train.perf = performance(train.pred, "tpr", "fpr")
plot(train.perf, colorize = TRUE, main="ROC- IN SAMPLE")

  #AUC of classification tree -Insample
slot(performance(train.pred, "auc"), "y.values")[[1]]

  #cost - Insample Tree
cost <- function(r, pi) {
  weight1 = 10
  weight0 = 1
  c1 = (r == 1) & (pi == 0)  #logical vector - true if actual 1 but predict 0
  c0 = (r == 0) & (pi == 1)  #logical vecotr - true if actual 0 but predict 1
  return(mean(weight1 * c1 + weight0 * c0))
}
cost(bank_train$y,bank.train.pred.tree1)

  #Out-Sample prediction
bank.test.pred.tree1 = predict(bank.rpart, bank_test, type = "class")
table(bank_test$y, bank.test.pred.tree1, dnn = c("Truth", "Predicted"))
mean(ifelse(bank_test$y != bank.test.pred.tree1, 1, 0))

  #ROC of classifiction tree - Outsample

bank.test.prob.rpart = predict(bank.rpart, bank_test, type = "prob")
test.pred = prediction(bank.test.prob.rpart[, 2], bank_test$y)
test.perf = performance(test.pred, "tpr", "fpr")
plot(test.perf, colorize = TRUE, main="ROC- Out SAMPLE")

  #AUC of classification tree -Outsample
slot(performance(test.pred, "auc"), "y.values")[[1]]

  #Cost - Out - sample
cost(bank_test$y, bank.test.pred.tree1)


  # Cross Validation of Classification Tree

library(boot)
install.packages("tree")
library(tree)
bank.tree <- tree(y ~ ., bank)
summary(bank.tree)
cv.model=cv.tree(bank.tree, K=10)
plot(cv.model)
summary(cv.model)


# SUPPORT VECTOR MACHINE
install.packages("e1071")
library(e1071)
install.packages("kernlab")
library(kernlab)
install.packages("ROCR")
library(ROCR)

tuned <- tune.svm(y~., data = bank, gamma = 10^(-6:-1), cost = 10^(-1:1))
summary(tuned)

svm.model <- svm(y ~ ., data = bank_train,cost = 1, gamma = 1/length(bank_train), probability=TRUE)
plot(svm.model, bank_train)
print(svm.model)
summary(svm.model)

  #In Sample Confusion Matrics
prob.svm.train = predict(svm.model, bank_train, probability = TRUE)
prob.svm.train = attr(prob.svm.train, "probabilities")[, 2]  #This is needed because prob.svm gives a 
pred.svm.train = as.numeric((prob.svm.train >= 0.10))
table(bank_train$y, pred.svm.train, dnn = c("Obs", "Pred"))
mean(ifelse(bank_train$y != pred.svm.train, 1, 0))
cost(bank_train$y, pred.svm.train)

  # ROC and AUC for Train data
svm.pred.train<-predict(svm.model, type="prob", bank_train[,-17],probability=TRUE)
svm.rocr.train<-prediction(attr(svm.pred.train,"probabilities")[,2], bank_train[,17])
svm.perf.train<-performance(svm.rocr.train, "tpr","fpr")
plot(svm.perf.train)
aucsvm.train <- performance(svm.rocr.train, 'auc') 
aucsvm.train

  #Lift Curve for SVM trainig
perflift.train <- performance(svm.rocr.train,"lift","rpp")
plot(perflift.train, main="lift curve", colorize=T)


  #Out Sample Confusion Matrix
prob.svm.test = predict(svm.model, bank_test, probability = TRUE)
prob.svm.test = attr(prob.svm.test, "probabilities")[, 2] 
pred.svm.test = as.numeric((prob.svm.test >= 0.10))
table(bank_test$y, pred.svm.test, dnn = c("Obs", "Pred"))
mean(ifelse(bank_test$y != pred.svm.test, 1, 0))
cost(bank_test$y, pred.svm.test)

# ROC and AUC for Test data
svm.pred.test<-predict(svm.model, type="prob", bank_test[,-17],probability=TRUE)
svm.rocr.test<-prediction(attr(svm.pred.test,"probabilities")[,2], bank_test[,17])
svm.perf.test<-performance(svm.rocr.test, "tpr","fpr")
plot(svm.perf.test)
aucsvm.test <- performance(svm.rocr.test, 'auc') 
aucsvm.test

#Lift Curve for SVM - Test
perf.test <- performance(svm.rocr.test,"lift","rpp")
plot(perf.test, main="lift curve", colorize=T)


cost <- function(observed, predicted) {
  weight1 = 10
  weight0 = 1
  c1 = (observed == 1) & (predicted == 0)  #logical vector - true if actual 1 but predict 0
  c0 = (observed == 0) & (predicted == 1)  #logical vecotr - true if actual 0 but predict 1
  return(mean(weight1 * c1 + weight0 * c0))
}

  #Cross Validation 
svm.modelcv <- svm(y ~ ., data = bank,cost = 1, gamma = 1/length(bank), probability=TRUE,cross=5)
prob.svmcv = predict(svm.modelcv, bank, probability = TRUE)
prob.svmcv = attr(prob.svmcv, "probabilities")[, 2]  
pred.svmcv = as.numeric((prob.svmcv >= 0.10))
table(bank$y, pred.svmcv, dnn = c("Obs", "Pred"))
mean(ifelse(bank$y != pred.svmcv, 1, 0))
cost(bank$y, pred.svmcv)

# ROC and AUC for cross validation
svm.predscv<-predict(svm.modelcv, type="prob", bank[,-17],probability=TRUE)
svm.rocrcv<-prediction(attr(svm.predscv,"probabilities")[,2], bank[,17])
svm.perfcv<-performance(svm.rocrcv, "tpr","fpr")
plot(svm.perfcv)
auccv <- performance(svm.rocrcv, 'auc') 
auccv

#Lift Curve for cross validation
perfcv <- performance(svm.rocrcv,"lift","rpp")
plot(perfcv, main="lift curve", colorize=T)
