# Read data
german = read.csv("http://homepages.uc.edu/~maifg/DataMining/data/GermanCredit.csv", header = T)
german$Class = as.factor(german$Class)

summary(german)
str(german)
german$class


# Sampling data
set.seed(620500)
german_sample = sample(nrow(german), nrow(german) * 0.90)
german_train=german[german_sample, ]
german_test=german[-german_sample, ]


str(german_train)



# 1.GLM 
german_glm=glm(Class~ . -X, family=binomial, data=german_train)
summary(german_glm)
AIC(german_glm)
BIC(german_glm)
german_glm$deviance
deviance_glm=german_glm$deviance/german_glm$df.residual
deviance_glm

# 1.1 Stepwise variable selection
german_step <- step(german_glm, direction = c("both"))
summary(german_step)
AIC(german_step)
BIC(german_step)

german_step$deviance/german_step$df.residual
german_step$deviance

# 1.2 Search grid to find the optimal cut off probability 
searchgrid = seq(0.01, 0.99, 0.01)  
result = cbind(searchgrid, NA)  
cost1 <- function(r, pi) 
{ 
weight1 = 5 
weight0 = 1 
c1 = (r == 1) & (pi < pcut) #logical vector - true if actual 1 but predict 0 
c0 = (r == 0) & (pi > pcut) #logical vecotr - true if actual 0 but predict 1 
return(mean(weight1 * c1 + weight0 * c0)) 
} 
cost <- function(obs, pred) 
{ 
  weight1 = 5 
  weight0 = 1 
  c1 = (obs==1) & (pred==0) #logical vector - true if actual 1 but predict 0 
  c0 = (obs==0) & (pred==1) #logical vecotr - true if actual 0 but predict 1 
  return(mean(weight1 * c1 + weight0 * c0)) 
} 

for (i in 1:length(searchgrid)) 
  { 
  pcut <- result[i, 1] # assign the cost to the 2nd col 
  result[i, 2] <- cost1(german_train$Class, predict(german_step, type = "response"))
  } 
plot(result, ylab = "Total Cost", xlab="Cut-off Probability")

index.min = which.min(result[, 2])
result[index.min, 2]
result[index.min, 1]




# 1.3 in sample performance
pcut_gam = 0.22 # from code above
german_insample <- predict(german_glm, german_train, type = "response")
german_predicted <- german_insample > pcut_gam
german_predicted <- as.numeric(german_predicted)
table(german_train$Class, german_predicted, dnn = c("Truth", "Predicted"))
#misclassification rate
mean(ifelse(german_train$Class != german_predicted, 1, 0))
#cost
cost(german_train$Class, german_predicted)
#deviance
deviance_glm=german_glm$deviance/german_glm$df.residual
deviance_glm

# 1.4 Out of sample performance
german_outsample <- predict(german_glm, german_test, type = "response")
german_predicted <- german_outsample > pcut_gam
german_predicted <- as.numeric(german_predicted)
table(german_test$Class, german_predicted, dnn = c("Truth", "Predicted"))
#misclassification rate
mean(ifelse(german_test$Class != german_predicted, 1, 0))
#cost
cost(german_test$Class, german_predicted)


# 1.5 ROC curve
library("verification")
#--in sample
roc.plot(german_train$Class == "1", german_insample)
roc.plot(german_train$Class == "1", german_insample)$roc.vol
#--out of sample
roc.plot(german_test$Class == "1", german_outsample)
roc.plot(german_test$Class == "1", german_outsample)$roc.vol

# 2. GAM 
install.packages("mgcv")
library(mgcv)
gam_formula <- 
  as.formula(paste("Class~s(Duration)+s(Amount)+InstallmentRatePercentage+
                 ResidenceDuration+s(Age)+NumberExistingCredits+
                 NumberPeopleMaintenance+",
                   paste(colnames(german_train)[c(9:10,12:63)], 
                         collapse = "+")))

german_gam <- gam(formula = gam_formula, family = binomial, data = german_train)
summary(german_gam)
plot(german_gam, seWithMean = TRUE, scale = 0, pages=1)
AIC(german_gam)
BIC(german_gam)

# 2.1 Search grid to find the optimal cut off probability 
searchgrid = seq(0.01, 0.99, 0.01)  
result = cbind(searchgrid, NA)  

for (i in 1:length(searchgrid)) 
{ 
  pcut <- result[i, 1] # assign the cost to the 2nd col 
  result[i, 2] <- cost1(german_train$Class, predict(german_gam, type = "response"))
} 
plot(result, ylab = "Total Cost", xlab="Cut-off Probability")

index.min = which.min(result[, 2])
result[index.min, 2]
result[index.min, 1]

# 2.2 in sample performance
pcut_gam = 0.17 # from code above
german_gam_insample <- predict(german_gam, german_train, type = "response")
german_gam_predicted <- german_gam_insample > pcut_gam
german_gam_predicted <- as.numeric(german_gam_predicted)
table(german_train$Class, german_gam_predicted, dnn = c("Truth", "Predicted"))
#misclassification rate
mean(ifelse(german_train$Class != german_gam_predicted, 1, 0))
#cost
cost(german_train$Class, german_gam_predicted)
#deviance
deviance_gam=german_gam$deviance/german_gam$df.residual
deviance_gam

# 2.3 Out of sample performance
german_gam_outsample <- predict(german_gam, german_test, type = "response")
german_gam_predicted <- german_gam_outsample > pcut_gam
german_gam_predicted <- as.numeric(german_gam_predicted)
table(german_test$Class, german_gam_predicted, dnn = c("Truth", "Predicted"))
#misclassification rate
mean(ifelse(german_test$Class != german_gam_predicted, 1, 0))
#cost
cost(german_test$Class, german_gam_predicted)

# 2.4 ROC curve
library("verification")
#--insample
roc.plot(german_train$Class == "1", german_gam_insample)
roc.plot(german_train$Class == "1", german_gam_insample)$roc.vol
#--out of sample
roc.plot(german_test$Class == "1", german_gam_outsample)
roc.plot(german_test$Class == "1", german_gam_outsample)$roc.vol

par(mfrow=c(1,2))


# 3. Tree
library(rpart)
install.packages("maptree")
library(maptree)
german_tree=rpart(formula = Class~.-X, data=german_train, method = "class",parms=list(loss=matrix(c(0,5,1,0),nrow=2)))
german_tree
printcp(german_tree)
draw.tree(german_tree, cex=0.7, digits=2)
plotcp(german_tree)

# 3.1 prune tree
german_tree_best=prune(german_tree, cp=0.017)
draw.tree(german_tree_best, cex=0.7, digits=2)
printcp(german_tree_best)
german_tree_best


# 3.1 in sample performance
german_tree_insample=predict(german_tree,german_train,type = "class")
table(german_train$Class, german_tree_insample, dnn = c("Truth", "Predicted"))
#misclassification rate
mean(ifelse(german_train$Class != german_tree_insample, 1, 0))
#cost
cost(german_train$Class,german_tree_insample)

# 3.2 out of sample performance
german_tree_outsample=predict(german_tree,german_test,type = "class")
table(german_test$Class, german_tree_outsample, dnn = c("Truth", "Predicted"))
#misclassification rate
mean(ifelse(german_test$Class != german_tree_outsample, 1, 0))
#cost
cost(german_test$Class,german_tree_outsample)

# 4. Discriminant Analysis

library(MASS)
str(german_train)
german_lda=lda(Class~Duration+Amount+Age, data=german_train)
summary(german_lda)

# 4.1 Search grid to find the optimal cut off probability 
searchgrid = seq(0.01, 0.99, 0.01)  
result = cbind(searchgrid, NA)  

for (i in 1:length(searchgrid)) 
{ 
  pcut <- result[i, 1] # assign the cost to the 2nd col
  german_lda_insample <- predict(german_lda, data = german_train)
  result[i, 2] <- cost1(german_train$Class, german_lda_insample$posterior[,2] )
} 
plot(result, ylab = "Total Cost", xlab="Cut-off Probability")

index.min = which.min(result[, 2])
result[index.min, 2]
result[index.min, 1]

# 4.2 in sample performance
pcut_lda = 0.20 # from code above
german_lda_insample <- predict(german_lda, german_train, type = "response")
german_lda_insample <- (german_lda_insample$posterior[, 2] >= pcut_lda) * 1
table(german_train$Class, german_lda_insample, dnn = c("Truth", "Predicted"))
#misclassification rate
mean(ifelse(german_train$Class != german_lda_insample, 1, 0))
#cost
cost(german_train$Class, german_lda_insample)

# 4.3 Out of sample performance
german_lda_outsample <- predict(german_lda, german_test, type = "response")
german_lda_outsample <- (german_lda_outsample$posterior[, 2] >= pcut_lda) * 1
table(german_test$Class, german_lda_outsample, dnn = c("Truth", "Predicted"))
#misclassification rate
mean(ifelse(german_test$Class != german_lda_outsample, 1, 0))
#cost
cost(german_test$Class, german_lda_outsample)


# 5. Neural Network
install.packages("nnet")
library(nnet)

german_net<-nnet(Class~.,size = 10, data=german_train, rang = 0.00001, 
                 linout = FALSE, maxit = 1000, decay = 0, skip = TRUE);



searchgrid = seq(0.01, 0.99, 0.01)  
result = cbind(searchgrid, NA)  

for (i in 1:length(searchgrid)) 
{ 
  pcut <- result[i, 1] # assign the cost to the 2nd col
  german_net_insample<-predict(german_net, german_train);
  result[i, 2] <- cost1(german_train$Class, german_net_insample )
} 
plot(result, ylab = "Total Cost", xlab="Cut-off Probability")

index.min = which.min(result[, 2])
result[index.min, 2]
result[index.min, 1]

# in sample
pcut_nnet = 0.14 # from code above
german_net_insample<-predict(german_net, german_train);


german_net_insample1 <- (german_net_insample > pcut_nnet)*1
table(german_train$Class, german_net_insample1, dnn = c("Truth", "Predicted"))
#misclassification rate
mean(ifelse(german_train$Class != german_net_insample1, 1, 0))
#cost
cost(german_train$Class, german_net_insample1)

library("verification")
#--in sample
roc.plot(german_train$Class == "1", german_net_insample1)
roc.plot(german_train$Class == "1", german_insample)$roc.vol
#--out of sample
roc.plot(german_test$Class == "1", german_outsample)
roc.plot(german_test$Class == "1", german_outsample)$roc.vol

roc.plot(german_train$Class == "1", german_net_insample)
roc.plot(german_train$Class == "1", german_net_insample)$roc.vol

# out of sample


german_net_outsample<-predict(german_net, german_test);
german_net_outsample <- (german_net_outsample > pcut_nnet) * 1
table(german_test$Class, german_net_outsample, dnn = c("Truth", "Predicted"))
#misclassification rate
mean(ifelse(german_test$Class != german_net_outsample, 1, 0))
#cost
cost(german_test$Class, german_net_outsample)


roc.plot(german_test$Class == "1", german_net_outsample)
roc.plot(german_test$Class == "1", german_net_outsample)$roc.vol


