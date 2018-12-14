credit.data <- read.csv("http://homepages.uc.edu/~maifg/7040/credit0.csv", header = T)
head(credit.data)
#x2-x9 -> continuous predictor variables
#x10 onwards -> categorical variables including dummies
subset <- sample(nrow(credit.data), nrow(credit.data) * 0.9)
credit.train = credit.data[subset, ]
credit.test = credit.data[-subset, ]
colnames(credit.train)
credit.glm0 <- glm(Y ~ . - id, family = binomial, credit.train)
summary(credit.glm0)

credit.glm1 <- glm(Y ~ X3 + X8 + X11_2, family = binomial, credit.train)
AIC(credit.glm0)
AIC(credit.glm1)

BIC(credit.glm0)
BIC(credit.glm1)


hist(predict(credit.glm1)) #predicts log odds
hist(predict(credit.glm1, type = "response")) #predicts probability
table(predict(credit.glm1, type = "response") > 0.5) # 50% of people will default
table(predict(credit.glm1, type = "response") > 0.2)
table(predict(credit.glm1, type = "response") > 0) 
#everyone to the right hand side of 0 will default

prob.glm1.insample <- predict(credit.glm1, type = "response")
predicted.glm1.insample <- prob.glm1.insample > 0.2
predicted.glm1.insample[1:10] # get first 10 records
predicted.glm1.insample <- as.numeric(predicted.glm1.insample)
# I do the above to compare predicted outcome with true outcome
table(credit.train$Y, predicted.glm1.insample, dnn = c("Truth", "Predicted"))
mean(ifelse(credit.train$Y != predicted.glm1.insample, 1, 0))#we want the number to be small - this is the misclassification rate


prob.glm1.outsample <- predict(credit.glm1, credit.test, type = "response")
predicted.glm1.outsample <- prob.glm1.outsample > 0.2
predicted.glm1.outsample <- as.numeric(predicted.glm1.outsample)
table(credit.test$Y, predicted.glm1.outsample, dnn = c("Truth", "Predicted"))

table(credit.train$Y)

install.packages("verification")
library("verification")
roc.plot(credit.test$Y == "1", prob.glm1.outsample)
roc.plot(credit.test$Y == "1", prob.glm1.outsample)$roc.vol
prob.glm0.outsample <- predict(credit.glm0, credit.test, type = "response")
roc.plot(x = credit.test$Y == "1", pred = cbind(prob.glm0.outsample, prob.glm1.outsample), 
         legend = TRUE, leg.text = c("Full Model", "X_3, X_8, and X_11_2"))$roc.vol

searchgrid = seq(0.01, 0.99, 0.01)
result = cbind(searchgrid, NA)
cost1 <- function(r, pi)  
  #r is the truth and pi is the predicted probability
  {
  weight1 = 10 # if I pass someone who is likely to default then it will cost me 10X than otherwise
  weight0 = 1
  c1 = (r == 1) & (pi < pcut)  #logical vector - true if actual 1 but predict 0
  c0 = (r == 0) & (pi > pcut)  #logical vecotr - true if actual 0 but predict 1
  return(mean(weight1 * c1 + weight0 * c0))
}
credit.glm1 <- glm(Y ~ X3 + X8 + X11_2, family = binomial, credit.train)
for (i in 1:length(searchgrid)) {
  pcut <- searchgrid[i]
  # assign the cost to the 2nd col
  result[i, 2] <- cost1(credit.train$Y, predict(credit.glm1, type = "response"))
}
result # output 1st column in pcut and 2nd column is the cost

plot(result, ylab = "Cost in Training Set")


library(boot)
#credit.glm1 <- glm(Y ~ X3 + X8 + X11_2, family = binomial, credit.train)
credit.glm3 <- glm(Y ~ X3 + X8 + X11_2, family = binomial, credit.train)
credit.glm.step <- step(credit.glm3)
cv.result = cv.glm(credit.train, credit.glm.step, cost1, 10)
cv.result$delta