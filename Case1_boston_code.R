library(MASS);
data(Boston)
summary(Boston)
str(Boston)


###HTML output###
#HTMLStart(outdir="G:\\MBA\\2012\\business analytics\\UC\\course\\8. DM\\DM2\\HW\\1",filename="case1output", extension="html", echo=FALSE,HTMLframe=TRUE)

###90% sample for Train and 10% for Test####
set.seed(12345)
sample_index=sample(nrow(Boston), nrow(Boston)*0.9)
Boston_train=Boston[sample_index,]
Boston_test=Boston[-sample_index,]

str(Boston_train)
str(Boston_test)
###GLM Model####
Boston.glm = glm(medv ~., family=gaussian, data=Boston_train)
Boston.glm.summary=summary(Boston.glm)
Boston.glm.summary

AIC(Boston.glm)
BIC(Boston.glm)
Boston.glm$deviance/Boston.glm$df.residual#MSE


###GLM Best Model###
Boston.glm.best=step(Boston.glm, direction="both")
Boston.glm.best.summary=summary(Boston.glm.best)
Boston.glm.best.summary

AIC(Boston.glm.best)
BIC(Boston.glm.best)

###in sample performance###
glm_isp=Boston.glm.best$deviance/Boston.glm.best$df.residual#in sample MSE
glm_isp
plot(Boston.glm.best)
plot(Boston_train$medv, Boston.glm.best$fitted.values,xlab = 'actual', ylab = 'fitted', main = 'Fitted vs.Actual for In-sample/Training data')


###out of sample performance###
predict_glm_osp=predict(Boston.glm.best, Boston_test)
glm_osp = sum((Boston_test$medv-predict_glm_osp)^2)/(nrow(Boston_test)-length(Boston.glm.best$coefficient))#out sample MSE
glm_osp

glm_osp_residuals=Boston_test$medv-predict_glm_osp

plot(predict_glm_osp,glm_osp_residuals,xlab = 'fitted', ylab = 'residuals', main = 'Residuals vs.Fitted for Out-sample/Test data')
plot(Boston_test$medv, predict_glm_osp,xlab = 'actual', ylab = 'fitted', main = 'Fitted vs.Actual for Out-sample/Test data')


###Tree####
install.packages("rpart")
library(rpart)
Boston.tree=rpart(formula = medv~., data=Boston_train)
Boston.tree
printcp(Boston.tree)
plot(Boston.tree)
text(Boston.tree,cex=0.6, digits=2)

library(cluster)
install.packages("rpart.plot")
library("rpart.plot")
prp(Boston.tree)

install.packages("rattle")
library("rattle")
fancyRpartPlot(Boston.tree)
draw.tree(Boston.tree, cex=0.7, digits=2)
plotcp(Boston.tree)

###prune tree###
Boston.tree.best=prune(Boston.tree, cp=0.012186)
draw.tree(Boston.tree.best, cex=0.7, digits=2)
printcp(Boston.tree.best)
plotcp(Boston.tree.best)
###in sample performance###

predict_tree_isp=predict(Boston.tree,Boston_train)
tree_isp=mean((Boston_train$medv-predict_tree_isp)^2)
tree_isp
tree_isp_residuals=Boston_train$medv-predict_tree_isp

plot(predict_tree_isp,tree_isp_residuals,xlab = 'fitted', ylab = 'residuals', main = 'Residuals vs. Fitted for In-sample/Train data')
plot(Boston_train$medv, predict_tree_isp,xlab = 'actual', ylab = 'fitted', main = 'Fitted vs.Actuals for Out-sample/Train data')


###out of sample performance###
predict_tree_osp=predict(Boston.tree,Boston_test)
tree_osp=mean((Boston_test$medv-predict_tree_osp)^2)
tree_osp
tree_osp_residuals=Boston_test$medv-predict_tree_osp

plot(predict_tree_osp,tree_osp_residuals,xlab = 'fitted', ylab = 'residuals', main = 'Residuals vs. Fitted for Out-sample/Test data')
plot(Boston_test$medv, predict_tree_osp,xlab = 'actual', ylab = 'fitted', main = 'Fitted vs. Actuals for Out-sample/Test data')


###GAM####

#gam_formula=as.formula(paste("medv~ s(", paste(colnames(Boston_train)[1:13], collapse =") + s("), paste(")")))
library(mgcv)
gam_formula=medv ~ s(crim) + s(zn) + s(indus) + chas + s(nox) + s(rm) + s(age) + s(dis) + rad + s(tax) + s(ptratio) + s(black) + s(lstat)
Boston.gam=gam(formula=gam_formula, data=Boston_train)
Boston.gam.summary=summary(Boston.gam)
Boston.gam.summary


###best model###

#gam_formula_best=medv ~ s(crim) + s(indus) + s(nox) + s(rm) + s(dis) + rad + s(tax) + s(ptratio) + s(lstat)
#Boston.gam.best=gam(formula=gam_formula_best, data=Boston_train)
#Boston.gam.best.summary=summary(Boston.gam.best)
#Boston.gam.best.summary

plot(Boston.gam, pages=1)

AIC(Boston.gam)
BIC(Boston.gam)

###in sample performance###
gam_isp=Boston.gam$deviance/Boston.gam$df.residual
gam_isp
plot(Boston.gam$fitted.values,Boston.gam$residuals,xlab = 'fitted', ylab = 'residuals', main = 'Residuals vs.Fitted for in-sample/Train data')
plot(Boston_train$medv, Boston.gam$fitted.values,xlab = 'actual', ylab = 'fitted', main = 'Fitted vs. Actuals for in-sample/Train data')


###out of sample performance###
predict_osp_gam=predict(Boston.gam, Boston_test)
gam_osp=mean((Boston_test$medv-predict_osp_gam)^2)
gam_osp
gam_osp_residuals=Boston_test$medv-predict_osp_gam

#par(mfrow=c(1,2))
plot(predict_osp_gam,gam_osp_residuals,xlab = 'fitted', ylab = 'residuals', main = 'Residuals vs. Fitted out-sample')
plot(Boston_test$medv, predict_osp_gam,xlab = 'actual', ylab = 'fitted', main = 'Fitted vs. Actuals out-sample')

###Neural Networks Models####
library(nnet)
Boston.nnet=nnet(medv ~ ., size = 30, data = Boston_train, rang = 0.00001, linout = TRUE, maxit = 10000, decay = 0, skip = TRUE)


###in sample performance###

nnet_isp=mean((Boston_train$medv-Boston.nnet$fitted.values)^2)
nnet_isp

plot(Boston.nnet$fitted.values,Boston.nnet$residuals,xlab = 'fitted', ylab = 'residuals', main = 'Residuals vs. Fitted in-sample')
plot(Boston_train$medv, Boston.nnet$fitted.values,xlab = 'actual', ylab = 'fitted', main = 'Fitted vs. Actuals in-sample')

###out sample performance###

predict_nnet_osp=predict(Boston.nnet,Boston_test)
nnet_osp=mean((Boston_test$medv-predict_nnet_osp)^2)
nnet_osp
nnet_osp_residuals=Boston_test$medv-predict_nnet_osp

plot(predict_nnet_osp,nnet_osp_residuals,xlab = 'fitted', ylab = 'residuals', main = 'Residuals vs Fitted out-sample')
plot(Boston_test$medv, predict_nnet_osp,xlab = 'actual', ylab = 'fitted', main = 'Fitted vs. Actual out-sample')


###Model Performance Comparison####
performance=data.frame(rbind(c(glm_isp, glm_osp), c(tree_isp, tree_osp), c(gam_isp, gam_osp), c(nnet_isp, nnet_osp)))
colnames(performance) = c("In Sample MSE", "Out Sample MSE")
rownames(performance) = c("GLM", "TREE", "GAM", "NNET")
performance
