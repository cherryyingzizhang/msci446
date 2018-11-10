##########################################
#Numeric Regression
##########################################
library(caret)
library(dplyr)

#Import data: includes explanatory variables AND class variable but also other columns (e.g. community area name)
data <- read.csv("4A/MSCI 446/R/explanatoryvariables/predTable.csv")
#remove extraneous columns (e.g. community area name)
dataForPred <- dplyr::select(data, -X, -community, -communityAreaNum)
dataForPred <- dataForPred[,1:13]
#dataForPred$numHospitals <- ifelse(dataForPred$numHospitals >= 3, 1, 0)
#colnames(dataForPred)[5] <- "has3OrMoreHospitals"

#Use 10-fold cross-validation for getting alpha/lambda values for glmnet
tControlObj <- caret::trainControl(
  method = "cv", number = 10,
  verboseIter = TRUE,
  summaryFunction = defaultSummary
)

k <- 77
#10 fold cross validation for getting performance metrics (RMSE, Rsquared, etc.)
splitPlan <- kWayCrossValidation(nrow(dataForPred), k, NULL, NULL)

metricsDF <- as.data.frame(matrix(nrow = 3, ncol = 4))
names(metricsDF) <- c("Model", "RMSE", "Rsquared", "MAE")

##########################################
#train using linear regression#
modelLM <- train(
  x = dataForPred[,2:13], 
  y = dataForPred[,1],
  method = "lm",
  trControl = tControlObj
)
plot(modelLM$finalModel$fitted.values, dataForPred[,1], main='Predicted vs Actual for Simple Linear Regression', xlab='Predicted', ylab='Actual')
plot(modelLM$finalModel$fitted.values, modelLM$finalModel$residuals, main='Residual Plot for Simple Linear Regression', xlab='Predicted', ylab='Residuals')
abline(h = 0, col = "darkgrey", lty = 2)
summary(modelLM)

lmPredValues <- data.frame("predicted" = rep(0, nrow(dataForPred)))
for(i in 1:k) {
  split <- splitPlan[[i]]
  model <- lm(percentViolentCrimePer1000Population ~ ., data = dataForPred[split$train,])
  lmPredValues$predicted[split$app] <- predict(model, newdata = dataForPred[split$app,])
}
metricsDF[1,] <- c("Linear Regression CV", postResample(lmPredValues$predicted, dataForPred[,1]))
metricsDF[4,] <- c("Linear Regression", postResample(predict(modelLM, dataForPred[2:13]), dataForPred[,1]))

##########################################
#train using linear regression and preprocessing steps#####################
#preprocessing includes: PCA, normalization
modelLMPreProc <- train(
  x = dataForPred[,2:13], 
  y = dataForPred[,1], 
  method = "lm",
  trControl = tControlObj,
  preProcess = c("center", "scale", "pca")
)
plot(modelLMPreProc$finalModel$fitted.values, dataForPred[,1], main='Predicted vs Actual for PCA Linear Regression', xlab='Predicted', ylab='Actual')
plot(modelLMPreProc$finalModel$fitted.values, modelLMPreProc$finalModel$residuals, main='Residual Plot for PCA Linear Regression', xlab='Predicted', ylab='Residuals')
abline(h = 0, col = "darkgrey", lty = 2)
summary(modelLMPreProc)

##########################################
#train using glmnet#
modelGLMNET <- train(
  x = dataForPred[,2:13], 
  y = dataForPred[,1],
  method = "glmnet", 
  metric = "RMSE",
  tuneGrid = expand.grid(alpha = 0:10/10, lambda = seq(0.0001, 1, length = 20)),
  trControl = tControlObj
)
predictionGLMNET <- predict(modelGLMNET, dataForPred[, 2:13])
plot(modelGLMNET,  main='Alpha and Lambda Values for GLMNET') #plots RMSE over different alpha and lambda values. colors = alpha, each dot is each lambda.
plot(predictionGLMNET, dataForPred[,1], main='Predicted vs Actual for GLMNET Regression', xlab='Predicted', ylab='Actual')
plot(predictionGLMNET, (dataForPred[,1]-predictionGLMNET), main='Residual Plot for GLMNET Regression', xlab='Predicted', ylab='Residuals')
abline(h = 0, col = "darkgrey", lty = 2)
coef(modelGLMNET$finalModel, modelGLMNET$bestTune$lambda)
postResample(predictionGLMNET, dataForPred[,1])

glmnetPredValues <- data.frame("predicted" = rep(0, nrow(dataForPred)))
for(i in 1:k) {
  split <- splitPlan[[i]]
  model <- glmnet(as.matrix(dataForPred[split$train,2:13]), dataForPred[split$train,1], alpha = modelGLMNET$bestTune$alpha, lambda = modelGLMNET$bestTune$lambda)
  glmnetPredValues$predicted[split$app] <- predict(model, s = modelGLMNET$bestTune$lambda, newx = as.matrix(dataForPred[split$app,2:13]))
}
metricsDF[2,] <- c("Elastic Net CV", postResample(glmnetPredValues$predicted, dataForPred[,1]))
metricsDF[5,] <- c("Elastic Net", postResample(predictionGLMNET, dataForPred[,1]))

##########################################
#train using GLMNET and preprocessing steps#########
#preprocessing includes: PCA, normalization
modelGLMNETPreProc <- train(
  x = dataForPred[,2:13], 
  y = dataForPred[,1],
  method = "glmnet", 
  tuneGrid = expand.grid(lambda = seq(0.0001, 1, length=10), alpha = seq(0,1,0.1)),
  trControl = tControlObj,
  preProcess = c("center", "scale", "pca")
)
plot(modelGLMNETPreProc, main='Alpha and Lambda Values for GLMNETPreProc')
predictionGLMNETPreProc <- predict(modelGLMNETPreProc, dataForPred[, 2:13])
plot(predictionGLMNETPreProc, dataForPred[,1], main='Predicted vs Actual for PreProc GLMNET Regression', xlab='Predicted', ylab='Actual')
plot(predictionGLMNETPreProc, (dataForPred[,1]-predictionGLMNETPreProc), main='Residual Plot for PreProc GLMNET Regression', xlab='Predicted', ylab='Residuals')
abline(h = 0, col = "darkgrey", lty = 2)
coef(modelGLMNETPreProc$finalModel, modelGLMNETPreProc$bestTune$lambda)
postResample(predictionGLMNETPreProc, dataForPred[,1])
#names(df)[[nearZeroVar(df)]]

##########################################
#GAM model#
#since caret can only do standard GAM model of y = s(x1) + s(x2) + etc. we will not be using caret
library(mgcv)
library(vtreat)

#GAM formula, based off scatter plots of each explanatory variable vs class variable from EDA
GAMformula <- percentViolentCrimePer1000Population ~ 
                                                  avgSchoolRating + 
                                                  avgSSLRating + 
                                                  s(totalParkArea) +
                                                  numHospitals + #numHospitals or has3OrMoreHospitals
                                                  s(teenMomRate) +
                                                  s(infantMortalityRate) + 
                                                  s(hispanic) + 
                                                  black +
                                                  s(white) + 
                                                  s(asian) + 
                                                  other +
                                                  s(percentChildrenInPov)

gamPredValues <- data.frame("predicted" = rep(0, nrow(dataForPred)))
for(i in 1:k) {
  split <- splitPlan[[i]]
  model <- gam(GAMformula, data = dataForPred[split$train,], family = gaussian)
  gamPredValues$predicted[split$app] <- predict(model, newdata = dataForPred[split$app,])
}
metricsDF[3,] <- c("GAM cv", postResample(gamPredValues$predicted, dataForPred[,1]))

#Building the final model
gamModel <- gam(GAMformula, data = dataForPred, family = gaussian)
finalPredictions <- predict(gamModel, dataForPred[, 2:13])
metricsDF[6,] <- c("GAM", postResample(finalPredictions, dataForPred[,1]))

plot(finalPredictions, dataForPred[,1], main='Predicted vs Actual for GAM Regression', xlab='Predicted', ylab='Actual')
plot(finalPredictions, (dataForPred[,1]-finalPredictions), main='Residual Plot for GAM Regression', xlab='Predicted', ylab='Residuals')
abline(h = 0, col = "darkgrey", lty = 2)
postResample(predict(gamModel, dataForPred[, 2:13]), dataForPred[,1])

#plot all the fitted splines
plot(gamModel,residuals=TRUE,all.terms=TRUE,shade=FALSE)



