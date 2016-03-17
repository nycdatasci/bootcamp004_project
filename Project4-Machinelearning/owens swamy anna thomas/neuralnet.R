###########################################
###########################################
#####[14] Neural Networks Lecture Code#####
###########################################
###########################################



###################################
#####Tools for Neural Networks#####
###################################
#Reading in the data and inspecting its contents.
train = read.csv("/Users/bohun/Documents/kaggleproject/filteredtrain.csv", sep=',', stringsAsFactors = FALSE)
train$X = NULL


train = train2[1:700,]
test = train2[701:800,]
plot(train$var15, train$var3, col = train$TARGET)

x = paste(names(train[,2:370]), collapse = " + ")
x = paste("TARGET ~ ", x, collapse = "")

#Ainear kernel will fail in this scenario. Let's try using a radial kernel.
svm.radial = svm(as.formula(x),
                 data = train,
                 kernel = "radial",
                 cost = 100,
                 gamma = 0.5)

ypred = predict(svm.radial, test)
ypred2 = (ypred - min(ypred))/(max(ypred)-min(ypred))
table("Predicted Values" = ifelse(ypred2>0.999, 1, 0), "True Values" = test$TARGET)




#We notice that our data range from values of 0 to upwards of 1,000; neural
#networks work best when we account for the differences in our variables and
#scale accordingly such that values are close to 0. Let's define our own
#normalize function as follows:
normalize = function(x) { 
  return((x - min(x)) / (max(x) - min(x)))
}

#We now apply our normalization function to all the variables within our dataset;
#we store the result as a data frame for future manipulation.
train_norm = as.data.frame(lapply(train, normalize))

#Inspecting the output to ensure that the range of each variable is now between
#0 and 1.
summary(train_norm)

#Since the data has already been organized in random order, we can simply split
#our data into training and test sets based on the indices of the data frame;
#here we create a 75% training set and 25% testing set split.
concrete_train = train_norm[1:773, ]
concrete_test = train_norm[774:1030, ]

#Verifying that the split has been successfully made into 75% - 25% segments.
nrow(concrete_train)/nrow(concrete_norm)
nrow(concrete_test)/nrow(concrete_norm)

#Loading the neuralnet library for the training of neural networks.
library(neuralnet)

#Training the simplest multilayer feedforward neural network that includes only
#one hidden node.
set.seed(0)
x = paste(names(train[,2:370]), collapse = " + ")
x = paste("TARGET ~ ", x, collapse = "")
concrete_model = neuralnet(as.formula(x),
                           hidden = c(15, 3, 4), #Default number of hidden neurons.
                           data = concrete_train)

#Visualizing the network topology using the plot() function.
plot(concrete_model)

#Generating model predictions on the testing dataset using the compute()
#function.
model_results = compute(concrete_model, concrete_test[, 1:8])

#The model_results object stores the neurons for each layer in the network and
#also the net.results which stores the predicted values; obtaining the
#predicted values.
predicted_strength = model_results$net.result

#Examining the correlation between predicted and actual values.
cor(predicted_strength, concrete_test$strength)
plot(predicted_strength, concrete_test$strength)

#Attempting to fit a more complex neural network topology with 5 hidden neurons;
#takes about 10 seconds to run.
set.seed(0)
concrete_model2 = neuralnet(strength ~ cement + slag +
                              ash + water + superplastic +
                              coarseagg + fineagg + age,
                            hidden = 5,
                            data = concrete_train)

#Visualizing the network topology using the plot() function.
plot(concrete_model2)

#Generating model predictions on the testing dataset using the compute()
#function; obtaining the predicted values.
model_results2 = compute(concrete_model2, concrete_test[, 1:8])
predicted_strength2 = model_results2$net.result

#Evaluating the model performance on the test set.
cor(predicted_strength2, concrete_test$strength)
plot(predicted_strength2, concrete_test$strength)

#Can create even more complex models by varying the hidden parameter.
set.seed(0)
concrete_model3 = neuralnet(strength ~ cement + slag +
                              ash + water + superplastic +
                              coarseagg + fineagg + age,
                            hidden = c(15, 3, 4),
                            data = concrete_train)

#Visualizing the network topology using the plot() function.
plot(concrete_model3)

#Generating model predictions on the testing dataset using the compute()
#function; obtaining the predicted values.
model_results3 = compute(concrete_model3, concrete_test[, 1:8])
predicted_strength3 = model_results3$net.result

#Evaluating the model performance on the test set.
cor(predicted_strength3, concrete_test$strength)
plot(predicted_strength3, concrete_test$strength)

#Model 1:
#-1 hidden layer.
#-1 hidden node.
#-Error of 5.082.
#-1,663 steps.
#-Correlation of 0.806.

#Model 2:
#-1 hidden layer.
#-5 hidden nodes.
#-Error of 1.612.
#-25,383 steps.
#-Correlation of 0.928.

#Model 3:
#-3 hidden layers.
#-15, 3, and 4 hidden nodes.
#-Error of 0.709.
#-9,239 steps.
#-Correlation of 0.951.



###################################################
###################################################
#####[10] Support Vector Machines Lecture Code#####
###################################################
###################################################



###################################
#####Maximal Margin Classifier#####
###################################
#Generating linearly separable data.
set.seed(0)
x1 = c(rnorm(100, 0, 4), rnorm(100, 1, 3))
x2 = c(rnorm(100, 0, 1), rnorm(100, 6, 1))
y = as.factor(c(rep(-1, 100), rep(1, 100)))
linearly.separable = data.frame(x1, x2, y)

#Plotting the linearly separable data.
plot(linearly.separable$x1, linearly.separable$x2, col = linearly.separable$y)

#Creating training and test sets.
set.seed(0)
train.index = sample(1:200, 200*.8)
test.index = -train.index

#Importing the e1071 library in order to use the svm() function to fit support
#vector machines.
library(e1071)

#Fitting a maximal margin classifier to the training data.
svm.mmc.linear = svm(y ~ ., #Familiar model fitting notation.
                     data = linearly.separable, #Using the linearly separable data.
                     subset = train.index, #Using the training data.
                     kernel = "linear", #Using a linear kernel.
                     cost = 1e6) #A very large cost; default is 1.

#Visualizing the results of the maximal margin classifier.
plot(svm.mmc.linear, linearly.separable[train.index, ])

#Additional information for the fit.
summary(svm.mmc.linear)

#Finding the indices of the support vectors.
svm.mmc.linear$index

#Predicting on the test data.
ypred = predict(svm.mmc.linear, linearly.separable[test.index, ])
table("Predicted Values" = ypred, "True Values" = linearly.separable[test.index, "y"])

#Adding a single point to display the sensitivity of the maximal margin classifier.
linearly.separable2 = rbind(linearly.separable, c(-5, 3, 1))
plot(linearly.separable2$x1, linearly.separable2$x2, col = linearly.separable2$y)

#Fitting a maximal margin classifier to the new data.
svm.mmc.linear2 = svm(y ~ .,
                      data = linearly.separable2,
                      kernel = "linear",
                      cost = 1e6)

#Visualizing the results of the maximal margin classifier; comparing the output.
plot(svm.mmc.linear, linearly.separable[train.index, ]) #Old model.
plot(svm.mmc.linear2, linearly.separable2) #New model.

#Additional information for the fit.
summary(svm.mmc.linear2)

#Finding the indices of the support vectors.
svm.mmc.linear2$index



###################################
#####Support Vector Classifier#####
###################################
#Fitting a support vector classifier by reducing the cost of a misclassified
#observation.
library(e1071)

svm.svc.linear2 = svm(TARGET ~ var38 + saldo_medio_var44_hace3,
                      data = concrete_train,
                      kernel = "radial",
                      cost = 100)

#Visualizing the results of the support vector classifier.
plot(svm.svc.linear2, linearly.separable2)
summary(svm.svc.linear2)
svm.svc.linear2$index



#What happens if we reduce the cost even more?
svm.svc.linear3 = svm(y ~ .,
                      data = linearly.separable2,
                      kernel = "linear",
                      cost = .1)
plot(svm.svc.linear3, linearly.separable2)
summary(svm.svc.linear3)
svm.svc.linear3$index
3   6  11  15  21  25  30  34  37  43  53  54  64  66  72  78  86  92 106 111 113 120 129 140 150
[26] 160 166 168 171 178 182 194 196 197 201

#We generally find the best cost parameter by implementing the cross-validation
#procedure; this isn't as interesting with linearly separable data. Let's generate
#some data that is not linearly separable.
set.seed(0)
x1 = c(rnorm(100, -1, 1), rnorm(100, 1, 1))
x2 = c(rnorm(100, -1, 1), rnorm(100, 1, 1))
y = as.factor(c(rep(-1, 100), rep(1, 100)))
overlapping = data.frame(x1, x2, y)
plot(overlapping$x1, overlapping$x2, col = overlapping$y)

#Implement cross-validation to select the best parameter value of the cost.
set.seed(0)
cv.svm.overlapping = tune(svm,
                          y ~ .,
                          data = overlapping[train.index, ],
                          kernel = "linear",
                          ranges = list(cost = 10^(seq(-5, .5, length = 100))))

#Inspecting the cross-validation output.
summary(cv.svm.overlapping)
0.005994843

- best performance: 0.0625 

- Detailed performance results:
  cost   error dispersion
1   1.000000e-05 0.48125 0.08359334
2   1.136464e-05 0.48125 0.08359334
3   1.291550e-05 0.48125 0.08359334


#Plotting the cross-validation results.
plot(cv.svm.overlapping$performances$cost,
     cv.svm.overlapping$performances$error,
     xlab = "Cost",
     ylab = "Error Rate",
     type = "l")

#Inspecting the best model.
best.overlapping.model = cv.svm.overlapping$best.model
summary(best.overlapping.model)


Parameters:
  SVM-Type:  C-classification 
SVM-Kernel:  linear 
cost:  0.005994843 
gamma:  0.5 

Number of Support Vectors:  132

( 66 66 )


#Using the best model to predict the test data.
ypred = predict(best.overlapping.model, overlapping[test.index, ])
table("Predicted Values" = ypred, "True Values" = overlapping[test.index, "y"])

#Constructing and visualizing the final model.
svm.best.overlapping = svm(y ~ .,
                           data = overlapping,
                           kernel = "linear",
                           cost = best.overlapping.model$cost)
plot(svm.best.overlapping, overlapping)
summary(svm.best.overlapping)
svm.best.overlapping$index
ypred = predict(svm.best.overlapping, overlapping)
table("Predicted Values" = ypred, "True Values" = overlapping[, "y"])



#################################
#####Support Vector Machines#####
#################################
#What happens if we have data that is not linearly separable?
set.seed(0)
x1 = c(rnorm(100, 2), rnorm(100, -2), rnorm(100))
x2 = c(rnorm(100, 2), rnorm(100, -2), rnorm(100))
y = as.factor(c(rep(-1, 200), rep(1, 100)))
nonlinear = concrete_train[,c('var38', 'var3', 'TARGET')]
train = nonlinear[1:700,]
test = nonlinear[701:773,]
plot(nonlinear$var38, nonlinear$var3, col = concrete_train$TARGET)

#Ainear kernel will fail in this scenario. Let's try using a radial kernel.
svm.radial = svm(as.formula(x),
                 data = train_norm[1:70000,],
                 kernel = "radial",
                 cost = 100,
                 gamma = 0.5)


ypred = predict(svm.radial, test)
ypred2 = (ypred - min(ypred))/(max(ypred)-min(ypred))
table("Predicted Values" = ifelse(ypred2>0.96, 1, 0), "True Values" = test$TARGET)




#Visualizing the results of the support vector machine
plot(svm.radial, nonlinear)
summary(svm.radial)
svm.radial$index

#What happens if we make gamma small?
svm.radial.smallgamma = svm(y ~ .,
                            data = nonlinear,
                            kernel = "radial",
                            cost = 1,
                            gamma = .05)
plot(svm.radial.smallgamma, nonlinear)
summary(svm.radial.smallgamma)
svm.radial.smallgamma$index

#What happens if we make gamma large?
svm.radial.largegamma = svm(y ~ .,
                            data = nonlinear,
                            kernel = "radial",
                            cost = 1,
                            gamma = 10)
plot(svm.radial.largegamma, nonlinear)
summary(svm.radial.largegamma)
svm.radial.largegamma$index

#Let's use cross-validation to figure out the best combination of the tuning
#parameters.

#Creating training and test sets.
set.seed(0)
train.index = sample(1:300, 300*.8)
test.index = -train.index

#Performing the cross-validation.
#CAUTION: Will take about 30 seconds.
set.seed(0)
cv.svm.radial = tune(svm,
                     y ~ .,
                     data = nonlinear[train.index, ],
                     kernel = "radial",
                     ranges = list(cost = 10^(seq(-1, 1.5, length = 20)),
                                   gamma = 10^(seq(-2, 1, length = 20))))

#Inspecting the cross-validation output.
summary(cv.svm.radial)
best parameters:
  cost    gamma
9.41205 1.128838

- best performance: 0.08333333 


#Plotting the cross-validation results.
library(rgl)
plot3d(cv.svm.radial$performances$cost,
       cv.svm.radial$performances$gamma,
       cv.svm.radial$performances$error,
       xlab = "Cost",
       ylab = "Gamma",
       zlab = "Error",
       type = "s",
       size = 1)

#Inspecting the best model.
best.nonlinear.model = cv.svm.radial$best.model
summary(best.nonlinear.model)
rnel:  radial 
cost:  9.41205 
gamma:  1.128838 

Number of Support Vectors:  68


#Using the best model to predict the test data.
ypred = predict(svm.radial, nonlinear)
ypred2 = (ypred - min(ypred))/(max(ypred)-min(ypred))
table("Predicted Values" = ypred, "True Values" = nonlinear)

#Constructing and visualizing the final model.
svm.best.nonlinear = svm(y ~ .,
                         data = nonlinear,
                         kernel = "radial",
                         cost = best.nonlinear.model$cost,
                         gamma = best.nonlinear.model$gamma)
plot(svm.best.nonlinear, overlapping)
summary(svm.best.nonlinear)
svm.best.nonlinear$index

ypred = predict(svm.best.nonlinear, nonlinear)
table("Predicted Values" = round(ypred), "True Values" = test$TARGET)



########################################
#####Multi-Class SVM Classification#####
########################################
#Creating multi-class data.
set.seed(0)
x1 = c(rnorm(100, 2), rnorm(100, -2), rnorm(100), rnorm(100, 2))
x2 = c(rnorm(100, 2), rnorm(100, -2), rnorm(100), rnorm(100, -2))
y = as.factor(c(rep(-1, 200), rep(1, 100), rep(2, 100)))
multi = data.frame(x1, x2, y)
plot(multi$x1, multi$x2, col = multi$y)

#Creating training and test sets.
set.seed(0)
train.index = sample(1:400, 400*.8)
test.index = -train.index

#Performing the cross-validation.
#CAUTION: Will take about 45 seconds.
set.seed(0)
cv.multi = tune(svm,
                y ~ .,
                data = multi[train.index, ],
                kernel = "radial",
                ranges = list(cost = 10^(seq(-1, 1.5, length = 20)),
                              gamma = 10^(seq(-2, 1, length = 20))))

#Inspecting the cross-validation output.
summary(cv.multi)

#Plotting the cross-validation results.
plot3d(cv.multi$performances$cost,
       cv.multi$performances$gamma,
       cv.multi$performances$error,
       xlab = "Cost",
       ylab = "Gamma",
       zlab = "Error",
       type = "s",
       size = 1)

#Inspecting the best model.
best.multi.model = cv.multi$best.model
summary(best.multi.model)

#Using the best model to predict the test data.
ypred = predict(best.multi.model, multi[test.index, ])
table("Predicted Values" = ypred, "True Values" = multi[test.index, "y"])

#Constructing and visualizing the final model.
svm.best.multi = svm(y ~ .,
                     data = multi,
                     kernel = "radial",
                     cost = best.multi.model$cost,
                     gamma = best.multi.model$gamma)
plot(svm.best.multi, multi)
summary(svm.best.multi)
svm.best.multi$index
ypred = predict(svm.best.multi, multi)
table("Predicted Values" = ypred, "True Values" = multi[, "y"])







library(dplyr)
library(data.table)
library(xgboost)
df <- read.csv("/Users/bohun/Documents/kaggleproject/santandertrain.csv",stringsAsFactors = FALSE)
features = c("var38", "saldo_medio_var5_hace3", "var15", "saldo_medio_var5_hace2",
             "num_var45_ult1",
             "num_var45_ult3", "imp_trans_var37_ult1", "saldo_var5", "imp_ent_var16_ult1", 
             "saldo_medio_var5_ult3", "TARGET")
df = df[, features]
df = as.data.table(df)
df[,ID:=NULL]
df=na.omit(df)

nameLastCol <- names(df)[ncol(df)]
y <- df[, nameLastCol, with = F][[1]]
df[, nameLastCol:=NULL, with = F]


trainMatrix <- as.matrix(df)
numberOfClasses <- max(y) + 1

param <- list("objective" = "binary:logistic",
              "eval_metric" = "auc",
              "num_class" = numberOfClasses)

cv.nround <- 5
cv.nfold <- 3

bst.cv = xgb.cv(param=param, data = trainMatrix, label = y, 
                nfold = cv.nfold, nrounds = 200, scale_pos_weight=TRUE,
                max.depth = 10, eta = 0.01,
                stratified = TRUE, missing =NULL)


nround = 50
bst = xgboost(param=param, data = trainMatrix, label = y, nrounds=10)


names <- dimnames(trainMatrix)[[2]]

importance_matrix <- xgb.importance(names, model = bst)

# Nice graph
xgb.plot.importance(importance_matrix[1:10,])







