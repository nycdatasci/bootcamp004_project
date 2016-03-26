### STANDARDIZING TRAINING DATA

train = read.csv("train.csv", header = TRUE)

response <- train$TARGET
train <-train[-c(1,371)]

#Remove no variance predictors

zero_var = nearZeroVar(train, names=TRUE, freqCut = 95/5,uniqueCut = 10,saveMetrics = TRUE)
train = train[,-which(zero_var$zeroVar)]

train_cat_names = list()
train_num_names = list()

#loop through training data by column / predictor variable

for (i in (1:length(train))){
  if (all(train[,c(i)] == floor(train[,c(i)]))){
    train_cat_names[length(train_cat_names)+1]=(names(train[c(i)]))
  }else{
    train_num_names[length(train_num_names)+1]=(names(train[c(i)]))
  }
}

idx <- match(train_cat_names, names(train))
train_cat = train[,idx]
train_num = train[,-idx]

#change categorical variables to factors

for (j in (1:length(train_cat))){
  train_cat[,c(j)] = as.factor(train_cat[,c(j)])
}

#normalize continuous variables

preproc = preProcess(train_num,method = c("center", "scale"))
train_standardized <- predict(preproc, train_num)


train_standardized = cbind(train_num,train_cat,response)

### STANDARDIZING THE TEST DATA


test = read.csv("test.csv", header = TRUE)

test.id = test$ID
test = test[-c(1)]

test_names = names(test)
# idx_t <- match(test_names, names(train_standardized[-c(336)]))
# test = test[,idx_t]

#Select the same categorical and continuous variables as in the training set

test_cat_names = train_cat_names
test_num_names = train_num_names

#Make categorical and continuous dataframes

idx_t2 <- match(test_cat_names, names(test))
test_cat = test[,idx_t2]

idx_t3 <- match(test_num_names, names(test))
test_num = test[,idx_t3]

#change categorical variables to factors

for (k in (1:length(test_cat))){
  test_cat[,c(k)] = as.factor(test_cat[,c(k)])
}

#normalize continusous variables based on x standardization parameters

test_standardized <- predict(preproc, test_num)

#reconstitute the pre-processed test set

test_standardized = cbind(test_standardized,test_cat)

#Make test set predictions.
preds <- predict(train.boost,newdata=test_standardized,type="prob")  #make the predictions

submission <- data.frame(ID=test.id, TARGET=preds[2])
write.csv(submission, "santander_boosted_tree.csv",row.names=F,quote=F)

