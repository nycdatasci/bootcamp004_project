# title: "Santander Customer Satisfaction Kaggle Competition"
# author: "Sricharan Maddineni"
# date: "March 14, 2016"

test <- read.csv("test.csv", stringsAsFactors = F)
train <- read.csv("train.csv", stringsAsFactors = F)

library(xgboost)

# feature importance plot
xgb.plot.importance(importance_matrix[1:10,])
bst <- xgb.train(data=dtrain, max.depth=5.5, eta=.02, nthread = 2, nround=5, watchlist=watchlist, objective = "binary:logistic")
xgb.dump(bst, with.stats = T)
xgb.plot.tree(model = bst)
importance_matrix <- xgb.importance(model = bst)
xgb.plot.importance(importance_matrix = importance_matrix)

#5 FOLD Cross Validation
library(caret)
set.seed(333)
folds<-createFolds(train$TARGET, k=5, list=F, returnTrain=F)

finaldf_3<-data.frame(ID=NA, TARGET=NA)
for(i in 1:5){
    ktrain<-train[folds!=i,]
    ktest<-train[folds==i,]
    train.y <- ktrain$TARGET
    ktrain$TARGET <- NULL
    ktrain$TARGET <- train.y
    train_new <- sparse.model.matrix(TARGET ~ ., data = ktrain)
    
    dtrain <- xgb.DMatrix(data=train_new, label=train.y)
    watchlist <- list(train=dtrain)
    
    param <- list(  objective           = "binary:logistic", 
                    booster             = "gbtree",
                    eval_metric         = "auc",
                    eta                 = 0.02,
                    max_depth           = 5,
                    subsample           = .9,
                    colsample_bytree    = .87,
                    min_child_weight    = 5,
                    scale_pos_weight    = 1,
                    max_delta_step      = 5
    )
    
    clf <- xgb.train(   params              = param, 
                        data                = dtrain, 
                        nrounds             = 750, 
                        verbose             = 1,
                        watchlist           = watchlist,
                        maximize            = FALSE
                        
    )
    test.id <- ktest$ID
    ktest$TARGET <- -1
    test <- sparse.model.matrix(TARGET ~ ., data = ktest)
    
    preds <- predict(clf, test)
    submission3 <- data.frame(ID=test.id, TARGET=preds)
    finaldf_3 <- rbind(finaldf_3, submission3)
}
write.csv(finaldf_3, "xgb15_train.csv", row.names = F)


################################################
### NEW test & train for Ensamble stats here ###
################################################

newtrain <- cbind(train, nn=wendynn.train[,2], knn=wendyknn[,2], knn19=wendy[,2],knn8=wendy2[,2],knn4=wendy3[,2],knn2=wendy4[,2], mike=mike[,2])
newtest<-cbind(test, nn=wendynn.test[,2], knn=wendyknn[,2], knn19=wendy[,2], knn8=wendy2[,2],knn4=wendy3[,2],knn2=wendy4[,2], mike=mike[,2])
#write.csv(newtrain, "newtrain.csv")

### ENSAMBLE
train.y <- newtrain$TARGET
newtrain$TARGET <- NULL
newtrain$TARGET <- train.y
train_new <- sparse.model.matrix(TARGET ~ ., data = newtrain)

dtrain <- xgb.DMatrix(data=train_new, label=train.y)
watchlist <- list(train=dtrain)

param <- list(  objective           = "binary:logistic", 
                booster             = "gbtree",
                eval_metric         = "auc",
                eta                 = 0.02,
                max_depth           = 5,
                subsample           = .85,
                colsample_bytree    = .85,
                min_child_weight    = 1,
                scale_pos_weight    = 1
)

clf <- xgb.train(   params              = param, 
                    data                = dtrain, 
                    nrounds             = 500, 
                    verbose             = 1,
                    watchlist           = watchlist,
                    maximize            = FALSE
                    
)

test.id <- test$ID
newtest$TARGET <- -1
test <- sparse.model.matrix(TARGET ~ ., data = newtest)

preds <- predict(clf, test)
submission19 <- data.frame(ID=test.id, TARGET=preds)

write.csv(submission19, "ens_13_test.csv", row.names=F, quote=F)

