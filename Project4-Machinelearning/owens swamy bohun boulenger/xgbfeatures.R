library(readr) # CSV file I/O, e.g. the read_csv function
library(xgboost)

# Reading the data
dat_train <- read.csv("/Users/bohun/Documents/kaggleproject/santandertrain.csv", stringsAsFactors = F)
dat_test <- read.csv("/Users/bohun/Documents/kaggleproject/santandertest.csv", stringsAsFactors = F)

# Merging the test and train data
dat_test$TARGET <- NA
all_dat <- rbind(dat_train, dat_test)

# Removing the constant variables
train_names <- names(dat_train)[-1]
for (i in train_names)
{
  if (class(all_dat[[i]]) == "integer") 
  {
    u <- unique(all_dat[[i]])
    if (length(u) == 1) 
    {
      all_dat[[i]] <- NULL
    } 
  }
}

#Removing duplicate columns
train_names <- names(all_dat)[-1]
fac <- data.frame(fac = integer())    

for(i in 1:length(train_names))
{
  if(i != length(train_names))
  {
    for (k in (i+1):length(train_names)) 
    {
      if(identical(all_dat[,i], all_dat[,k]) == TRUE) 
      {
        fac <- rbind(fac, data.frame(fac = k))
      }
    }
  }
}
same <- unique(fac$fac)
all_dat <- all_dat[,-same]

#Removing hghly correlated variables
cor_v<-abs(cor(all_dat))
diag(cor_v)<-0
cor_v[upper.tri(cor_v)] <- 0
cor_f <- as.data.frame(which(cor_v > 0.85, arr.ind = T))
all_dat <- all_dat[,-unique(cor_f$row)]

# Splitting the data for model
train <- all_dat[1:nrow(dat_train), ]
test <- all_dat[-(1:nrow(dat_train)), ]


#Building the model
#set.seed(88)
param <- list("objective" = "binary:logistic",booster = "gbtree",
              "eval_metric" = "auc",colsample_bytree = 0.85, subsample = 0.95, max_delta_step = 3)

y <- as.numeric(train$TARGET)



bstmodel = xgb.cv(param=param, data = as.matrix(train[,-c(1,151)]), label = y, 
                  nfold = 10, nrounds = 300)


min.merror.idx = which.max(bstmodel[, test.auc.mean]) 
bstmodel[min.merror.idx]



#AUC was highest in 310th round during cross validation
xgbmodel <- xgboost(data = as.matrix(train[,-c(1,151)]), params = param,
                    nrounds = min.merror.idx, max.depth = 5, eta = 0.03,
                    label = y, maximize = T)



#Prediction
res <- predict(xgbmodel, newdata = data.matrix(test[,-c(1,151)]))
res <- data.frame(ID = test$ID, TARGET = res)

write.csv(res, "/Users/bohun/Documents/submission.csv", row.names = FALSE)





















# Mergin the test and train data
dat_test$TARGET <- NA
all_dat <- rbind(dat_train, dat_test)

# Removing the constant variables
train_names <- names(dat_train)[-1]
for (i in train_names)
{
  if (class(all_dat[[i]]) == "integer") 
  {
    u <- unique(all_dat[[i]])
    if (length(u) == 1) 
    {
      all_dat[[i]] <- NULL
    } 
  }
}

#Removing duplicate columns
train_names <- names(all_dat)[-1]
fac <- data.frame(fac = integer())    

for(i in 1:length(train_names))
{
  if(i != length(train_names))
  {
    for (k in (i+1):length(train_names)) 
    {
      if(identical(all_dat[,i], all_dat[,k]) == TRUE) 
      {
        fac <- rbind(fac, data.frame(fac = k))
      }
    }
  }
}
same <- unique(fac$fac)
all_dat <- all_dat[,-same]

#Removing hghly correlated variables
cor_v<-abs(cor(all_dat))
diag(cor_v)<-0
cor_v[upper.tri(cor_v)] <- 0
cor_f <- as.data.frame(which(cor_v > 0.85, arr.ind = T))
all_dat <- all_dat[,-unique(cor_f$row)]

all_dat$var3 ###replace 999999 with most common value 2
ind2 = which(all_dat$var3==-999999)
all_dat$var3[ind2] = 2

all_dat = scale(all_dat)

# Splitting the data for model
train <- all_dat[1:nrow(dat_train), ]
test <- all_dat[-(1:nrow(dat_train)), ]



#Building the model
set.seed(88)
param <- list("objective" = "binary:logistic",booster = "gbtree",
              "eval_metric" = "auc",colsample_bytree = 0.85, subsample = 0.95, "max.delta.step" = 2)

y <- as.numeric(train$TARGET)


bst.cv = xgb.cv(param=param, data = as.matrix(train[,-c(1,151)]), label = y, 
                  nfold = 10, nrounds = 300, stratified=TRUE, early.stop.round = 20)


min.merror.idx = which.max(bst.cv[, test.auc.mean]) 
bst.cv[min.merror.idx]



xgbmodel <- xgboost(data = as.matrix(train[,-c(1,151)]), params = param,
                    nrounds = 1000, max.depth = 5, eta = 0.001, 
                    label = y, maximize = T, early.stop.round = 100)

#Prediction
res <- predict(xgbmodel, newdata = data.matrix(test[,-c(1,151)]))
res <- data.frame(ID = test$ID, TARGET = res)

write.csv(res, "/Users/bohun/Documents/submission.csv", row.names = FALSE)
