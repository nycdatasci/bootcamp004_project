library(doMC)
library(FSelector)
library(usdm)
library(xgboost)
library(ggplot2)
library(plyr)
library(dplyr)
library(dummies)
library(reshape2)
library(sampling)
library(caret)
library(tidyr)

dat_train = read.csv('ML Projects/Santander/santander_train.csv', stringsAsFactors = F)
dat_test = read.csv('ML Projects/Santander/santander_test.csv', stringsAsFactors = F)

##############################################################################
#  Exploratory Data Analysis
##############################################################################

non_zero <- colSums(matrix(dat_train[, 1:370]!=0)) #Exclue dependent variable
zeros <- colSums(matrix(dat_train[, 1:370]==0))  #Exclue dependent variable
barplot(cbind(non_zero,zeros))

#check % of sparse matrix
nrow(dat_train)
non_zero/(76020*370)
zeros/(76020*370)
#training set is 90.65% zeros

#same drill for the test set
non_zero_test <- colSums(matrix(dat_test!=0))
zeros_test <- colSums(matrix(dat_test==0))
barplot(cbind(non_zero_test,zeros_test))
dim(dat_test)

#check % of sparse matrix
non_zero_test/(75818*370) 
zeros_test/(75818*370)
#test set also ~90.6% sparse

# Merge test and train data
dat_test$TARGET <- NA
all_dat <- rbind(dat_train, dat_test)
summary(dat_train)
head(dat_train)
summary(dat_test)
head(dat_test)
#lots of zeros and wild outliers

#Prepare heat map dataframe
dfz = dat_train[,2:370]
dfsplit = sample(1:nrow(dat_train), .1*nrow(dat_train)/1) #10% test subset
dfz = dfz[dfsplit, ] #split data
dfz_dummy = dfz[ , grep("ind_", names(dfz))] #seperate dummy variables
dfz_dummy[] = "Dummy Variables"
dfz = dfz[ , -grep("ind_", names(dfz))] #non-dummy variable dataframe
dfz[dfz > 999998] = "Outlier"
dfz[dfz < -999998] = "Outlier"
dfz[dfz == 0] = "Zero"
dfz = cbind(dfz, dfz_dummy) #combine dummy variables at the end
dfz[dfz > 0 & dfz < 999998] = "Continuous"
dfz[dfz < 0] = "Continuous"
ID = 1:nrow(dfz)
dfz = cbind(ID, dfz)
dfz = melt(dfz, id.vars = 'ID')

#plot the heat map
df_plot1 = ggplot(dfz, aes(variable, ID))+ 
  geom_tile(aes(fill = value)) + 
  scale_fill_manual(values=c("#4ea1d3", "#8FBC94", "#f94e3f","#F0E5DE")) +
  scale_x_discrete(labels = NULL) +
  theme_minimal() +
  theme(legend.position='bottom',
      legend.direction="horizontal",
      plot.title = element_text(size=20, face="bold", vjust=2))+
  ggtitle("Santander Data Frame (10% Sample)") 

plot(df_plot1)

##############################################################################
#  Feature Engineering
##############################################################################

# Removing the constant variables
train_names <- names(dat_train)[-1]
for (i in train_names)
{ if (class(all_dat[[i]]) == "integer") 
  { u <- unique(all_dat[[i]])
    if (length(u) == 1) 
    { all_dat[[i]] <- NULL
    } } }

#Removing duplicate columns
train_names <- names(all_dat)[-1]
fac <- data.frame(fac = integer())    

for(i in 1:length(train_names))
{ if(i != length(train_names))
  { for (k in (i+1):length(train_names)) 
    { if(identical(all_dat[,i], all_dat[,k]) == TRUE) 
      { fac <- rbind(fac, data.frame(fac = k))
      } } } }
same <- unique(fac$fac)
all_dat <- all_dat[,-same]

#Removing hghly correlated variables
cor_v<-abs(cor(all_dat))
diag(cor_v)<-0
cor_v[upper.tri(cor_v)] <- 0
cor_f <- as.data.frame(which(cor_v > 0.8, arr.ind = T))
all_dat <- all_dat[,-unique(cor_f$row)]

#Prepare heat map II data frame
dfz2 = all_dat[1:nrow(dat_train),2:ncol(all_dat)]
dfsplit = sample(1:nrow(dat_train), .25*nrow(dat_train)/1) 
dfz2 = dfz2[dfsplit, ] 
dfz2_dummy = dfz2[ , grep("ind_", names(dfz2))] 
dfz2_dummy[] = "Dummy Variables"
dfz2 = dfz2[ , -grep("ind_", names(dfz2))] 
dfz2[dfz2 > 999998] = "Outlier"
dfz2[dfz2 < -999998] = "Outlier"
dfz2[dfz2 == 0] = "Zero"
dfz2 = cbind(dfz2, dfz2_dummy)
dfz2[dfz2 > 0 & dfz2 < 999998] = "Continuous"
dfz2[dfz2 < 0] = "Continuous"
ID2 = 1:nrow(dfz2)
dfz2 = cbind(ID2, dfz2)
dfz2 = melt(dfz2, id.vars = 'ID2')

#plot the heat map
ggplot(dfz2, aes(variable, ID2))+ 
  geom_tile(aes(fill = value)) + 
  scale_fill_manual(values=c("#4ea1d3", "#8FBC94", "#f94e3f","#F0E5DE")) +
  scale_x_discrete(labels = NULL) +
  theme_minimal() +
  theme(legend.position='bottom',
        legend.direction="horizontal",
        plot.title = element_text(size=20, face="bold", vjust=2))+
  ggtitle("Data Frame After Feature Engineering (25% Sample)") 

###########################################
# Advanced Feature Engineering for testing

#readjusting outliers
delta_dft = all_dat[ , grep("delta", names(all_dat))]
delta_dft[delta_dft > 9999999] = 5
all_datz = all_dat[ , -which(names(all_dat) %in% names(delta_dft))]
all_datz = cbind(delta_dft, all_datz)
all_datz$var3[all_datz$var3 < 9999999] = -5

# Creating dummy variables from var36
dummy_var36 = dummy(all_datz$var36, drop = T, fun = as.integer)
all_datz$var36 = NULL
all_datz = cbind(dummy_var36, all_datz)

#perform VIF in addition to previous correlation adjustments 
xclude_list = vifstep(all_datz,th = 15)
all_datz = exclude(all_datz, xclude_list)

#Feature importance linear regression using Fselector
weights <- information.gain(TARGET~., df_train)
print(weights)

#Feature importance random forest using Fselector
df_train <- all_datz[1:nrow(dat_train), ]  #just use the training set
weights_rf = random.forest.importance(TARGET~., df_train, importance.type = 1)
print(weights_rf)
weights_rf_df = as.data.frame(weights_rf) %>%
  arrange(., desc(attr_importance))

#Plot feature importance
neg_features = colSums(matrix(weights_rf_df[] < 0))
zero_features = colSums(matrix(weights_rf_df[] == 0))
pos_features = colSums(matrix(weights_rf_df[] > 0))
barplot(cbind(neg_features, zero_features, pos_features), col= c("#79bd9a"))

#slice and dice the dataset depending on your preference
subset <- cutoff.biggest.diff(weights)
f <- as.simple.formula(subset, "TARGET")
print(f)

subset_rf <- cutoff.k(weights_rf, 71)
f <- as.simple.formula(subset_rf, "TARGET")
print(f)

#Create subset of important features (don't forget to add back "TARGET")
subset1 = c(subset_rf, "TARGET")
subset_rf = names(all_datz) %in% subset1
all_datz = all_datz[subset_rf]

#Prepare heat map III data frame
dfz3 = all_datz[1:nrow(dat_train),2:ncol(all_datz)]
dfsplit = sample(1:nrow(dat_train), .5*nrow(dat_train)/1) 
dfz3 = dfz3[dfsplit, ] 
dfz3_dummy = dfz3[ , grep("ind_", names(dfz3))] 
dfz3_dummy[] = "Dummy Variables"
dfz3 = dfz3[ , -grep("ind_", names(dfz3))] 
dfz3[dfz3 > 999998] = "Outlier"
dfz3[dfz3 < -999998] = "Outlier"
dfz3[dfz3 == 0] = "Zero"
dfz3 = cbind(dfz3, dfz3_dummy)
dfz3[dfz3 > 0 & dfz3 < 999998] = "Continuous"
dfz3[dfz3 < 0] = "Continuous"
ID3 = 1:nrow(dfz3)
dfz3 = cbind(ID3, dfz3)
dfz3 = melt(dfz3, id.vars = 'ID3')

#plot the heat map
ggplot(dfz3, aes(variable, ID3))+ 
  geom_tile(aes(fill = value)) + 
  scale_fill_manual(values=c("#4ea1d3", "#8FBC94", "#f94e3f","#F0E5DE")) +
  scale_x_discrete(labels = NULL) +
  theme_minimal() +
  theme(legend.position='bottom',
        legend.direction="horizontal",
        plot.title = element_text(size=14, face="bold", vjust=2))+
  ggtitle("Data Frame after Advanced Feature Engineering (50%)") 

##############################################################################
#  XGB Models
##############################################################################

####################################################################################
#set up parallel computing
detectCores() #4 cores
getDoParWorkers() #only 1 core is currently per default

#test for parallel computing
registerDoMC(cores = 4)
ptime <- system.time(xgb_model())

#test for sequential computing
registerDoMC(cores = 1)
stime <- system.time(xgb_model())

#plot the difference between parallel and sequential computing
timing <- rbind(sequential = stime, parallel = ptime)
as.data.frame(timing)
barplot(timing[, "elapsed"], col=c("#4ea1d3", "#8FBC94"), ylab="Elapsed time [s]")

##############################################################################
# Model 1 with basic engineering
registerDoMC(cores = 4)

#Splitting all_dat xgb
train <- all_dat[1:nrow(dat_train), ]
test <- all_dat[-(1:nrow(dat_train)), ]
dim(train)
train = mutate(train, TARGET_Output = factor(TARGET,labels = c("Failure", "Success")))
set.seed(50)

# set up the cross-validated hyper-parameter search
xgb_grid1 = expand.grid(
  nrounds = 100,
  eta = c(.15,.1,.05, .03, .01, .005),
  max_depth = c(8,7,6,5),
  gamma = 1,
  colsample_bytree = c(1,.95,.9,.85,.8), 
  min_child_weight = 1 
)

# training control parameters
xgb_trcontrol_1 = trainControl(
  method = "cv",
  number =4,
  verboseIter = TRUE,
  returnData = FALSE,
  returnResamp = "all",                                                        
  classProbs = TRUE,                                                           
  summaryFunction = twoClassSummary,
  allowParallel = TRUE
)

# train the model for each parameter combination in the grid, 
# using CV to evaluate

xgb_model <-  function() {train(
  x = as.matrix(select(train, -TARGET_Output, -TARGET)),
  y = as.factor(train$TARGET_Output),
  trControl = xgb_trcontrol_1,
  tuneGrid = xgb_grid1,
  method = "xgbTree"
)  }

# Run through full grid search
xgb_model1 = xgb_model()

# scatter plot of the AUC against max_depth and eta
ggplot(xgb_model2$results, aes(x = as.factor(eta), y = max_depth, color = ROC)) + 
  geom_point(size = 25) + 
  theme_minimal() +
  scale_colour_gradient(limits = c(.825,.839), low = "#2b90d9", high = "#fcbe32")

# best model
set.seed(50)
xgb_params_bst1 = list(
  objective = "binary:logistic",                                              
  eta = 0.03,
  max.depth = 5,  # max tree depth
  gamma = 1,
  colsample_bytree = .85,
  subsample = .5,
  eval_metric = "auc" )

# cross-validate xgboost to get the accurate measure of error
xgb_cv_1 = xgb.cv(params = xgb_params_bst1,
                  data = as.matrix(train %>%
                                     select(-TARGET_Output, -TARGET)),
                  label = train$TARGET,
                  nrounds = 1000, 
                  nfold = 5,                                                   
                  prediction = TRUE,                                           
                  showsd = TRUE,                                               
                  stratified = TRUE,                                           
                  verbose = TRUE,
                  print.every.n = 1, 
                  early.stop.round = 40,
                  booster = "gbtree")

# plot the AUC for the training and testing samples
xgb_cv_1$dt %>%
  select(-contains("std")) %>%
  mutate(IterationNum = 1:n()) %>%
  gather(TestOrTrain, AUC, -IterationNum) %>%
  ggplot(aes(x = IterationNum, y = AUC, group = TestOrTrain, color = TestOrTrain)) + 
  geom_line() + 
  theme_bw()

# fit the final model
xgb_1 = xgboost(data = as.matrix(train %>%
                                   select(-TARGET_Output, -TARGET)),
                label = train$TARGET,
                params = xgb_params_bst1,
                nrounds = 150,
                verbose = TRUE,                                         
                print.every.n = 1,
                early.stop.round = 25 )

xgb.importance(xgb_1)
##############################################################################
# Model with Advanced Engineering

#Splitting all_dat xgb
trainz <- all_datz[1:nrow(dat_train), ]
testz <- all_datz[-(1:nrow(dat_train)), ]

trainz = mutate(trainz, TARGET_Output = factor(TARGET,labels = c("Failure", "Success")))
set.seed(50)

# set up the cross-validated hyper-parameter search
xgb_grid2 = expand.grid(
  nrounds = 500,
  eta = c(.15, .1, .05, .03, .01),
  max_depth = c(7,6,5,4),
  gamma = 1,
  colsample_bytree = c(.95, .9, .85,.8,.75), 
  min_child_weight = 1 
)

# pack the training control parameters
xgb_trcontrol_2 = trainControl(
  method = "cv",
  number =4,
  verboseIter = TRUE,
  returnData = FALSE,
  returnResamp = "all",                                                        
  classProbs = TRUE,                                                           
  summaryFunction = twoClassSummary,
  allowParallel = TRUE
)

# train the model for each parameter combination in the grid, 
# using CV to evaluate

xgb_model2 <-  function() {train(
  x = as.matrix(select(trainz, -TARGET_Output, -TARGET)),
  y = as.factor(trainz$TARGET_Output),
  trControl = xgb_trcontrol_2,
  tuneGrid = xgb_grid2,
  method = "xgbTree"
)  }

####################################################################################

# Run through full grid search
xgb_model2 = xgb_model2()

# scatter plot of the AUC against max_depth and eta
ggplot(xgb_model2$results, aes(x = as.factor(eta), y = max_depth, color = ROC)) + 
  geom_point(size = 20) + 
  theme_minimal() +
  scale_colour_gradient(limits = c(.8145,.836), low = "#2b90d9", high = "#fcbe32")

#######################################################
# best model
set.seed(50)
xgb_params_bst2 = list(
  objective = "binary:logistic",                                              
  eta = 0.03,
  max.depth = 4,  # max tree depth
  gamma = 1,
  colsample_bytree = .75,
  subsample = .5,
  eval_metric = "auc" )

# cross-validate xgboost to get the accurate measure of error
xgb_cv_2 = xgb.cv(params = xgb_params_bst2,
                  data = as.matrix(trainz %>%
                                     select(-TARGET_Output, -TARGET)),
                  label = trainz$TARGET,
                  nrounds = 1000, 
                  nfold = 5,                                                   
                  prediction = TRUE,                                           
                  showsd = TRUE,                                               
                  stratified = TRUE,                                           
                  verbose = TRUE,
                  print.every.n = 1, 
                  early.stop.round = 50,
                  booster = "gbtree" )

# plot the AUC for the training and testing samples
xgb_cv_2$dt %>%
  select(-contains("std")) %>%
  mutate(IterationNum = 1:n()) %>%
  gather(TestOrTrain, AUC, -IterationNum) %>%
  ggplot(aes(x = IterationNum, y = AUC, group = TestOrTrain, color = TestOrTrain)) + 
  geom_line() + 
  theme_bw()

# fit the final model
xgb_2 = xgboost(data = as.matrix(trainz %>%
                                   select(-TARGET_Output, -TARGET)),
                label = trainz$TARGET,
                params = xgb_params_bst2,
                nrounds = 225,
                verbose = TRUE,                                         
                print.every.n = 1,
                early.stop.round = 25                                          
)


res <- predict(xgb_2, newdata = data.matrix(testz), missing = NaN)
res <- data.frame(ID = dat_test$ID, TARGET = res)
write.csv(res, "ML Projects/Santander/submissionXGB7.csv", row.names = FALSE)

##############################################################################
#  Leftovers


##############################################################################
#  Naive Bayes Classifier
##############################################################################

train_nb_all <- all_dat[1:nrow(dat_train), ]
dfsplit = sample(1:nrow(train_nb_all), 7*nrow(train_nb_all)/10) #70% test subset
train_nb = train_nb_all[dfsplit, ]
test_nb = train_nb_all[-dfsplit, ]
train_labels = as.factor(train_nb$TARGET)
test_labels = as.factor(test_nb$TARGET)

classifier2 = naiveBayes(train_nb, train_labels, laplace = 1)
pred2 = predict(classifier2, test_nb)
table(pred2, test_labels)

model = train(train_nb,train_labels,'nb',trControl=trainControl(method='cv',number=10))

#Evaluating the model performance by predicting the test observations.
preds = predict(model$finalModel,test_nb)
preds2 = as.data.frame(preds)
round(preds2$posterior.1,3)
prop.table(table(preds$class,test_labels))

#predict on the test set
test_nb_all <- all_dat_nb[-(1:nrow(dat_train)), ]
test_nb_pred <- predict(model$finalModel, test_nb_all)

################### Shitty results!!!!! 

#### fselector
subset_rf <- cutoff.k.percent(weights_rf, .5)
f <- as.simple.formula(subset_rf, "TARGET")
print(f)

