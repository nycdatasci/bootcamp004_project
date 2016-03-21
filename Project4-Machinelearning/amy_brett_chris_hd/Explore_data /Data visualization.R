setwd("~/Documents/kaggle home depot/new features and data vis")
# load the dataset
df= read.csv("df_all.csv",stringsAsFactors=FALSE)
train= read.csv("train.csv",stringsAsFactors=FALSE)
test= read.csv("test.csv",stringsAsFactors=FALSE)
dscrp=read.csv("product_descriptions.csv",stringsAsFactors = FALSE)
attr=read.csv("attributes.csv",stringsAsFactors = FALSE)
# need to separte the data into test and training data sets
dim(train)
# [1] 74067     6
n=nrow(train)
train_df=df[1:n,]
test_df=df[(n+1):nrow(df),]
# get the y and id
train_y=train$relevance
train_id=train$id
test_id=test$id 
train_pid = unique(train[,2])
test_pid = unique(test[,2])
dscrp_pid=unique(dscrp[,1])
attr_pid=unique(attr[,1])

# the data contains same products
#install.packages('VennDiagram')
library(VennDiagram)
grid.newpage()
venn.plot1<-draw.pairwise.venn(area1 =length(train_pid) , 
                   area2 = length(test_pid), 
                   cross.area = length(intersect(train_pid,test_pid)), 
                   category = c("Train Data", "Test Data"),
                   lty = rep("blank", 2), 
                   fill = c("light blue", "pink"),
                   alpha = rep(0.5, 2), 
                   cat.pos = c(0,0), 
                   cat.dist = rep(0.025, 2))

# How many has description?
# All of them has description
grid.newpage()
draw.triple.venn(area1 =length(train_pid) , 
                   area2 = length(test_pid), 
                   area3 = length(dscrp_pid),
                   n12 = length(intersect(train_pid,test_pid)), 
                   n13= length(intersect(train_pid,dscrp_pid)),
                   n23=length(intersect(test_pid,dscrp_pid)),
                   n123=length(intersect(dscrp_pid,intersect(test_pid,train_pid))),
                   category = c("Train Data", "Test Data","Descriptions"),
                   lty = "blank", 
                   fill = c("skyblue", "pink1", "mediumorchid"),
                   alpha = rep(0.5, 3))
# how many has attributes' information
grid.newpage()
draw.triple.venn(area1 =length(train_pid) , 
                 area2 = length(test_pid), 
                 area3 = length(attr_pid),
                 n12 = length(intersect(train_pid,test_pid)), 
                 n13= length(intersect(train_pid,attr_pid)),
                 n23=length(intersect(test_pid,attr_pid)),
                 n123=length(intersect(attr_pid,intersect(test_pid,train_pid))),
                 category = c("Train Data", "Test Data","Attributes"),
                 lty = "blank", 
                 fill = c("skyblue", "pink1", "orange"),
                 alpha = c(0.5, 0.5,0.3))
# discover the attribute data
library(dplyr)
plot(table(attr$name))
a<-data.frame(table(attr$name))
a=a %>% arrange(desc(Freq))
head(a,10)
# > head(a,10)
# Var1  Freq
# 1        MFG Brand Name 86250
# 2              Bullet02 86248
# 3              Bullet03 86226
# 4              Bullet04 86174
# 5              Bullet01 85940
# 6   Product Width (in.) 61137
# 7              Bullet05 60529
# 8  Product Height (in.) 54698
# 9   Product Depth (in.) 53652
# 10 Product Weight (lb.) 45175

# how many has brand name?
# not all of them have brand names.
attr_type<-unique(attr$name)   # there are 5411 attributes
length(attr_type)
sum(attr$name=="MFG Brand Name")  # 86250 products have their brand name 
attr_bpid<-unique(attr[attr$name=="MFG Brand Name",]$product_uid)

grid.newpage()
draw.triple.venn(area1 =length(train_pid) , 
                 area2 = length(test_pid), 
                 area3 = length(attr_bpid),
                 n12 = length(intersect(train_pid,test_pid)), 
                 n13= length(intersect(train_pid,attr_bpid)),
                 n23=length(intersect(test_pid,attr_bpid)),
                 n123=length(intersect(attr_bpid,intersect(test_pid,train_pid))),
                 category = c("Test Data", "Train Data","Brand Data"),
                 lty = "blank", 
                 fill = c("skyblue", "pink1", "red"),
                 alpha = c(0.5, 0.5,0.3))
#--------------------------------------------------------------------------------------------------------#
                                # Numeric Data Analysis#
#--------------------------------------------------------------------------------------------------------#
summary(train_df)
train_x=train_df[,10:26]
test_x=test_df[,10:26]

#--------------------------------------------------------------------------------------------------------#
# Linear L1,L2 regession
#--------------------------------------------------------------------------------------------------------#
grid = 10^seq(5, -2, length = 100) # create a vector of lambdas
data1=train_df[,c(5,10:21,23:26)]
# Build model
x = model.matrix(relevance~ .,data1)[, -1] #Dropping the intercept column.
y = data1$relevance
#Fitting the ridge regression. Alpha = 0 for ridge regression.
library(glmnet) 
ridge.models = glmnet(x,y, alpha = 0, lambda = grid) # alpha=0 ridge, alpha=1 lasso

dim(coef(ridge.models)) #20 different coefficients, estimated 100 times --
#once each per lambda value.
coef(ridge.models) #Inspecting the various coefficient estimates.

#What do the estimates look like for a smaller value of lambda?
ridge.models$lambda[80] #Lambda = 0.2595.
coef(ridge.models)[, 80] #Estimates not close to 0.
sqrt(sum(coef(ridge.models)[-1, 80]^2)) #L2 norm is 136.8179. 

#What do the estimates look like for a larger value of lambda?
ridge.models$lambda[15] #Lambda = 10,235.31.
coef(ridge.models)[, 15] #Most estimates close to 0.
sqrt(sum(coef(ridge.models)[-1, 15]^2)) #L2 norm is 7.07.

#Visualizing the ridge regression shrinkage.
plot(ridge.models, xvar = "lambda", label = TRUE, main = "Ridge Regression")






