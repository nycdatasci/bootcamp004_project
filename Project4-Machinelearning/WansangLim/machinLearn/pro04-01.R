molData <- read.csv("/media/ubun10/64GB/NYC/project04/molecular.csv", stringsAsFactors=TRUE)
linearly.separable <- molData[, 5:ncol(molData)]
#View(linearly.separable)

# set.seed(input$randomSet)
# train.index = sample(1:111, 111*0.07)
# test.index = -train.index

all.samples = sample(1:111, 111)
num.all = length(all.samples)
train.index = all.samples[1:(num.all*0.7)]
test.index = -train.index

#get polymorphic column
firstColName <- as.character(names(linearly.separable)[1])


polycolName <- c(firstColName)
for (i in 2:125) {
  if(sum(linearly.separable[train.index, i]) != length(train.index) &
     sum(linearly.separable[test.index, i]) != length(test.index)) {
    colName <- as.character(names(linearly.separable)[i])
    polycolName <- c(polycolName, c(colName) )
  }
}

#Chose best column
nyData <- linearly.separable[linearly.separable$PopuNY== "NY", ]
nonNYData <- linearly.separable[linearly.separable$PopuNY== "noNY",]
numNY <- nrow(nyData)
numNonNY <- nrow(nonNYData)
diffColumn <- c()
for (j in 2:125) {
  nyBand <- sum(nyData[, j])
  nonNYBand <- sum(nonNYData[, j])
  NYFrequen <- (nyBand/(nyBand + nonNYBand))*(numNonNY/nrow(linearly.separable))
  nonNYFrequen <- (nonNYBand/(nyBand + nonNYBand))*(numNY/nrow(linearly.separable))
  diffBetweenNYnon <- abs(NYFrequen - nonNYFrequen)
  diffColumn <- c(diffColumn, c(diffBetweenNYnon))
}
dataDiffFrame <- data.frame(firstColName, diffColumn)

linearly.separable <- linearly.separable[, polycolName]

# set.seed(0)
# x1 = c(rnorm(100, 0, 4), rnorm(100, 1, 3))
# x2 = c(rnorm(100, 0, 1), rnorm(100, 6, 1))
# y = as.factor(c(rep(-1, 100), rep(1, 100)))
# linearly.separable = data.frame(x1, x2, y)

# set.seed(0)
# train.index = sample(1:200, 200*.8)
# test.index = -train.index


library(e1071)
svm.mmc.linear = svm(PopuNY ~ ., #Familiar model fitting notation.
                     data = linearly.separable, #Using the linearly separable data.
                     subset = train.index, #Using the training data.
                     kernel = "linear", #Using a linear kernel.
                     cost = 1) #1e6 A very large cost; default is 1.

#Visualizing the results of the maximal margin classifier.
#plot(svm.mmc.linear, linearly.separable[train.index, ])

#Additional information for the fit.
summary01 <<- summary(svm.mmc.linear)
print(summary01)

#Finding the indices of the support vectors.
svm.mmc.linear$index

#Predicting on the test data.
ypred = predict(svm.mmc.linear, linearly.separable[test.index, ])
predicTable01 <<- table("Predicted Values" = ypred, "True Values" = linearly.separable[test.index, "PopuNY"])
print(predicTable01)
