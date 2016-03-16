library(tree)

#Loading the ISLR library in order to use the Carseats dataset.
library(ISLR)
oriCul <- read.csv("/media/ubun10/64GB/NYC/project04/oriCul.csv", stringsAsFactors=FALSE)
oriCul$Year <- NULL 

#Making data manipulation easier.
attach(oriCul)
oriCul$culMethod <- as.factor(oriCul$culMethod)
oriCul$popu <- as.factor(oriCul$popu)

#High = ifelse(Sales <= 8, "No", "Yes")

tree.oriCul = tree(culMethod ~ . -culMethod, split = "gini", data = oriCul)
summary(oriCul)

#Plotting the classification tree.
plot(tree.oriCul)
text(tree.oriCul, pretty = 0) #Yields category names instead of dummy variables.

#Detailed information for the splits of the classification tree.
tree.oriCul

