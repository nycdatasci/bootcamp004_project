## Question 1
library(PASWR)
data(titanic3)
## 1.
nbColMiss = sapply(titanic3, function(x) sum(is.na(x)))
# age: 263; fare: 1; body: 1188 --- columns number: 5, 9, 13
length(which(is.na(titanic3))) # 1452 missing values in total
# proportion of missing values per variables with missing values
prop = sapply(titanic3[, c(5, 9, 13)], function(x) sum(is.na(x))/length(x))
# age: 20.09%; fare: 0.076%; body: 90.75% --- columns number: 5, 9, 13
## 2.
rowmiss = complete.cases(titanic3)
nbRowMiss = nrow(titanic3) - sum(rowmiss)
# 1190 rows with missing values, proportion of missing values
temp = sapply(titanic3[rowmiss, ], function(x) is.na(x))
# proportion of rows with missing values: 90.90%
nbRowMiss/nrow(titanic3)
## 3.
cellmiss = sapply(titanic3, function(x) is.na(x))
cellmiss_nb = sum(cellmiss)
# 1452 cells have missing values, that's 7.92% of the cells with NA value
cellmiss/(nrow(titanic3)*ncol(titanic3))
## 4.
library(VIM)
sapply(titanic3, sd)
aggr(titanic3)
# Seems the missing information is MNAR, as age and body seem to overlap, young men bodies are presumably 
#missing more often as more of them drowned. It could also be MAR as well
## 5.
age_miss = which(is.na(titanic3$age))
histMiss(titanic3[, c('age', 'body')])
# age and body seem to be MAR distributed while fare, with only 1 missing value is likely to be MCAR
# there might be a small MNAR correlation between age and body, as more young men should be missing, but 
# it doesn't seem obvious here
## 6.
library(Hmisc)
# mean imputation
titanic3.age.imputed.mean = impute(titanic3$age, mean)
histogram(titanic3$age)
histogram(as.vector(titanic3.age.imputed.mean))
# the original distribution apeared much thicker, with many missing values imputed to the mean, the number 
# of values taking the maximum value increase a lot, making stretching up the distribution
## 7.
# random imputation
titanic3.age.imputed.random = impute(titanic3$age, 'random')
histogram(titanic3$age)
histogram(as.vector(titanic3.age.imputed.random))
# the general look of the distribution is now preserved. It looks much safer, but for the risk that
# outliers get randomly chosen, possibly skewing the distribution a lot



## Question 2
## 1.
titanic3.fare.imputed.random = impute(titanic3$fare, 'random')
titanic3.fare.imputed.random[is.na(titanic3$fare)]
# value 26.55 was imputed
## 2.
library(dplyr)
library(ggplot2)
temp = tbl_df(titanic3) %>% mutate(., fare.fit = as.numeric(titanic3.fare.imputed.random), 
                                      age.fit = as.numeric(titanic3.age.imputed.random))
ggplot(temp, aes(x = age.fit, y = fare.fit, colour = pclass)) +
  geom_point()
# 1st class seem more or less normally distributed, with every age represented when 3rd class seem 
# very skewed towards young people and 2nd class seem concentrated in the middle, between 20 and 60
## 3.
newdat = data.frame(age.fit = c(50, 10), fare.fit = c(400, 100), pclass = c(NA, NA))
pass = select(temp, age.fit, fare.fit, pclass) %>% rbind(., newdat)
ggplot(pass, aes(x = age.fit, y = fare.fit, colour = pclass)) +
  geom_point() +
  geom_point(data = newdat, aes(x = age.fit, y = fare.fit, colour = 'cyan'))
## 4.
# visually, from a kNN point of view, it seems they should both be 1st class
## 5.
imputed.pass.1nn = kNN(as.data.frame(pass), k = 1)
imputed.pass.1nn
# both were assigned to 1st class
## 6.
k0 = sqrt(nrow(pass))
imputed.pass.36nn = kNN(as.data.frame(pass), k = 36)
imputed.pass.36nn
## they were assigned 1st and 3rd respectively
## because of the concentration of young passengers among 3rd class

## Question 3
## 1.
tit = tbl_df(titanic3) %>% select(., pclass, survived, sex, age, sibsp, parch) %>%
  mutate(., fare = titanic3.fare.imputed.random)
## 2.
tit.complete = tit[complete.cases(tit), ]
tit.missing = tit[!complete.cases(tit), -4]
## 3.
library(kknn)
tit.manhattan = kknn(age ~ ., tit.complete, tit.missing, k = 1, distance = 1)
summary(tit.manhattan)

tit.euclide = kknn(age ~ ., tit.complete, tit.missing, k = 1, distance = 2)
summary(tit.euclide)

tit.mink = kknn(age ~ ., tit.complete, tit.missing, k = 1, distance = 10)
summary(tit.mink)

man = c(tit.complete$age, tit.manhattan$fitted.values)
euc = c(tit.complete$age, tit.euclide$fitted.values)
mink = c(tit.complete$age, tit.mink$fitted.values)

imputed.frame = data.frame(man = man, euc = euc, mink = mink, orig = titanic3$age)
library(reshape)
imputed.frame.melt = melt(imputed.frame)
## 4
ggplot(imputed.frame.melt, aes(value, colour = variable)) +
  geom_density()
# best one seems to be L^p for p = 10, which preserves the original mean and fits the curve pretty well, 
# whereas L^2 seems to be the worst with a bump on the upper tail. 
## 5.
library(kknn)
k0 = sqrt(nrow(tit.complete))
tit.manhattan = kknn(age ~ ., tit.complete, tit.missing, k = k0, distance = 1)
summary(tit.manhattan)

tit.euclide = kknn(age ~ ., tit.complete, tit.missing, k = k0, distance = 2)
summary(tit.euclide)

tit.mink = kknn(age ~ ., tit.complete, tit.missing, k = k0, distance = 10)
summary(tit.mink)

man = c(tit.complete$age, tit.manhattan$fitted.values)
euc = c(tit.complete$age, tit.euclide$fitted.values)
mink = c(tit.complete$age, tit.mink$fitted.values)

imputed.frame = data.frame(man = man, euc = euc, mink = mink, orig = titanic3$age)
library(reshape)
imputed.frame.melt = melt(imputed.frame)
## 6.
ggplot(imputed.frame.melt, aes(value, colour = variable)) +
  geom_density()
# the density plot looks sensibly worse with an increased mean and skewed to the right, 
# probably because too many people are concentrated in the same region: the assumption for kNN might be
# wrong, the regions in which we would like to classify our variables are not distinct. Could also be
# that kknn doesn't work as well because we are trying to compare locations with too many variables








