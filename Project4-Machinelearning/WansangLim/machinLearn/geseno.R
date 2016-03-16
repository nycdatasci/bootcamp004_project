oriCul <- read.csv("/media/ubun10/64GB/NYC/project04/oriCul.csv", stringsAsFactors=FALSE)
oriCul$Year <- NULL 

oriCul$culMethod[oriCul$culMethod == "O"] <- 0
oriCul$culMethod[oriCul$culMethod == "B"] <- 1
#oriCul$culMethod <- as.integer(oriCul$culMethod)

# ##///////////////////////////////////////////////////////////////
reduOriCul <- oriCul[, c("culMethod","Rb1", "Rc", "Rd", "popu", "Dwg" )]
reduOriCul$culMethod <- as.factor(reduOriCul$culMethod)
# ##///////////////////////////////////////////////////////////////
 oriCul <- reduOriCul
# #QQQQQQQQQQQQQq
library(MASS)
oriCul$culMethod <- as.factor(oriCul$culMethod)
fit <- qda(culMethod ~ ., data=oriCul,
           na.action="na.omit", CV=TRUE)
fit # show results
plot(fit)

library(klaR)
partimat(culMethod ~.,data=oriCul,method="qda")
oriCul$culMethod <- as.integer(oriCul$culMethod)
# #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!1
summary(oriCul) #Looking at the five number summary information.
sapply(oriCul, sd) #Looking at the individual standard deviations.
sapply(oriCul, class) #Looking at the variable classes.

plot(oriCul, col = oriCul$culMethod + 2)
oriCul[, "popu"] <- as.factor(oriCul[, "popu"])

#Fitting the logistic regression with all variables; the family parameter
#specifies the error distribution and link function to be used. For logistic
#regression, this is binomial.
logit.overall = glm(culMethod ~ .,
                    family = "binomial",
                    data = oriCul)

#Residual plot for logistic regression with an added loess smoother; we would
#hope that, on average, the residual values are 0.
scatter.smooth(logit.overall$fit,
               residuals(logit.overall, type = "deviance"),
               lpars = list(col = "red"),
               xlab = "Fitted Probabilities",
               ylab = "Deviance Residual Values",
               main = "Residual Plot for\nLogistic Regression of Admission Data")
abline(h = 0, lty = 2)

library(car)
influencePlot(logit.overall) #Can still inspect the influence plot.

summary(logit.overall) #Investigating the overall fit of the model.
##################################################
#reduOriCul <- oriCul[, c("culMethod","Rb1", "Rc", "Rd", "Dwg", "popu")]
##################################################
exp(logit.overall$coefficients)

cbind("Log Odds" = logit.overall$coefficients,
      "Odds" = exp(logit.overall$coefficients))

confint(logit.overall) #For logistic regression objects, the confint() function
#defaults to using the log likelihood to generate confidence
#intervals; this is similar to inverting the likelihood
#ratio test.

confint.default(logit.overall) #To generate confidence intervals for logistic
#regression models based on the standard errors
#as we are accustomed to, we can use the
#confint.default() function.

#Generating confidence intervals for the coefficients on the odds scale.
exp(confint(logit.overall))
exp(confint.default(logit.overall))

#Do the categories for rank add any predictive power to the model? Let's
#conduct the drop in deviance test:
logit.norank = glm(culMethod ~popu+ Rb1 + Rc + Rd + Dwg,
                   family = "binomial",
                   data = oriCul)

reduced.deviance = logit.norank$deviance #Comparing the deviance of the reduced
reduced.df = logit.norank$df.residual    #model (the one without rank) to...

full.deviance = logit.overall$deviance #...the deviance of the full model (the
full.df = logit.overall$df.residual    #one with the rank terms).

pchisq(reduced.deviance - full.deviance,
       reduced.df - full.df,
       lower.tail = FALSE)

#More simply, we can use the anova() function and set the test to "Chisq".
anova(logit.norank, logit.overall, test = "Chisq")
######==================================================
newdata = with(oriCul, data.frame(Rg1 = mean(Rg1),
                                 Re  = mean(Re),
                                 Rb1 = mean(Rb1),
                                 Rc = mean(Rc),
                                 Rb2 = mean(Rb2),
                                 Rd  = mean(Rd),
                                Dwg = mean(Dwg),
                                 popu = factor(1:8)))
newdata #Creating a data frame with the average GRE and GPA for each level of
#the rank variable.

predict(logit.overall, newdata) #This gives us the log odds; but we want
#the probabilities.



  
#Using the formula to convert to probabilities:
exp(predict(logit.overall, newdata))/(1 + exp(predict(logit.overall, newdata)))

#Setting the type to "response" converts the predictions to probabilities for
#us automatically:
predict(logit.overall, newdata, type = "response")

#Making it easier to see the effects of the rank variable by printing out the
#results side-by-side:
cbind(newdata, "Prob. Admitted" = predict(logit.overall, newdata, type = "response"))

#Converting the fitted probabilities to binary:
admitted.predicted = round(logit.overall$fitted.values)

#Comparing the true values to the predicted values:
table(truth = oriCul$culMethod, prediction = admitted.predicted)

pchisq(logit.overall$deviance, logit.overall$df.residual, lower.tail = FALSE)

1 - logit.overall$deviance/logit.overall$null.deviance
  
  
 