#clear environment
rm(list = ls())

#set working directory
setwd("C:/R Studio Files/Teaching/POLS6480-Fall2020-UH-lab/Lab 12")

library(lmtest)
library(separationplot)

#1. 

 loan <- read.csv("https://www.dropbox.com/s/89g1yyhwpcqwjn9/lending_club_cleaned.csv?raw=1")

#2. Look at the data

head(loan)

#3

#Original lab script - will give an error!
logit1 <- glm(good ~ fico, data = loan, family = "binomial")
summary(logit1)

#The variable good is a character variable, so we have to turn it into a binary (0 or 1)
#or the logit function won't work

#One way is to run logit using "as.factor" - but this can be confusing to interpret
logit1 <- glm(as.factor(good) ~ fico, data = loan, family = "binomial")
summary(logit1)

#The better way is to coerce the variable to a factor of zeros and ones - this way we know that good = 0 
#and 1 = bad, so interpretation is easier
loan$good.f <- factor(loan$good,levels = c("good","bad"), labels = c(0,1))

logit2 <- glm(good.f ~ fico, data = loan, family = (binomial))
summary(logit2)


#4. 

exp(coef(logit1))


#5. You're on your own.


#6. 
test <- data.frame(fico=c(700,750))
test$pred <- predict(logit1,test, type="response")
test


#7 You're on your own.

#8
logit3 <- glm(good.f ~ fico + loan_amnt + purpose, data = loan, family = "binomial")
summary(logit3)


round(exp(coef(logit3)),3)


#9

logit4 <- glm(good.f ~ fico + loan_amnt + income + purpose, data = loan, family = "binomial")
summary(logit4)


#10. Let's compare models 1 and 3 using a log-likelihood test. 

lrtest(logit1,logit3)

#11. 

separationplot(pred=logit3$fitted.values, actual=logit3$y, type="rect",
               line=TRUE, show.expected=TRUE,  width = 9, height = 1.2, heading="Probability of Y", zerosfirst)
