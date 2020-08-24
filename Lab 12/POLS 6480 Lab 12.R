library(lmtest)
library(separationplot)

#1. 

 loan <- read.csv("https://www.dropbox.com/s/89g1yyhwpcqwjn9/lending_club_cleaned.csv?raw=1")

#2. Look at the data

head(loan)

#3
logit1 <- glm(good ~ fico, data = loan, family = "binomial")
summary(logit1)


#4. 

exp(coef(logit1))


#5. You're on your own.


#6. 
test <- data.frame(fico=c(700,750))
test$pred <- predict(logit1,test, type="response")
test


#7 You're on your own.

#8
logit3 <- glm(good ~ fico + loan_amnt + purpose, data = loan, family = "binomial")
summary(logit3)


round(exp(coef(logit3)),3)


#9

logit4 <- glm(good ~ fico + loan_amnt + income + purpose, data = loan, family = "binomial")
summary(logit4)


#10. Let's compare models 1 and 3 using a log-likelihood test. 

lrtest(logit1,logit3)

#11. 

separationplot(pred=logit3$fitted.values, actual=logit3$y, type="rect",
               line=TRUE, show.expected=TRUE,  width = 9, height = 1.2, heading="Probability of Y", zerosfirst)
