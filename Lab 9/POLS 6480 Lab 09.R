rm(list=ls())

setwd("C:/R Studio Files/Teaching/POLS6480-Fall2020-UH-lab/Lab 9")

experiment <- read.csv("anorexia.csv")
treatment.b <- experiment[experiment$therapy == "b", ]
treatment.f <- experiment[experiment$therapy == "f", ]
control  <- experiment[experiment$therapy == "c", ]

boxplot(treatment.f$after,treatment.b$after,control$after, horizontal=TRUE, 
        names=c("Family","Behavioral","Control"))

m.f <- mean(treatment.f$after); v.f <- var(treatment.f$after); n.f <- length(treatment.f$after)
m.b <- mean(treatment.b$after); v.b <- var(treatment.b$after); n.b <- length(treatment.b$after)
m.t <- (n.f*m.f + n.b*m.b)/(n.f+n.b)
ss.between = n.f*((m.f-m.t)^2) + n.b*((m.b-m.t)^2)
ss.within = ((n.f-1)*v.f) + ((n.b-1)*v.b)
numerator = ss.between
denominator = ss.within/(n.f+n.b-2)
F = numerator/denominator
qf(.95, 1, n.f+n.b-1)
pf(F, 1, n.f+n.b-1, lower.tail = FALSE)

nocontrol <- experiment[experiment$therapy != "c", ]
reg1 <- aov(after ~ therapy, data=nocontrol)
summary(reg1)

reg.anova <- lm(after ~ therapy, data=nocontrol)
anova(reg.anova)

t.test(treatment.f$after, treatment.b$after, var.equal = TRUE)

reg.ancova <- lm(after ~ therapy + before, data=nocontrol)
anova(reg.ancova)

install.packages("reshape2")
library(reshape2)
long <- melt(nocontrol, id=c("subj", "therapy"))
long$prepost <- as.numeric(long$variable)-1
reg.new <- lm(value ~ prepost, data=long); anova(reg.new)
reg.new$coefficients
reg.twoway <- lm(value ~ prepost + therapy + therapy*prepost, data = long); anova(reg.twoway)
reg.twoway$coefficients

reg.three <- lm(after ~ therapy, data=experiment); anova(reg.three)
reg.three$coefficients

rm(list=ls())