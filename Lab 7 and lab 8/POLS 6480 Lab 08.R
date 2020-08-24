rm(list=ls())
cereal <- read.csv("C:/cereal.csv"); attach(cereal)
children <- cereal[Intended.for == "Children", ]
adults <- cereal[Intended.for == "Adults", ]

par(mfrow=c(2,1))
hist(children$Sugar, seq(0,16,2), main="")
hist(adults$Sugar, seq(0,16,2), main="")
par(mfrow=c(1,1))

m1 <- mean(children$Sugar); s1 <- sd(children$Sugar); n1 <- length(children$Sugar)
m2 <- mean(adults$Sugar); s2 <- sd(adults$Sugar); n2 <- length(adults$Sugar)
diff.means = m1-m2
se.welch = sqrt((s1^2/n1)+(s2^2/n2))
t.welch = diff.means/se.welch
A=s1^2/n1; B=s2^2/n2
df.welch <- (A+B)^2/(A^2/(n1-1)+B^2/(n2-1))
t.critical <- qt(0.975, df.welch)
p <- 2*(1-pt(t.welch,df.welch))

t.test(children$Sugar, adults$Sugar, alternative="two.sided")
t.test(cereal$Sugar ~ cereal$Intended.for)

experiment <- read.csv("C:/anorexia.csv")
treatment.b <- experiment[experiment$therapy == "b", ]
treatment.f <- experiment[experiment$therapy == "f", ]
control  <- experiment[experiment$therapy == "c", ]

boxplot(treatment.f$after,treatment.b$after,control$after, horizontal=TRUE, 
        names=c("Family","Behavioral","Control"))
boxplot(experiment$after ~ experiment$therapy, horizontal = TRUE)

t.test(treatment.f$after, control$after)
t.test(treatment.f$after, control$after, alt="greater")

t.test(treatment.f$after, treatment.f$before, alt="greater")
treatment.f$delta <- treatment.f$after - treatment.f$before
cor(treatment.f$after, treatment.f$before)
plot(treatment.f$before, treatment.f$after, pch=19, xlim=c(70,100), ylim=c(70,110))
abline(lm(after~before, data=treatment.f)); abline(a=0,b=1, col="grey")
t.test(treatment.f$delta, mu=0, alt="greater")

t.test(treatment.f$after, treatment.f$before, alt="greater", paired = TRUE)

control$delta <- control$after - control$before
cor(control$after, control$before)
plot(control$before, control$after, pch=19, xlim=c(70,100), ylim=c(70,110))
abline(lm(after~before, data=control)); abline(a=0,b=1, col="grey")

t.test(treatment.f$delta, control$delta, alt="greater")
rm(list=ls())