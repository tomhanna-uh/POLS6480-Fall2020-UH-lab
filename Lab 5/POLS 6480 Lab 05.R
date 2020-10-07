setwd("C:/R Studio Files/Teaching/POLS6480-Fall2020-UH-lab/Lab 5") # change it for your wd
hurricanes <- read.csv("hurricanes(2).csv")
attach(hurricanes)

set.seed(87654321)
d <- seq(1, 20, length = 20)
m <- numeric(length(d))
for(i in 1:length(d)) {
  m[i] <- mean(sample(TStorms, size=4, replace=T)) }

mu <- mean(TStorms)
sigma <- sd(TStorms)
se <- sigma/sqrt(4)
upper = mu + 1.96*se
lower = mu - 1.96*se
zstat <- function(m, mu, sigma) {(mean(m)-mu)/(sigma/sqrt(4)) }
z <- numeric(length(d))
for(i in 1:length(d)) {z[i] <- zstat(m[i], mu, sigma)}

for(i in 1:length(d)) {upper[i] <- m[i] + 1.96*se}
for(i in 1:length(d)) {lower[i] <- m[i] - 1.96*se}
plot(d,upper,pch=25,ylim=c(0,25))
points(d,lower,pch=24)
with(sapply(d,function(i) lines(c(i,i),c(lower[i],upper[i]))))
abline(a=mu, b=0, col="red")

rm(d,i,m,mu,sigma,se,upper,lower,z)
hist(Hurricanes, freq = F, breaks=14)
lambda <- mean(Hurricanes)
i <- k <- seq(1, 15, length = 15)
p <- numeric(length(k))
for(k in 1:15) {p[k] <- exp(-lambda)*lambda^k/factorial(k)}
plot(i,p,type="b")

sigma <- sd(Hurricanes)
plot(i,p,type="b"); curve(dnorm(x, mean=lambda, sd=sigma), add=T, col="blue")
hist(Hurricanes, prob=T); curve(dnorm(x, mean=lambda, sd=sigma), add=T, col="blue")

set.seed(87654321)
d <- seq(1, 20, length = 20)
m <- numeric(length(d))
for(i in 1:length(d)) {
  m[i] <- mean(sample(Hurricanes, size=4, replace=T)) }

upper = lambda + 1.96*sigma/2
lower = lambda - 1.96*sigma/2
plot(d,m, pch=19,ylim=c(0,12)); abline(a=upper, b=0, col="red"); abline(a=lower,b=0, col="red")

for(i in 1:length(d)) {m[i] <- mean(sample(Hurricanes, size=25, replace=T)) }
for(i in 1:length(d)) {upper[i] <- m[i] + 1.96*sigma/5}
for(i in 1:length(d)) {lower[i] <- m[i] - 1.96*sigma/5}
plot(d,upper,pch=25,ylim=c(0,12))
points(d,lower,pch=24)
with(sapply(d,function(i) lines(c(i,i),c(lower[i],upper[i]))))
abline(a=lambda, b=0, col="red")
