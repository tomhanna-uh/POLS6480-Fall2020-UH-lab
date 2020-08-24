
rm(list=ls())

## 1
# A
x <- c(-2,-1,0,1,2)

# B - value of CDF at each value of x
pnorm(x)

# C
y <- c(0,1,2,5,8,10,15,20)

# D - value of the CDF at a given value of y
pbinom(y, size=20, prob=.2) # prob sets the probability of success at each trial; size = # of trials

## CDF = a function whose value is the probability that a corresponding continuous random variable 
##       has a value less than or equal to the argument of the function.

# E - Poisson
ppois(x, 6) # distribution function - vector, lambda (mean occurence per interval, whatever that may be)

## higher probability of higher numbers if the mean occurence is 6 - compare when changing lambda to 2

## WHY zero for the first two?? MUST BE non-zero in vector  because you cna't have negative occurences of anything




## 2 - calculating probabilities

# 2A.1 normal random variable
pnorm(27.5, 22, sd=5)-pnorm(16.2, 22, sd=5) # tell me why the standard deviation is 5

# 2A.2
1-pnorm(29, 22, sd=5)

# 2A.3
pnorm(17, 22, sd=5)

# 2A.4
pnorm(15,22,sd=5)+1-pnorm(25,22,sd=5)


# 2B.1 - number of heads 20, 25, OR 30 times
sum(dbinom(c(20,25,30), 60, prob=0.5))

# 2b.2 - less than 20 times
pbinom(19, 60, prob=0.5)

# 2b.3 - between 20 and 30 times
pbinom(30,60,prob=0.5)-pbinom(20,60,prob=0.5)


## 2c - Poisson distribution with mean (lambda) 7
# 2c.1 - X is less than 5
ppois(4,7)

# 2c.2 - X is greater than 10 
1-ppois(10,7) # low, given mean of 7

# 2c.3 - X is between 4 and 16
ppois(16,7)-ppois(3,7) 




# 3 - generate random samples from well-known distributions

# 3a
z <- rnorm(10) # base random normal distribution with 10 observations
z

w <- rnorm(1000, mean=5, sd=1) # 1000 obs. 
hist(w) # look normally distributed as we would expect

# 3b - binomial
k <- rbinom(20, size=5, prob=.2) # change prob to see differences according to the size of the trials
k

# 3c - Poisson
x <- rpois(20, 6) # N; lambda (mean value at interval, t)
x




## 4 - some real data
load(url("http://www.openintro.org/stat/data/bdims.RData"))

head(bdims) # most measurements are in diameters or girths

# basic subsetting - create new dataframes
mdims = subset(bdims, bdims$sex == 1)
fdims = subset(bdims, bdims$sex == 0)

hist(mdims$hgt)
hist(fdims$hgt)

# store the histograms as objects
p1 = hist(mdims$hgt)
p2 = hist(fdims$hgt)

# pretty histogram for comparison
xlim = c(min(fdims$hgt), max(mdims$hgt)) + c(-5, 5)
plot( p1, col=rgb(0,0,1,1/4), xlim=xlim) # rgb = color intensity, based on the hexidecimal scaling
plot( p2, col=rgb(1,0,0,1/4), add=T) # add = overlay

# store female descriptives as objects
fhgtmean = mean(fdims$hgt)
fhgtsd = sd(fdims$hgt)

# now let's make a density histogram 
summary(fdims$hgt) # see the range - this will inform our "x" creation below to condition the axis

hist(fdims$hgt, probability = TRUE, ylim=c(0, 0.07)) # ylim just zooms in a bit given that women are shorter than men (and the area in a density plot must add up to 1)
x = 140:190
y = dnorm(x = x, mean = fhgtmean, sd = fhgtsd)
lines(x = x, y = y, col = "blue")

## Q-Q
qqnorm(fdims$hgt, col="orange", pch=19)
qqline(fdims$hgt, lwd=2)

# Let's zoom in for a more precise answer
## first simulate some data
sim.norm = rnorm(n = length(fdims$hgt), mean = fhgtmean, sd = fhgtsd)

qqnorm(sim.norm, col="orange", pch=19)
qqline(sim.norm, lwd=2)

# let's simulate a few times to compare a bunch of random normal distriobutions to get a sense of what a normal distributions' deviations from the line look like
qqnormsim(fdims$hgt)

## Next, let's look at female weight
# First store the basic descriptive objects
wgtm = mean(fdims$wgt)
wgts = sd(fdims$wgt)

# generate our histogram
xlim = c(min(fdims$wgt), max(fdims$wgt)) + c(-5, +5)
hist(fdims$wgt, probability=T, xlim=xlim)
x = seq(xlim[1], xlim[2], 0.5) # read as, "from the first element in xlim, to the second element in xlim, by an increment of .5"
y = dnorm(x=x, mean=wgtm, sd=wgts)
lines(x=x, y=y, col="blue")

# now the Q-Q again
qqnorm(fdims$wgt, col="orange", pch=19)
qqline(fdims$wgt, lwd=2)
sim.norm = rnorm(n=length(fdims$wgt), mean=wgtm, sd=wgts)
qqnorm(sim.norm, col="orange", pch=19)
qqline(sim.norm, lwd=2)

# simulate again based on the actual mean and standard deviation from the data, drawn from a random normal distribution - with the first upper left plot as the actual data for comparison purposes - does it look normal?
qqnormsim(fdims$wgt)






# Not as close to normal as the other for sure, but close enough?





