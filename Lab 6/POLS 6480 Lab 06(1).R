rm(list=ls()) # good idea to clear the environment just in case



# Load the data & set wd
source("http://www.openintro.org/stat/data/cdc.R")
setwd("C:/R Studio Files/Teaching/POLS6480-Fall2020-UH-lab/Lab 6")

## First look at contingency tables: smoking and health (relationship between condition and unconditional distributions of variables)
attach(cdc)
table(smoke100)
table(gender, smoke100)
mytable <- table(smoke100, genhlth)
mytable # distribution by health

prop.table(mytable, 1) # percentages instead of frequencies; 1 = row % 
prop.table(mytable, 2) # (2 = column %)

# Now, height and weight instead of smoking
m.cdc <- subset(cdc, cdc$gender == "m")
f.cdc <- subset(cdc, cdc$gender == "f")

par(mfrow=c(2,1)) # rows, columns
hist(m.cdc$weight, seq(50,500,25), main="") # begin at 50, end at 500, by 25 "units"
hist(f.cdc$weight, seq(50,500,25), main="")

par(mfrow=c(1,1))
plot(jitter(m.cdc$height), m.cdc$weight)
plot(jitter(f.cdc$height), f.cdc$weight)

f.cdc$bmi <- 703*(f.cdc$weight/(f.cdc$height^2)) # BMI calculation
hist(f.cdc$bmi, seq(12.5,74.5, 2)) # from to by

underweight <- ifelse(f.cdc$bmi<18.5,c(1),c(0)) 
regular <- ifelse(f.cdc$bmi>=18.5 & f.cdc$bmi<25,c(1),c(0))
overweight <- ifelse(f.cdc$bmi>=25 & f.cdc$bmi<30,c(1),c(0))
obese <- ifelse(f.cdc$bmi>=30,c(1),c(0))

f.cdc$bmiclas <- underweight + 2*regular + 3*overweight + 4*obese
f.cdc$fbmiclas <-factor(f.cdc$bmiclas,levels=1:4)

levels(f.cdc$fbmiclas) <- c("under","regular","over","obese")

table(f.cdc$fbmiclas, f.cdc$genhlth) -> contingency

prop.table(t(contingency),2)
barplot(prop.table(t(contingency),2), legend.text=colnames(contingency))
barplot(prop.table(t(contingency),2), beside=T)

par(mfrow=c(2,2))
slices <-c("white","grey75","grey50","grey25","black")
pie(contingency[1,], main="underweight", col=slices)
pie(contingency[2,], main="regular", col=slices)
pie(contingency[3,], main="overweight", col=slices)
pie(contingency[4,], main="obese", col=slices)

rm(list=ls()); par(mfrow=c(1,1))

# New Data on HISD Suspensions and the Chi-Square Test
## First, create the data and store 
susp.h <- c(39, 114, 19980, 15681, 16, 168, 904)
enrl.h <- c(483, 7394, 53238, 131004, 204, 1857, 17372)
barplot(susp.h/enrl.h, names.arg=c("Indian", "Asian", "Black", "Latino", "Pacific", "Multiple", "White"))

nots.h <- enrl.h - susp.h
table.h <- cbind(susp.h, nots.h)
rownames(table.h) <- c("Indian", "Asian", "Black", "Latino", "Pacific", "Multiple", "White")
colnames(table.h) <- c("Suspended", "Not")
ftable(table.h)

margin.table(table.h, margin = 2) # 1=sum by row; 2=sum by column

# The way that the chi-square test is calculated, you use the proportion generated 
# by the marginal (in this case 17.5%) and then generate expected cell frequencies 
# based on that. The chi-square test is the cell-by-cell sum of the squared difference 
# between the observed and expected cell frequency, divided by the expected cell frequency. 
# tests the degree to which individual deviates from the aggregate
# e.g., if 17.5% of students are suspended, then we should expect 17.5% of each ethnicity suspended
# significant p = difference between observed and expected (17.5%) is larger than expected due to chance alone - i.e., ethnicity matters in suspensions
chisq.test(table.h) 

# Do we reject the null of independence between etthnicities? Or do we fail to reject, suggesting ethnicity matters in suspensions?


## Now try on your own with new data - Cy-Fair District
susp.c <- c(66, 230, 5719, 7578, 13, 321, 2421)
enrl.c <- c(647, 9568, 18443, 48561, 88, 2851, 31552)


## Clear memory before closing
rm(list=ls())