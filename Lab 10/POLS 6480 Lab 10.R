rm(list=ls())

#Set your working directory
setwd("C:/R Studio Files/Teaching/POLS6480-Fall2020-UH-lab/Lab 10")

data <- read.csv("Cruise.csv")
leading <- data[data$Role == "Lead", ]
counts <- table(leading$Rating)
barplot(counts, ylab="Number", xlab="Rating") 

boxplot(leading$Domestic ~ leading$Rating, horizontal=TRUE)
anova <- aov(Domestic ~ Rating, data=leading)
summary(anova)

hist(leading$Freshness)
plot(leading$Domestic ~ leading$Freshness, pch=19)

reg.simple <- lm(Domestic ~ Freshness, data=leading)
reg.simple$coefficients
plot(leading$Domestic ~ leading$Freshness, pch=19); abline(reg.simple)

names(summary(reg.simple))
summary(reg.simple)[4]
names(reg.simple)
reg.simple[8]
t.critical = qt(.975,31, lower.tail = TRUE)

cor(leading$Freshness, leading$Year)
cor(leading$Domestic, leading$Year)

reg.multiple <- lm(Domestic ~ Freshness + Year, data = leading)
reg.multiple$coefficients
summary(reg.multiple)[4]

cpidata <- read.csv("cpi1983.csv")
combined <- merge(leading, cpidata, by="Year")
combined$constant = combined$Domestic/(10000*combined$Annual)
cor(combined$constant, combined$Year)

reg.new <- lm(constant ~ Freshness, data=combined)
reg.new$coefficients
summary(reg.new)[4]
reg.new[8]
t.critical = qt(.975,31, lower.tail = TRUE)

predict(reg.new, data.frame(Freshness = c(33, 66, 100)))

reg.mpaa <- lm(constant ~ Freshness + Rating, data=combined)
summary(reg.mpaa)

rm(list=ls())