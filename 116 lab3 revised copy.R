# Name: Faye Bandet
# Date: 8/29/19
# ISTA 116 Section B || Section Leader : Jacob Heller
# Lab Assignment 1
# Collaborator(s): Nick Ackerman

source("http://www.openintro.org/stat/data/cdc.R")

#1
plot(cdc$weight ~ cdc$wtdesire)
# We see that most people's desired weight is less than 300.People's actual weight is generally above thier goal weight.

#2
wdiff <- (cdc$weight - cdc$wtdesire)
wdiff
# Zeros mean they are at desired weight
# Positive numbers want to lose weight 
# Negative numbers mean people want to gain weight

#3
mean(wdiff)
median(wdiff)
summary(wdiff)
boxplot(wdiff, ylim = c(-50, 100))
# mean is 14.59
# median is 10
# Using mean and median definitions as center. Using mean the center is 14.5891 because it is more accurate.

#4
boxplot(wdiff, ylim = c(-50, 100))
summary(wdiff)
# shape is mean is greater than median so it is right skew.
# spread is from -500.00 to 300.00. The first quadrant is 0 and the third quadrant is 21
# chose that because it is the most accurate

#5
# center shows that the average weight people want to lose is is 14.59.
# shape is right skewed, shows that more people want to lose weight than gain weight
# spread shows that their might be outliers but people want to gain or lose weight to range from -500.00 to 300.00.

#6
boxplot(wdiff ~ cdc$gender, ylim = c(-50, 100))
# women want to lose weight and men want to gain weight

#7
boxplot(wdiff ~ cdc$gender, ylim = c(-150, 400))
# positive outliers look like data errors are generally above 200. look at people who want to lose weight there are outliers for loosing 300 pounds. The person at 1995 was this outlier.

#8
# standard deviation below the mean
lbs <- cdc$weight
below <- mean(lbs) - sd(lbs)
above <- mean(lbs) + sd(lbs)
below
above
below*2
above*2
# standard deviation above the mean
# 209.76
# standard deviations below the mean
# 129.6
# 2 standard deviations above the mean
# 419.53
# 2 standard deviations below the mean
# 259.2

#9
# proportion of the observations are within 1 standard deviation of the mean? 
nrow(cdc[(cdc$weight >= below) & (cdc$weight <= above),])/nrow(cdc)
# proportion is .7076
# proportion within 2 standard deviations
nrow(cdc[(cdc$weight >= below*2) & (cdc$weight <= above*2),])/nrow(cdc)
# proportion is .02945

#10
belowh <- mean(cdc$height)-sd(cdc$height)
aboveh <- mean(cdc$height)+sd(cdc$height)
below2 <- mean(cdc$height)-(sd(cdc$height)*2)
above2 <- mean(cdc$height)+(sd(cdc$height)*2)

nrow(cdc[(cdc$height >=belowh) & (cdc$height <= aboveh),])/nrow(cdc)
nrow(cdc[(cdc$height >=below2) & (cdc$height <= above2),])/nrow(cdc)

# What proportion of heights are within 1 standard deviation of the mean?
# proportion is .62125
# What proportion of heights are within 2 standard deviations of the mean?
# proportion is .97725

belowA <- mean(cdc$age)-sd(cdc$age)
aboveA <- mean(cdc$age)+sd(cdc$age)
above2 <- mean(cdc$age)+(sd(cdc$age)*2)
below2 <- mean(cdc$age)-(sd(cdc$age)*2)

nrow(cdc[(cdc$age >= belowA) & (cdc$age <= aboveA),])/nrow(cdc)
nrow(cdc[(cdc$age >= below2) & (cdc$age <= above2),])/nrow(cdc)
# What proportion of ages are within 1 standard deviation of the mean?
# proportion is .6403
# What proportion of ages are within 2 standard deviations of the mean?
# proportion is .9693

# problem 10 code adapted from Nick
