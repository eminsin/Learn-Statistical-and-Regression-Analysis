# This document belongs to the book "Using R for Introductory Statistics" by John Verzani.
# copyright (c) 2004 - Taylor and Francis
#
# In the document, foundations of statistical knowledge is practiced on R.
#
# written by Erkam Minsin
#
# last modified: Aug 2022
# first written: Aug 2022

setwd("C:/Users/erkam/R Learning/Using_R_for_Introductory_Statistics_John_Verzani")

## install.packages("UsingR")
library(UsingR)

## update.packages()


# Chapter 2 Univariate Data -----------------------------------------------


# three types of data in statistics: categorical, discrete numeric, continuous numeric
# what is the average value? if it doesn't make sense, then the data is categorical

# 2.1 Categorical Data
 
# categorical data is summarized by:
#   tables
#   barplots
#   dot charts
#   pie charts 
 
# 2.1.1 Tables

table()

res=c("Y","Y","N","Y","N")
table(res)

central.park.cloud
table(central.park.cloud)

# 2.1.2 Barplots

?barplot
beer=c(3,4,1,1,3,4,3,3,1,3,2,1,2,1,2,3,2,3,1,1,1,1,4,3,1)
barplot(beer)                                                        # this is not correct
barplot(table(beer),xlab="beer",ylab = "frequency")
barplot(table(beer)/length(beer),xlab="beer",ylab = "proportion")

# Misleading barplots
sales=c(45,44,46)
names(sales)=c("John","Jack","Suzy")
barplot(sales,main = "Sales",ylab = "Thousands")                     # Basic bar plot
barplot(sales,main = "Sales",ylab = "Thousands",
        ylim = c(42,46),xpd = FALSE)                                 # misleading bar plot, it doesn't start at 0
                                                                     # xpd=FALSE to have R print only within ylimits
																	 
# names.arg=
barplot(central.park$MAX,names.arg = 1:31,
        xlab="day",ylab = "max.temp.")                               # used for time series data (a measurement of the same thing taken several times)

# barplot(table(x), xlab="", ylab="", names.arg=seq(), ylim=c(), xpd=TRUE,FALSE, horizontal=TRUE,FALSE, col="bar colors")

# text()
our.data=c(1,2,2,5)
names(our.data)=1:4
bp=barplot(our.data)
text(bp,our.data,labels=our.data,pos=1)                              # to put labels on top of the bars of a barplot, 1: just below the bar

# 2.1.3 Pie Charts

?pie
sales
pie(sales,main = "Sales",col=gray(c(.7,.85,.95)))

# 2.1.4 Dot charts

?dotchart
dotchart(sales,xlab = "Amount of Sales")

# 2.1.5 Factors

# R uses factors to store categorical data

1:5                                              # a numeric vector
factor(1:5)                                      # now a factor. Note levels
mean(factor(1:5))                                # factors are not numeric. Thus, the result is NA
letters[1:5]                                     # a character vector
factor(letters[1:5])                             # turned into a factor

# Problems 2.1.6

# Question 2.4
central.park$WX
table(central.park$WX)
table(central.park$WX,exclude = FALSE)

# Question 2.5
statbrowser=c(0.86,0.04,0.05,0.01,0.04)
names(statbrowser)=c('Internet Explorer','Gecko-based','Netscape Navigator4','Opera','Unidentified')
bp=barplot(statbrowser,main = 'Browser Statistics',xlab='Browser',ylab ='Usage Percentage')
text(bp,statbrowser,labels=statbrowser,pos=1)
pie(statbrowser,main = 'Browser Statistics')
dotchart(statbrowser,main = 'Browser Statistics',xlab = 'Usage Percentage', ylab='Browser')

# Question 2.6
share=c(0.18,0.15,0.144,0.135,0.062)
(1-sum(share))*100                               #1 other percentage is 32.9
share[6]=0.329
share
names(share)=c('Apple','RCA','Rio',
      'iRiver','Creative Labs','Others')
share*22                                         #2 (million $) Apple:3.960 , RCA:3.300, Rio:3.168, 
                                                 #              iRiver:2.970, Creative Labs:1.364, Others:7.238
												 
barplot(share,main = 'Market shares',
        xlab='Brand',ylab = 'Percentage')
pie(share,main = 'Market shares')
dotchart(share,ylab='Brands',
         xlab ='Percentages')                    #3 Dot Chart shows the relationship the best in this case for me

# Question 2.7
mtcars
mtcars$mpg
dotchart(mtcars$mpg,
         labels = dimnames(mtcars)[[1]])

# Question 2.8
table(npdb$state)
sort(table(npdb$state))                          # CA (CALIFORNIA)

# Question 2.9
table(npdb$ID)                                   # It allows to understand if any doctor has more than one entry.

# Question 2.10
attach(MLBattend)
numofwins=wins[franchise=='NYA']
detach(MLBattend)
names(numofwins)=(1969:2000)
numofwins
bp=barplot(numofwins,xlab = 'Year',
           ylab='Number of wins', 
		   main='New York Yankees',
		   ylim = c(40,120),xpd = FALSE)
text(bp,numofwins,labels=numofwins,pos=1)
dotchart(numofwins,main='New York Yankees')

# 2.2 Numeric Data

# 2.2.1 Stem-and-leaf plots

x=c(2,3,16,23,14,12,4,13,2,0,
    0,0,6,28,31,14,4,8,2,5)
stem(x)                                          # extra argument scale=integers can be set

# 2.2.2 Strip Charts

attach(kid.weights)
x <- height[48 <= age & age < 60]
stripchart(x,method = "stack",                   # alternative to stem-and-leaf plots
           xlab="x",pch=1,offset=1,cex = 2)
detach(kid.weights)

# pch:uses a different plot character than the default square
# offset:pushes the points apart
# cex:changes the size of the plot character

DOTplot(x)                                       # alternative to strip chart

# 2.2.3 The center:mean,median,and mode
 
# mean
x=c(2,3,16,23,14,12,4,13,2,0,0,0,6,28,31,14,4,8,2,5)
mean(x)
mean(x,trim = 0.3,na.rm = TRUE)

# median
bar=c(50,60,100,75,200)
bar.with.gates=c(bar,50000)
mean(bar)
mean(bar.with.gates)
median(bar)
median(bar.with.gates)                           # median is resistant to large values (outliers)

# trimmed mean
income=cfb$INCOME
mean(income)
median(income)                                   # The data is skewed to the right,as the mean is significantly more than the median.
mean(income,trim = .2)                           # The trimmed mean is more in line with the median.
sum(income<=mean(income))/length(income)*100     # 70.5% of the values are less than or equal to the sample mean.

# mode                                           # there is no built-in function for mode in R. However,
x=c(72,75,84,84,98,94,55,62)
which(table(x)==max(table(x)))                   # or directly,
which.max(table(x))

# midrange                                       # the midrange is a natural measure of center, the middle of the range
mean(range(x))

# 2.2.4 Variation:the variance,standard deviation,and IQR

# Variance and SD
test.scores=c(80,85,75,77,87,82,88)
test.scores.b=c(100,90,50,57,82,100,86)
mean(test.scores)
mean(test.scores.b)
var(test.scores)
var(test.scores.b)
sd(test.scores)
sd(test.scores.b)

# quantiles
x=0:5
lenght(x)
median(x)
quantile(x)                                      # default gives all quantiles
quantile(x,.25)                                  # just the first quantile
quantile(x,c(.25,.5,.75))                        # more than 1 at a time

# example for quantiles
sum(exec.pay>100)/length(exec.pay)               # 9% of executives make more than 1 million
quantile(exec.pay,.9)                            # 914000 dollars is 90 percentile
quantile(exec.pay,.99)                           # top 1% of executives get more than 9 million
sum(exec.pay<=10)/length(exec.pay)               # 14% make 100000 or less
quantile(exec.pay,.1)                            # 10 percentile is 90000

# IQR
IQR(exec.pay)

# z-score
scale()                                          # returns the collection of z-scores for a data set

# summary
summary(exec.pay)                                # there is a large difference between median and mean, because
sum(exec.pay<=mean(exec.pay))/length(exec.pay)   # mean is almost 84 percentile. Just 16% of execs get more salary than the mean of the sample

# Problems 2.2.5

# Question 2.11
x=c(80,82,88,91,91,95,95,97,98,101,106,106,109,110,111)
stem(x)
median(x)

# Question 2.12
x=c(14,17,23,27,34,43,50,59,66,72,79,87,96,103,110)
?stripchart
stripchart(x,xlab = "x",method = "stack",pch=1,
           offset = 1,cex = 1.5,vertical = FALSE,xlim = c(0,120))
median(x)
mean(x)
mean(x,trim = .1)

# Question 2.13
# it seems no problem with the paragraph. 

# Question 2.14
# median is resistant to large values. Mean can change a lot with an outlier.

# Question 2.15
sum(pi2000<=3)/length(pi2000)                    # 39.5%
sum(pi2000>=5)/length(pi2000)                    # 50.75%

# Question 2.16
sum(rivers<500)/length(rivers) #58.15%
sum(rivers<mean(rivers))/length(rivers) #66.6%
quantile(rivers,.75) #680 miles

#Question 2.17
View(nym.2002)
sum(nym.2002$time<180)/length(nym.2002$time)     # 2.6%
quantile(nym.2002$time,.10)                      # 208.695 minutes
quantile(nym.2002$time,.25)                      # 233.775 minutes
quantile(nym.2002$time,.90)                      # 331.75 minutes

# Question 2.18
mean(rivers)                                     # 591 miles
median(rivers)                                   # 425 miles
mean(rivers,trim = .25)                          # 450 miles
# There is a difference between the mean and the median. Large value effect.

# Question 2.19
stem(islands)
mean(islands)                                    # 1253
median(islands)                                  # 41
mean(islands,trim=.25)                           # 51
# There is a difference between the mean and the median. Large value effect.

# Question 2.20
View(OBP)
scale(OBP)                                       # 5.990191891

# Question 2.21
mean(scale(rivers))                              # 0
sd(scale(rivers))                                # 1

# Question 2.22
sd(exec.pay)                                     # 207
IQR(exec.pay)                                    # 27.5
mad(exec.pay)                                    # 20.75 #mad:median absolute deviation
# mad: resistant measure of spread. How resistant the median is against out-liers.

# Question 2.23
View(npdb)
attach(npdb)
mean(amount,na.rm = TRUE)                         # 166257.2 dollars
median(amount,na.rm = TRUE)                       # 37500 dollars
sum(amount<=mean(amount))/length(amount)          # 75%. 3rd quantile. Only 25% of the award amounts is more than the mean.
detach(npdb)

# Question 2.24
View(cabinet)
attach(cabinet)
mean(est.tax.savings)                            # 42157.47 dollars
median(est.tax.savings)                          # 6590 dollars... Large value effect
detach(cabinet)

# Question 2.25
# coefficient of variation
sd(rivers)/mean(rivers)                          # 0.8353922
sd(pi2000)/mean(pi2000)                          # 0.632253

# Question 2.26
x=rnorm(100)                                     # random data
plot(x)
x=sin(1:100)                                     # structured data
plot(x)

# Question 2.27

# Question 2.28

# Question 2.29

# 2.3 Shape of a distribution

# 2.3.1 Histogram

?hist
attach(faithful)
hist(waiting)
hist(eruptions)
hist(faithful)

# bin size is controlled by the break=argument
hist(waiting)                                    # use defaults ("Sturges algorithm")
hist(waiting,breaks = 10)                        # suggest 10 breaks
hist(waiting,breaks=seq(43,108,length=10))       # use these breaks
hist(waiting,breaks="scott")                     # use "Scott" algorithm

# The choice to draw a histogram of frequencies or proportions is made by the argument probability=argument. 
# By default, this is FALSE and frequencies are drawn.

hist(waiting,probability = TRUE)                 # or prob=T shortly

# By default, intervals: (a,b]. include.lowest=TRUE: [a,b]

hist(OBP,breaks = "scott",prob=TRUE,col=gray(0.9))

hist(OBP,main="My histogram of the OBP dataset")

# Frequency polygon
bins=seq(42,109,by=10)
freqs=table(cut(waiting,bins))
y.pts=c(0,freqs,0)
x.pts=seq(37,107,by=10)
plot(x.pts,y.pts,type="o")
rug(waiting)

# density
attach(faithful)
hist(waiting,breaks="scott",prob=T,
     main = "Waiting time probs for eruption",
	 xlab="waiting time",ylab="probabilities")
lines(density(waiting))                          # add to histogram
detach(faithful)
?density

last.tie
hist(last.tie,prob=T)
lines(density(last.tie))

# 2.3.2 Modes,symmetry, and skew

# tails of a distribution and skew
attach(cfb)
summary(VEHIC)
hist(VEHIC,breaks = "scott",prob=T)              # long-tailed
lines(density(VEHIC))
detach(cfb)

# 2.3.3 Box plots
?boxplot
View(alltime.movies)
attach(alltime.movies)
boxplot(Gross,ylab="All-time gross sales",horizontal = FALSE)
f=fivenum(Gross)
text(rep(1.3,5),f,labels = c("minumum","lower hinge(1Q)","median","upper hinge(3Q)","maximum"))

# getting out-liers
f=fivenum(Gross)
the.names=rownames(alltime.movies)
the.names[Gross>f[4]+1.5*IQR(Gross)]
detach(alltime.movies)

# Problems 2.3.4

# Question 2.30
bumpers
firstchi
math
hist(bumpers,breaks="scott",prob=F)
hist(firstchi,breaks = "scott",ylim=c(0,40),probability = FALSE)
hist(math,breaks = "scott",ylim=c(0,15),probability = F)
mean(bumpers)
median(bumpers)
sd(bumpers)
mean(firstchi)
median(firstchi)
sd(firstchi)
mean(math)
median(math)
sd(math)

# Question 2.31
x1=rnorm(100)                                    # generates 100 random numbers
hist(x1,breaks="scott",probability = T)
# Repeat
# No

# Question 2.32
hist(pi2000,breaks =0:10-0,probability = T)      # -0 part moves the x-axis left and right
lines(density(pi2000))

# Question 2.33
attach(normtemp)
fivenum(temperature)
hist(temperature,breaks = "scott",probability = F)
mean(temperature)
detach(normtemp)

# Question 2.34
library(UsingR)
DDT
hist(DDT,breaks = "scott",probability = F)
boxplot(DDT,horizontal = TRUE)
mean(DDT)                                        # 3.328
sd(DDT)                                          # 0.4371531

# Question 2.35
state.abb
state.area
names(state.area)=state.abb
state.area
sum(state.area<7836)/length(state.area)          # 8%
sum(state.area<49576)/length(state.area)         # 40%
hist(state.area,breaks = "scott",prob=F)
boxplot(state.area)
f=fivenum(state.area)
state.area[state.area>f[4]+1.5*IQR(state.area)]  # outliers

# Question 2.36
attach(nym.2002)
hist(time,xlab = "time (in minutes)",
     probability = F)                            # symmetrical...mean and median is close to each other.
detach(nym.2002)

# Question 2.37
mean(lawsuits)
median(lawsuits)
hist(lawsuits)                                   # large values (outliers) are too large. they have a high impact on the mean.

# Question 2.38
View(babyboom)
attach(babyboom)
hist(wt)                                         # left-skewed
diff(running.time)
hist(diff(running.time))                         # right-skewed
detach(babyboom)

# Question 2.39
attach(hall.fame)
hist(HR)                                         # right-skewed
hist(BA)                                         # symmetric
hist(OBP)                                        # symmetric
detach(hall.fame)

# Question 2.40

# Question 2.41
x=rnorm(1000)
boxplot(x,range=0.5)
boxplot(x,range=1)
boxplot(x,range=1.5)                             # the best
boxplot(x,range=2)
hist(x)

# Question 2.42
attach(cfb)
hist(AGE)                                        # multimodal
hist(EDUC)                                       # left-skewed
hist(NETWORTH)                                   # unimodal
hist(log(SAVING+1))                              # relatively symmetric 
detach(cfb)

# Question 2.43
hist(brightness,main="Brightness of 966 stars")  # symmetric

# Question 2.44
par(mfrow=c(2,1))
hist(lawsuits)
boxplot(lawsuits,horizontal = TRUE)
hist(log(1+lawsuits))
boxplot(log(1+lawsuits),horizontal = TRUE)
par(mfrow=c(1,1))

# Question 2.45
par(mfrow=c(2,1))
hist(exec.pay)
mean(exec.pay)
median(exec.pay)
hist(log(1+exec.pay,10))
mean(log(1+exec.pay,10))
median(log(1+exec.pay,10))                       # closer, thus symmetric
par(mfrow=c(1,1))

# Question 2.46
sum(((pi2000-mean(pi2000))/sd(pi2000))^3)        # -6.743236
sum(((exec.pay-mean(exec.pay))/sd(exec.pay))^3)  # 1906.13
