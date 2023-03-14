# This document belongs to the book "Using R for Introductory Statistics" by John Verzani.
# copyright (c) 2004 - Taylor and Francis
#
# In the document, foundations of statistical knowledge is practiced on R.
#
# written by Erkam Minsin
#
# last modified: Dec 2022
# first written: Dec 2022

setwd("C:/Users/erkam/R Learning/Using_R_for_Introductory_Statistics_John_Verzani")

## install.packages("UsingR")
library(UsingR)

## update.packages()


# Chapter 6 Simulation ---------------------------------------------


# simulating (repeating the same action) a distribution can give us great insight into the distribution's shape, tails, mean and variance etc.
# we will use simulation to justify the size of n needed in the central limit theorem for approximate normality of the sample mean
# in this chapter, first "for loops" will be introduced
# "for loops" are used to repeat something again and again, such as sampling from a distribution
# then we will see how to define "simple functions" in R
# defining functions not only makes for less typing; it also organizes our work and train of thought
# this is indispensable when we approach larger problems

# 6.1 The normal approximation for the binomial

# we begin with a simulation to see how big n should be for the binomial distribution to be approximated by the normal distribution
# although we know explicitly the distribution of the binomial, 
# we approach this problem by taking a random sample from this distribution to illustrate the approach of simulation
# to perform the simulation, we will take m samples from the binomial distribution for some n and p
# we should take m to be some large number, so that we get a good idea of the underlying population the sample comes from
# then we will compare our sample to the normal distribution with mu=n*p and sd=sqrt(n*p*(1-p))
# if the sample appears to come from this distribution; we will say the approximation is valid

m = 200; p = 1/2

n = 5
res <- rbinom(m, n, p)                           # store results (200 (n=5 size) samples)
hist(res, prob = TRUE, main = "n = 5")           # don't forget prob=TRUEs
curve(dnorm(x, n*p, sqrt(n*p*(1-p))), 
      add = TRUE)                                # add density

n = 15                                           # repeat for n=15     
res <- rbinom(m, n, p)
hist(res, prob = TRUE, main = "n = 15")
curve(dnorm(x, n*p, sqrt(n*p*(1-p))), 
      add = TRUE)                                     

n = 25                                           # repeat for n=25     
res <- rbinom(m, n, p)
hist(res, prob = TRUE, main = "n = 25")
curve(dnorm(x, n*p, sqrt(n*p*(1-p))), 
      add = TRUE)

# we see from the graphics that n=5 the approximation is not valid at all- 
# the discreteness of the binomial distribution is still apparent for n=5
# by n=15 and n=25, the approximation looks pretty good
# this agrees with the rule of thumb that when n*p and n*(1-p) are both greater than 5, the normal approximation is valid

# a better way to judge normality : qqnorm()

m = 200; p = 1/5; n = 25
res <- rbinom(m, n, p)
qqnorm(res)
# if the sampling distribution is normal, then this plot will show an approximate straight line as in the example above

# 6.2 for loops

# generating samples (as such 200 sample above) from the binomial distribution is straightforward with rbinom()
# for other statistics, we can also generate samples but perhaps only one at a time
# in this case, to get a large enough sample to investigate the sampling distribution, we use a for loop to repeat sampling

res <- c()
for (i in 1:100) {
  res[i] = mean(runif(10, min=0, max=1))
}
# the variable res now holds 100 samples of Xbar for n=10 and each Xi being Uniform(0,1)

# 6.3 Simulations related to the central limit theorem

# let us investigate the normality of Xbar for different sample sizes for Uniform(0,1)

plot(0,0, type = "n", xlim = c(0,1), ylim = c(0,13.5),               # set up plot window 
	 xlab= "Density estimate", ylab = "f(x)")                              
m = 500 
a = 0 
b = 1
n = 2
for (i in 1:m) res[i] = mean(runif(n,min=a,max=b))                   # if there is just a single command, then no braces are necessary
lines(density(res), lwd = 2)
n = 10
for (i in 1:m) res[i] = mean(runif(n,min=a,max=b))
lines(density(res), lwd = 2)
n = 25
for (i in 1:m) res[i] = mean(runif(n,min=a, max=b))
lines(density(res), lwd = 2)
n = 100
for (i in 1:m) res[i] = mean(runif(n,min=a, max=b))
lines(density(res), lwd = 2)

# the more skewed the data (parent population) is, the larger n must be for the normal dist to approximate the sampling dist of Xbar
# we will see skewed data examples later in the problems

# 6.4 Defining a function

# it is often convenient to define functions to perform tasks that require more than one step

f <- function() {
  mean(rexp(10, rate=1))
}
f()                                              # finds the mean of a sample of size 10 from Exponential(1)

# if we define a function to find a single observation from the sampling distribution, then
res <- c()
for (i in 1:500) res[i] = f()

# 6.4.1 Editing a function

# by editing a function, the entire function need not to be retyped when changes are desired
# editing can be done by fix() and edit()

# fix(f) will open an editor to the definition of function f
# we make the desired changes then exit the editor
# the changes are directly assigned to f which can be used desired

# edit(f) requires the assignment of f again as f <- edit(f)

# 6.4.2 Function arguments

# a function usually has a different answer depending on the value of its arguments
# passing arguments to R functions are done by arguments' names or positions 

f <- function(n=10) {
  mean(rexp(n))
}

# n is an argument now
# n=10 is set as default, thus the answers of f(), f(10), f(n=10) are the same
# f(100) will return the mean of a sample with n=100 size and Exponential(1) dist

res <- c()
for (i in 1:200) res[i] = f(n=50)                # simulating 200 samples of for Xbar, n=50

f <- function (n=10, rate=1) {                   # sets the 1st argument with default n=10, 2nd argument with default rate=1
  mean(rexp(n, rate=rate))                       # f(50,2) : size=50, rate=2 (mean=1/2)
}                                                # f(rate=1/2) : size=10 (default) and rate=1/2
                                                 # f(1/2) : error because 1/2 would match the position for n and not that of rate

args()                       # helps sorting out the available arguments and their names as a quick alternative to the more informative help page                          
 
# 6.4.3 The function body

# the function body is a block of commands enclosed in braces
# braces are optional if there is a single command
# the return value for a function is the last command executed
# the function return() will force the return of a function, with its argument becoming the return value

# Inside a function, nothing is printed unless we ask it to be
# print() : will display an object as though it were typed on the command line
# cat() : can be used to concatenate values together
# unlike print(), the cat() function will not print a new line character, nor the element numbers, such as [1]
# a new line can be printed by including "\n" in the cat() command

# when a function is called, the return value will print unless it is assigned to some object
# if we don't want this, such as when producing a graphic, the function invisible() will suppress the printing

x <- 5                                           # defines x to be 5 outside the block
f <- function() {
  x = 6                                          # assigns x to be 6 inside the block
  x
}
f()                                              # 6 is printed as x=6 inside the block
x                                                # 5 is printed as x=5 outside the block

# if we really want to force x to change inside the block, 
# " <<- (global assignment operator) ", or the assign() function can be used 

# if none had been assigned inside the block, R looks for a definition outside the block
x <- 5
f <- function() print(x)
f()                                              # 5 is printed as x=5 outside the block 
rm(x)
f()                                              # error : object "x" not found

# 6.5 Investigating distributions

# example : the sample median

# the sample median, M, is a measurement of central tendency like the sample mean
# Does it, too, have an approximately normal distribution?

# let us perform a simulation of Exponential(1) with mean = 1, median log(2) = 0.6931 for n = 25, 100, 400
f <- function(n) median(rexp(n))
m = 500
res.25 <- c() ; res.100 <- c() ; res.400 <- c()
for(i in 1:m) res.25[i] <- f(25)
for(i in 1:m) res.100[i] <- f(100)
for(i in 1:m) res.400[i] <- f(400)
summary(res.25) ; summary(res.100) ; summary(res.400)                
# the summary() commands show that the mean and median are similar for each sample and
# appear to be centered around the median of the parent population

plot(density(res.400), xlim = range(res.25), type = "l", main = "",                      # n=400 first so the y-axis is large enough for all 3 graphs
	 xlab = "sampling distributions of median for n=25, 100, 400") 
lines(density(res.100))
lines(density(res.25))
# as n gets large, the sampling distribution tends to a normal dist which is centered on the median of the parent population

# example : comparing measurements of spread

# we can use either the standard deviation or the IQR to measure the spread
# how we choose one over the other : if the sampling variation of one is significantly smaller than that of the other

f <- function(n) sd(rnorm(n))
g <- function(n) IQR(rnorm(n))
res.sd <- c() ; res.IQR <- c()
for(i in 1:200) {
  res.sd[i] = f(100)
  res.IQR[i] = g(100)
}
boxplot(list(sd=res.sd, IQR=res.IQR))            # the spread of the sd is significantly smaller than that of the IQR
                                                 # thus, the sd is a better measure of spread for normal data
												 # however, it isn't always a better measure of spread
												 # we will repeat the simulation with exponential data and investigate later

# 6.5.1 Script files and source()

# R can "read" the contents of a file and execute the commands as though they were typed in at the command line

# let us say a file named "sim.R" contains these commands below
f <- function(n) sd(rexp(n))
g <- function(n) IQR(rexp(n))
res.sd <- c() ; res.IQR <- c()
for(i in 1:200) {
  res.sd[i] = f(100)
  res.IQR[i] = g(100)
}
boxplot(list(sd=res.sd, IQR=res.IQR))
# then command source("sim.R") will read and execute these commands

# with exponential data, the spread of sd and IQR is similar
# result : the more skewed or long-tailed the data is, the wider the spread of the sd compared to the IQR

# 6.5.2 The geometric distribution

# in the sequence of i.i.d Bernoulli trials, there is a time of the first success
# this can happen on the first, second trial or at any other point
# let X be the time of the firt success
# then X is a random variable with distribution on the positive integers
# the distribution of X is called the geometric distribution
# f(k) = P(X=k) = ((1-p)^k-1)(p)

# let us simulate the random variable X to investigate its mean
# X : the distribution of tossing coins until we have a success

# while() loop is ideal to use here as we don't know in advance how many times we will need to toss the coin


first.success <- function(p) {
  k = 0
  success = FALSE
  while(success == FALSE) {
    k = k+1
	if (rbinom(1,1,p) == 1) success = FALSE }
}
k
res.5 <- c() ; res.05 <- c()
for(i in 1:500) {
  res.5[i] = first.success(0.5)
  res.05[i] = first.success(0.05)
}
summary(res.5)                                   # has mean 2 = 1/0.5
summary(res.05)                                  # has mean 20 = 1/0.05
                                                 # thus, for any p in [0,1], the mean of the geometric distribution is 1/p

# 6.6 Bootstrap samples

# the basic idea of bootstrap sample is to sample with replacement from the data
# thereby, creating a random sample of the same size as the original
# for this random sample, the value of statistics is computed
# call this a replicate
# this process is repeated to get the sampling dist of the replicates
# from this, inferences are made about the unknown parameters

data(bycatch)
hauls <- with(bycatch, 
              rep(no.albatross, no.hauls))       # a data vector containing the number of albatross caught on each of the 897 hauls
n <- length(hauls)                               # 897 hauls                                               
											 
# a histogram shows a skewed distribution, usually none are caught, but occasionally many are
# as the data is skewed, we know the sample mean can be a poor predictor of the center
# so we create 1000 bootstrap samples as follows, using sample()

xbarstar <- c()                                  # contains the replicate of 1000 bootstrap samples
for(i in 1:1000) {
  boot.samp = sample(hauls, n, replace = TRUE)
  xbarstar[i] = mean(boot.samp)
}

mean(xbarstar) ; sd(xbarstar)                    # estimates for the population mean and variance
quantile(xbarstar, c(0.05, 0.95))                # estimate for where the population mean is for 95% of the time

# 6.7 Alternates to for loop

# for practical reasons, alternatives to for loops can be faster
# for aesthetic reasons, a vectorized approach may be desired
# in this approach, we use a function on multiple values at once, rather than one at a time as in a for loop

m = 100; n = 10
tmp <- matrix(rnorm(m*n), nrow = m)

# apply()
xbar <- apply(tmp, 1, mean)                      # calculates the mean of each row of the tmp matrix

# sapply()
m = 10 ; n = 25
sapply(1:m, function(x) mean(rnorm(n)))          # will generate ten random samples of the mean of 25 random numbers

# sim() in UsingR package
sim(n = 25, m = 10, statistic = "mean",           # built-in function for sapply()
    family = "norm", mean = 0, sd = 1)

# Problems 6.8

# Question 6.1
m = 200 ; n = 100 ; p1 = 0.02 ; p2 = 0.2

res.02 <- rbinom(m, n, p1)                       # simulation of the binomial dist for n=100, p=0.02
res.2 <- rbinom(m, n, p2)                        # simulation of the binomial dist for n=100, p=0.2

hist(res.02, prob = TRUE, main = "p = 0.02")    
curve(dnorm(x, n*p1, sqrt(n*p1*(1-p1))),         # normal approximation for the first  binomial dist; more skewed than normally distributed
      add = TRUE)

hist(res.2, prob = TRUE, main = "p = 0.2")
curve(dnorm(x, n*p2, sqrt(n*p2*(1-p2))),         # normal approximation for the second  binomial dist; normally distributed
      add = TRUE)
	  
# Question 6.2
data(lawsuits)
res <- c()
n = 5
for(i in 1:300) res[i] = mean(sample(lawsuits, n, replace = TRUE))   # simulation 1
plot(density(scale(res)))                                            # scale() finds the z-scores for each value in the data vector
n = 15
for(i in 1:300) res[i] = mean(sample(lawsuits, n, replace = TRUE))   # simulation 2
plot(density(scale(res)))
n = 25
for(i in 1:300) res[i] = mean(sample(lawsuits, n, replace = TRUE))   # simulation 3
plot(density(scale(res)))
n = 50
for(i in 1:300) res[i] = mean(sample(lawsuits, n, replace = TRUE))   # simulation 4
plot(density(scale(res)))
n = 100                                                              
for(i in 1:300) res[i] = mean(sample(lawsuits, n, replace = TRUE))   # simulation 5
plot(density(scale(res)))                                            # after n=100, it becomes roughly normal with the mean z-score=0
n = 350
for(i in 1:300) res[i] = mean(sample(lawsuits, n, replace = TRUE))   # simulation 6
plot(density(scale(res)))

qqnorm(res)

# Question 6.3
plot(0,0, type = "n", xlim = c(0,1), ylim = c(0,13.5),               # set up plot window 
	 xlab= "Density estimate", ylab = "f(x)") 
res <- c()
n = 5; for(i in 1:500) res[i] = mean(runif(n, min=0, max=1))         # simulation 1 of the Uniform(0,1) for n=5; normality quickly starts
lines(density(res))
n = 15; for(i in 1:500) res[i] = mean(runif(n, min=0, max=1))        # simulation 2 for n=15
lines(density(res), lty=2)
n = 25; for(i in 1:500) res[i] = mean(runif(n, min=0, max=1))        # simulation 3 for n=25
lines(density(res), lty=3)

# Question 6.4
res <- c()
n = 5; for(i in 1:500) res[i] = mean(rexp(n, rate = 1))              # simulation 1 of the Exponential(1) for n=5 
plot(density(scale(res)))
n = 15; for(i in 1:500) res[i] = mean(rexp(n, rate = 1))             # simulation 2 for n=15 
plot(density(scale(res)))
n = 25; for(i in 1:500) res[i] = mean(rexp(n, rate = 1))             # simulation 3 for n=25; looks approximately normal
plot(density(scale(res)))
n = 50; for(i in 1:500) res[i] = mean(rexp(n, rate = 1))             # simulation 4 for n=50 
plot(density(scale(res)))

# Question 6.5
res <- c()
n = 2; for(i in 1:500) res[i] = mean(rt(n, df = 3))                  # simulation 1 of the t-dist with 3 df for n=2                      
plot(density(res), lwd = 2)
n = 12; for(i in 1:500) res[i] = mean(rt(n, df = 3))                 # simulation 2 for n=12
plot(density(res), lwd = 2)
n = 25; for(i in 1:500) res[i] = mean(rt(n, df = 3))                 # simulation 3 for n=25; looks approximately normal       
plot(density(res), lwd = 2)

# Question 6.6
f <- function(n) mean(rt(n, df = 3))
g <- function(n) median(rt(n, df = 3))
res.mean <- c() ; res.median <- c()
for(i in 1:200) {
  res.mean[i] = f(10)
  res.median[i] = g(10)
}
boxplot(list(mean=res.mean, median=res.median))                      # mean has a bigger spread, 
                                                                     # thus median is a better measure of spread for t-dist with df 3

# Question 6.7
res = c()
n = 4;   for(i in 1:500) res[i] = sum(rnorm(n)^2); qqnorm(res)
n = 10;  for(i in 1:500) res[i] = sum(rnorm(n)^2); qqnorm(res)
n = 25;  for(i in 1:500) res[i] = sum(rnorm(n)^2); qqnorm(res)
n = 50;  for(i in 1:500) res[i] = sum(rnorm(n)^2); qqnorm(res)
n = 100; for(i in 1:500) res[i] = sum(rnorm(n)^2); qqnorm(res)
# the central limit theorem says that, 
# the sampling distribution of the sample average of a large number of i.i.d random numbers is approximately normal 
# this indicates that the chi-squared distribution should become bell-shaped for large values of n
# the above simulation shows that this happens by n=100 or so

# Question 6.8
xbar <- c() ; std <- c()
for(i in 1:500) {
  sam = rnorm(10)
  xbar[i] = mean(sam)
  std[i] = sd(sam)
}
plot(xbar,std)
cor(xbar, std)               # for a normal parent dist, the two are actually independent

for(i in 1:500) {
  sam = rt(10, df = 3)
  xbar[i] = mean(sam)
  std[i] = sd(sam)
}
plot(xbar,std)
cor(xbar, std)               # for a t- parent dist wit df 3, the two are independent

for(i in 1:500) {
  sam = rexp(10, rate = 1)
  xbar[i] = mean(sam)
  std[i] = sd(sam)
}
plot(xbar,std)
cor(xbar, std)               # for an exponential dist with rate 1, the two look more correlated

# Question 6.9 
res <- c() ; m = 200
n = 3 ; for(i in 1:m) {
          x = rnorm(n)                           # mu=0, sigma=1 
          res[i] = (mean(x)-0/(sd(x)/sqrt(n))) }
qqnorm(res)
n = 10 ; for(i in 1:m) {
          x = rnorm(n)                           # mu=0, sigma=1 
          res[i] = (mean(x)-0/(sd(x)/sqrt(n))) }
qqnorm(res)
n = 25 ; for(i in 1:m) {
          x = rnorm(n)                           # mu=0, sigma=1 
          res[i] = (mean(x)-0/(sd(x)/sqrt(n))) }
qqnorm(res)
n = 50 ; for(i in 1:m) {
          x = rnorm(n)                           # mu=0, sigma=1 
          res[i] = (mean(x)-0/(sd(x)/sqrt(n))) }
qqnorm(res)
n = 100 ; for(i in 1:m) {
          x = rnorm(n)                           # mu=0, sigma=1 
          res[i] = (mean(x)-0/(sd(x)/sqrt(n))) }
qqnorm(res)
# it gets approximately normal for no values

# Question 6.10
par(mfrow = c(1,2))
res <- c() ; m = 1000
n = 3 ; for(i in 1:m) {
          x = rnorm(n)                           # mu=0, sigma=1 
          res[i] = (mean(x)-0/(sd(x)/sqrt(n))) }                    
qqplot(res,rt(m, df = n-1))
qqnorm(res)
n = 10 ; for(i in 1:m) {
          x = rnorm(n)                           # mu=0, sigma=1 
          res[i] = (mean(x)-0/(sd(x)/sqrt(n))) }                    
qqplot(res,rt(m, df = n-1))
qqnorm(res)
n = 25 ; for(i in 1:m) {
          x = rnorm(n)                           # mu=0, sigma=1 
          res[i] = (mean(x)-0/(sd(x)/sqrt(n))) }                    
qqplot(res,rt(m, df = n-1))
qqnorm(res)
n = 50 ; for(i in 1:m) {
          x = rnorm(n)                           # mu=0, sigma=1 
          res[i] = (mean(x)-0/(sd(x)/sqrt(n))) }                    
qqplot(res,rt(m, df = n-1))
qqnorm(res)
n = 100 ; for(i in 1:m) {
          x = rnorm(n)                           # mu=0, sigma=1 
          res[i] = (mean(x)-0/(sd(x)/sqrt(n))) }                    
qqplot(res,rt(m, df = n-1))
qqnorm(res)
par(mfrow = c(1,1))
# for n=3 the long tail may produce outliers that skew the expected straight line for the q-q plot 
# this isn't so apparent by n=10, yet the difference between the plot produced with qqplot() and that with qqnorm() should still be apparent

# Question 6.11
par(mfrow = c(1,3))
res <- c() ; m = 1000
n = 16 ; for(i in 1:m) {
          x = rt(n, df = n-1) 
          res[i] = (mean(x)-0/(sd(x)/sqrt(n))) }                    
qqplot(res, rt(m, df = n-1))                     # having an approximate t-distribution
n = 3 ; for(i in 1:m) {
          x = rt(n, df = n-1) 
          res[i] = (mean(x)-0/(sd(x)/sqrt(n))) }                    
qqplot(res, rt(m, df = n-1))                     # produced outliers, skewed
n = 10 ; for(i in 1:m) {
          x = (rexp(n, rate = 1))-1 
          res[i] = (mean(x)-0/(sd(x)/sqrt(n))) }                    
qqplot(res, rt(m, df = n-1))                     # no outliers but skewed 
par(mfrow = c(1,1))

# Question 6.12

