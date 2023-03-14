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


# Chapter 7 Confidence Intervals ---------------------------------------------


# in this chapter, we use probability models to make statistical inferences about the parent dist of a sample

# 7.1 Confidence interval ideas

# 7.1.1 Finding confidence intervals using simulation

pop <- rep(0:1, c(10000-5600, 5600))
phat <- mean(sample(pop, 100))                   # without replacement
phat                                             # the estimated proportion of 1 ("for" votes) in 100 samples selected from the population

res <- c()
for(i in 1:1000) res[i] = mean(sample(pop, 100)) # simulation of phat

quantile(res, c(0.1,0.9))                        # 80% of the time; P(0.50 <= phat <= 0.63) = 0.80
quantile(res, c(0.05,0.95))                      # 90% of the time; P(0.48 <= phat <= 0.64) = 0.90
quantile(res, c(0.025,0.975))                    # 95% of the time; P(0.47 <= phat <= 0.66) = 0.95

# we interpret these by envisioning picking one of the 1000 samples at random and asking the probability that it is in that range
# this should be close to the true probability (p=0.56) that phat is in the range, as we have generated many realizations of phat

# let us rewrite
# if we specify a probability, 
# then we can find an interval around a randomly chosen sample value, phat, that contains p with the specified probability
# this interval is called a confidence interval,
# as we have a certain confidence (given by the probability) that the parameter, p, is in this random interval

# 7.2 Confidence intervals for a population proportion, p

zstar <- -qnorm(alpha/2)                         # left tail
zstar <- qnorm(1-alpha/2)                        # right tail

alpha <- 2*pnorm(-zstar)                         # inverse relationship

# example
n = 1013
phat <- 466/n
SE <- sqrt(phat*(1-phat)/n)
alpha = 0.05
zstar <- -qnorm(alpha/2)
zstar                                            # nearly 2 if doing by hand
c(phat-zstar*SE, phat+zstar*SE)                  # (0.4293,0.4907) ; the CI does not include 0.5
phat + c(-1,1)*zstar*SE                          # less typing for CI

# example : missing CI
# phat=0.57, n=1000, margin of error=0.03 what is alpha?
zstar <- 0.03/sqrt(0.57*(1-0.57)/1000)
zstar
alpha <- 2*pnorm(-zstar)
alpha
1-alpha                                          # there is an implied 95 CI

# 7.2.1 Using prop.test() to find confidence intervals

# prop.test(x, n, conf.level=0.95, conf.int=TRUE)
# x : frequency ; n : sample size ; conf.level = 0.95 by default

prop.test(466, 1013, conf.level = 0.95)

# binom.test() will also find CI. In this case, it uses the binomial dist in place of the normal approximation

# Problems 7.2.2

# Question 7.1

# Question 7.2
# not everyone in the USA has a listed phone number

# Question 7.3
# because first not every Internet user choose to participate
# second not everyone in the population is an Internet user

# Question 7.4
# I would believe more in CNN's poll as sample size is way larger and may include all Zogby poll participants

# Question 7.5
n = 100 ; phat = 0.45
SE <- sqrt(phat*(1-phat)/n)
zstar <- -qnorm(0.20/2)                          # with 80% confidence ; alpha = 0.20
phat + c(-1,1)*zstar*SE
zstar <- -qnorm(0.10/2)                          # 90% confidence ; alpha = 0.10
phat + c(-1,1)*zstar*SE

prop.test(n*phat, n, conf.level = 0.80)
prop.test(n*phat, n, conf.level = 0.90)

# Question 7.6
n = 100 ; x = 5 ; p = 0.10
prop.test(x, n, conf.level = 0.95)               # CI -> 0.01855256 - 0.11829946 ; YES, population parameter p = 0.10 is in the CI

# Question 7.7
n = 10 ; x = 9 
prop.test(x, n, conf.level = 0.80)               #  CI -> 0.6577132 - 0.9901076

# Question 7.8
alpha = 0.05 ; SE*zstar = 0.02 ; phat = 0.54
zstar <- -qnorm(alpha/2)
zstar
n <- phat*(1-phat)*(zstar^2)/((0.02)^2)          # n = 787

# Question 7.9
n = 5 ; x = 4                                    # thus phat = 0.80
prop.test(x, n, conf.level = 0.90)               # CI -> 0.3493025 - 0.9861052
n = 100 ; phat = 0.80
prop.test(n*phat, n, conf.level = 0.90)          # CI -> 0.7212471 - 0.8617706
n = 1000 ; phat = 0.80
prop.test(n*phat, n, conf.level = 0.90)          # CI -> 0.7778789 - 0.8204633

# Question 7.10
n = 250 ; phat = 0.45
prop.test(n*phat, n, conf.level = 0.95)          # CI -> 0.3876067 - 0.5139554

ME <- phat - 0.3876067                           # or ME <- 0.5139554 - phat ; roughly 0.062 

n = 1000 ; phat = 0.45
prop.test(n*phat, n, conf.level = 0.95)          # CI -> 0.4189204 - 0.4814685

ME <- phat - 0.4189204                           # or ME <- 0.4814685 - phat ; roughly 0.031

# CI for n=1000 is less than for n=250 as n increases, the CI gets less
# ME for n=1000 is half of ME for n=250 as margin of error is inversely proportional with sqrt(n) 

# Question 7.11
# no matter what phat is phat(1-phat) is largest when phat = 1/2
# so we have n large enough if
alpha = 0.05
zstar <- -qnorm(alpha/2)
(zstar/0.01)^2/4                                 # n should be 9604

alpha = 0.20
zstar <- -qnorm(alpha/2)
(zstar/0.01)^2/4                                 # n should be 4106

# Question 7.12
m = 50; n = 20 ; p = 0.5
alpha = 0.50
zstar <- qnorm(1-alpha/2)
phat <- rbinom(m, n, p)/n                        # divide by n for proportions
SE <- sqrt(phat*(1-phat)/n)
sum(phat-zstar*SE<p & p<phat+zstar*SE)/m
matplot(rbind(phat - zstar*SE , phat + zstar*SE),
        rbind(1:m , 1:m), type = "l", lty = 1)
abline(v = p)                                    # indicate parameter value

# 7.3 Confidence intervals for the population mean, mu

# the success of finding a CI for p in terms of phat depended on knowing the sampling dist of phat once we standardized it
# we can use the same approach to find a CI for mu, the population mean, from the sample mean Xbar

# if sigma IS known (population data IS known) and n IS large enough
# the central limit theorem : Z = Xbar-mu / sigma/sqrt(n), z-scores for each Xbar (Z), will have an approximately normal dist
# result: mu is in the interval Xbar +/- 2*sigma/sqrt(n) with probability 0.95

# if sigma IS NOT known (population data IS NOT known) and n IS still large enough
# replace the unknown population sd, sigma, by the known sample sd, s
# the central limit theorem still applies : T = Xbar-mu / s/sqrt(n) , T has a sampling dist approximately normal
# result: mu is in the interval Xbar +/- 1.96*s/sqrt(n) with probability 0.95

# if sigma MAY BE known (population data MAY BE known) but n IS NOT large enough
# In this case, sampling dist of T is the t-distribution with n-1 df
# t-distribution is a symmetric, bell-shaped dist that asymptotically approaches the standard normal dist but for small n has fatter tails
# the df, n-1, is a parameter for this dist the way the mean and sd are for the normal dist

# t.test(x, conf.level=0.95) will compute the confidence intervals
# x : data stored in data vector x 

tstar <- qt(1-alpha/2, df = n-1)
alpha <- 2*pt(-tstar, df = n-1)

zstar <- qnorm(1-alpha/2)
alpha <- 2*pnorm(-zstar)

# example
Xbar = 66; s = 4; n = 30
alpha = 0.20
tstar <- qt(1-alpha/2, df = n-1)
SE <- s/sqrt(n)
c(Xbar - tstar*SE, Xbar + tstar*SE)

# example
ozs <- c(1.95,1.80,2.10,1.82,1.75,2.01,1.83,1.90)
qqnorm(ozs)                                      # approximately linear
t.test(ozs, conf.level = 0.80)                   # CI -> 1.836 - 1.954

# the T-statistic robust : 
# sampling dist of T is the t-dist with n-1 df. This is true when the Xi are i.i.d. normal
# what if the Xi are not normal?
# if n is small, we can do simulations to see that the dist of T still approximately the t-dist if the parent dist of the Xi is not too far from normal
# that is, the tails can't be too long, or the skew can't be too great
# when n is large, the central limit theorem applies
# a statistic whose sampling distribution doesn't change dramatically for moderate changes in the population dist is called a robust statistic

# 7.3.1 One-sided confidence intervals

# prop.test(), binom.test(), t.test() can return one-sided confidence intervals
# when argument alt="less" is used, an interval of the type (-Inf,b] is printed
# similarly, when alt="greater" is used, an interval of the type [b,Inf) is printed

# example
x <- c(175,185,170,184,175)
t.test(x, conf.level = 0.90, alt = "less")       # 90 percent confidence interval: -Inf - 182.2278

# Problems 7.3.2

# Question 7.13
Xbar = 9.5 ; s = 1 ; n = 15
alpha = 0.1
tstar <- qt(1 - alpha/2, df = n-1)
SE <- s/sqrt(n)
c(Xbar - tstar*SE, Xbar + tstar*SE)              # CI for n=15 -> 9.045 - 9.955 ; 10 days is not in this interval

# Question 7.14
with(stud.recs,                                  # data set is stud.recs, data vector x is sat.m
     t.test(sat.m, conf.level = 0.90))           # mean of x : 485.9375 ; CI for df=159 -> 476.8953 - 494.9797

# Question 7.15
with(homedata,                                   # data set is homedata, data variable is y1970
     t.test(y1970, conf.level = 0.90))           # mean of y1970 : 70820.93 ; CI for df=6840 -> 70377.72 - 71264.14
with(homedata,                                   # data set is homedata, data variable is y2000
     t.test(y2000, conf.level = 0.90))           # mean of y2000 : 268369.8 ; CI for df=6840 -> 265769.5 - 270970.0

# Question 7.16
# 5-year-olds = 60-72-months-old
ind <- kid.weights$age < 72 & kid.weights$age >= 60 
t.test(kid.weights$weight[ind], conf.level = 0.90)

# Question 7.17
hist(brightness)
t.test(brightness, conf.level = 0.90)            # mean of brightness : 8.417743 ; CI for df=965 -> 8.349184 - 8.486303

# Question 7.18
with(normtemp, hist(temperature))
t.test(normtemp$temperature, conf.level = 0.90)  # mean of temperature : 98.24923 ; CI for df=129 -> 98.14269 - 98.35577. It doesn't include 98.6 F

# Question 7.19
Xbar <- 67.5 ; s = 2.54 ; n = 4
alpha = 0.05
tstar <- qt(1-alpha/2, df = n-1)
SE <- s/sqrt(n)
c(Xbar - tstar*SE, Xbar + tstar*SE)              # CI for df=3 -> 63.45829 - 71.54171

# Question 7.20
n = 10 ; m = 250 ; df = n-1
res <- c()
for(i in 1:m) {
  x = rnorm(n, mean=0, sd=1)                     # change this line only
  res[i] = (mean(x)-0)/(sd(x)/sqrt(n))
}
qqplot(res, rt(m, df=df))                        # compare the dist of the sample with a sample from the t-distribution 
                                                 # close to linear, which means that sampling distribution is the t-distribution

n = 10 ; m = 250 ; df = n-1
res <- c()
for(i in 1:m) {
  x = runif(n)-1/2
  res[i] = (mean(x)-0)/(sd(x)/sqrt(n))
}
qqplot(res, rt(m, df=df))                        # short-tailed

n = 10 ; m = 250 ; df = n-1
res <- c()
for(i in 1:m) {
  x = rt(n,3)
  res[i] = (mean(x)-0)/(sd(x)/sqrt(n))
}
qqplot(res, rt(m, df=df))                        # symmetric, long-tailed

n = 10 ; m = 250 ; df = n-1
res <- c()
for(i in 1:m) {
  x = rt(n,30)
  res[i] = (mean(x)-0)/(sd(x)/sqrt(n))
}
qqplot(res, rt(m, df=df))                        # symmetric, not so long-tailed

n = 10 ; m = 250 ; df = n-1
res <- c()
for(i in 1:m) {
  x = rexp(n)-1
  res[i] = (mean(x)-0)/(sd(x)/sqrt(n))
}
qqplot(res, rt(m, df=df))                        # skewed

# Question 7.21
n = 3                                            # for n=3
boxplot(rt(1000, df = n-1), rnorm(1000))         # compare the tail of t-distribution with normal distribution 

x <- seq(0,1, length = 150)
plot(qt(x, df = n-1), qnorm(x))                  # compare the quantiles of t-distribution with normal distribution
abline(0,1)

curve(dnorm(x), -3.5, 3.5)
curve(dt(x, df = n-1), lty = 2, add = TRUE)      # compare the densities of t-distribution with normal distribution

n = 10                                           # for n=10
boxplot(rt(1000, df = n-1), rnorm(1000))
x <- seq(0,1, length = 150)
plot(qt(x, df = n-1), qnorm(x))
abline(0,1)
curve(dnorm(x), -3.5, 3.5)
curve(dt(x, df = n-1), lty = 2, add = TRUE)

n = 25                                           # for n=25 ; seems large enough to say the two dists are essentially the same
boxplot(rt(1000, df = n-1), rnorm(1000))
x <- seq(0,1, length = 150)
plot(qt(x, df = n-1), qnorm(x))
abline(0,1)
curve(dnorm(x), -3.5, 3.5)
curve(dt(x, df = n-1), lty = 2, add = TRUE)

n = 50                                           # for n=50
boxplot(rt(1000, df = n-1), rnorm(1000))
x <- seq(0,1, length = 150)
plot(qt(x, df = n-1), qnorm(x))
abline(0,1)
curve(dnorm(x), -3.5, 3.5)
curve(dt(x, df = n-1), lty = 2, add = TRUE)

n = 100                                           # for n=100
boxplot(rt(1000, df = n-1), rnorm(1000))
x <- seq(0,1, length = 150)
plot(qt(x, df = n-1), qnorm(x))
abline(0,1)
curve(dnorm(x), -3.5, 3.5)
curve(dt(x, df = n-1), lty = 2, add = TRUE)

# Question 7.22
m = 200 ; n = 10 ; sigma = 2 ; mu = 0 ; alpha = 0.10

SD <- c()                                        # confidence intervals using the dist of Z (population, sigma)
SE <- c()                                        # confidence intervals using the dist of T (sample, s)
for (i in 1:m) {
  X <- rnorm(n, mean=mu, sd=sigma)
  SD[i] <- qnorm(1-alpha/2)*sigma/sqrt(n)
  SE[i] <- qt(1-alpha/2, df = n-sigma)*sd(X)/sqrt(n) 
}
100 * (sum(SE > SD) / m)                         # the percentage of the time the CI using SD(Xbar) was smaller than usind SE(Xbar)

# 7.4 Other confidence intervals

# to form confidence intervals, we have used the key fact that certain statistics, 
# (phat-p) / sqrt(phat(1-phat)/n) and (Xbar-mu) / s/sqrt(n) ,
# have known sampling distributions that do not involve any population parameters 
# from this, we could then solve for confidence intervals for the parameter in terms of known quantities
# in general, such a statistic is called a pivotal quantity and can be used to generate number of confidence intervals in various situations

# 7.4.1 Confidence interval for sigma^2

# for example, if the Xi are i.i.d. normal, then the distribution of (n-1)*s^2 / sigma^2 is known to be the chi-squared distribution with n-1 df
# this allows us to solve for confidence intervals for sigma^2 in terms of the sample variance s^2
# for a given alpha : P(lstar <= (n-1)*s^2 / sigma^2 <= rstar) = 1-alpha
# given that lstar and rstar yield equal areas in the tails,
# the interval ((n-1)*s^2/rstar, (n-)*s^2/lstar) gives a (1-alpha)100% confidence interval for sigma^2

# example
s2 = 12 ; n = 10 ; alpha = 0.05
lstar <- qchisq(alpha/2, df = n-1)
rstar <- qchisq(1-alpha/2, df = n-1)
(n-1)*s2*c(1/rstar,1/lstar)                      # CI for sigma^2
sqrt((n-1)*s2*c(1/rstar,1/lstar))                # CI for sigma

# Problems 7.4.2

# Question 7.23
# let X1,X2,..., Xn and Y1,Y2,...,Ym be two i.i.d. samples with sample variances s_x and s_y 
# a CI for the equivalence of sample variances can be given from the F-statistic, (s_x^2/sigma_x^2) / (s_y^2/sigma_y^2)
# if the underlying Xi and Yi are normally distributed, 
# then the distribution of F is known to be the F-distribution with n-1 and m-1 degrees of freedom
# that is, F is a pivotal quantity, so probability statements,
# such as P (a <= (s_x^2/sigma_x^2) / (s_y^2/sigma_y^2) <= b) can be answered with the known quantiles of the F distribution

# example
n = 11 ; m = 16 ; alpha = 0.10
qf(c(alpha/2 , 1-alpha/2), df1 = n-1, df2 = m-1) # says that P(0.3515 <= (s_x^2/sigma_x^2) / (s_y^2/sigma_y^2) <= 2.5437) = 0.90 when n=11, m=16

n = 10 ; m = 20 ; alpha = 0.20
s_x = 2.3 ; s_y = 2.8
qf(c(alpha/2 , 1-alpha/2), df1 = n-1, df2 = m-1) # P(0.434 <= (s_x^2/sigma_x^2) / (s_y^2/sigma_y^2) <= 1.984) = 0.80
upperbound <- sqrt(1/0.434)*(s_x/s_y)
lowerbound <- sqrt(1/1.984)*(s_x/s_y)            # P(0.583 < sigma_x / sigma_y < 1.247) = 0.80

# Question 7.24
# assume our data, X1, X2,..., Xn is uniform on the interval [0,teta] (teta is an unknown parameter) 
# set max(X) to the be maximum value in the data set 
# then the quantity max(X)/teta is pivotal with distribution P(max(X)/teta < x) = x^n , 0 <= x <= 1
# thus P(max(X)/x < teta) = x^n 
# as teta is always bigger than max(X), we can solve for x^n=alpha and get that teta is in the interval [max(X),max(X)/x] with probability 1-alpha

attach(nym.2002)
n <- length(place)
alpha = 0.10
(1-alpha)^(1/n)
max(place)/(1-alpha)^(1/n)                       # 23664
max(place)                                       # 23662 
detach(nym.2002)
# so it is 90% certain that teta is between 23662 and 23664 (the real answer is 23664)

# 7.5 Confidence intervals for differences

# 7.5.1 Difference of proportions

# to see if a difference in the proportions is explainable by sampling error, 
# we look at phat1-phat2 and find a confidence interval for p1-p2 
# this can be done, as the statistic Z = (phat1-phat2)-(p1-p2) / SE(phat1-phat2) is a pivotal quantity
# with standard normal distribution when n1 and n2 are large enough
# the SE is SE(phat1-phat2) = sqrt(phat1(1-phat1)/n1 + phat2(1-phat2)/n2)
# Z has an asymptotic normal distribution 
# prop.test() can do the calculations

# example

prop.test(x = c(560,579), n = c(1000,1200), conf.level = 0.95)

# prop1=0.56, prop2=0.4825 ; CI at 95% confidence level for n=1000 and n=1200 ->  0.03479986 - 0.12020014
# the CI misses including 0 (the mean of the standard normal dist, estimated dist of Z)
# thus, we conclude that there appears to be a real difference in the population parameters

# 7.5.2 Difference of means

# to see whether independent samples come from identical parent populations
# assume the populations for each sample are normally distributed
# the sampling dist of Xbar-Ybar is asymptotically normal as each is asymptotically normal
# consequently, T = (Xbar-Ybar)-(Mu_x-Mu_y) / SE(Xbar-Ybar), pivotal, will have an approximately normal dist, with mean=0 and var=1 fro large n_x and n_y
# for small n_x and n_y , T will have the t-distribution
# the SE(Xbar-Ybar) and degrees of freedom are computed differently depending on whether or not the population variances are equal
# t.test(x, y, var.equal=FALSE, conf.level=0.95) computes the CI of the difference of means of two samples to see if they come from the same population

# example
x <- c(0,0,0,2,4,5,13,14,14,14,15,17,17)
y <- c(0,6,7,8,11,13,16,16,16,17,18)

boxplot(list(drug=x, placebo=y), col = "gray")   # compare spreads ; the medians are the same; the spreads are different; the means to be checked

t.test(x, y, var.equal = TRUE, conf.level = 0.95)                     
# results of equal variances assumption:     t=-1.0542, df=22, p-value=0.3032 ; CI -> -8.279119 - 2.698699 ; mean of x=8.846154, mean of y=11.636364

t.test(x, y, var.equal = FALSE, conf.level = 0.95)
# results of different variances assumption: t=-1.0722, df=21.99, p-value=0.2953 ; CI -> -8.187298 - 2.606878 ; mean of x=8.846154, mean of y=11.636364

# overall results : the dfs are essentially identical for both cases (equal and unequal variances)
# the difference of 0 is still in the 95% CIs, even though the sample means differ quite a bit at first glance (8.846 vs 11.636), which means
# the difference in means may be explained by sampling error

# 7.5.3 Matched samples

# used for independent samples (paired or matched samples)
# T statistic, T = (Xbar-Ybar)-(Mu_x-Mu_y) / SE(Xbar-Ybar) is the pivotal quantity with a t-distribution
# what is standard error here as the samples are not independent?
# it is just the SE for the single sample Xi and Yi
# t.test() can compute the CI. If x and y store the data  t.test(x, y, paired=TRUE, conf.level=0.95) or shortly : t.test(x-y)

# example
library(MASS)
names(shoes)
with(shoes, t.test(A-B, conf.level = 0.90))      # or
with(shoes, t.test(A, B, paired = TRUE, conf.level = 0.90))          # it does't include 0, indicating that there may be a difference in the means

# Problems 7.5.4

# Question 7.25
# assumption 1 : parent populations are normally distributed
# assumption 2 : sample sizes (seven) are large enough 

x <- c(3.1,3.3,1.7,1.2,0.7,2.3,2.9)
y <- c(1.8,2.3,2.2,3.5,1.7,1.6,1.4)

boxplot(list(cocktail1=x,cocktail2=y), col="gray")               # compare spreads

t.test(x, y, var.equal = FALSE, conf.level = 0.80)   
# CI -> -0.5322402 - 0.7322402 , includes 0, there may not be a difference in the means ; the means : x=2.171429, y=2.071429

t.test(x, y, var.equal = TRUE, conf.level = 0.80)
# CI -> -0.5281055 - 0.7281055 , includes 0, there may not be a difference in the means ; the means : x=2.171429 , y=2.071429

# Question 7.26
# assumption : parent populations are normally distributed

x <- c(7,0,8,1,10,12,2,9,5,2)
y <- c(2,1,5,1,5,7,-1,8,7,3)

boxplot(list("400mg"=x, "1200mg"=y), col="gray")               # compare spreads

t.test(x, y, var.equal = FALSE, conf.level = 0.90)   
# CI -> -1.058279 - 4.658279 , includes 0, there may not be a difference in the means ; the means : x=5.6, y=3.8

t.test(x, y, var.equal = TRUE, conf.level = 0.90)
# CI -> -1.043488 - 4.643488 , includes 0, there may not be a difference in the means ; the means : x=5.6 , y=3.8

# Question 7.27
# this data is paired off, as it is reasonable to assume that there is a correlation between the IQ scores of twins
# assumption :  differences in the scores can be viewed as a random sample from a normally distributed population

x <- c(80,88,75,113,95,82,97,94,132,108)
y <- c(90,91,79,97,97,82,87,94,131,115)

plot(x,y)
boxplot(x-y)
# the exploratory plots show the correlation between the two sets of scores and the distribution of the differences, 
# indicating that a paired t-test is appropriate

t.test(x, y, paired = TRUE, conf.level = 0.90)   # CI for the differences of mean -> -4.368937 - 4.568937, including 0, no difference is possible

# Question 7.28
attach(babies)
plot(age, dage)              # seems they are correlated, thus the samples can be matched
boxplot(age, dage)           # distribution of the variables

t.test(age, dage, paired = TRUE, conf.level = 0.95)
# CI for the differences of mean -> -3.745356 - -2.986035, not including 0, there should be a difference in the means

detach(babies)

# 7.6 Confidence intervals for the median

# used when the sample is an independent sample but not taken from a normal or close to normal population
# for these situations, nonparametric methods are preferred

# 7.6.1 Confidence intervals based on the binomial

# let T count the number of data points more than the median in a sample of size n
# T is a Binomial(n,0.5) random variable
# let X1,X2,...,Xn be the sample after sorting from smallest to largest
# find the largest j so that P(j <= T <= n-j) > 1-alpha , which in turn becomes a search for the largest j with P(T<j)<alpha/2

# example
x <- c(110,12,2.5,98,1017,540,54,4.3,150,432)
n <- length(x)
pbinom(0:n, n, 0.5)                              # P(T <= k) here, not P(T < k)
 # [1] 0.0009765625 0.0107421875 0.0546875000 0.1718750000 0.3769531250
 # [6] 0.6230468750 0.8281250000 0.9453125000 0.9892578125 0.9990234375
 # [11] 1.0000000000
# for 90% CI, alpha/2 = 0.05, thus largest j that satisfies P(T < j) < 0.05 is 2
# so, 90% CI that fulfills P(Xj <= M <= X(n+1-j) >= 1-alpha is [4.3,540]

# shortly
j <- qbinom(0.05, n, 0.5)
sort(x)[c(j,n+1-j)]                              # 4.3 540.0

# 7.6.2 Confidence intervals based on signed-rank statistic

# the Wilcoxon signed-rank statistic allows for an improvement on the CI given by the number of data points above the median
# its usage is valid when the Xi are assumed to be symmetric about their median
# if this is so, then a data point is equally likely to be on the left and right of the median,
# and the distance from the median is independent of what side of the median the data point is on
# if we know the median then we can rank the data by distance to the median

# In R, it is available under the family name signrank
# qsignrank() : returns the quantiles
# the procedure is implemented in the wilcox.test()
# we need to specify that a CI is desired with the argument conf.int=TRUE

# example
x <- c(110,12,2.5,98,1017,540,54,4.3,150,432)
boxplot(list("CEO"=scale(x), "log.CEO"=scale(log(x))))               # scale() makes a data set have mean 0 and variance 1
title("Boxplot of CEO data and its logarithm")

wilcox.test(log(x), conf.int = TRUE, conf.level = 0.90)              # CI -> 2.963463 - 5.539530 , (pseudo)median = 4.344732
exp(c(2.963463,5.539530))                                            # the new CI -> [19.37,254.56] from [4.3,540.0]

# 7.6.3 Confidence intervals based on the rank-sum statistic

# if the two distributions are the same up to a possible shift of center, then a confidence interval based on a nonparametric statistic can be given
# let f(x) be a density for a mean-zero distribution, and suppose we have two independent random samples: 
# the first, X1,X2...,Xn_x from a population with density f(x-mu_x), 
# and the second,Y1,Y2,...,Yn_y from a population with density f(x-mu_y) 
# the basic statistic, called the rank-sum statistic, 
# looks at all possible pairs of the data and counts the number of times the X value is greater than or equal to the Y value 
# if the population mean for the X values is larger than the population mean for the Y values, this statistic will likely be large 
# if the mean is smaller, then the statistic will likely be small 
# the distribution of this statistic is given by R with the wilcox family 
# and is used to give a confidence interval for the difference of the means
# the command wilcox.test (x, y, conf . int=TRUE). function will find a confidence interval for the difference in medians of the two data sets

# example
pay.00 <- c(110,12,2.5,98,1017,540,54,4.3,150,432)
pay.02 <- c(312,316,175,200,92,201,428,51,289,1126,822)

plot(density(pay.02), main = "densities of y2000,y2002")
lines(density(pay.00),lty=2)
# shows two data sets that are quite skewed, so confidence intervals based on the T statistic would be inappropriate 
# rather, as the two data sets have a similar shape, we find the confidence interval returned by wilcox.test()

wilcox.test(pay.02, pay.00, conf.int = TRUE, conf.level = 0.9)
# the 90% confidence interval, [-18,282], contains 0 value of ()
# this example would be improved if we had matched or paired data—that is, the salaries for the same set of CEOs in the year 2000 and 2002 - as then
# differences in the sampling would be minimized 
# if that case is appropriate, then adding the argument paired=TRUE to wilcox.test() computes a confidence interval based on the signed-rank statistic

# Problems 7.6.4

# Question 7.29
x <- c(21,21,21,22,22,23,23,24,24,24,24,24,24,25,25,26,29,31,31,33)
stem(x)                      # the data set implies that the population distribution is skewed and not normally distributed or symmetric        
stem(log(x))                 # thus t.test() and wilcox.test() are not appropriate. However, after a log transform, it appears that wilcox.test() will be

wilcox.test(log(x), conf.int = TRUE, conf.level = 0.90)             
exp(c(3.134569,3.262468))    # 90% CI for the median -> 22.97873 - 26.11391 ; median: 24

# Question 7.30
attach(cabinet)
hist(est.tax.savings)        # skewed, not normally distributed or symmetric
hist(log(est.tax.savings))   # more symmetric

wilcox.test(log(est.tax.savings), conf.int = TRUE, conf.level = 0.90)
exp(c(7.240808,9.736620))
# 90% CI for the median -> 1395.221 - 16926.234 ; median : 5133.485
detach(cabinet)

# Question 7.31
# we might envision the collection of songs on an album as coming from a population of all possible songs the band could have written at the given time 
# a difference of means would indicate a tendency to write longer songs at one of the given times

wilcox.test(u2$October, u2$"The Joshua Tree", conf.int = TRUE, conf.level = 0.95)
# 95% CI for the difference in median between the albums October and "The Joshua Tree" is [-94 , -10]

# Question 7.32
hist(cfb$AGE)                # symmetric

wilcox.test(cfb$AGE, conf.int = TRUE, conf.level = 0.95)             # [48 , 50] ; median : 49

hist(cfb$INCOME)             # no symmetry
hist(log(cfb$INCOME+1))      # symmetric

wilcox.test(log(cfb$INCOME+1), conf.int = TRUE, conf.level = 0.95)
exp(c(10.49,10.61)) - 1      # [35953 , 40537] ; median : 38127.6

# Question 7.33
# we can simulate the signed-rank distribution and see that it applies for any symmetric distribution regardless of tail length

n = 20 ; m = 250                                 # 250 samples
res = c()                                        # the results
for(i in 1:m) {
  x = rnorm(n)
  res[i]=sum(rank(abs(x))[x>0])                  # only add positive values
}  
hist(res,prob=TRUE)
x = 40:140
lines(x,dsignrank(x,n))                          # density-like, but discrete

res = c()
for(i in 1:m) {
  x = rt(n, df = 2)
  res[i]=sum(rank(abs(x))[x>0])
}
hist(res,prob=TRUE)
x = 40:140
lines(x,dsignrank(x,n))

res = c()
for(i in 1:m) {
  x = runif(n, min = -1, max = 1)
  res[i]=sum(rank(abs(x))[x>0])
}
hist(res,prob=TRUE)
x = 40:140
lines(x,dsignrank(x,n))

res = c()
for(i in 1:m) {
  x = rexp(n, rate = 1) -1
  res[i]=sum(rank(abs(x))[x>0])
}
hist(res,prob=TRUE)
x = 40:140
lines(x,dsignrank(x,n))
