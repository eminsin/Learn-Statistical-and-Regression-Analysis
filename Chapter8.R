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


# Chapter 8 Significance Tests ---------------------------------------------


# finding a confidence interval for a parameter is one form of statistical inference. A significance test, or test of hypothesis, is another. 
# rather than specify a range of values for a population parameter, a significance test assumes a value for the population parameter
# and then computes a probability based on a sample given that assumption

# the performer of a significance test seeks to determine whether the null hypothesis (H0) is reasonable given the available data
# the evidence is an experiment that produces a test statistic 
# the probability that the test statistic is the observed value or is more extreme is calculated using the assumptions of the null hypothesis 
# this is called the p-value 
# this is like the weighing of the evidence - the jury calculating the likelihood that the evidence agrees with the assumption of innocence
# the calculation of the p-value is called a significance test 
# the p-value is based on both the sampling distribution of the test statistic under H0 and the single observed value of it during the trial

# p-value = P(test statistic is the observed value or is more extreme|H0)

# the p-value helps us decide whether differences in the test statistic from the null hypothesis are attributable to chance or sampling variation, 
# or to a failure of the null hypothesis 
# if a p-value is small, the test is called statistically significant, 
# as it indicates that the null hypothesis is unlikely to produce more extreme values than the observed one 
# small p-values cast doubt on the null hypothesis; large ones do not

# p-value range significance stars common description in R
# [0, .001]      ***   extremely significant
# (.001, .01]     **   highly significant
# (.01, .05]       *   statistically significant
# (.05, .1]        .   could be significant
# (.1, 1]              not significant

# a significance test is less vague, as a significance level is specified that the p-value is measured against
# a typical significance level is 0.05. 
# if the p-value is less than the significance level, then the null hypothesis is said to be rejected, or viewed as false
# if the p-value is larger than the significance level, then the null hypothesis is accepted

# when rejecting the null, we don't actually prove the null to be false or the alternative to be true
# all that is shown is that the null hypothesis is unlikely to produce values more extreme than the observed value
# when accepting the null we don't prove it is true, we just find that the evidence is not too unlikely if the null hypothesis is true

# by specifying a significance level, we indirectly find values of the test statistic that will lead to rejection 
# this allows us to specify a rejection region consisting of all values for the observed test statistic that produce p-values smaller than the significance level

# the boundaries between the acceptance and rejection regions are called critical values

# the use of a rejection region avoids the computation of a p-value: reject if the observed value is in the rejection region and accept otherwise 
# we prefer, though, to find and report the p-value rather than issue a simple verdict of "accept" or "reject"

# if the null hypothesis is falsely rejected, it is a type-1 error (innocent man is found guilty) 
# if the null hypothesis is false, it may be falsely accepted (guilty man found not guilty). This is a type-2 error

# the steps in finding the p-value
# 1. Identify H0 and Ha, the null and alternative hypotheses
# 2. Specify a test statistic that discriminates between the two hypotheses, collect data, then find the observed value of the test statistic
# 3. Using Ha, specify values of the test statistic that are "extreme" under H0 in the direction of Ha 
# the p-value will be the probability of the event that the test statistic is the observed value or more extreme
# 4. Calculate the p-value under the null hypothesis. The smaller the p-value, the stronger the evidence is against the null hypothesis

# 8.1 Significance test for a population proportion

# H0 : p = p0  
# Ha : p < p0  or  p> p0  or  p != p0

# can be performed with test statistic
# Z = phat-p0 / sqrt(p0*(1-p0)/n)

# if phat is based on a simple random sample and n is large enough, Z has a standard normal distribution under the null hypothesis

# in R the function prop.test() will perform this significance test

# example

# H0 : p = 0.113 ; Ha : p > 0.113
# a test statistic is based on the proportion living in poverty of a random sample of size 50,000
# in the sample, 5,850, or a proportion of 0.117, were found to be in poverty. Is this difference (between 0.113 and 0.117) significant?

p0  = 0.113 ; n = 50000 ; SD = sqrt(p0*(1-p0)/n)
pnorm(0.117, mean = p0, sd = SD, lower.tail = FALSE)

# the p-value is 0.002363 and is "highly significant"
# this data casts much doubt on the null hypothesis of no change
# we would think sampling variation alone does not explain the difference

# 8.1.1 Using prop.test() to compute p-values

# the calculation above is done by hand
# a template for usage to perform a significance test is prop.test(x, n, p=..., alternative="two.sided")
# the value for x is the sample frequency; in our case, 5,850=0.117*50,000
# the value of n is the sample size 50,000. These are the same as when we used this function to find confidence intervals
# to perform a significance test, the null and alternative hypotheses must be specified
# the null is done with the p= argument; for our example p=.113
# the alternative hypothesis is specified with the argument alternative=, which we abbreviate to alt= 
# this argument has one of these values: "less", "greater", or "two.sided". The default is two.sided

prop.test(x = 5850, n = 50000 , p = 0.113, alt = "greater")

# It isn't any more difficult to test the alternative hypothesis, that the rate has changed, or Ha : p != p0
prop.test(x = 5850, n = 50000 , p = 0.113, alt = "two.sided")

# Problems 8.1.2

# Question 8.1

# the FDA must assume that the drug is safe in the null hypothesis. The alternative would be that it is not safe

# Question 8.2
table(samhda$marijuana)

# the hypothesis test is between H0:p = 0.5 , Ha:p>0.5
prop.test(x = 134, n = 600, p = 0.5, alternative = "greater")        # p value=1; it is not significant; the data doesn't indicate an increase

# Question 8.3

# the hypothesis test is between H0:p=0.75 , Ha:p>0.75
prop.test(x = 40, n = 50, p = 0.75, alt = "greater")                 # p-value = 0.2568; not significant; the data doesn't support the alt hypothesis

# Question 8.4

# the hypothesis test is between H0:p=0.344 , Ha:p!=0.344
prop.test(x = 40, n = 75, p = 0.344, alt = "two.sided")              # p-value = 0.0008681; significant; the data supports a difference from 34.4%

# Question 8.5

# the hypothesis test is between H0:p=0.999 , Ha:p<0.999
prop.test(x = 5731, n = 5760, p = 0.999, alt = "less")               # p-value < 2.2e-16; significant; the data is inconsistent with the null hypothesis

# the normal approximation is said to be valid provided n*p and n*(1-p) are 5 or more

# Question 8.6

p = 0.113 ; n = 50000
qnorm(0.95, mean = p, sd = sqrt(p*(1-p)/n))      # 0.1153289
0.1153289*n                                      # any value of 5767 or larger would give a p-value less than 0.05

# Question 8.7

# the hypothesis test is between H0:p=0.10 , Ha:p>0.10
prop.test(x = 2700, n = 25000, p = 0.10, alt = "greater")            # p-value = 1.301e-05; extremely significant to support Ha

# Question 8.8

# the hypothesis test is between H0:p=0.50 , Ha:p!=0.50
prop.test(x = 200*0.16, n = 200, p = 0.50, alt = "two.sided")        # p-value < 2.2e-16; extremely significantly different than being 50% in favor

# 8.2 Significance test for the mean (t-tests)

# if the data X1, X2,..., Xn is an i.i.d. sequence from a Normal(mu, sigma) distribution, 
# or n is large enough for the central limit theorem to apply, a test of significance for :
# H0 : mu = mu0 
# Ha : mu < mu0 or mu > mu0 or mu != mu0
# can be performed with test statistic  T = Xbar-mu0 / s/sqrt(n)
# for a normally distributed population, T has the t-distribution with n−1 degrees of freedom under H0 
# for large n, T has the standard normal distribution

# in R, the function t.test () can be used to compute the p-value with unsummarized data, as in t.test (x, mu=..., alt=“two.sided”)
# the null hypothesis is specified by a value for the argument mu= 
# the alternative is specified as appropriate by alt="less", alt="greater", or alt="two. sided" (the default)

# example
# the hypothesis test is between H0:mu=17 , Ha:mu<17
# by hand :
mpg <- c(11.4,13.1,14.7,14.7,15.0,15.5,15.6,15.9,16.0,16.8)
xbar <- mean(mpg)
s <- sd(mpg)
n <- length(mpg)
c(xbar,s,n)                  # 14.87, 1.57, 10.0
SE <- s/sqrt(n)
T <- (xbar-17)/SE    # -4.285
pt(T, df = n-1, lower.tail = TRUE)       
# p-value = 0.00102; discredits the claim of 17 miles per gallon, as the difference of Xbar from 17 is not well explained by sampling variation

# by t.test() :
t.test(mpg, mu = 17, alt = "less")
# results : tcritical = -4.285 ; p-value = 0.00102 ; 95% CI [-Inf 15.78] ; mean of mpg = 14.87

# example
# the hypothesis test is between H0:mu=101.75 , Ha:mu>101.75
x <- c(140,125,150,124,143,170,125,94,127,53)
qqnorm(x)                    # check normality, OK
t.test(x, mu = 101.75, alt = "greater")                              
# the p-value, 0.02385, is small, indicating that the true amount per class may be more than that indicated under the null hypothesis

# Problems 8.2.1

# Question 8.9
# the hypothesis test is between H0:mu=55000 , Ha:mu>55000
mu = 55000 ; xbar = 58260 ; n = 25 ; s = 3250
SE <- s/sqrt(n)
T <- (xbar-mu)/SE
pt(T, df = n-1, lower.tail = FALSE)              
# p-value = 1.998964e-05 ; extremely significant ; the possibility of an average massage therapist makes more than $55000 per year is almost certain

# Question 8.10
# the hypothesis test is between H0:mu=2.00 , Ha:mu>2.00
mu = 2.00 ; xbar = 2.03 ; s = 0.22 ; n = 800
SE <- s/sqrt(n)
T <- (xbar-mu)/SE
pt(T, df = n-1, lower.tail = FALSE)              # p-value=6.2027e-05 ; extremely significant ; supports the Ha

# Question 8.11
# the hypothesis test is between H0:mu=500 , Ha:mu!=500
qqnorm(stud.recs$sat.m)      # normality check ; OK
t.test(stud.recs$sat.m, mu = 500, alternative = "two.sided")
# results : p-value = 0.01099 ; significant to reject the null hypothesis ; 95% CI -> [475.1437 496.7313] ; reject again for alpha=0.05

# Question 8.12
# the hypothesis test is between H0:mu=68cm , Ha:mu>68cm
x <- babies[babies$dht!=99,"dht"]
plot(density(x))             # normal check ; OK
t.test(x, mu = 68 , alternative = "greater")     # p-value= < 2.2e-16 ; extremely significant for rejecting the null hypothesis

# Question 8.13
# the hypothesis test is between H0:mu=0 , Ha:mu>0
mu = 0 ; xbar = 0.5 ; s = 3.77 ; n = 7
SE <- s/sqrt(n)
T <- (xbar-mu)/SE
pt(T, df = n-1, lower.tail = FALSE)              # p-value=0.3688294 ; the difference is NOT significantly greater than 0

# Question 8.14
# the hypothesis test is between H0:mu=0.330 , Ha:mu!=0.330
plot(density(OBP))           # normality is OK
t.test(OBP, mu = 0.330, alt = "two.sided")       # p-value = 0.8663 ; the mean onbase percentage may be equal to 0.330 under the null hypothesis

# Question 8.15
# the hypothesis test is between H0:mu=98.6 F , Ha:mu!=98.6 F
plot(density(normtemp$temperature))              # the data appears to come from a normal distribution
t.test(normtemp$temperature, mu = 98.6, alt = "two.sided")
# p-value = 2.411e-07 ; the data is extremely significant to assume value of 98.6 is NOT correct

# Question 8.16
# we can perform simulations to see how robust the t-test is to changes in the parent distribution

# for a normal population :
m = 250 ; n = 10
res = c()
for (i in 1:m) res[i] = t.test(rnorm(n), mu = 0, alt = "two.sided", df = n-1)$p.value
sum(res<0.05)/length(res)    # proportion of rejections = 0.052 ; 5.2% of the time we rejected at the alpha=0.05 significance level

# for others :
lst = list()
m = 250; n = 10
for(i in 1:m) {
  lst$exp[i]  = t.test(rexp(n),     mu=1,  df=n-1)$p.value
  lst$unif[i] = t.test(runif(n),    mu=.5, df=n-1)$p.value
  lst$t4[i]   = t.test(rt(n, df=4), mu=0,  df=n-1)$p.value
}
sapply(lst, function(x) sum(x<0.05)/length(x))
#  exp  unif    t4 
# 0.100 0.080 0.076  ;  When the population distribution is skewed, the sampling distribution of T may differ a lot from the t-distribution

# 8.3 Significance tests and confidence intervals

# H0:mu=mu0, HA:mu!=mu0.
# in either case, the T statistic, T = Xbar-mu0 / s/sqrt(n), is used to make the statistical inference
# the two approaches are related by the following:
# a significance test with significance level alpha will be rejected if and only if the (1-alpha)*100% confidence interval around does not contain mu0
# to see why, suppose alpha is given. The confidence interval uses tcritical found from P(-tcritical <= T <= +tcritical) = 1-alpha
# from this, the confidence interval will not contain mu0 if the value of T is more than +tcritical or less than -tcritical.
# if the observed value of T is more than +tcritical or less than -tcritical, 
# then the observed value is in the rejection region, and the null hypothesis is rejected

# 8.4 Significance tests for the median

# the significance test for the mean relies on a large sample, or on an assumption that the parent dist is normally (or nearly normally) distributed
# in the situation where this isn't the case, we can use test statistics similar to the ones used to find confidence intervals for the median
# Significance tests based on these test statistics are nonparametric tests, 
# as they do not make assumptions about the population parameters to calculate the test statistic 
# (though there may be assumptions about the shape of the distribution)

# 8.4.1 The sign test

# assume X1, X2,..., Xn are from a continuous distribution with positive density
# H0:median=m, Ha:median<m, median>m, or median!=m
# can be performed with test statistic T=the number of Xi with Xi>m
# if the data has values equal to m, then delete those values from the data set
# Under H0, T has a Binomial(n, 1/2) distribution 
# large values of T support the alternative that the median is greater than M
# small values of T support the alternative that the median is smaller than M 
# for two-sided alternatives, large or small values of T support Ha
# In R, the test statistic can be computed using sum() 
# the p-values are found using pbinom(k). However, as P(T >= k) = 1-P(T <= k-1), the p-value is is found with 1-pbinom(k-1, n, 1/2)

# example
# the hypothesis test is between H0:the median=5 , Ha:the median<5
calls <- c(2,1,3,3,3,3,1,3,16,2,2,12,20,3,1)
obs <- sum(calls<5)          # 12 calls smaller than 5 mins
n <- length(calls)
1-pbinom(11, n, 0.5)         # returns the one-sided p-value ; we use 11 because we want P(T>=12)
# we get a p-value of 0.0176, which leads us to believe that the median is less than 5

# for illustration, the p-value for the two-sided alternative can be computed as follows :
k <- max(obs, n-obs)
2*(1-pbinom(k-1 , n, 1/2))

# 8.4.2 The signed-rank test

# the signed-rank test is an improvement to the sign test when the population is symmetric, but not close enough to normal to use a t-test
# if the data, X1, X2, …, Xn, is an i.i.d. sample from a continuous, symmetric distribution, then
# a significance test of the hypotheses H0: the median=m, Ha: median<m, median>m, or median!=m can be performed with test statistic T
# T = sum(rank(Xi-m)) when Xi>m , which means
# if we rank all the data by its distance to m, the sum corresponding to the values larger than m may be viewed as 
# a random sample of a certain size from the numbers 1 through n
# Under H0, the distribution of T can be calculated. Large values of T support the alternative hypothsis Ha: median>m

# in R, the function wilcox.test() performs the test as wilcox.text(x, mu=..., alt="two.sided")
# the data is contained in x, the null hypothesis is specified by the argument mu=, and the alternative is specified with the argument alt=

# example
# a plot of the data, salmon.rate, shows that a normal population assumption is not correct; rather, the population appears to be lognormal
# Perform a significance test of H0: median=0.005, Ha: median>0.005

# after taking logs, we can see that the data is symmetric, so the signed-rank test can apply to the log-transformed data

wilcox.test(log(salmon.rate), mu = log(.005), alt = "greater")       # p-value=0.065

# to contrast, the p-value for the sign test is found with these commands :

T = sum(salmon.rate > 0.005)
n = length(salmon.rate)
1-pbinom(T-1, n, 1/2)

# Problems 8.4.3

# Question 8.17
# H0:the median=220000 , Ha: the median>220000
T <- sum(exec.pay>22)
n <- length(exec.pay)
pbinom(T-1, n, 1/2, lower.tail = FALSE)          # p-value=0.03252251 ; the data is significant to say that the median pay is more than 220K

# Question 8.18
wilcox.test(log(exec.pay), mu = log(22), alt = "greater")            # p-value=0.01366 ; reaches the same conclusion

# Question 8.19
attach(babies)
smokers <- gestation[smoke==1 & gestation != 999]                    # gestation times for mothers who smoked during pregnancy
plot(density(smokers))                                               # symmetric, wilcoxon test can be applies
wilcox.test(smokers, mu = 40, alt = "two.sided")                     # p-value= < 2.2e-16 ; the median is not equal to 40 weeks
detach(babies)

# Question 8.20

# if the sign test has fewer assumptions on the population, why wouldn't we always use that instead of a t-test? 
# the answer lies in the power of the sign test to detect when the null hypothesis is false
# the sign test will not reject a false null as often as the t-test

# Perform a simulation comparing the two tests on data that has a Normal(1,2) distribution
# H0:mu=0, Ha:mu>0

m = 200 ; n = 10
res.t = rep(0,m) ; res.sign = rep(0,m)
for(i in 1:m) {
  x = rnorm(n, mean = 1, sd = 2)
  if(t.test(x, mu = 0, alt = "greater")$p.value < 0.05)
    res.t[i] = 1
  if (1-pbinom(sum(x>0)-1, n, 1/2) < 0.05)  
    res.sign[i] = 1
}
sum(res.t)/m                 # proportion rejected by t-test
sum(res.sign)/m              # proportion rejected by sign-test
 
# 8.5 Two-sample tests of proportion

# in this section we consider how to compare two population proportions
# if we have sample proportions for two random samples, a significance test of H0:p1=p2, Ha:p1<p2, p1>p2, or p1!=p2 can be carried out with test statistic Z
# under H0, Z has a standard normal distribution if n1 and n2 are sufficiently large 
# large values of Z support the alternative p1>p2; small values support p1<p2
# in R, the function prop. test () will perform a two-sample test of proportions: prop.test(x, n, alt="two.sided")

# example
# H0:p1=p2, Ha:p1<p2
phat <- c(0.121,0.117)      # the sample proportions
n <- c(50000,60000)         # the sample sizes
n*phat                      # the counts

prop.test(n*phat, n, alt = "less")               # the small p-value of 0.02107 indicates an increase in the rate

# Problems 8.5.1

# Question 8.21
# H0:p1=p2, Ha:p1<p2
x <- c(14,15)
n <- c(150,125)
prop.test(x, n, alt = "less")                    #  p-value = 0.3016 ; there is not enough evidence that Brand A has a smaller chance of being returned

# Question 8.22
# H0:p1=p2, Ha:p1!=p2
x <- c(250,250)
n <- c(600,500)
prop.test(x, n, alt = "two.sided")               # p-value = 0.006871 ; the difference in proportions is statistically highly significant

# Question 8.23
# H0:p1=p2, Ha:p1!=p2
phat <- c(0.82,0.70)
n <- c(350,350)
x <- n*phat
prop.test(x, n, alt = "two.sided")               # p-value = 0.0002851 ; the difference in proportions is statistically extremely significant

# Question 8.24
# H0:p1=p2, Ha:p1!=p2
x <- c(153,196)
n <- c(30000,30000)
prop.test(x, n, alt = "two.sided")               # p-value = 0.02415 ; there is a significant difference between the death rates of the two groups

# Question 8.25
# H0:p1=p2, Ha:p1!=p2
x <- c(18,3)
n <- c(22,22)
prop.test(x, n, alt = "two.sided")               # p-value = 2.384e-05 ; there is an extremely significant difference between the proportions

# Question 8.26
# H0:p1=p2, Ha:p1<p2
phat <- c(0.989,0.969)
n <- c(1250,1100)
x <- n*phat
prop.test(x, n, alt = "two.sided")               # p-value = 0.001042 ; the difference in proportions is highly significant

# Question 8.27
# H0:p1=p2, Ha:p1!=p2
phat <- c(0.216,0.193)
n <- c(10000,10000)
x <- n*phat
prop.test(x, n, alt = "two.sided", conf.level = 0.01)
# p-value = 5.952e-05 ; the difference is extremely significant 
# however, if the surveys were only of size 1 million the difference would not have been statistically significant

# 8.6 Two-sample tests of center

# suppose Xi, i=1,...,n_x and Yj, j=1,...,n_y are random samples from the two populations of interest
# a significance test to compare the centers of their parent distributions would use the hypotheses H0:mu_x=mu_y, Ha:mu_x<mu_y, mu_x>mu_y, or mu_x!=mu_y
# a reasonable test statistic depends on the assumptions placed on the parent populations :
# if the populations are normally distributed or nearly so, and the samples are independent of each other, then a t-test can be used 
# if the populations are not normally distributed, then a nonparametric Wilcoxon test may be appropriate
# if the samples are not independent but paired off in some way, then a paired test might be called for

# 8.6.1 Two sample tests of center with normal populations

# assume are independent random samples from Normal(mu_i, sigma_i) distributions, where i=x or y. 
# a significance test of H0:mu_x=mu_y, Ha:mu_x<mu_y, mu_x>mu_y, or mu_x!=mu_y can be done with test statistic T
# T will have the t-distribution with a specified number of degrees of freedom under H0
# larger values of T support Ha:mu_x>mu_y
# if we assume that the variances are equal, then T has n_x+n_y−2 degrees of freedom, and the standard error is given by (8.5)
# if we assume that the variances are not equal, then T has degrees of freedom given by the Welch approximation and standard error given by (8.6)
# in each case, the function t.test() will perform the significance test
# it is used with the arguments t.test(x, y, alt="two.sided", var.equal=FALSE)
# the data is specified in two data vectors, x and y
# there is no need to specify the null hypothesis, as it is always the same
# under H0, the expected value of the difference is 0
# the alternative is specified by "less", "greater", or "two.sided" (the default)
# the argument var.equal=TRUE is given to specify the equal variance case
# the default is to assume unequal variances

# example
# the hypothesis test is between H0 : mu_x = mu_y  , Ha : mu_x != mu_y

x <- c(284,279,289,292,287,295,285,279,306,298)
y <- c(298,307,297,279,291,335,299,300,306,291)
plot(density(x))
lines(density(y), lty = 2)                       # normally distributed populations with similar spreads ; t-test look appropriate
t.test(x, y, var.equal = TRUE)

# the p-value is 0.05696 for the two-sided test
# this suggests a difference in the mean values, but it is not statistically significant at the 0.05 significance level
# a look at the reported confidence interval for the difference of the means shows a wide range of possible value for mu_x - mu_y 
# we conclude that this data is consistent with the assumption of no mean difference

# how would this change if we did not assume equal variances?

t.test(x,y)                  # default : two-sided and var.equal=FALSE

# the same observed value of the test statistic (marked t) is found as in the equal-variance case, 
# as (8.5) and (8.6) yield identical standard errors when the two sample sizes are the same
# we get a larger p-value, though, as the degrees of freedom decrease

# 8.6.2 Matched samples

# if the two sample X1, X2,..., Xn and Y1, Y2,..., Yn are matched so that the differences Xi - Yi are an i.i.d. sample, 
# then the significance test of hypotheses H0:mu_x=mu_y, Ha:mu_x<mu_y, mu_x>mu_y, or mu_x!=mu_y
# becomes a significance test of H0:mu=0, Ha:mu<0, mu>0, or mu!=0
# if the differences have a normally distributed population, a t-test can be used 
# if the differences are from a symmetric distribution, the Wilcoxon signed-rank test can be used
# otherwise, the sign test can be used, where mu is interpreted as the difference of medians
# in R, both the t.test() and wilcox.test() functions have an argument paired=TRUE that will perform the paired tests

# example
Finasteride <- c(5,3,5,6,4,4,7,4,3)
placebo     <- c(2,3,2,4,2,2,3,4,2)

t.test(Finasteride, placebo, paired=TRUE, alt = "two.sided")

# we see a very small p-value, indicating that the result is significant. The null hypothesis of no effect is in doubt

# example
# first, assume that the scores are randomly selected from the two tests. Next, assume that they are pairs of scores for ten students

pre  <- c(77, 56, 64, 60, 57, 53, 72, 62, 65, 66)
post <- c(88, 74, 83, 68, 58, 50, 67, 64, 74, 60)
boxplot(pre, post)
t.test(pre, post, var.equal = TRUE, alt = "less")                    # the p-value is small but not significant

t.test(pre, post, paired = TRUE, alt = "less")                       # this time, the difference is significant at the 0.05 level
# if small samples are to be used, it can often be advantageous to use paired samples, rather than independent samples

# 8.6.3 The Wilcoxon rank-sum test for equality of center

# the two-sample t-test tests whether two independent samples have the same center when both samples are drawn from a normal distribution 
# however, there are many situations in which the parent populations may be heavily skewed or have heavy tails. Then the t-test is not appropriate 
# however, if it is assumed that our two samples come from two distributions that are identical up a shift of center, 
# then the Wilcoxon rank-sum test can be used to perform a significance test to test whether the centers are identical
# H0:mu_x=mu_y, Ha:mu_x<mu_y, mu_x>mu_y, or mu_x!=mu_y
# wilcox.test(x, y, alt = "two.sided")

# example
A <- c(5.8, 1.0, 1.1, 2.1, 2.5, 1.1, 1.0, 1.2, 3.2, 2.7)
B <- c(1.5, 2.7, 6.6, 4.6, 1.1, 1.2, 5.7, 3.2, 1.2, 1.3)
plot(density(A))
lines(density(B))
# the populations are skewed with long tails. As such, the t-test assumptions are not met 
# however, we also see that the samples appear to have densities with the same shape, so the rank-sum test is available
wilcox.test(A, B)            # the p-value is not significant

# Problems 8.6.4

# Question 8.28
# the hypothesis test is between H0 : mu_x = mu_y  , Ha : mu_x < mu_y
Xbar = 79 ; Ybar = 110
sx = 25 ; sy = 20
nx = ny = 250
# we assume that the population variances are equal
sp = sqrt(((nx-1)*sx^2 + (ny-1)*sy^2 )/(nx+ny-2))
SE <- sp * sqrt(1/nx + 1/ny)
T <- (Xbar-Ybar)/SE                              # T = -15.30981 ; negative 
2 * pt(T, df = nx+ny-2)                          # two-sided ; p-value = 1.224e-43  
# the samples are extremely significant to conclude that the differences can't be explained by chance, mu_x is almost surely lower than mu_y

# Question 8.29
# the hypothesis test is between H0 : mu_x = mu_y  , Ha : mu_x < mu_y
Xbar = 5.3 ; Ybar = 5.4
sx = 2.5 ; sy = 2.5
nx = 200 ; ny = 207
# we assume that the population variances are equal
sp = sqrt(((nx-1)*sx^2 + (ny-1)*sy^2 )/(nx+ny-2))
SE <- sp * sqrt(1/nx + 1/ny)
T <- (Xbar-Ybar)/SE                              # T = -0.4034251 ; negative 
2 * pt(T, df = nx+ny-2)                          # two-sided ; p-value = 0.6868484 ; not significant 
# it is NOT a statistical evidence that children in the echinacea group had a quicker recovery

# Question 8.30
# the hypothesis test is between H0 : mu_x = mu_y  , Ha : mu_x < mu_y
attach(babies)
plot(density(age))
lines(density(dage), lty = 2)                    # normally distributed populations ; t-test look appropriate
t.test(age, dage, var.equal=FALSE, alt="less")   # p-value < 2.2e-16 ; extremely significant ; dads are almost surely older
detach(babies)

# Question 8.31
# the hypothesis test is between H0 : mu_x = mu_y  , Ha : mu_x != mu_y
males   <- normtemp$gender == 1
females <- normtemp$gender == 2
x <- normtemp$temperature[males]
y <- normtemp$temperature[females]
plot(density(x))
lines(density(y), lty = 2)                       # normally distributed populations ; t-test look appropriate
t.test(x, y, alt = "two.sided")                  # p-value = 0.02394 ; the difference between the populations' means is statistically significant

# Question 8.32
# the hypothesis test is between H0 : mu_x = mu_y  , Ha : mu_x != mu_y
pre  <- c(17,12,20,12,20,21,23,10,15,17,18,18)
post <- c(19,25,18,18,26,19,27,14,20,22,16,18)
plot(density(pre))
lines(density(post), lty = 2)                    # close to normally distributed populations ; t-test look appropriate

boxplot(pre,post)
t.test(pre, post, var.equal = TRUE, alt = "two.sided")
# p-value = 0.06 ; not significant ; the data supports the null hypothesis of "no improvement" in the test results for the unpaired scenario

t.test(pre, post, paired = TRUE, alt = "two.sided")
# p-value = 0.02638 ; significant ; it supports the alternative hypothesis of "change(improvement)" in the test results for the paired scenario 

# choose the paired scenario as the samples are dependent

# Question 8.33
# the hypothesis test is between H0 : mu_x = mu_y  , Ha : mu_x != mu_y
method1 <- c(45.9,57.6,54.9,38.7,35.7,39.2,45.9,43.2,45.4,54.8)
method2 <- c(48.2,64.2,56.8,47.2,43.7,45.7,53.0,52.0,45.1,57.5)
plot(density(method1))
lines(density(method2), lty = 2)                    # normally distributed populations ; t-test look appropriate

boxplot(method1, method2)
t.test(method1, method2, var.equal = TRUE, alt = "two.sided")
# p-value = 0.1163 ; not significant ; no difference in the means

t.test(method1, method2, paired = TRUE, alt = "two.sided")
# p-value = 0.0006649 ; extremely significant ; there is a difference in the means of collected samples

# choose the equal variances as the methods used are independent from each other

# Question 8.34
# the hypothesis test is between H0 : mu_x = mu_y  , Ha : mu_x != mu_y
library(MASS)
plot(density(shoes$A))
lines(density(shoes$B))                          # normally distributed populations ; t-test look appropriate

t.test(shoes$A, shoes$B, paired = TRUE, alt = "two.sided")
# p-value = 0.008539 ; highly significant

# Question 8.35
# the hypothesis test is between H0 : mu_x = mu_y  , Ha : mu_x != mu_y

# the data is not independent among the samples as there are only 205 parents and 930 children represented

t.test(galton$child, galton$parent, paired = TRUE, alt = "two.sided")
# p-value = 0.004082 ; indicates a difference in the means

# Question 8.36
x <- c(284,279,289,292,287,295,285,279,306,298)
y <- c(298,307,297,279,291,335,299,300,306,291)
var.test(x, y)

