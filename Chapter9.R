# This document belongs to the book "Using R for Introductory Statistics" by John Verzani.
# copyright (c) 2004 - Taylor and Francis
#
# In the document, foundations of statistical knowledge is practiced on R.
#
# written by Erkam Minsin
# find me on LinkedIn: https://www.linkedin.com/in/erkam-minsin-37537514a/
#
# last modified: Dec 2022
# first written: Dec 2022

setwd("C:/Users/erkam/R Learning/Using_R_for_Introductory_Statistics_John_Verzani")

## install.packages("UsingR")
library(UsingR)

## update.packages()


# Chapter 9 Goodness of Fit ---------------------------------------------


# in this chapter we return to problems involving categorical data
# we previously summarized such data using tables
# here we discuss a significance test for the distribution of the values in a table
# the test statistic will be based on how well the actual counts for each category fit the expected counts
# such tests are called goodness-of-fit tests, as they measure how well the distribution of the data fits a probability model
# in this chapter we will also discuss goodness-of-fit tests for continuous data
# for example, we will learn a significance test for investigating whether a data set is normally distributed

# 9.1 The chi-squared goodness-of-fit test

# if 100 people are surveyed, and the results are 35 for the Republican, 40 for the Democrat, and 25 undecided, 
# is the difference between the Republican and Democratic candidate significant?

# 9.1.1 The multinomial distribution

# before answering a question about significance, we need a probability model, so that calculations can be made
# when there are just two categories to choose from we use the binomial model as our probability model; 
# in this case, with more categories, we generalize and use the multinomial model

# example
cols        <- c("blue","brown","green","orange","red","yellow","purple")
prob        <- c(1,1,1,1,2,2,2)                                                          # ratio of colors
bagfull.mms <- sample(cols, 30, replace = TRUE, prob = prob)
table(bagfull.mms)

# suppose we expected the percentages to be 35% Republican, 35% Democrat, and 30% undecided 
# what is the probability in a survey of 100 that we see 35, 40, and 25 respectively?

choose(100,35)*choose(65,40)*choose(25,25)*(0.35^35)*(0.35^40)*(0.30^25)

# this small value is the probability of the observed value, but it is not a p-value 
# a p-value also includes the probability of seeing more extreme values than the observed one 
# we still need to specify what that means

# 9.1.2 Pearson's chi-squared statistic

# let Y1, Y2,..., Yk be the observed cell counts in a table that arise from random sampling
# suppose their joint distribution is described by the multinomial model with probabilities p1, p2,..., pk
# a significance test of H0:p1=(pi)1,..., pk=(pi)k, Ha:pi!=(pi)i for at least i can be performed with the chi-squared statistic
# the (pi)i are specified probabilities
# under H0 the sampling distribution is asymptotically the chi-squared distribution with  degrees of freedom
# this is a good approximation, provided that the expected cell counts are all five or more
# large values of the statistic support the alternative
# this test is implemented by the chisq.test(x, p=...)
# the data is given in tabulated form in x
# the null hypothesis is specified with the argument p= as a vector of probabilities
# this should be given as a named argument (p=argument), as it is not the second position in the list of arguments
# the alternative hypothesis is not specified, as it does not change
# a warning message will be returned if any category has fewer than five expected counts

# example

# suppose we wanted to know whether the voter data was generated according to the probabilities p1=.35, p2=.35, and p3=.30

#by hand
y    <- c(35,40,25)
p    <- c(35,35,30)                              # ratios
p    <- p/sum(p)                                 # proportions
n    <- sum(y)
chi2 <- sum((y-n*p)^2/(n*p))
chi2                                             # 1.547619
pchisq(chi2, df = 3-1, lower.tail = FALSE)       # 0.4612526

# by chisq.test(x, p=...)
chisq.test(y, p = p)                             # X-squared = 1.5476, df = 2, p-value = 0.4613 ; not significant

# example

# we investigate whether the sample proportions agree with the probabilities: p1=.15, p2=.05, p3=.05, p4=.05, p5=.10, p6=.20, p7=.40

attach(samhda)
y <- table(amt.smoke[amt.smoke<98])
p <- c(.15,.05,.05,.05,.10,.20,.40)
chisq.test(y, p = p)                             # X-squared = 7.9382, df = 6, p-value = 0.2427 ; not significant 
detach(samhda)

# Partially specified null hypotheses

# in the example with voting data, we might be interested in knowing :
# whether the Republican candidate is significantly trailing the Democrat or 
# whether the differences are due to sampling variation
# that is, we would want to test the hypotheses H0:p1=p2 , Ha:p1!=p2
# these, too, can be tested with the chi-squared statistic, but we need to specify what we mean by "expected" as under H0 this is not fully specified
# to do so, we use any values completely specified by the null hypothesis; 
# for those values that aren't, we estimate using the null hypothesis to pool our data as appropriate
# for this problem, none of the pi values are fully specified
# to estimate we use phat1 = phat2
# we use both of the cell counts through (Y1+Y2)/(2n)
# this leaves phat3 = 1-phat1-phat2 = Y3/n

# example
y    <- c(35,40,25)
phat <- c(75/200,75/200,25/100)
n    <- sum(y)
chi2 <- sum((y-n*phat)^2/(n*phat))               # 0.3333333
pchisq(chi2, df = 1, lower.tail = FALSE)         # 0.5637029 ; the difference is not statistically significant

# in general, the chi-squared statistic can be used in significance tests where the null specifies some relationship among the multinomial probabilities
# the asymptotic distribution of the test statistic under the null hypothesis will be chi-squared
# the degrees of freedom will depend on the number of values that we are free to specify

# Problems 9.1.3

# Question 9.1
# the significance test is between H0:pi=1/6 for each i , Ha:pi!=1/6 for at least one i
observed <- c(13,17,9,17,18,26)
p        <- c(1/6,1/6,1/6,1/6,1/6,1/6)
n        <- sum(observed)
expected <- n*p
chi2     <- sum((observed-expected)^2/(expected))                    # 9.68
pchisq(chi2, df = 5, lower.tail = FALSE)                             # p-value = 0.08482699 ; not significant ; the die could be fair enough

# Question 9.2
# the significance test is between H0:p1=48.6,p2=31.5,p3=12.5,p4=2.8,p5=0.6,p6=4.0 , Ha:pi!=pi(i) for at least one i
observed <- c(315,197,141,39,16,79)
p        <- c(0.486,0.315,0.125,0.028,0.006,0.04)
n        <- sum(observed)
expected <- n*p
chi2     <- sum((observed-expected)^2/(expected))    # 152.5565
pchisq(chi2, df = 5, lower.tail = FALSE)         # p-value=3.812955e-31 ; extremely significant ; the sample data isn't consistent with the actual results

# Question 9.3
p_milkchocolate <- c(0.10,0.30,0.10,0.10,0.20,0.20)
p_Peanut        <- c(0.20,0.20,0.10,0.10,0.20,0.20)
observed        <- c(15,34,7,19,29,24)

chisq.test(x = observed, p = p_milkchocolate)    # X-squared = 7.0651, df = 5, p-value = 0.2158 ; the difference is not significant
chisq.test(x = observed, p = p_Peanut)           # X-squared = 13.328, df = 5, p-value = 0.02049 ; the difference is significant

# based on the p-values, the true source of the candy sample is more likely the milk chocolate package

# better solution
prop           <- function(x) x/sum(x)
bagfull        <- c(15,34,7,19,29,24)
names(bagfull) <- c("blue","brown","green","orange","red","yellow")

chisq.test(bagfull,p = prop(mandms["milk chocolate",]))
chisq.test(bagfull,p = prop(mandms["Peanut",]))

# Question 9.4
p        <- rep(1/10, 10)
observed <- table(pi2000)
chisq.test(x = observed, p = p)                  # X-squared = 4.42, df = 9, p-value = 0.8817 ; the difference is not significant

# the digits appear with equal probabilities

# Question 9.5
prop     <- function(x) x/sum(x)
p        <- prop(c(9,12,9,8,4))
observed <- c(28,39,23,22,11)
chisq.test(x = observed, p = p)                  # X-squared = 1.084, df = 4, p-value = 0.8968 ; the difference is not significant

# the vowels appears to be from English

# Question 9.6
all.names   <- paste(bright.stars$name, sep="", collapse="")
x           <- unlist(strsplit(tolower(all.names), ""))
letter.dist <- sapply(letters, function(i) sum(x == i))
p           <- scrabble$frequency[1:26]
p           <- p / sum(p)

chisq.test(x = letter.dist, p = p)               # X-squared = 260.73, df = 25, p-value < 2.2e-16 ; the difference is extremely significant

# the names are not English words

# Question 9.7
p <- rep(1/7, 7)
observed <- c(53,42,51,45,36,37,65)
chisq.test(x = observed, p = p)                  # X-squared = 13.319, df = 6, p-value = 0.03824 ; the difference is significant

# the murder is not equally likely to occur on any given day

# the second part, we must do by hand 
# first note that if we specify pw, the weekend probability, then pd, the weekday probability, is (1-2pw)/5 
# there is only 1 degree of freedom
# we estimate pw with the average of the weekend counts:
n            <- sum(observed)
phatw        <- (53 + 65)/(2*n)
phatd        <- (1 - 2*phatw)/5
expected     <- n*c(phatw, rep(phatd,5), phatw)
chi2         <- sum((observed - expected)^2/expected)

1-pchisq(chi2,df=1)                              # X-squared = 4.793799, df = 1, p-value = 0.02856237 ; the difference is significant

# murders happen on each weekday do not have an equal probability

# Question 9.8
observed        <- c(41,48,105,58)
n               <- sum(observed)
phatbrownorange <- (41+48)/(2*n)
phatothers      <- (1-2*phatbrownorange)/2 
p               <- c(rep(phatbrownorange,2),rep(phatothers,2))
expected        <- n*p
chi2            <- sum((observed-expected)^2/expected)               # 14.10271
p.value         <- 1-pchisq(chi2, df = 1)                            # 0.0001730943 ; the difference is extremely significant 

# the probabilities of orange and brown are not equal

# Question 9.9
n = 20 ; m = 250 ; k = 3
f   <- factor(letters[1:k])
p   <- c(3,4,5)  
p   <- p/sum(p)
res <- c()
for(i in 1:m) {
  x <- sample(f, n, replace = TRUE, prob = p)
  y <- table(x)
  res[i] <- sum((y-n*p)^2/(n*p))
}
hist(res, prob = TRUE, col = gray(.8), ylim = c(0,0.5))              # extend y-limit
curve(dchisq(x, df = k-1), add = TRUE)

n = 5 ; m = 250 ; k = 3
f   <- factor(letters[1:k])
p   <- c(3,4,5)  
p   <- p/sum(p)
res <- c()
for(i in 1:m) {
  x <- sample(f, n, replace = TRUE, prob = p)
  y <- table(x)
  res[i] <- sum((y-n*p)^2/(n*p))
}
hist(res, prob = TRUE, col = gray(.8), ylim = c(0,0.5))
curve(dchisq(x, df = k-1), add = TRUE)

n = 20 ; m = 250 ; k = 3
f   <- factor(letters[1:k])
p   <- c(1,19,20)  
p   <- p/sum(p)
res <- c()
for(i in 1:m) {
  x <- sample(f, n, replace = TRUE, prob = p)
  y <- table(x)
  res[i] <- sum((y-n*p)^2/(n*p))
}
hist(res, prob = TRUE, col = gray(.8), ylim = c(0,0.5))
curve(dchisq(x, df = k-1), add = TRUE)

# Question 9.10

# 9.2 The chi-squared test of independence

# in a two-way contingency table we are interested in the relationship between the variables 
# in particular, we ask whether the levels of one variable affect the distribution of the other variable
# to approach this question with a significance test, we need to state the null and alternative hypotheses, a test statistic, and a probability model
# let n_r be the number of rows in the table (the number of levels of the row variable), n_c be the number of columns, 
# and Yij be a random variable recording the frequency of the (i, j) cell 
# let pij be the cell probability for the ith row and jth column
# the marginal probabilities are denoted (p_i)^r and (p_j)^c where, for example, (p_i)^r =(p_i)1 + (p_i)2 + ... + (p_i)n_j
# our null hypothesis is that the column variable should be independent of the row variable 
# when stated in terms of the cell probabilities, pij, this says that pij = (p_i)^r * (p_j)^c 
# this is consistent with the notion that independence means multiply

# let Yij, i=1, ..., nr, j=1, ..., nc be the cell frequencies in a two-way contingency table for which the multinomial model applies 
# a significance test of H0:the two variables are independent Ha:the two variables aren't independent can be performed using the chi-squared test statistic
# Under the H0, this statistic has sampling distribution that is approximated by the chi-squared distribution with (nr-1)(nc-1) degrees of freedom
# The p-value is computed using P(chi2 >= observed value|H0)

# in R this test is performed by the chisq.test() function
# if the data is summarized in a table or a matrix in the variable x the usage is chisq.test(x)
# if the data is unsummarized and is stored in two variables x and y where the ith entries match up, then the function can be used as chisq.test(x,y)
# alternatively, the data could be summarized first using table(), as in chisq.test(table(x,y))
# for each usage, the null and alternative hypotheses are not specified, as they are the same each time the test is used
# the argument simulate.p.value=TRUE will return a p-value estimated using a Monte Carlo simulation
# this is used if the expected counts in some cells are too small to use the chi-squared distribution to approximate the sampling distribution of chi2

# example
seatbelt <- rbind(c(56,8),c(2,16))
seatbelt
chisq.test(seatbelt)         # X-squared = 35.995, df = 1, p-value = 1.978e-09
# the small p-value is consistent with our observation that the two variables are not independent

# example
tbl <- xtabs( ~ gender + amt.smoke,                                  # no left side in formula
             subset = amt.smoke < 98 & gender !=7, data=samhda)
tbl
chisq.test(tbl)              # X-squared = 4.1468, df = 6, p-value = 0.6568

# the xtabs() function allows us to use the convenient subset= argument to eliminate the data for which the values are not applicable
# the significance test shows no reason to doubt the hypothesis that the two variables are independent

# the warning message is due to some expected counts being small. Could this significantly change the p-value reported?

chisq.test(tbl, simulate.p.value = TRUE)         # X-squared = 4.1468, df = NA, p-value = 0.6792 ; the p-value is not changed significantly

# 9.2.1 The chi-squared test of homogeneity

# how can we assess the effectiveness of a drug treatment? 
# typically, there is a clinical trial, with each participant randomly allocated to either a treatment group or a placebo group
# if the results are measured numerically, a t-test may be appropriate to investigate whether any differences in means are significant
# when the results are categorical, we see next how to use the chi-squared statistic to test whether the distributions of the results are the same

# example
#         much worse worse same muchimproved verymuchimproved
# Celexa      0        2    3         5             2
# placebo     0        2    8         2             0

# this as a significance test using hypotheses : H0: the two distributions are the same , Ha: the two distributions are different
# we use the chi-squared statistic. Again we need to determine the expected amounts, as they are not fully specified by H0

# the expected amounts are calculated as the chi-squared test of independence

# as the test statistic and its sampling distribution under H0 are the same as with the test of independence, 
# the chi-squared significance tests of homogeneity and independence are identical in implementation despite the differences in the hypotheses

# before proceeding, we combine the data so that there are three outcomes
celexa      <- c(2,3,7)
placebo     <- c(2,8,2)
x           <- rbind(celexa, placebo)
colnames(x) <- c("worse","same","better")
x
chisq.test(x)                # X-squared = 5.0505, df = 2, p-value = 0.08004

# Warning message: In chisq.test(x) : Chi-squared approximation may be incorrect
chisq.test(x, simulate.p.value = TRUE)           # X-squared = 5.0505, df = NA, p-value = 0.1064

# Problems 9.2.2

# Question 9.11
accidents           <- cbind(none = c(67,42,75,56,57), minor = c(10,6,8,4,15), major = c(5,5,4,6,1))
rownames(accidents) <- c("<18","18-25","26-40","40-65","over 65")
accidents
chisq.test(accidents)                            # X-squared = 12.586, df = 8, p-value = 0.1269 ; warning message, thus
chisq.test(accidents, simulate.p.value = TRUE)   # X-squared = 12.586, df = NA, p-value = 0.1324

# Question 9.12
injury           <- cbind(none = c(12813,65963), minimal = c(647,4000), minor = c(359,2642), major = c(42,303))
rownames(injury) <- c("yes","no")
injury
chisq.test(injury)           # X-squared = 59.224, df = 3, p-value = 8.61e-13 ; the variables seat belt and injury level are dependent on each other

# Question 9.13
aq <- airquality[complete.cases(airquality),]    # to remove missing data from the data set (removes the rows with any NA info)
attach(aq)
te <- cut(Temp, quantile(Temp))
oz <- cut(Ozone, quantile(Ozone))
table(te,oz)
chisq.test(table(te,oz))     # or chisq.test(te,oz) : X-squared = 76.309, df = 9, p-value = 8.713e-13 ; there is some dependency between the variables

# Question 9.14
retention           <- cbind("1year"=c(18,10),"2year"=c(15,5),"3year"=c(5,7),"4year"=c(8,18),"5+year"=c(4,10))
rownames(retention) <- c("nonblock","block")
retention
chisq.test(retention)        # X-squared = 14.037, df = 4, p-value = 0.007179 ; highly significant; the distributions are not the same (homogeneous)

# Question 9.15
chisq.test(oral.lesion)                                              # X-squared = 22.099, df = 16, p-value = 0.14 ; variables are independent
chisq.test(oral.lesion, simulate.p.value = TRUE)                     # X-squared = 22.099, df = NA, p-value = 0.02649 ; variables are dependent

# 9.3 Goodness-of-fit tests for continuous distributions

# when finding confidence intervals for a sample we were concerned about whether or not the data was sampled from a normal distribution
# to investigate, we made a quantile plot or histogram and eyeballed the result
# in this section, we see how to compare a continuous distribution with a theoretical one using a significance test

# the first method : 

# the chi-squared test is used for categorical data. We can try to make it work for continuous data by "binning" 
# that is, as in a construction of a histogram, we can choose some bins and count the number of data points in each 
# now the data can be thought of as categorical and the test can be used for goodness of fit
# this is fine in theory but works poorly in practice

# the second method :

# 9.3.1 Kolmogorov-Smirnov test

# the Kolmogorov-Smirnov test is a better alternative in the continuous distribution case
# assume X1, X2,..., Xn is an i.i.d. sample from a continuous distribution with c.d.f. F(x)
# let Fn(x) be the empirical c.d.f 
# a significance test of H0: F(x)=F0(x), Ha:F(x)!=F0(x) can be constructed with test statistic D 
# large values of D support the alternative hypothesis
# in R, this test is implemented in the function ks.test(). Its usage follows this pattern: ks.test(x, y="name",...)
# the variable x stores the data
# the argument y= is used to set the family name of the distribution in H0
# it has a character value of "name" containing the "p" function that returns the c.d.f. for the family (e.g., "pnorm" or "pt")
# the ... argument allows the specification of the assumed parameter values
# these depend on the family name and are specified as named arguments, as in mean= 1, sd= 1
# the parameter values should not be estimated from the data, as this affects the sampling distribution of D
# If we have two i.i.d. independent samples X1,..., Xn and Y1,..., Ym, from two continuous distributions FX and FY, 
# then a significance test of H0:FX=FY, Ha:FX!=FY can be constructed with a similar test statistic : D=maximum of x |FXn(x)-FYm(x)|
# in this case, the ks.test() can be used as ks.test(x,y) ks.test(x,y)

x <- rnorm(100, mean = 5, sd = 2)
ks.test(x, "pnorm", mean = 0, sd = 2)            # "wrong" parameters ; D = 0.79662, p-value < 2.2e-16 ; reject H0: no fit

ks.test(x, "pnorm", mean = 5, sd = 2)            # correct population parameters ; D = 0.084201, p-value = 0.4775 ; fail to reject H0 : fit 

x <- runif(100, min = 0, max = 5)
ks.test(x, "punif", min = 0, max =6)             # "wrong" parameters ; D = 0.17584, p-value = 0.004124 ; reject H0 ; no fit

ks.test(x, "punif", min = 0, max = 5)            # correct population parameters ; D = 0.070494, p-value = 0.703 ; fail to reject H0 : fit

# the p-values are significant only when the parameters do NOT match the known population ones

# example
attach(stud.recs)
ks.test(sat.m, sat.v)        # D = 0.2125, p-value = 0.001456 ; highly significant ; their population parameters are different from each other
detach(stud.recs)

# the third method :

# 9.3.2 The Shapiro-Wilk test for normality

# the Kolmogorov-Smirnov test for a univariate data set works when the distribution in the null hypothesis is fully specified prior to looking at the data
# in particular, any assumptions on the values for the parameters should not depend on the data, as this can change the sampling distribution

res.1 <- c()
res.2 <- c()
for(i in 1:500) {
  x <- rnorm(25)
  res.1[i] <- ks.test(x, pnorm)$statistic
  res.2[i] <- ks.test(x, pnorm, mean(x), sd(x))$statistic
}
plot(density(res.1), main = "K-S sampling distribution")
lines(density(res.2), lty = 2)                                       # sampling distribution has changed (same data, two different graphics)

# a consequence is that we can’t use the KS test to test for normality of a data set unless we know the parameters of the underlying distribution
# The Shapiro-Wilk test allows us to do this

# If X1, X2,..., Xn is an i.i.d. sample from a continuous distribution, 
# a significance test of H0:parent distribution is normal , Ha:the parent distribution isn't normal can be carried out with the Shapiro-Wilk test statistic
# in R, the function shapiro. test() will perform the test. The usage is simply shapiro.test(x) where the data vector x contains the sample data

# example
attach(stud.recs)
shapiro.test(sat.m)          # W = 0.98983, p-value = 0.3055

shapiro.test(sat.v)          # W = 0.99397, p-value = 0.752
detach(stud.recs)
# in each case, the p-value is not significant ; fail to reject H0 ; there is no evidence that the data is not normally distributed

# example
shapiro.test(OBP)            # W = 0.97092, p-value = 1.206e-07 ; significant ; no narmality (perhaps this is due to the one outlier)
shapiro.test(OBP[OBP<0.5])   # W = 0.99049, p-value = 0.006404 ; significant ; still no normality ; note the dramatic diff in the p-value w/o the outlier

# 9.3.3 Finding parameter values using fitdistr()

# if we know a data set comes from a known distribution and would like to estimate the parameter values, 
# we can use the convenient fitdistr() function from the MASS library.
# this function estimates the parameters for a wide family of distributions 
# the function is called with these arguments: fitdistr(x, densfun=family.name, start=list(…))
# we specify the data as a data vector, x; 
# the family is specified by its full name, unlike that used in ks.test(); 
# and, for many of the distributions, reasonable starting values are specified using a named list

# example
fitdistr(babyboom$wt, "normal")

#     mean          sd             
#  3275.95455    521.99760   # estimates for the population mean and standard deviation
#  (78.69410)    (55.64513)  # standard errors ; these can be used to give confidence intervals for the estimates

# Problems 9.3.4

# Question 9.16
CEOpay <- rbind(c(110,12,2.5,98,1017,540,54,4.3,250,432,0),c(312,316,175,200,92,201,428,51,289,1126,822))
rownames(CEOpay) <- c("2001","2002")
CEOpay
chisq.test(CEOpay["2001",], CEOpay["2002",], simulate.p.value = TRUE)

# Question 9.17
shapiro.test(babies$ht[babies$ht < 99])$p.value  # 4.893686e-10 ; extremely significant ; no normality
shapiro.test(babies$wt[babies$wt<999])$p.value   # 0.001191711  ; highly significant ; no normality

# Question 9.18
shapiro.test(brightness)$p.value                 # 1.825299e-09 ; extremely significant ; the data is not normal
hist(brightness, prob = TRUE)
lines(density(brightness))                       
curve(dnorm(x, mean(brightness), sd(brightness)), add  = TRUE)
# the data seems normal with eyeball estimate

# Question 9.19
shapiro.test(normtemp$temperature)$p.value       # 0.2331861 ; not significant ; normal

# Question 9.20
fitdistr(rivers, "gamma")
#      shape         rate    
#  2.578975570   0.004363394 
# (0.268578387) (0.000488203)

plot(density(rivers))
x = 0:4000
lines(x, dgamma(x, shape = 2.578975570, rate = 0.004363394), lty=2)

# the gamma shape matches the data, but the tail behavior does not seem that similar. A quantile-quantile plot will show this:
qqplot(rivers, rgamma(100,shape = 2.578975570, rate = 0.004363394))

# Question 9.21
fitdistr(stud.recs$sat.m , "normal")
#    mean          sd    
# 485.937500    68.913867 
# (5.448120)    (3.852402)
 
fitdistr(stud.recs$sat.v , "normal")
#     mean          sd    
#  455.843750    89.056200 
#  (7.040511)   (4.978393)

# Question 9.22
res <- sapply(1:100, function(x) ks.test(rt(25, df = 3), "pnorm")$p.value)
sum(res<0.05)/length(res)*100

res <- sapply(1:100, function(x) ks.test(rexp((25)-1), "pnorm")$p.value)
sum(res<0.05)/length(res)*100

# Kolmogorov-Smirnov test works better in rejecting the H0 when it is false for skewed data

# Question 9.23
qqplot(pnorm(rnorm(100)), runif(100))
qqplot(pt(rt(100, df = 5), df = 5), runif(100))
qqplot(pexp(rexp(100, rate = 5), rate = 5), runif(100))
qqplot(plnorm(rlnorm(100)), runif(100))

# Question 9.24
shapiro.test(c(rnorm(100),5))$p.value            # 0.003914 ; no normality ; the least resistant to an outlier
shapiro.test(c(rnorm(1000),5))$p.value           # 0.05522 ; normal
shapiro.test(c(rnorm(4000),5))$p.value           # 0.08312 ; normal ; the most resistant to an outlier

