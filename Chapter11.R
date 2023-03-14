# This document belongs to the book "Using R for Introductory Statistics" by John Verzani.
# copyright (c) 2004 - Taylor and Francis
#
# In the document, foundations of statistical knowledge is practiced on R.
#
# written by Erkam Minsin
# 
# last modified: Jan 2023
# first written: Jan 2023

setwd("C:/Users/erkam/R Learning/Using_R_for_Introductory_Statistics_John_Verzani")

## install.packages("UsingR")
library(UsingR)

## update.packages()


# Chapter 11 Analysis of Variance ---------------------------------------------


# Analysis of variance, ANOVA, is a method of comparing means based on variations from the mean

# 11.1 One-way ANOVA

# A one-way analysis of variance is a generalization of the t-test for two independent samples, allowing us to compare means for several independent samples
# Suppose we have k populations of interest. From each we take a random sample 
# These samples are independent if the knowledge of one sample does not effect the distribution of another
# Notationally, for the ith sample, let X_i1, X_i2, ..., X_ini designate the sample values
# The one-way analysis of variance applies to normally distributed populations
# Suppose the mean of the ith population is mu_i and its standard deviation is sigma_i
# We use a sigma if these are all equivalent
# A statistical model for the data with common standard deviation is Xij = mu_i + epsilon_ij,
# where the error terms, epsilon_ij, are independent with Normal(0, sigma) distribution

# example : Number of calories consumed by month
# We assume that the amounts consumed are normally distributed with common variance but perhaps different means 
# there appears to be more clustering around the means for each month than around the grand mean or mean for all the data
# This would indicate that the means may be different

# The goal of one-way analysis of variance is :
# to decide whether the difference in the sample means is indicative of a difference in the population means of each sample  
# or is attributable to sampling variation 
# This problem is approached as a significance test

# Suppose we have k independent, i.i.d. samples from populations with Normal(mu_i, sigma) distributions, i=1, ...,k
# A significance test of H0 : mu_1 = mu_2 = ... = mu_k , Ha : mu_i != mu_j for at least one pair i and j, can be performed with
# test statistic F = SSTr/(k-1) / SSE/(n-k) , where
# SSE : the error sum of squares : The interior sum, sum((x_ij - xbar_i)^2) for each j , measures the variation within the ith group 
# The SSE is then a measure of the within-group variability
# SSTr : treatment sum of squares : compares the means for each group, xbar_i, with the grand mean, xbar ; sum((n_i*((xbar_i-xbar)^2))) for each i
# It measures the variability among the means of the samples
# SST = SSE + SSTr
# If the data came from a common mean, then we would expect SSE and SSTr to be roughly the same
# If SSE and SSTr are much different, it would be evidence against the null hypothesis
# Under H0, F has the F-distribution with k-1 and n-k degrees of freedom
# The p-value is calculated from P(F >= observed value | H0)
# The R function oneway.test() will perform this significance test

# example
may  <- c(2166, 1568, 2233, 1882, 2019)
sep  <- c(2279, 2075, 2131, 2009, 1793)
dec  <- c(2226, 2154, 2583, 2010, 2190)
xbar <- mean(c(may,sep,dec))
SSE  <- (5-1)*var(may) + (5-1)*var(sep) + (5-1)*var(dec)
SSE
SSTr <- 5*((mean(may)-xbar)^2 + (mean(sep)- xbar)^2 + (mean(dec)-xbar)^2)
SSTr
F.obs <- (SSTr/(3-1)) / (SSE/(15-3))
pf(F.obs, df1 = 3-1, df2 = 15-3, lower.tail = FALSE)
# We get a p-value that is not significant
# Despite the graphical evidence, the differences can be explained by sampling variation

# 11.1.1 Using R's model formulas to specify ANOVA models

# If x stores all the data and f is a factor indicating which group the data value belongs to, then x~f represents the statistical model X_ij=mu_i+epsilon_ij
# Remember the default behavior for plot() of the model formula x ~ f was to make a boxplot
# Strip charts are good for a small data set, but the boxplot is preferred when there are larger data sets

# 11.1.2 Using oneway.test() to perform ANOVA

# The function oneway.test() is used as oneway.test(x~f, data=..., var.equal=FALSE)
# the argument var.equal=  is set to TRUE if appropriate
# we need to put the data into the appropriate form : 
# a data vector containing the values and a factor indicating the sample the corresponding value is from
# This can be done using stack()

d <- stack(list(may=may, sep=sep, dec=dec))      # need names for list
names(d)                                         # stack() returns two variables : "values" and "ind"

oneway.test(values ~ ind, data = d, var.equal = TRUE)
# We get the same p-value as in our previous calculation, but with much less effort

# 11.1.3 Using aov() for ANOVA

# The alternative aov() function will also perform an analysis of variance
# It returns a model object similar to lm() but has different-looking outputs for the print() and summary() extractor functions
# Again, it is called with a model formula, but with no specification of equal variances :

res <- aov(values ~ ind, data = d)
res                                              # uses print()
# Terms:
#                       ind   Residuals
# Sum of Squares   174664.1    586719.6
# Deg. of Freedom         2          12
# Residual standard error: 221.1183 -------------> sqrt(586719.6/12) : SE

summary(res)
#             Df  Sum Sq  Mean Sq  F value  Pr(>F)                   # These are the values needed to perform the one-way test
# ind          2  174664    87332    1.786   0.209
# Residuals   12  586720    48893

# example
UBP <- c(168.2,161.4,163.2,166.7,173.0,173.3,160.1,161.2,166.8)
grip.type <- rep(c("classic", "integrated", "modern"), c(3,3,3))
grip.type <- factor(grip.type)
boxplot(UBP ~ grip.type, ylab = "Power (watts)", main = "Effect of cross country grip")
# The boxplot indicates that the "integrated" has a significant advantage
# But is this due to sampling error? We use aov() to carry out the analysis of variance
res <- aov(UBP ~ grip.type)
summary(res)
#             Df  Sum Sq  Mean Sq  F value  Pr(>F)  
# grip.type    2  116.68    58.34    4.463  0.0649 .
# Residuals    6   78.43    13.07                 

# We see that there is a small p-value that is significant at the 10% level
# (Although, in most cases, samples with only three observations will fail to pick up on actual differences)

# 11.1.4 The nonparametric Kruskal-Wallis test

# The Kruskal-Wallis test, a nonparametric test, is analogous to the rank-sum test for comparing the population means of k independent samples
# Assume k populations, the ith one with density f(x-mu_i)
# Let X_ij,i  =1,..., k,  j=1,..., ni denote k independent, i.i.d. random samples from these populations
# A significance test of H0 : mu_1=mu_2=...=mu_k, Ha : mu_i != mu for at least one pair i and j,
# can be performed with the test statistic T given by : T = (12/n*(n+1))*sum((ni*(rbari-rbar))^2), where
# rij is the respective rank of a data point when all the data is ranked from smallest to largest, 
# rbar_i is the mean of the ranks for each group, and rbar is the grand mean
# The asymptotic distribution of T under H0 is the chi-square distribution with k-1 degrees
# This is used as the approximate distribution for T when there are at least five observations in each category
# Large values of T support the alternative hypothesis
# The kruskal.test() function will perform the test. The syntax is kruskal.test(x ~ f, data=..., subset=...)

# example
x <- c(63, 64, 95, 64, 60, 85)
y <- c(58, 56, 51, 84, 77)
z <- c(85, 79, 59, 89, 80, 71, 43)
d <- stack(list("test1" = x, "test2" = y, "test3" = z))
plot(values ~ ind, data = d, xlab = "test", ylab = "grade")         
# The boxplots  show that the assumption of independent samples from a common population, which perhaps is shifted, is appropriate

kruskal.test(values ~ ind, data = d)
# Kruskal-Wallis chi-squared = 1.7753, df = 2, p-value = 0.4116
# This large p-value indicates no reason to doubt the null hypothesis of equally difficult exams

# Problems 11.1.5

# Question 11.1
# The data is stored so that the function oneway.test() is straightforward to use:
names(morley)
plot(Speed ~ factor(Expt), data = morley, xlab = "Expt", ylab = "Speed")
oneway.test(Speed ~ Expt, data = morley, var.equal = FALSE)
#  One-way analysis of means (not assuming equal variances)
# data:  Speed and Expt
# F = 3.0061, num df = 4.000, denom df = 47.044, p-value = 0.02738

# The small p-value is consistent with the impression left after considering side-by-side boxplots that the centers are not the same

# Question 11.2
names(Cars93)
plot(MPG.highway ~ factor(DriveTrain), data = Cars93, xlab = "DriveTrain", ylab = "MPG.highway")
oneway.test(MPG.highway ~ DriveTrain, data = Cars93, var.equal = FALSE)
# One-way analysis of means (not assuming equal variances)
# data:  MPG.highway and DriveTrain
# F = 10.725, num df = 2.000, denom df = 22.527, p-value = 0.0005342

# The small p-value is consistent with the alternative hypothesis that at least one population mean is different than the others

# Question 11.3
names(female.inc)
plot(income ~ race, data = female.inc, xlab = "race", ylab = "income")                   # like most income data, is heavily skewed
# Thus, an analysis of variance using oneway.test() would be inappropriate
# However, the three population distributions appear to be roughly the same shape. The Kruskal-Wallis test then is appropriate
kruskal.test(income ~ race, data = female.inc)
# Kruskal-Wallis rank sum test
# data:  income by race
# Kruskal-Wallis chi-squared = 11.794, df = 2, p-value = 0.002748

# The small p-value is not consistent with the assumption of equal mean wages for all three races represented in the data

# Question 11.4
names(carsafety)
boxplot(Driver.deaths ~ type, data = carsafety)
kruskal.test(Driver.deaths ~ type, data = carsafety)  
#  Kruskal-Wallis rank sum test
# data:  Driver.deaths by type
# Kruskal-Wallis chi-squared = 14.141, df = 6, p-value = 0.0281      
         
# Not normally distributed populations ; not equal variances ; there is a difference in population means

boxplot(Other.deaths ~ type, data = carsafety)
kruskal.test(Other.deaths ~ type, data = carsafety)
# Kruskal-Wallis rank sum test 
# data:  Other.deaths by type
# Kruskal-Wallis chi-squared = 15.966, df = 6, p-value = 0.01393

# there is a difference in population means 

# Question 11.5
str(hall.fame)
boxplot(BA ~ Hall.Fame.Membership, data = hall.fame)                 # could be normally distributed with equal variances

oneway.test(BA ~ Hall.Fame.Membership, data = hall.fame, var.equal = TRUE)
# One-way analysis of means
# data:  BA and Hall.Fame.Membership
# F = 164.19, num df = 2, denom df = 1337, p-value < 2.2e-16         # extremely significant ; at least one population has a different mean

# Question 11.6
lab1 = c(4.13, 4.07, 4.04, 4.07, 4.05)
lab2 = c(3.86, 3.85, 4.08, 4.11, 4.08)
lab3 = c(4.00, 4.02, 4.01, 4.01, 4.04)
lab4 = c(3.88, 3.89, 3.91, 3.96, 3.92)
d <- stack(list("lab1" = lab1, "lab2" = lab2, "lab3" = lab3, "lab4" = lab4))
plot(values ~ ind, data = d, xlab = "Laboratory", ylab = "Measurement")                  # skewed ; (different variances)

kruskal.test(values ~ ind, data = d)
# Kruskal-Wallis rank sum test
# data:  values by ind
# Kruskal-Wallis chi-squared = 8.0557, df = 3, p-value = 0.04488     # significant ; there is a difference in population means

# Question 11.7
Type1 <- c(303, 293, 296, 299, 298)
Type2 <- c(322, 326, 315, 318, 320, 320)
Type3 <- c(309, 327, 317, 315)
d <- stack(list("Type1" = Type1, "Type2" = Type2, "Type3" = Type3))
boxplot(values ~ ind, data = d, xlab = "Type", ylab = "Wear times")  # possible normal dist with possible equal variances

oneway.test(values ~ ind, data = d, var.equal = TRUE)
# One-way analysis of means
# data:  values and ind
# F = 31.027, num df = 2, denom df = 12, p-value = 1.81e-05          # extremely significant ; at least one population has a different mean

# Question 11.8
str(PlantGrowth)
plot(weight ~ group, data = PlantGrowth, xlab = "Group", ylab = "Weight")                # skewed ; (equal variances)

kruskal.test(weight ~ group, data = PlantGrowth)
# Kruskal-Wallis rank sum test
# data:  weight by group
# Kruskal-Wallis chi-squared = 7.9882, df = 2, p-value = 0.01842     # there is a significant difference in the means

# Question 11.9
x <- c(63, 64, 95, 64, 60, 85)
y <- c(58, 56, 51, 84, 77)
z <- c(85, 79, 59, 89, 80, 71, 43)
d <- stack(list("test1" = x, "test2" = y, "test3" = z))
plot(values ~ ind, data = d, xlab = "test", ylab = "grade") 

oneway.test(values ~ ind, data = d, var.equal = FALSE)
#  One-way analysis of means (not assuming equal variances)
# data:  values and ind
# F = 0.36912, num df = 2.0000, denom df = 9.6449, p-value = 0.7007  # no different result ; equally difficult exams

oneway.test(values ~ ind, data = d, var.equal = TRUE)
# One-way analysis of means
# data:  values and ind
# F = 0.37129, num df = 2, denom df = 15, p-value = 0.696            # no different result ; equally difficult exams

# 11.2 Using lm() for ANOVA

# The mathematics behind analysis of variance is the same as that behind linear regression
# Namely, it uses least-squares estimates based on a linear model
# As such, it makes sense to unify the approaches. To do so requires a new idea in the linear model
# To illustrate, we begin with an example comprising just two samples, to see how ttests are handled with the lm() function

# example : ANOVA for two independent samples
# Suppose we have two independent samples from normally distributed populations. Let X11, X12,...,X1n record the first and X21, X22, ..., X2n the second
# Assume the population means are mu1 and mu2 and the two samples have a com-mon variance
# We may perform a two-sided significance test of mu1=mu2 with a t-test
mu1 = 0 ; mu2 = 1
x <- rnorm(15, mean = mu1, sd = 1)
y <- rnorm(15, mean = mu2, sd = 1)
t.test(x, y, var.equal = TRUE)                   # the p-value is significant ; reject the H0 that the difference in means is 0

d <- stack(list("x" =x, "y" =y))                 # Combine the in two columns (values and ind (indicators)) data frame

# Let us call the data vector of values as Y 
# and L1(i) be an indicator function that is 1 if the level of the factor for the ith data value is 1. Similarly, define L2(i)
# Then we can rewrite our model as Yi=mu1*L1(i) + mu2*L2(i) + epsiloni
# When the data for the first sample is considered, L2(i)=0, and this model is simply Yi=mu1+epsiloni
# When the second sample is considered, the other dummy variable is 0, and the model considered is Yi = mu2+epsiloni
# We can rewrite the model to use just the second indicator variable. We use different names for the coefficients: Yi=Beta1+Beta2*L2(i)+epsiloni
# Now when the data for the first sample is considered the model is Yi=Beta1+epsiloni, so Beta1 is still mu1
# However, when the second sample is considered, we have Yi=Beta1+Beta2+epsiloni, so mu2=Beta1+Beta2. That is, Beta2=mu2-mu1
# We say that level 1 is a reference level, as the mean of the second level is represented in reference to the first
# It turns out that statistical inference is a little more natural when we pick one of the means to serve as a reference
# The resulting model looks just like a linear-regression model where xi is L2(i)
# We can fit it that way and interpret the coefficients accordingly
# The model is specified the same way, as with oneway.test(), y ~ f, where y holds the data and f is a factor indicating which group the data is for

d
res <- lm(values ~ ind, data = d)
summary(res)
# Coefficients:
#              Estimate  Std. Error  t value  Pr(>|t|)  
# (Intercept)  0.008453    0.237362    0.036    0.9718  
# indy         0.830796    0.335680    2.475    0.0196 *
# F-statistic: 6.125 on 1 and 28 DF,  p-value: 0.01965

# the variable indy, which means the y part of ind
# The marginal t-test tests the null hypothesis that Beta2=0, which is equivalent to the test that mu1=mu2
# The F-statistic also tests the hypothesis that Beta2=0. In this example, it is identical to the marginal t-test, as there are only two samples

# Alternatively, we can try to fit the model using two indicator functions, Yi=mu1*L1(i)+mu2*L2(i)+epsiloni
# This model is specified in R by dropping the implicit intercept term with ind—1 in the model formula

res <- lm(values ~ ind-1, data = d)
summary(res)
# Coefficients:
#              Estimate  Std. Error  t value  Pr(>|t|)   
# indx         0.008453    0.237362    0.036   0.97184   
# indy         0.839249    0.237362    3.536   0.00144 **
# F-statistic: 6.251 on 2 and 28 DF,  p-value: 0.005694

# Now the estimates have a clear interpretation in terms of the means, 
# but the marginal t-tests are less useful, as they are testing simply whether the respective means are 0, rather than whether their difference is 0
# The F-statistic in this case is testing whether both Beta’s are 0

# 11.2.1 Treatment coding for analysis of variance

# When there are k levels, k-1 indicator variables are used
# For example, if the model is Xij=mui+epsilonij, i,...,k, then this can be fit using Yi=Beta1+Beta2*L2+...+Betak*Lk(i)+epsiloni
# The mean of the reference level, mu1, is coded by Beta1, and the other Beta’s are differences from that
# That is, Betai=mui-mu1 for i=2,...,k
# This method of coding is called treatment coding and is used by default in R with unordered factors
# It is not the only type of coding, but it is the only one we will discuss
# Treatment coding uses a reference level to make comparisons
# This is chosen to be the first level of the factor coding the group
# To change the reference level we can use the relevel() function in the following manner: f = relevel(f, ref=...)
# The argument ref=  specifies the level we wish to be the reference level

df <- subset(babies, select = c("wt", "smoke"))
plot(wt ~ factor(smoke), data = df, main = "Birthweight by smoking level")
res <- lm(wt ~ factor(smoke), data = df)
summary(res)
# Coefficients:
#                 Estimate  Std. Error  t value  Pr(>|t|)    
# (Intercept)     122.7776      0.7598  161.597   < 2e-16 ***
# factor(smoke)1   -8.6681      1.1073   -7.828  1.06e-14 ***
# factor(smoke)2    0.3066      1.9705    0.156     0.876    
# factor(smoke)3    1.6593      1.9042    0.871     0.384    
# factor(smoke)9    3.9224      5.6551    0.694     0.488

# The marginal t-tests indicate that the level 1 of the smoke factor is important, whereas the others may not contribute
# That is, this is strong evidence that a mother's smoking during pregnancy decreases a baby's birth weight
# The treatment coding quantifies this in terms of differences from the reference level of never smoked
# The estimate, -8.668, says that the birth weight of a baby whose mother smoked during her pregnancy is predicted to be 8.688 grams,
# less than that of a baby whose mother never smoked

# 11.2.2 Comparing multiple differences

# When analysis of variance is performed with lm(), the output contains numerous statistical tests : 
# The F-test that is performed uses for the null hypothesis that Beta2=Beta3=...= Betak=0 against an alternative that one or more differ from 0
# That is, that one or more of the treatments has an effect compared to the reference level
# The marginal t-tests that are performed are two-sided tests with a null hypothesis that Betai=Beta1. One each is done for i=2, ..., k
# These test whether any of the additional treatments have a different effect from the reference one when controlled by the other variables
# However, we may wish to ask other questions about the various parameters
# For example, comparisons not covered by the standard output are "Do the Beta2 and Beta3 differ?" and "Are Beta1 and Beta2 half of Beta3?" 
# We show next how to handle simultaneous pairwise comparisons of the parameters, such as the first comparison
# This procedure is implemented in the TukeyHSD() function as illustrated in the next example

# example
str(ewr)
ewr.out <- subset(ewr, subset = inorout == "out", select = 3:10)
out <- stack(ewr.out)
names(out) <- c("time", "airline")
levels(out$airline)
plot(time ~ airline, data = out)
res <- lm(time ~ airline, data = out)
summary(res)
# Coefficients:
#              Estimate  Std. Error  t value  Pr(>|t|)    
# (Intercept)  27.05652     0.72041    37.557     < 2e-16 ***
# airlineCO     3.83478     1.01881     3.764    0.000228 ***
# airlineDL    -2.05217     1.01881    -2.014    0.045503 *  
# airlineHP     1.52609     1.01881     1.498    0.135949    
# airlineNW    -4.06087     1.01881    -3.986    9.84e-05 ***
# airlineTW    -1.65217     1.01881    -1.622    0.106665    
# airlineUA    -0.03913     1.01881    -0.038    0.969406    
# airlineUS    -3.83043     1.01881    -3.760    0.000231 ***
# F-statistic:    13.82 on 7 and 176 DF,  p-value: 3.265e-14

TukeyHSD(res)                # the TukeyHSD() function wants aov() to fit the linear model, not lm()

res.aov <- aov(time ~ airline, data = out) 
TukeyHSD(res.aov)
# $airline
#              diff         lwr         upr     p adj                # reference level :
# CO-AA  3.83478261   0.7093117  6.96025351 0.0054791                                    AA
# DL-AA -2.05217391  -5.1776448  1.07329699 0.4752917                                    AA
# HP-AA  1.52608696  -1.5993839  4.65155786 0.8077141                                    AA
# NW-AA -4.06086957  -7.1863405 -0.93539867 0.0024526                                    AA
# TW-AA -1.65217391  -4.7776448  1.47329699 0.7366613                                    AA
# UA-AA -0.03913043  -3.1646013  3.08634046 1.0000000                                    AA
# US-AA -3.83043478  -6.9559057 -0.70496388 0.0055620                                    AA
# DL-CO -5.88695652  -9.0124274 -2.76148562 0.0000009                                    CO
# HP-CO -2.30869565  -5.4341666  0.81677525 0.3183921                                    CO
# NW-CO -7.89565217 -11.0211231 -4.77018127 0.0000000                                    CO
# TW-CO -5.48695652  -8.6124274 -2.36148562 0.0000063                                    CO
# UA-CO -3.87391304  -6.9993839 -0.74844214 0.0047828                                    CO
# US-CO -7.66521739 -10.7906883 -4.53974649 0.0000000                                    CO
# HP-DL  3.57826087   0.4527900  6.70373177 0.0129034                                    DL
# NW-DL -2.00869565  -5.1341666  1.11677525 0.5040123                                    DL
# TW-DL  0.40000000  -2.7254709  3.52547090 0.9999308                                    DL
# UA-DL  2.01304348  -1.1124274  5.13851438 0.5011248                                    DL
# US-DL -1.77826087  -4.9037318  1.34721003 0.6574978                                    DL
# NW-HP -5.58695652  -8.7124274 -2.46148562 0.0000039                                    HP
# TW-HP -3.17826087  -6.3037318 -0.05278997 0.0431694                                    HP
# UA-HP -1.56521739  -4.6906883  1.56025351 0.7867236                                    HP
# US-HP -5.35652174  -8.4819926 -2.23105084 0.0000115                                    HP
# TW-NW  2.40869565  -0.7167752  5.53416655 0.2656976                                    NW
# UA-NW  4.02173913   0.8962682  7.14721003 0.0028276                                    NW
# US-NW  0.23043478  -2.8950361  3.35590568 0.9999984                                    NW
# UA-TW  1.61304348  -1.5124274  4.73851438 0.7597334                                    TW
# US-TW -2.17826087  -5.3037318  0.94721003 0.3948730                                    TW
# US-UA -3.79130435  -6.9167752 -0.66583345 0.0063622                                    UA

# The output of TukeyHSD() is best viewed with the plot of the confidence intervals
# This is created by calling plot() on the output. The argument las=2 turns the tick-mark labels perpendicular to the axes
plot(TukeyHSD(res.aov), las=2)

# For a given confidence level and sample, if the confidence interval excludes a population parameter, 
# then the two-sided significance test of the same parameter will be rejected
# we see several statistically significant differences at the alpha=0.05 level, the first few being CO-AA and NW-AA (just visible on the graph shown)

# Problems 11.2.3

# Question 11.10
str(MLBattend)
summary(lm(attendance ~ league, data = MLBattend))
# Coefficients:
#              Estimate  Std. Error  t value  Pr(>|t|)    
# (Intercept)   1716321       36259   47.334    <2e-16 ***
# leagueNL       127296       52093    2.444    0.0147 *             # muNL-muAL != 0 ; the difference in mean attendance of two leagues is significant

AL <- subset(MLBattend, subset = league == "AL" , select = "attendance")
NL <- subset(MLBattend, subset = league == "NL" , select = "attendance")
plot(attendance ~ league, data = MLBattend)
t.test(AL, NL, var.equal = TRUE)                                     
# p-value = 0.01475 ; the difference in mean attendance of two leagues is significant 

# Question 11.11
str(Traffic)
summary(lm(y ~ as.factor(year), data = Traffic))
#Coefficients:
#                    Estimate Std. Error t value Pr(>|t|)    
# (Intercept)          22.7609     0.9097  25.020   <2e-16 ***
# as.factor(year)1962  -2.4239     1.2865  -1.884   0.0612 .         # Beta2 = mu1962-mu1961 "could be" (0.05-0.1) significant 
                                                                     # thus the variable year may not have an effect on the number of deaths

summary(lm(y ~ limit, data = Traffic))
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   23.130      0.799  28.951  < 2e-16 ***
# limityes      -4.217      1.305  -3.232  0.00146 **                # muyes-muno is highly significant ; the variable limit has an effect

# Question 11.12
Type1 <- c(303, 293, 296, 299, 298)
Type2 <- c(322, 326, 315, 318, 320, 320)
Type3 <- c(309, 327, 317, 315)
d <- stack(list("1" = Type1, "2" = Type2, "3" = Type3))
names(d) <- c("wear.time", "Type")
summary(lm(wear.time ~ Type, data = d))                              # assumption : Normal(mean_cyl, sigma) : 
# each cyl is normally distributed with its own level mean, but variance is equal for each level of the variable cyl
# Coefficients:
#              Estimate  Std. Error  t value  Pr(>|t|)    
# (Intercept)   297.800       2.205  135.077   < 2e-16 ***
# Type2          22.367       2.985    7.493   7.3e-06 ***           # extremely significant difference ; Beta2 = muType2 - muType1 != 0
# Type3          19.200       3.307    5.806   8.4e-05 ***           # extremely significant difference ; Beta3 = muType3 - muType1 != 0
# F-statistic: 31.03 on 2 and 12 DF,  p-value: 1.81e-05              # reject H0 : Beta1(muType1) = Beta2 = Beta3 = 0 (assuming variances are equal)

oneway.test(wear.time ~ Type, data = d, var.equal = FALSE)
# F = 46.475, num df = 2, denom df = 6.4165, p-value = 0.0001522     # the result (rejecting H0) is the same,
                                                                     # but p-values are different, because variances were assumed not equal
oneway.test(wear.time ~ Type, data = d, var.equal = TRUE)
# F = 31.027, num df = 2, denom df = 12, p-value = 1.81e-05          # again the same p-values with	lm() model																 

# Question 11.13
str(mtcars)
plot(mpg ~ as.factor(cyl), data = mtcars)                            # variances are not equal
oneway.test(mpg ~ as.factor(cyl), data = mtcars)                     # var.equal = FALSE is the default value

# F = 31.624, num df = 2, denom df = 18.032, p-value = 1.271e-06     # extremely significant difference in mean(s) of at least one cylinder level

# Question 11.14
str(npdb)
plot(amount ~ as.factor(year), data = npdb, xlab = "Year")           # skewed ; not appropriate for a one-way anova

plot(log(amount) ~ as.factor(year), data = npdb, xlab = "Year")      # symmetric distribution in 2000, 2001, 2002, but still skewed in 2003

summary(lm(log(amount) ~ factor(year), subset= year < 2003, data = npdb))
# F-statistic: 117.9 on 2 and 6789 DF,  p-value: < 2.2e-16
# The p-value for the F-test is tiny, indicating that the null hypothesis is not likely to have yielded this data

oneway.test(log(amount) ~ factor(year), subset= year < 2003, data = npdb)
# F = 115.3, num df = 2.0, denom df = 1086.6, p-value < 2.2e-16      # assuming not equal variances

# Question 11.15
str(mtcars)
plot(mpg ~ factor(am), data = mtcars, xlab = "automatic")            # skewed, not equal variances
oneway.test(mpg ~ factor(am), data = mtcars, var.equal = TRUE)
# F = 16.86, num df = 1, denom df = 30, p-value = 0.000285           # the difference in means of manual and automatic car numbers is extremely significant 

# Question 11.16
str(morley)
plot(Speed ~ factor(Expt), data = morley, xlab = "Expt")             # some skewed and not equal variances but not accepted like that in the question
summary(lm(Speed ~ factor(Expt), data = morley))
# The marginal tests find that all the means are different from the reference level 1

TukeyHSD(aov(Speed ~ factor(Expt), data = morley))                   # TukeyHSD works with aov() ; lm() and aov() returns the same p-values
plot(TukeyHSD(aov(Speed ~ factor(Expt), data = morley)), las = 2)
# TukeyHSD tests compare all possible pairs and find only 4 and 5 means are different from the reference level 1

# Question 11.17
str(carsafety)
plot(Other.deaths ~ type, data = carsafety)                          # variances are not equal but I assume it was accepted equal in the question
oneway.test(Other.deaths ~ type, data = carsafety, var.equal = TRUE)
# F = 7.2887, num df = 6, denom df = 26, p-value = 0.000121 ; there is at least one type that has an extremely significant difference in means
plot(TukeyHSD(aov(Other.deaths ~ type, data = carsafety), las = 2))
# TukeyHSD shows that pickup is the only type that has a different mean than all the others. All the other types than pickup have the same mean

# Question 11.18
plot(count ~ spray, data = InsectSprays)                             # variances are not equal but I assume it was accepted equal in the question
summary(aov(count ~ spray, data = InsectSprays))
#             Df  Sum Sq  Mean Sq  F value  Pr(>F)    
# spray        5    2669    533.8     34.7  <2e-16 ***
# Residuals   66    1015     15.4                                    # there is at least one spray that has an extremely significant difference in means
plot(TukeyHSD(aov(count ~ spray, data = InsectSprays)), las = 2) 
# spray C-A, D-A, E-A, C-B, D-B, E-B, F-C, F-D, F-E pairs have different means ; seems like A,B,F and C,D,E are similar , in the same group

# 11.3 ANCOVA

# An analysis of covariance(ANCOVA) is the term given to models where both categorical and numeric variables are used as predictors
# Performing an ANCOVA in R is also done using lm()

# example
str(babies)
plot(wt ~ wt1, data=babies, pch = smoke, subset = wt1 < 800)

# the model : birthweight = Beta1 + Beta2*mom'sweight + Beta3*Lmomsmokesnow

# This model is a parallel-lines model. For those mothers who don't smoke (L=0), the intercept is given by Beta1
# for those who do (L=1), the intercept is Beta1+Beta3
# The slope is given by Beta2 
# The actual model we fit is different, as there are four levels to the smoke variable, 
# so there would be three indicator variables, each indicating a difference in the intercept

# so we fit the model as follows : 
res <- lm(wt ~ wt1 + factor(smoke), data = babies, subset = wt1 < 800)
summary(res)
# Coefficients:
#                  Estimate  Std. Error  t value  Pr(>|t|)    
# (Intercept)     107.06738     3.26415   32.801   < 2e-16 ***
# wt1               0.12043     0.02445    4.925  9.60e-07 ***
# factor(smoke)1   -8.39710     1.12462   -7.467  1.58e-13 ***
# factor(smoke)2    0.79443     1.99742    0.398     0.691    
# factor(smoke)3    1.25500     1.91119    0.657     0.512    
# factor(smoke)9    2.86829     5.64518    0.508     0.611
# F-statistic: 20.06 on 5 and 1194 DF,  p-value: < 2.2e-16

# We read this output the same way we read the output of any linear regression
# For each coefficient, the marginal t-test of Betai=0 against a two-sided alternative is performed
# Three variables are flagged as highly significant
# The third one for the variable factor(smoke)1 says that the value of this coefficient, -8.3971, is statistically different from 0
# This value is an estimate of the difference between the intercept for the data of nonsmoking mothers(level 0) and the data of mothers who answered "smokes now" (level 1)

plot(wt ~ wt1, pch = smoke, data = babies, subset = wt1 < 800)
abline(107.0674, 0.1204)                                             # birthweight = Beta1 + Beta2*mom'sweight -------------> nonsmoking
abline(107.0674 - 8.3971, 0.1204, lty = 2)                           # birthweight = Beta1 + Beta2*mom'sweight + Beta3 -----> smokes now

# The last line of the output of summary(res) shows that the F-test is rejected
# This is a test of whether all the coefficients except the intercept are 0
# A better test would be to see whether the additional smoke variable is significant once we control for the mother's weight

# This is done using anova() to compare the two models :
res.1=lm(wt ~ wt1, data = babies, subset = wt1 < 800)
anova(res.1, res)
#   Res.Df     RSS  Df  Sum of Sq       F     Pr(>F)    
# 1   1198  394572                                  
# 2   1194  372847   4      21725  17.393  7.017e-14 ***             # The small p-value indicates that the additional term is warranted

# Problems 11.3.1

# Question 11.19
str(nym.2002)
res <- lm(time ~ age + gender, data = nym.2002)
summary(res)
# Coefficients:
#              Estimate   Std. Error   t value  Pr(>|t|)    
# (Intercept)  241.4502       6.2503    38.630   < 2e-16 ***
# age            1.2259       0.1548     7.918  6.42e-15 ***
# genderMale   -29.3967       3.6334    -8.091  1.72e-15 ***         # The difference between male and female is estimated to be a half-hour in total time
# F-statistic:  52.6 on 2 and 997 DF,  p-value: < 2.2e-16

# the running time for female athletes : 241.4502 + 1.2259*age
#                  for male runners   : (241.4502 - 29.3967) + 1.2259*age

# Question 11.20
str(mtcars)
res <- lm(mpg ~ wt + factor(am), data = mtcars) 
summary(res)   
# Coefficients:
#              Estimate  Std. Error  t value  Pr(>|t|)    
# (Intercept)  37.32155     3.05464   12.218  5.84e-13 ***
# wt           -5.35281     0.78824   -6.791  1.87e-07 ***  -------> the variable wt is significant 
# factor(am)1  -0.02362     1.54565   -0.015     0.988    ---------> the difference in the tranmission types is not significant
# F-statistic: 44.17 on 2 and 29 DF,  p-value: 1.579e-09

# the mpg for tramission type 0 : 37.32155 - 5.35281*weight
#         for tramission type 1 : 37.32155 - 5.35281*weight 

# Question 11.21
str(babies)
res <- lm(wt ~ gestation + wt1 + ht + factor(smoke), data = babies)
summary(res)
# Coefficients:
#                  Estimate  Std. Error  t value  Pr(>|t|)    
# (Intercept)     84.402829    7.508390   11.241   < 2e-16 ***       
# gestation        0.011101    0.006669    1.665    0.0962 .  
# wt1             -0.005256    0.004230   -1.242    0.2143    
# ht               0.558876    0.119209    4.688  3.06e-06 ***  ---> ht is the only significant variable
# factor(smoke)1  -8.941987    1.097627   -8.147  9.13e-16 ***  ---> the difference between smoking status 0 and 1 is extremely significant
# factor(smoke)2   0.604470    1.950611    0.310    0.7567    -----> between 2 and 0 is not significant
# factor(smoke)3   0.946599    1.891250    0.501    0.6168    -----> between 3 and 0 is not significant
# factor(smoke)9   3.958060    5.594959    0.707    0.4794    -----> between 9 and 0 is not significant
# F-statistic: 15.47 on 7 and 1228 DF,  p-value: < 2.2e-16

# the birth weight for nonsmoking mother (level 0)   : 84.402829 + 0.558876 *ht
#                  for "smokes now" mother (level 1) : (84.402829 - 8.941987) + 0.558876 *ht

# Question 11.22
str(kid.weights)
kid.weights$BMI <- (kid.weights$weight/2.54) / (kid.weights$height*2.54/100)^2
res <- lm(BMI ~ age + gender, data = kid.weights)
summary(res)
# Coefficients:
#               Estimate  Std. Error  t value  Pr(>|t|)    
# (Intercept)  16.559047    0.713778   23.199    <2e-16 ***
# age           0.009645    0.009983    0.966     0.335    --------> the variable age does not make a significant difference in the mean value of BMI
# genderM      -0.063286    0.794225   -0.080     0.937    --------> the difference between gender M and F is also not significant
# F-statistic: 0.4669 on 2 and 247 DF,  p-value: 0.6275    --------> all the Beta's (only Betaage here)in the model is 0

res.full   <- lm(BMI ~ age + gender, data = kid.weights)
res.age    <- lm(BMI ~ age,          data = kid.weights)
res.gender <- lm(BMI ~ gender,       data = kid.weights)
anova(res.age, res.full)
#   Res.Df    RSS Df Sum of Sq      F Pr(>F)
# 1    247 9685.7                           
# 2    248 9685.9 -1  -0.24898 0.0063 0.9366 --> additional gender variable makes no significant effect on BMI
anova(res.gender, res.full)
#   Res.Df    RSS Df Sum of Sq      F Pr(>F)
# 1    248 9722.3                           
# 2    247 9685.7  1    36.603 0.9334 0.3349 --> additional age variable makes no significant effect on BMI
stepAIC(res)
# ...
# lm(formula = BMI ~ 1, data = kid.weights)
# Coefficients:
# (Intercept)  
#      16.99 --------------> We see that neither variable is significant by stepAIC() as well

# Question 11.23
str(cfb)
res <- lm(log(INCOME+1) ~ AGE + factor(EDUC), data = cfb)
summary(res)
# Coefficients:
#                Estimate Std. Error t value Pr(>|t|)    
# (Intercept)     9.535988   0.547706  17.411  < 2e-16 ***
# AGE            -0.003675   0.002041  -1.801 0.072066 . 
# factor(EDUC)2   1.797469   0.712555   2.523 0.011807 *  
# factor(EDUC)3  -0.222847   0.711914  -0.313 0.754328    
# factor(EDUC)4   0.182693   0.919316   0.199 0.842518    
# factor(EDUC)5   0.097783   0.685195   0.143 0.886550    
# factor(EDUC)6   0.185091   0.612735   0.302 0.762660    
# factor(EDUC)7   0.662084   0.628059   1.054 0.292062    
# factor(EDUC)8   0.389109   0.597304   0.651 0.514913    
# factor(EDUC)9   0.503061   0.572068   0.879 0.379414    
# factor(EDUC)10 -0.300559   0.561380  -0.535 0.592499    
# factor(EDUC)11  0.593833   0.556132   1.068 0.285877    
# factor(EDUC)12  0.992501   0.534957   1.855 0.063854 .  
# factor(EDUC)13  1.073370   0.547779   1.959 0.050337 .  
# factor(EDUC)14  1.281775   0.539586   2.375 0.017717 *  
# factor(EDUC)15  0.818205   0.561654   1.457 0.145498    
# factor(EDUC)16  1.719069   0.538142   3.194 0.001446 ** 
# factor(EDUC)17  1.949972   0.541797   3.599 0.000335 ***

# according to alpha = 0.05 significance level, AGE is not significant

# Question 11.24
str(normtemp)
res <- lm(temperature ~ hr + factor(gender), data = normtemp)
summary(res)
# Coefficients:
#                  Estimate  Std. Error  t value  Pr(>|t|)    
# (Intercept)     96.250814    0.648717  148.371   < 2e-16 ***
# hr               0.025267    0.008762    2.884   0.00462 ** -----> significant
# factor(gender)2  0.269406    0.123277    2.185   0.03070 *  -----> the difference between gender1 (Male) and gender2(Female) is significant

# the body temperature for a Male   : 96.250814 + 0.025267*hr(heart rate)
#                      for a Female : (96.250814 + 0.269406) + 0.025267*hr(heart rate)

# 11.4 Two-way ANOVA

# "Two-way analysis of variance" is the term given when a numeric response variable is modeled by two categorical predictors
# After we fit the model into the regression framework, the t-tests and partial F test will be available for analysis

# Let Y be the response variable and x1 and x2 be two categorical predictors, with n1 and n2 levels respectively
# The simplest generalization of the one-way ANOVA model is the two-way additive model: Y_ijk = mu + alpha_i + gamma_j + epsilon_ijk
# mu : the grand mean, 
# alpha_i : the mean for the ith level of x1, 
# gamma_j : the mean for the ith level of x2, 
# epsilon_ijk : the error terms, an i.i.d. sequence with a Normal(0,sigma) distribution

# Two common significance tests (the t-tests and partial F test) investigate whether the different levels of x1 and x2 have an effect on the mean of Y
# For the first variable, x1, the hypotheses are H0 : alpha_1 = alpha_2 = ... = alpha_n1 = alpha , Ha : at least one alpha_i is different
# For the second variable, x2, the hypotheses are H0 : gamma_1 = gamma_2 = ... = gamma_n2 = gamma , Ha : at least one gamma_j is different

# example
#        Driver                 Driver
# Car    a     b     c   Car    a      b    c    # Ideally, there should be little variation. But is this the case with the data?
#   A 33.3  34.5  37.4     B 32.6  33.4  36.6
#     33.4  34.8  36.8       32.5  33.7  37.0
#     32.9  33.8  37.6       33.0  33.9  36.7

# 11.4.1 Treatment coding for additive two-way ANOVA

# Let L_b_driver_(i) be the indicator (0: not effective - 1:effective) that the observation is for driver b (similarly L_c_driver_(i) for driver c)
# and L_B_car_(i) be the indicator (0: not effective - 1:effective) that the car is B
# Then the additive model becomes : Yi = Beta1 + B2*L_b_driver + Beta3*L_c_driver + Beta4*L_B_car + epsiloni

# Recall that with treatment coding we interpret the parameters in terms of differences
# Beta1 = mu + alpha_A + gamma_a : the sum of the grand mean, the mean of the first level of the first variable, and the mean of the first level of the second variable 
# As Beta1 + Beta2 is the mean for car A, driver b, this would be mu + alpha_A + gamma_b or Beta2 = gamma_b - gamma_a
# Similarly, the Beta3 and Beta4 can be interpreted in terms of differences, as Beta3 = gamma_c - gamma_a and Beta4 = alpha_B - alpha_A

# 11.4.2 Testing for row or column effects

# To perform the significance test that the row variable has constant mean we can use the partial F-test
# In our example, this is the same as saying Beta4 = 0
# The partial F-test fits the model with and without Beta4 and uses the ratio of the residual sum of squares to make a test statistic
# The details are implemented in the anova() function

x <- c(33.3, 33.4, 32.9, 32.6, 32.5, 33.0, 34.5, 34.8, 33.8, 33.4, 33.7, 33.9, 37.4, 36.9, 37.6, 36.6, 37.0, 36.7)
car <- factor(rep(rep(1:2,c(3,3)) , 3))
levels(car) <- c("A", "B")
driver <- factor(rep(1:3, c(6,6,6)))
levels(driver) <- letters[1:3]                   # make letters not numbers

res.add <- lm(x ~ car + driver)                  # with additional car : car B is added in this example
res.nocar <- lm(x ~ driver)                      # no car added : Beta4 = 0 
anova(res.add, res.nocar)
#   Res.Df     RSS  Df  Sum of Sq   F    Pr(>F)   
# 1     14  1.3144                            
# 2     15  2.8167  -1    -1.5022  16  0.001316 ** ----------------> the difference is significant, leading us to rule out the simpler model

# We decided that the type of car is effective in the model
# How about the effect of drivers? Is there a difference? 
# The null hypothesis is now H0 : gamma_a = gamma_b = gamma_c (driver effects), which can be rewritten as Beta2 = Beta3 = 0
# As such, we fit the model without the Beta2 and Beta3 terms and compare to the full model as above

res.add <- lm(x ~ car + driver)
res.nodriver <- lm(x ~ car)
anova(res.add, res.nodriver)
#    Res.Df     RSS  Df  Sum of Sq       F     Pr(>F)    
# 1      14   1.314                                  
# 2      16  55.138  -2    -53.823  286.63  4.376e-12 *** ---------> the difference in drivers is also significant, leading us to rule out the simpler model

# 11.4.3 Testing for interactions

# What if there is an extra effect caused by the interaction(s) between one or more of the rows and columns?
# in the example above, let us assume that one is sportier, as there seems to be a difference in cars, which makes one of the drivers drive faster
# That is, there is an interaction when the two factors combine
# the model becomes : Yi = Beta1 + B2*L_b_driver + Beta3*L_c_driver + Beta4*L_B_car + Beta5*L_b_driver*L_B_car + Beta6*L_c_driver*L_B_car + epsiloni
# then, a significance test to see if the extra terms from the interaction are necessary can be done with the partial F-test

# Interaction plots :
# An interaction plot is a plot that checks to see whether there is any indication of interactions
# This graphic is made with the function interaction.plot()
# The template is interaction.plot(f, trace.factor, y, legend=TRUE)
# The f holds the main factor (x-axis), and the other is in trace factor (what we want to see in the graph), the response variable is stored in y (y-axis)
# By default, a legend will be drawn indicating the levels of the trace factor

interaction.plot(driver, car, x, legend = TRUE)                      # no interaction as they are parallel lines

# Significance test for presence of interactions :
# To test the hypothesis of no interaction formally we can use the partial F-test
# The null hypothesis can be expressed as, for our car-and-driver example, as Beta5 = Beta6 = 0 (means no interaction exist)
# An interaction can be specified in different ways in the model formula :
# : -> f1:f2 -> will introduce the interaction terms for the two factors
# * and + -> f1*f2 and f1+f2 -> will introduce not only an interaction, but the main effects
# ^ -> (f1+f2)^2 -> will do the main effects and all possible interactions up to order 2. This generalizes with higher powers and more terms

lm.int <- lm(x ~ car * driver)
lm.add <- lm(x ~ car + driver)
anova(lm.add, lm.int)
#    Res.Df     RSS  Df  Sum of Sq       F  Pr(>F)
# 1      14  1.3144                           
# 2      12  1.2800   2   0.034444  0.1615  0.8527 ----------------> The large p-value is consistent with the interaction plot, indicating no interaction

# example
x <- c(92, 80, 80, 78, 63, 65, 65, 69, 60, 59, 57, 51, 60, 58, 52, 65)
Seat <- factor(rep(c("Good","Bad"),c(8,8)))
Popcorn <- factor(rep(rep(c("Y","N"),c(4,4)), 2))
replicate <- rep(1:4,4)
ftable(xtabs(x ~ Popcorn + Seat + replicate))
interaction.plot(Seat, Popcorn, x)                                   # there is an interaction as the slopes are not parallel

# let us test and see this interaction with anova()
res.int <- lm(x ~ Seat * Popcorn)
res.add <- lm(x ~ Seat + Popcorn)
anova(res.int, res.add)
#    Res.Df    RSS  Df  Sum of Sq       F    Pr(>F)   
# 1      12  277.5                                
# 2      13  638.5  -1       -361  15.611  0.001924 ** ------------> The small p-value casts doubt on the null hypothesis model of no interaction

summary(res.int)
# Coefficients:
#                   Estimate  Std. Error  t value  Pr(>|t|)    
# (Intercept)         58.750       2.404   24.434  1.33e-11 ***
# SeatGood             6.750       3.400    1.985   0.07047 .  
# PopcornY            -2.000       3.400   -0.588   0.56732    
# SeatGood:PopcornY   19.000       4.809    3.951   0.00192 **

# It appears that a good seat and popcorn can go a long way toward a moviegoer's satisfaction (at least from this fabricated data)
# Perhaps new seats and less expensive popcorn will keep the customers coming back

# Problems 11.4.4

# Question 11.25
likability <- c(-1,4,0,-1,4,1,6,2,7,1,2,2,7,5,2,3,6,1)
web        <- factor(rep(c("N", "Y"), c(9,9)))
tv         <- factor(rep(c("0", "1-2", "3+"), c(6,6,6)))
res.web    <- lm(likability ~ web)
res.tv     <- lm(likability ~ tv)
res.both   <- lm(likability ~ tv + web)

summary(res.web)             #1
# Coefficients:
#              Estimate  Std. Error  t value  Pr(>|t|)  
# (Intercept)    2.4444      0.8731     2.80    0.0129 *
# webY           0.7778      1.2348     0.63    0.5377  -----------> no significant difference between being exposed and not to the ad on web
                                                                     # thus, web advertising is not effective alone

# How about together with TV ad exposure?
anova(res.both, res.tv)      #2 
#   Res.Df     RSS  Df  Sum of Sq       F   Pr(>F)  
# 1     14  69.500                              
# 2     15  86.167  -1    -16.667  3.3573  0.08826 . --------------> no additional effectiveness occur

# Question 11.26
str(grip)
ftable(xtabs(UBP ~ person + replicate + grip.type, data = grip))

with(grip, interaction.plot(grip.type, person, UBP, legend = TRUE))
res.int   <- lm(UBP ~ person * grip.type, data = grip)
res.noint <- lm(UBP ~ person + grip.type, data = grip)
res.per   <- lm(UBP ~ person, data = grip)
res.grip  <- lm(UBP ~ grip.type, data = grip)
res.none  <- lm(UBP ~ 1, data = grip)

anova(res.int, res.noint)
#   Res.Df     RSS  Df  Sum of Sq       F  Pr(>F)
# 1     24  483.86                           
# 2     30  509.01  -6     -25.15  0.2079  0.9709 -----------------> This agrees with the interaction plot. No evidence of an interaction is present

anova(res.none, res.per)
#   Res.Df     RSS  Df  Sum of Sq       F  Pr(>F)
# 1     35  875.55                           
# 2     32  848.19   3      27.35  0.3439  0.7937 -----------------> No evidence that the difference varies among subjects

anova(res.none, res.grip)
#   Res.Df     RSS  Df  Sum of Sq       F     Pr(>F)    
# 1     35  875.55                                  
# 2     33  536.36   2     339.18  10.434  0.0003079 *** ----------> The effect of the grip seems significant

stepAIC(res.int)
# Call:
# lm(formula = UBP ~ grip.type, data = grip)
# Coefficients:
#               (Intercept)  grip.typeintegrated  grip.typemodern  
#                   163.669                5.919           -1.055 

# Question 11.27
str(mtcars)
with(mtcars, interaction.plot(factor(cyl), factor(am), mpg, legend = TRUE))              # seems no interaction occurs
res.int   <- lm(mpg ~ factor(cyl) * factor(am), data = mtcars)
res.noint <- lm(mpg ~ factor(cyl) + factor(am), data = mtcars)
res.cyl   <- lm(mpg ~ factor(cyl), data = mtcars)
res.am    <- lm(mpg ~ factor(am), data = mtcars)
res.none  <- lm(mpg ~ 1, data = mtcars)

anova(res.int, res.noint)
#   Res.Df     RSS  Df  Sum of Sq       F  Pr(>F)
# 1     26  239.06                           
# 2     28  264.50  -2    -25.436  1.3832  0.2686 -----------------> no interaction evidence

anova(res.none, res.cyl)
#   Res.Df      RSS  Df  Sum of Sq       F     Pr(>F)    
# 1     31  1126.05                                  
# 2     29   301.26   2     824.78  39.697  4.979e-09 *** ---------> the effect of cyl is significant

anova(res.none, res.am)
#   Res.Df     RSS  Df  Sum of Sq      F    Pr(>F)    
# 1     31  1126.0                                
# 2     30   720.9   1     405.15  16.86  0.000285 *** ------------> the effect of am is also significant

# let us crosscheck this and get the final model with stepAIC()

stepAIC(res.int)
# Call:
# lm(formula = mpg ~ factor(cyl) + factor(am), data = mtcars)
# Coefficients:
#               (Intercept)  factor(cyl)6  factor(cyl)8   factor(am)1  
#                    24.802        -6.156       -10.068         2.560

# Question 11.28
str(ToothGrowth)
with(ToothGrowth, interaction.plot(factor(dose), supp, len))         # there seems to be an interaction
res.noint <- lm(len ~ supp + factor(dose), ToothGrowth)
res.int <- lm(len ~ supp * factor(dose), ToothGrowth)

anova(res.full, res.add)
#   Res.Df     RSS  Df  Sum of Sq      F   Pr(>F)  
# 1     56  820.43                             
# 2     54  712.11   2     108.32  4.107  0.02186 * ---------------> proves that there is an interaction

# Question 11.29
str(OrchardSprays)
with(OrchardSprays, interaction.plot(factor(rowpos), treatment, decrease))         # there are more than one interaction
res.noint <- lm(decrease ~ treatment + factor(rowpos), data = OrchardSprays)
res.int <- lm(decrease ~ treatment * factor(rowpos), data = OrchardSprays)

anova(res.int, res.noint)
#   Res.Df    RSS   Df  Sum of Sq    F   Pr(>F)
# 1      0      0                         
# 2     49  18802  -49     -18802  NaN     NaN --------------------> ?

stepAIC(res.int)
# Error in stepAIC(res.int) : AIC is -infinity for this model, so 'stepAIC' cannot proceed 
