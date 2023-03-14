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


# Chapter 12 Two Extensions of the Linear Model ---------------------------------------------


# The logistic-regression model covers the situation where the response variable is a binary variable
# The nonlinear models we discuss use a function to describe the mean response that is not linear in the parameters

# 12.1 Logistic regression

# A binary variable is one that can have only two values, "success" or "failure" , often coded as 1 or 0
# In the ANOVA model we saw that we can use binary variables as predictors in a linear-regression model by using factors
# But what if we want to use a binary variable as a response variable?

# example
#                         Number of spam e-mails opened

#                                Offer in subject
#                                    yes           no
# First name in subject   yes     20 of 1,250   15 of 1,250
#                         no      17 of 1,250   8  of 1,250

# what can we say about the importance of including a name or an offer in the subject heading?

# For simplicity, assume that we have two variables, X and Y, where Y is a binary variable coded as a 0 or 1 (e.g. 1 could mean a spam message was opened)
# If we try to model the response with Yi = Beta0 + epsiloni or Yi = Beta0 + Beta1*xi + epsiloni, 
# then, as Yi is either 0 or 1, the epsiloni can't be an i.i.d. sample from a normal population
# Consequently, the linear model won't apply
# As having only two answers puts a severe restriction on the error term, instead the probability of success is modeled

# Let pi_i = P(Yi=1)
# Then pi_i is in the range 0 to 1
# We might try to fit the model pi_i = Beta0 + Beta1*xi + epsiloni, but again the range on the left side is limited, whereas that on the right isn't
# Even if we restrict our values of the xi, the variation of the epsiloni can lead to probabilities outside of [0,1]

# We mentioned that the assumption on the error can be viewed two ways:
# 1st -> the error terms, the epsiloni values, are a random sample from a mean a normally distributed population, 
# 2nd -> equivalently that each data point Yi is randomly selected from a Normal (mu_y|x, sigma) distribution independently of the others
# Thus, we have the following ingredients in simple linear regression:
#   - The predictors enter in a linear manner through Beta0 + Beta1*x1
#   - The distribution of each Yi is determined by the mean, mu_y/x, and some scale parameter sigma
#   - There is a relationship between the mean and the linear predictors (mu_y|x = Beta0 + Beta1*x1)

# The last point needs to be changed to continue with the binary regression model
# Let eta = Beta0 + Beta1*x1
# Then the change is to assume that eta can be transformed to give the mean by some function m() via mu_y|x = W(eta), 
# which can be inverted to yield back eta = inverse(m(mu_y|x))
# The function m() is called a link function, as it links the predictor with the mean

# The logistic function m(x) = e^x/(1+e^x) is often used, and the corresponding model is called logistic regression
# For this, we have pi_i = m(eta) = m(Beta0 + Beta1*xi) = e^(Beta0 + Beta1*xi)/(1+e^(Beta0 + Beta1*xi))
# The logistic function turns values between -inf and +inf into values between 0 and 1, so the numbers specifying the probabilities will be between 0 and 1
# When m(eta) is inverted : log(pi_i/(1-pi_i)) = eta = Beta0 + Beta1*xi , where
# log(pi_i/(1-pi_i)) is called log-odds ratio , thus, odds-ratio : (p/(1-p))

# To finish the model, we need to specify the distribution of Yi
# It is Bernoulli with success probability pi_i (for a logistic regression), so that no extra parameters, such as a standard deviation, are needed

# 12.1.1 Generalized linear models

# Logistic regression is an example of a generalized linear model
# The key ingredients are as above: a response variable Y and some predictor variables x1, x2,...,xp
# The predictors enter into the model via a single linear function: eta = Beta0 + Beta1*x1 + ... + Betap*xp ,
# the mean of Y given the x values is related to eta by an invertible link function m() as mu = m(eta) or inverse(m(mu)) = eta
# Finally, the distribution of Y is given in terms of its mean and, perhaps, a scale parameter such as sigma
# Thus, the model is specified by the coefficients Betai, a link function m(), and a probability distribution that may have an additional scale parameter

# 12.1.2 Fitting the model using glm()

# Generalized linear models are fit in R using the glm() function
# A template for usage is res=glm(formula, family=..., data=...) , we need to specify the probability distribution and the link function
# The argument family=  allows us to specify the distribution and the link (link function can be specified different than the default function)
# For logistic regression the argument is specified by family=binomial, as the default link function is what we want
# For comparison to simple linear regression, the link function is just an identity, and the family is specified as family=gaussian

# example : Comparing glm() and 1m() 
x1 <- rep(1:10, 2)
x2 <- rchisq(20, df = 2)
y  <- rnorm(20, mean = x1+2*x2, sd = 2)

res.lm <- lm(y ~ x1 + x2)
summary(res.lm)
# Coefficients:
#                Estimate  Std. Error  t value  Pr(>|t|)    
# (Intercept)     -0.1746      1.3711   -0.127       0.9    
# x1               1.0309      0.2026    5.089  9.09e-05 ***
# x2               2.0535      0.3223    6.372  6.96e-06 ***
# F-statistic: 33.07 on 2 and 17 DF,  p-value: 1.381e-06

res.glm <- glm(y ~ x1 + x2, family = gaussian)
summary(res.glm)
# Coefficients:
#                Estimate  Std. Error  t value  Pr(>|t|)    
# (Intercept)     -0.1746      1.3711   -0.127       0.9    
# x1               1.0309      0.2026    5.089  9.09e-05 ***
# x2               2.0535      0.3223    6.372  6.96e-06 ***
# Null deviance: 562.89  on 19  degrees of freedom
# Residual deviance: 115.09  on 17  degrees of freedom
# AIC: 99.758
# Number of Fisher Scoring iterations: 2

# The outcomes of this comparison :
# The same coefficients are found
# This is not surprising, but technically a different method is used
# For each coefficient, a two-sided significance test is done with null hypothesis that the value is 0
# For this model, the results are identical, as with lm()
# No information about the F statistic is given, as the theory does not apply here in general. Rather, the AIC is given 

# example
# Info given : risk factors associated with premature births include smoking and maternal malnutrition
# Do we find this to be the case with the data in the babies (UsingR) data set?

# Manipulation of the data set
babies.prem <- subset(babies, subset = gestation < 999 & wt1 < 999 & ht < 99 & smoke < 9, select = c("gestation","smoke","wt1","ht"))

# A birth is considered premature if the gestation period is less than 37 full weeks : 
babies.prem$preemie <- as.numeric(babies.prem$gestation < 7*37)
babies.prem[1:10,]           # to see the manipulated data set , we see the premature babies with codes 0 and 1 in the preemie column added at the end
table(babies.prem$preemie)   # 96 premature babies and 1079 not premature babies in the data set

# BMI will be used as a measure of malnutrition
babies.prem$BMI <- with(babies.prem, (wt1/2.2) / (ht*2.54/100)^2)
hist(babies.prem$BMI)        # looks okay

# Now, model the variable preemie by the levels of smoke and the variable BMI
res <- glm(preemie ~ factor(smoke) + BMI, family = binomial, data = babies.prem)
summary(res)
# Coefficients:
#                 Estimate  Std. Error  z value  Pr(>|z|)     -----> Z distribution was used
# (Intercept)     -3.42458     0.71159   -4.813  1.49e-06 ***
# factor(smoke)1   0.19353     0.23569    0.821     0.412    
# factor(smoke)2   0.31370     0.38896    0.806     0.420    
# factor(smoke)3   0.10114     0.40499    0.250     0.803    
# BMI              0.04014     0.03043    1.319     0.187

# None of the variables are flagged as significant. This indicates that the model with no effects is, perhaps, preferred
# We check which model is preferred by the AIC using stepAIC()

stepAIC(res)
# Step:  AIC=666.83
# preemie ~ 1
# Call:  glm(formula = preemie ~ 1, family = binomial, data = babies.prem)
# Coefficients:
#               (Intercept)  
#                    -2.419 -> the model of constant mean is chosen by this criteria, indicating that these risk factors do not show up in this data set 

# example : Number of spam e-mails opened
first.name <- rep(1:0, c(2500,2500))
offer <- rep(c(1,0,1,0), rep(1250,4))
opened <- c(rep(1:0, c(20, 1250-20)), rep(1:0, c(15, 1250-15)), rep(1:0, c(17, 1250-17)), rep(1:0, c(8, 1250-8)))
xtabs(opened ~ first.name + offer)

# to save typing
f <- function(x) rep(1:0, c(x, 1250-x))
opened <- c(sapply(c(20,15,17,8), f))

res.glm <- glm(opened ~ factor(first.name) + factor(offer), family = binomial)
summary(res.glm)
# Coefficients:
#                      Estimate  Std. Error  z value  Pr(>|z|)    
# (Intercept)           -4.8639      0.2598  -18.724    <2e-16 *** -> only the intercept is flagged as significant at the 0.05 level
# factor(first.name)1    0.3407      0.2635    1.293    0.1959    
# factor(offer)1         0.4813      0.2671    1.802    0.0716 .

# Suppose the estimates are correct (significant). How can we interpret them?
#   1) The coding is such that when no first name or offer is included, the log-odds ratio is -4.864
#   2) When the first name is included but not the offer, the log-odds ratio is −4.864 + 0.341
#   3) When both are included, it's -4.864 + 0.341+ 0.481

# odds-ratio = pi/(1-pi) = e^ -4.8639
# If we include the first name, the odds ratio goes up to e^ -4.864+0.341 = odds-ratio * e^0.341 , which is an additional factor of e^0.341=1.406
# if the original odds were 2 to 100, they go up to 2*(1.406) to 100

# Avoiding replication :
# The data was first entered by replication to produce variables first.name, offer, and opened with 5,000 values, so that all the recorded data was present
# The interface for glm() conveniently allows for tabulated data when the binomial family is used
# Not only is tabulated data easier to type in, we can save memory as we don’t store large vectors of data

opened <- c(8,15,17,20)
not.opened <- c(1250-opened)
opened.matrix <- cbind(opened = opened, not.opened = not.opened)
rownames(opened.matrix) = c("no.offer.no.firstname", "no.offer.yes.firstname", "yes.offer.no.firstname", "yes.offer.yes.firstname")
opened.matrix
# The predictor variables match the levels for the rows
offer <- c(0,0,1,1)
first.name <- c(0,1,0,1)
glm(opened.matrix ~ first.name + offer, family = binomial)
# Coefficients:
#               (Intercept)   first.name        offer  
#                   -4.8639       0.3407       0.4813

# 12.2 Nonlinear models

# The linear model is called "linear" because of the way the coefficients Betai enter into the formula for the mean
# These coefficients simply multiply some term
# A nonlinear model allows for more complicated relationships
# For example, an exponential model might have the response modeled as Yi = Beta0*e^-Beta1*xi + epsiloni
# Here, mu_y|x = Beta0*e^-Beta1*xi is not linear in the parameters due to the Beta1. It does not appear as an additive term like Beta1xi
# Variations on the exponential model are Yi = Beta0*xi*e^-Beta1*xi + epsiloni and Yi = Beta0*(e^-Beta1*xi(1-Beta2)+Beta2)) + epsiloni
# The first exponential model, with Beta1>0, may be used when the response variable decays as the predictor increases
# The second model has a growth-then-decay phase, and the third a decay, not to 0 but to some threshold amount Beta0*Beta2
# In general, a single-covariate, nonlinear model can be written as follows : Yi = f(xi|Beta0,Beta1,...,Betar) + epsiloni
# We have r+1 parameters and only one predictor with an additive error
# More general models could have more predictors and other types of errors, such as multiplicative
# The possibilities seem endless but in fact are constrained by the problem we are modeling
# If the model has i.i.d. errors that are normally distributed, 
# then using the method of least squares allows us to find parameter estimates and use AIC to compare models

# 12.2.1 Fitting nonlinear models with nls()

# Nonlinear models can be fit in R using nls(). The nls() function computes nonlinear least squares
# A basic template is res = nls(formula, data=..., start=c(...), trace=FALSE)
# The formula again looks like response ~ mean, but the mean is specified using ordinary math notations
# For example, the exponential model for the mean could be written y ~ N * exp(-r*(t-t0)), where N, r, and t0 are parameters
# It is often convenient to use a function to return the mean, such as y ~ f(x,beta0,beta1,...)
# That is, a function that specifies the parameter values by name
# The method of nonlinear least squares uses an algorithm that usually needs to start with parameter values that are close to the actual ones
# The argument start=c(...) is where we put the initial guesses for the parameters
# This can be a vector or list using named values, such as start=c(beta0 = 1, betal = 2)
# Finally, the optional argument trace=TRUE can be used if we want to see what is happening during the execution of the algorithm
# This can be useful information if the algorithm does not converge. By default it is FALSE

# The initial parameter guesses are often found by doing some experimental plots
# These can be done quickly using the curve() function with the argument add=TRUE
# When we model with a given function, it helps to have a general understanding of how the parameters change the graph of the function
# For example, the parameters in the exponential model, written f(t|N,r,t0)= N.e^-r(t-t0), may be interpreted by : 
# t0 being the place where we want time to begin counting, 
# N the initial amount at this time, 
# r the rate of decay 
# For this model, the mean of the data decays by 1/e, or roughly 1/3 in 1/r units of time(r*t-t0)

# example : Yellowfin tuna catch rate
# The data set yellowfin (UsingR) contains data on the average number of yellowfin tuna caught per 100 hooks for various years
# Estimate the decline of fish stocks (biomass) since the advent of large-scale commercial fishing

plot(count ~ year, data = yellowfin)             # seem to decline exponentially to some threshold

# Try to fit the model Y = N*(e^-r(t-1952)*(1-d) +d) + epsiloni  ---->  N:Beta0, r:Beta1, t-1952(t0):xi, d:Beta2

f <- function(t, N, r, d) N*(exp(-r*(t-1952))*(1-d) +d)

# We need to find some good starting points for nls()
# The value of N=7 seems about right, as this is the starting value when t=1952
# The value r is a decay rate. It can be estimated by how long it takes for the data to decay by roughly 1/3. We guess about 10, so we start with r=1/10
# Finally, d is the percent of decay, which seems to be 0.6/6 = 0.10

curve(f(x, N=7, r=1/10, d=0.10), add=TRUE)

res.yf <- nls(count ~ f(year, N, r, d), data = yellowfin, start = c(N=7, r=1/10, d=0.1), trace = FALSE)
res.yf
#     N        r        d 
# 6.02018  0.09380  0.05359 -------------------> The value for d estimates that only 5.3% of the initial amount remains

curve(f(x, N=6.02018, r=0.09380, d=0.05359), add = TRUE, lty = 2, lwd = 2)
legend (1980, 6, legend = c("explaratory", "exponential"), lty = 1:2)

tmp = 1952:2000
lines(tmp, predict(res.yf, data.frame(year = tmp)))

# example : Sea urchin growth
# The urchin.growth (UsingR) data set contains growth data of reared sea urchins over time
# Typical growth starts at 0 and progresses to some limiting size

# Some models for growth :
logistic.model <- function(t, Y, k, t0) Y*(1+exp(-k*(t-t0)))^(-1)
Richards.model <- function(t, Y, k, t0, m) Y*(1-exp(-k*(t-t0)))^m

# Which one fits the data better?

plot(jitter(size) ~ jitter(age,3), data = urchin.growth, xlab="age", ylab="size", main="Urchin growth by age")

# Next, we try to fit logistic.model. The parameters can be interpreted from the scatterplot of the data
# The value of Y corresponds to the maximum growth of the urchins, which appears to be around 60
# The value of t0 is where the inflection point of the graph occurs. The inflection point is when the curve stops growing faster
# A guess is that it happens around 2 for the data
# Finally, k is a growth rate around this point. It should correspond to roughly 1 over the time it takes to grow onethird again after the value at t0
# We guess 1 from the data. With these guesses, we do an exploratory graph with curve()

curve(logistic.model(x, Y=60, k=1, t0=2), add=TRUE)

res.lm <- nls(size ~ logistic.model(age, Y, k, t0), start = c(Y=60, k=1, t0=2), data=urchin.growth)
res.lm
#   Y      k     t0 
# 53.903  1.393  1.958 
# residual sum-of-squares: 7299

curve(logistic.model(x, Y=53.903, k=1.393, t0=1.958), add=TRUE)

AIC(res.lm)                  # 1560.963

curve(Richards.model(x, Y=53.903, k=1.393, t0=1.958, m=1), add=TRUE)                     # not a great fit
legend(4, 20, legend=c("logistic growth", "Richards"), lty=1:2)

res.Rm <- nls(size ~ Richards.model(age, Y, k, t0, m), data = urchin.growth, start = c(Y=53, k=1.393, t0=1.958, m=1))
# Error in numericDeriv(form[[3L]], names(ind), env, central = nDcentral) : 
#  Missing value or an infinity produced when evaluating the model

# This is one of the error messages that can occur when the initial guess isn't good or the model doesn't fit well

res.Rm <- nls(size ~ Richards.model(age, Y, k, t0, m), data = urchin.growth, start = c(Y=53, k=.5, t0=0, m=1))
res.Rm
#   Y       k      t0       m 
# 57.2649  0.7843 -0.8587  6.0636 
# residual sum-of-squares: 6922

# Now we have convergence. The residual sum-of-squares, 6,922, is less than the 7,922 for the logistic model
# This is a good thing, but if we add parameters this is often the case

AIC(res.Rm)                  # 1549.703 ; Reduction from the other model ; we would select the Richards model as a better fit by this criteria

# Problems 

# Question 12.1
str(tastesgreat)
res <- glm(enjoyed ~ age + gender, family = binomial, data = tastesgreat)
summary(res)
# Coefficients:
#             Estimate Std. Error z value Pr(>|z|)   
# (Intercept) -8.18443    3.09644  -2.643  0.00821 **
# age          0.16491    0.06519   2.530  0.01142 *  -------------> significant
# genderMale   2.42241    0.95590   2.534  0.01127 *  -------------> significant

# odds-ratio for women : P(enjoyed=1|age) / P(enjoyed=0|age) = e^(-8.18443 + 0.16491*age)           -> exp(-8.18443 + 0.16491*age)
# odds-ratio for men   : P(enjoyed=1|age) / P(enjoyed=0|age) = e^(-8.18443 + 2.42241 + 0.16491*age) -> exp(-8.18443 + 2.42241 + 0.16491*age)

# Question 12.2
str(healthy)
res <- glm(healthy ~ p + g, family = binomial, data = healthy)
stepAIC(res)
# Coefficients:
#               (Intercept)            p  
#                    -6.845        1.827

# odds-ratio = P(healthy=1|p)/P(healthy=0|p) = exp(-6.845 + 1.827*p)

# Question 12.3
str(birthwt)
res <- glm(low ~ age + lwt + factor(smoke) + factor(ht) + factor(ui), family = binomial, data = birthwt)
stepAIC(res)
# Coefficients:
#               (Intercept)          lwt        smoke           ht           ui  
#                   0.72186     -0.01631      0.65300      1.92213      0.89627

# lwt(mother's weight), smoke(smoking status), ht(hypertension), ui(uterine irritability) are flagged as significant

# odds-ratio for a mother who smokes and with ht and ui = P(low infant birth weight = 1|lwt) / P(low infant birth weight = 0|lwt) 
#                                                       = exp(0.72186 + 0.65300 + 1.92213 + 0.89627 - 0.01631*lwt)

# Question 12.4
str(hall.fame)
hfm <- hall.fame$Hall.Fame.Membership != "not a member"
res <- glm(hfm ~ BA + HR + hits + games, family = binomial, data = hall.fame)
stepAIC(res)
# Coefficients:
#               (Intercept)           BA           HR         hits  
#                -21.843762    51.212224     0.004285     0.002495

# the batting average (BA), the number of lifetime home runs (HR), the number of hits (hits) are flagged as significant
# odds-ratio = P(being a member in hall of fame|BA,HR,hits) / P(not being a member in hall of fame|BA,HR,hits) 
#            = exp(-21.843762 + 51.212224*BA + 0.004285*HR + 0.002495*hits)

# Question 12.5
str(esoph)

res.int <- glm(cbind(ncases,ncontrols) ~ agegp + tobgp * alcgp, family = binomial, data = esoph)
stepAIC(res.int)
# Coefficients:
#               (Intercept)      agegp.L      agegp.Q      agegp.C      agegp^4      agegp^5  
#                  -1.19039      3.99663     -1.65741      0.11094      0.07892     -0.26219  
#                   tobgp.L      tobgp.Q      tobgp.C      alcgp.L      alcgp.Q      alcgp.C  
#                   1.11749      0.34516      0.31692      2.53899      0.09376      0.43930

res.noint <- glm(cbind(ncases,ncontrols) ~ agegp + tobgp + alcgp, family = binomial, data = esoph)
stepAIC(res.noint)
# Coefficients:
#               (Intercept)      agegp.L      agegp.Q      agegp.C      agegp^4      agegp^5  
#                  -1.19039      3.99663     -1.65741      0.11094      0.07892     -0.26219  
#                   tobgp.L      tobgp.Q      tobgp.C      alcgp.L      alcgp.Q      alcgp.C  
#                   1.11749      0.34516      0.31692      2.53899      0.09376      0.43930

# no interaction term between alcohol and tobacco is hinted as there is no difference between the models with or without interaction

# Question 12.6
View(Orange)
tree.1 <- Orange[Orange$Tree == 1, ]
res <- glm(circumference ~ age, family = gaussian, data = tree.1)
summary(res)
# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  24.437847   6.543311   3.735   0.0135 *  
# age           0.081477   0.006281  12.973 4.85e-05 ***

# Question 12.7
View(ChickWeight)
chick.1 <- subset(ChickWeight, subset = Chick == 1, select = c("weight", "Time"))
res <- glm(weight ~ Time, family = gaussian, data = chick.1)
summary(res)
# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
# (Intercept)    24.4654     6.7279   3.636  0.00456 ** 
# Time            7.9879     0.5236  15.255 2.97e-08 ***

# Question 12.8
# example(wtloss)
wtloss.fm <- nls(Weight ~ b0 + b1*2^(-Days/th), data = wtloss, start = list(b0=90, b1=95, th=120))
#     b0      b1      th 
#  81.37  102.68  141.91
plot(Weight ~ Days, data = wtloss)
lines(predict(wtloss.fm) ~ Days, data = wtloss)

# The value of b0 is the predicted long-term weight. If we want the predicted amount after 365 days we have
predict(wtloss.fm, newdata=data.frame(Days=365))

# Question 12.9
l =  function(t, b0, b1, k, t0) (b0 + b1*t)*(1 - exp(-k*(t-t0)))
l1 = function(t, b0, k, t0) l(t, b0, 0, k, t0)
res.l = nls(length ~ l(age, b0, b1, k, t0), data = reddrum, start = c(b0 = 32, b1 = 0.25, k = 0.5, t0 = 0))
res.l1 = nls(length ~ l1(age, b0, k, t0), data = reddrum, start = c(b0 = 32, k = 0.5, t0 = 0))
AIC(res.l)                   # 308.6806
AIC(res.l1)                  # 378.1829
# So the more complicated model is better by this criteria

# Question 12.10



