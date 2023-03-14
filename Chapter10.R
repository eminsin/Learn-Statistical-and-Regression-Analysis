# This document belongs to the book "Using R for Introductory Statistics" by John Verzani
# copyright (c) 2004 - Taylor and Francis
#
# In the document, foundations of statistical knowledge is practiced on R
#
# written by Erkam Minsin
#
# last modified: Jan 2023
# first written: Jan 2023

setwd("C:/Users/erkam/R Learning/Using_R_for_Introductory_Statistics_John_Verzani")

## install.packages("UsingR")
library(UsingR)

## update.packages()


# Chapter 10 Linear Regression ---------------------------------------------


# 10.1 The simple linear regression model

# A basic model for this is a simple linear Yi=Beta0+Beta1xi+epsiloni
# The Y variable is called the response variable and the x variable the predictor variable, covariate, or regressor
# Yi depends on three things: that of xi, the function Beta0+ Beta1x, and the value of the random variable epsiloni
# The model says that for a given x, the corresponding value of Y is found by first using the function on x and then adding the random error term epsiloni

# To be able to make statistical inference, we assume that the error terms, epsiloni, are i.i.d. and have a Normal (0, sigma) distribution
# This assumption can be rephrased as an assumption on the randomness of the response variable 
# If the x values are fixed, then the distribution of Yi is normal with mean muy|x=Beta0+Beta1Xi and variance sigma^2 
# This can be expressed as Yi has a Normal(Beta0+Beta1xi, sigma) distribution
# If the x values are random, the model assumes that, conditionally on knowing these random values, the same is true about the distribution of the Yi

# 10.1.1 Model formulas for linear models

# The basic format for a formula is response ~ predictor
# The ~(tilde) is read "is modeled by" and is used to separate the response from the predictor(s)
# The response variable can have regular mathematical expressions applied to it, 
# but for the predictor variables the regular notations +, -, *, /, and ^ have different meanings 
# A + means to add another term to the model, - means to drop a term, more or less coinciding with the symbols' common usage
# But *, /, and ^ are used differently 
# If we want to use regular mathematical notation for the predictor we must insulate the symbols' usage with the I() function, as in I(x^2)

# 10.1.2 Examples of the linear model

# Simple linear regression :
# If (xi, yi) are related by the linear model yi=Beta0+Beta1xi+epsiloni as above, 
# then the model is represented in R by the formula y ~ x 
# The intercept term, Beta0, is implicitly defined

# If for some reason the intercept term is not desired, it can be dropped from the model by including the term -1, as in y ~ x-1

# The mean of an i.i.d. sample :
# In finding confidence intervals or performing a significance test for the mean of an i.i.d. sample, Y1,Y2,...,Yn, 
# we often assumed normality of the population 
# In terms of a statistical model this could be viewed as Yi=mu+epsiloni, where the epsiloni are Normal(0, sigma)
# The model for this in R is y ~ 1 
# As there is no predictor variable, the intercept term is explicitly present

# The paired t-test :
# This test applies when two samples are somehow related and the differences between the two samples is random
# That is, Yi-Xi, is the quantity of interest
# This corresponds to the statistical model yi=xi+epsiloni
# If we assume epsiloni has mean 0, then we can model the mean difference between Y and X by mu, and our model becomes Yi=mu+Xi+epsiloni
# Our significance test with H0: mu1=mu2 turns into a test of mu=0
# The model formula to fit this in R uses an offset, which we won't discuss again, but for reference it would look like y ~ offset(x)

# 10.1.3 Estimating the parameters in simple linear regression

# One goal when modeling is to "fit" the model by estimating the parameters based on the sample
# For the regression model the method of least squares is used
# The method of LS finds values for the Beta's that minimize the squared difference between the actual values, yi, and those predicted by the function f

# We call yhat=Beta0hat+Beta1hatx the prediction line; a value yihat=Beta0hat+Beta1hatxi the predicted value for xi; 
# and the difference between the actual and predicted values, epsiloni=yi-yihat the residual
# The residual sum of squares is denoted RSS

# Quickly put, the regression line is chosen to minimize the RSS; 
# it has slope Beta1hat, intercept Beta0hat, and goes through the point (xbar,ybar)
# Furthermore, the estimate for sigma^2 is RSS/(n-2)

# Our task of inference is to decide how much the regression line can tell us about the underlying true model

# 10.1.4 Using lm() to find the estimates

# lm(formula, data=..., subset=...)

# By default, the lm () function will print out the estimates for the coefficients
# Usually, we store the results of the model in a variable, so that it can subsequently be queried for more information

# example
age <- rep(seq(20,60,by=5), 3)
mhr <- 209-0.7*age + rnorm(length(age),sd=4)     # the random results for each age using the given function f and random errors
plot(mhr~age, main="Age vs maximum heart rate")  # the scatterplot shows that the data lends itself to the linear model

res.mhr <- lm(mhr ~ age)                         # the regression coefficients are found using the 1m() function
res.mhr
# Call:
# lm(formula = mhr ~ age)
# Coefficients:
# (Intercept)        age  
#   212.4097      -0.7378                        # estimated model formula by lm() : 212.4097 - 0.7378*age

abline(res.mhr)                                  # add regression line

212.4097 - 0.7378*39                             # the predicted maximum heart rate for a 39-year-old individual

# Extractor functions for lm()

# summary()   : returns summary information about the regression
# plot()      : makes diagnostic plots
# coef()      : returns the coefficients
# residuals() : returns the residuals (can be abbreviated resid())
# fitted()    : returns the residuals
# deviance()  : returns RSS
# predict()   : performs predictions
# anova()     : finds various sums of squares
# AIC()       : is used for model selection

# example
sum(resid(res.mhr)^2) / (length(age)-2)          # the estimate for variance (sigma^2) : RSS/(n-2)
deviance(res.mhr) / (length(age)-2)              # the estimate for variance (sigma^2) : RSS/(n-2) ; deviance() finds the RSS directly

# Problems 10.1.5

# Question 10.1

#1
library(MASS)
f <- MPG.highway ~ Horsepower                    # model formula
plot(f, data = Cars93)                           # we expect a negative correlation. A scatterplot confirms this
res <- lm(f, data = Cars93)
res                                              # MPG.highway = 38.14988 - 0.06302*Horsepower
abline(res)

predict1 <- 38.14988 - 0.06302*225                                   # 23.97038
predict2 <- predict(res, newdata=data.frame(Horsepower=225))         # 23.97066

#2
f <- MPG.highway ~ Weight
plot(f, data = Cars93)                           # negative correlation
res <- lm(f, data = Cars93)
res                                              # MPG.highway =  51.601365 - 0.007327*Weight
abline(res)

predict1 <- 51.601365 - 0.007327*c("MINI"=2524, "HUMMER"=6400)                           # MINI : 33.108017 HUMMER : 4.708565
predict2 <- predict(res, newdata=data.frame(Weight=c("MINI"=2524, "HUMMER"=6400)))       # MINI : 33.107868 HUMMER : 4.708186

# The prediction for the MINI Cooper may be close, 
# but there is no reason to expect the prediction for the HUMMER to be close, as the value of the predictor is outside the range of the data.

#3
# The variable Min.Price records the value of the stripped-down version of the car, and Max.Price records the fully equipped version 
# We'd expect that Max.Price would roughly be a fixed amount more than Min.Price, 
# as the differences-the cost of leather seats, a bigger engine, perhaps- are roughly the same for each car
# That is why we might expect the slope to be around 1

f <- Max.Price ~ Min.Price
plot(f, data = Cars93)
res <- lm(f, data = Cars93)
abline(res)
res                                              # Max.Price = 2.314 + 1.144*Min.Price

# The slope of 1.144 indicates that perhaps add-ons for more expensive cars cost more, but in this case it appears to be due to the one large outlier
# A more reliable formula using robust regression can be obtained with the following :

res <- rlm(f, data = Cars93)                     # Max.Price = 3.609198 + 1.028727*Min.Price ; the slope is closer to 1

#4
# A scatterplot matrix may show additional linear relationships
# These are produced with the pairs() command, as in pairs(Cars93)
# Doing so directly produces too many scatterplots
# We can trim down the size of the data frame then plot again
# Doing so using only the nonfactors can be done as follows :

cars <- Cars93[,sapply(Cars93, function(x) !is.factor(x))]
pairs(cars)

# Question 10.2
f <- attendance ~ wins
plot(f, data = MLBattend)
res <- lm(f, data = MLBattend)
abline(res)
res                                              # attendance = -378164 + 27345*wins

diff(as.numeric(predict(res, newdata = data.frame(wins=c("lastyear"=80,"thisyear"=90)))))

# Question 10.3
height <- data.frame(c(39,30,32,34,35,36,36,30),c(71,63,63,67,68,68,70,64))
colnames(height) <- c("Age2", "Adult")
height

# People often think that 2-year-old height is half of adult height, we will compare it to a linear model

plot(Adult ~ Age2, data = height)
res <- lm(Adult ~ Age2, data = height)
abline(res)
res                                              # Adult = 35.1786 + 0.9286*Age2

predict(res, newdata = data.frame(Age2=33))      # Adult = 65.82143

# Question 10.4
f <- child ~ parent
plot(jitter(child, factor=1000) ~ jitter(parent, factor = 1000), data = galton)
res <- lm(f, data = galton)
abline(res)
res                                              # child = 23.9415 + 0.6463*parent

# the value of Beta1hat is 0.6463 ; this shows us the height change in the child for a 1 unit change in the parent height

# Question 10.5

# 10.2 Statistical inference for simple linear regression

# To assess whether the simple regression model is appropriate for the data we use a graphical approach

# 10.2.1 Testing the model assumptions

# The simple linear regression model places assumptions on the data set that we should verify before proceeding with any statistical inference
# The linear model should be appropriate for the mean value of the yi, and the error distribution should be normally distributed and independent
# The biggest key to the appropriateness of the model is found in the residuals 
# The residuals are not an i.i.d. sample, as they sum to 0 and they do not have the same variance
# The standardized residuals rescale the residuals to have unit variance

# Assessing the linear model for the mean :

# A scatterplot of the data with the regression line can show quickly whether the linear model seems appropriate for the data
# If the general trend is not linear, either a transformation or a different model is called for
x <- rep(1:10,4)
y <- rnorm(40, mean = 5*sin(x), sd=1)
plot(y ~ x, main = "scatterplot")
abline(lm(y~x))
# When there is more than one predictor variable, a scatterplot will not be as useful

# A residual plot can also show whether the linear model is appropriate and can be made with more than one predictor
# As well, it can detect small deviations from the model that may not show up in a scatterplot
x <- rep(1:10,4)
y <- rnorm(40,mean = x + .05*sin(x),sd=.01)      # small trend
res <- lm(y~x)
plot(fitted(res),resid(res), main = "not linear")

# Assessing normality of the residuals :

# The residuals are used to assess whether the error terms in the model are normally distributed
# In addition to normality, an assumption of the model is also that the error terms have a common variance
# A residual plot can show whether this is the case. When it is, the residuals show scatter about a horizontal line
x <- rep(1:10,4)
y <- rnorm(40, mean = 1 + 1/2*x, sd = x/10)
res <- lm(y ~ x)
plot(fitted(res),resid(res), main = "spread variance")

# In some data sets, there is a lack of independence in the residuals
# For example, the errors may accumulate. A lag plot may be able to show this 
# For an independent sequence, the lag plot should be scattered, whereas many dependent sequences will show some pattern
x <- rep(1:10,4)
epsilon <- rnorm(40,mean=0,sd=1)
y <- 1 + 2*x + cumsum(epsilon)                   # cumsum() correlates errors
res <- lm(y ~ x)
tmp <- resid(res)
n <- length(tmp)
plot(tmp[-n],tmp[-1], main="correlated errors")  # lag plot

# Influential points :

# the regression line can be greatly influenced by a single observation that is far from the trend set by the data
# The difference in slopes between the regression line with all the data and the regression line with the ith point missing will mostly be small, 
# except for influential points !!!
# The Cook's distance is based on the difference of the predicted values of yi for a given xi :
# when the point (xi, yi) is and isn't included in the calculation of the regression coefficients
# The predicted amounts are used for comparison, as comparing slopes isn't applicable for multivariate models
# The Cook's distance is computed by the extractor function cooks.distance()
res <- lm(CO2 ~ perCapita, data = emissions)
plot(CO2 ~ perCapita, data = emissions,
     cex = 10*sqrt(cooks.distance(res)),
     main = expression(paste("bubble plot of", CO[2], "emissions by per capita GDP")))   # make subscript on C02 in title

# 10.2.2 Statistical inferences

# If the linear model seems appropriate for the data, statistical inference is possible
# What is needed is an understanding of the sampling distribution of the estimators	
# The estimators are random but not arbitrary. Both Beta0hat and Beta1hat are normally distributed, with respective means Beta0 and Beta1
# the following statistics have a t-distribution with n-2 df: Beta0hat-Beta0 / SE(Beta0hat) or  Beta1hat-Beta1 / SE(Beta1hat)

# Marginal t-tests :
# We can find confidence intervals and construct significance tests from the T statistics
# For example, a significance test for H0:Beta1=b, Ha:Beta1!=b is carried out with the test statistic T = Beta1hat-b / SE(Beta1hat)
# Under H0, T has the t-distribution with n-2 degrees of freedom
# When the null hypothesis is Beta1=0 or Beta0=0 we call these marginal t-tests, as they test whether the parameter is necessary for the model

# The F-test :
# An alternate test for the null hypothesis Beta1=0 can be done using a different but related approach that generalizes to the multiple-regression problem
# The total variation in the y values about the mean is total sum of squares(SST)
# SST is the sum of residual(error) sum of squares(RSS) and regression sum of squares(SSReg) -> SST=RSS+SSReg
# RSS is the total variation in the y values about the fitted model(estimated y values) 
# SSReg is the total variation for the fitted model about the mean
# For each term, a number-called the degrees of freedom - is assigned that depends on the sample size and the number of estimated values in the term
# For the SST there are n data points and one estimated value, ybar, leaving n-1 degrees of freedom
# For RSS there are again n data points but two estimated values, Beta0hat and Beta1hat, so n-2 degrees of freedom
# This leaves 1 degree of freedom for the SSReg, as the degrees of freedom are additive in this case
# When a sum of squares is divided by its degrees of freedom it is referred to as a mean sum of squares
# If Beta1hat is close to 0, and the fitted model and the mean are similar in size, so we would have SST is nearly equal to RSS, where SSReg would be small
# Whereas, if Beta1hat is not close to 0, then SSReg is not small. So, SSReg would be a reasonable test statistic for the hypothesis H0: Beta1=0
# What do small and big mean? As usual, we need to scale the value by the appropriate factor (F statistic)
# The F statistic is equal to (mean SSReg)/(mean RSS) ; F = (SSReg/1) / (RSS/n-2) = SSReg / sigmahat^2
# Under the null hypothesis H0: Beta1=0, the sampling distribution of F is known to be the F-distribution with 1 and n-2 degrees of freedom
# This allows us to make the following significance test : F-test for Beta1=0
# A significance test for the hypotheses : H0: Beta1=0, Ha: Beta1!=0
# Larger values of F are more extreme, so the p-value is given by P(F >= observed value|H0)
# The F-statistic can be rewritten as F = (Beta1hat/SE(Beta1hat))^2 : 
# Under the assumption Beta1=0, this is the square of one of the t-distributed random variables
# For simple linear regression the two tests of H0:Beta1=0, the marginal t-test and the F-test, are equivalent
# However, we will see that with more predictors, the two tests are different

# R-square - the coefficient of determination :
# If the regression line fits the data well, then the residual sum of squares, RSS, will be small
# If there is a lot of scatter about the regression line, then RSS will be big
# To quantify this, we can divide by the total sum of squares, leading to the definition of the coefficient of determination:
# R-square = 1-(RSS/SST) = SSReg/SST
# This is close to 1 when the linear regression fit is good and close to a when it is not
# When the simple linear regression model is appropriate this value is interpreted as the proportion of the total response variation explained by the Reg
# That is, R-square*100% of the variation is explained by the regression line 
# When R-square is close to 1, most of the variation is explained by the regression line, and when R-square is close to 0, not much is
# This interpretation is similar to that given for the Pearson correlation coefficient, r
# This is no coincidence: for the simple linear regression model r-square=R-square
# The adjusted R-square:
# divides the sums of squares by their degrees of freedom 
# For the simple regression model, these are n-2 for RSS and n-1 for SST 
# This is done to penalize models that get better values of R-square by using more predictors
# This is of interest when multiple predictors are used

# 10.2.3 Using lm() to find values for a regression model

# Confidence Intervals
age <- rep(seq(20,60,by=5), 3)
mhr <- 209-0.7*age + rnorm(length(age),sd=4)
res <- lm(mhr ~ age)
n <- length(age)
beta0hat <- coef(res)[1]
sigmahat <- sqrt(sum(resid(res)^2)/(n-2))
SE <- sigmahat*sqrt(sum(age^2)/(n*sum((age-mean(age))^2)))
tcritical <- qt(1-0.05/2, df = n-2)
c(beta0hat-tcritical*SE, beta0hat+tcritical*SE)

summary(res)

# by reading the summary, 95% confidence interval for Beta1 may be more easily found than the one for Beta0 above

beta1hat = -0.7595                               # read from the summary
SE = 0.0561                                      # read from the summary
tcritical <- qt(1-0.05/2, df = 25)
c(beta1hat-tcritical*SE, beta1hat+tcritical*SE)

# Significance test
# For each coefficient a marginal t-test is performed
# This is a two-sided hypothesis test of the null hypothesis that Betai=0 against the alternative that Betai!=0
# We see in this case (in the summary) that both are rejected with very low p-values
# Other t-tests are possible. For example, we can test the null hypothesis that the slope is -1 with the commands :
T.obs <- (betahat1-(-1))/SE
T.obs                        # 4.287
2*pt(-4.287,df=n-2)          # or use lower.tail=FALSE with 4.287 
# This is a small p-value, indicating that the model with slope -1 is unlikely to have produced this data or anything more extreme than it

# Finding sigmahat^2 or R-square
# The value of R-square = cor(age ,mhr)^2 is given along with an adjusted value(adjusted R-square)

# F-test for Beta1=0
(-0.7595/0.0561)^2           # 183
# The significance test H0:Beta1=0 with two-sided alternative is performed and again returns a tiny p-value

# The sum of squares to compute F are also given as the output of the anova() extractor function
anova(res)
# These values in the column headed Sum Sq are SSReg and RSS. The total sum of squares, SST, would be the sum of the two
# the column headed Mean Sq are the mean sum squares of the regression line and residuals : SSReg/Df=1 and RSS/Df=n-2=25

# Predicting the response with predict()
# predict (res, newdata=data.frame(variablename=...), interval=..., level=...)
predict(res, newdata = data.frame(age=42))       # this finds the predicted maximum heart rate for a 42-year-old

# Prediction Intervals
# The value of yhat can be used to predict two different things: 
#   -the value of a single estimate of y for a given x or 
#   -the average value of many values of y for a given x
# Statistical inference about the predicted value of y based on the sample is done with a prediction interval
# As y is not a parameter, we don't call this a confidence interval
# The form of the prediction interval is similar to that of a confidence interval
# The prediction interval holds for all x simultaneously
# It is often plotted using two lines on the scatterplot to show the upper and lower limits
# The predict () function will return the lower and upper endpoints for each value of the predictor 
# We specify interval="prediction" (which can be shortened) and a confidence level with level= (The default is 0.95)

pred.res <- predict(res, interval="prediction")  # or shorlty pred.res <- predict(res, int="pred")
pred.res                                         

# A matrix is returned with columns giving the data we want ; colnames(pred.res) = "fit" , "lwr" , "upr" 
# We cannot access these with the data frame notation pred.res$lwr, as the return value is not a data frame
# Rather we can access the columns by name, like pred. res [,"lwr"] or by column number, as in

pred.res[,"lwr"]             # or pred.res[,2]

# Now we want to plot both the lower and upper limits
# As the age variable is not sorted, simply plotting will make a real mess
# To remedy this, we specify the values of the age variable for which we make a prediction
age.sort <- sort(unique(age))
pred.res <- predict(res, newdata = data.frame(age = age.sort), interval = "prediction")
pred.res[,2]
plot(mhr~age)
lines(age.sort, pred.res[,2], lty = 2)           # lower curve
lines(age.sort, pred.res[,3], lty = 2)           # upper curve

# There is a slight curve in the lines drawn
# This implies that estimates near the value (xhat,yhat) have a smaller variance
# This is expected: there is generally more data near this value, so the variances should be smaller

# Confidence intervals for mu_y|x
# If we had so much data (large n) that the estimates for the Beta's have small variance, 
#   -we would not have much uncertainty in predicting the mean amount, 
#   -but we would still have uncertainty in predicting a single deviation from the mean due to the error term in the model
# The values for this confidence interval are also returned by predict()
# In this case, we use the argument interval="confidence"

# Problems 10.2.4

# Question 10.6
price <- c(300,250,400,550,317,389,425,289,389)
bedrooms <- c(3,3,4,5,4,3,6,3,4)
plot(price~bedrooms)
abline(lm(price~bedrooms))
# the hypothesis test is between H0:Beta1=60000 , Ha:Beta1>60000
# T-test is applicable for the significance test
summary(lm(price~bedrooms))   # Estimate for a bedroom price=60.71 ; Standard Error of the sampling dist of the variable bedrooms=23.00
T <- (60.71-60)/23
pt(T, df=length(bedrooms)-1 , lower.tail=FALSE)  # p-value=0.4880649 ; not significant, fail to reject that an extra bedroom is 60K

# Question 10.7
beers <- c(5,2,9,8,3,7,3,5,3,0)
BAL <- c(0.10,0.03,0.19,0.12,0.04,0.095,0.07,0.06,0.02,0.05)
plot(BAL~beers)
res <- lm(BAL~beers)
abline(res)
beers.sort <- sort(unique(beers))
pred.res <- predict(res, newdata = data.frame(beers=beers.sort), interval = "prediction", level = 0.95)
lines(beers.sort, pred.res[,2], lty = 2)
lines(beers.sort, pred.res[,3], lty = 2)
# the hypothesis test is between H0:Beta1=0.02 , Ha:Beta1<0.02
# T-test is applicable for the significance test
summary(res)                 # Estimate = 0.015138 ; SE = 0.003411
T <- (0.015138-0.02)/0.003411
pt(T, df = length(beers)-1, lower.tail = TRUE)   # p-value=0.09389507 ; not significant ; fail to reject that one beer raises BAL by 0.02%

# Question 10.8
# it is already done in the summary results
summary(res)
#             Estimate   Std. Error t value Pr(>|t|)                 # not significant ; fail to reject H0: Beta0=0
# (Intercept) 0.009379   0.017888   0.524   0.61426 

# Question 10.9
# the hypothesis test is between H0: Beta1 = -9.8 C/km (-0.00534 F/feet) , Ha: Beta1 != -9.8 C/km (-0.00534 F/feet)
# T-test is applicable for the significance test 
elevation <- c(600,1000,1250,1600,1800,2100,2500,2900)
Temp <- c(56,54,56,50,47,49,47,45)
plot(Temp~elevation)
res <- lm(Temp~elevation)
abline(res)
summary(res)
#              Estimate   Std. Error t value Pr(>|t|)
# elevation   -0.0051146  0.0009214  -5.551  0.00144
T <- (-0.0051146-(-0.00534))/0.0009214
T
2*pt(T, df = length(elevation), lower.tail = FALSE)                  # two-sided; p-value=0.8129023 ; not significant

# Question 10.10

# Linearity Check 
year <- seq(1952,1962)
pop <- c(724,176,920,1392,1392,1448,1212,1672,2068,1980,2116)
plot(pop~year)
res <- lm(pop~year)
abline(res)                                                          # looks linear

# Prediction
round(predict(res, newdata=data.frame(year=1963)))                   # predicted pop : 2355
round(predict(res, newdata=data.frame(year=2004)))                   # predicted pop : 9064 

# Residual Analysis
#   1) Normality and Homoscedasticity Check
#   2) Correlation Check

#1
plot(year, resid(res), main="Normality and Homoscedasticity Check")  #  Normal(0,sigma) : Yes (normally distributed and homoscedastic around 0)
#2
tmp <- resid(res)
n <- length(tmp)
plot(tmp[-n], tmp[-1], main = "Correlation Check")                   # No correlation (LINEARLY independent)

# if there is no pattern in the "Normality and Homoscedasticity" residual plot, most probably, there is also no correlation between residuals 
 
# Conclusion : Thus, I would use this model for predicting population values for both years as the model fits the data

# Question 10.11
# Linearity Check
f <- y2000~y1970
plot(f, data = homedata)
res <- lm(f, data = homedata)
abline(res)                                                          # looks linear

# Prediction
round(predict(res, newdata = data.frame(y1970=80000)))               # $ 316633

# Residual Analysis
plot(resid(res))                                                     # simple plot
#  The simple plot of the residuals shows values that are scattered around 0, with nothing unusual
plot(homedata$y1970, resid(res), main = "Normality and Homoscedasticity Check")          # or plot(fitted(res), resid(res))
# However, the second plot using the 1970 values on the x-axis instead of the index, shows that the variance increases with the price of the house
# Normality(0,sigma) : No (heteroscedastic around 0)
# Is there any kind of pattern in the Normality and Homoscedasticity Check? No
tmp <- resid(res)
n<- length(tmp)
plot(tmp[-n], tmp[-1], main = "Correlation Check")                   # no correlation (LINEARLY independent)

# Conclusion : the model is not linear because of heteroscedasticity, thus not appropriate for the data

# Question 10.12
# Linearity Check
plot(Deflection~Load, data = deflection)
res <- lm(Deflection~Load, data = deflection)
abline(res)                                                          # looks perfectly linear

# Residual Analysis
plot(deflection$Load, residuals(res), main = "Normality and Homoscedasticity Check")            
# Normal(0,sigma) : No (heteroscedastic around 0)
# Is there any kind of pattern in the Normality and Homoscedasticity Check?  Yes, there is a pattern
tmp <- resid(res)
n <- length(tmp)
plot(tmp[-n],tmp[-1], main="Correlation Check")                      # Correlated (LINEARLY not independent)

# Conclusion : the model is not linear because of heteroscedasticity and correlation, thus not appropriate for the data

# Question 10.13
# Linearity Check
plot(lab.defect~field.defect, data = alaska.pipeline)
res <- lm(lab.defect~field.defect, data = alaska.pipeline)
abline(res)                                                          # looks linear

# Coefficients
res                                                                  # Intercept : -1.967  field.defect : 1.223

# Residual Analysis
plot(alaska.pipeline$field.defect, residuals(res), main = "Normality and Homoscedasticity Check")                   
# Normal(0,sigma) : No (heteroscedastic around 0)
# Is there any kind of pattern in the Normality and Homoscedasticity Check? No
tmp <- resid(res)
n <- length(tmp)
plot(tmp[-n], tmp[-1], main = "Correlation Check")                    # no correlation (LINEARLY independent)

# Conclusion : the model is not linear because of heteroscedasticity, thus not appropriate for the data

# Question 10.14
attach(best.times)
by.dist <- split(best.times, as.factor(Dist))
detach(best.times)
# We split the records by distance, allowing us to compare the factors for several distances
# This returns a list of data frames, one for each distance
# We can plot the times in the 800-meter run :
plot(Time~age, data = by.dist[["800"]])
# It is actually better to apply scale() first, so that we can compare times
# Through age 70, a linear regression model seems to fit. It can be found with :
# lm(Time~age, data = by.dist[["800"]], subset = age<70) ; to see the difference
lm(scale(Time)~age, data = by.dist[["800"]], subset = age<70)
# Coefficients:
# (Intercept)          age  
#    -1.29335      0.01359

# 100-meter
plot(Time~age, data = by.dist[["100"]])
lm(scale(Time)~age, data = by.dist[["100"]], subset = age<70)
# Coefficients:
# (Intercept)          age  
#   -0.972389     0.009363

# 400-meter
plot(Time~age, data = by.dist[["400"]])
lm(scale(Time)~age, data = by.dist[["400"]], subset = age<70)
# Coefficients:
# (Intercept)          age  
#    -1.39511      0.01558

# 10000-meter
plot(Time~age, data = by.dist[["10000"]])
lm(scale(Time)~age, data = by.dist[["10000"]], subset = age<70)
# Coefficients:
# (Intercept)          age  
#    -1.62029      0.02008

# Slopes are not similar, it increases by meters run

# Question 10.15
# Linearity Check
plot(child~parent, data = galton)
res <- lm(child~parent, data = galton)
abline(res)

# Residual Analysis
plot(galton$parent, resid(res), main = "Normality and Homoscedasticity Check")
# Normal(0,sigma) : Yes (normal and homoscedastic around 0)
# Is there any kind of pattern in the Normality and Homoscedasticity Check? No
tmp <- resid(res)
n <- length(tmp)
plot(tmp[-n], tmp[-1], main = "Correlation Check")                   # BUT correlated (LINEARLY not independent)

# Conclusion : the model is not linear because of correlation in residuals, not appropriate for the data

summary(res)
# Coefficients:
#             Estimate Std. Error
# parent       0.64629    0.04114

# the hypothesis is between H0: Beta1=1 , Ha: Beta1!=1
T <- (1-0.64629)/0.04114
2*pt(T, df = length(res)-1, lower.tail = FALSE)  # p-value = 3.271441e-06 ; extremely significant ; reject H0

# Question 10.16
age <- rep(seq(20,60,by=5), 3)
mhr <- 209-0.7*age + rnorm(length(age),sd=4)
plot(mhr~age) 
res <- lm(mhr~age)
abline(res)
age.sort <- sort(unique(age))
pred.res.conf <- predict(res, newdata = data.frame(age=age.sort), interval = "confidence", level = 0.95)
lines(age.sort, pred.res.conf[,2], lwd = 2)
lines(age.sort, pred.res.conf[,3], lwd = 2)
pred.res.pred <- predict(res, newdata = data.frame(age=age.sort), interval = "prediction", level = 0.95)
lines(age.sort, pred.res.pred[,2], lty = 2)
lines(age.sort, pred.res.pred[,3], lty = 2)

# Question 10.17
# Linearity Check
plot(log(lab.defect)~log(field.defect), data = alaska.pipeline)
res <- lm(log(lab.defect)~log(field.defect), data = alaska.pipeline)
abline(res)                                                          # looks linear

# Residual Analysis
plot(log(alaska.pipeline$field.defect), residuals(res), main = "Normality and Homoscedasticity Check")                   
# Normal(0,sigma) : Yes (normal and homoscedastic around 0)
# Is there any kind of pattern (correlation) in the Normality and Homoscedasticity Check? 
tmp <- resid(res)
n <- length(tmp)
plot(tmp[-n], tmp[-1], main = "Correlation Check")                    # no correlation(LINEARLY independent)

# Conclusion : the model is linear , thus appropriate for the data
# We resolved the heteroscedasticity problem (not equal variance for residual distribution) by log-transformation of each variable

# Question 10.18
# Simulation of Yi=1+2xi+epsiloni 
res <- matrix(0, nrow = 200, ncol = 2)
for(i in 1:200) {
  x <- rep(1:10,4)
  y <- rnorm(40, mean=1+2*x, sd=3)
  res[i,] <- coef(lm(y~x))
}
plot(res[,1], res[,2])                           # looks linear

cor(res[,1], res[,2])                            # -0.9043492 : close to -1 ; inversely correlated (Beta1 decrease when Beta0 on increse)

# Question 10.19
install.packages("ellipse")
library(ellipse)
res <- lm(Deflection~Load, data = deflection)
plot(ellipse(res), type = "l")

# 10.3 Multiple linear regression

# Multiple linear regression allows for more than one regressor to predict the value of Y
# Lots of possibilities exist
# These regressors may be separate variables, products of separate variables, powers of the same variable, or functions of the same variable
# In the next chapter, we will consider regressors that are not numeric but categorical
# We see, though, that much of the background for the simple linear regression model carries over to the multiple regression model
# Let Y be a response variable and let x1, x2,...,xp be p variables that we will use for predictors
# For each variable we have n values (i=1,...,n). The multiple regression model we discuss here is Yi=Beta0+Beta1x1i+…+ BetapXpi+epsiloni
# There are p+1 parameters in the model labeled Beta0, Beta1,...,Betap
# They appear in a linear manner, just like a slope or intercept in the equation of a line
# The xi's are predictor variables, or covariates. They may be random; they may be related, such as powers of each other; or they may be correlated 
# As before, it is assumed that the epsiloni values are an i.i.d. sample from a normal distribution with mean 0 and unknown variance sigma^2 
# In terms of the Y variable, the values Yi are an independent sample from a normal distribution with 
# mean Beta0+Beta1x1i+ ... + Betapxpi and common variance sigma^2 
# If the x variables are random, this is true after conditioning on their values

# Examples

# Multiple Linear Model :
# wt = Beta0 + Beta1*gestation + Beta2*m.age + Beta3*m.height + Beta4*m.weight + Beta5*f.age + Beta6*f.height + Beta7*f.weight + epsiloni

# Polynomial regression : (quadratic model)
# yi = Beta0 + Beta1xi + Beta2xi^2 + epsiloni -------> still a linear model

# An initial model might be to fit a linear model with all the predictors in the data
# However, we must note that other restricted models (using some of the predictors) might be appropriate

# 10.3.2 Fitting the multiple regression model using lm()

# The method of least squares is used to estimate the parameters in the multiple regression model as well
# we use + to add terms to a model, - to drop terms, and I() to insulate terms so that the usual math notations apply
# lm(formula, data=..., subset=...)
x = 1:10 ; y = rchisq(10,3) ; z = 11+x+y+rnorm(10)
lm(z ~ x+y)

# example
res.lm = lm(wt ~ gestation + age + ht + wt1 + dage + dht + dwt , 
            data = babies, subset= gestation < 350 & age < 99 & ht < 99 & wt1 < 999 & dage < 99 & dht < 99 & dwt < 999)
res.lm
plot(fitted(res.lm), resid(res.lm))              # Normal(0,sigma) -> Yes : Normal and Homoscedastic around 0
tmp <- resid(res.lm)
n <- length(res.lm)
plot(tmp[-n], tmp[-1])                           # Correlated : LINEARLY not independent

# The subset= argument is very useful, though repeated uses may make us wish that we could use it just once prior to modeling 
# In this case the subset() function is available

# Using update() with model formulas :
# updata(model.object, formula=.~.+new.terms)
# updata(model.object, formula=.~.-old.terms)

# example
init.h  <- c(600,700,800,950,1100,1300,1500)
h.d     <- c(253,337,395,451,495,534,573)
res.lm  <- lm(h.d ~ init.h)
res.lm2 <- update(res.lm, .~. + I(init.h^2))
res.lm3 <- update(res.lm2, .~. + I(init.h^3))

polynomial <- function(x, coefs) {
  tot = 0
  for(i in 1:length(coefs))  tot = tot + coefs[i]*x^(i-1)
  tot
}
plot(h.d ~ init.h)
curve(polynomial(x,coef(res.lm)), add = TRUE, lty = 1) 
curve(polynomial(x,coef(res.lm2)), add = TRUE, lty = 2) 
curve(polynomial(x,coef(res.lm3)), add = TRUE, lty = 3) 
legend(1200,400, legend = c("linear", "quadratic", "cubic"), lty = 1:3)

# 10.3.3 Interpreting the regression parameters

# In many cases, interpretation in simple regression is straightforward
# Changes in the predictor variable correspond to changes in the response variable in a linear manner: 
# a unit change in the predictor corresponds to a Beta1hat change in the response
# However, in multiple regression this picture may not be applicable, as we may not be able to change just a single variable
# If the variables are correlated then the sign of the coefficients can change, leading to a different interpretation
# The language often used is that we "control" the other variables while seeking a primary predictor variable

# example
# "Height matters for career success" said Timothy Judge, a UF management professor,
# Judge's study, which controlled for gender, weight, and age, found that mere inches cost thousands of dollars,
# Each inch in height amounted to about $789 more a year in pay, the study found.

# pay = Beta0 + Beta1*height + Beta2*gender + Beta3*weight + Beta4*age + epsilon

# The authors interpret this to mean that each extra inch of height corresponds to a $789 increase in expected pay
# However, actually, height and weight, or age and height might be correlated
# Thus, we must "control" the other variables as well

# 10.3.4 Statistical inferences

# If the model is correct, statistical inference can be made about the coefficients
# In general, the estimators for a linear model are unbiased and normally distributed,
# from this, t-tests and confidence intervals can be constructed for the estimators

summary(res.lm2)
# Coefficients:
#                Estimate Std. Error t value Pr(>|t|)   
# (Intercept) -2.402e+02  6.899e+01  -3.481  0.02532 * 
# init.h       1.052e+00  1.407e-01   7.478  0.00171 **
# I(init.h^2) -3.437e-04  6.678e-05  -5.147  0.00676 **

alpha = 0.05
tcritical = qt(1-alpha/2, df = 4)                # n=7 , p=2 , df=n-(p+1)
c(1.05-tcritical*0.141, 1.05+tcritical*0.141)    # [0.66 , 1.44]
 
# 10.3.5 Model selection

# There are many criteria for selecting a model

# Partial F-test :
# Consider these two nested models for Yi:
# Yi=Beta0+Beta1x1i+...+Betakxki+epsiloni
# Yi=Beta0+Beta1x1i+...+Betakxki+Betak+1x(k+1)i+...+BetapXpi+epsiloni
# The first model has k+1 parameters, and the second has p+1 with p>k (not counting sigma)
# For the model with p predictors, RSS(p) can only be less than RSS(k) for the model with k predictors
# Call the difference the "extra sum of squares"
# If the new parameters are not really important, then there should be little difference between the sums of squares when computed with or without the new parameters
# If they are important, then there should be a big difference
# To measure big or small, we can divide by the residual sum of squares for the full model :
# RSS(k) - RSS(p) / RSS(p) should measure the influence of the extra parameters
# If we divide the extra sum of squares by p-k and the residual sum of squares by n-(p+1) (the respective degrees of freedom), 
# then the statistic becomes F = (RSS(k) - RSS(p))/(p-k) / RSS(p)/(n-(p+1)) , where RSS(p)/(n-(p+1)) = sigmahat^2 (SE of full model)
# Under the null hypothesis that the extra Beta's are 0 (Betak+1=...=Betap=0), and the epsiloni are i.i.d. with a Normal (0, sigma^2) dist, 
# F will have the F-distribution with p-k and n-(p+1) degrees of freedom
# H0: Betak+1=Betak+2=...=Betap=0 and Ha: at least one Betaj!=0 for j>k
# Large values of F are in the direction of the alternative
#  This test is called the partial F-test
# The anova() function will perform the partial F-test 
# If res.1m1 and res.lm2 are the return values of two nested models : anova(res.lm1, res.lm2) will perform the test and produce an anova table

# example
anova(res.lm2, res.lm3)
# Analysis of Variance Table
# Model 1: h.d ~ init.h + I(init.h^2)
# Model 2: h.d ~ init.h + I(init.h^2) + I(init.h^3)
#   Res.Df      RSS  Df  Sum of Sq       F    Pr(>F)   
# 1      4   744.08                              
# 2      3    48.25   1     695.82   43.26   0.00715 **

# The F-test is significant (p=0.0072), indicating that the null hypothesis(Beta3=0) does not describe the data well
# This suggests that the underlying relationship is cubic and not quadratic

# The Akaike information criterion (AIC) :
# In the partial F-test, the trade-off between adding more parameters to improve the model fit and making a more complex model appears in the n-(p+1) divisor
# Another common criterion with this trade-off is Akaike's information criterion (AIC)
# The AIC is computed in R with the AIC() extractor function
# Models with lower AICs are preferred
# An advantage to the AIC is that it can be used to compare models that are not nested. This is a restriction of the partial F-test
# The extractor function AIC() will compute the value for a given model
# The convenient stepAIC() function from the MASS library will step through the submodels and do the comparisons for us

# example
pairs(stud.recs)             # shows strong correlations among the variables
res.lm <- lm(num.grade ~ . , data = stud.recs)
summary(res.lm)
library(MASS)
stepAIC(res.lm)

# Problems 10.3.6

# Question 10.20
res.lm = lm(wt ~ gestation + age + ht + wt1 + dage + dht + dwt , 
            data = babies, subset= gestation < 350 & age < 99 & ht < 99 & wt1 < 999 & dage < 99 & dht < 99 & dwt < 999)

summary(res.lm)                                  # formula = wt ~ gestation + ht + dwt

# So the coefficients for gestation, ht, and dwt are flagged
# indicating, gestation, ht, and dwt are the predictors of the model
# We reject the null hypotesis, their Betas are different than zero

# The AIC choice is found with stepAIC()
stepAIC(res.lm)                                  # formula = wt ~ gestation + age + ht + dwt

# Question 10.21
init.h  <- c(600,700,800,950,1100,1300,1500)
h.d     <- c(253,337,395,451,495,534,573)
res.lm  <- lm(h.d ~ init.h)
res.lm2 <- update(res.lm, .~. + I(init.h^2))
res.lm3 <- update(res.lm2, .~. + I(init.h^3))
res.lm4 <- update(res.lm3, .~. + I(init.h^4))

# Partial F-test comparison
anova(res.lm3, res.lm4)                          # the new coefficient is not significant (p-value=0.142) ; the H0 describe the data well 

# Question 10.22
res.lm <- lm(Volume ~ Girth + Height, data = trees)
summary(res.lm)
# the model fits the data well :
#   - all coefficients are flagged ; significant to reject H0
#   - Multiple R-squared:0.948 , Adjusted R-squared:0.9442
#   - F-statistic:   255 on 2 and 28 DF,  p-value: < 2.2e-16 ; significant to reject H0

# Question 10.23
res.lm <- lm(attendance ~ year + runs.scored + wins + games.behind , data = MLBattend)
summary(res.lm)              # years , runs.scored and games.behind are flagged as significant

pairs(MLBattend[,c("year", "runs.scored", "wins", "games.behind")])  
# runs.scored and wins seems linearly connected
# there seems to be no pattern between other variables
# thus the model seems valid when we eliminate the variable wins

# Question 10.24
res.lm  <- lm(Deflection ~ Load, data = deflection)
res.lm2 <- lm(Deflection ~ Load + I(Load^2), data = deflection)
summary(res.lm2)             # fits very well to the data
anova(res.lm, res.lm2)       # all variables were flagged ; the data fits the quadratic model better 

# Question 10.25
res.lm  <- lm(weight ~ age + height + I(height^2) + I(height^3) + I(height^4), data = kid.weights)
res.lm2 <- update(res.lm, .~. -I(height^4))
res.lm3 <- update(res.lm2, .~. -I(height^3))
res.lm4 <- update(res.lm3, .~. -I(height^2))
anova(res.lm, res.lm2, res.lm3, res.lm4) 
# The ANOVA table shows that for each nested model the new term is statistically significant
# The full model is the one selected by this criteria

# Question 10.26
res.lm <- lm(body.fat ~ age + weight + height + BMI + neck + chest + abdomen + hip + thigh + knee + ankle + bicep + forearm + wrist, data = fat)

stepAIC(res.lm)
submodel <- lm(body.fat ~ age + weight + neck + abdomen + hip + thigh + forearm + wrist, data = fat)

summary(submodel)            # Multiple R-squared:  0.7467,    Adjusted R-squared:  0.7383

# Question 10.27
res.lm <- lm(MPG.city ~ EngineSize + Weight + Passengers + Price, data = Cars93)
summary(res.lm)              # only Weight is flagged, marked as significant
stepAIC(res.lm)              # only Weight was selected by AIC

# Question 10.28
x = 1:10 ; y = rnorm(10, mean = 1+(2*x)+(3*x^2), sd = 4)             # 1st simulation ; not the right model
stepAIC(lm(y ~ x + I(x^2)))
x = 1:10 ; y = rnorm(10, mean = 1+(2*x)+(3*x^2), sd = 4) 
stepAIC(lm(y ~ x + I(x^2)))
x = 1:10 ; y = rnorm(10, mean = 1+(2*x)+(3*x^2), sd = 4) 
stepAIC(lm(y ~ x + I(x^2)))
x = 1:10 ; y = rnorm(10, mean = 1+(2*x)+(3*x^2), sd = 4) 
stepAIC(lm(y ~ x + I(x^2)))
x = 1:10 ; y = rnorm(10, mean = 1+(2*x)+(3*x^2), sd = 4) 
stepAIC(lm(y ~ x + I(x^2)))
x = 1:10 ; y = rnorm(10, mean = 1+(2*x)+(3*x^2), sd = 4) 
stepAIC(lm(y ~ x + I(x^2)))
x = 1:10 ; y = rnorm(10, mean = 1+(2*x)+(3*x^2), sd = 4) 
stepAIC(lm(y ~ x + I(x^2)))
x = 1:10 ; y = rnorm(10, mean = 1+(2*x)+(3*x^2), sd = 4)             # 8th simulation ; right model for the first time
stepAIC(lm(y ~ x + I(x^2)))

# Question 10.29
attach(baycheck)
n  <- length(year)
yt <- log(Nt[-1]/Nt[-n])
nt <- Nt[-n]
detach(baycheck)
lm(yt ~ nt)
# Coefficients:
# (Intercept)           nt  
#   0.3458097   -0.0004088
r = 0.3458097
K = r/0.0004088              # K = 845.9141
