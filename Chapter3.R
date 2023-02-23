# This document belongs to the book "Using R for Introductory Statistics" by John Verzani.
# copyright (c) 2004 - Taylor and Francis
#
# In the document, foundations of statistical knowledge is practiced on R.
#
# written by Erkam Minsin
# find me on LinkedIn: https://www.linkedin.com/in/erkam-minsin-37537514a/
#
# last modified: Aug 2022
# first written: Aug 2022

setwd("C:/Users/erkam/R Learning/Using_R_for_Introductory_Statistics_John_Verzani")

## install.packages("UsingR")
library(UsingR)

## update.packages()


# Chapter 3 Bivariate Data ------------------------------------------------


# 3.1 Pairs of categorical variables

# 3.1.1 Making two-way tables from summarized data

# c(): creation of data vectors. To combine data vectors together; rbind(): as rows, cbind(): as columns.

?rbind
rbind(c(56,8),c(2,16))                           # combines rows
cbind(c(56,2),c(8,16))                           # bind as columns

# creating matrices
x=matrix(c(56,2,8,16),nrow=2)                    # default is bycolumn, which means byrow=FALSE
x=matrix(c(56,2,8,16),nrow=2,byrow=TRUE)

x=matrix(1)                                      # need to initialize x
x=edit(x)                                        # will edit matrix with spreadsheet
x

# giving row/column names
rownames(x)=c("buckled","unbuckled")
colnames(x)=c("buckled","unbuckled")

# specifying variable names
tmp=c("unbuckled","buckled")                     # less typing. row names first, column names later.
dimnames(x)=list(parent=tmp,child=tmp)           # use with list()

# if rbind() was used to make a matrix
x=c(56,8);names(x)=("unbuckled","buckled")
y=c(2,16)
rbind(unbuckled=x,buckled=y)                     # names rows, columns come from x

# 3.1.2 Making two-way tables from unsummarized data

grades
attach(grades)
table(prev,grade)                                # if the two data vectors are x and y, then the command table(x,y) will create the table
table(grades)                                    # also works

# a quick glance at the table indicates that the current grade relates quite a bit to the previous grade

# 3.1.3 Marginal distributions of two-way tables

apply(x,1,sum)                                   # row sum of x
apply(x,2,sum)                                   # column sum of x, OR,
margin.table(x,1)                                # row sum is for parents
margin.table(x,2)                                # column sum for kids

addmargins(x)                                    # returns classical marginal dists table

margin.table(table(prev,grade),1)                # previous, also table(prev)
margin.table(table(prev,grade),2)                # current, table(grade). well-shaped

# 3.1.4 Conditional distributions of two-way tables

options("digits"=1)                              # to fit on the page (1*2=2 digits after comma, default=:"digits"=4)
prop.table(table(prev,grade),1)                  # apply(x, 1, x/sum(x))
options("digits" = 4)                            # set back to default
detach(grades)

# 3.1.5 Graphical summaries of two-way contingency tables

barplot(x,xlab="Parent",
        main = "Child seat-belt usage")           # segmented bar graphs are the default
barplot(x,xlab="Parent",
        main="Child seat-belt usage",beside=TRUE) # get side-by-side bar graphs
barplot(x,main = "Child seat-belt usage",
        legend.text = TRUE)                       # to add a legend in upright
barplot(t(x))                                     # to change the primary distribution in the bar plot

# Problems 3.1.6

# Question 3.1

# Question 3.2
spam <- c(50,110,225,315,390,450)
total <- c(125,210,375,475,590,700)
x <- matrix(c(spam,total), nrow = 2, byrow = TRUE)
colnames(x) <- c("2000","2001","2002","2003","2004","2005")
rownames(x) <- c("spam","total")
barplot(x, ylab = "in billions",
        main = "Volume of spam in commercial email",
		beside = FALSE, legend.text = TRUE)

# Question 3.3
coins
attach(coins)
sum(value)                                       #1
barplot(t(table(year,value)),     
        legend.text = TRUE)                      #2
?cut                                             #3
detach(coins)

# Question 3.4
dvdsales
barplot(t(dvdsales),ylab="DVD Sales",xlab="Year",beside = T,legend.text = F)

# Question 3.5

# Question 3.6
x=matrix(c(0.76,.70,.68,.52,.48,.46,(1-.76),(1-.7),(1-.68),(1-.52),(1-.48),(1-.46)),nrow=2,byrow = TRUE)
colnames(x)=c("'97","'98","'99","'00","'01","'02")
rownames(x)=c("Cash assistance","Noncash assistance")
barplot(x,legend.text = TRUE)

# Question 3.7
View(UScereal)
attach(UScereal)
table(mfr,shelf)
options("digits"=1)
prop.table(table(mfr,shelf),1)                   # for shelves
prop.table(table(mfr,shelf),2)                   # for manufacturers
options("digits" =4 )
detach(UScereal)

# 3.2 Comparing independent samples

# 3.2.1 Side-by-side boxplots

pl=c(0,0,0,2,4,5,14,14,14,13,17,17,15)
ep=c(0,6,7,9,11,13,16,16,16,17,18,20,21)
boxplot(pl,ep,names = c("placebo","ephedra"))

# 3.2.2 Density plots

plot(density(pl),ylim=c(0,.07),
     main="densityplots of ep and pl")
lines(density(ep),lty=2)                         # lty for choosing line type between 1 and 6

# 3.2.3 Strip charts

stripchart(list(ephedra=ep,placebo=pl),          # combine data vectors in a list
           method = "stack",                     # stack multiples
		   pch = 16,offset = 1/2,cex=3)          # big circles-not squares

# 3.2.4 Quantile-quantile plots

qqplot(ep,pl)
qqnorm(ep)
qqnorm(pl)

# Problems 3.2.5

# Question 3.8
View(reaction.time)
attach(reaction.time)
boxplot(time[control=="C"],time[control=="T"], names = c("Without cell phone", "With cell phone"))
detach(reaction.time)

# Question 3.9
View(twins)
attach(twins)
boxplot(Foster,Biological,names = c("Foster","Biological"))
detach(twins)

# Question 3.10
View(stud.recs)
attach(stud.recs)
plot(density(sat.v),ylim=c(0,.0055),main = "density plot of sat.v and sat.m",xlab="SAT score")
lines(density(sat.m),lty=2)

qqplot(sat.v,sat.m)
qqnorm(sat.v)
qqnorm(sat.m)
detach(stud.recs)

#Question 3.11
View(morley)
attach(morley)
boxplot(Speed[Expt==1],Speed[Expt==2],names = c("Experiment 1","Experiment 2"))
detach(morley)

# Question 3.12
View(normtemp)
attach(normtemp)
boxplot(temperature[gender=="1"],temperature[gender=="2"],names = c("Male","Female"))
detach(normtemp)

# 3.3 Relationships in numeric data

# 3.3.1 Using scatterplots to investigate relationships

attach(homedata)
plot(y1970,y2000)                                # make the scatterplot
summary(y1970)
summary(y2000)
summary(y2000/y1970)
detach(homedata)

attach(maydow)
names(maydow)                                    # shows the variables' names
plot(max.temp[-1],diff(DJA),
     main = "max.temp versus daily change")
detach(maydow)

attach(kid.weights)
names(kid.weights)
plot(height, weight, pch = as.character(gender))
detach(kid.weights)

?par()                                           # the documentation of the graphic parameters(arguments)
# main= : title to put on the graphic
# xlab= : label for the x-axis. Similarly for ylab=
# xlim= : specify the x-limits, as in xlim=c(0,10) for the interval [0,10]. Similarly for ylim=
# type= : type of plot to make. "p" for points, "l" for lines, "h" for vertical lines, "n" for nothing
# bty=  : type of box to draw. "l" for L-shaped, default "o" for O-shaped
# pch=  : the style of point that is plotted. This can be a number or a single character ( plot(0:25,pch =0:25) )
# cex=  : magnification factor. Default is 1
# lty=  : when lines are plotted, specifies the type of line to be drawn. Diffeernt numbers correspond to different dash combinations
# lwd=  : the thickness of lines. Numbers bigger than 1 increase the default
# col=  : specifies the color to use for points or lines

# 3.3.2 The correlation between two variables

# The Pearson correlation coefficient
attach(homedata)
attach(maydow)
attach(kid.weights)
cor(y1970,y2000)                                 # 0.896 -> close to 1 -> nearly linear
cor(max.temp[-1],diff(DJA))                      # 0.0102 -> close to 0 -> not linear
cor(height,weight)                               # 0.824 -> close to 1 -> doubtfully linear

# The Spearman rank correlation

# If the relationship between the variables is not linear but is increasing, 
# such as the apparent curve for the height-weight data set, 
# we can still use the correlation coefficient to understand the strength of the relationship. 
# It is the Pearson correlation coefficient computed with the ranked data. 
# Ties(aynı numaralar) are averaged.

x=c(30,20,7,42,50,20)
rank(x)                                          # ties are averaged by default

cor(rank(y1970),rank(y2000))                     # 0.888 -> decreased a bit
cor(max.temp[-1],diff(DJA),
    method = "spearman")                         # 0.132 -> increased
cor(height,weight,method = "s")                  # 0.882 -> increased

detach(homedata);detach(maydow);detach(kid.weights)

# Problems 3.3.3

# Question 3.13
attach(homedata)
View(homedata)
hist(y2000/y1970,xlab="multiplicative change",main="Histogram of multiplicative change",probability = T)
lines(density(y2000/y1970))
# Bimodal data (there are two tops, modes)
detach(homedata)

# Question 3.14
attach(galton)
names(galton)
cor(child,parent)                                # 0.459
cor(child,parent,method="spearman")              # 0.42.5 #or cor(rank(child),rank(parent))

# Question 3.15
attach(normtemp)
names(normtemp)
plot(temperature,hr)
cor(temperature,hr)                              # 0.254
detach(normtemp)

# Question 3.16
attach(fat)
names(fat)
plot(0:25,pch=0:25)
plot(body.fat,BMI,pch=20)
cor(body.fat,BMI) #0.728
detach(fat)

# Question 3.17
attach(twins)
names(twins)
plot(Foster,Biological,pch=20)
cor(Foster,Biological)                           # 0.882
cor(Foster,Biological,method = "spearman")       # 0.886
detach(twins)

# Question 3.18
View(state.x77)
x77=data.frame(state.x77)                        # easier to work with
attach(x77)
plot(Population,Frost,pch=20)
plot(Population,Murder,pch=20)
plot(Population,Area,pch=20)
plot(Population,Income,pch=20)
plot(Population,HS.Grad,pch=20)
# No
detach(x77)

# Question 3.19
attach(nym.2002)
names(nym.2002)
plot(age,time,pch=20)
cor(age,time)                                    # 0.19
detach(nym.2002)

# Question 3.20
with(state.center,plot(x,y))
# No pattern

# Question 3.21
attach(batting)
names(batting)
plot(SO,HR,pch=20)
# Yes
cor(SO,HR)
cor(SO,HR,method="spearman")
detach(nym.2002)

# Question 3.22

# jitter(): Used in a data set with many values with discrete and comma values.  
# In such a case, a simple scatter plot does not reflect the data completely.  
# So when we use jitter() and increase the factor value, the data is reflected more.
# Observe the difference below.  

attach(galton)
View(galton)
plot(parent,child)
plot(jitter(parent,factor = 1),jitter(child,factor=1))
plot(jitter(parent,factor = 5),jitter(child,factor=5))
plot(jitter(parent,factor = 10),jitter(child,factor=10))
plot(jitter(parent,factor = 100),jitter(child,factor=100))
detach(galton)

# 3.4 Simple Linear Regression

# lm(y~x): y is modeled by x with a predicted linear model(equation)
# "y~x" is called model formula

attach(homedata)
lm(y2000~y1970)                                  # Intercept is -$104000. 
# the negative intercept should be a warning not to use this model for prediction with a really low 1970 home value.

res=lm(y2000~y1970)                              # type res to see default output

# Adding the regression line to a scatter plot: "a-b-line"
plot(y1970,y2000,main = "-104000+5.258x",pch=20)
abline(res)                                      # the output of lm(), stored in res, is plotted

# other lines can also be added to a graphic using abline(). 
# abline(h = c) : add horizontal line at y = c
# abline(v = c) : add vertical line ay x = c

# Using the prediction lines for predictions

# let us say y1970 = $50000, manually
y=-104000+5.258*50000                            # 158900

# with coef();
betas=coef(res)
betas
sum(betas*c(1,50000))                            # beta0*1 + beta1*50000

# residuals() and predict()

# residuals() returns the residuals
# predict() will perform predictions as above
# predict() requires a data frame with properly named variables
# data.frame() will be discussed in the next chapter

# for the data point (55100,130200)
predict(res,data.frame(y1970=55100))             # 185709.6

# residual 
130200-predict(res,data.frame(y1970=55100))      # -55509.56, by subtraction
residuals(res)[which(y1970==55100&y2000==130200)]# -55509.56, by residuals()

# Creating scatterplot with modal formula (~)
plot(y2000~y1970)
res=lm(y2000~y1970)
abline(res)
# data= can usually be used to attach a data frame temporarily in lm()
# data= similar to that offered more generally by the function with()
# both enable users to use the variables within a data frame by their name
# subset= can be used in lm() to restrict the rows that are used in the data
# subset= can be specified by a logical condition or a specification of indices 

# 3.4.3 Transformations of the data

attach(kid.weights)
height.sq <- kid.weights$height^2
plot(weight~height.sq, data = kid.weights, pch=20)
res <- lm(weight~height.sq, data = kid.weights)
abline(res)                                      # weight=3.1089+0.0244*(height^2)

# shortly
plot(weight~height^2, data=kid.weights, pch=20)  # not as expected
res=lm(weight~height^2, data=kid.weights)
abline(res)
res
# The resulting graph would look identical to the graph of height vs weight, not the graph of height squared vs weight. 
# The reason for this is that the modal formula syntax uses the familiar math notations *,/,^ differently. 
# To use them in their ordinary sense, we need to insulate them in the formulas with the I() function.

plot(weight~I(height^2),data = kid.weights,pch=20)
res=lm(weight~I(height^2),data=kid.weights)
abline(res)
res                                              # the same with the first way of coding

detach(kid.weights)

# 3.4.4 Interacting with a scatterplot

?identify 
# identify() returns the indices of the selected points
# identify() must know about the points we want to identify
# identify(x,y, label= , n= ) - x & y : x-axis and y-axis locations, labels= : for the placement of other text, n= : number of points to identify
# by default, it identifies points with each mouse right-click until instructed to stops
# as points are identified, R will put the index of the point next to it
# labels= allows for the placement of other text
# for example: plot(x,y); identify(x,y,n=1) will identify the closest point to our first mouse click on the scatterplot by its index
# whereas identify(x,y,labels=names(x)) will identify as many points as we want, labeling them by the names of x
# Used as below (to identify outliers)

?locator
# locator() will locate the (x,y) coordinates of the points we select with our mouse
# it is called with the number of points desired, as with locator(2)
# the return value is a list containing two data vectors, x and y, holding the x and y positions of the selected points

plot(BUCHANAN~BUSH, data=florida, pch=20)
res <- lm(BUCHANAN~BUSH, data=florida)           # store it
abline(res)
with(florida,identify(BUSH,BUCHANAN,n=2,labels=County))
florida$County[c(13,50)]

with(florida,predict(res,data.frame(BUSH=BUSH[50])))
residuals(res)[50]
# this simple analysis indicates that 
# Buchanan received 2610 of Gore's votes-many more than the 567 that decided by the state and the presidency. 
# Real data 6607.

# 3.4.5 Outliers in the regression model

View(emissions)
names(emissions)
attach(emissions)
f <- CO2~perCapita                               # save formula
plot(f, data=emissions,pch=20)
abline(lm(f, data=emissions))
abline(lm(f, data=emissions, subset = -1), lty=2)

# 3.4.6 Resistant regression lines: lqs() and rlm()

# Least-trimmed squares: lqs() 
library(MASS)
abline(lqs(f,data=emissions),lty=3)

# Resistant regression: rlm()
abline(rlm(f,data=emissions,method = "MM"),lty=4)

# Adding legends to plots
legend()
the.labels <- c("lm","lm w/o 1","least trimmed squares","rlm with MM")
the.ltys <- 1:4
legend(5000,6000,legend=the.labels,lty=the.ltys) # x=5000 and y=6000 coordinates. Just a string as "upright" can also be used

# 3.4.7 Trend lines

# these produce a smooth curve summarizing the relationship between the two variables
# the stats package provides several functions for creating trend lines
# the scatter.smooth() uses the loess() function from the same package to plot both the scatterplot and a trend line
# smooth.spline() will fit the data using cubic splines
# supsmu() will perform Friedman's "super smoother" algorithm
attach(five.yr.temperature)
scatter.smooth(temps~days, col=gray(.75), bty="n")
lines(smooth.spline(temps~days), lty=2, lwd=2)
lines(supsmu(days,temps), lty=3, lwd=2)
legend(locator(1), lty=c(1,2,3), lwd=c(1,2,2),
       legend=c("scatter.smooth","smooth.spline","supsmu"))
detach(five.yr.temperature)

# Problems 3.4.8

# Question 3.23

# Question 3.24
View(fat)
names(fat)
attach(fat)
plot(abdomen~wrist,data=fat,pch=20)
res=lm(abdomen~wrist)
abline(res)
res

y=-37.954+7.159*17                               # prediction way 1 = 83.749

betas=coef(res)
sum(betas*c(1,17))                               # way 2 = 83.75187 (round-off difference)

predict(res,data.frame(wrist=17))                # way 3 = 83.75187

# Question 3.25
attach(wtloss)
names(wtloss)
plot(Weight~Days,data = wtloss,pch=20)

cor(Weight,Days)                                 # -0.985

res=lm(Weight~Days,data=wtloss)
abline(res)

plot(residuals(res)~Days,pch=20)

detach(wtloss)

# Question 3.26
x77=data.frame(state.x77)
attach(x77)

plot(Illiteracy~HS.Grad,data = x77,pch=20)
res=lm(Illiteracy~HS.Grad,data = x77)
abline(res)

plot(Life.Exp~Murder,data = x77,pch=20)
res=lm(Life.Exp~Murder,data = x77)
abline(res)

plot(Income~Illiteracy,data = x77,pch=20)
res=lm(Income~Illiteracy,data = x77)
abline(res)

detach(x77)

# Question 3.27
attach(batting)
plot(RBI~HR,data=batting,pch=20)
res=lm(RBI~HR,data=batting)
abline(res)

predict(res,data.frame(HR=33)) #104.1099. Real=98

residuals(res)[which(HR==33&RBI==98)] #-6.1099

detach(batting)

# Question 3.28
attach(too.young)
names(too.young)
res=lm(Male~Female,data=too.young)
res
plot(Male~Female,data=too.young,pch=20,main="How Young")
abline(res)

lines(7+0.5*Male,Male,lty=2,lwd=1)

detach(too.young)

# Question 3.29
attach(diamond)
names(diamond)
res=lm(price~carat,data=diamond)
res
plot(price~carat,data=diamond,pch=5)
abline(res)

betas=coef(res)
betas
sum(betas*c(1,I(1/3)))                           # 980.7157

detach(diamond)

# Question 3.30
attach(Animals)
names(Animals)
a=log(body)
b=log(brain)
res=lm(b~a,data = Animals)
res                                              # slope is 0.496

res2=lqs(b~a,data=Animals)
res2                                             # slope is 0.4633

# Question 3.31
attach(breakdown)
names(breakdown)
res=lm(voltage~log(time),data = breakdown)
res
plot(voltage~log(time),data = breakdown,pch=20,main="35.2356-0.9973*log(time)")
abline(res)

detach(breakdown)

# Question 3.32
attach(motors)
names(motors)
plot(time~temp,data = motors,pch=as.character(cens))
# 4
# Yes
res=lm(time~temp,data=motors)
res
abline(res) 
# coefficients:22999.1 and -106.8
predict(res,data.frame(temp=210))
# 580.5888

detach(motors)

# Question 3.33
attach(mw.ages)
names(mw.ages)
View(mw.ages)
plot(1:103,Male+Female,data=mw.ages,col=gray(0.75),pch=20)
lines(supsmu(1:103,Male+Female),lty=1,lwd=1)