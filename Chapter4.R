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


# Chapter 4 Multivariate Data ---------------------------------------------


# 4.1 Viewing multivariate data

# 4.1.1 Summarizing categorical data

View(student.expenses)
attach(student.expenses)
names(student.expenses)
table(cell.phone,car)

# Three or n -way contingency tables
table(cell.phone,car,cable.modem)
# first two entries (variables) to table function are used for the main table, 
# the remaining variables to construct different tables for each combination of the variables.

# Flattened contingency tables
ftable(table(cell.phone,car,cable.modem))

# set column variables
ftable(table(cell.phone,car,cable.modem),col.vars = c("cable.modem","car"))

detach(student.expenses)

# 4.1.2 Comparing independent samples

# When we have data for several variables of the same type, we often want to compare their centers, spreads, or distributions. 
# This can be done effectively using boxplots.

attach(ewr)
names(ewr)
boxplot(AA,CO,DL,HP,NW,TW,UA,US)
detach(ewr)

# 4.1.3 Comparing relationships

View(babies)
names(babies)
attach(babies)
gestation[gestation==999]=NA                     # 999 is code for NA
plot(gestation, wt)                              # scatterplot
plot(gestation,wt,pch=smoke)                     # different plot characters
table(smoke)                                     # values of plot characters, used to find out the range of values of smoke
legend(locator(1),
       legend = c("never","yes","until pregnant","long ago","unknown"),
	   pch=c(0:3,9))

# sometimes different colors will help where different plot characters don't

smoke[smoke==9]=4
plot(gestation,wt,col=rainbow(5)[smoke+1])
# if we make the scatterplot, it shows that changing colors tells more of a story than changing the plot characters

# still, no additional trends show up. 
# what might be useful are different scatterplots for each level of the smoke factor. 
# this can be done by subsetting. It will be described later.

# Plotting additional points and functions

# R's plotting functions come in two types
# "high-level" plot functions like plot(), "low-level" plot functions like abline()
# the difference between the two is that;
# the high-level ones set up a graphic window and produce a graphic
# the low-level ones add to the current graphic window

# plot() : when used for scatterplots, will plot points by default.
# use argument type="l" to produce lines
# high-level function, used to make many types of figures
# points() : a low-level plot function with arguments similar to plot()
# lines() : similar to points() but connects points with lines segments
# abline() : function for adding lines to a figure
# the arguments a= and b= will plot the line y=a+bx,
# the arguments h= and v= will plot horizontal or vertical lines 
# curve() : a high- or low-level plot function for adding the graph of a function of x
# when argument add=TRUE is given, will draw graph on the current figure using the current range of x
# if add=TRUE is not given, it will produce a new graph over the range specified with from= and to=
# the defaults are 0 and 1
# the function to be graphed may be specified by name or written as a function of x 
# rug() : adds lines along the x- or y-axis to show data values in a univariate data set
# by default, the lines are drawn on the x-axis
# use side=2 to draw on the y-axis
# arrows() : adds arrows to a figure
# text() : adds text to a figure at specified points
# title() : adds labels to a figure
# argument main= will set main title, sub= the subtitle, xlab= and ylab= will set x and y labels
# legend() : adds a legend to a figure

gestation[gestation==999]=NA
f=wt[smoke==0]~gestation[smoke==0]               # save typing
plot(f,xlab="gestation",ylab="wt")
abline(lm(f))

# to add to the graphic
f1=wt[smoke==1]~gestation[smoke==1]
points(f1,pch=16)
abline(lm(f1),cex=2,lty=2)
legend(150,175,legend = c("0=never smoked","1=smokes now"),pch=c(1,16),lty=1:2)

detach(babies)

# Problems 4.1.4

# Question 4.1
attach(samhda)
names(samhda)
table(amt.smoke,marijuana,gender)
table(amt.smoke,marijuana,live.with.father)
detach(samhda)

# Question 4.2
View(Cars93)
names(Cars93)
mpg=with(Cars93,cut(MPG.city,c(0,17,25,55)))     # cut(): numeric to categorical variables
names(mpg)=c("bad","decent","excellent")
price=with(Cars93,cut(Price,c(0,10,20,62)))
names(price)=c("cheap","mid-priced","expensive")
f.table=with(Cars93,ftable(table(mpg,price,Type)))
f.table

# Question 4.3
attach(Cars93)
plot(MPG.city,Price,pch=20)
table(Type)
plot(MPG.city,Price,pch=c("Compact","Large","Midsize","Small","Sporty","Van"))

detach(Cars93)

# Question 4.4
attach(carsafety)
names(carsafety)
View(carsafety)
plot(Driver.deaths,Other.deaths,pch=as.numeric(type))
identify(Driver.deaths,Other.deaths,n=1,labels = type)
# type[c(a,b)] finds the outlier, but I can't get the coordinates because my clicking don't work.

detach(carsafety)

# Question 4.5
attach(cancer)
names(cancer)
boxplot(stomach,bronchus,colon,ovary,breast)

detach(cancer)

# Question 4.6
attach(UScereal)
View(UScereal)
names(UScereal)
table(mfr,vitamins,shelf)

plot(calories,sugars,pch=20)
# identify(calories,sugars,...)

plot(calories,sugars,cex=2*sqrt(fat))
# this is called a bubble plot. The area of each bubble is proportional to the value of fat.

# 4.2 R basics: data frames and lists

# 4.2.1 Creating a data frame or list

# a data frame is used to store rectangular grids of data (equal-length data vectors)
# the collection of entries need not all be of the same type(e.g. numeric, character, or logical)
# but each column must contain the same type of entry as they are data vectors

# a list is a more general type of storage than a data frame
# each component in a list can be any R objects, such as a vector, a data frame, a function, or even another list
# many of the functions in R, such as lm(), have return values that are lists

data.frame()                                     # returned also by read.table() and read.csv().
list()

x=1:2 
y=letters[1:2]                                   # y=c("a","b")
z=1:3 
data.frame(x,y)                                  # rectangular, cols are variables
data.frame(x,y,z)                                # error because not all has the same size

# Data frames MUST have variables of the same length.

list(x,y,z)
# It specifies where the values are stored
# [[1]] says this is for the first top-level component of the list, which is a data vector 
# The following [1] refers to the first entry of this data vector

# in data frames, character variables, such as y, are coerced to be factors, unless insulated with I
# this coercion isn't done by list()
# this can cause confusion when trying to add new values to the variable

# Adding names to a data frame or list

# data.frame(x,y) assigns names of x and y automatically, but list() does not.

list(x.name=x,"y name"=y)

eg=data.frame(x,y)
names(eg)
names(eg)=c("x.name","y name")                   # change the names
names(eg)

colnames(eg)
rownames(eg)
dimnames(eg)

# the size of a data frame or list

View(ewr)
dim(ewr)                                         # number of rows(46) and columns(11)
dim(ewr)[2]                                      # returns the number of columns=11
 
nrow(ewr)                                        # number of rows
ncol(ewr)                                        # number of columns

length(ewr)                                      # number of top-level components(variables)

# 4.2.2 Accessing values in a data frame

# Accessing variables in a data frame by name

x <- data.frame(a=1:2,b=3:4)
a                                                # error: a is not found
attach(x)                                        # now a and b are there
a                                                # a is a vector
a[1]=5                                           # assignment
a                                                # a has changed
x                                                # not x though
detach(x)
a                                                # a still as changed
x                                                # and still not x

# the with() function and the data0 argument were mentiones in chapter 1 as alternatives to attaching a data frame
# we will use these when it is convenient

# Accessing a data frame using [,] notation

df=data.frame(x=1:3,y=4:6)
rownames(df)=c("row1","row2","row3")
df
df[3,2]                                          # by a single number, 3rd row, 2nd column
df["row3","y"]                                   # by a single name
df[1:3,1]                                        # by a vector of numbers, returns column 1
df[1:2,1:2]                                      # by a vector of numbers
df[,1]                                           # all rows & 1st column
df[1,]                                           # all columns & just 1st row
df[c(T,F,T),]                                    # by a logical vector of the appropriate length, all columns & row1 and row3

# Example : Using data frame notation to simplify tasks
attach(babies)
gestation[gestation==999]=NA
age[age==99]=NA
inc[inc==98]=NA
# bad idea because it doesn't change babies, only copies

pairs(babies[,c("gestation","wt","age","inc")])
# the graphic produced won't be correct, as we didn't actually change the values in the data frame babies; 
# rather we modified the local copies produced by attach().

rm(gestation);rm(age);rm(inc)                    # clear out copies
detach(babies)                                   # really clear out

# better way
attach(babies)
not.these=(gestation==999)|(age==99)|(inc==98)
tmp=babies[!not.these,
           c("gestation","age","wt","inc")]
pairs(tmp)                                       # pairs() produces the scatterplot matrix of the new data frame tmp                              
detach(babies)

# Example : Accessing a data frame with logical vectors
attach(ewr)
boxplot(ewr[inorout=="in",3:10],main="Taxi in")  # rows argument is a logical vector of the correct length
boxplot(ewr[inorout=="out",3:10],main="Taxi out")
detach(ewr)

# when a boxplot is called with a list, any names for the list are used to label the boxplot
# as a data frame is also a list, this behavior makes using boxplot() much easier than typing in all the variable names

# subset()

# new.df <- subset(old.df, subset= , select= )
# subset=argument works to restrict the rows by using a logical condition
# select=argument is used to restrict the columns when given a vector of the desired variable names or indices

# there is no need to attach the data frame or use with()
 
ewr.in=subset(ewr,subset = inorout=="in",select = 3:10)
ewr.out=subset(ewr,subset=inorout=="out",select=3:10)
boxplot(ewr.in,main="Taxi in")
boxplot(ewr.out,main="Taxi out")

# Example: Sorting a data frame by one of its columns
attach(mtcars)
sort(mpg)                                        # increasing order by default
sort(mpg,decreasing = TRUE)

# order()

# order(x) will return the indices of the sorted values of x
mtcars[order(mpg),]                              # sort  by mpg
mtcars[order(mpg,decreasing = TRUE),]            # best mpg first
rownames(mtcars[order(mpg,decreasing = TRUE),])  # only names
mtcars[order(cyl,hp),]                           # by cylinders then horsepower
detach(mtcars)

# Accessing a list

#[[]], double-square-bracket notation is used to access list components
lst=list(a=1:2,b=letters[1:3],c=FALSE)
lst[[1]]                                         # first component
lst[["a"]]                                       # by name

# $ Notation is the short version of lst[["variable name"]]
lst=list(one.to.two=1:2,"a-e"=letters[1:5])
lst$one.to.two
lst$o                                            # unique shortening also works here
lst$"a-e"                                        # when the names includes blanks or other special characters, it needs to be quoted.

# Lists as vectors
lst=list(a=1:2,b=letters[1:2],c=FALSE)
lst[1]                                           # printed with name "$a", returns a list with just the first component of lst
lst[[1]]                                         # no name

df=data.frame(a=1:2,b=letters[1:2])
df[1]

# To access the row "Honda Civic"
mtcars["Honda Civic",]                           # by row name
mtcars["Honda",]                                 # can shorten the name if unique match
mtcars[19,]                                      # It is the 19th row in the data set

# To access the column "mpg"
mtcars[,"mpg"]                                   # by column name
mtcars[,1]                                       # it is column 1
mtcars$mpg                                       # list access by name
mtcars[["mpg"]]                                  # alternate list access

mtcars["mpg"]                                    # Note: mtcars["mpg"] is not a vector but a data frame.

# To access the value "30.4"
mtcars["Honda Civic", "mpg"]                     # by name (with match)
mtcars[19,1]                                     # by row and column number
mtcars$mpg[19]                                   # mtcars$mpg is a vector, this is the 19th entry.

# 4.2.3 Setting values in a data frame or list

df=data.frame(a=1:2,b=3:4)
df[1,1]=5
df[,2]=9:10
df[1:2,3:4]=cbind(11:12,13:14)                   # rows and columns at once
df
df[1:2,10:11]=cbind(11:12,13:14)                 # error! it would create a hole
df[,2:3]=0                                       # recycling occurs
df

lst=list(a=1:2,b=1:4,c=c("A","B","C"))
lst$a=1:5                                        # replace the data vector
lst$b[3]=16                                      # replace single element
lst$c[4]="D"                                     # appends to the vector
lst

# the c() function can be used to combine lists using the top-level components
# this can be used with data frames with the same number of rows, BUT
# the result is a list, not a data frame
# it can be turned into a data frame again by using data.frame()

# 4.2.4 Applying functions to a data frame or list

df=ewr[,3:10]
mean(df)
median(df)
apply(df,2,mean)                                 # mean of columns
apply(df,2,median)                               # median of columns

# lapply() or sapply()-more user-friendly- are used for lists
lapply(df,median)                                # the lapply() function will return a list
sapply(df,median)                                # whereas sapply() will simplify the results into a vector or matrix

# Problems 4.2.5

# Question 4.7
View(mtcars)
mtcars[order(wt,decreasing = TRUE),]

mtcars[order(mpg,decreasing = TRUE),]            # Toyota Corolla, Lincoln Continental

imported=mtcars[c(1:3,8:14,18:21,26:28,30:32),"mpg"]
nrow(mtcars)
domestic=mtcars[c(4:7,15:17,22:25,29),"mpg"]
boxplot(imported,domestic,names=c("imported","domestic"))

plot(mtcars$mpg~mtcars$wt,pch=as.character(cyl),xlab="weight",ylab="miles per gallon")

# Question 4.8
View(cfb)
x=subset(cfb,subset=INCOME>0&NETWORTH<0,select = c("INCOME","NETWORTH"))
x
dim(x)

# Question 4.9
View(hall.fame)
attach(hall.fame)
hist(HR,main="Home Runs")

hf=hall.fame[,c("AB","hits","HR","RBI")]
hf

boxplot(lapply(hf,scale))

detach(hall.fame)

# Question 4.10

# Question 4.11
df=ewr[,3:10]
df
mean(df)
apply(df,1,mean)

# Question 4.12
View(u2)
boxplot(u2)
sort(sapply(u2,mean))
sort(sapply(u2,median))
sort(unlist(u2),decreasing = TRUE)[1:3]

# Question 4.13
View(normtemp)
plot(density(normtemp$temperature[normtemp$gender==1]),xlab = "Temperature F",main="Temperature Male vs Female",ylim=c(0,.6))
lines(density(normtemp$temperature[normtemp$gender==2]),lty=2)
legend(95.5,0.5,legend = c("Male","Female"),lty = 1:2)

# Question 4.14
# df[,] returns the data set.

# 4.3 Using model formula with multivariate data

# 4.3.1 Boxplots from a model formula

boxplot(gestation~inc,data=babies)               # not yet
boxplot(gestation~inc,
        subset = gestation!=999&inc!=98,
		data=babies)                             # better
boxplot(gestation~inc,
        subset = gestation!=999&inc!=98,
		data=babies,varwidth=TRUE,               # variable width to see sizes
		xlab = "income level",
		ylab="gestation (days)")

# 4.3.2 The plot() function with model formula

plot(gestation~factor(inc),
     data=babies,varwidth=TRUE,
	 subset = gestation!=999&inc!=98,
	 xlab="income level",
	 ylab="gestation (days)")
# plot(x(numeric)~f(factor)) will create side-by-side boxplots of values of x split up by the levels of f.

# 4.3.3 Creating contingency tables with xtabs()

percents=c(71,70,79,82,71,83,71,50,70,71,55,73)
car=rep(c("passenger","pickup","van/SUV"),4)
year=rep(rep(2001:2002,c(3,3)),2)
enforcement=rep(c("primary","secondary"),c(6,6))
seatbelts=data.frame(percents,car,year,enforcement)
seatbelts

tab=xtabs(percents~car+year+enforcement,data = seatbelts)
tab

ftable(tab,col.vars = c("enforcement","year"))

# 4.3.4 Manipulating data frames: split() and stack()

# split(x,f) : to split up a variable by the levels of some factor
# boxplot(split(x,f))=boxplot(x~f)
# tapply(x,f,function) : applying a single function to each component of the list returned by split()

# inverse to split() is stack() : takes a collection of variables stored in a data frame or list and stacks them into two variables
# one contains the values, and the other indicates which variable the data originally come from
# unstack() reverses stack(), returning only data frame, not a list

cancer
stack(cancer)

# Problems 4.3.5

# Question 4.15
View(MLBattend)
boxplot(MLBattend$attendance~MLBattend$year,
        xlab="year",ylab="attendance",varwidth=TRUE)
plot(MLBattend$attendance~factor(MLBattend$year),
     xlab="year",ylab="attendance",varwidth=TRUE)
# 1974 and 1975
boxplot(MLBattend$attendance~MLBattend$year,
        xlab="year",ylab="attendance",varwidth=TRUE,
		subset = MLBattend$year!=74&MLBattend$year!=75)

# Question 4.16
tapply(MLBattend$runs.scored[MLBattend$year==69&70&71],MLBattend$league[MLBattend$year==69&70&71],mean)

tapply(MLBattend$runs.scored[MLBattend$year!=69&70&71],MLBattend$league[MLBattend$year!=69&70&71],mean)

# Question 4.17
View(npdb)
table(npdb$ID)

tmp=split(npdb$amount,npdb$ID)
tmp
sapply(tmp,sum)
sapply(tmp,length)
df=data.frame(sum=sapply(tmp,sum),number=sapply(tmp,length))
df

boxplot(df$sum,df$number,ylab="total amount",xlab="number of awards",varwidth = TRUE,names = c("1","2"),ylim=c(0000,2000000))

# Question 4.18
View(morley)
boxplot(morley$Speed~morley$Expt,xlab="experiment",ylab="speed",varwidth=TRUE)

# Question 4.19
View(PlantGrowth)
boxplot(PlantGrowth$weight~factor(PlantGrowth$group),xlab="group",ylab="weight",varwidth=TRUE)

# Question 4.20
View(InsectSprays)
boxplot(InsectSprays$count~InsectSprays$spray,subset = InsectSprays$spray==c("C","D","E"),xlab = "spray",ylab = "count",varwidth=TRUE)

# Question 4.21
pairs(~gestation+age+wt+inc,data=babies,subset=gestation<999&age<99&inc<98)

View(UScereal)
plot(~calories+carbo+protein+fat+fibre+sugars,data=UScereal)

# 4.4 Lattice graphics

library(lattice)
?Lattice
?xyplot

# the formulas have the format response ~ predictor | condition

trellis.device(bg="white")                       # set background to white
# options(lattice.theme="col.whitebg")           # alternative way to set the bg to white

# histograms (univariate)
histogram(~wt|factor(smoke),data=babies,subset = wt!=999,type="density")

# density plots (alternative to histograms)
densityplot(~wt|factor(smoke),data=babies)

# box plots (univariate or multivariate)
bwplot(gestation~factor(inc),data=babies,subset = gestation!=999)

bwplot(gestation~factor(inc)|factor(smoke),data=babies,subset=gestation!=999)

# scatter plots
xyplot(wt~gestation|factor(smoke),data=babies,subset = (wt!=999&gestation!=999))

# scatter plots with regression line
panel.regression=function(x,y){panel.xyplot(x,y);panel.abline(lm(y~x))}
xyplot(wt~gestation|factor(smoke),data=babies,subset = (wt!=999&gestation!=999),panel = panel.regression)

# Problems 4.4.1

# Question 4.22
View(kid.weights)
?cut()
xyplot(weight~height|cut(kid.weights$age/12,3*(0:4)),data=kid.weights)

# Question 4.23
xyplot(weight~gender|cut(kid.weights$age/12,3*(0:4)),data=kid.weights)

# Question 4.24
View(female.inc)
bwplot(female.inc$income/100000~factor(female.inc$race),xlab = "race",ylab = "income ($100K)",varwidth=T)
summary(female.inc$income[female.inc$race=="black"])
summary(female.inc$income[female.inc$race=="hispanic"])
summary(female.inc$income[female.inc$race=="white"])

# Question 4.25
View(ToothGrowth)
bwplot(ToothGrowth$len~factor(ToothGrowth$dose)|factor(ToothGrowth$supp),xlab="dose",ylab="length",varwidth=T)

bwplot(ToothGrowth$len~factor(ToothGrowth$supp)|factor(ToothGrowth$dose),xlab="dose",ylab="length",varwidth=T)

# Question 4.26
View(carsafety)
boxplot(carsafety$Driver.deaths~factor(carsafety$type),xlab="type of car",ylab="driver deaths",varwidth=T)

boxplot(carsafety$Driver.deaths+carsafety$Other.deaths~factor(carsafety$type),xlab="type of car",ylab="total deaths",varwidth=T)

# Question 4.27
View(Orange)

xyplot(Orange$circumference~Orange$age|factor(Orange$Tree),pch=20,xlab = "age",ylab = "circumference")

# Question 4.28
View(survey)
panel.regression=function(x,y){panel.xyplot(x,y);panel.abline(lm(y~x))}
xyplot(survey$Wr.Hnd~survey$NW.Hnd|factor(survey$gender),pch=20,xlab = "writing hand size",ylab = "non-writing hand size",panel=panel.regression)

bwplot(survey$Pulse[survey$Pulse!=NA]~survey$Smoke[survey$Pulse!=NA]|factor(survey$gender),pch=20,xlab="smoke",ylab="pulse",varwidth=T,)

# 4.5 Types of data in R

# 4.5.1 Factors

class()                                          # the class of an object

# Creating factors: factor() or as.factor()
x=1:3; fac=letters[1:3]
factor(x)                                        # used to create levels
factor(fac)

# Adding levels : levels= and levels()
factor(x,levels = 1:10)                          # add more levels than 3

# the values of data vector being coerced into a factor should match those specified to levels=
factor(x,levels=fac)                             # returns NA if not in levels

# levels() : can list the levels of a factor directly, and
# allows us to change or add to the levels
x=factor(1:5)
x
levels(x)                                        # character vector

x=factor(1:3)
x
levels(x)=letters[1:3]
x
levels(x)=1:10
x
# the number of new levels must be at least as great as the current number

# Dropping levels of factors
x=factor(letters[1:5])
x
x[1:3]
factor(x[1:3])

x[1:3,drop=TRUE]                                 # more direct way to relevel

# Ordering levels in a factor

# the ordering of the levels is usually done alphabetically (according to the result of sort(unique(x)))
l=letters[1:5]
factor(l)
factor(l,levels=rev(l))

# 4.5.2 Coercion of objects

# coercion is the act of forcing one data type inot another
# this is typically done with an "as." function, such as as.data.frame()

# Coercing different data types
x=1:5
c(is.numeric(x),is.character(x),is.factor(x),is.logical(x))
x=as.character(x)
x
c(is.numeric(x),is.character(x),is.factor(x),is.logical(x))
x=as.factor(x)
x
c(is.numeric(x),is.character(x),is.factor(x),is.logical(x))
x=as.logical(x)                                  # fails. The coercion to logical is picky 
x
c(is.numeric(x),is.character(x),is.factor(x),is.logical(x))

# Coercing factors to other types
f=factor(letters[1:5])
f
unclass(f)                                       # shows how f is stored
as.vector(f)                                     # coerce to vector type
as.integer(f)                                    # coerce to integers

g=factor(2:4)
g
as.numeric(g)
as.numeric(as.character(g))                      # factor to numeric does not work directly. First, from factor character then to numeric.

# Coercing vectors, data frames, and lists

# Coercing a vector to a data frame 

# if x is a vector, then as.data.frame(x) will produce a data frame with one column vector
# by default, strings will be coerced to factors

# create matrix from numbers before coercing to a data frame
x=1:8
dim(x)=c(2,4)                                    # takes a vector where each component specifies the size of that dimension. Rows first, then columns
x
as.data.frame(x)                                 # turns matrixto data frame

# Coercing a data frame or list into a vector

# we should start out with all the same type of data
# when applied to a list, unlist() function is used
# when applied to a data frame, it goes column by column creating a vector
x=1:8
dim(x)=c(2,4)
df=data.frame(x)
df
unlist(df)