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


# Chapter 1 Data -----------------------------------------------


# 1.2.2 Using R as a calculator

2+2
2^2                          # powers are taken with ^
(1-2)*3
1-2*3

# multiple commands per line can be evaluated if separated by a semicolon ;

# functions
sqrt(2)                      # the square root
sin(pi)                      # the sine of built-in pi
exp(1)                       # exp(x) = e^x
log(10)                      # the log base e
log(10, base = 10)           # the log base 10        
log(10,10)
log10(10)

# 1.2.3 Assignment

x <- 2                       # assignment is quiet
x+3                          # x is now 2
pi                           # pi is a built-in, previously assigned.
e^2                          # error because e is not a built-in
e <- exp(1)                  # e is now its familiar value
e^2                          # no error. it works

# some variable names are naturally used to represent certain types of data.
# n is for a length,
# x or y stores a data vector,
# i and j are for integers and indices.

# 1.2.4 Using c() to enter data

# data vectors can be made with the c() function.
whales <- c(74,122,235,111,292,111,211,133,156,79)                   # numeric data vector

x <- c(74,122,235,111,292)
y <- c(111,211,133,156,79)
c(x,y)                       # combines x and y data vectors in one vector

# data vectors have a type
# one restriction on data vectors is that all the values have the same type.
# this type can be logical, numeric or character string type.

Simpsons <- c("Homer", "Marge", "Bart", "Lisa", "Maggie")            # character string type data vector

# if we mix the type within a data vector, the data will be coerced into a common type, which is usually a character.
# this can prevent arithmetic operations.

# giving data vectors named entries
names(Simpsons) <- c("dad", "mom", "son", "daughter1", "daughter2")
names(Simpsons)
Simpsons

# 1.2.5 Using functions on a data vector

sum(whales)                  # total number of beachings
length(whales)               # length of data vector
sum(whales)/length(whales)   # average number of beachings
mean(whales)                 # mean function finds average
sort(whales)                 # the sorted values
min(whales)                  # the minimum value
max(whales)                  # the maximum value
range(whales)                # range returns both min and max
diff(whales)                 # diff returns differences
cumsum(whales)               # a cumulative, or running tally

# vectorization of functions
whales.fla <- c(89,254,306,292,274,233,294,204,204,90)
whales + whales.fla
whales - whales.fla          # florida usually has more
whales - mean(whales)        # difference from the average

# variance
var()

# finding help
help("mean")                 # will find help on the mean() function. Good in case of knowing the function's names
?mean                        # shortcut for the help() function

# what if we do not know the function's names?
help.search("mean")          # returns many matches of functions that mention "mean" in certain parts of their help page
apropos("mean")              # will return all documented functions and variables with the word "mean" in their names

help.start()                 # will open a web browser to an index of all the available documentation, including the manuals, if installed

example(mean)                # returns the examples in the help page of function mean all at once
 
# history
history()                    # shows the last 25 commands entered

# data entry
data.entry() ; edit()        # allows to edit the data (vector,matrices,frames) entered
x <- c(1)                    # variable x with 1 as the first entry
data.entry(x)                # edit x with the same type entries (numeric here) as long as you want

# 1.2.6 Creating structured data

# simple sequences
1:10                         # sequence from 1 to 10
rev(1:10); 10:1              # countdown

# arithmetic sequence
seq(1, 9, by = 2)            # odd numbers
seq(1, 10, by = 2)           # as 11 > 10, 11 is not included
seq(1, 9, length = 5)        # 5 numbers only

# repeated numbers
rep(1,10)                    # repeat 1 ten times
rep(1:3,3)                   # repeat the sequence from 1 to 3 three times
rep(c("long", "short"), 
    c(1,2))                  # repeat "long" one time, "short" two times
	
# Problems 1.2.7

# Question 1.4
1+2*(3+4)                    # 1
4^3+3^(2+1)                  # 2
sqrt((4+3)*(2+1))            # 3
((1+2)/(3+4))^2              # 4

# Question 1.5
2+3-4; 2+(3-4); (2+3)-4      # 1
2+3*4; (2+3)*4; 2+(3*4)      # 2
2/3/4; (2/3)/4; 2/(3/4)      # 3
2^3^4; (2^3)^4; 2^(3^4)      # 4

# Question 1.6
p=c(2,3,5,7,11,13,17,19)     # 1
length(p)                    # 2

# Question 1.7
gas=c(65311,65624,65908,
      66219,66499,66821,
	  67145,67447)           # 1  
diff(gas)                    # 2 gives the difference of each fill-up mileage from the previous fill-up mileage
mean(gas)                    # 3 gives the average mileage of the car for the last 8 gas fill-ups
mean(diff(gas))              # 4 gives the average difference in mileage for the last 8 gas fill-ups

# Question 1.8
x=c(2,5,4,10,8)              # 1
x^2                          # 2
x-6                          # 3
(x-9)^2                      # 4

# Question 1.9
price=c(15.9,21.4,19.9,
        21.9,20.0,16.5,
		17.9,17.5)
min(price) ; max(price)      # 1
mean(price)                  # 2
mean(price)-min(price)
mean(price)-max(price)       # 3

# Question 1.10
Jan=c(2700,2600,3050,2900,
      3000,2500,2600,3000,
	  2800)  
Oct=c(3200,2800,3400)
H2=c(Jan,Oct)                # 1
cumsum(H2)                   # 2 Number of cars sold in 2002: 34550
diff(H2)                     # 3 
min(diff(H2)); max(diff(H2)) # 4 The greatest increase: December the greatest decrease: May

# Question 1.11
avcal=c(2450,2439,2866,2618)
percfatcal=c(0.37,0.362,0.34,0.321)
perccarbcal=c(0.422,0.431,0.481,0.50)
fatcal=avcal*percfatcal 
carbcal=avcal*perccarbcal    # there is no consistent increase or decrease in fat or carb calories
                             # we can not say anything about if there is a link between the increase in obesity and average fat calories, 
							 # because it is in consistent.
							 
# Question 1.12
rep("a",5)                   # 1
seq(1,100,by=2)              # 2
rep(c(1,2,3),c(3,3,3))       # 3
rep(c(1,2,3),c(3,2,1))       # 4
c(seq(1:5),rev(seq(1:4)))    # 5 or
c(1:5,4:1)                   # 5

# Question 1.13
fibo=c(1,2,3,5,8,13,21,34)   # 1
posint=seq(1:10)             # 2
recip=(1/seq(1:10))          # 3
cubes=(seq(1:6)^3)           # 4
years=seq(1964,2003)         # 5
stops=c(14,18,23,28,34,
        42,50,59,66,72,
		79,86,96,103,110)    # 6
zerothousand=seq(0,1000,
                 by=25)      # 7

# 1.3 Accessing data by using indices

# each observation is referred to by its index using square brackets
ebay <- c(88.8,88.3,90.2,93.5,95.2,94.7,99.2,99.4,101.6)
length(ebay)
ebay[1]                      # first value (88.8)
ebay[9]                      # last value (101.6)
ebay[length(ebay)]           # last value in case length is not known

# slicing a data vector
ebay[1:4]                    # first four entries
ebay[c(1,5,9)]               # first, fourth, and ninth entries

# negative indices
x[-i]                        # return all but the i th value of the vector x
ebay[-1]                     # all but the first
ebay[-(1:4)]                 # all but the 1st-4th

# accessing by name

# when data vector has names, then the values can be accessed by their names
# useful mostly for lists
x <- 1:3
names(x) <- c("one", "two", "three")
x["one"]
# lists will use double brackets, [[]], for indices

# 1.3.1 Assigning values to data vector

ebay[1] <- 88.0                                  # first value changed from 88.8 to 88.0
ebay[10:13] <- c(97.0,99.3,102.0,101.8)          # 10th-13th values changed with the vector
ebay

# data recycling

# if the vector assigned is shorter than the values to change, then vectors values are recycled.

# 1.3.2 Logical values

ebay > 100                   # returns a logical vector of the sequence of TRUEs and FALSEs
ebay[ebay > 100]             # values bigger than 100
which(ebay > 100)            # which indices (9,12,13)
ebay[c(9,12,13)]             # directly getting the values (if we know the indices)

# sum() will add up all TRUE values as 1 and all the FALSE values as 0

sum(ebay > 100)                                  # number bigger than 100 (3)
sum(ebay > 100) / length(ebay)                   # proportion of the values bigger than 100

# creating logical vectors by conditions
x <- 1:5 
x < 5                        # Is x smaller than 5? returns a logical vector for each x
x > 1                        # Is x bigger than 1? returns a logical vector for each x
x > 1 & x < 5                # returns a logical vector for each x 
x > 1 && x < 5               # returns a single value, FALSE, because the first value, 1, is not bigger than 1 but equal
x > 1 | x < 5                # returns a logical vector for each x 
x > 1 || x < 5               # returns a single value, TRUE, because the first value, 1, is smaller than 5
x == 3                       # Is x equal to 3? returns a logical vector for each x
x != 3 ; ! x == 3            # Is x not equal to 3? returns a logical vector for each x

# == allows us to compare a data vector with a value
# if we wish to use a range of values we can use the %in% operator

x %in% c(2,4)                # returns that 2nd and 4th entries of x are in the data vector c(2,4)

# 1.3.3 Missing values

shuttle <- c(0,1,0,NA,0,0)                       # there is no data for the forth flight
shuttle > 0                                      # logical vector... NA is in the answer
shuttle == NA                                    # doesn't work!
is.na(shuttle)                                   # works!
mean(shuttle)                                    # doesn't work because of NA in the vector
mean(shuttle, na.rm=TRUE)                        # works
mean(shuttle[!is.na(shuttle)])                   # hard way, na.rm=TRUE means x[!is.na(x)]

# 1.3.4 Managing the work environment

ls()                         # lists all the objects that have been defined or loaded into your work environment
ls(, pattern="")             # all objects with the string in their names will be listed
rm(); remove()               # used by specifying a name of the objects to be removed
rm("","",...)                # multiple names can be removed like this, or 
remove(list=c("","",...))    # like this

# Problems 1.3.5

# Question 1.14
commutetime=c(17,16,20,24,22,15,21,15,17,22)
max(commutetime)
mean(commutetime)
min(commutetime)
which(commutetime==24)
commutetime[4]=18
commutetime
mean(commutetime)
sum(commutetime>=20)
sum(commutetime<18)/length(commutetime)*100

# Question 1.15
dvd=c(79,74,161,127,133,210,99,143,249,249,368,302)
names(dvd)=c('JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG','SEP','OCT','NOV','DEC')
dvd
thirtyonedays <- dvd[c(1,3,5,7,8,10,12)]; 
remaining <- dvd[c(2,4,6,9,11)]
mean(thirtyonedays) ; mean(remaining)

# Question 1.16
bill=c(46,33,39,37,46,30,48,32,49,35,30,48)
sum(bill)
min(bill); max(bill)
sum(bill>40)
sum(bill>40)/length(bill)*100

# Question 1.17
salary=c(0.57,0.89,1.08,1.12,1.18,1.07,1.17,1.38,1.44,1.72)
salary
diff(salary)
sum(diff(salary)<0)
(diff(salary)/(length(salary)-1))*100            # percentage difference
max((diff(salary)/(length(salary)-1))*100)       # 1990

# Question 1.18    
x=c(1,3,5,7,9)
y=c(2,3,5,7,11,13)
x;y
x+1
y*2
length(x);length(y)
x+y                          # recycling
sum(x>5)
sum(x[x>5])
sum(x>5|x<3)
y[3]
y[-3]
y[x]                         # NA 7.ve 9. elemanın y'de bulunmadığını gösteriyor.
y[y>=7]

# Question 1.19
# because they are all strings

# 1.4 Reading in other sources of data

# 1.4.1 Using R's built-in libraries and data sets

library()                                        # lists all the installed packages
library(packagename)                             # load the package

data()                                           # lists all available data sets in loaded packages
data(package = "packagename")                    # lists all data sets for this package
data(datasetname)                                # load the data set, 
                                                 # not necessary after loading its package, the data set name can be directly used

data(datasetname, package = "packagename")       # loads just the data set without loading its package
                                                 # however, this will cause not to get any other info about its package
?datasetname                                     # finds help on this dat set
update.packages()                                # contact CRAN and interactively update installed packages
install.packages(packagename)                    # install the package from CRAN.
                                                 # use lib=argument to specify a nondefined directory for installation
                                                 # use contriburl=... to specify other servers (url) to find the package
												 
# accessing the variables in a data set: $, attach(), and with()

library(MASS)
names(geyser)                # what are variable   
geyser$waiting               # access "waiting", returns the data vector of variable "waiting"
geyser[["waiting"]]          # access "waiting" as a list

attach(); detach()           # used to avoid having to type the data frame name each time
                             # it allows to use only the variable name in a data set
							 # detach() clears out the attached variables, as these variables can be confusing to if used somewehere else as well
with(dataframe, command)     # with() performs attach() - detach() commands at once
#example
data(Sitka)                  # load data set
names(Sitka)                 # see variable names
length(Sitka$tree)           # use length function with the data frame name
#example
with(Sitka,range(tree))      # command with using just the variable name
#example
attach(Sitka)
summary(tree)
detach(Sitka)

# 1.4.2 Using the data sets that accompany this book

install.packages("UsingR")
library(UsingR)

# 1.4.3 Other methods of data entry

# cut and paste

# c() and scan() help copying and pasting from some other programs, sources
# if the data is separated by commas, then use c(input)
# if not, then use scan(input) instead. it reads the input until a blank line is entered

# using source() to read in R commands
dump("x", "somefile.txt")    # x will be written in a txt file, somefile, in the current working directory
getwd()                      # get the current working directory
source()                     # will read in the output of dump() to restore the objects

# reading data from formatted data sources
whale <- scan(file="whale.txt")                  # if the whale's data were stored in a file, whale.txt
                                                 # sep= for separator, comment.char= for comment lines
whale <- read.table("whale.txt", header = TRUE)  # if whale.txt contains data in tabular format, with numbers separated by space
                                                 # header includes information for the column name
read.csv()						                 # convinient way to import spreadsheet data 

# both read.table() and read.csv() return a data frame storing the data

# specifying the file
read.csv("whale.csv"); read.table("whale.txt")                       # specify with its name if the data file is in the same working directory
read.table("C:/R/whale.txt"); read.table("C:\\R\\whale.txt")         # specify with its path if not in the same wd. Both refer to the same file                                              
read.table(file = file.choose())                                     # choose the file interactively, rather than typing it

# finding files from the internet
site <- "website"
read.table(file = url(site), header = TRUE)      # url() is used only for clarity
read.table(file = site, header = TRUE)           # the file will be found without it as well 

# Problems 1.4.4

# Question 1.20
data(islands)
names(islands)
sort(islands,decreasing = TRUE)[1:7]

#Question 1.21
library(UsingR)
data(primes)
?primes
length(primes)
sum(primes<=100)
sum(primes>=100&primes<=1000)

# Question 1.22
sort(primes)                 # twin primes yapamadım. ?????SOR??????
primes[-1]                   # excludes the first value
n=length(primes); primes[-n] # excludes the nth value

# Question 1.23
data("treering")
length(treering)
min(treering)
max(treering)
sum(treering>1.5)

# Question 1.24
mandms
# Peanut butter, missing the color orange
# almond and kid minis
# milk chocolate, brown

# Question 1.25
nym.2002
nym.2002$time                # so 1001 entries
max(nym.2002$time)/60
0.446389*60                  # 9 hours 27 minutes
min(nym.2002$time)/60
0.455556*60                  # 2 hours 27 minutes

# Question 1.26
rivers
?rivers
max(rivers)                  # 3710
min(rivers)                  # 135

# Question 1.27
uspop
?uspop
names(uspop)=c('1790','1800','1810','1820','1830',
               '1840','1850','1860','1870','1880',
			   '1890','1900','1910','1920','1930',
			   '1940','1950','1960','1970')
                
max(diff(uspop))             # 1950-1960
