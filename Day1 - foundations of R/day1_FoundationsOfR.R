##
## 2021. 03. 29
## Foundations of R language
##

## $getwd(): get working directory
## $setwd(): set working directory, e.g. $setwd("C:/Users/User/Desktop/r_studio")

## Grammar of R is similar as c++ 
## shortcut of <- is "alt + -"
## my data
df <- cars 

## scala
2+3
3*pi
3/pi
3^4

## integer division
23 %/% 2 # ans: 11
23 %% 2 # ans: 1

## log
log(10) # log_exp(1)
log(10, base=10) # base = 10
## $?log: description of log

## parameter searching function
## $?XXX

## exponential
exp(1)

## vector
x <- 2
x <- 1:10 # if it is sequence data, i can use without c()
x <- c(2, 3, 5) # concatenate

## vector wise operation
x+1
2*x + 1
x %% 2

## using mtcars: default database
## searching default databases: data()
mtcars <- mtcars
## extract specific column.
mtcars$cyl

## Frequency table
mtcars_tb <- table(mtcars$cyl)
barplot(mtcars_tb) # focus on heights
hist(mtcars$mpg) # focus on the shape
boxplot(mtcars$mpg) # description of frequency of data

## chicken weights data
hist(chickwts$weight)
boxplot(chickwts$weight)

## mtcars
summary(mtcars)


## lab
am_tbl <- table(mtcars$am)
barplot(am_tbl)

hist(mtcars$wt)
boxplot(mtcars$wt)

summary(mtcars$wt)

aggregate(wt~ cyl, 
          data=mtcars,
          summary)

var(mtcars$wt)
aggregate(wt~ cyl, 
          data=mtcars,
          var)

IQR(mtcars$wt) # type = 7: there are 7 ways to get IQR value
