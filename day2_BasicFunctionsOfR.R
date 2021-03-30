##
## 2021. 03. 30
## Basic functions of R
##

## IQR calculation
df <- c(2,3,5,7,9,10)

## distribution table
mydf2 <- table(mtcars$cyl, mtcars$am)
barplot(mydf2) # file the chart

mydf <- table(mtcars$cyl, mtcars$am,
              dnn = list(c = "Cylinder",
                         r = "Transmission"))
barplot(mydf, beside = T, legend.text = T) # beside: put on the same baselines

## Correlation
x <- c(88, 55, 66, 62, 69)
y <- c(73, 65, 70, 62, 68)

mean_x <- mean(x)
mean_y  <- mean(y)

var_x <- sum((x - mean_x)^2) / (5-1)
var_y <- sum((y - mean_y)^2) / (5-1)

sd_x <- sqrt(var_x)
sd_y <- sqrt(var_y)

cov_xy <- sum((x - mean_x)*(y - mean_y)) / (5-1)

piearson_r <- cov_xy / (sd_x*sd_y)

df <- data.fram(x = x, y = y)
plot(df)
lines(lm(y~x, data = df), col="red")


##
a <- c(1:10) # numeric type
b <- c(TRUE, FALSE, FALSE) # logical
c <- c("Red", "Blue", "Black") # char

my_chr_df <- vector("character", length = 5)
my_logi_df <- vector("logical", length = 5)
my_num_df <- vector("numeric", length = 5)
my_int_df <- vector("integer", length = 5)

## Matrix
mymat_byrow_F <- matrix(data = 1:12, nrow = 2) # ncol is set automatically
mymat_byrow_T <- matrix(data = 1:12, nrow = 2, byrow = T) # ncol is set automatically
mymat_dimname <- matrix(data = 1:12, nrow = 3, 
                              byrow = F,
                              dimnames = list(rname = c("r1", "r2", "r3"),
                                              cname = c("c1", "c2", "c3", "c4")))

mymat_dimname[2, 3]
mymat_dimname[2:3, 3]
mymat_dimname[1, ]
mymat_dimname[1, c(1,3)]

## Data Frame
mydf <- data.frame(v1 = c(1:4),
                   v2 = letters[1:4],
                   v3 = vector("logical", length=4)) # v1= variable 1

mydf$v2


## List
mylist <- list(academy = "YOUNG WOO",
               class = "Daata science",
               score = c(89, 88))

mylist$score
mylist[[3]]


## Data view
boxplot(weight~ feed, data = chickwts)
head(chickwts)
dim(chickwts)
names(chickwts)
str(chickwts)


## install packages

# $install.packages("tidyverse")
# $library(tidyverse) # have to type this command whenever turning on R-studio
# CRAN: Official storage of R packages (like Github)


## save data to csv file
mydf_save <- data.frame(v1 = c(1:4),
                   v2 = letters[1:4],
                   v3 = vector("logical", length=4)) # v1= variable 1

write.csv(x = mydf_save, 
          file = "mydf_save.csv") # csv = comma separate variate

## read csv file
# $install.packages("xlsx")
# $install.packages("rJava")
# $library(xlsx)

## bind
xx <- 1:4
yy <- 5:8
cbind(xx, yy)
rbind(xx, yy)

x <- c(1/0, NA, 2, NA, NaN) # NaN: not a number, Inf: infinite
is.na(x) # NA always make errors, so need to check whether NA is 
is.infinite(x)
which(is.na(x))


## convert class(data type)
mymat <- matrix(data = 1:12, nrow = 3, 
                        byrow = F,
                        dimnames = list(rname = c("r1", "r2", "r3"),
                                        cname = c("c1", "c2", "c3", "c4")))
class(mymat)
mymat_df <- as.data.frame(mymat)
class(mymat_df)
mymat_df$c1

## R has 2 grammars
# 1- base (e.g. rep{base})
# 2- tidy

## R basic function
# sequence
seq(from=1, to=10, by=2)
seq(1, 10, 2)
seq(1, 10, length.out=4)
seq_along(mtcars) # the number of variables
dim(mtcars)

# replication
rep(1:4, times = 2)
rep(1:4, each = 2, times = 2)
rep(1:4, each = 2, times = 2, length.out = 4)

# sampling
sample(x = 1:10, size = 5, replace = F) # extract only once 비복원추출
sample(x = 1:10, size = 5, replace = T) # can extract the same number 복원추출

# vector operator
vec1 <- 1:8
vec1 <= 2 | vec1 <= 4
vec1 <= 2 & vec1 <= 4

# sorting
v1 <- c(40, 35, 32, 50, 22)
sort(v1, decreasing = T) # without index number
order(v1) # show index number

