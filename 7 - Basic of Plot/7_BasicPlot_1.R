##
## 2021. 04. 06
## Plotting
##
library(tidyverse)
library(car)


## SCATTER PLOT
## Default S3 method:
# plot(x, y = NULL, type = "p",  xlim = NULL, ylim = NULL,
#      log = "", main = NULL, sub = NULL, xlab = NULL, ylab = NULL,
#      ann = par("ann"), axes = TRUE, frame.plot = axes,
#      panel.first = NULL, panel.last = NULL, asp = NA,
#      xgap.axis = NA, ygap.axis = NA,
#      ...)

head(cars)
plot(x = cars$speed, y = cars$dist)
plot(dist ~speed, data = cars) # long format의 dataframe이어야 함.

cars %>% 
  plot(dist ~speed, data = .,
       main = "Speed vs Distance", 
       xlab = "Speed (kph)",
       ylab = "Distance (m)") 
# 변수명은 놔두고 plotting할때 label을 바꾸는게 편리

# change points format
?pch
cars %>% 
  plot(dist ~speed, data = .,
       main = "Speed vs Distance", 
       xlab = "Speed (kph)",
       ylab = "Distance (m)",
       pch = 7)

# change point/title size
# cex(확대, 축소)
cars %>% 
  plot(dist ~speed, data = .,
       main = "Speed vs Distance", 
       xlab = "Speed (kph)",
       ylab = "Distance (m)",
       pch = 7, cex = 2, # point size
       cex.main = 2, # cex.main 배 만큼 크게 만듦.
       cex.lab = 1.5, # label size
       cex.axis = 1.5) # size of number on axis

# change font
# font
cars %>% 
  plot(dist ~speed, data = .,
       main = "Speed vs Distance", 
       xlab = "Speed (kph)",
       ylab = "Distance (m)",
       pch = 7, cex = 2,
       cex.main = 2, cex.lab = 1.5, cex.axis = 1.5,
       font.main = 8, font.lab = 8, font.axis = 8) 


# change colour
# col, there are around 400 colours already.
cars %>% 
  plot(dist ~speed, data = .,
       main = "Speed vs Distance", 
       xlab = "Speed (kph)",
       ylab = "Distance (m)",
       pch = 19, 
       cex = 2, cex.main = 2, cex.lab = 1.5, cex.axis = 1.5,
       font.main = 8, font.lab = 8, font.axis = 8,
       col = "red", col.main = "violet", col.lab = "tomato", col.axis = "blue"
       )
?palette
palette()
plot(1:10, cex = 3, pch = 19,
     col = 1:10)
# palette에서 색을 가져옴. recursive 하게 colour 지정.
# plot에서 숫자로 index를 사용하거나, "DF536B"의 값을 사용해도 됨.


## plotting of mtcars mpg vs wt
palette("Tableau")
palette()
mtcars %>% 
  plot(mpg ~wt, data = .,
       main = "MTCARS / mpg vs. wt",
       xlab = "weight", ylab = "mile per gallan",
       pch = 19, col = 3,
       cex = 1.5, cex.main = 2, cex.lab = 1.5, cex.axis = 1.5,
       font.main = 9, font.lab = 8, font.axis = 8,
       col.main = 4, col.axis = 5, col.lab = 2
       )
# check all parameters about plot
?par

## colour packages
#install.packages("RColorBrewer")
library(RColorBrewer)
display.brewer.all() # data type별로 색 조합을 사용하자.

mycolour_1 <- brewer.pal(n=8, name = "Accent")
palette(mycolour_1)
plot(1:10, col = 1:10, pch = 19, cex = 4)

mycolour_2 <- brewer.pal(n=8, name = "Set3")
palette(mycolour_2)
plot(1:10, col = 1:10, pch = 19, cex = 4)

palette("default")
plot(1:10, col = 1:10, pch = 19, cex = 4)

## cars
mycolour_1 <- brewer.pal(n=8, name = "Accent")
palette(mycolour_1)
plot(1:10, col = 1:10, pch = 19, cex = 4)

names(cars)
cars %>% 
  plot(dist ~speed, data = .,
       pch = 19, main = "speed vs. distance",
       cex = 1.5, cex.main = 2)

?abline # it helps line fitting
# a = intercept, b = slope
# abline(a = NULL, b = NULL, h = NULL, v = NULL, reg = NULL,
#        coef = NULL, untf = FALSE, ...)
# abline(-1, 2, col = "red")

lm.result <- cars %>% 
  lm(dist ~speed, data = .)

# abline(lm.result$coefficients[1], lm.result$coefficients[2],
#        col = "red")
abline(lm(dist ~speed, data = cars), col = "red")


## put text on my plot
?text
# ## Default S3 method:
# text(x, y = NULL, labels = seq_along(x$x), adj = NULL,
#      pos = NULL, offset = 0.5, vfont = NULL,
#      cex = 1, col = NULL, font = NULL, ...)
lm.result <- cars %>% 
  lm(dist ~speed, data = .)
coef(lm.result)

cars %>% 
  plot(dist ~speed, data = .,
       pch = 19, cex = 2,
       main = "speed vs. distance")
  abline(lm(dist~speed, data = cars), col = "blue")
  text(x = 8, y = 110,
       labels = "Y = -17.6 + 3.93x", col = "red",
       cex = 1.5)

## example: Ginzberg (depression vs. fatalism)
# scatter plot, fitted line and regression result
head(Ginzberg)
lm.result <- Ginzberg %>% 
  lm(depression ~fatalism, data = .)

Ginzberg %>% 
  lm(depression ~fatalism, data = .) %>% 
  summary()
# Adjusted R-squared: 클수록 좋음. 얼마나 regression line으로부터 퍼져있나.
# 클수록 line에 fitting 

Ginzberg %>% 
  plot(depression ~fatalism, data = .,
       pch = 19, cex = 1,
       main = "depression vs. fatalism", xlab = "Fatalism", ylab = "Depression",
       cex.main = 2, cex.xlab = 1.5, cex.ylab = 1.5,
       font.main = 9, font.xlab = 8, font.ylab = 8,
       col = 2, col.main = 1, col.lab = 3,
       xlim = c(-0.5,2.5), ylim = c(0, 2.5)
       )
abline(lm.result$coefficients, 
       col = "red", lwd = 2
       )
coef(lm.result)
text(x = 0.1, y = 1.75,
     labels = "y = 0.34 + 0.65x, p < 0.001, adj R^2 = 0.425", col = "red",
     cex = 1.5
     )

?plot

## data = iris, Sepal.Length vs. Sepal.Width
names(iris)
summary(iris$Species)

iris %>% 
  lm(Sepal.Width ~Sepal.Length, data = .) %>% 
  summary()

lm.result <- iris %>% 
  group_by(Species) %>% 
  lm(Sepal.Width ~Sepal.Length, data = .)
coef(lm.result)

iris %>% 
  plot(Sepal.Width ~Sepal.Length, data = .,
       cex = 1.5, pch = 19,
       col = iris$Species,
       main = "IRIS")
legend("topright",
       legend = c("setosa", "versicolor", "virginica"),
       pch = 19, col = 1:3, bty = "n",
       cex = 1.1, pt.cex = 2)

abline(lm.result$coefficients, col = "red")
# 실제로 speices 나눠보면 양의 상관관계이나
# 합치면 음의 상관관계이다.


## save images
?png
getwd()
png(filename = "myplot.png", width = 600, height = 400)
iris %>% 
  plot(Sepal.Width ~Sepal.Length, data = .,
       cex = 1.5, pch = 19,
       col = iris$Species,
       main = "IRIS")
legend("topright",
       legend = c("setosa", "versicolor", "virginica"),
       pch = 19, col = 1:3, bty = "n",
       cex = 1.1, pt.cex = 2)
dev.off()

?pdf
getwd()
pdf(file = "myplot.pdf", width = 4, height = 3)
iris %>% 
  plot(Sepal.Width ~Sepal.Length, data = .,
       cex = 1.5, pch = 19,
       col = iris$Species,
       main = "IRIS")
legend("topright",
       legend = c("setosa", "versicolor", "virginica"),
       pch = 19, col = 1:3, bty = "n",
       cex = 1.1, pt.cex = 2)
dev.off()

# 아래와 같이 3번 진행해서 각각의 선형회귀식을 따로 plotting
lm.res.setosa = iris %>% 
  filter(Species == "setosa") %>% 
  lm(Sepal.Width ~Sepal.Length, data = .)

## BAR PLOT
?mtcars
head(mtcars)

# cross table to make bar plot
mytbl <- table(mtcars$vs, mtcars$cyl, 
      dnn = c("Engine", "Cylinder") # naming
      )

barplot(t(mytbl),  # transpose
        beside = T # wide하게 펼침
)

barplot(mytbl, 
        beside = T, # wide하게 펼침
        main = "Engine type vs. Number of cylinders",
        xlab = "# of Cylinders", ylab = "Frequency",
        names.arg = c("4 cyls", "6 cyls", "8 cyls"),
        cex.main = 2, cex.names = 1.5, cex.lab = 1.5,
        font.main = 9, font.lab =8,
        ylim = c(0, 18),
        legend.text = c("V-shape", "Straight"),
        args.legend = list(x = "topleft", bty = "n")
        )

# VS vs. AM
mytbl <- table(mtcars$vs, mtcars$am, 
               dnn = c("Engine", "Transmission") # naming
              )
barplot(mytbl, 
        beside = T, # wide하게 펼침
        main = "Engine type vs. Transmission",
        xlab = "Transmission", ylab = "Frequency",
        names.arg = c("automatic", "manual"),
        cex = 1, # size of y axis number
        cex.main = 2, cex.names = 1.5, cex.lab = 1.5,
        font.main = 9, font.lab =8,
        ylim = c(0, 18),
        legend.text = c("V-shape", "Straight"),
        args.legend = list(x = "topright", bty = "n")
        )














