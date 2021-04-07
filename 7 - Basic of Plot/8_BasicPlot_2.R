##
## 2021. 04. 07
## 
##

library(tidyverse)
library(car)
library(RColorBrewer)

##
## palette
##
mycolour_1 <- brewer.pal(n=8, name = "Accent")
palette(mycolour_1)
plot(1:10, col = 1:10, pch = 19, cex = 2)

##
## iris Sepal.Width ~Sepal.Length by group with regression
##
lm.result <- iris %>% 
  lm(Sepal.Width ~Sepal.Length, data = .)
iris %>% 
  plot(Sepal.Width ~Sepal.Length, data = .,
       cex = 1, pch = 19,
       col = iris$Species,
       main = "IRIS")
legend("topright",
       legend = c("setosa", "versicolor", "virginica"),
       pch = 19, col = 1:3, bty = "n",
       cex = 1.1, pt.cex = 2)
abline(lm.result$coefficients, col = "red", lty = 2)

##
## mtcars cyl vs. vs with legend
##
mytbl <- table(mtcars$am, mtcars$cyl, 
               dnn = c("Transmission", "Cylinders") # naming
)
# barplot(t(mytbl),  # transpose
#         beside = T # wide하게 펼침
# )
barplot(mytbl, 
        beside = T, # wide하게 펼침
        main = "mtcars data",
        xlab = "Number of Cylinders", ylab = "Frequency",
        names.arg = c("4 cyls", "6 cyls", "8 cyls"),
        cex.main = 2, cex.names = 1.5, cex.lab = 1.5,
        font.main = 9, font.lab =8,
        col = 2:3,
        legend.text = c("Automatic", "Manual"),
        args.legend = list(x = "topleft", bty = "n")
        )

# put measurement on the bar graph
bar_x <- barplot(mytbl, 
                 beside = T, # wide하게 펼침
                 main = "mtcars data",
                 xlab = "Number of Cylinders", ylab = "Frequency",
                 names.arg = c("4 cyls", "6 cyls", "8 cyls"),
                 cex.main = 2, cex.names = 1.5, cex.lab = 1.5,
                 font.main = 9, font.lab =8,
                 col = 2:3, ylim = c(0, 14),
                 legend.text = c("Automatic", "Manual"),
                 args.legend = list(x = "topleft", bty = "n")
                 )
text(bar_x, mytbl, labels = mytbl,
     pos = 3)

##
## ToothGrowth
##
table(ToothGrowth$supp, ToothGrowth$dose)
?sample_frac
ToothGrowth_sample <- ToothGrowth %>% 
  sample_frac(0.7)
table(ToothGrowth_sample$supp, ToothGrowth_sample$dose)

names(ToothGrowth_sample)

mytable2 <- table(ToothGrowth_sample$dose, ToothGrowth_sample$supp,
                  dnn = c("dose", "supp"))
bar_xx <- barplot(mytable2, beside = T,
                 main = "ToothGrowth sample",
                 xlab = "supp", ylab = "Frequency",
                 cex.main = 2, cex.names = 1.5, cex.lab = 1.5,
                 font.main = 9, font.lab = 8,
                 col = 2:4, ylim = c(0,12),
                 legend.text = c("0.5mg", "1.0mg", "2.0mg"),
                 args.legend = list(x = "topright", bty = "n"),
                 names.arg = c("orange juice", "ascorbic acid")
                 )
text(bar_xx, mytable2, labels = mytable2,
     pos = 3)

##
## pie plot
##
pie(rep(1,8), col = 1:8,
    clockwise = T)

palette(brewer.pal(n=8, "Set3"))
pie(rep(1,8), col = 1:8,
    clockwise = T)

n <- 12
pie(rep(1, n), col = rainbow(n), clockwise = T)
pie(rep(1, n), col = rainbow(n, alpha = 0.6), clockwise = T)
pie(rep(1, n), col = heat.colors(n), clockwise = T)
pie(rep(1, n), col = topo.colors(n), clockwise = T)
pie(rep(1, n), col = terrain.colors(n), clockwise = T)

##
## Histogram
##
?hist

# hist의 목적은 자료의 분포 파악
# 모양에 관심을 갖지, 높이에 관심은 없다.
hist(mtcars$mpg, nclass = 10) # nclass = bin

# 확률적으로 분포로 나타냄.
hist(mtcars$mpg, nclass = 10, freq = F)

display.brewer.all()

hist(mtcars$mpg, freq = F, nclass = 10,
     col = heat.colors(n = 10))

palette(brewer.pal(n = n, name = "Oranges"))
br_pnt <- seq(min(mtcars$mpg), max(mtcars$mpg), length.out = 21)
hist(mtcars$mpg, freq = F, breaks = br_pnt, 
     # col = rev(heat.colors(n = length(br_pnt))),
     main = "자동차 연비 분포",
     xlab = "mile per gallon",
     cex.main = 2, font.main = 9, col.main = "blue",
     cex.lab = 1.3, font.lab = 8,
     density = 80, border = "black"
     )
dd <- density(mtcars$mpg)
lines(dd, col = "red")

plot(dd, col = "red")

## airquality$Ozone
?airquality
head(airquality)

n <- 12
df <- airquality$Ozone
my_bre <- seq(min(df, na.rm = T), max(df, na.rm = T), length.out = (n+1))

# 농도가 높을수록 위험하므로 RED를 뒤쪽으로 나오게 reverse
hist(airquality$Ozone, breaks = my_bre,
     col = rev(heat.colors(n = n, alpha = 0.6)), 
     main = "오존 분포",
     xlab = "Ozone", freq = F,
     cex.main = 2, font.main = 9, col.main = "blue",
     cex.lab = 1.3, font.lab = 8,
     density = 80, border = "black"
     )

oz_density <- density(airquality$Ozone, na.rm = T)
lines(oz_density, col = "blue", lwd = 2)


##
## Box plot, 연속형 변수
##
boxplot(airquality$Ozone)

boxplot(chickwts$weight, range = 1.5) # range = Q3 + range*IQR
boxplot(chickwts$weight, range = 0.5,
        notch = T, # median 강조
        outline = T # outline 제거
        )

boxplot(chickwts$weight, range = 0.5,
        notch = T, 
        outline = T,
        # col = rainbow(n = 2, alpha = 0.6)
        col = 1, # colour는 palette에 저장하고 쓰는게 편하다.
        border = "red",
        pars = list(boxwex = 0.8, # 박스의 폭이 변함.
                    staplewex = 1), # max, min 막대기의 폭이 변함.(boxwex 폭에 비례)
        horizontal = T #boxplot을 눕힘
        )
chickwts %>% 
  boxplot(weight ~feed, data = .,
          range = 0.5, notch = F, 
          outline = T, col = 1, 
          border = "red",
          pars = list(boxwex = 0.8, 
                      staplewex = 1),
          horizontal = F,
          main = "사료 vs 몸무게",
          xlab = "사료", ylab = "몸무게",
          names = LETTERS[1:6]
          )

## ToothGrowth supp*dose
ToothGrowth %>% 
  boxplot(len ~supp*dose, data = .,
          range = 0.5, notch = F, 
          outline = T, col = 1, 
          border = "red",
          pars = list(boxwex = 0.8, 
                      staplewex = 1),
          horizontal = F,
          main = "치아 길이",
          xlab = "treatment"
          # names = LETTERS[1:6]
          )

mycolour <- rainbow(n=6, alpha = 0.4)
ToothGrowth %>% 
  boxplot(len ~dose*supp, data = .,
          range = 0.5, notch = F, 
          outline = T, col = mycolour[1:6], 
          border = "red",
          pars = list(boxwex = 0.8, 
                      staplewex = 1),
          horizontal = F,
          main = "치아 길이",
          xlab = "treatment",
          at = c(1:3, 5:7) # 중간에 term을 두어 그래프 해석에 용이이
          # names = LETTERS[1:6]
  )
head(ToothGrowth)


##
## stripchart, 자료를 mapping
##
mycolour <- rainbow(n=6, alpha = 0.4)
ToothGrowth %>% 
  boxplot(len ~dose*supp, data = .,
          range = 0.5, notch = F, 
          outline = T, col = mycolour[1:6], 
          border = "red",
          pars = list(boxwex = 0.8, 
                      staplewex = 1),
          horizontal = F,
          main = "치아 길이",
          xlab = "treatment",
          at = c(1:3, 5:7) # 중간에 term을 두어 그래프 해석에 용이이
          # names = LETTERS[1:6]
  )

ToothGrowth %>% 
  stripchart(len ~dose*supp, data = .,
             vertical = T, add = T, 
             at = c(1:3, 5:7),
             method = "jitter",
             pch = 21,
             col = "maroon",
             bg = "bisque")

##
## multilayer
##
par(mfrow = c(2, 2))
iris %>% 
  plot(Sepal.Width ~Sepal.Length, data = .)
iris %>% 
  boxplot(Sepal.Width ~Species, data = .)
hist(iris$Sepal.Width)
pie(table(iris$Species))




