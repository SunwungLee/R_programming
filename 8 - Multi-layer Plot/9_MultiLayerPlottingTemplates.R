##
## 2021. 04. 07
## multi plotting template
##

##
## mtcars 
## 1. scatter + regression  : mpg ~wt 
## 2. boxplot + scriptchart :
## 3. histogram + density   : mpg ~wt
## 4. pie chart             : cyl
##
## etc. bar plot
##

library(tidyverse)
library(car)
library(RColorBrewer)
display.brewer.all() # data type별로 색 조합을 사용하자.
palette(brewer.pal(n=8, "Set3"))
mycolour <- rainbow(n=6, alpha = 0.4)

##
## PNG saving
##
# png(filename = "myplot.png", width = 1920, height = 1080)


##
## Create multi-layer
##
par(mfrow = c(2, 2))

##
## 1. scatter + regression
## mpg ~wt
##
mtcars %>% 
  plot(mpg ~wt, data = .,
       pch = 21, col = "maroon",
       bg = "bisque",
       main = "Scatter + Regression",
       xlab = "weight", ylab = "mile per gallan",
       cex = 1.5, cex.main = 2, cex.lab = 1.3, cex.axis = 1.3,
       font.main = 9, font.lab = 8, font.axis = 8,
       col.main = 4, col.axis = "black", col.lab = "black"
  )
lm.result <- mtcars %>% 
  lm(mpg ~wt, data = .)
mtcars %>% 
  lm(mpg ~wt, data = .) %>% 
  summary()
abline(lm.result$coefficients, col = "blue",
       lty = 2)
text(x = 4, y = 30,
     labels = "y = 37.28 - 5.344 x, p < 0.001, adj R^2 = 0.7446", col = "blue",
     cex = 1.5
)

##
## 2. box 
## 
##
mtcars %>% 
  boxplot(mpg ~cyl, data = .,
          range = 1.5,
          notch = F, outline = T,
          col = mycolour[1:3], border = "red",
          pars = list(boxwex = 0.6,
                      staplewex = 1),
          horizontal = F,
          main = "Boxplot + Stripchart",
          xlab = "", ylab = "MPG",
          names = c("4 cyls", "6 cyls", "8 cyls"),
          #at = c(1:3),
          font.main = 9, font.lab = 8, font.axis = 8,
          col.main = 4, col.axis = "black", col.lab = "black"
          )

mtcars %>% 
  stripchart (mpg ~cyl, data = .,
              vertical = T,
              add = T,
              #at = c(1:3),
              method = "jitter",
              pch = 21, col = "maroon", bg = "bisque"
              )


##
## 3. histogram + density 
## mpg ~wt
##
hist(mtcars$mpg,
     col = rev(heat.colors(n = 12, alpha = 0.6)),
     main = "Histogram + density",
     xlab = "Mile per gallon", freq = F,
     cex.main = 2, cex.lab = 1.3,
     font.main = 9, font.lab = 8,
     density = 80, border = "black",
     font.main = 9, font.lab = 8, font.axis = 8,
     col.main = 4, col.axis = "black", col.lab = "black",
     nclass = 10
     )
mpg_density <- density(mtcars$mpg, na.rm = T)
lines(mpg_density, col = "blue", lwd = 2)


##
## 4. pie
## 
##
mytbl <- table(mtcars$cyl)
pie(mytbl, col = 1:3,
    clockwise = F, 
    main = "Pie ",
    #xlab = "weight", ylab = "mile per gallan",
    cex.main = 2, #cex = 1.5,
    font.main = 9, font.lab = 8, font.axis = 8,
    col.main = 4, #col.axis = "black", col.lab = "black"
    )



##
## PNG saving file close
##
# dev.off()






##
## BAR PLOT
## mtcars cyl vs. vs with legend
##

# mytbl <- table(mtcars$am, mtcars$cyl, 
#                dnn = c("Transmission", "Cylinders") # naming
# )
# # barplot(t(mytbl),  # transpose
# #         beside = T # wide하게 펼침
# # )
# barplot(mytbl, 
#         beside = T, # wide하게 펼침
#         main = "mtcars data",
#         xlab = "Number of Cylinders", ylab = "Frequency",
#         names.arg = c("4 cyls", "6 cyls", "8 cyls"),
#         cex.main = 2, cex.names = 1.5, cex.lab = 1.5,
#         font.main = 9, font.lab =8,
#         col = 2:3,
#         legend.text = c("Automatic", "Manual"),
#         args.legend = list(x = "topleft", bty = "n")
# )
