##
## 2021. 04. 09
## ggplot 
##
library(tidyverse)
library(car)
library(RColorBrewer)
library(plotly)

##
## RECAP
##
# 현재 parameter 저장
opar <- par(no.readonly = T)
# plot 분할
par(mfrow = c(2, 2)) # (rows, cols)
## iris
names(iris) # "Sepal.Length" "Sepal.Width" "Petal.Length" "Petal.Width" "Species"
palette(brewer.pal(n = 8, "Accent"))
# display.brewer.all()

# scatter + regression
iris %>% 
  plot(Petal.Width ~Petal.Length, data = .,
       main = "scatter + regression", xlab = "length", ylab = "width",
       pch = 19, cex = 2, col = iris$Species,
       cex.main = 2, cex.label = 1.5, cex.axis = 1.2,
       font.main = 9, font.label = 8,
       col.main = "blue", col.label = "gray"
       )
abline(lm(Petal.Width ~Petal.Length, data = iris), 
       col = "red", lwd = 2)
legend("topleft", legend = c("setosa", "versicolor", "virginica"),
       col = 1:3, pch = 19, bty = "n")
text(labels = "y = -0.36 + 0.41x", x = 5, y = 0.5, 
     col = "red")
# boxplot
iris %>% 
  boxplot(Petal.Width ~Species, data = .,
          main = "boxplot", ylab = "width",
          col = 1:3,
          font.main = 9, font.label = 8,
          horizontal = F, border = "red",
          pars = list(boxwex = 0.5, staplewex = 1)
          )
# histogram
n <- 12
df <- iris$Petal.Width
my_break <- seq(min(df), max(df), length.out = n+1)
hist(df, breaks = my_break, 
     col = rev(heat.colors(n, alpha = 0.6)),
     main = "histogram + density",
     freq = F
     )
dd <- density(df)
lines(dd, col = "red")
# barplot
my_tbl <- table(iris$Species)

bar_x <- barplot(my_tbl,
        ylim = c(0,60), 
        main = "bar", ylab = "frequency", xlab = "species"
        )
text(bar_x, my_tbl, labels = my_tbl,
     pos = 3, col = "red", font = 4)
# 기존의 plot parameter로 복원
par(opar)


##
## ggplot
##
library(ggplot2)
my_ggplt <- ggplot(data = diamonds)

# 데이터 양이 너무 많기 때문에, sampling 
my_dia <- diamonds %>% 
  sample_frac(0.1)

# ggplot() + 
#   geom_point(data = my_dia, aes(x = carat, y = price))

my_dia %>% 
  ggplot() + 
  geom_point(aes(x = carat, y = price), # 변수는 axis(aes)에 넣어주어야함.
             color = 2, size = 1.5, alpha = 0.4,
             shape = 1)

my_dia %>% 
  ggplot() + 
  geom_point(aes(x = carat, y = price, color = cut), # 각 수준바다 따로 그림.
             size = 1.5, alpha = 0.4,
             shape = 19)

my_dia %>% 
  ggplot() + 
  geom_point(aes(x = carat, y = price, color = cut), # 각 수준바다 따로 그림.
             size = 1.5, alpha = 0.4,
             shape = 19
             ) +
  geom_smooth(aes(x = carat, y = price),
              method = "lm" # linear fit
              ) # fitting line + confidence line

my_dia %>% 
  ggplot() + 
  geom_point(aes(x = carat, y = price, color = cut), # 각 수준바다 따로 그림.
             size = 1.5, alpha = 0.4,
             shape = 19
  ) +
  geom_smooth(aes(x = carat, y = price, color = cut),
              method = "lm" # linear fit
  ) # fitting line + confidence interval

# data 변수 상속가능.
my_dia %>% 
  ggplot(aes(x = carat, y = price, color = cut)) + 
  geom_point(size = 1.5, alpha = 0.4,
             shape = 19
             ) +
  geom_smooth(method = "lm", linetype = "dashed", 
              se = F) # se: confidence interval

# smooth type
my_dia %>% 
  ggplot(aes(x = carat, y = price, color = cut)) + 
  geom_point(size = 1.5, alpha = 0.4,
             shape = 19
  ) +
  geom_smooth(method = "loess", linetype = "dashed", 
              se = F) # 구간별 끊어서 평균을 구한 뒤, 직선으로 연결.

# 다른 변수 사용용
my_dia %>% 
  ggplot(aes(x = carat, y = price, color = color)) + 
  geom_point(size = 1.5, alpha = 0.4,
             shape = 19
  ) +
  geom_smooth(method = "loess", linetype = "dashed", 
              se = F) # 구간별 끊어서 평균을 구한 뒤, 직선으로 연결.

# main, label, axis limitation
my_ggplt_1 <- my_dia %>% 
  ggplot(aes(x = carat, y = price, color = cut)) + 
  geom_point(size = 1.5, alpha = 0.4,
             shape = 19
  ) +
  geom_smooth(method = "loess", linetype = "dashed", 
              se = F, show.legend = T) +
  ylim(c(0, 15000)) + xlim(c(0,3)) +
  labs(title = "Diamods with ggplot", 
       subtitle = "friday study",
       y = "price", x = "carat",
       caption = "courtesy from ggplot")

my_ggplt_2 <- my_ggplt_1 + scale_color_brewer(palette = "Set1")

plot(my_ggplt_2)

# label 조정
my_ggplt_3 <- my_ggplt_2 + 
  scale_x_continuous(breaks = seq(0,3, by = 1),
                     labels = fruit[1:4]) +
  xlim(c(0, 3)) + 
  theme_bw() 

plot(my_ggplt_3)

# install.packages("plotly")
library(plotly)
# 구체적인 값들을 볼 수 있음.
# html로 저장해서 presentation할 때 사용.
ggplotly(my_ggplt_3)


#
# scatter
#
my_ggplt_4 <- my_dia %>% 
  ggplot(aes(x = carat, y = price, color = cut)) + 
  geom_point(aes(size = exp(x)), alpha = 0.4, # scatter point size를 변수에 대해 다르게 조정정
             shape = 19
  ) +
  geom_smooth(method = "loess", linetype = "dashed", 
              se = F, show.legend = T) +
  ylim(c(0, 15000)) + xlim(c(0,3)) +
  labs(title = "Diamods with ggplot", 
       subtitle = "friday study",
       y = "price", x = "carat",
       caption = "courtesy from ggplot") +
  scale_color_brewer(palette = "Set1") +
  scale_x_continuous(breaks = seq(0,3, by = 1),
                     labels = fruit[1:4]) +
  xlim(c(0, 3)) + 
  theme_bw() 

ggplotly(my_ggplt_4)


#
# barplot
#
diamonds %>% 
  ggplot(aes(x = clarity, aes(fill = cut))) + 
  geom_bar(stat = "count", position = "dodge") +
  scale_x_discrete(labels = fruit[1:8])
  
# flip  
diamonds %>% 
  ggplot(aes(x = clarity)) + 
  geom_bar(stat = "count", position = "dodge") +
  scale_x_discrete(labels = fruit[1:8]) +
  coord_flip()
  
## sorting by 'n'
# fill을 넣게되면, legend의 이름을 바꿔줘야함.
# stat = "identity": y 값이 있기 때문에 y값을 받아서 써라.
# stat = "count": 기계가 스스로 카운트해서 y값을 넣어라.
myppl <- diamonds %>% 
  group_by(clarity) %>% 
  summarise(n_clarity = n()) %>% 
  ggplot(aes(x = fct_reorder(clarity, n_clarity), 
             y = n_clarity, fill = clarity)) + 
  geom_bar(stat = "identity", show.legend = F) +
  scale_x_discrete(labels = fruit[1:8]) +
  labs(x = "fruits", y = "frequency") +
  coord_flip() +
  # scale_color_manual(labels = c("I1", "SI2", ... ,""),
  #                    values = c("apple", "apricot", "avocado", "banana", "bell pepper", ... ))
  # scale_fill_discrete(name = "fruit",
  #                     labels = fruit[1:8])
  
ggplotly(myppl)

#
# histogram
#
diamonds %>% 
  filter(cut %in% c("Premium", "Ideal")) %>% 
  ggplot(aes(x = price, color = cut)) +
  geom_histogram(aes(y = ..density..), bins = 30, alpha = 0.4, # y값을 density로 바꾼다.
                 fill = "white", position = "identity") + # identity
  geom_density(size = 1.5) + # density fitting
  scale_color_brewer(palette = "Set2") +
  theme(legend.position = "top") + # bottom
  labs(color = "Cutting stat")

#
# boxplot
#
# tidyverse는 data structure에 굉장히 민감.
ToothGrowth %>% 
  mutate(dose = factor(dose)) %>% 
  ggplot(aes(x = dose, y = len, color = supp)) +
  geom_boxplot(alpha = 0.4, 
               outlier.shape = 21, outlier.size = 3) + 
  stat_summary(aes(group = supp), fun.y = mean,
               position = position_dodge(0.75)) +
  geom_jitter(position = position_jitter(0.1))
  
















