##
## 2021. 04. 02
## T-test
##

library(tidyverse)

## shapiro-wilk test 정규성 검정
# T test하기전 반드시 검정을 해야한다.
set.seed(7201)
df <- round(rnorm(n = 20, mean = 95, sd = 4), 0)

shapiro.test(df) # p-value is over alpha value.

df_exp <- rexp(n = 20)
shapiro.test(df_exp)

##
mtcars %>% 
  select(mpg, wt) %>% 
  plot()
  
mtcars %>% 
  select(c(am:carb)) %>% 
  head()

mtcars %>% 
  select(-c(am:carb)) %>% 
  head()

## iris data
## select 
# column direction
iris %>% 
  select(c(Petal.Length, Petal.Width, Species)) %>% 
  head()

iris %>% 
  select(starts_with("Petal"), Species) %>% 
  head()

iris %>% 
  select(ends_with("width"), Species) %>% 
  head()

iris %>% 
  select(Species, everything()) %>% 
  head()
#iris[, c(5, 1:4)]

iris %>% 
  select(contains("."), Species) %>% 
  head()

iris %>% 
  select(matches("[pt]al"), Species) %>% 
  head()
# p나 t를 포함하고 l로 끝나는거 뽑아라

iris %>% 
  select(last_col()) %>% 
  head()

boxplot(Sepal.Width ~Species, data=iris)

## filter
# row direction
iris %>% 
  filter(Species == "setosa") %>% 
  head()

## ToothGrowth
ToothGrowth %>% 
  boxplot(len ~supp*dose, data = .)

ToothGrowth %>% 
  filter(supp == "OJ", dose == 1) %>% # comma is and gate, or gate is |
  head()

ToothGrowth %>% 
  filter(row_number() == 1) %>% 
  head()

ToothGrowth %>% 
  filter(row_number() == n()) %>% # n() is the last number 
  head()

ToothGrowth %>% 
  filter(between(row_number(), 5, n())) %>% # n() is the last number 
  head()

# Group_by
ToothGrowth %>% 
  filter(len > mean(len)) #전체의 평균

ToothGrowth %>% 
  group_by(supp) %>% 
  filter(len > mean(len)) %>%  # supp 별로 평균을 구해서
  tail()

ToothGrowth %>% 
  group_by(supp, dose) %>% 
  filter(len > mean(len))

ToothGrowth %>% 
  group_by(supp, dose) %>% 
  summarise(mean = mean(len), 
            sd = sd(len)) # 굉장히 많이 사용된다. group_by, 많은양을 한번에 처리하기 좋음.

# Sort, order
ToothGrowth %>% 
  #arrange(len) # 오름차순 정렬
  arrange(desc(len)) %>% 
  head()


## mtcars
# summarise
mtcars %>% 
  summarise(mean = mean(mpg), 
            sd = sd(mpg),
            median = median(mpg))
# mutate는 뒤에 새로운 column을 만듦. but summarise는 당장 값만 도출
# summarise + group_by
mtcars %>% 
  group_by(am) %>% 
  summarise(mean = mean(mpg), 
            sd = sd(mpg),
            median = median(mpg))

mtcars %>% 
  group_by(am, vs) %>% # 경우의 수 별로 group을 묶어서 밑의 값들을 계산 
  summarise(mean = mean(mpg), 
            sd = sd(mpg),
            median = median(mpg))

#
mtcars %>% 
  group_by(cyl) %>% 
  summarise(tot_n = n(),
            n5th = nth(mpg, 5),
            mean = mean(mpg),
            median = median(mpg),
            sd = sd(mpg),
            min = min(mpg),
            max = max(mpg),
            iqr = IQR(mpg))

## case_when
x <- 1:10
case_when(
  x %% 2 == 0 ~ "A", # x를 2로 나눈 나머지 0이면, "A" 라고 해라.
  x %% 3 == 0 ~ "B",
  x %% 5 == 0 ~ "C",
  TRUE ~ as.character(x) # TRUE: 나머지는, character 로 x를 저장해라. 
)

x <- 60:100
case_when(
  x >= 90 ~ "A", # x를 2로 나눈 나머지 0이면, "A" 라고 해라.
  x >= 80  ~ "B",
  x >= 70  ~ "C",
  TRUE ~ as.character(x) # TRUE: 나머지는, character 로 x를 저장해라. 
)

## 
x <- c(1, 3, 5, 4, 6)
cumsum(x) # 누적한 덧셈
cummean(x) # 누적한 평균


## Chickweight
# diet 기준, weight에 대한 값들을 구해보자.
ChickWeight %>% 
  group_by(Diet) %>% 
  summarise(tot_nub = n(),
            wt_mean = mean(weight),
            wt_sd = sd(weight),
            wt_iqr = IQR(weight),
            wt_min = min(weight),
            wt_max = max(weight))


## PlantGrowth
# 각 그룹별로 weight의 평균 이상인 것들만 구해서 그것들의 평균을 구해라
PlantGrowth %>% 
  group_by(group) %>% 
  filter(weight >= mean(weight)) %>% 
  summarise(mean = mean(weight))

## one sample t-test
set.seed(1234)  
df <- rnorm(n = 10, mean = 95, sd = 4)

t.test(df, alternative = "two.sided",
       mu = 100, conf.level = 0.95)
# df=9, cuz n=10

# 
t.test(df, alternative = "greater",
       mu = 100, conf.level = 0.95)
# p-value increses dramatically.
# because 위쪽을 계산함. 귀무가설을 100% 지지한다.


## two-sample test
g1 <- c(1.7, 3.9, 1.6, 4.4, 2, 2.2, 2.6, 2, 0.9, 0.7)
g2 <- c(1.2, 3.4, 2, 3.6, 2.5, 1.4, 4.7, 2.9)
boxplot(g1, g2)
# 대립가설: H1: u1 != u2
# 귀무가설: H0: u1 = u2
# 가설이 채택/기각 인지 p-value로 정하자.

t.test(x = g1, y = g2, alternative = "two.sided", 
       mu = 0, 
       paired = F, var.equal = T,
       conf.level = 0.95) 
# two.sided = p-value*2 계산이 되어서 나옴.
# mu=0: two-sample에서 대부분 검정.
# paired: 독립사건 유무.
# var.equal: pooled variance를 쓸 것인가.


## mission about two-sample test
summary(ToothGrowth)
# 두 그룹간의 length의 차이가 있는지를 검증해라.
# 대립가설: 두 그룹간 length 평균이 같지 않다.
# 귀무가설: 두 그룹간 length 평균이 같다.


t.test(len ~supp, data = ToothGrowth, alternative = "two.sided",
       mu = 0, 
       paired = FALSE,
       var.equal = T,
       conf.level = 0.95)
# p-value= 0.06063

t.test(len ~supp, data = ToothGrowth, alternative = "two.sided",
       mu = 0, 
       paired = FALSE,
       var.equal = F,
       conf.level = 0.95)
# var.equal의 값에 상관없이 같은 값이 나온다
# = 두 자료의 분산이 같다.
# 두 자료의 분산값이 같던 다르던, var.equal의 값이 보정해서 나오므로
# 우리는 항상 var.equal = FALSE로 놓고 계산한다.


## chickwts
boxplot(weight ~feed, data = chickwts)

chickwts %>% 
  filter(feed == "casein" | feed =="horsebean") %>% 
  t.test(weight ~feed, data = .,
         var.equal= T)
# p-value가 너무 작아서, 통계의 가치가 없다.

chickwts %>% 
  filter(feed == "soybean" | feed =="meatmeal") %>% 
  t.test(weight ~feed, data = .,
         var.equal= F)

## Mission
# mtcars에서 
summary(as.factor(mtcars$cyl))

mtcars %>% 
  filter(cyl == 4 | cyl == 6) %>% 
  t.test(mpg ~cyl, data =., 
         var.equal = F)

## ToothGrowth
names(ToothGrowth)

oj <- ToothGrowth %>% 
  filter(supp == "OJ")
vc <- ToothGrowth %>% 
  filter(supp == "VC")

shapiro.test(oj$len) # p-value = 0.023: 정규분포를 안따름.
shapiro.test(vc$len) # p-value = 0.4284: 정규분포를 따름.

ToothGrowth %>% 
  t.test(len ~supp, data = .)


# 등분산 검정
library(car)
ToothGrowth %>% 
  leveneTest(len ~supp, data = .,
             center = "mean")
# p-value = 0.2992 > 0.05, 두 데이터의 분산이 같다: 귀무가설 채택.


## sleep: paired design
?sleep
names(sleep)
view(sleep)

sleep %>% 
  t.test(extra ~group, data = .,
         paired = F)

sleep %>% 
  t.test(extra ~group, data = .,
         paired = T)







