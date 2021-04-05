##
## 2021. 04. 05
## ANOVA(analysis of variance)
##

library(tidyverse)
library(car)

## Welch's test
?InsectSprays
head(InsectSprays)

# 두개 sample 이 있으니까 two-sample t-test를 진행해야겠다.
#t.test(x, y = NULL,
#       alternative = c("two.sided", "less", "greater"),
#       mu = 0, paired = FALSE, var.equal = FALSE,
#       conf.level = 0.95, ...)

# 이 실험은 등분산가정, 정규화가정을 안했음.
# p-value 외에 다른 값들이 어떻게 나왔는지에 대한 고민 필요.
InsectSprays %>% 
  filter(spray == "A" | spray == "B") %>% 
  t.test(count ~spray, data = .,
         alternative = "two.sided", mu = 0,
         paired = F, var.equal = F, 
         conf.level = 0.95)

## Normality test (Shapiro-Wilk normality test)
Insect_A <- InsectSprays %>% 
  filter(spray == "A")
Insect_B <- InsectSprays %>% 
  filter(spray == "B")
shapiro.test(Insect_A$count)
shapiro.test(Insect_B$count)

# another way(better way, code skill)

## leven test
InsectSprays %>% 
  filter(spray == "A" | spray == "B") %>%
  leveneTest(count ~spray, data = .)



## PlantGrowth
# Question: Group ctrl과 trt1간의 차이가 있는가를 검정해보라.

# Normality test(Shapiro-Wilk normality test)
plant_ctrl <- PlantGrowth %>% 
  filter(group == "ctrl")
plant_trt1 <- PlantGrowth %>% 
  filter(group == "trt1")
shapiro.test(plant_ctrl$weight)
shapiro.test(plant_trt1$weight)

# Equality of variance test
PlantGrowth %>% 
  filter(group == "ctrl" | group == "trt1") %>% 
  leveneTest(weight ~group, data = .)

# two-sample t-test
PlantGrowth %>% 
  filter(group == "ctrl" | group == "trt1") %>% 
  t.test(weight ~group, data = .,
         alternative = "two.sided",
         paired = F, var.equal = T,
         conf.level = 0.95)

# Answer: 
# p-value = 0.249 -> 두 그룹 각각으로 자라난 plant들의 weight 차이가 없다.


##
# mpg가 평균이상, transmission이 automatic인 것의 평균
mtcars %>% 
  filter(mpg >= mean(mpg), am == 0) %>% 
  summarise(mean = mean(mpg))

# auto 와 manual을 같이 보고싶다면,
mtcars %>% 
  filter(mpg >= mean(mpg)) %>%
  group_by(am) %>% 
  summarise(mean = mean(mpg))

head(iris)
# 주로 엑셀, wide type: 여러 column을 사용 -> sepal.length, sepal.width, ...
# 기계는 long format을 선호 -> 한개의 col

iris %>% 
  group_by(Species) %>% 
  summarise(mean = mean(Sepal.Length))
  
## pivot
# data를 long type으로 바꿔줌.
iris_long <- iris %>% 
  pivot_longer(cols = 1:4, 
               names_to = "leaf", 
               values_to = "length")

iris_long %>% 
  group_by(Species, leaf) %>% 
  summarise(median = median(length))


# column간의 연산이 필요할 경우에는 wide format이 유용.
iris_wide <- iris_long %>%
  pivot_wider(names_from = "leaf",
              values_from = "length")
# 위의 코드를 사용하면 제대로 변환이 일어나지 않는다.
# 위의 시행착오 해결하고자, id를 사용
iris_long <- iris %>% 
  mutate(id = row_number()) %>%  # idx를 이용해 자료의 순서를 명확히 함.
  pivot_longer(cols = 1:4, 
               names_to = "leaf", 
               values_to = "length")

iris_wide <- iris_long %>% 
  pivot_wider(names_from = "leaf",
              values_from = "length")


## nycflights13 data
# install.packages("nycflights13")
library("nycflights13")

head(flights)
names(flights) # 변수명을 다 알고싶을때때
dim(flights) # dataset의 갯수 알고 싶을 때

barplot(table(flights$origin))


flights %>% 
  filter(origin == "JFK", month == 2) # data type으로 filter 걸기

flights %>% 
  filter(origin == "JFK" | origin == "LGA") # data type으로 filter 걸기

flights %>% 
  filter(origin != "EWR")

flights %>% 
  filter(origin %in% c("JFK","LGA")) #문자열에서 사용할 경우.

flights %>% 
  arrange(dep_delay) # 오름차순
flights %>% 
  arrange(desc(dep_delay)) # 내림차순

flights %>% 
  select(year, month, day, origin, dep_delay)

flights %>% 
  select(1:5)

flights %>% 
  select(year:day)

flights %>% 
  select(origin, everything())

flights %>% 
  select(contains("time"))

flights %>% 
  select(ends_with("time")) # vs. starts_with()

flights %>% 
  rename(anual1 = year) # 변수 명을 바꾼다.

flights %>% 
  transmute(annual = year)

flights %>% 
  mutate(time_diff = dep_delay - arr_delay) # 뒤에 붙인다.

flights %>% 
  transmute(time_diff = dep_delay - arr_delay) # 하나만 남겨두고 다 지운다

flights %>% 
  mutate(evaluation = if_else(dep_delay <= 0, "good", "bad")) %>% 
  select(evaluation, everything)

## system information
str(Sys.Date()) # structure

library(lubridate)
week(Sys.Date())

mydate <- c("2021-04-05")
str(mydate)

mydate2 <- as.Date("2021-04-05", format = "%Y-%m-%d")
str(mydate2)

mydate3 <- as.Date("2021/04/05", format = "%Y/%m/%d")
str(mydate3)



## ANOVA table
# 보통 3-5 개의 데이터일경우 함.

anova.example <- data.frame(A = seq(2,6, length.out = 6),
                            B = seq(4,8, length.out = 6),
                            C = seq(6,10, length.out = 6))

anova.example.long <- anova.example %>% 
  pivot_longer(cols = 1:3,
               names_to = "비료",
               values_to = "무게")

aov.result <- anova.example.long %>% 
  aov(무게 ~비료, data = .)

summary(aov.result)

#result
# Df Sum Sq Mean Sq F value Pr(>F)
# 비료         2     24      12       3  0.125
# Residuals    6     24       4     
# 그룹간분산/그룹내분산 = 3 -> when length.out = 3
# Df Sum Sq Mean Sq F value  Pr(>F)   
# 비료         2   48.0   24.00   10.71 0.00129 **
#   Residuals   15   33.6    2.24                   
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 그룹간분산/그룹내분산 = 10.71 -> when length.out = 6
# 즉, 어느정도의 데이터수가 있어야 기각이 가능하다.

## ANOVA example with InsectSprays data
# 1. normality test
norm_test <- InsectSprays %>% 
  group_by(spray) %>% 
  nest() %>% 
  mutate(shapiro = map(data, ~shapiro.test(.x$count)$p.value)) %>% 
  unnest(shapiro)
# 정규성을 따르지 않는 부분이 문제를 일으킬수 있겠다라는 것 인식.

# 2. Equality of variance test
InsectSprays %>% 
  leveneTest(count ~spray, data = .)
# 등분산을 만족하지 않으므로, ANOVA를 사용할 수 없음.
# 이럴 경우, 비모수를 사용해야한다.

# 만약 위 과정을 다 통과했다면,
# 3. ANOVA
InsectSprays %>% 
  aov(count ~spray, data = .) %>% 
  summary()

InsectSprays %>% 
  boxplot(count ~spray, data = .)


## ANOVA example with chickwts data
#1. normality test
names(chickwts)
norm_test <- chickwts %>% 
  group_by(feed) %>% 
  nest() %>% 
  mutate(shapiro = map(data, ~shapiro.test(.x$weight)$p.value)) %>% 
  unnest(shapiro)
# 모두 정규분포를 따름.

#2. Equality of variance test
chickwts %>% 
  leveneTest(weight ~feed, data = .)
# pvalue = 0.58, 같은 분산을 가짐.

#3. ANOVA
chickwts %>% 
  aov(weight ~feed, data = .) %>% 
  summary()
# 그룹간의 평균에 차이가 있다.
# df= 자료수가 6개, Df = N-a, 
# (SSB/df) / (SSW/N-a)
# F value=15.37: 그룹간 분산/그룹내 분산이 15배
# 일반적으로는 그룹간 분산이 적다
# 그가 아니라면, treatment effect에 의해 값이 커진다.

chickwts %>% 
  boxplot(weight ~feed, data = .)
  

# 사후검정 Post_hoc
# 어떤 treatment에서 어떻게 차이가 나느냐
# 0. 가정검정(normality, equality of variance)
# 1. ANOVA P<0.05
# 2. post_hoc

aov.result <- chickwts %>% 
  aov(weight ~feed, data = .)

## post_hoc
TukeyHSD(aov.result)
# 각 테스트는 t.test, p adj(adjust) <- 일반 p value와 다름

## FWER
fwer <- 1-0.95^12
1-(1-0.05/12)^12

## TukeyHSD는 각 수준당 표본의 개수가 동일해야함.
table(chickwts$feed)

## ToothGrowth
# example of interaction effect
ToothGrowth %>% 
  boxplot(len ~supp*dose, data = .)
# 이원배치 분산 분석: OJ vs. VC / 0.5 vs. 1 vs. 2
# 2mg에서는 두 그룹간의 차이가 잘 안보인다.
# 이를 interaction이 있다고 함
# 이는 ANOVA에서 분석이 불가능하다고 함

with(ToothGrowth, interaction.plot(dose, supp, len, fun = mean,
                                   main = "Interaction Plot"))
# 그룹간의 효과가 있다고 말하기 모호
# interaction effect가 있으면 = 유의하면, 주효과 분석이 의미가 없다. / 하지 않는다.
# 예측 불가능한 패턴
# 일반적으로 있지만 interaction이 있다.
# 이원배치부터는 이런 현상이 생긴다.
