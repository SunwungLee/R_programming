##
## 2021. 04. 06
## Linear regression 
##

library(tidyverse)
library(car)

## 이원배치 분산분석
head(ToothGrowth)

ToothGrowth %>% 
  leveneTest(len ~supp*dose, data = .)
# Levene's test is not appropriate with quantitative explanatory variables.
# 연속형 변수라 format이 안맞는다.
table(ToothGrowth$dose)
str(ToothGrowth$dose)

# numeric 변수를 factor로 만들어 주어야함.
# 그래야 leveneTest의 input으로 사용가능함.
ToothGrowth <- ToothGrowth %>% 
  mutate(dose = factor(dose))

ToothGrowth %>% 
  leveneTest(len ~supp*dose, data = .)

## 
aov.result1 <- ToothGrowth %>% 
  aov(len ~supp+dose, data = .)

# interaction term: 요인효과만으로는 해석 불가능한 부분.
aov.result2 <- ToothGrowth %>% 
  aov(len ~ supp*dose, data = .)

with(ToothGrowth, interaction.plot(dose, supp, len, fun = mean,
                                   main = "Interaction Plot"))

# post hoc
TukeyHSD(aov.result2)


## Simple regression
names(cars)
cars %>% 
  plot(dist ~speed, data = .)

cars %>% 
  lm(dist ~speed, data = .) %>% 
  summary()
# intercept는 유의해도 관심이 없음 = 일반적인 상황에서는 사용하지 않는다.
# speed의 p-value ***: 상당히 유의함.
# 즉, speed는 제동거리에 상당한 영향을 준다.
# estimate: 1단위만큼 증가하면, 제동거리가 3.93만큼 증가한다.
# input들의 단위들을 잘 넣어줘야 해석이 편함.

## Multiple regression
mtcars %>% 
  lm(mpg ~wt, data = .) %>% # lm: linear model
  summary()
# F value가 높다면, 너무나도 연관성이 있음.
# beta1 이 0이 아니다. 유의하다.
mtcars %>% 
  plot(mpg ~wt, data = .)

mtcars %>% 
  lm(mpg ~ wt + factor(cyl) + am, data = .) %>% 
  summary()
# p-value를 보고 유의한지 판단
# F value를 보고 얼마나 유의한지 확인

# data type을 꼭 확인하고 진행해야함.
str(mtcars)
mtcars %>% 
  lm(mpg ~ wt + factor(cyl) + factor(am), data = .) %>% 
  summary()
# 변수가 연속 or 이산으로 나눠서 생각 cyl를 factor(이산형)로 변환
# 4개짜리를 기준으로 6, 8개 짜리가 따로 결과값으로 나옴.
# R에서는 항상 가장 낮은수치가 기준수치로 지정된다.
# 결론: weight와 cylinder의 개수는 차량 연비(mpg)에 영향을 준다.
# (estimate와 같은 비율로)

# interaction
mtcars %>% 
  lm(mpg ~ wt*factor(cyl), data = .) %>% 
  summary()
# 8기통으로 올라가면, 이 기울기가 원래 측정한 것에 비해 변한다
# 즉 interaction effect가 있다.
# interaction도 Regression에 존재한다.

# post hoc
lm.result <- mtcars %>% 
  lm(mpg~wt, data = .)

plot(lm.result)
# Residual = 회귀선과 데이터간 사이의거리, 이걸 plot함
# 첫 그래프가 0 선으로 됨
# 일정 term 안에 데이터들이 존재해야만 함. 비교적 symmetric함.

# Q-Q plot: 자료의 정규성
# -1~1 사이에서 직선이 나와야 함 ==> 오차가 정규성을 따른다.
# 즉, 자료가 정규분포를 따른다.

# scale-location plot: 절대값으로 확인함.
# model 검증

# residuals vs. leverage(지렛대)
# 데이터에 따라서 회귀직선이 변하게 되는 것을 찾아라
# outlier를 찾아라.
# 그리고 그 점을 leverage라고한다.
# 주로 양 끝단에 존재한다.
# 해석: 이 데이터가 critical 하므로, 이 데이터를 빼고 한번 예측해봐라.