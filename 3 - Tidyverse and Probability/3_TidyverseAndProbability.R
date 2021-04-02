##
## 2021. 03. 31
## basic functions of tidyverse & normal distribution
##

#$?quantile , mainly use to sequence data

## quantile
quantile(mtcars$mpg)

## data cutting
cut(x = mtcars$mpg, 
    breaks = quantile(mtcars$mpg),
    labels = c("Q1", "Q2", "Q3", "Q4"))

## if-else
score <- 80
if (score >= 80){
  print("Excellent")
} else {
  print("Study harder")
}

## for-loop
for (i in 1:10){
  print(i)
}

## switch-case
type <- "lemon"
switch (type,
  "apple" = paste("delicious"),
  "lemon" = paste("sour")
)

## aggregate
#boxplot(weight ~feed, data = chickwts)
mean(chickwts$weight)

aggregate(weight ~feed, data = chickwts,
          mean)

aggregate(weight ~feed, data = chickwts,
          sd)

## apply
mean(cars$speed)

apply(cars, 2, mean) # 2: column 
apply(cars, 2, max)

## draw graph
x <- seq(1, 20, by=0.1)
y <- cos(x)
plot(x, y, type="l")

## pipe operator: strong function for optimising code (tidyverse)
# %>% : ctrl+shift+m
x <- seq(1, 20, by=0.1)
x %>% 
  cos() %>% 
  plot(type = "l", col = "blue")

y <- x %>% 
  cos()

x <- table(mtcars$cyl)
barplot(x)

table(mtcars$cyl) %>% 
  barplot()

x %>% 
  log() %>% 
  sqrt() %>% 
  hist()

## mutate
kph_spd <- cars$speed * 1.61
m_dist <- cars$dist * 0.304

# original way: change raw data
cars$kph_spd <- cars$speed * 1.61
cars$m_dist <- cars$dist * 0.304
cars$ratio <- cars$m_dist / cars$kph_spd

# using pipe operator
cars %>% 
  mutate(ratio = dist*0.304 / speed*1.61)

# Do not change the raw data.
chickwts %>% 
  mutate(log_weight = log(weight),
         check = if_else(weight > median(weight), "Upper", "Lower")) %>% 
  head()

# Can save my own variable with modified data.
mych <- chickwts %>% 
  mutate(log_weight = log(weight),
         check = if_else(weight > median(weight), "Upper", "Lower"))

chickwts %>% 
  mutate(log_weight = log(weight)) %>% 
  boxplot(log_weight ~feed, data = .)

# extract variables
mtcars


## 
set.seed(7201)
df <- round(rnorm(n = 10, mean = 95, sd = 50), 0) # Montecarlo simulation (simulation from normal distribution)
df_mean <- mean(df) # if this mean is over the original mean, then have to calculate other side -> 1-()
df_se <- 50 / sqrt(10) # sd / sqrt(n)

# 
pnorm(q = df_mean, mean = 100, sd = df_se)*2 # 0.0005 이므로 귀무가설이 기각 -> 대립가설이 채택
# 만약 위 값이 1을 초과할 경우 (1-())*2 로 계산해준다.

## example
# 정규모집단으로부터 얻어진 음료수 생플이 있다.
# 음료수의 양(ml) = c(99,97,102,88,103,91)
# 대립가설 H1: u != 100ml이 옳은지 유의수준 0.05에서 검정하여라
set.seed(7201)
x = c(99, 97, 102, 88, 103, 91)

df_mean = mean(x)
df_se = sd(x)/sqrt(6)

a <- pnorm(q=df_mean, mean = 100, sd = df_se)*2
# 귀무가설이 채택 -> 대립가설 기각
# 왜 물 그래프 그릴때, sd 를 df_se로 그렸나?
# 
dd <- dnorm(x=90:110, mean=100, sd=df_se)
plot(x=90:110, dd, type="l", col="red")


## 정규분포의 표준화 ************************** recap 
set.seed(7201)
df <- round(rnorm(n=10, mean=95, sd=16))
df

# mean and var
df_mean <- mean(df)
df_se <-  sd(df) / sqrt(10)
z_value <- (df_mean - 100)/df_se

pnorm(z_value, mean=0, sd=1)*2







