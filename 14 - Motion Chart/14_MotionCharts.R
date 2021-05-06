##
## 2021. 04. 23
## Motion chart
##
library(tidyverse)
library(car)
library(RColorBrewer)
library(plotly)
library(leaflet)
library(leaflet.extras)

library(rayshader)
library(rgl)
library(av)


library(gganimate)

iris %>% 
  plot(Petal.Length ~Petal.Width, data=.)

p1 <- iris %>% 
  ggplot(aes(x=Petal.Width, y = Petal.Length, color = Species))+
  geom_point(size = 3, alpha = 0.6)

p1 + 
  transition_states(states = Species, 
                    transition_length = 0.1,
                    state_length = 0.5 # 자리 머물렀을 때 시간
                    ) +
  #ease_aes("linear") +
  #ease_aes("cubic-in-out") +
  ease_aes("bounce-in") +
  labs(title = paste("Iris type: {closest_state}")) + # 정보가 위에 바뀌면서 나타남.
  enter_fade() +
  exit_fade()



#
# mtcars
# mpg, weight, cyl
# x = wt, y = mpg, factor = cyl
#
mt1 <- mtcars %>% 
  ggplot(aes(x = wt, y = mpg, color = as.factor(cyl)))+
  geom_point(size = 3, alpha = 0.6)
mt <- mt1 +
  transition_states(states = as.factor(cyl),
                    transition_length = 0.1,
                    state_length = 0.5
                    ) +
  ease_aes("bounce-in") +
  labs(title = paste("mtcars cylinder type: {closest_state}")) +
  enter_fade() +
  exit_fade()


# animate(mt, renderer=av_renderer())


#
# population 자료
# readr 로 import (base는 encoding error)
# 밑에같이 code로도 가능, but import가 더 편함
# population <- read.csv('경로', header = T, fileEncoding = 'euc-kr', encoding = 'utf-8')
#
str(population) # year별로 plotting 할 것이므로 integer형으로 바꿔준다

# wide type -> long type으로 변환
dt_population <- population %>% 
  pivot_longer(contains("male"), names_to = "gender",
               values_to = "population")

dt_population %>% 
  ggplot(aes(x = age2, 
             y = if_else(gender == "male", population, # 남자면, 위로 쌓고 / 여자면, 아래로 쌓아라.
                         -population), fill = gender)) + 
  geom_bar(stat = "identity")

pp <- dt_population %>% 
  ggplot(aes(x = age2, 
             y = if_else(gender == "male", population, # 남자면, 위로 쌓고 / 여자면, 아래로 쌓아라.
                         -population), fill = gender)) + 
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = paste("한국 연령: {frame_time}"),
       x = "연령", y = "인구수")

pp +
  transition_time(year) +
  ease_aes("linear") +
  scale_y_continuous(breaks = seq(-5*10^5, 5*10^5, 2.5*10^5),# y축 음수 없애기
                     labels = paste(abs(seq(-5*10^5, 5*10^5, 2.5*10^5)))
                     ) 
  



