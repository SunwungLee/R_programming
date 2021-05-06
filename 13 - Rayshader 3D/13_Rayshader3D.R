##
## 2021. 04. 23
## 3D rayshader
##
library(tidyverse)
library(car)
library(RColorBrewer)
library(plotly)
library(leaflet)
library(leaflet.extras)

head(faithful)

# 간헐천 기다림시간 + 폭발 시간
# 기다림이 길수록 폭발시간이 길다.

## baseplot
faithful %>% 
  plot(waiting ~eruptions, data = .)

## ggplot, scatter
p <- faithful %>% 
  ggplot(aes(x = eruptions, y = waiting)) +
  geom_point(size = 3, alpha = 0.6) +
  xlim(0, 6) + ylim(20, 110)

p + geom_density_2d(size = 0.25,
                    color = 'red')

p + geom_density_2d_filled()

##
##
ggfaithful <- faithful %>% 
  ggplot(aes(x = eruptions, y = waiting)) +
  stat_density_2d(aes(fill = stat(nlevel)),
                  geom = "polygon",
                  n = 100,
                  bins = 100) +
  xlim(0, 6) + ylim(20, 110)

library(rayshader)
plot_gg(ggfaithful)


#
# diamonds
# x = x, y = depth
#
head(diamonds)
ggdiamonds <-diamonds %>% 
  ggplot(aes(x = x, y = depth)) + 
  stat_density_2d(aes(fill = stat(nlevel)),
                  geom = "polygon",
                  n = 100,
                  bins = 100) +
  scale_fill_viridis_c(option = "A")

plot_gg(ggdiamonds,
        width = 5, # figure size
        height = 5, #
        multicore = T, # 컴퓨터 프로세싱 코어
        scale = 250,
        theta = 10, # angle, 3d polar coordinate 검색(Paul's onli)
        phi = 30, # 3d에서의 angle
        windowsize = c(600, 600)
        )

# 만약에 결과값이 안뜰 경우.
# 창을 불러들여야 하기 때문에 rgl을 선언
library(rgl)
library(av)

# 항상 이미지 만들떄
# getwd, setwd() 사용하자
# rgl 하나는 열어놓은 상태에서 실행시켜야 함.
# Error in png::readPNG(png_files[1]) : file is not in PNG format
# 추가정보: 50건 이상의 경고들을 발견되었습니다 (이들 중 처음 50건을 확인하기 위해서는 warnings()를 이용하시길 바랍니다).
# --> 이 에러는 실행 후, 저장할 때 발생.
# --> filename 안 정해줘서 발생했었음.

render_movie(filename = 'ggdiamonds-orbit', 
             type = "orbit",
             theta = 60,
             phi = 45)

render_movie(filename = 'ggdiamonds-oscillate', 
             type = "oscillate",
             theta = 60,
             phi = 45)

#
# diamonds
# x = x, y = depth
#
head(diamonds)
ggdiamonds_farcet_clarity <-diamonds %>% 
  ggplot(aes(x = x, y = depth)) + 
  stat_density_2d(aes(fill = stat(nlevel)),
                  geom = "polygon",
                  n = 100,
                  bins = 100) +
  scale_fill_viridis_c(option = "A") +
  facet_wrap(clarity ~.)

ggdiamonds_farcet_clarity # clarity에 따라서 plot을 새로 그림

plot_gg(ggdiamonds_farcet_clarity,
        width = 5, # figure size
        height = 5, #
        multicore = T, # 컴퓨터 프로세싱 코어
        scale = 250,
        theta = 10, # angle, 3d polar coordinate 검색(Paul's onli)
        phi = 30, # 3d에서의 angle
        windowsize = c(600, 600)
)

render_movie(filename = 'ggdiamonds_farcet_clarity', 
             type = "orbit",
             theta = 60,
             phi = 45)
