##
## 2021. 04. 23
## flexdashboard
##
library(tidyverse)
library(car)
library(RColorBrewer)
library(plotly)
library(leaflet)
library(leaflet.extras)



a <- leaflet()

leaflet() %>% 
  addTiles() %>% 
  addMarkers(lat = 37.55, 
             lng = 127.07,
             label = "영우글로벌"
             )

# run uniform: 해당 범위 내에서 동일한 확률로 뽑아라.
df <- data.frame(lat = runif(6, min = 37.52, max = 37.56),
                 lng = runif(6, min = 127.06, max = 127.08),
                 name = LETTERS[1:6]
                 )  

# df$name 대신에 ~name사용가능
df %>% 
  leaflet() %>% 
  addTiles() %>% 
  #addMarkers(label = ~name) %>% 
  addCircles(lat = ~lat, lng = ~lng,
             radius = 40,
             color = "red"
             )

#
# quake data 시각화
#
head(quakes)
mean_lng <- mean(quakes$long)
mean_lat <- mean(quakes$lat)

quakes %>% 
  leaflet() %>% 
  addProviderTiles(providers$OpenStreetMap) %>% 
  setView(lng = mean_lng,
          lat = mean_lat,
          zoom = 5) %>% 
  addHeatmap(lng = ~long, lat = ~lat,
             intensity = ~mag,  # magnitute에 대해서 그림
             radius = 10, # trial and error 방식으로 radius 정리
             max = 10, # maximum intensity
             blur = 20
             )

#
# starbucks 판매량 heatmap
#
# 1. download 스타벅스 매장 csv 파일
# 2. import data option으로 dataframe 저장.
# starbucks / 1424obs. of 4 variables

# 랜덤으로 매장별 수입을 만들어주자.
starbucks$income <- rnorm(dim(starbucks)[1], 100, 100)^2
head(starbucks)

mean_lng = mean(starbucks$경도)
mean_lat = mean(starbucks$위도)

starbucks %>% 
  leaflet() %>% 
  addProviderTiles(providers$OpenStreetMap) %>% 
  setView(lng = mean_lng, lat = mean_lat,
          zoom = 5) %>% 
  addHeatmap(lng = ~경도, lat = ~위도,
             intensity = ~income,
             radius = 10,
             max = 30,
             blur = 20
             )









