##
## 2021. 04. 09
## leaflet 
##
library(tidyverse)
library(car)
library(RColorBrewer)
library(plotly)

##
## leaflet (지도 위에 그림 그리기)
##
#install.packages("leaflet")
library(leaflet) 

#
# single icon
#
yglIcon <- makeIcon(
  iconUrl = "http://www.ycampus.co.kr/images/common/head_logo.png",
  iconWidth = 80, iconHeight = 20,
  iconAnchorX = 40, iconAnchorY = 10
)
yglLink <- c(
  "<a href = 'http://www.ycampus.co.kr/'> 영우글로벌 </a>"
)
my_leaflet <- leaflet() %>% 
  addTiles() %>% 
  addMarkers(lat = 37.513979777830244, 
             lng = 127.02953929767202, 
             popup = yglLink,
             icon = yglIcon,
  )
my_leaflet

#
# Multiple icons
#
fruitsIcon <- makeIcon(
  iconUrl = c(
    "https://t1.daumcdn.net/cfile/blog/990B12335A896ABA32",
    "https://littledeep.com/wp-content/uploads/2020/01/littledeep_illustration_banana_style1.png"
  ),
  iconWidth = 30, iconHeight = 20,
  iconAnchorX = 15, iconAnchorY = 10
)
fruitsLink <- c(
  "<a href = 'http://www.ycampus.co.kr/'> 영우글로벌 </a>",
  "<a href = 'http://www.naver.com/'> 네이버 </a>",
  "<a href = 'http://www.google.com/'> 구글 </a>"
)
test_df <- data.frame(lat = runif(20, min = 37.504, max = 37.524),
                      lng = runif(20, min = 127.019, max = 127.039),
                      name = fruit[1:20]
)
test_leaflet <- leaflet() %>% 
  addTiles() %>% 
  addMarkers(lat = test_df$lat, 
             lng = test_df$lng,
             popup = fruitsLink,
             icon = fruitsIcon
  )
test_leaflet


#
# cluster option
#
test_df_2 <- data.frame(lat = runif(200, min = 37.414, max = 37.614),
                        lng = runif(200, min = 126.909, max = 127.129),
                        name = fruit[1:20]
)
test_leaflet_2 <- leaflet() %>% 
  addTiles() %>% 
  addMarkers(lat = test_df_2$lat, 
             lng = test_df_2$lng,
             popup = test_df_2$name,
             clusterOptions = markerClusterOptions() ##
  )
test_leaflet_2

#
# additional information
#
# lat = 37.513979777830244, lng = 127.02953929767202
addInfo_df <- data.frame(lat = runif(200, min = 37.414, max = 37.614),
                         lng = runif(200, min = 126.909, max = 127.129),
                         modality = sample(c("red", "blue", "green", "violet"), 
                                      200, replace = T),
                         distance = rnorm(200, mean = 10, sd = 5)
                         )

addInfo_leaflet <- addInfo_df %>% 
  leaflet() %>% 
  addTiles() %>% 
  addMarkers(lat = addInfo_df$lat, 
             lng = addInfo_df$lng,
             clusterOptions = markerClusterOptions()
             ) %>% 
  addCircles(weight = 1, radius = addInfo_df$distance*50,
             color = addInfo_df$modality, fillOpacity = 0.3
             ) %>% 
  addLegend(labels = c("car", "autocycle", "bike", "walk"),
            colors = c("red", "blue", "green", "violet"),
            opacity = 0.6
            )

addInfo_leaflet

#
# new tile
#
?addProviderTiles
# 아래 링크에서 원하는 map copy해서 tile 바꾸기기
# http://leaflet-extras.github.io/leaflet-providers/preview/

changeTile_df <- data.frame(lat = runif(200, min = 37.414, max = 37.614),
                         lng = runif(200, min = 126.909, max = 127.129),
                         modality = sample(c("red", "blue", "green", "violet"), 
                                           200, replace = T),
                         distance = rnorm(200, mean = 10, sd = 5)
)

changeTile_leaflet <- changeTile_df %>% 
  leaflet() %>% 
  addProviderTiles("OpenTopoMap") %>%  ##############
  addMarkers(lat = changeTile_df$lat, 
             lng = changeTile_df$lng,
             clusterOptions = markerClusterOptions()
  )

changeTile_leaflet
