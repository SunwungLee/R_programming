##
## 2021. 04. 09
## example using Seoul open data on https://data.seoul.go.kr/
##
library(tidyverse)
library(car)
library(RColorBrewer)
library(plotly)
library(leaflet) 
# install.packages("RCurl")
library(RCurl)

#
# read .csv file
# seoul library location
#
root_path <- getwd()
path <- paste0(root_path, "/dataset/seoul_library_locations.csv")
seoul_library <- read.csv(path, header = TRUE)

seoul_library <- seoul_library %>% 
  drop_na()

my_df <- data.frame(lat = seoul_library$위도,  
                    lng = seoul_library$경도,
                    name = seoul_library$도서관명, 
                    na.rm = T
                    )

name <- seoul_library$도서관명
link <- seoul_library$홈페이지.URL  

frontUrl = "<a href = '"
middleUrl = "'> "
endUrl = " </a>"
urlForm <- paste0(frontUrl, link, middleUrl, name, endUrl)

head(urlForm)

my_leaflet <- leaflet() %>% 
  addTiles() %>% 
  addMarkers(lat = my_df$lat, 
             lng = my_df$lng,
             popup = urlForm,
             clusterOptions = markerClusterOptions()
             )

my_leaflet
  

#
# seoul emergancy
#
root_path <- getwd()
path <- paste0(root_path, "/dataset/seoul_emergancy_locations.csv")
seoul_emergancy <- read.csv(path, header = TRUE)

seoul_emergancy <- seoul_emergancy %>% 
  drop_na() %>% 
  unite(col = "name", c("기관명", "대표전화1"), sep = "\n")
emergancy_df <- data.frame(lat = seoul_emergancy$병원위도,
                           lng = seoul_emergancy$병원경도,
                           name = seoul_emergancy$name,
                           na.rm = T
                           )
# name <- seoul_emergancy$기관명
emergancy_leaflet <- leaflet() %>% 
  addTiles() %>% 
  addMarkers(lat = emergancy_df$lat,
             lng = emergancy_df$lng,
             popup = emergancy_df$name,
             clusterOptions = markerClusterOptions()
             )

emergancy_leaflet 


#
# hair salon, utm coordinate
#
root_path <- getwd()
path <- paste0(root_path, "/dataset/seoul_hair_salon.csv")
seoul_hair_salon <- read.csv(path, header = TRUE)

# convert coordinate using "swl__utm_to_wgs84.R"

#
# Test
#
my_map <- utm_to_wgs_converted_coord_df %>%
  leaflet() %>%
  addTiles() %>%
  addMarkers(lat = utm_to_wgs_converted_coord_df$lat, 
             lng = utm_to_wgs_converted_coord_df$lng,
             popup = utm_to_wgs_converted_coord_df$사업장명,
             clusterOptions = markerClusterOptions())
my_map
