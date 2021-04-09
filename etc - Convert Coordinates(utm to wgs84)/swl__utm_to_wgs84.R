# http://rstudio-pubs-static.s3.amazonaws.com/27657_2df4320564894a0195e6a0a0a214d3b5.html
library(sp)
library(rgdal)

# 본인 데이터에 맞게 
# 1. "seoul_hair_salon" 부분 수정
# 2. 좌표 정보 수정
##
## 
##
common_var <- seoul_hair_salon
common_var <- common_var %>% 
  drop_na(c("좌표정보.X.", "좌표정보.Y."))
common_var <- common_var %>% mutate(lngi = `좌표정보.X.`,
                                    lati = `좌표정보.Y.`)
coord <- data.frame(utmk.long=common_var$lngi, 
                    utmk.lat=common_var$lati
)
##
##
##

convertCoordSystem <- function(long, lat, from.crs, to.crs){
  xy <- data.frame(long=long, lat=lat)
  coordinates(xy) <- ~long+lat
  
  from.crs <- CRS(from.crs)
  from.coordinates <- SpatialPoints(xy, proj4string=from.crs)
  
  to.crs <- CRS(to.crs)
  changed <- as.data.frame(SpatialPoints(spTransform(from.coordinates, to.crs)))
  names(changed) <- c("lng", "lat")
  
  return(changed)
}

# *중부원점(Bessel): 서울 등 중부지역
from.crs = "+proj=tmerc +lat_0=38 +lon_0=127 +k=1 +x_0=200000 +y_0=500000 +ellps=bessel +units=m +no_defs +towgs84=-115.80,474.99,674.11,1.16,-2.31,-1.63,6.4"
to.crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
coord <- cbind(coord, 
               convertCoordSystem(coord$utmk.long, coord$utmk.lat, from.crs, to.crs)
               )
common_var <- bind_cols(common_var, coord)
utm_to_wgs_converted_coord_df <- common_var

# #
# # Test
# #
# my_map <- utm_to_wgs_converted_coord_df %>%
#   leaflet() %>%
#   addTiles() %>%
#   addMarkers(lat = utm_to_wgs_converted_coord_df$lat, 
#              lng = utm_to_wgs_converted_coord_df$lng,
#              # popup = utm_to_wgs_converted_coord_df$사업장명,
#              clusterOptions = markerClusterOptions())
# my_map

