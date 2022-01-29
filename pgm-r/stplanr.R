library(dplyr, warn.conflicts = F)
library(osmdata)
library(sf)

bb <- c(2.2832, 48.8906, 2.4081, 48.9382)
x <- opq(bbox = bb) %>% 
  add_osm_feature(key = "amenity", value = "hospital") %>%
  osmdata_sf()

x1 <- st_transform(x$osm_points, 2154)
plot(x1)

# HN2
library(tidyverse)
library(sf)
library(mapview)
library(units)

hn2_lon <- 2.331271
hn2_lat <- 48.906228
hn2 <- st_point(x = c(hn2_lon, hn2_lat), dim = "XY")
hn2 <- hn2 %>% st_sfc(crs = 4326)

hn2_buffer <-  st_buffer(hn2, 3000)

mapview::mapview(hn2_buffer)


p <- readr::read_rds('~/Devel/finess_etalab/data_results/etalab-cs1100507-stock-20210913-0412-wgs84.rds') %>% 
  filter(categagretab %in% c("1101", "1102", "1103", "1104", "1106", "1107", "1109", "1110")) %>% 
  select(nofinesset, rs, coordxet, coordyet, lat, lon)
pnts_sf <- sf::st_as_sf(p, crs = 4326L, coords = c("lon", "lat"))

ok = lengths(st_intersects(pnts_sf, hn2_buffer)) > 0
plot(pnts_sf[ok,], pch=19, col="blue", add=TRUE)
mapview::mapview(pnts_sf[ok,])
