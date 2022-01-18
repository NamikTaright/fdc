# https://r-spatial.org/r/2019/09/26/spatial-networks.html
# 
library(dplyr, warn.conflicts = F)
library(sf)
library(osrm)
options(osrm.server = "https://routing.openstreetmap.de/", osrm.profile = "car")

library(osmdata)
library(sfnetworks)
library(tidygraph)
library(tidyverse)
library(igraph)

# Import des données cartographiques des communes
mtq <- st_read(dsn = "../Data/idf.gpkg")

# Extraction des finess avec la longitude et la latitude
readr::read_rds('~/Devel/finess_etalab/data_results/etalab-cs1100507-stock-20210913-0412-wgs84.rds') %>% 
  filter(nofinessej == "750712184") %>% 
  select(nofinesset, rs, coordxet, coordyet, lat, lon) -> p

# pnts_sf <- sf::st_as_sf(p, crs = 4326L, coords = c("coordxet", "coordyet"))
# coordinate reference system : lambert-93 (epsg : 2154)
pnts_sf <- sf::st_as_sf(p, crs = 4326L, coords = c("lon", "lat"))

# les centroïdes des communes de Paris
paris <- mtq %>% filter(substr(INSEE_COM, 1, 2) %in%  c("75", "92", "93", "94")) #,  "78", "91", "77", "95")) 
paris_c <- st_transform(st_centroid(paris), 4326)

disteucl <- st_distance(paris_c, pnts_sf[c(38,80),])

plot(st_geometry(paris))
plot(st_geometry(paris_c), add=TRUE, cex=1.2, col="red", pch=20)

distA2 <- osrmTable(src = paris_c[, c("INSEE_COM","geom")],
                    dst = pnts_sf[c(38,80), c("nofinesset", "geometry")])


# --------------------------------------------------
q <- opq(bbox=st_bbox(st_transform(mtq, 4326)))
res <- add_osm_feature(opq = q, key = 'highway', value = "residential")
res.sf <- osmdata_sf(res)
res.sf.pts  <- res.sf$osm_points[!is.na(res.sf$osm_points$highway),]
resto <- st_transform(res.sf.pts, st_crs(mtq))
plot(st_geometry(mtq), col="darkseagreen3", border="darkseagreen4", bg = "lightblue1")
plot(st_geometry(resto), add=TRUE, pch=20, col = "#330A5FFF", cex = 0.5)
title("Répartition des restaurants pour Paris et les trois départements de la petite couronne")
mtext(text = "INSEE, 2016 - OSM, 2021",side = 1, line = -1, cex = 0.8)

resto <- st_transform(mtq, 4326)
plot(st_geometry(resto), add=TRUE, pch=20, col = "#330A5FFF", cex = 0.5)

#
p1 = st_point(c(2.3078, 48.8922))
p2 = st_point(c(2.3687, 48.8922))
p3 = st_point(c(2.3687, 48.9248))
p4 = st_point(c(2.3078, 48.9248))
poly = st_multipoint(c(p1, p2, p3, p4)) %>%
  st_cast("POINT") %>%
  st_sfc(4326)

net = as_sfnetwork(roxel) %>%
  st_transform(3035)

filtered = st_filter(mtq, poly, .pred = st_intersects)

plot(st_geometry(mtq), col = "grey")
plot(poly, border = "red", lty = 4, lwd = 4, add = TRUE)
plot(filtered)
