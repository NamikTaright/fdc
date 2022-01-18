# Préparation du polygone de Paris et de la petite ou grande couronne
library(rgdal)
library(rgeos)
library(spdep)
library(sf)
library(dplyr, warn.conflicts = F)
# Téléchargement des données
# https://geoservices.ign.fr/adminexpress
# Chargement des donnéess
an <- '2018'
home <- "/home/namik/Ressources/GEO/"
v <- "1-1"
racine <- paste0("ADMIN-EXPRESS-COG_", v, "__SHP__FRA")
proj <- ""
suffixe <- "_2018-04-03/"
suffixe2 <- "_2018-04-03/"
predirshp <- paste0(home, racine, proj, suffixe)
suite <- paste0("ADMIN-EXPRESS-COG/1_DONNEES_LIVRAISON", suffixe2)
fin <- paste0("ADE-COG_", v, "_SHP_LAMB93_FR")
dirshp <- paste0(predirshp, suite, fin)
  
# Départements de l'Île-de-France
deps <- st_read(dsn=dirshp, layer="DEPARTEMENT", quiet = FALSE)
depres = c('75', '92', '93', '94', '78', '95', '77', '91')
depppc <- deps[substr(deps$INSEE_DEP, 1, 2) %in% depres,]
ppcbordure <- rgeos::gUnaryUnion(depppc) 

# communes de l'Île-de-France
deps <- st_read(dsn=dirshp, layer="COMMUNE")
depres = c('92', '93', '94', '78', '95', '77', '91')
deppc <- deps[substr(deps$INSEE_DEP, 1, 2) %in% depres,]

# entités rattachées de l'Île-de-France
entr <- st_read(dsn=dirshp, layer="ARRONDISSEMENT_MUNICIPAL", quiet = FALSE)
depres = c('75')
commpar <- entr[substr(entr$INSEE_COM, 1, 2) %in% depres,]

# communes + arrondissements
region <- deppc %>% 
  select(ID, NOM, NOM_M, INSEE_COM, POPULATION, STATUT) %>% 
  rbind(commpar %>% select(-c(INSEE_ARM)) %>% mutate(STATUT = 'Arrondissement')) %>%
  rename(NOM_COM = NOM,
         NOM_COM_M = NOM_M,
         TYPE = STATUT)

plot(st_geometry(region))

st_write(obj = region, dsn = paste0("../Data/idf_", an, ".gpkg"), layer = "region", delete_layer = TRUE)

# fond de carte pour Paris et la petite couronne
# communes de la petite couronne + arrondissements de Paris
depres = c('75', '92', '93', '94')
ppc <- region %>% filter(substr(INSEE_COM, 1, 2) %in% depres)

plot(st_geometry(ppc))

st_write(obj = ppc, dsn = paste0("../Data/ppc_", an, ".gpkg"), layer = "region", delete_layer = TRUE)
