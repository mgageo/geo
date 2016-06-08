# <!-- coding: utf-8 -->
#
#
# quelques fonctions d'utilisation/préparation des couches
# auteur: Marc Gauthier
# licence : Paternité - Pas d’Utilisation Commerciale 2.0 France (CC BY-NC 2.0 FR)
#
mga  <- function() {
  source("geo/scripts/couches.R");
}

clc_abc <- function() {
  target_dir <- sprintf("%s/web.var/geo/COUCHES/Acigne", Drive)
  dsn <-  sprintf('%s/osm_commune.shp', target_dir)
  layers <- ogrListLayers(dsn)
  print(sprintf("clc_abc() layers:%s %s", layers, dsn))
  commune.spdf <- readOGR(dsn, layers)
  if ( interactive() ) {
    plot(commune.spdf, border="black", lwd=2, add=FALSE)
  }
  dsn <-  sprintf('%s/clc_clc12.shp', target_dir)
  layers <- ogrListLayers(dsn)
  print(sprintf("clc_abc() layers:%s", layers))
  clc.spdf <- readOGR(dsn, layers)
  clc.legendes <- clc_legend_lire()
  clc.spdf@data <- merge(clc.spdf@data, clc.legendes, by.x = "CODE_12", by.y = "Code")
  print(summary(clc.spdf))
  print(head(clc.spdf@data, 2))
  if ( interactive() ) {
    plot(clc.spdf, border="red", lwd=2, add=TRUE)
  }
}
#
# la création des couches pour un abc
clc_abc_couches <- function() {
  target_dir <- sprintf("%s/web.var/geo/COUCHES/Acigne", Drive)
  dir.create(target_dir, showWarnings = FALSE, recursive = TRUE)
  commune.spdf <- osm_commune_lire(c("35001"))
  dsn <-  sprintf('%s/osm_commune.shp', target_dir)
  writeOGR(commune.spdf, dsn, "commune", driver="ESRI Shapefile", overwrite_layer=T)
  if ( interactive() ) {
    plot(commune.spdf, border="black", lwd=2, add=FALSE)
  }
  clc.spdf <- clc_clc12_load()
  clc.spdf <- spTransform(clc.spdf, CRS("+init=epsg:2154"))
  clc.spdf <- clc.spdf[commune.spdf,]
  dsn <-  sprintf('%s/clc_clc12.shp', target_dir)
  writeOGR(clc.spdf, dsn, "clc12", driver="ESRI Shapefile", overwrite_layer=T)
  print(summary(clc.spdf))
  print(head(clc.spdf@data, 2))
  if ( interactive() ) {
    plot(clc.spdf, border="red", lwd=2, col=clc.spdf@data$RGB, add=TRUE)
  }
}
#
# pour avoir les différents secteur de Cesson pour les hirondelles rustiques
rustique <- function() {
  dico_bbox("cesson_no")
  dico_couche("GB_bzh_wmts", "carte")
  wmts_dl_bbox()
}
#
# pour avoir les différentes couches de Via Silva
viasilva <- function() {
  dico_bbox("viasilva")
  dico_couche("GB_wms", "rennesmetropole", "mnt_courbe_niveau")
  rc <- wms_dl_bbox()
}
#
# pour avoir toutes les couches sur Acigné
acigne <- function() {
  dico_bbox("acigne_la_perlais")
  dico_bbox("la_balusais")
  dico_bbox("acigne")
  dico_bbox("35")
  dico_bbox("bzh")
  dico_bbox("rennes")
  dico_bbox("lerheu")
  dico_couche("GB_satellite_wms")
  wms_dl_bbox()
  stop("lerheu()")
  dico_couche("GB_bzh_wmts", "satellite")
  wmts_dl_bbox()
  dico_couche("GB_ign_scan_wms")
  wms_dl_bbox()
  dico_couche("GB_osm_wmts", "osm:map")
  wmts_dl_bbox()
  dico_couche("GB_osm_wmts", "osm:google")
  wmts_dl_bbox()
  dico_couche("GB_osm_wmts", "osm:bing")
  wmts_dl_bbox()
  dico_couche("GB_osm_wmts", "osm:roads")
  wmts_dl_bbox()
  stop("acigne()")
  dico_couche("GB_bzh_wmts", "satellite-ancien")
  wmts_dl_bbox()
  stop("acigne()")
  dico_couche("GB_bzh_wmts", "carte")
  wmts_dl_bbox()
  dico_couche("GB_bzh_wmts", "satellite")
  wmts_dl_bbox()
  dico_couche("GB_ign_scan_wms")
  wms_dl_bbox()
  dico_couche("GB_satellite_wms")
  wms_dl_bbox()
  stop("acigne()")
  dico_couche("GB_draaf_wfs")
  wfs_dl_bbox()
  dico_couche("GB_bretagnevivante_wfs")
  wfs_dl_bbox()
  dico_couche("GB_draaf_wms")
  wms_dl_bbox()
}
#
# pour avoir la photo aérienne du 35
d35 <- function() {
  dico_bbox("35")
  couches <- c("ir-35-2012", "ortho-35", "ortho-composite", "landsat_bretagne")
  for ( couche in couches) {
    dico_couche("GB_wms", "photo", couche)
    wms_dl_bbox()
  }
}
#
# téléchargement des couches OpenStreetMap
dl_osm <- function() {
  osm_dl()
}
#
# téléchargement des couches wmts
dl_wmts <- function() {
  dico_couche("GB_osm_wmts", "osm:map")
  wmts_dl_bbox()
  dico_couche("GB_osm_wmts", "osm:google")
  wmts_dl_bbox()
  dico_couche("GB_osm_wmts", "osm:bing")
  wmts_dl_bbox()
  dico_couche("GB_osm_wmts", "osm:roads")
  wmts_dl_bbox()
}
#
# téléchargement des couches Corine Land Cover
dl_clc <- function() {
  dico_bbox("35")
  dico_couche("CarmenCarto_wfs", "", "Parcelles_Forestieres")
#  wfs_dl_bbox()
  dico_bbox("35")
  dico_couche("clc_wfs", "", "CLC12")
  wfs_dl_bbox()
}
#
# téléchargement des couches inventaires communaux des zones humides
dl_iav <- function() {
  dico_bbox("pierrick")
  dico_couche("GB_wfs", "iav", "izh_sage_vilaine")
  wfs_dl_bbox()
}
# pour avoir toutes les couches sur Rennes
rennes <- function() {
#  dl_osm()
  dico_bbox("rennes")
#  limites_administratives()
#  dl_wmts()
#  grille_emprise()
#  grille_lambert()
#  commune_les_cartes()
  maille_quartier()
}
#####################################################################################################################
#
# le contexte
Drive <- substr( getwd(),1,2)
baseDir <- sprintf("%s\\\\web", Drive)
setwd(baseDir)
source("geo/scripts/misc.R");
source("geo/scripts/couches_gdal.R");
source("geo/scripts/couches_ign.R");
source("geo/scripts/couches_osm.R");
source("geo/scripts/couches_inpn.R");
source("geo/scripts/couches_wfs.R");
source("geo/scripts/couches_wms.R");
source("geo/scripts/couches_wmts.R");
source("geo/scripts/couches_clc.R");
source("geo/scripts/couches_rpg.R");
source("geo/scripts/couches_opendatarennes.R");
source("geo/scripts/couches_bbox.R");
source("geo/scripts/couches_grille.R");
source("geo/scripts/couches_maille.R");
source("geo/scripts/couches_commune.R");
source("geo/scripts/misc_geo.R");
target_dir <- sprintf("%s/web.var/geo/COUCHES", Drive)
dir.create(target_dir, showWarnings = FALSE, recursive = TRUE)
cfg_dir <- sprintf("%s/web/geo/COUCHES", Drive)
dir.create(cfg_dir, showWarnings = FALSE, recursive = TRUE)
DEBUG <- FALSE
if ( interactive() ) {
  DEBUG <- TRUE
#  limites_administratives()
#  ign_route()
#  les_cartes()
} else {
#  les_cartes()
#  ign_dl()
#  ign_route()
#  ign_geofla()
#  ign_commune_35()
}
#opendatarennes_dl()
#opendatarennes_lire()
#  limites_administratives('35238')
# wmts_gc()
# acigne()
#  rennes()
#  d35()
#  wms_gc()
# inpn_dl()
# ign_dl()
# osm_dl()
#  wfs_gc()
# wfs_dl()
#  wmts_dl();
#  clc_legend()
#  rpg_legend()
# clc_dl()
# clchr_dl()
# clc_abc()
#  wms_gc()
# wms_dl()
# gdal_cmd('gdalinfo geo/CORINE/lceugr250_90_pct.tif')
# dl_clc()
#dl_iav()
# rustique()
viasilva();
