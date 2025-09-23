library(sf)
library(dplyr)

#this script follows two assumptions: 
#1. that DO, PN and RR study area polygons were used to subset GEDI data.
#   GEDI data subsetting at the download stage significantly lowers required 
#   disk space and processing time needed to prepare the data
#
#2. that downloaded GEDI files were extracted from HDF files to .gpkg files
#   this step can be done, for example, using GEDI Subsetter python script

#list GEDI measurements located in study areas
glist_do = list.files("path/to/gedi/files/located/in/DO", pattern = ".gpkg$", full.names = TRUE)
glist_pn = list.files("path/to/gedi/files/located/in/PN", pattern = ".gpkg$", full.names = TRUE)
glist_rr = list.files("path/to/gedi/files/located/in/RR", pattern = ".gpkg$", full.names = TRUE)

#define function that will:
# -read GEDI data
# -filter out bad quality measurements
# -calculate measurement acquisition date
# -transform CRS to EPSG:2180 (PL-1992)
f_rqfd = function(gedilist) {
  g = sf::st_read(gedilist)
  g = dplyr::filter(g, quality_flag != 0)
  g = dplyr::filter(g, degrade_flag == 0)
  g = dplyr::mutate(g, acq_date = as.POSIXct("2018-01-01") + delta_time)
  g = sf::st_transform(g, 2180)
  
  return(g)
}
#apply prepared function to all GEDI measurements
g_do = lapply(glist_do, f_rqfd)
g_do = do.call(rbind, g_do)
g_pn = lapply(glist_pn, f_rqfd)
g_pn = do.call(rbind, g_pn)
g_rr = lapply(glist_rr, f_rqfd)
g_rr = do.call(rbind, g_rr)

#write processed GEDI measurements to file
st_write(g_do, "path/to/gedi/measurements/in/DO/study/area")
st_write(g_pn, "path/to/gedi/measurements/in/PN/study/area")
st_write(g_rr, "path/to/gedi/measurements/in/RR/study/area")