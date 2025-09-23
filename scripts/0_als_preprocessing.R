library(lidR)
library(sf)
library(future)

#### Step 1 - normalise height of the point clouds ####
#read ALS point clouds as LAS Catalog
als = readALSLAScatalog("path/to/ALS/point/clouds", filter = "-keep_class 2 3 4 5")
#make sure that point clouds have appropriate CRS - EPSG:2180 (PL-1992)
st_crs(als) = 2180
#create naming convention for point clouds with normalised height
opt_output_files(als) = "/path/to/ALS/pont/clouds/{ORIGINALFILENAME}_norm"
#plot the data that is going to be processed
plot(als)
#define multisession processing strategy to speed up the process
plan(multisession, workers = 5L)
#normalise height of point clouds using tin algorithm
als_norm = normalize_height(als, algorithm = tin())

#### Step 2 - calculate reference point cloud metrics for GEDI measurements ####
#define function calculating canopy cover
canopyCover = function(Z, RN, hgtbreak) {
  above_hgtbreak = length(which(RN == 1 & Z >= hgtbreak))
  firstReturns = length(which(RN == 1))
  cover = above_hgtbreak/firstReturns
  return(cover)
}
#define function to apply height percentile, VCI and canopy cover functions
myMetrics = function(Z, RN, zmax)
{metrics = list(
  p100 = max(Z),
  p099 = quantile(Z, 0.99),
  p098 = quantile(Z, 0.98),
  p097 = quantile(Z, 0.97),
  p096 = quantile(Z, 0.96),
  p095 = quantile(Z, 0.95),
  p094 = quantile(Z, 0.94),
  p093 = quantile(Z, 0.93),
  p092 = quantile(Z, 0.92),
  p091 = quantile(Z, 0.91),
  p090 = quantile(Z, 0.90),
  vcp = VCI(Z, by = 1, zmax),
  ccv = canopyCover(Z, RN, 2)
)
return(metrics)
}

#read GEDI L2A measurements
#this script assumes that GEDI data was processed using 1_gedi_data_preprocessing script located in the repository
g_do = st_read("path/to/gedi/measurements/in/DO/study/area")
g_pn = st_read("path/to/gedi/measurements/in/PN/study/area")
g_rr = st_read("path/to/gedi/measurements/in/RR/study/area")

#define multisession processing strategy to speed up the process
plan(multisession, workers = 5L)
#calculate reference ALS point cloud metrics for GEDI measurements
ref_metrics_do = plot_metrics(als_norm, func = myMetrics(Z, ReturnNumber, 40), g_do)
ref_metrics_pn = plot_metrics(als_norm, func = myMetrics(Z, ReturnNumber, 40), g_pn)
ref_metrics_rr = plot_metrics(als_norm, func = myMetrics(Z, ReturnNumber, 40), g_rr)

#write calculated metrics
st_write(ref_metrics_do, "path/to/output/file_do.gpkg")
st_write(ref_metrics_pn, "path/to/output/file_pn.gpkg")
st_write(ref_metrics_rr, "path/to/output/file_rr.gpkg")
