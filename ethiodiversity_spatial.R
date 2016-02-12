## CLEAR
ls()
rm(list=ls())
ls()

library(raster)
library(rgdal)
library(rgeos)
library(maptools)
library(plyr)
library(ggplot2)

# Functions
quick.subset = function(x, longlat){
  
  # longlat should be a vector of four values: c(xmin, xmax, ymin, ymax)
  x@data$id = rownames(x@data)
  
  x.f = fortify(x, region="id")
  x.join = join(x.f, x@data, by="id")
  
  x.subset = subset(x.join, x.join$long > longlat[1] & x.join$long < longlat[2] &
                      x.join$lat > longlat[3] & x.join$lat < longlat[4])
  
  x.subset
}



# Read in data
points = read.csv("Apr 2015/Rdata.csv")

# Read in map data

# Climate change
# Source: https://cds.nccs.nasa.gov/nex-gddp/

# Net Primary Productivity
# Source: http://neo.sci.gsfc.nasa.gov/view.php?datasetId=MOD17A2_M_PSN
raster = stack("rawdata/MOD17A2_M_PSN_2015-12-01_rgb_3600x1800.TIFF")
# INSERT CODE HERE TO LOOP THROUGH ALL GEOTIFFS AND IMPORT DATA FOR ETH
# INCLUDE ALL DATA, DON'T TAKE AVERAGES
# SUBSET FOR EACH ZONE, THEN DEAL WITH DATA
# RE-DOWNLOAD DATA FOR 2007

# Biodiversity
# Source: GBIF.org (28th January 2016) GBIF Occurrence Download http://doi.org/10.15468/dl.3icitz
biodiv = read.csv("rawdata/0002267-160118175350007.csv", header=TRUE, sep="\t")
# SUBSET BY ZONE

# Agricultural production
# Source: http://www.csa.gov.et/images/general/news/crop_utlize_2007

# Cultural boundaries
# Source: http://www.naturalearthdata.com/downloads/10m-cultural-vectors/
provinces.shp = readOGR(dsn="rawdata",layer="ne_10m_admin_1_states_provinces")
# New source: http://mapeastafrica.com/countries/east-africa-shapefiles/ethiopia-shapefiles/
# Or: http://www.gadm.org/
zones = readRDS("rawdata/ETH_adm2.rds")
woredas = readRDS("rawdata/ETH_adm3.rds")



# Subset data
domain = c(32.7, 48.3, 2.9, 15.3)

raster.crop = crop(raster, y=extent(domain))
raster.table = data.frame(xyFromCell(raster.crop, 1:ncell(raster.crop)),
                          getValues(raster.crop))
colnames(raster.table) = c("x","y","rgb")

provinces.shp = subset(provinces.shp, adm0_a3 == "ETH")



# Set NPP '255' values to NA
raster.table[,3] = sapply(raster.table[,3], function(x) as.integer(gsub(255, -1, x)))



# Collate NPP data for each province