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
# Average a year's (or more) worth of NPP data?

# Cultural boundaries
# Source: http://www.naturalearthdata.com/downloads/10m-cultural-vectors/
countries = readOGR(dsn="rawdata",layer="ne_10m_admin_0_countries")
provinces = readOGR(dsn="rawdata",layer="ne_10m_admin_1_states_provinces_lines")
provinces.shp = readOGR(dsn="rawdata",layer="ne_10m_admin_1_states_provinces")



# Subset data
domain = c(32.7, 48.3, 2.9, 15.3)

raster.crop = crop(raster, y=extent(domain))
raster.table = data.frame(xyFromCell(raster.crop, 1:ncell(raster.crop)),
                        getValues(raster.crop))
colnames(raster.table) = c("x","y","rgb")

countries.subset = quick.subset(countries, domain)
provinces.subset = quick.subset(provinces, domain)

provinces.shp = subset(provinces.shp, adm0_a3 == "ETH")

# Set NPP '255' values to NA
raster.table[,3] = sapply(raster.table[,3], function(x) as.integer(gsub(255, -1, x)))



# Plot
windows(); ggplot(data = raster.table, aes(x = x, y = y)) +
  geom_tile(aes(fill = rgb)) +
  scale_fill_distiller(type="div", palette="Greens", direction = 1) +
  geom_path(data=countries.subset, aes(x = long, y = lat, group = group), color = "#000000", size = 1, lty = "solid") +
  geom_path(data=provinces.subset, aes(x = long, y = lat, group = group), color = "#000000", size = 0.5, lty = "solid") +
  coord_cartesian(xlim=c(domain[1],domain[2]), ylim=c(domain[3],domain[4])) +
  xlab("") + ylab("") +
  theme(panel.grid.minor = element_line(colour = NA),
        panel.grid.minor = element_line(colour = NA),
        panel.background = element_rect(fill = NA, colour = NA))
