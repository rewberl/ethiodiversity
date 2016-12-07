## CLEAR
ls()
rm(list=ls())
ls()

library(dplyr)
library(tidyr)
library(rgbif)

# Read in combined tidy data
c31 = read.csv("data/combined-3.1.csv", header=T, stringsAsFactors=F)
c32 = read.csv("data/combined-3.2.csv", header=T, stringsAsFactors=F)

summary(c31)
summary(c32)

aggregate(population ~ ethnic.group, c31, sum)[order(-aggregate(population ~ ethnic.group, c31, sum)$population),]
aggregate(population ~ language, c32, sum)[order(-aggregate(population ~ language, c32, sum)$population),]

c31.no0 = c31[c31$population != 0 & !is.na(c31$population),]
c32.no0 = c32[c32$population != 0 & !is.na(c32$population),]

# Can find references to there being no registered speakers of Mejengerigna or Shetagna by searching for "mejengerigna" online
# For instance, this looks helpful: https://msu.edu/~hudson/HECcensus.pdf


# Retrieve GBIF data

# Animalia
gbif.ani = occ_search(country="ET", hasCoordinate=T, kingdomKey=1, limit=199999, return="all")
save(gbif.ani, file=paste("data\\gbf.ani_", Sys.Date(), ".RData", sep=""))

# Plantae
gbif.pla = occ_search(country="ET", hasCoordinate=T, kingdomKey=6, limit=199999, return="all")
save(gbif.pla, file=paste("data\\gbf.pla_", Sys.Date(), ".RData", sep=""))

occ_get(key=766766824, fields="all")





# Calculate Shannon and Simpson diversity indices for ethnic groups and languages
# Shannon: H' = -\sum_{i=1}^{R} p_i ln(p_i)
# Simpson: \lambda = \sum_{i=1}^{R} p_i^2

# Ethnic Diversity
e.diversity = data.frame(region=character(),
                       zone=character(),
                       richness=numeric(),
                       shannon=numeric(),
                       inv.simpson=numeric(),
                       stringsAsFactors=F)
for (i in 1:length(unique(c31.no0$region))) {  # Across region
  region = c31.no0[c31.no0$region == unique(c31.no0$region)[i],]
  for (j in 1:length(unique(region$zone))) {  # Across zone
    zone = region[region$zone == unique(region$zone)[j],]
    H = 0
    lambda = 0
    R = length(zone$ethnic.group)
    N = sum(zone$population)
    for (k in 1:R) {  # Across ethnic group
      n = sum(zone$population[zone$ethnic.group == zone$ethnic.group[k]])
      p = n / N
      plnp = p * log(p)
      psq = p^2
      H = H + plnp
      lambda = lambda + psq
    }
    H = -H
    invlambda = 1 / lambda
    e.diversity[nrow(e.diversity)+1,] = list(unique(c31.no0$region)[i],
                                            unique(region$zone)[j],
                                            R,
                                            H,
                                            invlambda)
  }
}


# Linguistic Diversity
l.diversity = data.frame(region=character(),
                         zone=character(),
                         richness=numeric(),
                         shannon=numeric(),
                         inv.simpson=numeric(),
                         stringsAsFactors=F)
for (i in 1:length(unique(c32.no0$region))) {  # Across region
  region = c32.no0[c32.no0$region == unique(c32.no0$region)[i],]
  for (j in 1:length(unique(region$zone))) {  # Across zone
    zone = region[region$zone == unique(region$zone)[j],]
    H = 0
    lambda = 0
    R = length(zone$language)
    N = sum(zone$population)
    for (k in 1:R) {  # Across language
      n = sum(zone$population[zone$language == zone$language[k]])
      p = n / N
      plnp = p * log(p)
      psq = p^2
      H = H + plnp
      lambda = lambda + psq
    }
    H = -H
    invlambda = 1 / lambda
    l.diversity[nrow(l.diversity)+1,] = list(unique(c32.no0$region)[i],
                                          unique(region$zone)[j],
                                          R,
                                          H,
                                          invlambda)
  }
}


# EDA on diversity data

# Mean diversity by region
aggregate(cbind(richness, shannon, inv.simpson) ~ region, e.diversity, mean)[order(-aggregate(cbind(richness, shannon, inv.simpson) ~ region, e.diversity, mean)$shannon),]
aggregate(cbind(richness, shannon, inv.simpson) ~ region, l.diversity, mean)[order(-aggregate(cbind(richness, shannon, inv.simpson) ~ region, l.diversity, mean)$shannon),]

# Median diversity by region
aggregate(cbind(richness, shannon, inv.simpson) ~ region, e.diversity, median)[order(-aggregate(cbind(richness, shannon, inv.simpson) ~ region, e.diversity, median)$shannon),]
aggregate(cbind(richness, shannon, inv.simpson) ~ region, l.diversity, median)[order(-aggregate(cbind(richness, shannon, inv.simpson) ~ region, l.diversity, median)$shannon),]


# Visualization
windows()
par(mfrow=c(2,3))
hist(e.diversity$richness, xlim=c(0, 400), ylim=c(0, 35))
hist(e.diversity$shannon, xlim=c(0, 10), ylim=c(0, 35))
hist(e.diversity$inv.simpson, xlim=c(0, 2.5), ylim=c(0, 60))
hist(l.diversity$richness, xlim=c(0, 400), ylim=c(0, 35))
hist(l.diversity$shannon, xlim=c(0, 10), ylim=c(0, 35))
hist(l.diversity$inv.simpson, xlim=c(0, 2.5), ylim=c(0, 60))
