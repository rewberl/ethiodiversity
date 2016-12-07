## CLEAR
ls()
rm(list=ls())
ls()

library(dplyr)
library(tidyr)

# Read in combined tidy data
c31 = read.csv("data/combined-3.1.csv", header=T)
c32 = read.csv("data/combined-3.2.csv", header=T)

summary(c31)
summary(c32)

aggregate(population ~ ethnic.group, c31, sum)[order(-aggregate(population ~ ethnic.group, c31, sum)$population),]
aggregate(population ~ language, c32, sum)[order(-aggregate(population ~ language, c32, sum)$population),]

# Can find references to there being no registered speakers of Mejengerigna or Shetagna by searching for "mejengerigna" online
# For instance, this looks helpful: https://msu.edu/~hudson/HECcensus.pdf

# Calculate Shannon and Simpson diversity indices

# Shannon: -\sum_{i=1}^{R} p_i ln(p_i)
for (i in 1:I) {  # Across region
  for (j in 1:J) {  # Across zone
    for (k in 1:K) {  # Across ethnic group/language
      
    }
  }
}