## CLEAR
ls()
rm(list=ls())
ls()

library(dplyr)
library(tidyr)

combine = FALSE

### UN-COMMENT NAME OF REGION TO PROCESS ###
targetRegion =
#  "addis.ababa"
#  "affar"
#  "amhara"
#  "benishangul-gumuz"
#  "dire.dawa"
#  "gambela"
#  "harari"
#  "oromiya"
#  "snnpr"
#  "somali"
#  "tigray"

### UN-COMMENT TYPE OF DATA TO PROCESS ###
dataType =
#  "ethnic.group"
#  "language"

### UN-COMMENT TO COMBINE ALL OUTPUT TIDY DATASETS OF SELECTED TYPE ###
# combine = TRUE



# Read list of political regions and zones
zones = read.csv("data/zones.csv", header=FALSE, fill=TRUE,
                 stringsAsFactors=FALSE)
regions = zones[,1]
zones = zones[,-1] %>%
  split(seq_len(nrow(zones))) %>%
  lapply(function(x) x[x != ""])
names(zones) = regions

# Read list of ethnicities
ethnicities = scan("data/ethnicities.csv", character(), sep=",",
                   quiet=TRUE)

# Read list of languages
languages = scan("data/languages.csv", character(), sep=",",
                 quiet=TRUE)

# Convert targetRegion to place in vector of regions
targetRegionNumber = match(targetRegion, regions)

# Convert dataType to table number
if (dataType == "ethnic.group")
  {dataTypeNumber = "3.1"} else
  {dataTypeNumber = "3.2"}



# Open connection to file
fPath = file.path("rawdata/", targetRegion, "-", dataTypeNumber,
                   ".txt", fsep="")
f = file(fPath)

# Read in fixed width table
text = read.fwf(f, widths = c(20, 12, 12, 12, 12, 12, 12, 12, 12, 12))

# Remove blank rows
text = na.omit(text)

# Remove separators and headers
text = text[!grepl("--", text[,2]),]
text = text[!grepl("URBAN", text[,3]),]
if (dataType == "ethnic.group")
  {text = text[!grepl("Ethnic", text[,1]),]} else
  {text = text[!grepl("Mother", text[,1]),]}
if (dataType == "ethnic.group")
  {text = text[!grepl("All", text[,1]),]} else
  {text = text[!grepl("Total", text[,1]),]}


# Add region and zones
if (dataType == "ethnic.group") {
  text = cbind(regions[targetRegionNumber],
               rep(zones[[targetRegionNumber]], each=length(ethnicities)),
               text)
} else {
  text = cbind(regions[targetRegionNumber],
               rep(zones[[targetRegionNumber]], each=length(languages)),
               text)
}

# Replace data type names
if (dataType == "ethnic.group")
  {text[,3] = ethnicities} else
  {text[,3] = languages}

# Replace column names
colnames(text) = c("region","zone",dataType,
                   "urbanrural.mf","urbanrural.m","urbanrural.f",
                   "urban.mf","urban.m","urban.f",
                   "rural.mf","rural.m","rural.f")

# Change "-" NA character to 0
text[,4:12] = lapply(text[,4:12], function(x) gsub("-", 0, x))

# Remove extraneous alphabetic characters, spaces, and commas
text[,4:12] = lapply(text[,4:12], function(x) gsub("[A-z]", "", x))
text[,4:12] = lapply(text[,4:12], function(x) gsub("\\s", "", x))
text[,4:12] = lapply(text[,4:12], function(x) as.numeric(gsub(",", "", x)))



# Tidy dataset
text.tidy = gather(text, "residence.sex", "population", 4:12) %>%
  separate("residence.sex", c("residence", "sex"))
  
# Remove combined data (region totals, combined residency, combined sex)
#   and nonexistent rural population for Addis Ababa
if (targetRegion != "addis.ababa" & targetRegion != "dire.dawa" &
    targetRegion != "harari") {
  text.tidy = subset(text.tidy, zone != "region")
}
text.tidy = subset(text.tidy, residence != "urbanrural")
text.tidy = subset(text.tidy, sex != "mf")
if (targetRegion == "addis.ababa") {
  text.tidy = subset(text.tidy, residence != "rural")
}



# Output tidy dataset
outputPath = file.path("data/", targetRegion, "-",
                       dataTypeNumber, "-tidy.csv", fsep="")
write.csv(text.tidy, outputPath, row.names=FALSE)



# Combine all available tidy datasets of selected type
if (combine == TRUE) {
  combineNames = list.files(path="data/", full.names=TRUE)
  combineNames = combineNames[grepl(paste(dataTypeNumber, "-tidy.csv", sep=""),
                                    combineNames)]
  combineList = lapply(combineNames, read.csv, header=TRUE,
                       stringsAsFactors=FALSE)
  combineData = do.call(rbind, combineList)
  rm(combineList)
  write.csv(combineData, file.path("data/combined-", dataTypeNumber, ".csv",
                                   fsep=""), row.names=FALSE)
}