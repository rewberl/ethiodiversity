## CLEAR
ls()
rm(list=ls())
ls()


setwd("Q:\Research\Analysis\ethiodiversity")
library(stringr)

# Open Connection to file
pathToFile <- path.expand("~/path/to/file/myfile.txt")
f <- file(pathToFile, "rb")  

# Read in lines
rawText <- readLines(f)


# Find the dahses
dsh <- str_locate_all(rawText, " - ")

# Splice, using the dashes as a guide
lng <- length(rawText)
spliced <- sapply(1:lng, function(i) 
  spliceOnDash(rawText[[i]], dsh[[c(i, 1)]], dsh[[c(i, 2)]])
)

# make it purtty
nicelyFormatted <- formatNicely(spliced)
nicelyFormatted


#-------------------#
#    FUNCTIONS      #
#-------------------#


spliceOnDash <- function(strn, start, end)  {
  
  # split around the date
  pre <- substr(strn, 1, start-6)
  dates <- substr(strn, start-5, end+5)
  post <- substr(strn, end+6, str_length(strn))
  
  # Clean up
  pre <- str_trim(pre)
  
  # replace all double spaces with single spaces
  while(str_detect(post, "  ")) {
    post <- str_replace_all(str_trim(post), "  ", " ")    
  }
  
  # splice on space
  post <- str_split(post, " ")
  
  # if dates are one field, remove this next line
  dates <- str_split(dates, " - ")
  
  # return
  c(unlist(pre), unlist(dates), unlist(post))
}

# Function to clean up the list into a nice table
formatNicely <- function(spliced)  {
  lngst <- max(sapply(spliced, length))
  t(sapply(spliced, function(x)  
    if(length(x) < lngst) c(x, rep(NA, lngst-length(x))) else x ))