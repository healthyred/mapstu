#this is the script to describe the classes of the regions
#plans to take in the data set
install.packages("tmap")
install.packages("leaflet")
library("tmap")
library("leaflet")


usshapefile <- "data/cb_2014_us_county_5m/cb_2014_us_county_5m.shp"
usgeo <- read_shape(file=usshapefile)

