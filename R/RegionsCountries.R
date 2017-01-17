#This attempts to map all the foreign countries
#plans to take in the data set
install.packages("tmap")
install.packages("leaflet")
install.packages("stringdist")
library("tmap")
library("leaflet")
library("tmaptools")
library("stringdist")
library(raster)

#List of colors for convenience(lightestshade to darkest shade) Can use this for the created palette
green1 <- "#dbefd3"
green2 <- "#c1eeb4"
green3 <- "#9de686"
green4 <- "#78dd54"
green5 <- "#4fc431"
green6 <- "#47a426"
green7 <- "#37801d"
green8 <- "#265b12"
green9 <- "#1b4109"
green10 <- "#081f02"
gray <- "white"
red1 <- "#ffcacb"
red2 <- "#ff7f80"
red3 <- "#ff3235"
red4 <- "#ff4c4e"
red5 <- "#ff5656"
red6 <- "#f90204"
red7 <- "#e60000"
red8 <- "#cd0002"
red9 <- "#b20001"
red10 <- "#800000"

countryshpfile ="~/HutchinHill/ne_110m_admin_0_countries/ne_110m_admin_0_countries.shp"
countries2000.2002 <- read_shape(file = countryshpfile)
