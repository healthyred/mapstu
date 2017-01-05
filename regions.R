#this is the script to describe the classes of the regions
#plans to take in the data set
install.packages("tmap")
install.packages("leaflet")
library("tmap")
library("leaflet")

#a function that takes in present and past which are vectors of the dataframe and calculates the absolute change of students
#that come to Williams from each state/country
absolutechange <- function(present , past, name){
  present-past
}

#Create a dataframe with the countries and the colors attached to each country dependent on the absolute change
color <- function(){

}


usshapefile <- "data/cb_2014_us_county_5m/cb_2014_us_county_5m.shp"
usgeo <- read_shape(file=usshapefile)

