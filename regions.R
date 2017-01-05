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

#List of colors for convenience
green1 <- #dbefd3
green2 <- #c1eeb4
green3 <- #9de686
green4 <- #78dd54
green5 <- #4fc431
green6 <- #47a426
green7 <- #37801d
green8 <- #265b12
green9 <- #1b4109
green10 <- #081f02


#a function that takes in the absolutechange of two vectors and
#returns a dataframe with the countries and the colors (based on scales of very red to very green)
#attached to each country dependent on the absolute change
color <- function(value){
  copy <- as.data.frame(matrix(0, ncol = 2, nrow =207))
  copy$V1 <- totalframe14$State.Countries
  colnames(copy)[1] <- "States.Countries"
  colnames(copy)[2] <- "Color"
}


usshapefile <- "data/cb_2014_us_county_5m/cb_2014_us_county_5m.shp"
usgeo <- read_shape(file=usshapefile)

