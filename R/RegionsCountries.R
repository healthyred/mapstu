#' CountryRegions
#'
#' Takes in two years worth of datasets and returns a map of the
#' international countries that maps the difference of the two datasets
#'
#' @param currentyear The vector for the most current year
#' @param oldyear The vector for the year you want to compare to
#' @param name The name that the map should be saved to
#' @param title The title of the plot
#' @return a s4 object that has the difference of the datasets mapped to it
#'
#' @import tmap leaflet
#' @export
#'

library("tmap")
library("leaflet")
library("tmaptools")

countrymap <- function(currentyear, oldyear, namesave, title){

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

  #creating a copy matrix of a 2 by 207 matrix and filling in the first columnn with the countries
  copy <- as.data.frame(matrix(0, ncol = 1, nrow =207))
  copy$V1 <- yearsdata$State.Countries
  colnames(copy)[1] <- "NAME"

  #Adds the absolutechange colomn to each data frame
  copy$Change <- c(currentyear - oldyear)

  #reads in the map data of the U.S.
  countryshpfiles ="~/mapstu/inst/extdata/TM_WORLD_BORDERS-0.3/TM_WORLD_BORDERS-0.3.shp"
  countrygeo <- read_shape(file = countryshpfiles)

  #Must correct democratic republic of congo, republic of korea, georgia, bosnia and herzgonivia, trinidad and tobago, united republic of tanzania
  copy$NAME <- as.character(copy$NAME)
  copy[copy$NAME == "Districtof Columbia", 'NAME'] <- "District of Columbia"
  copy[copy$NAME == "Trinidadand Tobago", 'NAME'] <- "Trinidad and Tobago"
  copy[copy$NAME == "Muscatand Oman", 'NAME'] <- "Oman"
  copy[copy$NAME == "Republicof Korea", 'NAME'] <- "Trinidad and Tobago"
  copy[copy$NAME == "Georgia", 'NAME'] <- "Georgia(State)"
  copy[copy$NAME == "Georgia(Country)", 'NAME'] <- "Georgia"
  copy[copy$NAME == "Laos", 'NAME'] <- "Lao People's Democratic Republic"

  #Creating map
  countrygeo@data <- data.frame(countrygeo@data, copy[match(countrygeo@data[,"NAME"], copy[,"NAME"]),])

  #Code for the interative map and the legends, titles, and other map things
  break1 = c(-Inf ,-27, -24, -21, -18, -15, -12, -9, -6, -3, -1, 1, 3, 6, 9, 12, 15, 18, 21, 24, 27, Inf)
  RGcolors = c(red10, red9, red8, red7, red6, red5, red4, red3, red2, red1, gray, green1, green2, green3, green4, green5, green6, green7, green8, green9, green10)
  countrymap <- tm_shape(countrygeo) + tm_polygons("Change", breaks = break1, palette = RGcolors , contrast=.7, id="name", title= title) + tm_style_gray() + tm_format_World()

  #Saving the map
  save_tmap(countrymap, namesave)

  #Returns the desired s4 object
  return(countrymap)
}

#Creates the desired maps from year to year
countrymap2013.2014 <- countrymap(yearsdata$X2014, yearsdata$X2013, "usmap2002.2003.png", "Change in Students 2002-2003")


#Calls the maps
