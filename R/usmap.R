#' US Map
#'
#' Takes in two years worth of datasets and returns a map of the
#' US States that maps the difference of the two datasets
#'
#' @param currentyear The vector of a dataset for the most current year
#' @param oldyear The vector of a dataset for the year you want to compare to
#' @param title The title of the plot and the name which the plot is saved to
#' @param save Whether you want to save or not
#' @return a s4 object that has the difference of the datasets mapped to it
#'
#' @examples
#' ##Creating the map of the US with data from 2001 and 2002
#' usmap2000.2002 <- usmap(yearsdata$X2001, yearsdata$X2000, "Change in Students 2000-2001")
#'
#' @import tmap leaflet
#' @export
#'

##This is a function that takes in two years of the William's college students Geographical Distribution data
##and returns a plot that maps RG values that represents the difference between the designated two years

usmap <- function(currentyear, oldyear, title, save = FALSE){
  library(leaflet)
  library(tmap)
  library(tmaptools)

  ##List of colors for convenience(lightestshade to darkest shade) Can use this for the created palette
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

  ##creating a copy matrix of a 2 by 207 matrix and filling in the first columnn with the countries
  copy <- as.data.frame(matrix(0, ncol = 1, nrow =207))
  copy$V1 <- yearsdata$State.Countries
  colnames(copy)[1] <- "NAME"

  ##Adds the absolutechange colomn to each data frame
  copy$Change <- c(currentyear - oldyear)

  ##reads in the map data of the U.S.
  nationshapefile <- "~/mapstu/inst/extdata/cb_2015_us_state_20m/cb_2015_us_state_20m.shp"
  nationgeo <- read_shape(file = nationshapefile)

  ##Corrects the Districtof Columbia issue
  copy$NAME <- as.character(copy$NAME)
  copy[copy$NAME == "Districtof Columbia", 'NAME'] <- "District of Columbia"

  ##Creating map
  nationgeo@data <- data.frame(nationgeo@data, copy[match(nationgeo@data[,"NAME"], copy[,"NAME"]),])

  ##Code for the interative map and the legends, titles, and other map things
  break1 = c(-Inf ,-27, -24, -21, -18, -15, -12, -9, -6, -3, -1, 1, 3, 6, 9, 12, 15, 18, 21, 24, 27, Inf)
  RGcolors = c(red10, red9, red8, red7, red6, red5, red4, red3, red2, red1,
               gray, green1, green2, green3, green4, green5, green6, green7, green8, green9, green10)
  statemap <- tm_shape(nationgeo) + tm_polygons("Change",
                                                breaks = break1,
                                                palette = RGcolors ,
                                                contrast=.7,
                                                id="name",
                                                title= title) + tm_style_gray() + tm_format_World()

  ##Saving the map
  if (save == TRUE){
    save_tmap(statemap, paste(title, ".png", sep = ""))
  }


  ##Returns the desired s4 object
  return(statemap)
}
