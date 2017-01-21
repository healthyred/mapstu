#' StateRegions
#'
#' Takes in two years worth of datasets and returns a map of the
#' US States that maps the difference of the two datasets
#'
#' @param currentyear The vector for the most current year
#' @param oldyear The vector for the year you want to compare to
#' @param name The name that the map should be saved to
#' @param title, The title of the plot
#' @return a s4 object that has the difference of the datasets mapped to it
#'
#' @import tmap leaflet
#' @export
#'
#this is the script to describe the classes of the regions
#plans to take in the data set
library("tmap")
library("leaflet")
library("tmaptools")


usmap <- function(currentyear, oldyear, namesave, title){

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
  nationshapefile <- "~/mapstu/inst/extdata/cb_2015_us_state_20m/cb_2015_us_state_20m.shp"
  nationgeo <- read_shape(file = nationshapefile)

  #Corrects the Districtof Columbia issue
  copy$NAME <- as.character(copy$NAME)
  copy[copy$NAME == "Districtof Columbia", 'NAME'] <- "District of Columbia"

  #Creating map
  nationgeo@data <- data.frame(nationgeo@data, copy[match(nationgeo@data[,"NAME"], copy[,"NAME"]),])

  #Code for the interative map and the legends, titles, and other map things
  break1 = c(-Inf ,-27, -24, -21, -18, -15, -12, -9, -6, -3, -1, 1, 3, 6, 9, 12, 15, 18, 21, 24, 27, Inf)
  RGcolors = c(red10,red9,red8,red7,red6,red5,red4,red3,red2,red1, gray, green1,green2,green3,green4,green5,green6,green7,green8,green9,green10)
  statemap <- tm_shape(nationgeo) + tm_polygons("Change", breaks = break1, palette = RGcolors , contrast=.7, id="name", title= title) + tm_style_gray() + tm_format_World()

  #Saving the map
  save_tmap(statemap, namesave , width=1920, height=1080)

  #Returns the desired s4 object
  return(statemap)
}

#Creating the maps for year to year

usmap2000.2002 <- usmap(yearsdata$X2001, yearsdata$X2000, "usmap2000.2001.png", "Change in Students 2000-2001")
usmap2001.2003 <- usmap(yearsdata$X2002, yearsdata$X2001, "usmap2001.2002.png", "Change in Students 2001-2002")
usmap2002.2004 <- usmap(yearsdata$X2003, yearsdata$X2002, "usmap2002.2003.png", "Change in Students 2002-2003")
usmap2003.2005 <- usmap(yearsdata$X2004, yearsdata$X2003, "usmap2003.2004.png", "Change in Students 2003-2004")
usmap2004.2006 <- usmap(yearsdata$X2005, yearsdata$X2004, "usmap2004.2005.png", "Change in Students 2004-2005")
usmap2005.2007 <- usmap(yearsdata$X2006, yearsdata$X2005, "usmap2005.2006.png", "Change in Students 2005-2006")
usmap2006.2008 <- usmap(yearsdata$X2007, yearsdata$X2006, "usmap2006.2007.png", "Change in Students 2006-2007")
usmap2007.2009 <- usmap(yearsdata$X2008, yearsdata$X2007, "usmap2007.2008.png", "Change in Students 2007-2008")
usmap2008.2010 <- usmap(yearsdata$X2009, yearsdata$X2008, "usmap2008.2009.png", "Change in Students 2008-2009")
usmap2009.2011 <- usmap(yearsdata$X2010, yearsdata$X2009, "usmap2009.2010.png", "Change in Students 2009-2010")
usmap2010.2012 <- usmap(yearsdata$X2011, yearsdata$X2010, "usmap2010.2011.png", "Change in Students 2010-2011")
usmap2011.2013 <- usmap(yearsdata$X2012, yearsdata$X2011, "usmap2011.2012.png", "Change in Students 2011-2012")
usmap2012.2014 <- usmap(yearsdata$X2013, yearsdata$X2012, "usmap2012.2013.png", "Change in Students 2012-2013")
usmap2013.2015 <- usmap(yearsdata$X2014, yearsdata$X2013, "usmap2013.2014.png", "Change in Students 2013-2014")
usmap2014.2016 <- usmap(yearsdata$X2015, yearsdata$X2014, "usmap2014.2015.png", "Change in Students 2014-2015")

#loading the maps
usmap2000.2002
usmap2001.2003
usmap2002.2004
usmap2003.2005
usmap2004.2006
usmap2005.2007
usmap2006.2008
usmap2007.2009
usmap2008.2010
usmap2009.2011
usmap2010.2012
usmap2011.2013
usmap2012.2014
usmap2013.2015
usmap2014.2016


