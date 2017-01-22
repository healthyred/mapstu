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
#' @import leaflet tmap tmaptools
#' @export
#'

countrymap <- function(currentyear, oldyear, namesave, title){
  library(tmap)
  library(tmaptools)
  library(leaflet)
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
countrymap2000.2001 <- countrymap(yearsdata$X2001, yearsdata$X2000, "countrymap2000.2001.png", "Change in Students 2000-2001")
countrymap2001.2002 <- countrymap(yearsdata$X2002, yearsdata$X2001, "countrymap2001.2002.png", "Change in Students 2001-2002")
countrymap2002.2003 <- countrymap(yearsdata$X2003, yearsdata$X2002, "countrymap2002.2003.png", "Change in Students 2002-2003")
countrymap2003.2004 <- countrymap(yearsdata$X2004, yearsdata$X2003, "countrymap2003.2004.png", "Change in Students 2003-2004")
countrymap2004.2005 <- countrymap(yearsdata$X2005, yearsdata$X2004, "countrymap2004.2005.png", "Change in Students 2004-2005")
countrymap2005.2006 <- countrymap(yearsdata$X2006, yearsdata$X2005, "countrymap2005.2006.png", "Change in Students 2005-2006")
countrymap2006.2007 <- countrymap(yearsdata$X2007, yearsdata$X2006, "countrymap2006.2007.png", "Change in Students 2006-2007")
countrymap2007.2008 <- countrymap(yearsdata$X2008, yearsdata$X2007, "countrymap2007.2008.png", "Change in Students 2007-2008")
countrymap2008.2009 <- countrymap(yearsdata$X2009, yearsdata$X2008, "countrymap2008.2009.png", "Change in Students 2008-2009")
countrymap2009.2010 <- countrymap(yearsdata$X2010, yearsdata$X2009, "countrymap2009.2010.png", "Change in Students 2009-2010")
countrymap2010.2011 <- countrymap(yearsdata$X2011, yearsdata$X2010, "countrymap2010.2011.png", "Change in Students 2010-2011")
countrymap2011.2012 <- countrymap(yearsdata$X2012, yearsdata$X2011, "countrymap2011.2012.png", "Change in Students 2011-2012")
countrymap2012.2013 <- countrymap(yearsdata$X2013, yearsdata$X2012, "countrymap2012.2013.png", "Change in Students 2012-2013")
countrymap2013.2014 <- countrymap(yearsdata$X2014, yearsdata$X2013, "countrymap2013.2014.png", "Change in Students 2013-2014")
countrymap2014.2015 <- countrymap(yearsdata$X2015, yearsdata$X2014, "countrymap2014.2015.png", "Change in Students 2014-2015")
countrymap2015.2016 <- countrymap(yearsdata$X2016, yearsdata$X2015, "countrymap2015.2016.png", "Change in Students 2015-2016")
#Calls the maps
