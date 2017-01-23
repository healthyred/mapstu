#' Worldplot
#'
#' Takes in two years worth of datasets and returns a map of the
#' international countries that maps the difference of the two datasets
#'
#' @param year The vector for the year that is to be mapped
#' @param title The title of the plot and the name which it will be saved to
#' @param US choice of mapping US territories, or mapping international countries
#' @param save Whether you want to save or not
#' @return a s4 object that has the year dataset mapped to it
#'
#' @examples
#' ##Creating the map of the data with data from 2001 and 2000
#' countrymap2000.2001 <- countrymap(yearsdata$X2001, yearsdata$X2000, "Change in Students 2000-2001")
#'
#' @import leaflet tmap tmaptools
#' @export
#'

worldplot <- function(year, title, US = TRUE, save = FALSE){
  library(leaflet)
  library(tmap)
  library(tmaptools)

  ##creating a copy matrix of a 2 by 207 matrix and filling in the first columnn with the countries
  copy <- as.data.frame(matrix(0, ncol = 1, nrow =207))
  copy$V1 <- yearsdata$State.Countries
  colnames(copy)[1] <- "NAME"

  ##Adds the absolutechange colomn to each data frame
  copy$Students <- c(year)

  ##reads in the map data of the U.S. and the worldmap
  nationshapefile <- "~/mapstu/inst/extdata/cb_2015_us_state_20m/cb_2015_us_state_20m.shp"
  countryshpfiles ="~/mapstu/inst/extdata/TM_WORLD_BORDERS-0.3/TM_WORLD_BORDERS-0.3.shp"

  if (US = TRUE){
    #selects the map of the U.S.
    nationgeo <- read_shape(file = nationshapefile)

    ##Corrects the Districtof Columbia issue
    copy$NAME <- as.character(copy$NAME)
    copy[copy$NAME == "Districtof Columbia", 'NAME'] <- "District of Columbia"
  }

  else{
    countrygeo <- read_shape(file = countryshpfiles)

    ##Must correct democratic republic of congo, republic of korea, georgia,
    ##bosnia and herzgonivia, trinidad and tobago, united republic of tanzania
    copy$NAME <- as.character(copy$NAME)
    copy[copy$NAME == "Districtof Columbia", 'NAME'] <- "District of Columbia"
    copy[copy$NAME == "Trinidadand Tobago", 'NAME'] <- "Trinidad and Tobago"
    copy[copy$NAME == "Muscatand Oman", 'NAME'] <- "Oman"
    copy[copy$NAME == "Republicof Korea", 'NAME'] <- "Trinidad and Tobago"
    copy[copy$NAME == "Georgia", 'NAME'] <- "Georgia(State)"
    copy[copy$NAME == "Georgia(Country)", 'NAME'] <- "Georgia"
    copy[copy$NAME == "Laos", 'NAME'] <- "Lao People's Democratic Republic"
  }

  ##Creating map
  nationgeo@data <- data.frame(nationgeo@data, copy[match(nationgeo@data[,"NAME"], copy[,"NAME"]),])

  ##Code for the interative map and the legends, titles, and other map things
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
