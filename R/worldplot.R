#' Worldplot
#'
#' Takes in a year vector of data and returns a map of the US or
#' international countries that maps the geographical distribution of the datasets
#'
#' @param year The vector for the year that is to be mapped
#' @param title The title of the plot and the name which it will be saved to
#' @param US choice of mapping US territories or mapping international countries
#' @param save TRUE to save, FALSE to not
#' @param interactive TRUE to choose the interactive mode in viewer, FALSE to view in just the default plot
#' @return a S4 object that has the year dataset mapped to it
#'
#' @examples
#' ##Creating a state map of the data with data from 2001
#' usmap2001 <- worldplot(yearsdata$X2001, title = "Change in Students 2000-2001")
#'
#' ##Creating an international map with interactive view
#' worldmap <- worldplot(yearsdata$X2014, title = "Change in Students 2000-2001", interactive = TRUE)
#'
#' @import leaflet tmap tmaptools
#' @export
#'

##The function takes in a year vector organizing all of the data into a list object.
##Then appends it to an S4 object and returns a plot of either the US, or international countries.
worldplot <- function(year, title = "", US = TRUE, save = FALSE, interactive = FALSE){
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

  ##Sets the breaks in the maps. The US map uses breaksby10 for a better visual representation,
  ##whereas the international map auto scales.
  breaksby10 = NULL

  ##the if loop determines whether the user wants to plot international areas or states
  if (US == TRUE){
    #selects the map of the U.S.
    mapgeo <- read_shape(file = nationshapefile)

    ##Corrects the Districtof Columbia issue
    copy$NAME <- as.character(copy$NAME)
    copy[copy$NAME == "Districtof Columbia", 'NAME'] <- "District of Columbia"

    ##A for loop that generates the breaks in the legend that is used to map the data
    breaksby10 = c()
    for (i in 0:300){
      if (i%%20 == 0){
        breaksby10 = c(breaksby10, i)
      }
    }

    ##Adds Inf to the end of the list
    breaksby10 = c(breaksby10, Inf)
  }

  ##choice for the selection of international countries
  else{
    #selects to graph the international countries
    mapgeo <- read_shape(file = countryshpfiles)

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
  mapgeo@data <- data.frame(mapgeo@data, copy[match(mapgeo@data[,"NAME"], copy[,"NAME"]),])

  ##Code for the legends, titles, color scheme, format of the map, and creation of the actual plot


  ##Creation of the completed plot with the data incorporated
  worldmap <- tm_shape(mapgeo) + tm_polygons(c("Students"),
                                             breaks = breaksby10,
                                             palette = "Purples",
                                             contrast=.7,
                                             id="name",
                                             auto.palette.mapping=FALSE,
                                             title= title) + tm_style_gray() + tm_format_World()

  ##Saving the map
  if (save == TRUE){
    save_tmap(worldmap, paste(title, ".png", sep = ""))
  }

  ##Choosing how the map is viewed, either plot or interactive mode
  if (interactive == TRUE){
    tmap_mode("view")
  }
  else{
    tmap_mode("plot")
  }

  ##Returns the desired s4 object
  return(worldmap)
}
