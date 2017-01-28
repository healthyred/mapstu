#' Sidebyside
#'
#' Plots two of the years side by side to better compare the diversity of
#' Williams College's geographical distribution of students. There is an
#' international options, and a statemap option.
#'
#' @param year1 The vector for a year of data. It is important to note that
#'     you can also plot sidebyside data of differences
#' @param year2 The vector for a year of data
#' @param title1 The title of the plot and the name which it will be saved to
#' @param title2 The title of the second plot
#' @param savename The name the plot is saved to
#' @param US choice of mapping US territories or mapping international countries
#' @param save TRUE to save, FALSE to not
#' @param interactive TRUE to choose the interactive mode in viewer, FALSE to view in just the default plot
#' @param change Selects whether the year vectors given are differences between years,
#'     or just comparing dirrect yearly geographical distributions
#' @return sidebyside S4 object that has the different year datasets mapped to it
#'
#' @examples
#' ##Creating a state map of the data with data from 2001
#' usmap2001 <- sidebyside(yearsdata$2001, yearsdata$2002, title1 = "2001", title2 = "2002")
#'
#' ##Creating an international map with interactive view
#' worldmap <- sidebyside(yearsdata$2001, yearsdata$2002, title1 = "2001", title2 = "2002", US = FALSE)
#'
#' ##Creating a sidebyside map with change
#' usmapofdifferences <- sidebyside(yearsdata$X2001-yearsdata$X2000, yearsdata$X2015-yearsdata$X2014,
#'                                                                  title1 = "Change from 2000-2001",
#'                                                                  title2 = "Change from 2014-2015",
#'                                                                                    change = TRUE)
#'
#' @import leaflet tmap tmaptools
#' @export
#'

##A function that takes in two different year vectors, and then creates two different S4
##objects and plots them sidebyside each other for better comparison and analysis.
sidebyside <- function(year1, year2,
                       title1 = "",
                       title2 = "",
                       savename = "plot",
                       US = TRUE,
                       save = FALSE,
                       interactive = FALSE,
                       change = FALSE){
  library(leaflet)
  library(tmap)
  library(tmaptools)

  ##creating a copy matrix of a 2 by 207 matrix and filling in the first columnn with the countries
  copy1 <- as.data.frame(matrix(0, ncol = 1, nrow =207))
  copy1$V1 <- yearsdata$State.Countries
  colnames(copy1)[1] <- "NAME"

  ##creating a second copy matrix of a 2 by 207 matrix and filling in the first columnn with the countries
  copy2 <- as.data.frame(matrix(0, ncol = 1, nrow =207))
  copy2$V1 <- yearsdata$State.Countries
  colnames(copy2)[1] <- "NAME"

  ##Adds the absolutechange colomn to each data frame
  copy1$Students1 <- c(year1)
  copy2$Students2 <- c(year2)

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
    copy1$NAME <- as.character(copy1$NAME)
    copy1[copy1$NAME == "Districtof Columbia", 'NAME'] <- "District of Columbia"

    ##Corrects the Districtof Columbia issue
    copy2$NAME <- as.character(copy2$NAME)
    copy2[copy2$NAME == "Districtof Columbia", 'NAME'] <- "District of Columbia"

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
    copy1$NAME <- as.character(copy1$NAME)
    copy1[copy1$NAME == "Districtof Columbia", 'NAME'] <- "District of Columbia"
    copy1[copy1$NAME == "Trinidadand Tobago", 'NAME'] <- "Trinidad and Tobago"
    copy1[copy1$NAME == "Muscatand Oman", 'NAME'] <- "Oman"
    copy1[copy1$NAME == "Republicof Korea", 'NAME'] <- "Trinidad and Tobago"
    copy1[copy1$NAME == "Georgia", 'NAME'] <- "Georgia(State)"
    copy1[copy1$NAME == "Georgia(Country)", 'NAME'] <- "Georgia"
    copy1[copy1$NAME == "Laos", 'NAME'] <- "Lao People's Democratic Republic"

    ##Must correct democratic republic of congo, republic of korea, georgia,
    ##bosnia and herzgonivia, trinidad and tobago, united republic of tanzania
    copy2$NAME <- as.character(copy2$NAME)
    copy2[copy2$NAME == "Districtof Columbia", 'NAME'] <- "District of Columbia"
    copy2[copy2$NAME == "Trinidadand Tobago", 'NAME'] <- "Trinidad and Tobago"
    copy2[copy2$NAME == "Muscatand Oman", 'NAME'] <- "Oman"
    copy2[copy2$NAME == "Republicof Korea", 'NAME'] <- "Trinidad and Tobago"
    copy2[copy2$NAME == "Georgia", 'NAME'] <- "Georgia(State)"
    copy2[copy2$NAME == "Georgia(Country)", 'NAME'] <- "Georgia"
    copy2[copy2$NAME == "Laos", 'NAME'] <- "Lao People's Democratic Republic"
  }

  ##Creating map for year1 and year2
  mapgeo@data <- data.frame(mapgeo@data, copy1[match(mapgeo@data[,"NAME"], copy1[,"NAME"]),])
  mapgeo@data <- data.frame(mapgeo@data, copy2[match(mapgeo@data[,"NAME"], copy2[,"NAME"]),])
  ##Code for the legends, titles, color scheme, format of the map, and creation of the actual plot


  ##Creation of the completed plot with the data incorporated
  ##defaults for color, and EPH spirit
  color = "Purples"

  ##If statement for comparison of change between years
  if (change == TRUE){

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
    breaksby10 = c(-Inf ,-27, -24, -21, -18, -15, -12, -9, -6, -3, -1, 1, 3, 6, 9, 12, 15, 18, 21, 24, 27, Inf)
    color = c(red10, red9, red8, red7, red6, red5, red4, red3, red2, red1,
              gray, green1, green2, green3, green4, green5, green6, green7, green8, green9, green10)
  }

  ##plotting the S4 object
  worldmap <- tm_shape(mapgeo) + tm_polygons(c("Students1", "Students2"),
                                             breaks = breaksby10,
                                             palette = list(color, color),
                                             contrast=.7,
                                             id="name",
                                             auto.palette.mapping=FALSE,
                                             title= c(title1, title2)) + tm_style_gray() + tm_format_World()

  ##Saving the map
  if (save == TRUE){
    save_tmap(worldmap, paste(savename, ".png", sep = ""))
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
