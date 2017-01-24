#' @title Comparison
#'
#' @description Provides comparison summary data of the students that were in each class.
#'     For example, the mean change of the number of students across all areas
#'     or the total number of students accepted each year, or the percent change of students
#'     from year to year. The code is still somewhat buggy as there are territories mentioned
#'     in the Williams data that are not official countries or states, such as Palestine territory occuppie.
#'     Thus the (#ofinternationals) + (#ofdomestics) do not always equal (#oftotalstudents).
#'
#' @param year1 The data vector that you want to subtract from. It is the year that will be used first
#' @param year2 The data vector is that will be subtracted from
#' @return a comparison summary between year 1 and year 2
#'
#' @export


comparison <- function(year1, year2){

  ##creating a copy matrix of a 2 by 207 matrix and filling in the first columnn with the countries
  copy <- as.data.frame(matrix(0, ncol = 1, nrow =207))
  copy$V1 <- yearsdata$State.Countries
  colnames(copy)[1] <- "NAME"

  ##Beginning sorting of international and domestic students
  nationshapefile <- "~/mapstu/inst/extdata/cb_2015_us_state_20m/cb_2015_us_state_20m.shp"
  countryshpfiles ="~/mapstu/inst/extdata/TM_WORLD_BORDERS-0.3/TM_WORLD_BORDERS-0.3.shp"

  ##This is a helper function that sorts out the sum of international students in each year
  international <- function(year1, year2){

    ##reads in the internation shapefiles
    intmapgeo <- read_shape(file = countryshpfiles)

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

    ##intializes copies of the data frame
    copy1 <- copy
    copy2 <- copy

    ##adds in the data of students from each year into the dataframe
    copy1$Students <- c(year1)
    copy2$Students <- c(year2)

    ##appends both years of data to the S4 class
    intmapgeo@data <- data.frame(intmapgeo@data, copy1[match(intmapgeo@data[,"NAME"], copy1[,"NAME"]),])
    intmapgeo@data <- data.frame(intmapgeo@data, copy2[match(intmapgeo@data[,"NAME"], copy2[,"NAME"]),])

    ##pulls the sum of the international data
    numintyear1 <- sum(intmapgeo$Students, na.rm = TRUE)
    numintyear2 <- sum(intmapgeo$Students.1, na.rm = TRUE)

    return(c(numintyear1, numintyear2))
  }

  ##Calculates the approximate number of international students at Williams in each year
  int <- international(year1, year2)
  intstuyear1 <- int[1]
  intstuyear2 <- int[2]

  ##This is a domestic function that sorts out the sum of domestic students in each year
  domestic <- function(year1, year2){

    ##Reads in the state shape files
    usmapgeo <- read_shape(file = nationshapefile)

    ##Corrects the Districtof Columbia issue
    copy$NAME <- as.character(copy$NAME)
    copy[copy$NAME == "Districtof Columbia", 'NAME'] <- "District of Columbia"

    ##intializes copies of the data frame
    copy1 <- copy
    copy2 <- copy

    ##adds in the data of students from each year into the dataframe
    copy1$Students <- c(year1)
    copy2$Students <- c(year2)

    ##appends both years of data to the S4 class
    usmapgeo@data <- data.frame(usmapgeo@data, copy1[match(usmapgeo@data[,"NAME"], copy1[,"NAME"]),])
    usmapgeo@data <- data.frame(usmapgeo@data, copy2[match(usmapgeo@data[,"NAME"], copy2[,"NAME"]),])

    numdomyear1 <- sum(c(usmapgeo$Students, na.rm = TRUE))
    numdomyear2 <- sum(c(usmapgeo$Students.1, na.rm = TRUE))

    return(c(numdomyear1, numdomyear2))
  }

  ##Calculates the approximate number of domestic students at Williams in each year
  dom <- domestic(year1, year2)
  domyear1 <- dom[1]
  domyear2 <- dom[2]

  ##Calculates the total number of students in each year
  total1 <- sum(c(year1))
  total2 <- sum(c(year2))

  ##Adds the absolutechange colomn to each data frame
  copy$Change <- c(year1 - year2)

  ##Prints the country, largest change in the year
  print("Max Change:")
  maxchange <- copy[which.max(copy$Change),]
  print(maxchange)

  ##Prints the country, smallest change in the year
  print("Min Change:")
  minchange <- copy[which.min(copy$Change),]
  print(minchange)
  ##Calculates the mean change of each year vector
  meanchange <- mean(c(year1-year2))

  ##Creates the dataframe that is to be returned with comparison values
  df <- as.data.frame(matrix(0, ncol = 1, nrow = 4))
  df$V1 <- c("Number of Domestic Students", "Number of International Students",
               "Total Number of Students", "Mean Change")
  df$Year1 <- c(domyear1, intstuyear1, total1, meanchange)
  df$Year2 <- c(domyear2, intstuyear2, total2, NA)

  ##Returns the desired data frame
  return(df)
}
