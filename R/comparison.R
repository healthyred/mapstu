#' @title Comparison
#'
#' @description Provides comparison summary data of the students that were in each class.
#'     For example, the mean change of the number of students across all areas
#'     or the total number of students accepted each year, or the percent change of students
#'     from year to year
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
  colnames(copy1)[1] <- "NAME"

  ##Adds the absolutechange colomn to each data frame
  copy$Change <- c(year1 - year2)

  ##Calculates the total number of students in each year
  total1 <- sum(c(year1))
  total2 <- sum(c(year2))

  ##Calculates the total change of domestic students in each year


  ##Calculates the total change of international students in each year
  #selects to graph the international countries
  mapgeo <- read_shape(file = countryshpfiles)


  ##Must correct democratic republic of congo, republic of korea, georgia,
  ##bosnia and herzgonivia, trinidad and tobago, united republic of tanzania
  copy1$NAME <- as.character(copy$NAME)
  copy1[copy1$NAME == "Districtof Columbia", 'NAME'] <- "District of Columbia"
  copy1[copy1$NAME == "Trinidadand Tobago", 'NAME'] <- "Trinidad and Tobago"
  copy1[copy1$NAME == "Muscatand Oman", 'NAME'] <- "Oman"
  copy1[copy1$NAME == "Republicof Korea", 'NAME'] <- "Trinidad and Tobago"
  copy1[copy1$NAME == "Georgia", 'NAME'] <- "Georgia(State)"
  copy1[copy1$NAME == "Georgia(Country)", 'NAME'] <- "Georgia"
  copy1[copy1$NAME == "Laos", 'NAME'] <- "Lao People's Democratic Republic"


  ##Returns the country, largest change in the year
  maxchange <- copy[which.max(copy$Change),]

  ##Returns the country, smallest change in the year
  minchange <- copy[which.min(copy$Change),]
}
