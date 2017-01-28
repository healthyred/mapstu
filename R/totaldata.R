#' @title Cleaning Data
#'
#' @description Reads in all the data in a clean format
#'
#' @param x Clean text files that have been parsed into a readable format by
#'     the read years function
#' @param year The fall year of the dataset
#' @return a dataframe for the dataset given
#' @example yearsdata <- totaldata()
#' @import plyr
#'
#' @export

##We will read each 2000-2001 data as the year2000 for simpicities sake
##After using readYears the first time, I changed some words that were
##not cleaned properly manually, so this is not used again
#readYears("~/mapstu/inst/extdata/2000-2001.rtf", "2000.2001_Clean2")
#readYears("~/mapstu/inst/extdata/2000-2001.rtf", "2001.2002_Clean2")
#readYears("~/mapstu/inst/extdata/2000-2001.rtf", "2002.2003_Clean2")
#readYears("~/mapstu/inst/extdata/2000-2001.rtf", "2003.2004_Clean2")
#readYears("~/mapstu/inst/extdata/2000-2001.rtf", "2004.2005_Clean2")
#readYears("~/mapstu/inst/extdata/2000-2001.rtf", "2005.2006_Clean2")
#readYears("~/mapstu/inst/extdata/2000-2001.rtf", "2006.2007_Clean2")
#readYears("~/mapstu/inst/extdata/2000-2001.rtf", "2007.2008_Clean2")
#readYears("~/mapstu/inst/extdata/2000-2001.rtf", "2008.2009_Clean2")
#readYears("~/mapstu/inst/extdata/2000-2001.rtf", "2009.2010_Clean2")
#readYears("~/mapstu/inst/extdata/2000-2001.rtf", "2010.2011_Clean2")
#readYears("~/mapstu/inst/extdata/2000-2001.rtf", "2011.2012_Clean2")
#readYears("~/mapstu/inst/extdata/2000-2001.rtf", "2012.2013_Clean2")
#readYears("~/mapstu/inst/extdata/2000-2001.rtf", "2013.2014_Clean2")
#readYears("~/mapstu/inst/extdata/2000-2001.rtf", "2014.2015_Clean2")
#readYears("~/mapstu/inst/extdata/2000-2001.rtf", "2015.2016_Clean2")



##Creates a total data frame by states and countries using recursion
totaldata <- function(){

  library(plyr)

  ##function that converts the dataset to a data frame
  datatodf <- function(x, name){

    ##Creates dataframes for all of the data
    year <- read.csv(x, header = FALSE, check.names = FALSE)

    ##Renames all of the columns in the dataframe
    names(year) <- c("State/Countries", name)

    ##Creating the data frames for all of these lists
    yeardf <- data.frame(year)

    ##Returns the dataframe
    return(yeardf)
  }

  ##Creating the data frames
  year2000 <- datatodf("~/mapstu/inst/extdata/2000.2001_Clean", "2000")
  year2001 <- datatodf("~/mapstu/inst/extdata/2001.2002_Clean", "2001")
  year2002 <- datatodf("~/mapstu/inst/extdata/2002.2003_Clean", "2002")
  year2003 <- datatodf("~/mapstu/inst/extdata/2003.2004_Clean", "2003")
  year2004 <- datatodf("~/mapstu/inst/extdata/2004.2005_Clean", "2004")
  year2005 <- datatodf("~/mapstu/inst/extdata/2005.2006_Clean", "2005")
  year2006 <- datatodf("~/mapstu/inst/extdata/2006.2007_Clean", "2006")
  year2007 <- datatodf("~/mapstu/inst/extdata/2007.2008_Clean", "2007")
  year2008 <- datatodf("~/mapstu/inst/extdata/2008.2009_Clean", "2008")
  year2009 <- datatodf("~/mapstu/inst/extdata/2009.2010_Clean", "2009")
  year2010 <- datatodf("~/mapstu/inst/extdata/2010.2011_Clean", "2010")
  year2011 <- datatodf("~/mapstu/inst/extdata/2011.2012_Clean", "2011")
  year2012 <- datatodf("~/mapstu/inst/extdata/2012.2013_Clean", "2012")
  year2013 <- datatodf("~/mapstu/inst/extdata/2013.2014_Clean", "2013")
  year2014 <- datatodf("~/mapstu/inst/extdata/2014.2015_Clean", "2014")
  year2015 <- datatodf("~/mapstu/inst/extdata/2015.2016_Clean", "2015")

  ##merges all of the dataframes together to create a complete data frame
  totalframe <- merge(year2000, year2001, by= c("State.Countries"), all = TRUE)
  totalframe1 <- merge(totalframe, year2002, by= c("State.Countries"), all = TRUE)
  totalframe2 <- merge(totalframe1, year2003, by= c("State.Countries"), all = TRUE)
  totalframe3 <- merge(totalframe2, year2004, by= c("State.Countries"), all = TRUE)
  totalframe4 <- merge(totalframe3, year2005, by= c("State.Countries"), all = TRUE)
  totalframe5 <- merge(totalframe4, year2006, by= c("State.Countries"), all = TRUE)
  totalframe6 <- merge(totalframe5, year2007, by= c("State.Countries"), all = TRUE)
  totalframe7 <- merge(totalframe6, year2008, by= c("State.Countries"), all = TRUE)
  totalframe8 <- merge(totalframe7, year2009, by= c("State.Countries"), all = TRUE)
  totalframe9 <- merge(totalframe8, year2010, by= c("State.Countries"), all = TRUE)
  totalframe10 <- merge(totalframe9, year2011, by= c("State.Countries"), all = TRUE)
  totalframe11 <- merge(totalframe10, year2012, by= c("State.Countries"), all = TRUE)
  totalframe12 <- merge(totalframe11, year2013, by= c("State.Countries"), all = TRUE)
  totalframe13 <- merge(totalframe12, year2014, by= c("State.Countries"), all = TRUE)
  totalframe14 <- merge(totalframe13, year2015, by= c("State.Countries"), all = TRUE)

  ##Changes all the NA values in the df to zero to make calculations easier
  totalframe14[is.na(totalframe14)] <- 0

  ##Rename the names of the year columns for less easier calling
  totalframe14 <- rename(totalframe14, c("X2000" = "2000",
                                         "X2001" = "2001",
                                         "X2002" = "2002",
                                         "X2003" = "2003",
                                         "X2004" = "2004",
                                         "X2005" = "2005",
                                         "X2006" = "2006",
                                         "X2007" = "2007",
                                         "X2008" = "2008",
                                         "X2009" = "2009",
                                         "X2010" = "2010",
                                         "X2011" = "2011",
                                         "X2012" = "2012",
                                         "X2013" = "2013",
                                         "X2014" = "2014",
                                         "X2015" = "2015"))

  ##Returns the complete dataframe
  return(totalframe14)
}


