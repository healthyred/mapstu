#' Cleaning Data
#'
#' Reads in all the data in a clean format
#'
#' @param x Clean text files that have been parsed into a readable format by
#' the read years function
#' @return a dataframe for the dataset given
#' @export

#We will read each 2000-2001 data as the year2000 for simpicities sake
#After using readYears the first time, I changed some words that were
#not cleaned properly manually, so this is not used again
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

#function that converts the dataset to a data frame
datatodf <- function(x){

  #Creates dataframes for all of the data
  year <- read.csv(x, header = FALSE)

  #Renames all of the columns in the dataframe
  newname <- c("State/Countries", "Number of Students")
  names(year) <- c("State/Countries", "Students Attending in 2000-2001")

  #Creating the data frames for all of these lists
  yeardf <- data.frame(year)

  #Returns the dataframe
  return(yeardf)
}

#Creating the data frames
year2000 <- datatodf("~/mapstu/inst/extdata/2000.2001_Clean")
year2001 <- datatodf("~/mapstu/inst/extdata/2001.2002_Clean")
year2002 <- datatodf("~/mapstu/inst/extdata/2002.2003_Clean")
year2003 <- datatodf("~/mapstu/inst/extdata/2003.2004_Clean")
year2004 <- datatodf("~/mapstu/inst/extdata/2004.2005_Clean")
year2005 <- datatodf("~/mapstu/inst/extdata/2005.2006_Clean")
year2006 <- datatodf("~/mapstu/inst/extdata/2006.2007_Clean")
year2007 <- datatodf("~/mapstu/inst/extdata/2007.2008_Clean")
year2008 <- datatodf("~/mapstu/inst/extdata/2008.2009_Clean")
year2009 <- datatodf("~/mapstu/inst/extdata/2009.2010_Clean")
year2010 <- datatodf("~/mapstu/inst/extdata/2010.2011_Clean")
year2011 <- datatodf("~/mapstu/inst/extdata/2011.2012_Clean")
year2012 <- datatodf("~/mapstu/inst/extdata/2012.2013_Clean")
year2013 <- datatodf("~/mapstu/inst/extdata/2013.2014_Clean")
year2014 <- datatodf("~/mapstu/inst/extdata/2014.2015_Clean")
year2015 <- datatodf("~/mapstu/inst/extdata/2015.2016_Clean")

#Creates a total data frame by states and countries using recursion
totaldata <- function(){
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
  totalframe14[is.na(totalframe14)] <- 0
  save(totalframe14, file = "completedata2000.2015.Rda")
  yearsdata <- totalframe14
  return(yearsdata)
}

#Creates the total dataframe with respect to the names assigned above
yearsdata <- totaldata()
