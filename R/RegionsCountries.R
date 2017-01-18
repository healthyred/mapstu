#This attempts to map all the foreign countries
#plans to take in the data set
install.packages("tmap")
install.packages("leaflet")
install.packages("stringdist")
library("tmap")
library("leaflet")
library("tmaptools")
library("stringdist")
library(raster)

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
copy$V1 <- totalframe14$State.Countries
colnames(copy)[1] <- "NAME"

#Initiates a basic data frame for every year
year2000.2002 <- copy
year2001.2003 <- copy
year2002.2004 <- copy
year2003.2005 <- copy
year2004.2006 <- copy
year2005.2007 <- copy
year2006.2008 <- copy
year2007.2009 <- copy
year2008.2010 <- copy
year2009.2011 <- copy
year2010.2012 <- copy
year2011.2013 <- copy
year2012.2014 <- copy
year2013.2015 <- copy
year2014.2016 <- copy

#Adds the absolutechange colomn to each data frame
year2000.2002$Change <- c(yearsdata$Students.Attending.in.2001.2002- yearsdata$Students.Attending.in.2000.2001)
year2001.2003$Change <- c(yearsdata$Students.Attending.in.2002.2003- yearsdata$Students.Attending.in.2001.2002)
year2002.2004$Change <- c(yearsdata$Students.Attending.in.2003.2004- yearsdata$Students.Attending.in.2002.2003)
year2003.2005$Change <- c(yearsdata$Students.Attending.in.2004.2005- yearsdata$Students.Attending.in.2003.2004)
year2004.2006$Change <- c(yearsdata$Students.Attending.in.2005.2006- yearsdata$Students.Attending.in.2004.2005)
year2005.2007$Change <- c(yearsdata$Students.Attending.in.2006.2007- yearsdata$Students.Attending.in.2005.2006)
year2006.2008$Change <- c(yearsdata$Students.Attending.in.2007.2008- yearsdata$Students.Attending.in.2006.2007)
year2007.2009$Change <- c(yearsdata$Students.Attending.in.2008.2009- yearsdata$Students.Attending.in.2007.2008)
year2008.2010$Change <- c(yearsdata$Students.Attending.in.2009.2010- yearsdata$Students.Attending.in.2008.2009)
year2009.2011$Change <- c(yearsdata$Students.Attending.in.2010.2011- yearsdata$Students.Attending.in.2009.2010)
year2010.2012$Change <- c(yearsdata$Students.Attending.in.2011.2012- yearsdata$Students.Attending.in.2010.2011)
year2011.2013$Change <- c(yearsdata$Students.Attending.in.2012.2013- yearsdata$Students.Attending.in.2011.2012)
year2012.2014$Change <- c(yearsdata$Students.Attending.in.2013.2014- yearsdata$Students.Attending.in.2012.2013)
year2013.2015$Change <- c(yearsdata$Students.Attending.in.2014.2015- yearsdata$Students.Attending.in.2013.2014)
year2014.2016$Change <- c(yearsdata$Students.Attending.in.2015.2016- yearsdata$Students.Attending.in.2014.2015)

#Reading in the Nationfile and intiating shpfiles for each year
countryshpfiles ="~/HutchinHill/TM_WORLD_BORDERS-0.3/TM_WORLD_BORDERS-0.3.shp"
countrygeo2000.2002<- read_shape(file = countryshpfiles)
countrygeo2001.2003<- read_shape(file = countryshpfiles)
countrygeo2002.2004<- read_shape(file = countryshpfiles)
countrygeo2003.2005<- read_shape(file = countryshpfiles)
countrygeo2004.2006<- read_shape(file = countryshpfiles)
countrygeo2005.2007<- read_shape(file = countryshpfiles)
countrygeo2006.2008<- read_shape(file = countryshpfiles)
countrygeo2007.2009<- read_shape(file = countryshpfiles)
countrygeo2008.2010<- read_shape(file = countryshpfiles)
countrygeo2009.2011<- read_shape(file = countryshpfiles)
countrygeo2010.2012<- read_shape(file = countryshpfiles)
countrygeo2011.2013<- read_shape(file = countryshpfiles)
countrygeo2012.2014<- read_shape(file = countryshpfiles)
countrygeo2013.2015<- read_shape(file = countryshpfiles)
countrygeo2014.2016<- read_shape(file = countryshpfiles)

#Creates maps for every single year
countrygeo2000.2002@data<- data.frame(countrygeo2000.2002@data, year2000.2002[match(countrygeo2000.2002@data[,"NAME"], year2000.2002[,"NAME"]),])
countrymap2000.2002 <- tm_shape(countrygeo2000.2002)+ tm_polygons("Change", breaks = c(-Inf ,-27, -24, -21, -18, -15, -12, -9, -6, -3, -1, 1, 3, 6, 9, 12, 15, 18, 21, 24, 27, Inf), palette = c(red10,red9,red8,red7,red6,red5,red4,red3,red2,red1, gray, green1,green2,green3,green4,green5,green6,green7,green8,green9,green10), contrast=.7, id="name", title="Change in Students 2000-2002")
countrymap2000.2002
