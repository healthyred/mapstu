#This attempts to map all the foreign countries
#plans to take in the data set
install.packages("tmap")
install.packages("leaflet")
library("tmap")
library("leaflet")
library("tmaptools")


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


#Must correct democratic republic of congo, republic of korea, georgia, bosnia and herzgonivia, trinidad and tobago, united republic of tanzania
correct <- function(x){
  x$NAME <- as.character(x$NAME)
  x[x$NAME == "Districtof Columbia", 'NAME'] <- "District of Columbia"
  x[x$NAME == "Trinidadand Tobago", 'NAME'] <- "Trinidad and Tobago"
  x[x$NAME == "Muscatand Oman", 'NAME'] <- "Oman"
  x[x$NAME == "Republicof Korea", 'NAME'] <- "Trinidad and Tobago"
  x[x$NAME == "Georgia", 'NAME'] <- "Georgia(State)"
  x[x$NAME == "Georgia(Country)", 'NAME'] <- "Georgia"
  x[x$NAME == "Laos", 'NAME'] <- "Lao People's Democratic Republic"
  return(x)
}

year2000.2002 <- correct(year2000.2002)
year2001.2003 <- correct(year2001.2003)
year2002.2004 <- correct(year2002.2004)
year2003.2005 <- correct(year2003.2005)
year2004.2006 <- correct(year2004.2006)
year2005.2007 <- correct(year2005.2007)
year2006.2008 <- correct(year2006.2008)
year2007.2009 <- correct(year2007.2009)
year2008.2010 <- correct(year2008.2010)
year2009.2011 <- correct(year2009.2011)
year2010.2012 <- correct(year2010.2012)
year2011.2013 <- correct(year2011.2013)
year2012.2014 <- correct(year2012.2014)
year2013.2015 <- correct(year2013.2015)
year2014.2016 <- correct(year2014.2016)

#Adds the absolutechange colomn to each data frame
year2000.2002$Change <- c(yearsdata$Students.Attending.in.2001.2002 - yearsdata$Students.Attending.in.2000.2001)
year2001.2003$Change <- c(yearsdata$Students.Attending.in.2002.2003 - yearsdata$Students.Attending.in.2001.2002)
year2002.2004$Change <- c(yearsdata$Students.Attending.in.2003.2004 - yearsdata$Students.Attending.in.2002.2003)
year2003.2005$Change <- c(yearsdata$Students.Attending.in.2004.2005 - yearsdata$Students.Attending.in.2003.2004)
year2004.2006$Change <- c(yearsdata$Students.Attending.in.2005.2006 - yearsdata$Students.Attending.in.2004.2005)
year2005.2007$Change <- c(yearsdata$Students.Attending.in.2006.2007 - yearsdata$Students.Attending.in.2005.2006)
year2006.2008$Change <- c(yearsdata$Students.Attending.in.2007.2008 - yearsdata$Students.Attending.in.2006.2007)
year2007.2009$Change <- c(yearsdata$Students.Attending.in.2008.2009 - yearsdata$Students.Attending.in.2007.2008)
year2008.2010$Change <- c(yearsdata$Students.Attending.in.2009.2010 - yearsdata$Students.Attending.in.2008.2009)
year2009.2011$Change <- c(yearsdata$Students.Attending.in.2010.2011 - yearsdata$Students.Attending.in.2009.2010)
year2010.2012$Change <- c(yearsdata$Students.Attending.in.2011.2012 - yearsdata$Students.Attending.in.2010.2011)
year2011.2013$Change <- c(yearsdata$Students.Attending.in.2012.2013 - yearsdata$Students.Attending.in.2011.2012)
year2012.2014$Change <- c(yearsdata$Students.Attending.in.2013.2014 - yearsdata$Students.Attending.in.2012.2013)
year2013.2015$Change <- c(yearsdata$Students.Attending.in.2014.2015 - yearsdata$Students.Attending.in.2013.2014)
year2014.2016$Change <- c(yearsdata$Students.Attending.in.2015.2016 - yearsdata$Students.Attending.in.2014.2015)

#Reading in the Nationfile and intiating shpfiles for each year
countryshpfiles ="~/HutchinHill/TM_WORLD_BORDERS-0.3/TM_WORLD_BORDERS-0.3.shp"
countrygeo2000.2002 <- read_shape(file = countryshpfiles)
countrygeo2001.2003 <- read_shape(file = countryshpfiles)
countrygeo2002.2004 <- read_shape(file = countryshpfiles)
countrygeo2003.2005 <- read_shape(file = countryshpfiles)
countrygeo2004.2006 <- read_shape(file = countryshpfiles)
countrygeo2005.2007 <- read_shape(file = countryshpfiles)
countrygeo2006.2008 <- read_shape(file = countryshpfiles)
countrygeo2007.2009 <- read_shape(file = countryshpfiles)
countrygeo2008.2010 <- read_shape(file = countryshpfiles)
countrygeo2009.2011 <- read_shape(file = countryshpfiles)
countrygeo2010.2012 <- read_shape(file = countryshpfiles)
countrygeo2011.2013 <- read_shape(file = countryshpfiles)
countrygeo2012.2014 <- read_shape(file = countryshpfiles)
countrygeo2013.2015 <- read_shape(file = countryshpfiles)
countrygeo2014.2016 <- read_shape(file = countryshpfiles)

#Creates maps for every single year
countrygeo2000.2002@data <- data.frame(countrygeo2000.2002@data, year2000.2002[match(countrygeo2000.2002@data[,"NAME"], year2000.2002[,"NAME"]),])
countrygeo2001.2003@data <- data.frame(countrygeo2001.2003@data, year2001.2003[match(countrygeo2001.2003@data[,"NAME"], year2001.2003[,"NAME"]),])
countrygeo2002.2004@data <- data.frame(countrygeo2002.2004@data, year2002.2004[match(countrygeo2002.2004@data[,"NAME"], year2002.2004[,"NAME"]),])
countrygeo2003.2005@data <- data.frame(countrygeo2003.2005@data, year2003.2005[match(countrygeo2003.2005@data[,"NAME"], year2003.2005[,"NAME"]),])
countrygeo2004.2006@data <- data.frame(countrygeo2004.2006@data, year2004.2006[match(countrygeo2004.2006@data[,"NAME"], year2004.2006[,"NAME"]),])
countrygeo2005.2007@data <- data.frame(countrygeo2005.2007@data, year2005.2007[match(countrygeo2005.2007@data[,"NAME"], year2005.2007[,"NAME"]),])
countrygeo2006.2008@data <- data.frame(countrygeo2006.2008@data, year2006.2008[match(countrygeo2006.2008@data[,"NAME"], year2006.2008[,"NAME"]),])
countrygeo2007.2009@data <- data.frame(countrygeo2007.2009@data, year2007.2009[match(countrygeo2007.2009@data[,"NAME"], year2007.2009[,"NAME"]),])
countrygeo2008.2010@data <- data.frame(countrygeo2008.2010@data, year2008.2010[match(countrygeo2008.2010@data[,"NAME"], year2008.2010[,"NAME"]),])
countrygeo2009.2011@data <- data.frame(countrygeo2009.2011@data, year2009.2011[match(countrygeo2009.2011@data[,"NAME"], year2009.2011[,"NAME"]),])
countrygeo2010.2012@data <- data.frame(countrygeo2010.2012@data, year2010.2012[match(countrygeo2010.2012@data[,"NAME"], year2010.2012[,"NAME"]),])
countrygeo2011.2013@data <- data.frame(countrygeo2011.2013@data, year2011.2013[match(countrygeo2011.2013@data[,"NAME"], year2011.2013[,"NAME"]),])
countrygeo2012.2014@data <- data.frame(countrygeo2012.2014@data, year2012.2014[match(countrygeo2012.2014@data[,"NAME"], year2012.2014[,"NAME"]),])
countrygeo2013.2015@data <- data.frame(countrygeo2013.2015@data, year2013.2015[match(countrygeo2013.2015@data[,"NAME"], year2013.2015[,"NAME"]),])
countrygeo2014.2016@data <- data.frame(countrygeo2014.2016@data, year2014.2016[match(countrygeo2014.2016@data[,"NAME"], year2014.2016[,"NAME"]),])

#Creates the Legend and the map titles for each map
break1 = c(-Inf , -27, -24, -21, -18, -15, -12, -9, -6, -3, -1, 1, 3, 6, 9, 12, 15, 18, 21, 24, 27, Inf)
RGcolors = c(red10, red9, red8, red7, red6, red5, red4, red3, red2, red1, gray, green1, green2, green3, green4, green5, green6, green7, green8, green9, green10)
countrymap2000.2002 <- tm_shape(countrygeo2000.2002)+ tm_polygons("Change", breaks = break1, palette = RGcolors, contrast=.7, id="name", title="Change in Students 2000-2002")
countrymap2001.2003 <- tm_shape(countrygeo2001.2003)+ tm_polygons("Change", breaks = break1, palette = RGcolors, contrast=.7, id="name", title="Change in Students 2001-2003")
countrymap2002.2004 <- tm_shape(countrygeo2002.2004)+ tm_polygons("Change", breaks = break1, palette = RGcolors, contrast=.7, id="name", title="Change in Students 2002-2004")
countrymap2003.2005 <- tm_shape(countrygeo2003.2005)+ tm_polygons("Change", breaks = break1, palette = RGcolors, contrast=.7, id="name", title="Change in Students 2003-2005")
countrymap2004.2006 <- tm_shape(countrygeo2004.2006)+ tm_polygons("Change", breaks = break1, palette = RGcolors, contrast=.7, id="name", title="Change in Students 2004-2006")
countrymap2005.2007 <- tm_shape(countrygeo2005.2007)+ tm_polygons("Change", breaks = break1, palette = RGcolors, contrast=.7, id="name", title="Change in Students 2005-2007")
countrymap2006.2008 <- tm_shape(countrygeo2006.2008)+ tm_polygons("Change", breaks = break1, palette = RGcolors, contrast=.7, id="name", title="Change in Students 2006-2008")
countrymap2007.2009 <- tm_shape(countrygeo2007.2009)+ tm_polygons("Change", breaks = break1, palette = RGcolors, contrast=.7, id="name", title="Change in Students 2007-2009")
countrymap2008.2010 <- tm_shape(countrygeo2008.2010)+ tm_polygons("Change", breaks = break1, palette = RGcolors, contrast=.7, id="name", title="Change in Students 2008-2010")
countrymap2009.2011 <- tm_shape(countrygeo2009.2011)+ tm_polygons("Change", breaks = break1, palette = RGcolors, contrast=.7, id="name", title="Change in Students 2009-2011")
countrymap2010.2012 <- tm_shape(countrygeo2010.2012)+ tm_polygons("Change", breaks = break1, palette = RGcolors, contrast=.7, id="name", title="Change in Students 2010-2012")
countrymap2011.2013 <- tm_shape(countrygeo2011.2013)+ tm_polygons("Change", breaks = break1, palette = RGcolors, contrast=.7, id="name", title="Change in Students 2011-2013")
countrymap2012.2014 <- tm_shape(countrygeo2012.2014)+ tm_polygons("Change", breaks = break1, palette = RGcolors, contrast=.7, id="name", title="Change in Students 2012-2014")
countrymap2013.2015 <- tm_shape(countrygeo2013.2015)+ tm_polygons("Change", breaks = break1, palette = RGcolors, contrast=.7, id="name", title="Change in Students 2013-2015")
countrymap2014.2016 <- tm_shape(countrygeo2014.2016)+ tm_polygons("Change", breaks = break1, palette = RGcolors, contrast=.7, id="name", title="Change in Students 2014-2016")

#Loads the maps to check
countrymap2000.2002
countrymap2001.2003
countrymap2002.2004
countrymap2003.2005
countrymap2004.2006
countrymap2005.2007
countrymap2006.2008
countrymap2007.2009
countrymap2008.2010
countrymap2009.2011
countrymap2010.2012
countrymap2011.2013
countrymap2012.2014
countrymap2013.2015
countrymap2014.2016

#saving map
save_tmap(countrymap2000.2002, "CYears2000-2002.png", width=1920, height=1080)
save_tmap(countrymap2001.2003, "CYears2001-2003.png", width=1920, height=1080)
save_tmap(countrymap2002.2004, "CYears2002-2004.png", width=1920, height=1080)
save_tmap(countrymap2003.2005, "CYears2003-2005.png", width=1920, height=1080)
save_tmap(countrymap2004.2006, "CYears2004-2006.png", width=1920, height=1080)
save_tmap(countrymap2005.2007, "CYears2005-2007.png", width=1920, height=1080)
save_tmap(countrymap2006.2008, "CYears2006-2008.png", width=1920, height=1080)
save_tmap(countrymap2007.2009, "CYears2007-2009.png", width=1920, height=1080)
save_tmap(countrymap2008.2010, "CYears2008-2010.png", width=1920, height=1080)
save_tmap(countrymap2009.2011, "CYears2009-2011.png", width=1920, height=1080)
save_tmap(countrymap2010.2012, "CYears2010-2012.png", width=1920, height=1080)
save_tmap(countrymap2011.2013, "CYears2011-2013.png", width=1920, height=1080)
save_tmap(countrymap2012.2014, "CYears2012-2014.png", width=1920, height=1080)
save_tmap(countrymap2013.2015, "CYears2013-2015.png", width=1920, height=1080)
save_tmap(countrymap2014.2016, "CYears2014-2016.png", width=1920, height=1080)


