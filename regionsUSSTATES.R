#this is the script to describe the classes of the regions
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
nationshapefile <- "~/HutchinHill/cb_2015_us_state_20m/cb_2015_us_state_20m.shp"
nationgeo2000.2002<- read_shape(file = nationshapefile)
nationgeo2001.2003<- read_shape(file = nationshapefile)
nationgeo2002.2004<- read_shape(file = nationshapefile)
nationgeo2003.2005<- read_shape(file = nationshapefile)
nationgeo2004.2006<- read_shape(file = nationshapefile)
nationgeo2005.2007<- read_shape(file = nationshapefile)
nationgeo2006.2008<- read_shape(file = nationshapefile)
nationgeo2007.2009<- read_shape(file = nationshapefile)
nationgeo2008.2010<- read_shape(file = nationshapefile)
nationgeo2009.2011<- read_shape(file = nationshapefile)
nationgeo2010.2012<- read_shape(file = nationshapefile)
nationgeo2011.2013<- read_shape(file = nationshapefile)
nationgeo2012.2014<- read_shape(file = nationshapefile)
nationgeo2013.2015<- read_shape(file = nationshapefile)
nationgeo2014.2016<- read_shape(file = nationshapefile)

#Corrects the Districtof Columbia issue
year2000.2002$NAME <- as.character(year2000.2002$NAME)
year2000.2002[year2000.2002$NAME== "Districtof Columbia", 'NAME'] <- "District of Columbia"
year2001.2003$NAME <- as.character(year2001.2003$NAME)
year2001.2003[year2001.2003$NAME== "Districtof Columbia", 'NAME'] <- "District of Columbia"
year2002.2004$NAME <- as.character(year2002.2004$NAME)
year2002.2004[year2002.2004$NAME== "Districtof Columbia", 'NAME'] <- "District of Columbia"
year2003.2005$NAME <- as.character(year2003.2005$NAME)
year2003.2005[year2003.2005$NAME== "Districtof Columbia", 'NAME'] <- "District of Columbia"
year2004.2006$NAME <- as.character(year2004.2006$NAME)
year2004.2006[year2004.2006$NAME== "Districtof Columbia", 'NAME'] <- "District of Columbia"
year2005.2007$NAME <- as.character(year2005.2007$NAME)
year2005.2007[year2005.2007$NAME== "Districtof Columbia", 'NAME'] <- "District of Columbia"
year2006.2008$NAME <- as.character(year2006.2008$NAME)
year2006.2008[year2006.2008$NAME== "Districtof Columbia", 'NAME'] <- "District of Columbia"
year2007.2009$NAME <- as.character(year2007.2009$NAME)
year2007.2009[year2007.2009$NAME== "Districtof Columbia", 'NAME'] <- "District of Columbia"
year2008.2010$NAME <- as.character(year2008.2010$NAME)
year2008.2010[year2008.2010$NAME== "Districtof Columbia", 'NAME'] <- "District of Columbia"
year2009.2011$NAME <- as.character(year2009.2011$NAME)
year2009.2011[year2009.2011$NAME== "Districtof Columbia", 'NAME'] <- "District of Columbia"
year2010.2012$NAME <- as.character(year2010.2012$NAME)
year2010.2012[year2010.2012$NAME== "Districtof Columbia", 'NAME'] <- "District of Columbia"
year2011.2013$NAME <- as.character(year2011.2013$NAME)
year2011.2013[year2011.2013$NAME== "Districtof Columbia", 'NAME'] <- "District of Columbia"
year2012.2014$NAME <- as.character(year2012.2014$NAME)
year2012.2014[year2012.2014$NAME== "Districtof Columbia", 'NAME'] <- "District of Columbia"
year2013.2015$NAME <- as.character(year2013.2015$NAME)
year2013.2015[year2013.2015$NAME== "Districtof Columbia", 'NAME'] <- "District of Columbia"
year2014.2016$NAME <- as.character(year2014.2016$NAME)
year2014.2016[year2014.2016$NAME== "Districtof Columbia", 'NAME'] <- "District of Columbia"

#Creating maps for every single year
nationgeo2000.2002@data<- data.frame(nationgeo2000.2002@data, year2000.2002[match(nationgeo2000.2002@data[,"NAME"], year2000.2002[,"NAME"]),])
nationgeo2001.2003@data<- data.frame(nationgeo2001.2003@data, year2001.2003[match(nationgeo2001.2003@data[,"NAME"], year2001.2003[,"NAME"]),])
nationgeo2002.2004@data<- data.frame(nationgeo2002.2004@data, year2002.2004[match(nationgeo2002.2004@data[,"NAME"], year2002.2004[,"NAME"]),])
nationgeo2003.2005@data<- data.frame(nationgeo2003.2005@data, year2003.2005[match(nationgeo2003.2005@data[,"NAME"], year2003.2005[,"NAME"]),])
nationgeo2004.2006@data<- data.frame(nationgeo2004.2006@data, year2004.2006[match(nationgeo2004.2006@data[,"NAME"], year2004.2006[,"NAME"]),])
nationgeo2005.2007@data<- data.frame(nationgeo2005.2007@data, year2005.2007[match(nationgeo2005.2007@data[,"NAME"], year2005.2007[,"NAME"]),])
nationgeo2006.2008@data<- data.frame(nationgeo2006.2008@data, year2006.2008[match(nationgeo2006.2008@data[,"NAME"], year2006.2008[,"NAME"]),])
nationgeo2007.2009@data<- data.frame(nationgeo2007.2009@data, year2007.2009[match(nationgeo2007.2009@data[,"NAME"], year2007.2009[,"NAME"]),])
nationgeo2008.2010@data<- data.frame(nationgeo2008.2010@data, year2008.2010[match(nationgeo2008.2010@data[,"NAME"], year2008.2010[,"NAME"]),])
nationgeo2009.2011@data<- data.frame(nationgeo2009.2011@data, year2009.2011[match(nationgeo2009.2011@data[,"NAME"], year2009.2011[,"NAME"]),])
nationgeo2010.2012@data<- data.frame(nationgeo2010.2012@data, year2010.2012[match(nationgeo2010.2012@data[,"NAME"], year2010.2012[,"NAME"]),])
nationgeo2011.2013@data<- data.frame(nationgeo2011.2013@data, year2011.2013[match(nationgeo2011.2013@data[,"NAME"], year2011.2013[,"NAME"]),])
nationgeo2012.2014@data<- data.frame(nationgeo2012.2014@data, year2012.2014[match(nationgeo2012.2014@data[,"NAME"], year2012.2014[,"NAME"]),])
nationgeo2013.2015@data<- data.frame(nationgeo2013.2015@data, year2013.2015[match(nationgeo2013.2015@data[,"NAME"], year2013.2015[,"NAME"]),])
nationgeo2014.2016@data<- data.frame(nationgeo2014.2016@data, year2014.2016[match(nationgeo2014.2016@data[,"NAME"], year2014.2016[,"NAME"]),])


#Code for the interative map
tmap_mode("plot")
tmap_mode("view")
usmap2000.2002 <- tm_shape(nationgeo2000.2002)+ tm_polygons("Change", breaks = c(-Inf ,-27, -24, -21, -18, -15, -12, -9, -6, -3, -1, 1, 3, 6, 9, 12, 15, 18, 21, 24, 27, Inf), palette = c(red10,red9,red8,red7,red6,red5,red4,red3,red2,red1, gray, green1,green2,green3,green4,green5,green6,green7,green8,green9,green10), contrast=.7, id="name", title="Change in Students 2000-2002") + tm_style_gray() + tm_format_World()
usmap2001.2003 <- tm_shape(nationgeo2001.2003)+ tm_polygons("Change", breaks = c(-Inf ,-27, -24, -21, -18, -15, -12, -9, -6, -3, -1, 1, 3, 6, 9, 12, 15, 18, 21, 24, 27, Inf), palette = c(red10,red9,red8,red7,red6,red5,red4,red3,red2,red1, gray, green1,green2,green3,green4,green5,green6,green7,green8,green9,green10) , contrast=.7, id="name", title="Change in Students 2001-2003") + tm_style_gray() + tm_format_World()
usmap2002.2004 <- tm_shape(nationgeo2002.2004)+ tm_polygons("Change", breaks = c(-Inf ,-27, -24, -21, -18, -15, -12, -9, -6, -3, -1, 1, 3, 6, 9, 12, 15, 18, 21, 24, 27, Inf), palette = c(red10,red9,red8,red7,red6,red5,red4,red3,red2,red1, gray, green1,green2,green3,green4,green5,green6,green7,green8,green9,green10), contrast=.7, id="name", title="Change in Students 2002-2004") + tm_style_gray() + tm_format_World()
usmap2003.2005 <- tm_shape(nationgeo2003.2005)+ tm_polygons("Change", breaks = c(-Inf ,-27, -24, -21, -18, -15, -12, -9, -6, -3, -1, 1, 3, 6, 9, 12, 15, 18, 21, 24, 27, Inf), palette = c(red10,red9,red8,red7,red6,red5,red4,red3,red2,red1, gray, green1,green2,green3,green4,green5,green6,green7,green8,green9,green10) , contrast=.7, id="name", title="Change in Students 2003-2005") + tm_style_gray() + tm_format_World()
usmap2004.2006 <- tm_shape(nationgeo2004.2006)+ tm_polygons("Change", breaks = c(-Inf ,-27, -24, -21, -18, -15, -12, -9, -6, -3, -1, 1, 3, 6, 9, 12, 15, 18, 21, 24, 27, Inf), palette = c(red10,red9,red8,red7,red6,red5,red4,red3,red2,red1, gray, green1,green2,green3,green4,green5,green6,green7,green8,green9,green10) , contrast=.7, id="name", title="Change in Students 2004-2006") + tm_style_gray() + tm_format_World()
usmap2005.2007 <- tm_shape(nationgeo2005.2007)+ tm_polygons("Change", breaks = c(-Inf ,-27, -24, -21, -18, -15, -12, -9, -6, -3, -1, 1, 3, 6, 9, 12, 15, 18, 21, 24, 27, Inf), palette = c(red10,red9,red8,red7,red6,red5,red4,red3,red2,red1, gray, green1,green2,green3,green4,green5,green6,green7,green8,green9,green10) , contrast=.7, id="name", title="Change in Students 2005-2007") + tm_style_gray() + tm_format_World()
usmap2006.2008 <- tm_shape(nationgeo2006.2008)+ tm_polygons("Change", breaks = c(-Inf ,-27, -24, -21, -18, -15, -12, -9, -6, -3, -1, 1, 3, 6, 9, 12, 15, 18, 21, 24, 27, Inf), palette = c(red10,red9,red8,red7,red6,red5,red4,red3,red2,red1, gray, green1,green2,green3,green4,green5,green6,green7,green8,green9,green10) , contrast=.7, id="name", title="Change in Students 2006-2008") + tm_style_gray() + tm_format_World()
usmap2007.2009 <- tm_shape(nationgeo2007.2009)+ tm_polygons("Change", breaks = c(-Inf ,-27, -24, -21, -18, -15, -12, -9, -6, -3, -1, 1, 3, 6, 9, 12, 15, 18, 21, 24, 27, Inf), palette = c(red10,red9,red8,red7,red6,red5,red4,red3,red2,red1, gray, green1,green2,green3,green4,green5,green6,green7,green8,green9,green10) , contrast=.7, id="name", title="Change in Students 2007-2009") + tm_style_gray() + tm_format_World()
usmap2008.2010 <- tm_shape(nationgeo2008.2010)+ tm_polygons("Change", breaks = c(-Inf ,-27, -24, -21, -18, -15, -12, -9, -6, -3, -1, 1, 3, 6, 9, 12, 15, 18, 21, 24, 27, Inf), palette = c(red10,red9,red8,red7,red6,red5,red4,red3,red2,red1, gray, green1,green2,green3,green4,green5,green6,green7,green8,green9,green10) , contrast=.7, id="name", title="Change in Students 2008-2010") + tm_style_gray() + tm_format_World()
usmap2009.2011 <- tm_shape(nationgeo2009.2011)+ tm_polygons("Change", breaks = c(-Inf ,-27, -24, -21, -18, -15, -12, -9, -6, -3, -1, 1, 3, 6, 9, 12, 15, 18, 21, 24, 27, Inf), palette = c(red10,red9,red8,red7,red6,red5,red4,red3,red2,red1, gray, green1,green2,green3,green4,green5,green6,green7,green8,green9,green10) , contrast=.7, id="name", title="Change in Students 2009-2011") + tm_style_gray() + tm_format_World()
usmap2010.2012 <- tm_shape(nationgeo2010.2012)+ tm_polygons("Change", breaks = c(-Inf ,-27, -24, -21, -18, -15, -12, -9, -6, -3, -1, 1, 3, 6, 9, 12, 15, 18, 21, 24, 27, Inf), palette = c(red10,red9,red8,red7,red6,red5,red4,red3,red2,red1, gray, green1,green2,green3,green4,green5,green6,green7,green8,green9,green10) , contrast=.7, id="name", title="Change in Students 2010-2012") + tm_style_gray() + tm_format_World()
usmap2011.2013 <- tm_shape(nationgeo2011.2013)+ tm_polygons("Change", breaks = c(-Inf ,-27, -24, -21, -18, -15, -12, -9, -6, -3, -1, 1, 3, 6, 9, 12, 15, 18, 21, 24, 27, Inf), palette = c(red10,red9,red8,red7,red6,red5,red4,red3,red2,red1, gray, green1,green2,green3,green4,green5,green6,green7,green8,green9,green10) , contrast=.7, id="name", title="Change in Students 2011-2013") + tm_style_gray() + tm_format_World()
usmap2012.2014 <- tm_shape(nationgeo2012.2014)+ tm_polygons("Change", breaks = c(-Inf ,-27, -24, -21, -18, -15, -12, -9, -6, -3, -1, 1, 3, 6, 9, 12, 15, 18, 21, 24, 27, Inf), palette = c(red10,red9,red8,red7,red6,red5,red4,red3,red2,red1, gray, green1,green2,green3,green4,green5,green6,green7,green8,green9,green10) , contrast=.7, id="name", title="Change in Students 2012-2014") + tm_style_gray() + tm_format_World()
usmap2013.2015 <- tm_shape(nationgeo2013.2015)+ tm_polygons("Change", breaks = c(-Inf ,-27, -24, -21, -18, -15, -12, -9, -6, -3, -1, 1, 3, 6, 9, 12, 15, 18, 21, 24, 27, Inf), palette = c(red10,red9,red8,red7,red6,red5,red4,red3,red2,red1, gray, green1,green2,green3,green4,green5,green6,green7,green8,green9,green10) , contrast=.7, id="name", title="Change in Students 2013-2015") + tm_style_gray() + tm_format_World()
usmap2014.2016 <- tm_shape(nationgeo2014.2016)+ tm_polygons("Change", breaks = c(-Inf ,-27, -24, -21, -18, -15, -12, -9, -6, -3, -1, 1, 3, 6, 9, 12, 15, 18, 21, 24, 27, Inf), palette = c(red10,red9,red8,red7,red6,red5,red4,red3,red2,red1, gray, green1,green2,green3,green4,green5,green6,green7,green8,green9,green10) , contrast=.7, id="name", title="Change in Students 2014-2016") + tm_style_gray() + tm_format_World()

#loading the maps
usmap2000.2002
usmap2001.2003
usmap2002.2004
usmap2003.2005
usmap2004.2006
usmap2005.2007
usmap2006.2008
usmap2007.2009
usmap2008.2010
usmap2009.2011
usmap2010.2012
usmap2011.2013
usmap2012.2014
usmap2013.2015
usmap2014.2016

#saving the map
save_tmap(usmap2000.2002, "Years2000-2002.png", width=1920, height=1080)
save_tmap(usmap2001.2003, "Years2001-2003.png", width=1920, height=1080)
save_tmap(usmap2002.2004, "Years2002-2004.png", width=1920, height=1080)
save_tmap(usmap2003.2005, "Years2003-2005.png", width=1920, height=1080)
save_tmap(usmap2004.2006, "Years2004-2006.png", width=1920, height=1080)
save_tmap(usmap2005.2007, "Years2005-2007.png", width=1920, height=1080)
save_tmap(usmap2006.2008, "Years2006-2008.png", width=1920, height=1080)
save_tmap(usmap2007.2009, "Years2007-2009.png", width=1920, height=1080)
save_tmap(usmap2008.2010, "Years2008-2010.png", width=1920, height=1080)
save_tmap(usmap2009.2011, "Years2009-2011.png", width=1920, height=1080)
save_tmap(usmap2010.2012, "Years2010-2012.png", width=1920, height=1080)
save_tmap(usmap2011.2013, "Years2011-2013.png", width=1920, height=1080)
save_tmap(usmap2012.2014, "Years2012-2014.png", width=1920, height=1080)
save_tmap(usmap2013.2015, "Years2013-2015.png", width=1920, height=1080)
save_tmap(usmap2014.2016, "Years2014-2016.png", width=1920, height=1080)

#Attempts to make a working code that matches other word
#year2000.2002$match[!is.na(i)] <- nationgeo@data$NAME[i[!is.na(i)]]
#nationgeo@data <- data.frame(nationgeo@data, year2000.2002[match(nationgeo@data[,"NAME"], year2000.2002[,"match"]),])
